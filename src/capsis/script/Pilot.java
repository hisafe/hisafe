/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
 * the GNU Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.script;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import jeeb.lib.util.Alert;
import jeeb.lib.util.ProgressDispatcher;
import jeeb.lib.util.ProgressListener;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.StatusListener;
import capsis.kernel.AbstractPilot;
import capsis.kernel.GModel;
import capsis.kernel.Relay;

/*
 * * Pilot for script mode. This mode is for long or repetitive simulation. They can be described in a script class.
 * 
 * @author F. de Coligny - march 2002
 */
public class Pilot extends AbstractPilot implements StatusListener, ProgressListener {

	static private Pilot instance; // it's not a Singleton pattern
	private final String[] pilotArguments;

	private int currentProgressValue; // fc - 16.5.2006

	/**	Used to get an instance of Pilot.
	 * 	Note: the constructor must have been called once before.
	 */
	static public Pilot getInstance() { // this is not exactly a Singleton pattern
		if (instance == null) { throw new Error("Design error: capsis.script.Pilot constructor must be called once before calling getInstance (), aborted."); }
		return instance;
	}

	/**	Constructor
	 */
	public Pilot(String[] pilotArguments) throws Throwable {
		// This constructor can only be called once
		if (instance != null) { throw new Error("Design error, capsis.script.Pilot constructor must be called only once, stopped"); }
		instance = this;

		setStatusToConsole(true);
		setProgressToConsole(true);

		this.pilotArguments = pilotArguments;
	}

	/**	Starts the pilot
	 */
	@Override
	public void start() throws Exception {

		String scriptName;

		// 2. Retrieve script class name
		try {
			scriptName = pilotArguments[0].trim();
		}
		catch (Exception e) {
			StatusDispatcher.print("Error in Pilot (): Script name not found, can not start script");
			return;
		}

		// Some tests
		if (scriptName.length() == 0) {
			StatusDispatcher.print("Error in Pilot (): Script name is empty, can not start script");
			return;
		}

		StatusDispatcher.print("");
		StatusDispatcher.print("Launching script " + scriptName + "...");

		// The alert messages will be redirected to the terminal (no dialogs)
		Alert.setInteractive(false);

		String className = scriptName;

		Class<?> c = null;

		// Get class
		try {
			c = Class.forName(className);
		}
		catch (ClassNotFoundException t) {
			StatusDispatcher.print("Error in Pilot (): " + scriptName + ": error during script instanciation (constructor). " + t);
			t.printStackTrace(System.out);
			throw t;
		}

		// Get the type
		if (c.getSuperclass() == GScript.class) {
			runGScript(c, pilotArguments);
		}
		else {
			runC4Script(c, pilotArguments);
		}
	}

	/** Execute main 
	 */
	public void runC4Script(Class<?> c, String[] pilotArguments) throws Exception {

		Method mainMethod = c.getMethod("main", new Class[] { String[].class });
		try {

			mainMethod.invoke(null, new Object[] { pilotArguments });
		}
		catch (Exception e) {
			e.printStackTrace();
		}

	}

	/** Execute the script as a GScript 
	 */
	public void runGScript(Class<?> c, String[] pilotArguments) throws Exception {

		GScript script = null;
		// Build object
		try {

			Constructor<?> ctr;
			try {
				// try empty constructor
				ctr = c.getConstructor();
				script = (GScript) ctr.newInstance();
			}
			catch (Exception e) {

				// try constructor with String[]
				ctr = c.getConstructor(new Class[] { String[].class });
				script = (GScript) ctr.newInstance(new Object[] { pilotArguments });
			}
		}
		catch (Exception t) {
			StatusDispatcher.print("Error in Pilot (): " + c.getName() + ": error during script instanciation (constructor). " + t);
			t.printStackTrace(System.out);
			throw t;
		}

		// Run the script
		try {

			script.run(); // it's always a Runnable

		}
		catch (Throwable t) { // we catch everything: Exceptions and Errors
			StatusDispatcher.print("Error in Pilot (): " + c.getName() + ": script error was caught by pilot (run). " + t);
			t.printStackTrace(System.out);
			return;
		}

		StatusDispatcher.print("End of script " + c.getName());
	}

	/**	StatusListener
	 *	In Script mode, StatusDispatcher prints to console.
	 */
	public void print(String msg) {
		if (progressing) {
			System.out.print(" " + msg);
		}
		else {
			System.out.println(msg);
		}
	}

	private boolean progressing = false;

	/**	ProgressListener
	 *	In Script mode, ProgressDispatcher prints to console.
	 */
	public void setMinMax(int min, int max) {
		// MP 060916 (granted by fc): replace System.out by StatusDispatcher --> StatusDispatcher allows muting
		StatusDispatcher.print("[" + min + "->" + max + ":");
		progressing = true;
	}

	/**	ProgressListener
	 *	In Script mode, ProgressDispatcher prints to console.
	 */
	public void setValue(int value) {
		if (value == currentProgressValue) { return; } // fc - 16.5.2006
		// MP 060916 (granted by fc): replace System.out by StatusDispatcher
		StatusDispatcher.print(" " + value);
		currentProgressValue = value;
	}

	/**	ProgressListener
	 *	In Script mode, ProgressDispatcher prints to console.
	 */
	public void stop() {
		progressing = false;
		// MP 060916 (granted by fc): replace System.out by StatusDispatcher
		StatusDispatcher.print("]");
	}

	public void setStatusToConsole(boolean v) {
		if (v) {
			StatusDispatcher.addListener(this);
			// StatusDispatcher.print ("StatusDispatcher was redirected to console");
		}
		else {
			StatusDispatcher.removeListener(this);
		}
	}

	public void setProgressToConsole(boolean v) {
		if (v) {
			ProgressDispatcher.addListener(this);
			// StatusDispatcher.print ("ProgressDispatcher was redirected to console");
		}
		else {
			ProgressDispatcher.removeListener(this);
		}
	}

	/**	Each pilot returns its default relay on demand.
	 * 	This relay will be used to load modules without a specific Relay class.
	 * 	e.g.: for capsis.gui -> capsis.commongui.DefaultRelay
	 */
	@Override
	public Relay getDefaultRelay(GModel model) {
		return new capsis.script.DefaultRelay(model);
	}

}
