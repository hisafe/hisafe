/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2015 INRA
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
 */
package capsis.run;

import java.io.FileInputStream;
import java.lang.reflect.Method;
import java.util.Properties;
import java.util.TreeSet;

import jeeb.lib.util.Alert;
import jeeb.lib.util.Log;
import capsis.kernel.AbstractPilot;
import capsis.kernel.GModel;
import jeeb.lib.util.PathManager;
import capsis.kernel.Relay;

/**
 * Pilot for run mode. This mode allows to run the main() method of a tool in
 * the Capsis ecosystem. It allows to use the main multi-os scripts capsis.sh /
 * capsis.bat to run tools more easily than with java -cp class:ext/*
 * package.name.MainClass under Linux, Mac or Windows.
 * 
 * @author F. de Coligny - March 2015
 */
public class Pilot extends AbstractPilot {

	static private Pilot instance; // it's not a Singleton pattern
	private final String[] pilotArguments;

	/**
	 * Used to get an instance of Pilot. Note: the constructor must have been
	 * called once before.
	 */
	static public Pilot getInstance() { // this is not exactly a Singleton
										// pattern
		if (instance == null) {
			throw new Error(
					"Design error: capsis.run.Pilot constructor must be called once before calling getInstance (), aborted.");
		}
		return instance;
	}

	/**
	 * Constructor
	 */
	public Pilot(String[] pilotArguments) throws Throwable {
		// This constructor can only be called once
		if (instance != null) {
			throw new Error("Design error, capsis.run.Pilot constructor must be called only once, stopped");
		}
		instance = this;

		this.pilotArguments = pilotArguments;

	}

	/**
	 * Starts the pilot
	 */
	@Override
	public void start() throws Exception {

		// Default: the alert messages will be redirected to the terminal (no
		// dialogs)
		Alert.setInteractive(false); // Can be set to true in the app main()
										// method if theapp has a gui

		String appName;

		// 2. Retrieve application class name
		try {
			appName = pilotArguments[0].trim();
		} catch (Exception e) {
			System.out.println("Error in Pilot (): could not start Application: app name not found");
			return;
		}

		// Some tests
		if (appName.length() == 0) {
			System.out.println("Error in Pilot (): could not start Application: app name must be provided");
			return;
		}
		
		// Load the app classNames file
		String appFileName = PathManager.getDir("etc") + "/capsis.apps";
		Properties props = loadApps(appFileName);

		if (appName.equals("list")) {
			printAppList (appFileName, props);
			return;
		}

		String className = props.getProperty(appName);

		if (className == null) {
			System.out.println("Error in Pilot (): could not find app: "+appName+"\n-> please check available apps in "+appFileName);
			return;
		}
		
		System.out.println("");
		System.out.println("Launching app " + appName + ": " + className + "...");

		Class<?> c = null;

		// Get class
		try {
			c = Class.forName(className);
		} catch (ClassNotFoundException t) {
			System.out.println("Run pilot could not find the class for: " + appName);
			throw t;
		}

		// Run the app
		runApp(c, pilotArguments);

	}
	
	private void printAppList (String appFileName, Properties props) {
		System.out.println();
		System.out.println("Available apps in "+appFileName);
		for (Object name : new TreeSet (props.keySet())) {
			String app = (String) name;
			String className = props.getProperty(app);
			System.out.println("  - "+app+": "+className);
			
		}
		System.out.println();
	}
	
	/**
	 * Reads the given app file. The key is an app short name (e.g.
	 * QuestKnotViewer) and the value is the matching class fully qualified
	 * className (e.g. capsis.lib.quest.knotviewer.QuestKnotViewer).
	 */
	private Properties loadApps(String appFileName) throws Exception {

		Properties properties = new Properties();
		try {
			properties.load(new FileInputStream(appFileName));
			return properties;

		} catch (Exception e) {
			Log.println(Log.ERROR, "Pilot.loadApps()", "Could not load apps file: " + appFileName, e);
			throw new Error("Pilot: aborted (see Log), could not load apps file: " + appFileName);
		}

	}

	/**
	 * Execute the app main() method
	 */
	public void runApp(Class<?> c, String[] pilotArguments) throws Exception {

		Method mainMethod = c.getMethod("main", new Class[] { String[].class });

		try {
			mainMethod.invoke(null, new Object[] { pilotArguments });

		} catch (Exception e) {
			e.printStackTrace();

		}

	}

	// /**
	// * Execute the script as a GScript
	// */
	// public void runGScript(Class<?> c, String[] pilotArguments) throws
	// Exception {
	//
	// GScript script = null;
	// // Build object
	// try {
	//
	// Constructor<?> ctr;
	// try {
	// // try empty constructor
	// ctr = c.getConstructor();
	// script = (GScript) ctr.newInstance();
	// } catch (Exception e) {
	//
	// // try constructor with String[]
	// ctr = c.getConstructor(new Class[] { String[].class });
	// script = (GScript) ctr.newInstance(new Object[] { pilotArguments });
	// }
	// } catch (Exception t) {
	// StatusDispatcher.print("Error in Pilot (): " + c.getName()
	// + ": error during script instanciation (constructor). " + t);
	// t.printStackTrace(System.out);
	// throw t;
	// }
	//
	// // Run the script
	// try {
	//
	// script.run(); // it's always a Runnable
	//
	// } catch (Throwable t) { // we catch everything: Exceptions and Errors
	// StatusDispatcher.print("Error in Pilot (): " + c.getName() +
	// ": script error was caught by pilot (run). "
	// + t);
	// t.printStackTrace(System.out);
	// return;
	// }
	//
	// StatusDispatcher.print("End of script " + c.getName());
	// }

	/**
	 * Each pilot returns its default relay on demand. This relay will be used
	 * to load modules without a specific Relay class. e.g.: for capsis.gui ->
	 * capsis.commongui.DefaultRelay
	 */
	@Override
	public Relay getDefaultRelay(GModel model) {
		// This specific pilot is only used to start a main() method, it does
		// not provide the relay feature needed to run models in Capsis.
		return null;
		// return new capsis.run.DefaultRelay(model);
	}

}
