/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.automation;


import jeeb.lib.util.ProgressListener;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.StatusListener;
//import capsis.app.CapsisSettings;
import capsis.kernel.AbstractPilot;
import capsis.kernel.GModel;
import capsis.kernel.Relay;
import capsis.kernel.automation.Automation;
import capsis.util.AutomationRunner;

/**
 * Pilot for automation 
 * Multi threaded automation runner
 * 
 * @author S. Dufour - 
 */
public class Pilot extends AbstractPilot implements StatusListener, ProgressListener {

	static private Pilot instance;			// it's not a Singleton pattern
	private String scriptName;
	private String[] scriptArgs;
	private int nbthread = 2;
	private String summaryFileName;
	
	
	
	/**	Used to get an instance of Pilot.
	 * 	Note: the constructor must have been called once before.
	 */
	static public Pilot getInstance () {  // this is not exactly a Singleton pattern
		if (instance == null) {
			throw new Error ("Design error: capsis.automation.Pilot constructor must be called once before calling getInstance (), aborted.");
		}
		return instance;
	}

	/**	Constructor	
	 */
	public Pilot (String[] pilotArguments) throws Throwable {
		// This constructor can only be called once
		if (instance != null) {
			throw new Error ("Design error, capsis.automation.Pilot constructor must be called only once, stopped");
		}			
		instance = this;
		
		init (pilotArguments);
	}

//	/** Constructor 
//	 * @throws Throwable */
//	public Pilot () throws Exception {
//				 
//		init(getScriptArgsFromSettings());
//	}
//	
//	public Pilot (String name) throws Exception {
//		 
//		String[] args = {name};
//		init(args);
//	}
	
	
	protected void init(String[] args) throws Exception {
		
		// console output
		StatusDispatcher.addListener (this);
		
		scriptArgs = args;
		
		// 2. Retrieve script class name
		try {
			scriptName = scriptArgs[0].trim ();
		} catch (Exception e) {
			
			throw new Exception ("Error in Pilot (): File name not found, can not start script");
			
		}
		
		if (scriptName.length () == 0) {
			throw new Exception ("Error in Pilot (): File name is empty, can not start script");
			
		}
		
		// try to get the number of thread
		if(scriptArgs.length > 1) {
			try {
				nbthread = new Integer(scriptArgs[1]);
				
			} catch(Exception e) {}
			StatusDispatcher.print ("Number of threads : " + nbthread);
		}
		
		// try to get the output filename
		if(scriptArgs.length > 2) {
			try {
				summaryFileName = scriptArgs[2];
				
			} catch(Exception e) {}
			StatusDispatcher.print ("Summary filename : " + summaryFileName);
		}
		
//		// 6. Starts the Capsis application engine (Singleton pattern)
//		// Engine retrieves the settings by Options.getInstance ().
//		Options.getInstance ();
//		StatusDispatcher.print ("");
		
		
		
	}
	
	@Override
	public void start() throws Exception {
		
		try{
			Automation automation = Automation.buildFromXML(scriptName);
			AutomationRunner.runInTread(automation, nbthread, summaryFileName);
			
		} catch(Exception e) {
			StatusDispatcher.print ("ERROR : cannot load file " + scriptArgs[0] + ". Simulation aborded.");
			StatusDispatcher.print(e.toString());
			return;
		}

		

	}
	
	

	/**
	 * From StatusListener interface.
	 * In Script mode, StatusDispatcher prints to console.
	 */
	@Override
	public void print (String msg) {
		System.out.println(msg);
	}

	
	
	/**
	 * From ProgressListener interface.
	 * In Script mode, ProgressDispatcher prints to console.
	 */
	@Override
	public void setMinMax (int min, int max) {
		
	}

	@Override
	public void setValue(int value) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void stop() {
		
	}

	
	/**	Each pilot returns its default relay on demand.
	 * 	This relay will be used to load modules without a specific Relay class.
	 * 	e.g.: for capsis.gui -> capsis.commongui.DefaultRelay
	 */
	@Override
	public Relay getDefaultRelay(GModel model) {
		return new capsis.script.DefaultRelay (model);
	}

	
	
}


