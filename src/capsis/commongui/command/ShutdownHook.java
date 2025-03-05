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

package capsis.commongui.command;

import java.io.File;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JFrame;

import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.UserDialog;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;


/**	In case of unexpected application shutdown in gui mode, this class tries 
*	to save the session before closing.
*	Also contains methods to reload a rescued session at application launch time.
*	@author F. de Coligny - november 2007, may 2010
*/
public class ShutdownHook extends Thread {
	
	private static ShutdownHook instance; 
		
	private String applicationDir;
	private String rescueDir;
	private String rescueSessionFileName;
	private JFrame frame;
	
	
	/**	Kind of Singleton pattern (needs one call to the constructor).
	 * 	If the constructor was not called once, returns null.
	 */
	public static ShutdownHook getInstance () {  // Singleton pattern
		return instance;
	}
	
	/**	Constructor .
	 * 	Must be called once, then use ShutdownHook.getInstance ().
	 */
	public ShutdownHook (JFrame frame, String applicationName) {
		ShutdownHook.instance = this; 
		
		this.frame = frame;
		
		// Application directory (in user home directory)
		applicationDir = System.getProperty ("user.home")
				+ File.separator
				+ "." + applicationName;
		
		// Rescue session directory (in application directory)
		this.rescueDir = applicationDir
				+ File.separator
				+ "rescue-session";
		
		// Ensure the directories exist or create them
		checkdirectories ();
				
		this.rescueSessionFileName = rescueDir
				+ File.separator
				+ "session";
		
		// Set the hook here
		Runtime.getRuntime ().addShutdownHook (this);
		
	}	
	
	/** Ensure the directories exist or create them
	 */
	private void checkdirectories () {
		// Check applicationDir
		File dir = new File (applicationDir);
		if (!dir.exists ()) {
			boolean b = dir.mkdir ();
		}
		
		// Check rescueDir
		dir = new File (rescueDir);
		if (!dir.exists ()) {
			boolean b = dir.mkdir ();
		}
	}
	
	/**	If the application is interrupted, this method will be run by Runtime.
	*/
	public void run () {
		
		if(Settings.getProperty("crash.recovery", true) == false) { return; }
		// Save main properties file
		try {
			Settings.savePropertyFile ();
		} catch (Exception e1) {
			// In case of trouble: do nothing
		}
		
		// Save rescue session
		try {
			saveRescueSession ();
		} catch (Exception e) {
			// In case of trouble: do nothing
		}
		
	}
	
	/**	When closing the application normally, call this method before System.exit ()
	*/
	public void removeShutdownHook () {
		Runtime.getRuntime ().removeShutdownHook (instance);
	}
	
	/**	Try to detect if the application crashed last time by searching a 
	*	rescue file.
	*/
	public boolean applicationEndedAbnormally () {
		return new File (rescueSessionFileName).exists ();
	}

	/**	Disable the rescueSession when restored or if the user does not 
	* 	choose to restore it (by deleting it).
	*/
	public void disableRescueSession () {
		
		// Delete all files in recueDir, then delete rescueDir
		File dir = new File (rescueDir);
		
		File[] files = dir.listFiles();
		for (File f : files) {
			f.delete ();
		}
		
		dir.delete ();
		
	}

	/**	Save the rescueSession
	*/
	public void saveRescueSession () throws Exception {
		Session session = Engine.getInstance ().getSession ();
		
	
		print (Translator.swap ("ShutdownHook.interruptionSavingSessionInDir")
				+ " " + rescueDir + "...");
		boolean oneProjectWasNotSaved = false;
		for (Project p : session.getProjects ()) {
			
			if (!p.isSaved ()) {
				oneProjectWasNotSaved = true;
				String fileName = rescueDir
						+File.separator
						+p.getName ();
				p.setFileName (fileName);
				
				checkdirectories ();  // needed
				
				Engine.getInstance ().processSaveAsProject (p, fileName);							
			}
			
		} 

		// If all the projects were saved, no rescue session needed
		if (oneProjectWasNotSaved) {
			Engine.getInstance ().processSaveAsSession (rescueSessionFileName);
		}
		
	}

	/**	Reload the rescueSession
	*/
	public void restoreRescueSession () {
		// Load the rescueSession
		try {
			Log.println ("restoring rescue session... "+rescueSessionFileName);
			
			// Will update the project manager
			new capsis.commongui.command.OpenSession (frame, rescueSessionFileName).execute ();
			
			// The projects in the just rescued session are not considered saved
			for (Project p : Engine.getInstance().getSession().getProjects()) {
				p.setWasSaved(false);
				p.setSaved(false);
				p.setFileName("");
			}
			
			Log.println ("done");

		} catch (Exception e) {
			Log.println (Log.ERROR, "ShutdownHook.restoreRescueSession ()", 
					"Exception while restoring rescueSession", e);
			MessageDialog.print (frame, Translator.swap ("ShutdownHook.errorDuringRescueSessionRestorationSeeLog"));
		}
	}
	
	/**	If a rescue session exists, ask the user if he want it to be restored.
	*	If so, restore it. In any case, delete the rescue session for good 
	*	manageent next time.
	*/
	public void proposeToRestoreRescueSessionIfAny () {
		
		if (applicationEndedAbnormally ()) {
			JButton restoreRescueSession = new JButton (Translator.swap ("ShutdownHook.restoreRescueSession"));
			JButton ignore = new JButton (Translator.swap ("ShutdownHook.ignore"));
			Vector<JButton> buttons = new Vector<JButton> ();
			buttons.add (restoreRescueSession);
			buttons.add (ignore);

			JButton choice = UserDialog.promptUser (frame, Translator.swap ("ShutdownHook.restoreRescueSession"), 
					Translator.swap ("ShutdownHook.thePreviousSessionSeemsToHaveEndedAbnormallyDoYouWantToRestoreTheRescueSession")+" ?", 
					buttons, restoreRescueSession);
			if (choice != null && choice.equals (restoreRescueSession)) {
				restoreRescueSession ();
			}
			
			// In all cases, once the choice was made, delete the rescue session
			disableRescueSession ();
			
			
		}

	}

	/**	Local print method: in terminal and in status bar
	 */
	private void print (String message) {
		System.out.println (message);	// print in terminal
		final String m = message;
		javax.swing.SwingUtilities.invokeLater (new Runnable () {
			public void run () {
				StatusDispatcher.print (m);	// print in main frame status bar
			}
		});
	}

}
