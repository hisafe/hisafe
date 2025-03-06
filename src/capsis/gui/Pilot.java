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

package capsis.gui;

import java.awt.event.KeyEvent;
import java.io.IOException;
import java.lang.reflect.Constructor;

import javax.swing.UIManager;

import capsis.commongui.util.Helper;
import capsis.extension.generictool.assistant.TranslationAssistant;
import capsis.kernel.AbstractPilot;
import capsis.kernel.GModel;
import capsis.kernel.Relay;
import jeeb.lib.util.Alert;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**	Pilot for interactive "gui" mode.
*	@author F. de Coligny - september 2001
*/
public class Pilot extends AbstractPilot {	
	
	static private Pilot instance;
	static private MainFrame mainFrame;
	static private Positioner positioner;



	/**	Used to get an instance of Pilot.
	 * 	Note: the constructor must have been called once before.
	 */
	static public Pilot getInstance () {  // this is not exactly a Singleton pattern
		if (instance == null) {
			throw new Error ("Design error: capsis.gui.Pilot constructor must be called once before calling getInstance (), aborted.");
		}
		return instance;
	}
	
	/**	Constructor.
	 */
	public Pilot (String[] pilotArguments) throws Throwable {
		// This constructor can only be called once
		if (instance != null) {
			throw new Error ("Design error, capsis.gui.Pilot constructor must be called only once, stopped");
		}			
		instance = this;
		
		// capsis.gui.Pilot needs no arguments
	}
	
	/**	Starts the pilot.
	 */
	@Override
	public void start()  throws Exception {
		// Schedule a job for the event-dispatching thread:
        // creating and showing this application's GUI.
		javax.swing.SwingUtilities.invokeLater (new Runnable () {
            public void run () {
                startPilot ();
            }
        });
		
	}

	/**	This method must be called in the even-dispatching thread.
	 */
	private void startPilot () {
		// Please keep this trace
		System.out.println ("Gui Pilot booting...");
	
		Alert.setInteractive (true);	// Alerts will appear in Dialogs
		
		// Select system Look & Feel
		try {
			UIManager.setLookAndFeel (UIManager.getSystemLookAndFeelClassName ());
		} catch (Exception exc) {
			System.out.println ("Error in Pilot (): Can not load Look & Feel: " + exc);
		}
	
		// This was moved to Engine
//		// Initialise Translator
//		try {
//			Translator.initActiveLexicon (Locale.getDefault ());
//		} catch (Exception e) {}
		
		Translator.addSystemBundle ("capsis.Labels");
		
		// fc-7.8.2018 DESACTIVATED the offline help downloading, zip is no more updated
//		// Download help files
//		try {
//			
//			// fc-7.8.2018 added destinationDirectory
//			String destinationDirectory = PathManager.getInstallDir() + "/doc"; // "/capsis_help/";
//			Helper.downloadOfflineData (destinationDirectory, true); // force is true while debugging
//			
//		} catch (IOException e) {
//			Log.println ("Cannot load help file : " + e);
//		}
		
		// Create the main frame
		mainFrame = MainFrame.getInstance ();
		
		// init positionner
		String pstr = Settings.getProperty ("capsis.positioner", "");
		try {
			Class<?> cl = getClass().getClassLoader().loadClass(pstr);
			Constructor<?> ctr = cl.getConstructor(MainFrame.class);
			positioner = (Positioner) ctr.newInstance(mainFrame);
		} catch (Exception e) {
			Log.println(Log.WARNING, "Pilot", "cannot load previous positioner" , e);
			positioner = new ReportPositioner (mainFrame);	// default
		}
		
		// Add a listener on the AmapDialogs to open the TranslationAssistant on f2
		class F2Listener implements Listener {
			public void somethingHappened (ListenedTo l, Object param) {
				TranslationAssistant d = new TranslationAssistant (mainFrame, l.getClass ().getName ());
				d.dispose();
				
			}
		}
		AmapDialog.addKeyListener (new F2Listener (), KeyEvent.VK_F2);
		
		// getting rid of the Positioners
//		// Add a listener on the AmapDialogs to tell the positioner when they are set visible
//		class VisibleListener implements Listener {
//			public void somethingHappened (ListenedTo l, Object param) {
//				Positioner.layoutDialog ((JDialog) l);
//			}
//		}
//		AmapDialog.addKeyListener (new VisibleListener (), AmapDialog.SET_VISIBLE_TRUE);
		
		// Init the main frame
		mainFrame.init ();		

		
	}

	static public MainFrame getMainFrame () 				{return mainFrame;}
	
	static public Positioner getPositioner () 				{return positioner;}
	
	static public void setPositioner (Positioner pos)			{positioner = pos;}
	
	
	/**	Each pilot returns its default relay on demand.
	 * 	This relay will be used to load modules without a specific Relay class.
	 * 	e.g.: for capsis.gui -> capsis.commongui.DefaultRelay
	 */
	@Override
	public Relay getDefaultRelay(GModel model) {
		return new capsis.commongui.DefaultRelay (model);
	}
	

}


