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

import javax.swing.JFrame;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.kernel.AbstractSettings;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**	ReinitialiseTask SwingWorker.
*	This task calls the constructor and doInWorker () method of InitialiseTask, 
*	then does something different in doInEDTafterWorker ().
*	@author F. de Coligny - may 2010
*/
public class ReinitialiseTask extends InitializeTask {
	// doInBackground () and get () returns a Step
	// publish () and process () are unused at present time

	private Project sourceProject;  // The project to be replaced
	
	
	/**	Constructor.
	*/
	public ReinitialiseTask (String name, String projectName, InitialParameters ip, GModel model, JFrame frame, 
			Project sourceProject) {
		super (name, projectName, ip, model, frame);
		this.sourceProject = sourceProject;
	}

	/**	doFirstInEDT () is called in the init () method at the very beginning 
	*	of doInBackground () and runs in the EDT.
	*	Sets some GUI components in waiting mode
	*/
//	protected void doFirstInEDT () {}	// Do nothing here
	
	/**	doInBackground () runs in a worker thread.
	*	It does the long job apart the EDT.
	*/
//	protected Step doInWorker () {
//		
//		System.out.println ("ReinitialiseTask doInBackground ()...");
//
//		try {
//			StatusDispatcher.print (Translator.swap ("ReinitialiseTask.initializingProject"));
//			
//			step = project.initialize (initialParameters);
//			StatusDispatcher.print (Translator.swap ("ReinitialiseTask.done"));
//			
//		} catch (final Throwable e) {
//			Log.println (Log.ERROR, "ReinitialiseTask.doInBackground ()", "Error in ReinitialiseTask", e);
//			
//			SwingUtilities.invokeLater (new Runnable () {
//				public void run () {
//					MessageDialog.print (frame, Translator.swap ("ReinitialiseTask.errorDuringTaskInitializationSeeLog"), e);
//				}
//			});
//		}
//		
//		return step;
//
//	}		
	
	/**	doInEDTafterWorker () runs in the EDT when doInWorker () is over.
	*/
	protected void doInEDTafterWorker () {	
		
		try {
			System.out.println ("ReinitialiseTask.done ()...");
			
			// Try to move the viewers from the source project root step to the new one
			// NOT EASY to move a color from a project to another - TODO
//			try {
//				Step sourceRoot = (Step) sourceProject.getRoot();
//				StepButton sb = ProjectManager.getInstance().getStepButton(sourceRoot);
//				
//				Step newRoot = (Step) project.getRoot();
//				StepButton newSb = ProjectManager.getInstance().getStepButton(newRoot);
//			
//				// Testing refs chaining
//				AbstractSettings s1= sourceRoot.getScene().getStep().getProject().getModel().getSettings(); 
//				System.out.println("ReinitialiseTask sourceRoot = "+sourceRoot);
//				AbstractSettings s2 = newRoot.getScene().getStep().getProject().getModel().getSettings(); 
//				System.out.println("ReinitialiseTask newRoot    = "+sourceRoot);
//				
//				
//				
//				if (sb.isColored()) {
//					ButtonColorer.getInstance().tellListenersColorMoved (sb, newSb);
//				}
//				
//			} catch (Exception e) {
//				// If trouble, do nothing, does not matter
//			}
			
			// Close the project to be replaced
			Engine.getInstance().processCloseProject(sourceProject);
			
//			Current.getInstance ().setStep (step); fc-25.4.2013 this line was moved below
			
			// Enable the project level commands
			CommandManager.getInstance ().setCommandsEnabled(CommandManager.Level.PROJECT, true);
			
			// fc-25.4.2013 MOVED this line AFTER setCommandsEnabled () to fix the bug[Simeo: new
			// project Lollymangrove results in an enabled TwistManually command when it should be
			// disabled]
			Current.getInstance ().setStep (step);
			
			CommandManager.getInstance ().updateFrameTitle ();
			
			StatusDispatcher.print (Translator.swap ("ReinitialiseTask.ready"));
			
		} catch (Throwable e) {		// Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "ReinitialiseTask.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			
		} finally {
			
			
		}			

	}


/*
		} catch (final InterruptedException e) {
			System.out.println ("ReinitialiseTask was interrupted");
			
			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					// ReinitialiseTask is stopped, restore disabled actions
					enableGUI (true);
					StatusDispatcher.print (Translator.swap ("Pilot.interruptionWhileProcessEvolution"));
					MessageDialog.promptError (Translator.swap ("Pilot.interruptionWhileProcessEvolution"), e);
				}
			});
			return null;
			
		} catch (final Throwable e) {
			Log.println (Log.ERROR, "ReinitialiseTask.doInBackground ()", 
					"Exception/Error in doInBackground ()", e);
			
			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					// ReinitialiseTask is stopped, restore disabled actions
					enableGUI (true);
					StatusDispatcher.print (Translator.swap ("Pilot.errorWhileProcessEvolution"));
					MessageDialog.promptError (Translator.swap ("Pilot.errorWhileProcessEvolution"), e);
				}
			});
			return null;
		}

*/
	
	
}