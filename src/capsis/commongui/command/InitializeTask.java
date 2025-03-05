/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */

package capsis.commongui.command;

import javax.swing.JFrame;
import javax.swing.SwingUtilities;

import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.kernel.Engine;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**
 * InitializeTask SwingWorker.
 * 
 * @author F. de Coligny - may 2009
 */
public class InitializeTask extends Task<Step, Void> {

	// doInBackground () and get () returns a Step
	// publish () and process () are unused at present time

	protected Project project;
	protected InitialParameters initialParameters;
	protected Step step;
	protected JFrame frame;

	/**
	 * Constructor. We need to pass the dialog to be able to dispose it at the end
	 * of done ().
	 */
	public InitializeTask(String name, String projectName, InitialParameters ip, GModel model, JFrame frame) {
		super(name);
		this.frame = frame;

		this.initialParameters = ip;
		project = Engine.getInstance().processNewProject(projectName, model);

		setIndeterminate();
	}

	/**
	 * doFirstInEDT () is called in the init () method at the very beginning of
	 * doInBackground () and runs in the EDT. Sets some GUI components in waiting
	 * mode
	 */
	protected void doFirstInEDT() {
	} // Do nothing here

	/**
	 * doInBackground () runs in a worker thread. It does the long job apart the
	 * EDT.
	 */
	protected Step doInWorker() {

		// System.out.println ("InitializeTask doInWorker ()...");

		try {
			StatusDispatcher.print(Translator.swap("InitializeTask.initializingProject"));

			step = project.initialize(initialParameters);
			if (step == null) {
				throw new Exception(
						"InitializeTask.doInWorker (): project.initialize () returned a null Step, aborted");
			}
			StatusDispatcher.print(Translator.swap("InitializeTask.done"));

		} catch (final Throwable e) {

			// fc-19.8.2019
			// Check if the exception was caused by a user cancellation
			// If so, log and stop
			Throwable aux = e;
			do {
				if (aux instanceof CancellationException) {
					Log.println(Log.INFO, "InitializeTask.doInWorker ()",
							"Detected a user cancellation, stopped initialization task", e);
					return null; // stop (see doInEDTafterWorker())
				}

				aux = aux.getCause();
			} while (aux != null);
			// fc-19.8.2019

			Log.println(Log.ERROR, "InitializeTask.doInWorker ()", "Error in InitializeTask", e);

			SwingUtilities.invokeLater(new Runnable() {

				public void run() {
					MessageDialog.print(frame, Translator.swap("InitializeTask.errorDuringTaskInitializationSeeLog"),
							e);
				}
			});
		}

		return step;

	}

	/**
	 * doInEDTafterWorker () runs in the EDT when doInWorker () is over.
	 */
	protected void doInEDTafterWorker() {

		try {
			// fc - 31.8.2010 - if big initialization problem (found with
			// Simeo-glpbm)
			if (get() == null) {
				return;
			}

			// Current.getInstance ().setStep (step); fc-25.4.2013 this line was
			// moved below

			// Enable the project level commands
			CommandManager.getInstance().setCommandsEnabled(CommandManager.Level.PROJECT, true);

			// fc-25.4.2013 MOVED this line AFTER setCommandsEnabled () to fix
			// the bug[Simeo: new
			// project Lollymangrove results in an enabled TwistManually command
			// when it should be
			// disabled]
			Current.getInstance().setStep(step);

			// ProjectManager update needed for models creating several steps in
			// initializeModel () (like Phenofit) // fc-3.2.2015
			Project p = Current.getInstance().getProject();

			// fc+bc-10.3.2021 Reviewed: store / restore visibility frequency at model level (default
			// for first opening is model's defaultVisiFreq)
			String modelName = p.getModel().getPackageName();
			int defaultVisiFreq = p.getModel().getDefaultStepVisibilityFreq();
			int f = Settings.getProperty("project.visibility.frequency.for." + modelName, defaultVisiFreq);
			p.setVisibilityFrequency(f);
//			pro.setVisibilityFrequency(10); // was 10

			ProjectManager.getInstance().update();
			// ProjectManager update needed for models creating several steps in
			// initializeModel () (like Phenofit) // fc-3.2.2015

			StatusDispatcher.print(Translator.swap("InitializeTask.ready"));

		} catch (Throwable e) { // Catch Errors in every command (for
								// OutOfMemory)
			Log.println(Log.ERROR, "InitializeTask.doInEDTafterWorker ()", "An Exception/Error occured", e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			MessageDialog.print(frame, Translator.swap("Shared.commandFailed"), e);

		} finally {

		}

	}

	/*
	 * } catch (final InterruptedException e) { System.out.println
	 * ("InitializeTask was interrupted");
	 * 
	 * SwingUtilities.invokeLater (new Runnable () { public void run () { //
	 * InitializeTask is stopped, restore disabled actions enableGUI (true);
	 * StatusDispatcher.print (Translator.swap
	 * ("Pilot.interruptionWhileProcessEvolution")); MessageDialog.promptError
	 * (Translator.swap ("Pilot.interruptionWhileProcessEvolution"), e); } });
	 * return null;
	 * 
	 * } catch (final Throwable e) { Log.println (Log.ERROR,
	 * "InitializeTask.doInBackground ()", "Exception/Error in doInBackground ()",
	 * e);
	 * 
	 * SwingUtilities.invokeLater (new Runnable () { public void run () { //
	 * InitializeTask is stopped, restore disabled actions enableGUI (true);
	 * StatusDispatcher.print (Translator.swap
	 * ("Pilot.errorWhileProcessEvolution")); MessageDialog.promptError
	 * (Translator.swap ("Pilot.errorWhileProcessEvolution"), e); } }); return null;
	 * }
	 */

}
