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

import java.util.concurrent.CancellationException;

import javax.swing.JFrame;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.CancellableTask;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**
 * EvolutionTask runs an evolution stage in a task.
 * 
 * @author F. de Coligny - april 2009
 */
public class EvolutionTask extends CancellableTask<Step, Void> {

	// doInBackground () and get () returns a Step
	// publish () and process () are unused at present time

	private Step fromStep;
	private EvolutionParameters evolutionParameters;
	private Project project;
	private ProjectManager scenarioManager;
	private CommandManager commandManager;
	private JFrame frame;

	// private boolean memoDateCorrected;

	/**
	 * Constructor.
	 */
	public EvolutionTask(String name, Step fromStep, EvolutionParameters evolutionParameters, Project project,
			ProjectManager scenarioManager, CommandManager commandManager, JFrame frame) {

		super(name);

		this.fromStep = fromStep;
		this.evolutionParameters = evolutionParameters;
		this.project = project;
		this.scenarioManager = scenarioManager;
		this.commandManager = commandManager;
		this.frame = frame;
	}

	/**
	 * doFirstInEDT () is called in the init () method at the very beginning of
	 * doInBackground () and runs in the EDT. Sets some GUI components in
	 * waiting mode
	 */
	protected void doFirstInEDT() {
		StatusDispatcher.print(Translator.swap("EvolutionTask.processingEvolution"));

		enableGUI(false);
	}

	// Must be called in the event-dispatching thread
	private void enableGUI(boolean v) {

		// Enable / disable some actions during evolution
		commandManager.setCommandsEnabled(CommandManager.Level.PROJECT, v);

		// busy was removed from Project
		// fromStep.getProject ().setBusy (!v); // enable=false -> busy=true

	}

	/**
	 * doInWorker () runs in a worker thread. It does the long job apart the
	 * EDT.
	 */
	protected Step doInWorker() throws InterruptedException {

		// WAS REMOVED to remove a dependence to DateCorrectable in capsis
		// // Memorize date correction / turn it off now
		// memoDateCorrected = false;
		// try {
		// DateCorrectable std = (DateCorrectable) fromStep.getScene ();
		// memoDateCorrected = (std.isDateCorrected () &&
		// std.isDateCorrectionEnabled ());
		// if (memoDateCorrected) {std.setDateCorrected (false);}
		//
		// } catch (Exception e) {}

		// Run processEvolution ()
		try {
			// StatusDispatcher.print (Translator.swap
			// ("EvolutionTask.processingEvolution"));

			Step step = project.evolve(fromStep, evolutionParameters);

			if (isCancelled()) {
				StatusDispatcher.print("EvolutionTask doInBackground () [***] cancelled, step=" + step);
			}

			return step;

		} catch (InterruptedException e) {
			throw e; // interruption

			// fc-24.11.2017 suspecting an Error instead of an Exception,
			// catching all possible Throwables
		} catch (Throwable t) {
			// } catch (Exception e) {
			Log.println(Log.ERROR, "EvolutionTask.doInBackground ()", "Error", t);
			MessageDialog.print(frame, Translator.swap("EvolutionTask.errorWhileProcessingEvolutionSeeLog"), t);
			return null; // error

		}

	}

	/**
	 * doInEDTafterWorker () runs in the Event Dispatch Thread when the worker
	 * thread is done. Update the GUI when normal termination.
	 */
	@Override
	protected void doInEDTafterWorker() throws CancellationException {

		try {
			Step newStep = (Step) get();

			// Check if background task finished normally
			if (newStep == null) {
				Log.println(Log.WARNING, "EvolutionTask.doInEDTafterWorker ()",
						"processEvolution returned a null step, aborted");
				StatusDispatcher.print(Translator.swap("EvolutionTask.errorWhileProcessingEvolutionSeeLog"));
				// The MessageDialog has already been opened at the end of
				// doInWorker ()
				// This would open 2 messages
				// MessageDialog.print (frame, Translator.swap
				// ("Pilot.errorWhileProcessingEvolutionSeeLog"));

				return;
			}

			// WAS REMOVED to remove a dependence to DateCorrectable in capsis
			// // Restore date correction if needed (see upper)
			// if (newStep.getScene () instanceof DateCorrectable) { // fc -
			// 23.11.2004
			// DateCorrectable std = (DateCorrectable) newStep.getScene ();
			// std.setDateCorrected (memoDateCorrected);
			// }

			// ProjectManager listens to Current and will update
			Current.getInstance().setStep(newStep);

			// This synchronizes the tools of the last colored StepButton on the
			// new stepButton
			StepButton sb = ProjectManager.getInstance().getStepButton(newStep);
			ButtonColorer.getInstance().moveColor(sb);
			
			// fc-4.10.2021 Ensure the final stepButton is visible at the end
			sb.scrollRectToVisible(sb.getBounds());
			
			StatusDispatcher.print(Translator.swap("EvolutionTask.evolutionIsOver"));

		} catch (CancellationException e) {
			throw e;

		} catch (Exception e) {
			Log.println(Log.ERROR, "EvolutionTask.done ()", "Error during evolution", e);
			StatusDispatcher.print(Translator.swap("EvolutionTask.errorWhileProcessingEvolutionSeeLog"));
			MessageDialog.print(frame, Translator.swap("EvolutionTask.errorWhileProcessingEvolutionSeeLog"), e);

		} finally {
			// In all cases (normal end, error, cancelled), restore disabled
			// actions
			enableGUI(true);

		}

	}

	/**
	 * In case of cancellation, doInEDTifCancelled () runs in the Event Dispatch
	 * Thread when the worker thread is done. Update the GUI at the end of a
	 * task cancellation.
	 */
	protected void doInEDTifCancelled() {

		// TRACE
		System.out.println("EvolutionTask doInEDTifCancelled ()");

		// Update the GUI
		fromStep.getProject().updateVisibility(true);
		enableGUI(true);

		// fc-28.5.2014 In case of cancellation, the project manager must be
		// refreshed to
		// show the steps calculated so far
		ProjectManager.getInstance().update();
		System.out.println("EvolutionTask updated the ProjectManager.");

	}

}
