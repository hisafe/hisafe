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

import javax.swing.SwingUtilities;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.kernel.DateCorrectable;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;

/**	InterventionTask applies an intervener in a SwingWorker.
 *	@author F. de Coligny - may 2009, october 2010
 */
public class InterventionTask extends Task<Step, Void> {
	// doInBackground () and get () returns a Step
	// publish () and process () are unused at present time

	private Step step;

	private boolean memoDateCorrected;
	private Intervener intervener;
	
	// got rid of this field, see Genotype static initializer
//	private Step memoLastKnownStep;

	
	
	/**	Constructor.
	 */
	public InterventionTask (String name, 
			Step step, Intervener intervener) {
		super (name);

		this.intervener = intervener;
		this.step = step;

		setIndeterminate ();
	}

	/**	doFirstInEDT () is called in the init () method at the very beginning 
	 *	of doInBackground () and runs in the EDT.
	 *	Sets some GUI components in waiting mode
	 */
	protected void doFirstInEDT () {
		
		StatusDispatcher.print (Translator.swap ("Intervention.processingIntervention"));

		// fc + jl - 19.5.2005 - An intervener with genetics considerations
		// is ran on the root step.
//		memoLastKnownStep = GeneticTools.lastKnownStep;
//		GeneticTools.lastKnownStep = step;

		// Memorize date correction / turn it off now
		boolean memoDateCorrected = false;
		try {
			DateCorrectable std = (DateCorrectable) step.getScene ();
			memoDateCorrected = (std.isDateCorrected () && std.isDateCorrectionEnabled ());
			if (memoDateCorrected) {std.setDateCorrected (false);}
		} catch (Exception e) {}


	}	// Do nothing here

	/**	doInBackground () runs in a worker thread.
	 *	It does the long job apart the EDT.
	 *	init () must be called at the beginning of Task.doInBackground ()
	 */
	protected Step doInWorker () {

		System.out.println ("InterventionTask doInBackground ()...");

		// Cut only if ok chosen and parameters checked correct
		if (!intervener.isReadyToApply ()) {
			StatusDispatcher.print (Translator.swap (
			"Intervention.wrongIntervenerParameteringNotReadyToApply"));
			return null;
		}

		try {
//			StatusDispatcher.print (Translator.swap ("Intervention.processingIntervention"));

			step = step.getProject().intervention(step, intervener, true);

			StatusDispatcher.print (Translator.swap ("Intervention.done"));
		} catch (final Throwable t) {
			Log.println (Log.WARNING, "Intervention.execute ()",
					"Exception while applying intervener "+intervener
					+" on stand "+ step.getScene(), t);

			SwingUtilities.invokeLater (new Runnable () {
				public void run () {
					MessageDialog.print (this, Translator.swap ("InterventionTask.anErrorOccurredDuringInterventionSeeLog"), t);
				}
			});

			return null;
		}


		return step;	// no return value

	}		

	/**	doInEDTafterWorker () runs in the EDT when doInWorker () is over.
	 */
	protected void doInEDTafterWorker () {	

		try {

			// Restore lastKnownStep
//			GeneticTools.lastKnownStep = memoLastKnownStep;

			// Restore date correction if needed (see upper)
			try {
				DateCorrectable std = (DateCorrectable) step.getScene ();
				std.setDateCorrected (memoDateCorrected);
			} catch (Exception e) {}

			// ProjectManager listens to Current and will update
			Current.getInstance ().setStep (step);
			
			// This synchronizes the tools of the last colored StepButton on the new stepButton
			StepButton sb = ProjectManager.getInstance ().getStepButton(step);
			ButtonColorer.getInstance ().moveColor (sb);
			
			// fc-4.10.2021 Ensure the final stepButton is visible at the end
			sb.scrollRectToVisible(sb.getBounds());
			
			StatusDispatcher.print (Translator.swap ("InterventionTask.interventionIsOver"));

		} catch (Throwable e) {		// fc - 30.7.2004 - catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "InterventionTask.done ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("InterventionTask.anErrorOccurredAtTheEndOfTheInterventionProcessSeeLog"));
			MessageDialog.print (this, Translator.swap ("InterventionTask.anErrorOccurredAtTheEndOfTheInterventionProcessSeeLog"), e);
		} finally {
			//interventionDialog.dispose ();	// In any case
		}
	}




}
