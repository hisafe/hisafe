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

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.kernel.Project;
import capsis.kernel.Step;

/**
 * Command DeleteStep.
 * 
 * @author F. de Coligny - October 2000, April 2010
 */
public class DeleteStep extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("DeleteStep.deleteStep");

	private JFrame frame;

	/**
	 * Constructor
	 */
	public DeleteStep (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("delete-step_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("delete-step_24.png"));
		// fc-1.10.2012 reviewing icons
		
		this.frame = frame;
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_D, ActionEvent.CTRL_MASK));
	}

	/**
	 * Action interface
	 */
	public void actionPerformed (ActionEvent e) {
		execute ();
	}

	/**
	 * Command interface
	 */
	public int execute () {

		try {
			// Get the current step, to be deleted
			Step step = Current.getInstance ().getStep ();

			// If step is root in the project, deleting is not allowed, return
			if (step.isRoot ()) {
				StatusDispatcher.print (Translator.swap ("DeleteStep.rootStepCanNotBeDeleted"));
				return 2;
			}

			// If step is not a leaf, deleting is not allowed, return
			if (!step.isLeaf ()) {
				StatusDispatcher.print (Translator.swap ("DeleteStep.onlyLeafStepsCanBeDeleted"));
				return 3;
			}

			// Get the related stepButton, will be disposed
			StepButton stepButton = ProjectManager.getInstance ().getStepButton (step);

			// Get the visible father to delete the segment [it, current step]
			Step prevVisibleStep = (Step) step.getVisibleFather ();

			// If StepButton was colored, try to move color to previous StepButton
			if (stepButton.isColored ()) {
				StepButton prevButton = ProjectManager.getInstance ()
						.getStepButton (prevVisibleStep);
				if (prevButton != null) {
					ButtonColorer.getInstance ().moveColor (prevButton);
				}
			}

			// Destroy related StepButton
			stepButton.dispose ();

			// Destroy steps
			Project project = step.getProject ();
			project.processDeleteStep (step);

			// Update current step (the project manager listens to Current and will update itself)
			Current.getInstance ().setStep (prevVisibleStep);

			StatusDispatcher.print (Translator.swap ("DeleteStep.stepDeleted"));

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "DeleteStep.execute ()", "An Exception/Error occurred", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
