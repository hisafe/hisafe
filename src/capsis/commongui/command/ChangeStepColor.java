/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2017 INRA
 * 
 * Authors: F. de Coligny, N. Beudez, S. Dufour-Kowalski
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

import java.awt.Color;
import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JColorChooser;
import javax.swing.JFrame;

import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.kernel.Step;
import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.ColoredIcon;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**
 * Command to change the color assigned to the stepButton of a given step.
 * 
 * @author F. de Coligny - January 2017
 */
public class ChangeStepColor extends AbstractAction implements ActionCommand, Listener {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("ChangeStepColor.changeStepColor");

	private JFrame frame;

	private Step memoStep; // for updateIcon

	/**
	 * Constructor
	 */
	public ChangeStepColor(JFrame frame) {
		super(name);

		// fc-31.7.2018 the icon is managed in updateIcon() below
		// putValue(SMALL_ICON, IconLoader.getIcon("change-step-color_16.png"));
		// putValue(LARGE_ICON_KEY, IconLoader.getIcon("change-step-color_24.png"));

		this.frame = frame;

		// No accelerator
		// this.putValue (Action.ACCELERATOR_KEY, KeyStroke
		// .getKeyStroke (KeyEvent.VK_D, ActionEvent.CTRL_MASK));

		// We would like to be told if user changes the current step to update
		// the icon color
		Current.getInstance().addListener(this); // fc-4.1.2017

	}

	/**
	 * Action interface
	 */
	public void actionPerformed(ActionEvent e) {
		execute();
	}

	@Override
	public boolean isEnabled() {
		try {
			// Get the current step
			Step step = Current.getInstance().getStep();

			// Get the related stepButton
			StepButton stepButton = ProjectManager.getInstance().getStepButton(step);

			return stepButton.isColored();

		} catch (Exception e) {
			// All projects closed: no current step...
			return false;
		}
	}

	/**
	 * Command interface
	 */
	public int execute() {

		try {
			// Get the current step
			Step step = Current.getInstance().getStep();

			// Get the related stepButton
			StepButton stepButton = ProjectManager.getInstance().getStepButton(step);

			Color currentColor = stepButton.getColor();

			if (currentColor == null)
				return 1; // stepButton is not colored

			Color newColor = JColorChooser.showDialog(frame,
					Translator.swap("ChangeStepColor.chooseANewColorForThisStep"), currentColor);

			if (newColor == null)
				return 2; // User cancellation

			// User cancellation
			if (newColor.equals(currentColor))
				return 0;

			ButtonColorer.getInstance().changeColor(stepButton, newColor);

			// System.out.println("ChangeStepColor newColor: " + newColor);

			StatusDispatcher.print(Translator.swap("ChangeStepColor.stepColorChanged"));

		} catch (Throwable e) { // Catch Errors in every command (for
								// OutOfMemory)
			Log.println(Log.ERROR, "ChangeStepColor.execute ()", "An Exception/Error occurred", e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			MessageDialog.print(frame, Translator.swap("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

	@Override
	public void somethingHappened(ListenedTo l, Object param) { // fc-4.1.2017

		// Events are sent when user clicks in the project manager, changing
		// current project or current step
		if (l instanceof Current)
			updateIcon();

	}

	/**
	 * Update the icon of the actionCommand with the color of the current
	 * stepButton.
	 */
	private void updateIcon() { // fc-4.1.2017

		// Get the current step
		Step step = Current.getInstance().getStep();

		if (step == null)
			return; // at project closing time...

		// Get the related stepButton
		StepButton stepButton = ProjectManager.getInstance().getStepButton(step);

		// fc-13.3.2017 added stepButton != null (bug with a phenofit
		// simulation)
		if (stepButton != null && stepButton.isColored()) {
			Color c = stepButton.getColor();
			ColoredIcon icon = new ColoredIcon(c);
			putValue(Action.SMALL_ICON, icon);
		} else {
			putValue(Action.SMALL_ICON, null);
		}

		// System.out.println("ChangeStepColor updateIcon ()...");

	}

}
