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
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.kernel.Engine;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.Relay;
import capsis.kernel.Session;
import capsis.kernel.Step;

/**
 * Command Evolution.
 * 
 * @author F. de Coligny - October 2000, April 2010
 */
public class Evolution extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("Evolution.evolution");

	private JFrame frame;

	/**
	 * Constructor
	 */
	public Evolution(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("evolution_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("evolution_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("Evolution.evolution"));
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_E, ActionEvent.CTRL_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Action interface
	 */
	public void actionPerformed(ActionEvent e) {
		execute();
	}

	/**
	 * Disable the evolution command if the model has no evolution method
	 */
	@Override
	public boolean isEnabled() {
		// fc-7.2.2018 Added this feature for clarity

		// Project level commands may be disabled (e.g. during an evolution
		// stage), if so, keep disabled
		if (!super.isEnabled())
			return false;

		// if no session, keep disabled
		if (Engine.getInstance().getSession() == null)
			return false;

		// if no project, keep disabled
		int nbProjects = Engine.getInstance().getSession().getProjectCount();
		if (nbProjects == 0)
			return false;

		// if projects found, one is current, check its model
		try {
			Step wStep = Current.getInstance().getStep();
			GModel model = wStep.getProject().getModel();

			return model.isEvolutionModel();

		} catch (Exception e) {
			// In case of trouble (no matter here)
			return super.isEnabled();
		}
	};

	/**
	 * Command interface
	 */
	public int execute() {

		Step wStep;

		wStep = Current.getInstance().getStep();
		Relay relay = wStep.getProject().getModel().getRelay();

		// WAS REMOVED to remove a dependence to DateCorrectable in capsis
		// Memorize date correction / turn it off now - fc - 15.5.2003
		// boolean memoDateCorrected = false;
		// try {
		// DateCorrectable std = (DateCorrectable) wStep.getScene ();
		// memoDateCorrected = (std.isDateCorrected () &&
		// std.isDateCorrectionEnabled ());
		// if (memoDateCorrected) {std.setDateCorrected (false);}
		// } catch (Exception e) {}

		EvolutionParameters p = null;
		try {
			// Get the evolution parameters (ask the relay, will open a dialog)
			StatusDispatcher.print(Translator.swap("Evolution.retrievingEvolutionParameters"));
			p = relay.getEvolutionParameters(wStep); // dialog box
			StatusDispatcher.print(Translator.swap("Evolution.done"));

		} catch (Exception e) {
			Log.println(Log.ERROR, "Evolution.execute ()", "Could not execute", e); // fc-23.10.2020
			MessageDialog.print(frame,
					Translator.swap("Evolution.aTroubleOccurredWhileRetrievingTheEvolutionParametersSeeLog"), e);
			return 2;
		}

		// Check the EvolutionParameters
		if (p == null) {
			StatusDispatcher.print(Translator.swap("Evolution.thisModelDoesNotSupportEvolution"));
			return 2;
		}

		// Check if evolution was cancelled (interactive, cancel button of the
		// dialog)
		if (p instanceof AmapDialog && !((AmapDialog) p).isValidDialog()) {
			((AmapDialog) p).dispose();
			StatusDispatcher.print(Translator.swap("Evolution.evolutionWasCancelled"));
			return 3;
		}

		// Prepare the evolution task
		String modelName = relay.getModel().getIdCard().getModelName();
		String name = Translator.swap("Evolution.evolution") + " " + modelName;

		Task task = new EvolutionTask(name, wStep, p, wStep.getProject(), ProjectManager.getInstance(),
				CommandManager.getInstance(), frame);

		// Run the task
		TaskManager.getInstance().add(task);

		return 0;
	}

}
