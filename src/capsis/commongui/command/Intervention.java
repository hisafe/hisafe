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
import java.lang.reflect.Constructor;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.commongui.projectmanager.Current;
import capsis.kernel.Relay;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;

/**
 * Intervention command.
 * 
 * @author F. de Coligny - october 2000, may 2009, october 2010
 */
public class Intervention extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("Intervention.intervention");
	static private Class<? extends InterventionDialog> dialogClass = capsis.commongui.command.DefaultInterventionDialog.class; // default

	private JFrame frame;

	/**
	 * Constructor
	 */
	public Intervention (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("intervention_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("intervention_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("Intervention.intervention"));
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_I, ActionEvent.CTRL_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'I');
	}

	/**
	 * Use this optional method to tell the Intervention command to use a specific dialog instead of
	 * the default DefaultInterventionDialog. The new dialog must implement InterventionDialog and
	 * run the same way than the default.
	 */
	static public void setDialog (Class<? extends InterventionDialog> dialogClass) {
		Intervention.dialogClass = dialogClass;
	}

	/**
	 * Action interface
	 */
	public void actionPerformed (ActionEvent e) {
		execute ();
	}

	/**
	 * Run the intervention, then run processPostIntervention ()
	 */
	public int execute () {

		Step wStep = null; // step is not altered (if null, stays null)
		Relay relay;

		wStep = Current.getInstance ().getStep ();
		relay = wStep.getProject ().getModel ().getRelay ();

		// Choose an intervener
		InterventionDialog dlg = openDialog (wStep);
		// InterventionDialog dlg = new DIntervention (frame, wStep);

		// Check if intervention was canceled (interactive, cancel button of the dialog)
		if (!dlg.isValidDialog ()) {
			dlg.dispose ();
			StatusDispatcher.print (Translator.swap ("Intervention.interventionWasCancelled"));
			return 2;
		}

		// Get the intervener
		Intervener intervener = dlg.getIntervener ();

		// Cut only if ok chosen and parameters checked correct
		if (!intervener.isReadyToApply ()) {
			StatusDispatcher.print (Translator
					.swap ("Intervention.wrongIntervenerParameteringNotReadyToApply"));
			return 3;
		}

		// Prepare postIntervention task
		String modelName = wStep.getProject ().getModel ().getIdCard ().getModelName ();
		String name = Translator.swap ("Intervention.intervention") + " " + modelName;

		Task task = new InterventionTask (name, wStep, intervener);

		// Run the task
		TaskManager.getInstance ().add (task);
		dlg.dispose ();

		return 0;
	}

	/**
	 * Opens the intervention dialog
	 */
	private InterventionDialog openDialog (Step step) {
		try {

			Constructor<? extends InterventionDialog> c = Intervention.dialogClass
					.getConstructor (JFrame.class, Step.class);
			InterventionDialog dlg = c.newInstance (frame, step);
			return dlg;

		} catch (Exception e) {
			Log.println (Log.ERROR, "Intervention.openDialog ()", "Could not open this dialog, dialogClass = "
					+ dialogClass, e);
		}
		return null;

	}

}
