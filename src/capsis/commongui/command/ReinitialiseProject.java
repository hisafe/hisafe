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

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JFrame;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.commongui.projectmanager.Current;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Project;

/**	Command ReinitialiseProject.
 *	@author F. de Coligny - may 2010
 */
public class ReinitialiseProject extends AbstractAction implements ActionCommand {
	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("ReinitialiseProject.reinitialiseProject");

	private JFrame frame;
	private int status; // 0 = finished correctly

	
	/**	Constructor
	 */
	public ReinitialiseProject(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("reinitialise-project_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("reinitialise-project_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;

		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("ReinitialiseProject.newProject"));
//		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(
//				KeyEvent.VK_N, ActionEvent.CTRL_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Action interface
	 */
	public void actionPerformed(ActionEvent e) {
		execute();
	}

	/**
	 * Command interface
	 */
	public int execute() {

		Project sourceProject = Current.getInstance ().getProject ();
		GModel model = sourceProject.getModel();
		InitialParameters p = null;
		
		boolean yep = Question.ask(frame, Translator.swap("ReinitialiseProject.projectReinitialisation"), 
				Translator.swap("ReinitialiseProject.thisWillDeleteTheCurrentProjectWithoutSavingContinue")+" ?");
		if (!yep) {return 0;}  // user aborted
		
		try {
			// Recall the getInitialParameters () method
			p = model.getRelay().getInitialParameters();
		} catch (Exception e) {
			Log.println(Log.ERROR, "ReinitialiseProject.execute ()",
					"getInitialParameters() threw an exception", e);
			MessageDialog.print(frame, 
					Translator.swap("ReinitialiseProject.couldNotReinitialiseTheProjectSeeLog"), e);
			status = 2;
			return status;
		}
		
		if (p == null) {
			// User abort
//			Log.println(Log.ERROR, "ReinitialiseProject.execute ()",
//					"InitialParameter is null");
//			MessageDialog.print(frame, 
//					Translator.swap("ReinitialiseProject.troubleWithThisModelErrorDuringModelInitializationSeeLog"));
			status = 3;
			return status;
		}

		// Prepare the initialization task
		String projectName = sourceProject.getName();
//		String modelName = model.getIdCard().getModelName();
		String name = Translator.swap("ReinitialiseProject.reInitializingProject") + " "
				+ projectName + " ...";

		Task task = new ReinitialiseTask(name, projectName, p, model, frame, sourceProject);

		// Run the task
		TaskManager.getInstance().add(task);

		status = 0;
		return status;
	}

	public int getStatus() {
		return status;
	}

}

