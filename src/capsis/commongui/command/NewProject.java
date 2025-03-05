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
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;

/**
 * Command NewProject.
 * 
 * @author F. de Coligny - October 2000, April 2010
 */
public class NewProject extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("NewProject.newProject");
	static private Class<? extends NewProjectDialog> dialogClass = capsis.commongui.command.DefaultNewProjectDialog.class;

	private JFrame frame;
	private int status; // 0 = finished correctly

	/**
	 * Constructor
	 */
	public NewProject(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("new-project_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("new-project_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;

		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("NewProject.newProject"));
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, ActionEvent.CTRL_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Use this optional method to tell the NewProject command to use a specific
	 * dialog instead of the default DefaultNewProjectDialog. The new dialog
	 * must implement NewProjectDialog and run the same way than the default.
	 */
	static public void setDialog(Class<? extends NewProjectDialog> dialogClass) {
		NewProject.dialogClass = dialogClass;
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

		// Open the New Project dialog
		NewProjectDialog dns = openDialog();

		// Dialog canceled
		if (!dns.isValidDialog()) {
			dns.dispose();
			StatusDispatcher.print(Translator.swap("NewProject.ready"));
			status = 1;
			return status;
		}

		// Ok on the dialog -> create the project

		// Check the InitialParameters
		InitialParameters ip = dns.getInitialParameters();
		if (ip == null) {
			Log.println(Log.ERROR, "NewProject.execute ()", "InitialParameter is null");
			StatusDispatcher.print(Translator
					.swap("NewProject.troubleWithThisModelErrorDuringModelInitializationSeeLog"));
			status = 2;
			return status;

		} else if (ip.getInitScene() == null) {
			// fc-18.2.2011 Prevents opening a messageDialog when canceling a
			// module initial dialog

			// may happen when canceling the module initial dialog
			// if the NewProjectDialog closes automatically
			// -> this is considered as a user cancelation
			dns.dispose();
			StatusDispatcher.print(Translator.swap("NewProject.userCancelation"));
			status = 2;
			return status;

		}

		// Prepare the initialization task
		GModel model = dns.getModel();
		String modelName = model.getIdCard().getModelName();
		String name = Translator.swap("NewProject.initializingProject") + " " + modelName + " ...";

		Task task = new InitializeTask(name, dns.getProjectName(), ip, model, frame);

		// Run the task
		TaskManager.getInstance().add(task);

		// Dispose dialog in any case
		dns.dispose();

		// Mathieu Fortin: fixed a memory leak - fc-31.8.2015
		try {
			dns.finalize();
		} catch (Throwable e) {
			Log.println(Log.WARNING, "NewProject.execute ()", "Exception while finalizing the dialog, passed", e);
		}

		status = 0;
		return status;
	}

	/**
	 * Opens the project dialog
	 */
	private NewProjectDialog openDialog() {
		try {

			Constructor<? extends NewProjectDialog> c = NewProject.dialogClass.getConstructor(JFrame.class);
			NewProjectDialog dlg = c.newInstance(frame);
			return dlg;

		} catch (Exception e) {
			Log.println(Log.ERROR, "NewProject.openDialog ()", "Could not open this dialog, dialogClass = "
					+ dialogClass, e);
		}
		return null;

	}

	public int getStatus() {
		return status;
	}

}
