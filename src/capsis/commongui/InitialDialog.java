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
package capsis.commongui;

import java.awt.Window;

import javax.swing.JFrame;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.commongui.command.InitialSceneBuilderTask;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;

/**
 * A superclass for the initial dialogs in Capsis.
 * @author S. Dufour
 */
public abstract class InitialDialog extends AmapDialog implements InitialDialogInterface {

	protected InitialParameters initialParameters;
	// If set, the owner of the InitialDialogs when built with constructor 2.
	static protected Window ownerWindow;

	/**
	 * Constructor
	 */
	public InitialDialog(JFrame frame) {
		super(frame);
	}

	/**
	 * Constructor 2
	 */
	public InitialDialog() {
		super(InitialDialog.ownerWindow == null ? null
				: InitialDialog.ownerWindow);
	}

	/**
	 * This optional feature can help manage focus correctly by linking the
	 * modules initial dialogs to an owner frame or dialog. See constructor 2.
	 */
	static public void setOwnerWindow(Window ownerWindow) {
		InitialDialog.ownerWindow = ownerWindow;
	}

	@Override
	public InitialParameters getInitialParameters() {
		return initialParameters;
	}

	@Override
	public void setInitialParameters(InitialParameters ip) {
		initialParameters = ip;
	}

	/**
	 * Runs buildInitScene () in a task, disables the dialog during the process,
	 * reports the progress, then closes the dialog with setValidDialog (true)
	 * if successful. This can be called in the initialDialogs in okAction ()
	 * after all the user data has been successfully checked and copied to the
	 * InitialParameters object.
	 */
	static public void buildInitScene(GModel model, InitialDialogInterface initialDialog,
			InitialParameters initialParameters) {

		// Run buildInitScene in a task (will close the dialog 'valid' if ok)
		Task task = new InitialSceneBuilderTask(model, initialDialog,
				initialParameters);

		// TaskManager is a Singleton pattern
		TaskManager.getInstance().add(task); // the task is started by the task
												// manager
	}

}
