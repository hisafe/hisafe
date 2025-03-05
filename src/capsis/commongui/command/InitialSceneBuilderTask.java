/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA 
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

import jeeb.lib.util.Alert;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import capsis.commongui.InitialDialogInterface;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;

/**
 * InitialSceneBuilderTask SwingWorker. It calls buildInitScene () in a task and
 * manages the gui: temporary blocking and progress report on the given dialog.
 * The dialog is closed at the end with setValid (true) if the process is successful.
 * 
 * @author F. de Coligny - December 2011
 */
public class InitialSceneBuilderTask extends Task<Object, Void> {

	private GModel model;
	private InitialDialogInterface initialDialog;
	private InitialParameters initialParameters;

	/**
	 * Constructor.
	 */
	public InitialSceneBuilderTask(GModel model, InitialDialogInterface initialDialog,
			InitialParameters initialParameters) {
		super(model.getIdCard().getModelName());

		this.model = model;
		this.initialDialog = initialDialog;
		this.initialParameters = initialParameters;
		setIndeterminate();

		initialDialog.setInitialParameters(initialParameters);
	}

	@Override
	protected void doFirstInEDT() {

//		System.out.println("InitialSceneBuilder.doFirstInEDT ()...");
//		try {Thread.sleep(3000);} catch (Exception e) {} // debug
		
		// Desactivate the dialog gui
		AmapTools.setEnabled(initialDialog.getContentPane(), false);
	}

	@Override
	public Object doInWorker() { // this method returns null if trouble
		StatusDispatcher.print(Translator
				.swap("InitialSceneBuilderTask.buildingTheInitialScene") + "...");
		// Long process
		try {
			
//			System.out.println("InitialSceneBuilder.doInWorker ()...");
//			try {Thread.sleep(3000);} catch (Exception e) {} // debug
			
			initialParameters.buildInitScene(model);
		} catch (Exception e) {
			Log.println(Log.ERROR, "InitialSceneBuilderTask.doInWorker ()",
					"Could not build the initialScene", e);
			Alert.print(model.getIdCard().getModelName() + ": " + Translator
					.swap("InitialSceneBuilderTask.couldNotBuildTheInitialScene"), e);
			return null;
		}
		return "Ok"; // not null
	}

	@Override
	public void doInEDTafterWorker() {
		
//		System.out.println("InitialSceneBuilder.doInEDTafterWorker ()...");
//		try {Thread.sleep(3000);} catch (Exception e) {} // debug

		// Reactivate the dialog gui
		AmapTools.setEnabled(initialDialog.getContentPane(), true);

		try {
			Object result = get(); // what did doInWorker return ?

			if (result != null) {
				initialDialog.setValidDialog(true); // allright, close the
													// dialog and continue
			} else { // there was a trouble
				// Do not close the dialog, user will check and try again
			}
		} catch (Exception e) {
			// Do not close the dialog, user will check and try again
		}
	}

}
