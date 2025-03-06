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

package capsis.gui.command;

import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.csvfileviewer.CsvFileViewer;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.commongui.command.CommandManager;
import capsis.commongui.projectmanager.Current;
import capsis.gui.DExport;
import capsis.kernel.GModel;
import capsis.kernel.Relay;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;

/**
 * Command ExportStep.
 * 
 * @author F. de Coligny - january 2001
 */
public class ExportStep extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("MainFrame.exportStep");

	private JFrame frame;

	/**
	 * Constructor
	 */
	public ExportStep (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("export_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("export_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("NewProject.newProject"));
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_T, ActionEvent.CTRL_MASK));
		// this.putValue (Action.MNEMONIC_KEY, 'N');

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
			Step wStep;
			wStep = Current.getInstance ().getStep ();

			// 2. Check if model is exportable
			GModel model = wStep.getProject ().getModel ();
			ExtensionManager em = CapsisExtensionManager.getInstance ();
			if (!em.hasExtensions (CapsisExtensionManager.IO_FORMAT, model)) {
				MessageDialog.print (this, Translator.swap ("ExportStep.modelIsNotExportable"));

				// MainFrame.getInstance ().getAction ("ExportStep").setEnabled
				// (false);
				CommandManager.getInstance ().getCommand (ExportStep.class).setEnabled (false);

				return 2;
			}

			// 3. Dialog: format ? file name ?
			Relay relay = model.getRelay ();
			DExport dlg = new DExport (relay.getModel ());
			if (dlg.isValidDialog ()) {

				// Export format class name is retrieved from DExport dialog
				OFormat ioFormat = dlg.getOFormat ();
				String fileName = dlg.getFileName ();

				// 4. Export now
				try {
					wStep.getProject ().export (wStep, ioFormat, fileName);

					StatusDispatcher.print (Translator.swap ("ExportStep.exportOfStep") + " "
							+ wStep.getCaption () + " " + Translator.swap ("ExportStep.inFile")
							+ " " + fileName + " " + Translator.swap ("ExportStep.isOver"));
					
					// fc-3.9.2015 Open the file in a CSV viewer if required
					if (dlg.isRequiredOpenFileInCsvViewer ()) {
						new CsvFileViewer(frame, fileName);
					}
					
				} catch (Exception exc) {
					MessageDialog
							.print (this, Translator.swap ("ExportStep.errorWhileExporting"), exc);
					StatusDispatcher.print (Translator.swap ("ExportStep.exportFailed"));
				}
			}
			dlg.dispose ();

		} catch (Throwable e) { // fc - 30.7.2004 - catch Errors in every
								// command (for OutOfMemory)
			Log.println (Log.ERROR, "ExportStep.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;

	}

}
