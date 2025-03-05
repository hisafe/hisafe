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
import java.io.File;

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JFileChooser;
import javax.swing.JFrame;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.kernel.Engine;
import capsis.kernel.Session;

/**
 * Command SaveAsSession.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class SaveAsSession extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("SaveAsSession.saveAsSession");

	private JFrame frame;
	private int status; // 0 = finished correctly

	/**
	 * Constructor.
	 */
	public SaveAsSession(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("save-as-session_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("save-as-session_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// //~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("SaveSession.saveSession"));
		// this.putValue (Action.ACCELERATOR_KEY,
		// KeyStroke.getKeyStroke (KeyEvent.VK_S, ActionEvent.SHIFT_MASK));
		// //~ this.putValue (Action.MNEMONIC_KEY, 'N');
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

		try {
			// Retrieve some references
			Engine engine = Engine.getInstance();
			Session session = engine.getSession();

			if (session != null) {

				boolean trouble = false;
				JFileChooser chooser = null;
				int returnVal = 0;
				do {
					trouble = false;

					// fc-4.10.2021 Use Settings instead of System here, + capsis.session.path may
					// be empty, added tmp
					chooser = new JFileChooser(Settings.getProperty("capsis.session.path", PathManager.getDir("tmp")));
//					chooser = new JFileChooser (System.getProperty ("capsis.session.path"));

					chooser.setDialogType(JFileChooser.SAVE_DIALOG);
					chooser.setApproveButtonText(Translator.swap("SaveAsSession.save"));
					chooser.setDialogTitle(Translator.swap("SaveAsSession.saveAsSession"));

					chooser.setDialogType(JFileChooser.SAVE_DIALOG);

					returnVal = chooser.showDialog(frame, null); // null : approveButton text was
																	// already set

					if (returnVal == JFileChooser.APPROVE_OPTION && chooser.getSelectedFile().exists()) {
						if (!Question.ask(frame, Translator.swap("SaveAsSession.confirm"),
								"" + chooser.getSelectedFile().getPath() + "\n"
										+ Translator.swap("SaveAsSession.fileExistsPleaseConfirmOverwrite"))) {
							trouble = true;
						}
					}

				} while (trouble);

				if (returnVal == JFileChooser.APPROVE_OPTION) {
					File fileName = chooser.getSelectedFile();

					session.setName(fileName.getName());
					Settings.setProperty("capsis.session.path", fileName.getParent());

					// We know that user has checked file name now
					session.setWasSaved(true);
					new SaveSession(frame).execute();

					CommandManager.getInstance().updateFrameTitle();

				}
			}

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println(Log.ERROR, "SaveAsSession.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			MessageDialog.print(frame, Translator.swap("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
