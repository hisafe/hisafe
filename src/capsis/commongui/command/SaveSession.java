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
import java.io.File;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.UserDialog;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;

/**
 * Command SaveSession.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class SaveSession extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("SaveSession.saveSession");

	private JFrame frame;
	private int status; // 0 = finished correctly

	/**
	 * Constructor.
	 */
	public SaveSession (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("save-session_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("save-session_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap ("SaveSession.saveSession"));
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_S, ActionEvent.SHIFT_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'N');
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
			Engine engine = Engine.getInstance ();
			Session session = engine.getSession ();

			if (session != null) {
				// fc - 12.4.2001 - enhancement if (session.getName () == null) {
				if (session.getName () == null || !session.wasSaved ()) {
					new SaveAsSession (frame).execute (); // session name not set
				} else {

					// Check if some projects are unsaved
					for (Project sce : session.getProjects ()) {

						// String name = sce.getFileName ();
						if (!sce.isSaved ()) {
							new SaveProject (frame, sce).execute ();
							// if !sce.isSaved () -> canceled, should return...
						}
					}

					// Check if chosen session file name already exists and not this session's one
					if (new File (session.getFileName ()).exists () && (!session.wasSaved ())) {

						String message = Translator.swap ("SaveSession.theSession") + " "
								+ session.getName () + " "
								+ Translator.swap ("SaveSession.alreadyExists");

						JButton overwriteButton = new JButton (
								Translator.swap ("SaveSession.overwrite"));
						JButton saveAsButton = new JButton (Translator.swap ("SaveSession.saveAs"));
						JButton dontSaveButton = new JButton (
								Translator.swap ("SaveSession.dontSave"));
						Vector buttons = new Vector ();
						buttons.add (overwriteButton);
						buttons.add (saveAsButton);
						buttons.add (dontSaveButton);

						JButton choice = UserDialog.promptUser (frame, Translator
								.swap ("SaveSession.saveSession"), message, buttons, saveAsButton);
						if (choice == null) {
							return 2; // escape hit
						} else if (choice.equals (overwriteButton)) {
							// overwrite with same name
						} else if (choice.equals (saveAsButton)) {
							// new name not to scratch old session on disk
							new SaveAsSession (frame).execute ();

							return 3;
						} else if (choice.equals (dontSaveButton)) { return 4; // stop saving
						}
					}

					StatusDispatcher.print (Translator.swap ("SaveSession.savingSession")
							+ session.getName () + "...");
					try {
						engine.processSaveAsSession (session.getFileName ());
						StatusDispatcher.print (Translator.swap ("SaveSession.sessionSaved"));
					} catch (Exception exc) {
						MessageDialog.print (frame, Translator
								.swap ("SaveSession.errorWhileSavingSession"), exc);
						StatusDispatcher.print (Translator.swap ("SaveSession.ready"));
					}
				}
			}

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "SaveSession.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
