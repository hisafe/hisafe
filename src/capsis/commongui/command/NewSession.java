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
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.kernel.Engine;

/**
 * Command NewSession.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class NewSession extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("NewSession.newSession");

	private JFrame frame;
	private int status; // 0 = finished correctly

	/**
	 * Constructor.
	 */
	public NewSession (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("new-session_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("new-session_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap ("NewSession.newSession"));
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_N, ActionEvent.SHIFT_MASK));
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

			NewSessionDialog dlg = new NewSessionDialog (frame);
			if (dlg.isValidDialog ()) {

				// Close previous session
				if (engine.getSession () != null) {
					new CloseSession (frame).execute ();
				}

				engine.processNewSession (dlg.getSessionName ());

				// Enable the projects level commands
				CommandManager.getInstance ()
						.setCommandsEnabled (CommandManager.Level.PROJECT, true);
				CommandManager.getInstance ().updateFrameTitle ();

				// If requested, create first project
				if (dlg.getFirstProject ().isSelected ()) {
					// NOTE: Session created, session dialog set unvisible but not disposed yet
					new NewProject (frame).execute ();
				}

			}
			dlg.dispose (); // when all finished, destroy the new session dialog
			StatusDispatcher.print (Translator.swap ("NewSession.ready"));

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "NewSession.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
