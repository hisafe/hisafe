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
import java.util.ArrayList;
import java.util.List;
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
 * Command CloseSession.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class CloseSession extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("CloseSession.closeSession");

	private JFrame frame;
	private int status; // 0 = finished correctly

	/**
	 * Constructor.
	 */
	public CloseSession (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("close-session_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("close-session_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("CloseSession.closeSession"));
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_W, ActionEvent.SHIFT_MASK));
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

			if (session == null) { return 0; } // if no session, ok it's closed

			boolean saveProjects = true;

			// Check if session saved
			if (!session.isSaved () && !session.isEmpty () && session.getProjects ().size () > 1) {
				String message = Translator.swap ("CloseSession.theSession") + " "
						+ session.getName () + " " + Translator.swap ("CloseSession.sheIsNotSaved");

				JButton saveSessionButton = new JButton (
						Translator.swap ("CloseSession.saveSession"));
				JButton saveProjectButton = new JButton (
						Translator.swap ("CloseSession.saveProjects"));
				JButton dontSaveButton = new JButton (Translator.swap ("CloseSession.dontSave"));
				JButton cancelButton = new JButton (Translator.swap ("CloseSession.cancel"));
				Vector buttons = new Vector ();
				buttons.add (saveSessionButton);
				buttons.add (saveProjectButton);
				buttons.add (dontSaveButton);
				buttons.add (cancelButton);

				JButton choice = UserDialog.promptUser (frame, Translator
						.swap ("CloseSession.closeSession"), message, buttons, saveSessionButton);
				if (choice == null || choice.equals (cancelButton)) {
					return 0; // cancel
				} else if (choice.equals (saveSessionButton)) {
					new SaveSession (frame).execute (); // save, then go on closing
					saveProjects = true;
				} else if (choice.equals (saveProjectButton)) {
					saveProjects = true;
				} else if (choice.equals (dontSaveButton)) {
					saveProjects = false;
					// don't save & go on closing
				}
			}

			// Close projects
			List<Project> copy = new ArrayList<Project> ();
			copy.addAll (session.getProjects ());
			for (Project sce : copy) {
				new CloseProject (frame, sce, saveProjects).execute ();
				if (session.getProjects ().contains (sce)) { return 0; } // cancel on close
			}

			// Close session
			engine.processCloseSession ();

			CommandManager.getInstance ().setCommandsEnabled (CommandManager.Level.PROJECT, false);
			CommandManager.getInstance ().updateFrameTitle ();

			StatusDispatcher.print (Translator.swap ("CloseSession.sessionClosed"));

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "CloseSession.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0; // close was done
	}

}
