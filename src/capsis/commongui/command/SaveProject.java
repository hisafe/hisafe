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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;

import capsis.commongui.projectmanager.Current;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;
import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;

/**
 * Command SaveProject.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class SaveProject extends AbstractAction implements ActionCommand, ListenedTo {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("SaveProject.saveProject");

	private Project project;
	private JFrame frame;

	private List<Listener> listeners; // ListenedTo interface

	/**
	 * Constructor 1
	 */
	public SaveProject (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("save-project_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("save-project_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_S, ActionEvent.CTRL_MASK));
	}

	/**
	 * Constructor 2
	 */
	public SaveProject (JFrame frame, Project project) {
		this (frame);
		this.project = project;
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

		// try {

		// Save the 'p' project
		Project p = project != null ? project : Current.getInstance ().getProject ();

		if (p == null) { return 1; }

		// fc-4.10.2021
		// Check if several projects have the same fileName
		// If so, move to SaveAs to let user manage the fileNames (overwrite...)
		String candidateFileName = p.getFileName();
		Session session = Engine.getInstance().getSession ();
		boolean duplicateFileName = false;
		for (Project proj : session.getProjects()) {
			if (proj.equals (p))
				continue;
			if (proj.getFileName().equals(candidateFileName)) {
				duplicateFileName = true;
				break;
			}
		}
		
		
		
		// Needs project name to save file
		// If no project name or project was never saved before, SaveAs
		if (p.getName () == null || p.getName ().equals ("") || !p.wasSaved ()
				|| duplicateFileName) { // Need to ask user ?

			int rc = new SaveAsProject (frame, p, this).execute ();

			// SavaAs may ahev been canceled
			if (rc != 0) { return 1; }
		}

		final Project final_p = p;

		// A task is needed to properly report the StatusDispatcher messages
		Task<Exception,Void> t = new Task<Exception,Void> (
				Translator.swap ("OpenProject.saveProjectName")) {

			@Override
			protected Exception doInWorker () {
				try {
					StatusDispatcher.print (Translator.swap ("SaveProject.savingProject") + " : "
							+ final_p.getFileName () + "...");

					try {
						SwingUtilities.invokeAndWait (new Runnable () {

							public void run () {
								// Disable project level commands
								CommandManager.getInstance ()
										.setCommandsEnabled (CommandManager.Level.PROJECT, false);
							}
						});
					} catch (Exception e) {}

					Engine.getInstance ().processSaveAsProject (final_p, final_p.getFileName ());

					StatusDispatcher.print (Translator.swap ("SaveProject.projectSaved") + " : "
							+ final_p.getFileName ());

					return (Exception) null; // correct end, return null

				} catch (Exception e) {
					return e; // if exception, return it
				}

			}

			@Override
			protected void doInEDTafterWorker () {
				try {

					// Re-enable project level commands
					CommandManager.getInstance ()
							.setCommandsEnabled (CommandManager.Level.PROJECT, true);

					Exception e = get ();
					if (e != null) { // trouble

						Log.println (Log.ERROR, "SaveProject.execute ()", "An Exception/Error occured", e);
						MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
						StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));

					}

					SaveProject.this.done (e);

				} catch (Exception e) { // should never happen
					Log.println (Log.ERROR, "SaveProject.execute ()", "'Should never happen' exception in doInEDTafterWorker (), passed", e);
				}

			}

		};

		t.setIndeterminate ();
		TaskManager.getInstance ().add (t);

		return 0;
	}

	/**
	 * Called when the saving is over, notify listener if any
	 */
	private void done (Exception e) {

		// If an exception is passed, pass it to the listeners
		// If exception is null -> the process was ok
		tellSomethingHappened (e);
	}

	/**
	 * Add a listener to this object. ListenedTo interface
	 */
	public void addListener (Listener l) {
		if (listeners == null) {
			listeners = new ArrayList<Listener> ();
		}
		listeners.add (l);
	}

	/**
	 * Remove a listener to this object. ListenedTo interface
	 */
	public void removeListener (Listener l) {
		if (listeners == null) { return; }
		listeners.remove (l);
	}

	/**
	 * Notify all the listeners by calling their somethingHappened (listenedTo, param) method.
	 * ListenedTo interface
	 */
	public void tellSomethingHappened (Object param) {
		if (listeners == null) { return; }
		for (Listener l : listeners) {
			l.somethingHappened (this, param);
		}
	}

}
