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
import java.util.List;
import java.util.Vector;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.UserDialog;
import capsis.commongui.command.CommandManager.Level;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;

/**
 * Command CloseProject.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class CloseProject extends AbstractAction implements ActionCommand, Listener {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("CloseProject.closeProject");

	private Project project;
	private JFrame frame;
	private boolean save = true;
	// specific to Close > Save > back to Close to finish when the saving is over
	// See below in somethingHappened ()
	private Project pendingProject;

	/**
	 * Constructor 1.
	 */
	public CloseProject (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("close-project_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("close-project_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_W, ActionEvent.CTRL_MASK));
	}

	/**
	 * Constructor 2.
	 */
	public CloseProject (JFrame frame, Project project) {
		this (frame, project, true);
	}

	/**
	 * Constructor 3.
	 */
	public CloseProject (JFrame frame, Project project, boolean saveProject) {
		this (frame);
		this.project = project;
		save = saveProject;
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
			// Close the project 'p'
			Project p = null;

			if (project != null) { // constructor 2 or 3
				p = project;

			} else { // constructor 1
				p = Current.getInstance ().getProject ();
			}

			if (p == null) { return 1; }

			Engine engine = Engine.getInstance ();
			Session session = engine.getSession ();
			ProjectManager projectManager = ProjectManager.getInstance ();

			// Ask for a confirmation if project not saved
			if (!p.isSaved () && save) {
				String message = Translator.swap ("CloseProject.theProject") + " " + p.getName ()
						+ " " + Translator.swap ("CloseProject.heIsNotSaved");

				JButton saveButton = new JButton (Translator.swap ("CloseProject.save"));
				JButton ignoreButton = new JButton (Translator.swap ("CloseProject.dontSave"));
				JButton cancelButton = new JButton (Translator.swap ("CloseProject.cancel"));
				Vector buttons = new Vector ();
				buttons.add (saveButton);
				buttons.add (ignoreButton);
				buttons.add (cancelButton);

				JButton choice = UserDialog.promptUser (frame, Translator
						.swap ("CloseProject.projectClosing"), message, buttons, saveButton);
				if (choice == null || choice.equals (cancelButton)) {
					return 2; // Stop closing

				} else if (choice.equals (saveButton)) {

					SaveProject sp = new SaveProject (frame, p);
					// We want to be told when saving is over
					sp.addListener (this);
					pendingProject = p;

					sp.execute ();

					// This defers the closing: SaveProject will save in a task,
					// then call our somethingHappened () and we will be able
					// to finish the project closing
					// Note: closing the project now would interfere with the saving task
					return 0;

				} else if (choice.equals (ignoreButton)) {
					// nothing to do, go on with closing
				}
			}

			return performClosing (p);

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "CloseProject.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}

	}

	private int performClosing (Project p) {

		try {

			Session session = Engine.getInstance ().getSession ();

			int index = session.getProjects ().indexOf (p); // nov 2011

			// Close in engine (destroy)
			Engine.getInstance ().processCloseProject (p);

			// Remove project StepButtons colors / close the synchronized tools
			ButtonColorer.getInstance ().clean ();

			// Select another project
			if (session.isEmpty ()) {
				
//				Current.getInstance ().setProject (null); fc-25.4.2013 this line was moved below

				// Manage Project commands enabling / disabling
				CommandManager.getInstance ().setCommandsEnabled (Level.PROJECT, false);
				
				// fc-25.4.2013 MOVED this line AFTER setCommandsEnabled () to fix the bug[Simeo: new
				// project Lollymangrove results in an enabled TwistManually command when it should be
				// disabled]
				Current.getInstance ().setProject (null);
				
			} else {
				// Select another project
				// Changed in nov 2011 to consider the new option "project.manager.reverse.order"
				Project newPro = null;
				try {
					// nov 2011
					if (Settings.getProperty ("project.manager.reverse.order", true)) {
						// reverse order
						try {
							newPro = (Project) session.getProjects ().get (index - 1);
						} catch (Exception e) {
							newPro = (Project) session.getProjects ().iterator ().next (); // select
																							// first
																							// project
						}

					} else {
						// classic order
						try {
							System.out
									.println ("CloseProject | classic order | getting project at index: "
											+ index);
							newPro = (Project) session.getProjects ().get (index);
						} catch (Exception e) {
							System.out
									.println ("CloseProject | classic order | error | getting project at getProjectCount () - 1: "
											+ (session.getProjectCount () - 1));
							newPro = (Project) session.getProjects ()
									.get (session.getProjectCount () - 1); // select last project
							System.out.println ("CloseProject | classic order | done");
						}
					}
					// nov 2011
				} catch (Exception e) { // in case of trouble
					newPro = (Project) session.getProjects ().iterator ().next (); // select first
																					// project
				}
				Current.getInstance ().setProject (newPro);

			}

			StatusDispatcher.print (Translator.swap ("CloseProject.projectClosed"));

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "CloseProject.performClosing ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (frame, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

	/**
	 * This is called by SaveProject when its done in the case the project must be saved before
	 * closing. SaveProject saves in a task and this two pass method is needed.
	 */
	@Override
	public void somethingHappened (ListenedTo l, Object param) {
		if (param == null) { // ok

			performClosing (pendingProject);

		} else { // trouble, param is an exception

			StatusDispatcher.print (Translator.swap ("CloseProject.closeProjectAborted"));

		}

	}

}
