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
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.kernel.Project;

/**
 * Command SaveAsProject.
 * 
 * @author F. de Coligny - October 2000, April 2010
 */
public class SaveAsProject extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("SaveAsProject.saveAsProject");

	private Project project;
	private JFrame frame;
	private SaveProject caller; // this command may be called by SaveProject

	/**
	 * Constructor 1
	 */
	public SaveAsProject(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("save-as-project_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("save-as-project_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
	}

	/**
	 * Constructor 2
	 */
	public SaveAsProject(JFrame frame, Project p) {
		this(frame);
		project = p;
	}

	/**
	 * Constructor 3
	 */
	public SaveAsProject(JFrame frame, Project p, SaveProject caller) {
		this(frame, p);
		this.caller = caller;
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
			// SaveAs the project 'p'
			Project p = null;

			if (project != null) { // constructor 2
				p = project;

			} else { // constructor 1
				p = Current.getInstance().getProject();
			}

			if (p == null) {
				return 1;
			}

			String name = p.getName();

			boolean trouble = false;
			JFileChooser chooser = null;
			int returnVal = 0;
			do {
				trouble = false;

				// fc-4.10.2021 Settings instead of System
				chooser = new JFileChooser(Settings.getProperty("project.path", PathManager.getDir("tmp")));
//				chooser = new JFileChooser(System.getProperty("project.path"));

				chooser.setDialogType(JFileChooser.SAVE_DIALOG);
				chooser.setApproveButtonText(Translator.swap("SaveAsProject.save"));
				chooser.setDialogTitle(Translator.swap("SaveAsProject.saveAsProject") + " : " + name);

				chooser.setDialogType(JFileChooser.SAVE_DIALOG);

				// Try to set the project name as a default proposal for fileName
				// fc-4.10.2021 Using the raw name here, do not propose a nameUniquenessSuffix
				// if any.
				// added tmp option
				chooser.setSelectedFile(new File(Settings.getProperty("project.path", PathManager.getDir("tmp"))
						+ File.separator + p.getRawName()));
//				chooser.setSelectedFile(
//						new File(Settings.getProperty("project.path", "") + File.separator + p.getRawName()));

				returnVal = chooser.showDialog(frame, null); // null : approveButton text was
																// already set

				if (returnVal == JFileChooser.APPROVE_OPTION && chooser.getSelectedFile().exists()) {
					if (!Question.ask(frame, Translator.swap("SaveAsProject.confirm"),
							"" + chooser.getSelectedFile().getPath() + "\n"
									+ Translator.swap("SaveAsProject.fileExistsPleaseConfirmOverwrite"),
							Translator.swap("SaveAsProject.overwrite"))) {
						trouble = true;
					}
				}

			} while (trouble);

			if (returnVal == JFileChooser.APPROVE_OPTION) {
				File file = chooser.getSelectedFile();

				// Rename the project
				// 28.9.2021: only if needed and forceUpdate
				if (!file.getName().equals(p.getName())) { // 28.9.2021
					p.setName(file.getName());
					Current.getInstance().forceUpdate(); // 28.9.2021
				}

				p.setFileName(file.getAbsolutePath());
				Settings.setProperty("project.path", file.getParent());

				// We know user has checked file name now
				p.setWasSaved(true);

				// If SaveAsProject was called by SaveProject, continue in the caller
				if (caller != null) {
					return 0;
				}

				// Else call SaveProject
				new SaveProject(frame, p).execute();

				// Maybe the project name was changed, refresh ProjectManager
				try {
					ProjectManager.getInstance().update();
				} catch (Exception e) {
				}

			}

			// If SaveAsProject was called by SaveProject, continue in the caller
			if (caller != null) {
				return 1;
			}

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println(Log.ERROR, "SaveAsProject.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			MessageDialog.print(frame, Translator.swap("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
