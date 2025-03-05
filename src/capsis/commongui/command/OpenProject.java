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

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.fileloader.CheckingFileChooser;
import jeeb.lib.util.serial.Reader;
import jeeb.lib.util.serial.SerializerFactory;
import jeeb.lib.util.task.Task;
import jeeb.lib.util.task.TaskManager;
import capsis.commongui.projectmanager.Current;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.ProjectIdCard;
import capsis.kernel.Session;
import capsis.kernel.Step;

/**
 * Command OpenProject.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class OpenProject extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("OpenProject.openProject");

	private String projectFileName;
	private JFrame frame;
	private int status; // 0=finished correctly

	/**
	 * Constructor.
	 */
	public OpenProject(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("open-project_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("open-project_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.CTRL_MASK));
	}

	/**
	 * Constructor 2.
	 */
	public OpenProject(JFrame frame, String projectFileName) {
		this(frame);
		this.projectFileName = projectFileName;
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
			if (projectFileName != null) {
				loadProject(frame, projectFileName);
				status = 0;

			} else {

				// JFileChooser chooser = new JFileChooser(
				// Settings.getProperty("project.path",
				// PathManager.getInstallDir ()));

				// fc-15.12.2017 Added CheckingFileChooser, checks the listed
				// files : are they loadable projects ?
				JFileChooser chooser = new CheckingFileChooser(Settings.getProperty("project.path",
						PathManager.getInstallDir())) {

					/**
					 * Throw an exception if the file can not be opened (i.e. is
					 * not a loadable project)
					 */
					@Override
					public void check(File f) throws Exception {
						String fileName = f.getAbsolutePath();
						
						try {
							Reader reader = SerializerFactory.getReader(fileName);
							Project project = (Project) reader.readObject();
							reader.testCompatibility();
							
						} catch (java.io.InvalidClassException e) {
							// Revision error ?
							Engine engine = Engine.getInstance ();
							
							String message = Translator.swap("Engine.revisionError") + ". " + engine.getApplicationName() + " "
									+ engine.getVersionAndRevision() + " "
									+ Translator.swap("Engine.canNotOpenThisProjectFile") + ": " + fileName;
							try {
								// Try to know what versionAndRevision of the
								// app can reopen the project
								ProjectIdCard pic = new ProjectIdCard(new File(fileName));
								message += "\n" + Translator.swap("Engine.youMayOpenThisProjectWithThisVersion") + ": "
										+ pic.getVersion();
							} catch (Exception e2) {
								message += "\n" + Translator.swap("Engine.couldNotReadTheVersionInTheProjectFile");
							}
							throw new Exception(message, e);
							
						} catch (Throwable t) {
							// Other exception / throwable
							throw t;
						}
						
					}
				};

				chooser.setMultiSelectionEnabled(true);
				chooser.setDialogType(JFileChooser.OPEN_DIALOG);
				chooser.setApproveButtonText(Translator.swap("OpenProject.open"));
				chooser.setDialogTitle(Translator.swap("OpenProject.openProjectTitle"));

				// new FileAccessory(chooser);

				int returnVal = chooser.showDialog(frame, null); // null :
																	// approveButton
																	// text was
																	// already
																	// set

				if (returnVal == JFileChooser.APPROVE_OPTION) {
					Settings.setProperty("project.path", chooser.getSelectedFile().getParent());

					for (File f : chooser.getSelectedFiles()) {
						loadProject(frame, f.getPath());
					}
					status = 0;
				} else {
					status = 1;
				}

			}
		} catch (Throwable e) { // Catch Errors in every command (for
								// OutOfMemory)
			Log.println(Log.ERROR, "OpenProject.execute ()", "An Exception/Error occured"); // no
																							// dump,
																							// already
																							// done
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			status = 2;
		}
		return status;
	}

	/**
	 * Loads a project.
	 */
	static public void loadProject(final Object parent, final String completeFileName) throws Exception {

		// A task is needed to properly report the StatusDispatcher messages
		Task<Object, Void> t = new Task<Object, Void>(Translator.swap("OpenProject.openProjectName")) {

			@Override
			protected Object doInWorker() {
				try {
					StatusDispatcher.print(Translator.swap("OpenProject.loadingProject") + " : " + completeFileName
							+ "...");

					// Ask the engine to load the project
					Project project = Engine.getInstance().processOpenProject(completeFileName);
					if (project != null) {
						// Add project to session
						Session session = Engine.getInstance().getSession();
						session.addProject(project);
						Step root = (Step) project.getRoot();
						if (root == null) {
							Log.println(Log.ERROR, "OpenProject.loadProject ()", "Project root is null, load aborted");
							throw new Exception("Project root is null");
						}
					}
					return project; // correct end, return the project

				} catch (Exception e) {
					return e; // if exception, return it
				}

			}

			@Override
			protected void doInEDTafterWorker() {
				try {
					Object r = get();
					if (r instanceof Project) {
						Project project = (Project) r;
						Step root = (Step) project.getRoot();

						// Current.getInstance ().setStep (root); fc-25.4.2013
						// this line was moved below

						// Enable the project level commands
						CommandManager.getInstance().setCommandsEnabled(CommandManager.Level.PROJECT, true);

						// fc-25.4.2013 MOVED this line AFTER setCommandsEnabled
						// () to fix the bug[Simeo: new
						// project Lollymangrove results in an enabled
						// TwistManually command when it should be
						// disabled]
						Current.getInstance().setStep(root);

						StatusDispatcher.print(Translator.swap("OpenProject.projectLoaded") + " : " + completeFileName);

					} else {
						Exception e = (Exception) r;
						// Do not dump the exception in the Log,
						// processOpenProject () already did
						MessageDialog.print(parent, Translator.swap("OpenProject.errorWhileOpeningProject"), e);
						StatusDispatcher.print(Translator.swap("OpenProject.ready"));

					}
				} catch (Exception e) { // should never happen
					Log.println(Log.ERROR, "OpenProject.loadProject ()",
							"'Should never happen' exception in doInEDTafterWorker (), passed", e);
				}

			}

		};

		t.setIndeterminate();
		TaskManager.getInstance().add(t);

		// try {
		// StatusDispatcher.print (Translator.swap
		// ("OpenProject.loadingProject")
		// +" : "+completeFileName+"...");
		//
		// // Ask the engine to load the project
		// Project project = Engine.getInstance ()
		// .processOpenProject (completeFileName);
		// if (project != null) {
		// // Add project to session
		// Session session = Engine.getInstance ().getSession ();
		// session.addProject (project);
		// Step root = (Step) project.getRoot ();
		// if (root == null) {
		// Log.println (Log.ERROR, "OpenProject.loadProject ()",
		// "Project root is null, load aborted");
		// throw new Exception ("Project root is null");
		// }
		//
		// // Update Current
		// Current.getInstance ().setStep (root);
		//
		// // Enable the project level commands
		// CommandManager.getInstance
		// ().setCommandsEnabled(CommandManager.Level.PROJECT, true);
		// CommandManager.getInstance ().updateFrameTitle ();
		//
		// StatusDispatcher.print (Translator.swap
		// ("OpenProject.projectLoaded")+" : "+completeFileName);
		//
		// }
		//
		// } catch (Exception e) {
		// // Do not dump the exception in the Log, processOpenProject ()
		// already did
		// MessageDialog.print (parent, Translator.swap
		// ("OpenProject.errorWhileOpeningProject"),
		// e);
		// StatusDispatcher.print (Translator.swap ("OpenProject.ready"));
		// throw e;
		// }

	}

	public int getStatus() {
		return status;
	}

}
