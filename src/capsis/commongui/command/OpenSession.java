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
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.Current;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;
import capsis.kernel.Step;

/**
 * Command OpenSession.
 * 
 * @author F. de Coligny - october 2000, april 2010
 */
public class OpenSession extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}
	static private String name = Translator.swap("OpenSession.openSession");

	private JFrame frame;

	private String sessionFileName;
	private int status; // 0=finished correctly

	/**
	 * Constructor 1
	 */
	public OpenSession(JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("open-session_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("open-session_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// ~ this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("OpenSession.openSession"));
		this.putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_O, ActionEvent.SHIFT_MASK));
		// ~ this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Constructor 2
	 */
	public OpenSession(JFrame frame, String sessionFileName) {
		this(frame);
		this.sessionFileName = sessionFileName;
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
			if (sessionFileName != null) {
				loadSession(sessionFileName);

			} else {

				JFileChooser chooser = new JFileChooser(
						Settings.getProperty("session.path", PathManager.getInstallDir()));
				chooser.setDialogType(JFileChooser.OPEN_DIALOG);
				chooser.setApproveButtonText(Translator.swap("OpenSession.open"));
				chooser.setDialogTitle(Translator.swap("OpenSession.openSession"));

				new FileAccessory(chooser);

				int returnVal = chooser.showDialog(frame, null); // null :
																	// approveButton
																	// text was
																	// already
																	// set

				if (returnVal == JFileChooser.APPROVE_OPTION) {
					Settings.setProperty("session.path", chooser.getSelectedFile().getParent());
					loadSession(chooser.getSelectedFile().getPath());
					status = 0;
				} else {
					status = 1;
				}
			}
		} catch (Throwable e) { // Catch Errors in every command (for
								// OutOfMemory)
			Log.println(Log.ERROR, "OpenSession.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			status = 1;
			return 1;
		}
		return 0;
	}

	/**
	 * Load a session file
	 */
	private void loadSession(String completeFileName) throws Exception {
		StatusDispatcher.print(Translator.swap("OpenSession.loadingSession") + " " + completeFileName + " ...");

		Engine engine = Engine.getInstance();

		// Close previous session
		if (engine.getSession() != null) {
			new CloseSession(frame).execute();
		}

		// Load the given session
		try {
			StringBuffer report = new StringBuffer(); // messages if trouble
			Session se = engine.processOpenSession(completeFileName, report);

			if (se != null) {
				engine.setSession(se);

				if (!se.getProjects().isEmpty()) {

					// fc-25.4.2013 MOVED this line BEFORE Current.getInstance
					// ().setStep () to fix the bug[Simeo: new
					// project Lollymangrove results in an enabled TwistManually
					// command when it should be
					// disabled]
					CommandManager.getInstance().setCommandsEnabled(CommandManager.Level.PROJECT, true);

					// Set a Current step
					for (Project p : se.getProjects()) {
						Step s = (Step) p.getRoot();
						Current.getInstance().setStep(s);

						break; // fc_25.4.2013 added break: one single current
								// step is needed
					}

					// fc-25.4.2013 MOVED upper
					// CommandManager.getInstance ()
					// .setCommandsEnabled (CommandManager.Level.PROJECT, true);
				}

				CommandManager.getInstance().updateFrameTitle();

				StatusDispatcher.print(Translator.swap("OpenSession.sessionLoaded"));
			}

			// Notify the user if report is not empty
			if (report.length() != 0) {
				MessageDialog.print(frame, report.toString());
			}

		} catch (Exception e) {
			// Do not dump the exception in the Log, processOpenSession ()
			// already did
			MessageDialog.print(frame, Translator.swap("OpenSession.errorWhileOpeningSession"), e);
			StatusDispatcher.print(Translator.swap("OpenSession.ready"));
			throw e;

		}

		// // THIS EXCEPTION list should be simplified in a single 'catch
		// (Exception e)'
		// } catch (FileNotFoundException exc) {
		// MessageDialog.print (frame, Translator.swap
		// ("OpenSession.errorWhileOpeningSession")
		// +"\n"
		// +Translator.swap ("OpenSession.fileNotFound")
		// +" : "+completeFileName);
		// StatusDispatcher.print (Translator.swap ("OpenSession.ready"));
		// throw exc;
		//
		// } catch (InvalidClassException exc) {
		// Log.println (Log.ERROR, "OpenSession.loadSession ()",
		// "Can not open this session", exc);
		// MessageDialog.print (frame, Translator.swap
		// ("OpenSession.errorWhileOpeningSession")
		// +"\n"
		// +Translator.swap ("OpenSession.capsis")
		// +" "+Engine.getVersion ()+" "
		// +Translator.swap ("OpenSession.canNotOpenThisSession")+"\n"
		// +report.toString (), exc); // some projects not found...
		// StatusDispatcher.print (Translator.swap ("OpenSession.ready"));
		// throw exc;
		//
		// // [ThierrySardin] not a session file ?
		// } catch (ClassCastException exc) {
		// Log.println (Log.ERROR, "OpenSession.loadSession ()",
		// "Seems not to be a correct session file", exc);
		// MessageDialog.print (frame, Translator.swap
		// ("OpenSession.errorWhileOpeningSession")
		// +"\n"
		// +Translator.swap ("OpenSession.seemsNotToBeACorrectSessionFile"));
		// StatusDispatcher.print (Translator.swap ("OpenSession.ready"));
		// throw exc;
		//
		// } catch (Exception exc) {
		// Log.println (Log.ERROR, "OpenSession.loadSession ()",
		// "Error while opening session",
		// exc);
		// MessageDialog.print (frame,
		// Translator.swap ("OpenSession.errorWhileOpeningSession"), exc);
		// StatusDispatcher.print (Translator.swap ("OpenSession.ready"));
		// throw exc;
		// }

	}

	public int getStatus() {
		return status;
	}

}
