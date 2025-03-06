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
import javax.swing.Icon;
import javax.swing.JFrame;
import javax.swing.KeyStroke;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.Command;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.Current;
import capsis.extensiontype.ModelTool;
import capsis.gui.DToolBox;
import capsis.kernel.GModel;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * Command ToolBox.
 * 
 * @author F. de Coligny -july 2001
 */
public class ToolBox extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("MainFrame.toolBox");

	private JFrame frame;
	private Step step;

	/**
	 * Constructor 1
	 */
	public ToolBox (JFrame frame, Step step) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("tool-box_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("tool-box_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		// this.putValue (Action.SHORT_DESCRIPTION, Translator.swap
		// ("NewProject.newProject"));
		this.putValue (Action.ACCELERATOR_KEY, KeyStroke
				.getKeyStroke (KeyEvent.VK_B, ActionEvent.CTRL_MASK));
		// this.putValue (Action.MNEMONIC_KEY, 'N');
		this.step = step;
	}

	/**
	 * Constructor 2
	 */
	public ToolBox (JFrame frame) {
		this (frame, null);
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
			Step wStep = step; // step is not altered (if null, stays null)

			// 1. In case default constructor was used...
			try {
				if (wStep == null) {
					wStep = Current.getInstance ().getStep ();
				}
			} catch (Exception e) {
				Log.println (Log.ERROR, "ToolBox.execute ()", "Exception caught.", e);
				return 1;
			}

			// 2. Check if model has specific tools
			GModel model = wStep.getProject ().getModel ();
			ExtensionManager em = CapsisExtensionManager.getInstance ();

			if (!em.hasExtensions (CapsisExtensionManager.MODEL_TOOL, model)) {
				MessageDialog.print (this, Translator.swap ("ToolBox.modelHasNoModelTools"));
				// fc - 26.10.2007 - compatibility can now be changed with
				// vetoes and lists -> do not disable
				// ~ MainFrame.getInstance ().getAction ("ToolBox").setEnabled
				// (false); // fc - 25.11.2003
				return 2;
			}

			DToolBox dlg = new DToolBox (model);
			if (dlg.isValidDialog ()) {

				String className = dlg.getModelToolClassName ();

				// Load an extension of type: the one chosen by user
				try {
					ModelTool mt = (ModelTool) em
							.loadInitData (className, new GenericExtensionStarter ("model", model,
									"step", wStep));
					mt.init (model, wStep);

				} catch (Exception e) {
					MessageDialog
							.print (this, Translator.swap ("ToolBox.modelToolCanNotBeLoaded"), e);
					Log.println (Log.ERROR, "ToolBox.execute ()", "Exception", e);
					return 3;
				}

			}

		} catch (Throwable e) { // Catch Errors in every command (for
								// OutOfMemory)
			Log.println (Log.ERROR, "ToolBox.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
		}
		return 0;
	}

}
