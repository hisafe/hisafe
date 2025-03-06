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
import capsis.commongui.projectmanager.Current;
import capsis.gui.DStepProperties;
import capsis.kernel.Step;

/**
 * Command StepProperties.
 * 
 * @author F. de Coligny - november 2000
 */
public class StepProperties extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("MainFrame.stepProperties");
	static private Icon icon = IconLoader.getIcon ("empty_16.png"); // for alignment

	private JFrame frame;
	private Step step;

	/**
	 * Constructor 1
	 */
	public StepProperties (JFrame frame, Step step) {
		super (name, icon);
		this.frame = frame;
		// this.putValue (Action.SHORT_DESCRIPTION, Translator.swap ("NewProject.newProject"));
		// this.putValue (Action.ACCELERATOR_KEY,
		// KeyStroke.getKeyStroke (KeyEvent.VK_E, ActionEvent.CTRL_MASK));
		// this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Constructor 2
	 */
	public StepProperties (JFrame frame) {
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

			// In case default constructor was used...
			try {
				if (wStep == null) {
					wStep = Current.getInstance ().getStep ();
				}
			} catch (Exception e) {
				Log.println (Log.ERROR, "StepProperties.execute ()", "Exception caught.", e);
				return 2;
			}

			new DStepProperties (wStep);

		} catch (Throwable e) { // Catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "StepProperties.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
