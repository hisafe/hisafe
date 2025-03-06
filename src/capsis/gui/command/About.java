/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
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
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.gui.DAbout;
import capsis.gui.MainFrame;

/**	Command About.
 *	@author F. de Coligny - october 2000, january 2011
 */
public class About extends AbstractAction implements ActionCommand {
	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("MainFrame.helpAbout");
	
	private JFrame frame;

	/**	Constructor
	*/
	public About (JFrame frame) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("help_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("help_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
//		this.putValue (Action.SHORT_DESCRIPTION, Translator.swap ("NewProject.newProject"));
//		this.putValue (Action.ACCELERATOR_KEY, 
//				KeyStroke.getKeyStroke (KeyEvent.VK_H, ActionEvent.CTRL_MASK));
//		this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	
	/**	Action interface
	*/
	@Override
	public void actionPerformed(ActionEvent e) {
		execute ();
		
	}

	/**	Command interface
	*/
	@Override
	public int execute() {
		
		try {
			DAbout dlg = new DAbout (MainFrame.getInstance ());
			
		} catch (Throwable e) {		// fc - 30.7.2004 - catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "About.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}


}
