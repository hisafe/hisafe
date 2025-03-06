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

import javax.swing.AbstractAction;
import javax.swing.Icon;
import javax.swing.JFrame;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.gui.DGrouperCatalog;
import capsis.kernel.Step;

/**
 * Command BuildGrouper.
 *
 * @author F. de Coligny - october 2000
 */
public class BuildGrouper extends AbstractAction implements ActionCommand {
	static {
		IconLoader.addPath ("capsis/images");
	}
	static private String name = Translator.swap ("MainFrame.buildGrouper");
	
	private JFrame frame;
	private Step step;


	/**	Constructor 1
	 */
	public BuildGrouper (JFrame frame, Step step) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("group_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("group_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
//		this.putValue (Action.SHORT_DESCRIPTION, Translator.swap ("NewProject.newProject"));
//		this.putValue (Action.ACCELERATOR_KEY, 
//				KeyStroke.getKeyStroke (KeyEvent.VK_E, ActionEvent.CTRL_MASK));
//		this.putValue (Action.MNEMONIC_KEY, 'N');
		this.step = step;
	}
	
	
	/**	Constructor 2
	 */
	public BuildGrouper (JFrame frame) {
		this (frame, null);
	}
	
	/**	Action interface
	*/
	public void actionPerformed (ActionEvent e) {
		execute ();
	}
	
	/**	Command interface
	*/
	public int execute () {
		
		try {
			Step wStep = step;	// step is not altered (if null, stays null)
			
			// In case default constructor was used...
			try {
				if (wStep == null) {
					wStep = Current.getInstance ().getStep ();
				}
			} catch (Exception e) {
				Log.println (Log.ERROR, "BuildGrouper.execute ()", "Exception caught.", e);
				return 1;
			}
			
			// Retrieve some references
			ProjectManager pm = ProjectManager.getInstance ();
			
			DGrouperCatalog dlg = new DGrouperCatalog (wStep);
			if (dlg.isValidDialog ()) {
				pm.update ();
			}
			dlg.dispose ();
		
		} catch (Throwable e) {		// fc - 30.7.2004 - catch Errors in every command (for OutOfMemory)
			Log.println (Log.ERROR, "BuildGrouper.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			MessageDialog.print (this, Translator.swap ("Shared.commandFailed"), e);
			return 2;
		}
		return 0;
	}


}
