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
package capsis.commongui.command;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JFrame;

import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.kernel.Project;

/**	Redefine step visibility in the current project
 * @author S. Dufour - 2010
 */
public class ShowStep extends AbstractAction implements ActionCommand {
	static {
		IconLoader.addPath ("capsis/images");
	}
	
	private JFrame frame;
	private int frequency;

	
	/**	Constructor
	 */
	public ShowStep(JFrame frame, String name, int frequency) {
		// fc-1.10.2012 reviewing icons
		super (name);

		putValue (SMALL_ICON, IconLoader.getIcon ("show-step_16.png"));
		putValue (LARGE_ICON_KEY, IconLoader.getIcon ("show-step_24.png"));
		// fc-1.10.2012 reviewing icons

		this.frame = frame;
		this.frequency = frequency;
		this.putValue (Action.SHORT_DESCRIPTION, Translator.swap ("ShowStep.visibility")+" : "+name);
		//~ this.putValue (Action.ACCELERATOR_KEY, 
				//~ KeyStroke.getKeyStroke (KeyEvent.VK_Q, ActionEvent.CTRL_MASK));
	}

	/**	Action interface
	 */
	public void actionPerformed (ActionEvent e) {
		execute ();
	}
	
	/**	Command interface
	 */
	public int execute() {
		
		try {
			Project p = Current.getInstance ().getProject ();
			p.setVisibilityFrequency (frequency);
			ProjectManager.getInstance ().update ();
			return 0;
			
		} catch (Throwable e) {
			Log.println (Log.ERROR, "ShowStep.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
			return 1;
		}
		
	}

}
