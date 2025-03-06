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

package capsis.util;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Icon;

import jeeb.lib.util.Command;
import jeeb.lib.util.Log;

/**
 * An action linked with a command. Listens to itself and trigers itself.
 * see Command Pattern.
 * 
 * @author F. de Coligny - october 2000
 */
public class CommandAction extends AbstractAction implements CommandHolder {
	private Command command;
//	private JButton button;
	
	public CommandAction (String title, Icon icon, Command command, ActionHolder holder) {
		super (title, icon);
		setCommand (command);
		
	
		try {
//			holder.registerAction (Tools.getClassLittleName (command.getClass ().getName ()) , this);
			holder.registerAction (command.getClass ().getSimpleName () , this);
		} catch (Exception e) {}	// for subclass DummyAction
	}
	
	public void actionPerformed (ActionEvent evt) {
		//System.out.println ("- - - CommandAction.actionPerformed ()");
		try {
			getCommand ().execute ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "CommandAction.actionPerformed ()", "Exception", e);
		}
	}
	
	public void setCommand (Command c) {command = c;}
	public Command getCommand () {return command;}



}
