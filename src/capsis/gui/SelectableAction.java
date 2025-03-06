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

package capsis.gui;

import java.awt.event.ActionEvent;

import javax.swing.Icon;
import javax.swing.JToggleButton;

import jeeb.lib.util.Command;
import jeeb.lib.util.Log;
import capsis.util.ActionHolder;
import capsis.util.CommandAction;
import capsis.util.SelectableCommand;
import capsis.util.SelectableMenuItem;

/**
 * SelectableAction can be controlled (selected / deselected) 
 * by program with setSelected (boolean).
 *
 * @author F. de Coligny - May 2000
 */
public class SelectableAction extends CommandAction {
	private boolean selected;


	/**
	 * Constructor does not execute command. If needed,
	 * trigger the action with setSelected (boolean) and command will be executed.
	 */
	public SelectableAction (String title, Icon icon, Command command, ActionHolder holder) {
		super (title, icon, command, holder);
		selected = false;	// command is not executed at construction
	}

	/**
	 * Command checkboxes can select the action by sending an event.
	 */
	public void actionPerformed (ActionEvent evt) {
		try {
			Object source = evt.getSource ();
			if (source instanceof SelectableMenuItem) {
				// The event source is a SelectableMenuItem, subclass of
				// JCheckBoxMenuItem. getState () is true if the item is checked.
				SelectableMenuItem s = (SelectableMenuItem) source;
				setSelected (s.getState ());	
			} else {
				// Source may be a single JCheckbox (...)
				JToggleButton b = (JToggleButton) source;
				setSelected (b.isSelected ());
			}
		} catch (Exception e) {}	// soure type may be unknown -> no effect
	}

	/**
	 * Select/unselect the selectable action (ex: from a checkbox).
	 */
	public void setSelected (boolean s) {
		selected = s;
		
		// fire a PropertyChangeEvent
		setEnabled (!isEnabled ());		// to trigger a PropertyChangeEvent
		setEnabled (!isEnabled ());		// to trigger a PropertyChangeEvent
		
		SelectableCommand c = (SelectableCommand) getCommand ();
		if(c != null) {
			c.setSelected (selected);	// selects the command as the action
			try {
				c.execute ();
			} catch (Exception e) {		// fc - 5.12.2008
				Log.println (Log.ERROR, "SelectableAction.setSelected ()", "Exception", e);	
			}
		}
	}

	public boolean isSelected () {
		return selected;
	}


}

