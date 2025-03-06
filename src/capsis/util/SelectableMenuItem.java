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

import java.awt.event.ActionListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JCheckBoxMenuItem;

import capsis.gui.SelectableAction;

/**
 * SelectableMenuItem is a JCheckBoxMenuItem with facilities to init
 * and trigger the actions by program.
 * 
 * @author F. de Coligny - may 1999
 */
public class SelectableMenuItem extends JCheckBoxMenuItem implements PropertyChangeListener {
	private SelectableAction action;

	public SelectableMenuItem (String str, SelectableAction act) {
		super (str);
		action = act;
		setEnabled (action.isEnabled ());
		setState (action.isSelected ());
		addActionListener ((ActionListener) action);
		action.addPropertyChangeListener (this);	// listen to action to check enabled and selected
	}

	public void propertyChange (PropertyChangeEvent evt) {
		setEnabled (action.isEnabled ());
		setState (action.isSelected ());
	}

	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("SelectableMenuItem[selected=");
		b.append (isSelected ());
		b.append (",state=");
		b.append (getState ());
		b.append ("]");
		return b.toString ();
	}

}