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

import java.awt.Component;
import java.awt.Container;
import java.awt.ContainerOrderFocusTraversalPolicy;
import java.util.Iterator;
import java.util.Vector;

/**
 *  A tool to change focus order in a Container.
 * 
 * @author James W. Cooper - 2003
 */
public class CustomFocusPolicy extends ContainerOrderFocusTraversalPolicy {
	private Vector components;
	private int index;
	
	//-----
	public CustomFocusPolicy () {
		super ();
		components = new Vector ();
		index = 0;
	}
	//-----
	public void addComponent (Component c) {
		components.addElement (c);
	}
	//-----
	public Component getFirstComponent () {
		index = 0;
		return (Component) components.elementAt (index);
	}
	//-----
	public Component getComponentAfter (Container c, Component comp) {
		Iterator iter = components.iterator ();
		boolean found = false;
		while (!found && iter.hasNext ()) {
			Component test = (Component) iter.next ();
			found = (test == comp);
		}
		if (iter.hasNext ()) {
			return (Component) iter.next ();
		} else {
			return getFirstComponent ();
		}
	}
	//-----
	public Component getLastComponent (Container c) {
		int i = components.size () - 1;
		if (i >= 0) {
			return (Component) components.elementAt (i);
		} else {
			return null;
		}
	}
	//-----
	public Component getComponentBefore (Container c, Component comp) {
		int i = 0;
		int memo = 0;
		boolean found = false;
		while (!found && i < components.size ()) {
			Component test = (Component) components.elementAt (i);
			found = (test == comp);
			memo = i;
			i++;	// fc - 7.4.2003 ;-)
		}
		if (memo > 0) {
			return (Component) components.elementAt (memo - 1);
		} else {
			return getLastComponent (null);
		}
	}
}
