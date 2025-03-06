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

import javax.swing.Action;
import javax.swing.Icon;
import javax.swing.JButton;

/**
 * ActionButton - Jbutton constructed from a javax.swing.Action.
 * Action on the button throws the action. The action can be triggered by many graphical components.
 * JButton.add (Action) registers the JButton as "PropertieChangeListener" of the action,
 * so when the action is disabled (Action.setEnabled (false)), the button is disabled too.
 * 
 * @author Cay Horstmann & Gary Cornell + F. de Coligny
 */
public class ActionButton extends JButton {
	public ActionButton (Action act) {
		setText ((String) act.getValue (Action.NAME));	// the button gets the text of the action
		Icon icon = (Icon) act.getValue (Action.SMALL_ICON);
		if (icon != null) {
			setIcon (icon);
		}
		addActionListener (act);	// the action listens for the button
									// and the button listens for the changes of activation of the action
		// if the action is disabled when creating the button, set it well
		if (!act.isEnabled ()) {
			setEnabled (false);
		}
	}

}
