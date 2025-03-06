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
import java.awt.event.ActionListener;

import javax.swing.JPanel;

/**	ConfigPanel / ConfigPanelable for light configuration framework
*	based on listeners.
*
*	@author F. de Coligny - march 2006
*/
abstract public class ConfigPanel extends JPanel
		implements ActionListener {

	private ConfigPanelable subject;

	/**	Constructor, memorizes the config panelable to be
	*	notified when config changes.
	*/
	public ConfigPanel (ConfigPanelable subject) {
		this.subject = subject;
	}

	/**	Check every config option.
	*	If trouble, tell the user and return false.
	*	If everything is correct, return true.
	*/
	abstract public boolean everythingCorrect ();

	/**	When config changes, call this method to check config
	*	and, if correct, notify the ConfigPanelable.
	*/
	public void actionPerformed (ActionEvent e) {
		if (!everythingCorrect ()) {return;}

		Object source = this;
		int id = 0;	//unused
		String command = "option changed";
		ActionEvent e2 = new ActionEvent (source, id, command);
		subject.actionPerformed (e2);
	}

}


