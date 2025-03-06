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

import jeeb.lib.util.Settings;
import capsis.gui.Pilot;
import capsis.gui.Positioner;
import capsis.util.SelectableCommand;

/**	Command Windows | Cascade.
*	@author F. de Coligny - november 2005
*/
public class WindowCascade implements SelectableCommand {
	private boolean selected;

	/**	Constructor.
	*/
	public WindowCascade () {selected = false;}

	/**	Constructor 2.
	*/
	public WindowCascade (boolean s)	{selected = s;}		// for direct command invocation

	public void setSelected (boolean s)	{selected = s;}
	
	public boolean isSelected ()		{return selected;}

	public int execute () {
		// switch cascade property true / false
		Settings.setProperty ("auto.mode.cascade", ""+selected);
		
		try {
			Positioner p = Pilot.getPositioner ();
			p.autoLayoutDesktop ();
		} catch (Exception e) {
			return 1;
		}
		return 0;
	}

}
