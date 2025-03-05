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

package capsis.extension;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.extensiontype.ModelTool;
import capsis.gui.Pilot;

/**
 * ModelTool - Superclass of Capsis model dependent tools.
 * 
 * @author F. de Coligny - July 2001
 */
abstract public class DialogModelTool extends AmapDialog implements ModelTool {

	/**
	 * Constructor
	 */
	public DialogModelTool() {

		// fc-14.12.2020 Extension now defines getName ()
		setTitle(this.getName());
//		setTitle(ExtensionManager.getName(this));

	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate () {}

	/**
	 * From Repositionable interface.
	 */
	public void reposition() {
		Pilot.getPositioner().layOut(this);
	}

}
