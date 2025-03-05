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
package capsis.extensiontype;

import java.awt.Window;

import jeeb.lib.defaulttype.Extension;

/**
 * The super interface for all generic tools.
 * 
 * @author F. de Coligny - December 1999
 */
public interface GenericTool extends Extension {

	/**
	 * GenericTools often open a JDialog. This dialog must have its owner window
	 * correctly set to avoid focus and visibility issues. This involves that
	 * GenericTools can not be JDialogs by themselves because the extension
	 * architecture states that they must be constructed with a default
	 * constructor (they can not be passed a Window owner at construction time).
	 * GenericTools can open the dialog in their init (Window) method.
	 */
	public void init (Window window) throws Exception;

}
