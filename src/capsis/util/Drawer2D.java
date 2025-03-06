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

import java.awt.Graphics;
import java.awt.Rectangle;
import java.util.Collection;

import jeeb.lib.util.InstantConfigurable;


/**	Drawer2D classes can draw side views in a Panel2D.
* 
*	@author F. de Coligny - november 2006
*/
public interface Drawer2D extends InstantConfigurable {

	/**	Next call to draw2D () should rerun init2D ().
	*/
	public void reset ();
	
	/**	init2D () should be called from draw2D () at first time and 
	*	each time the configuration changed : color...
	*/
	public void init2D (Graphics g, Rectangle.Double viewedRectangle);

	/**	Drawing delegation: should draw something in the given Graphics.
	*/
	public void draw2D (Graphics g, Rectangle.Double viewedRectangle);
	
	/**	Selection delegation: must return the collection of subjects drawn in the selectionRectangle.
	*/
	public Collection select2D (Rectangle.Double selectionRectangle);
	
	/**	Return the name of this Drawer2D to be written on a gui (translated in current language)
	*/
	public String getName ();
	
	
}

