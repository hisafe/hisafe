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
import java.awt.geom.AffineTransform;

/**
 * A zone in a JPanel where a Drawer can draw.
 * 
 * @author F. de Coligny
 */
public class DrawerZone extends DrawingZone {
	protected Drawer drawer;

	public DrawerZone (Drawer drawer) {
		super (new AffineTransform ());
		this.drawer = drawer;
	}
	public DrawerZone (Drawer drawer, AffineTransform t) {
		super (t);
		this.drawer = drawer;
	}
	
	public void paintComponent (Graphics g) {
		super.paintComponent (g);
	////System.out.println ("DrawerZone.paintComponent ()");
		// drawing code in Drawer.draw (...)
		drawer.draw (g, new Rectangle.Double ());	// rectangle is not used here
	}
}

