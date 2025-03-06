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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Vertex2d;

/**
 * SpatialIterator : a tool for neighbourhood problems solving.
 * 
 * @author F. de Coligny - february 2002
 */
public class SpatialIterator implements SpatialHelper {

	private Collection points;
	
	public SpatialIterator (Collection objects) {	// They must be instances of Vertex2d
		this.points = objects;
	}
	
	public Collection getNeighbours (Vertex2d object, double distance) {
		double x0 = object.x;
		double y0 = object.y;
		double x1 = x0 - distance;
		double x2 = x0 + distance;
		double y1 = y0 - distance;
		double y2 = y0 + distance;
		
		Collection candidates = new ArrayList ();
		for (Iterator i = points.iterator (); i.hasNext ();) {
			Vertex2d v2 = (Vertex2d) i.next ();
			if (x1 <= v2.x && v2.x <= x2 && y1 <= v2.y && v2.y <= y2) {
				candidates.add (v2);
			}
		}
		return candidates;
	}
		
	public void reset () {
		points = null;
	}

}

