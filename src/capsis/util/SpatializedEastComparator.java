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

import java.util.Comparator;

import jeeb.lib.util.Spatialized;


/**
 * Comparator for spatialized objects to represent them in side view 
 * from the east.
 * 
 * @author F. de Coligny - march 2002
 */
public class SpatializedEastComparator implements Comparator {

	public int compare (Object o1, Object o2) throws ClassCastException {
		if (!(o1 instanceof Spatialized)) {
				throw new ClassCastException ("SpatializedEastComparator: o1 is not a Spatialized : "+o1);}
		if (!(o2 instanceof Spatialized)) {
				throw new ClassCastException ("SpatializedEastComparator: o2 is not a Spatialized : "+o2);}
				
		double x1 = ((Spatialized) o1).getX ();
		double x2 = ((Spatialized) o2).getX ();
		
		if (x1 < x2) {
			return -1;		// t1 > t2
		} else if (x1 > x2) {
			return 1;		// t1 > t2
		} else {
			return 0;		// t1 == t2
		}
	}

}

