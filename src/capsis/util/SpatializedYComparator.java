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
 * Comparator on x field for Spatialized.
 * The compare method deals with two instances of Spatialized.
 * If ascending, Returns -1 if t1.getY () < t2.getY (),
 * returns +1 if t1.getY () > t2.getY (),
 * returns 0 otherwise (==).
 *
 * @author F. de Coligny - september 2007
 */
public class SpatializedYComparator implements Comparator {

	boolean ascending;


	public SpatializedYComparator (boolean a) {ascending = a;}
	
	public SpatializedYComparator () {this (true);}

	public int compare (Object o1, Object o2) throws ClassCastException {
		if (!(o1 instanceof Spatialized)) {
				throw new ClassCastException ("Object is not a Spatialized : "+o1);}
		if (!(o2 instanceof Spatialized)) {
				throw new ClassCastException ("Object is not a Spatialized : "+o2);}
		
		double d1 = ((Spatialized) o1).getY ();
		double d2 = ((Spatialized) o2).getY ();
		
		if (d1 < d2) {
			if(!ascending) return 1;		// for descending order
			else return -1;							// t1 < t2
		} else if  (d1 > d2) {
			if(!ascending) return -1;		// for descending order
			else return 1;							// t1 > t2
		} else {
			
			double d3 = ((Spatialized) o1).getX ();
			double d4 = ((Spatialized) o2).getX ();
			
			if (d3 < d4) {
				if(!ascending) return 1;		// for descending order
				else return -1;							// t1 < t2
			} else if  (d3 > d4) {
				if(!ascending) return -1;		// for descending order
				else return 1;							// t1 > t2
			} else {
				return 0;		// t1 == t2
			}
		}
	}

}

