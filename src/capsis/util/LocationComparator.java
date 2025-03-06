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

/**	Comparator on x and y for Locations.
*	Can be used to sort tress in a viewer2Dhalf (the nearest the latest drawn)
*	@author F. de Coligny - june 2005
*/
public class LocationComparator implements Comparator {

	public static final byte X_AXIS = 1;
	public static final byte Y_AXIS = 2;
	private byte type;
	private boolean ascending;


	/**	Constructor
	*/
	public LocationComparator (byte type, boolean ascending) {
		this.type = type;
		this.ascending = ascending;
	}
	
	/**	Comparison method
	*/
	public int compare (Object o1, Object o2) throws ClassCastException {
		if (!(o1 instanceof Location)) {
				throw new ClassCastException ("Object is not a Location : "+o1);}
		if (!(o2 instanceof Location)) {
				throw new ClassCastException ("Object is not a Location : "+o2);}
		
		double d1 = 0;
		double d2 = 0;

		if (type == X_AXIS) {
			d1 = ((Location) o1).x;
			d2 = ((Location) o2).x;
		} else {
			d1 = ((Location) o1).y;
			d2 = ((Location) o2).y;
		}
		
		if (d1 < d2) {
			return (ascending) ? -1 : 1;
		} else if  (d1 > d2) {
			return (ascending) ? 1 : -1;
		} else {
			return 0;		// t1 == t2
		}
	}

}

