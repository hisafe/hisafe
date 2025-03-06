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

import jeeb.lib.util.Identifiable;

/**	Comparator on Id for Identifiable objects.
*	The compare method deals with two instances of Identifiable. 
*	returns -1 if t1 < t2, 0 if t1 == t2, 1 if t1 > t2.
* 
*	@author F. de Coligny - September 2007
*/
public class IdentifiableComparator implements Comparator {
	
	boolean ascending;	// ascending / descending


	public IdentifiableComparator (boolean a) {ascending = a;}
	
	public IdentifiableComparator () {this (true);}

	public int compare (Object o1, Object o2) throws ClassCastException {
		if (!(o1 instanceof Identifiable)) {
			try {	// fc - 16.11.2007 - "ignore lines beginning by #" (comments) -> they are always smaller
				String line = o1.toString ();
				if (line.startsWith ("#")) {
					return -1;
				} else {
					throw new Exception ();
				}
			} catch (Exception e) {
				throw new ClassCastException ("o1 is not an Identifiable : "+o1);
			}
		}
		if (!(o2 instanceof Identifiable)) {
			try {	// fc - 16.11.2007 - "ignore lines beginning by #" (comments) -> they are always smaller
				String line = o2.toString ();
				if (line.startsWith ("#")) {
					return -1;
				} else {
					throw new Exception ();
				}
			} catch (Exception e) {
				throw new ClassCastException ("o2 is not an Identifiable : "+o2);
			}
		}
				
		int id1 = ((Identifiable) o1).getId ();
		int id2 = ((Identifiable) o2).getId ();
		
		if (id1 < id2) {
			return ascending ? -1 : 1;		// t1 < t2
		} else if (id1 > id2) {
			return ascending ? 1 : -1;		// t1 > t2
		} else {
			return 0;		// t1 == t2
		}
	}

}
