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

import java.io.Serializable;
import java.util.Comparator;

import capsis.defaulttype.Tree;

/**
 * Comparator on Dbh and Id for Tree objects.
 * The compare method deals with two instances of Tree. 
 * First comparison value : dbh. Second comparison value (if dbh ==) : id.
 * returns -1 if t1 < t2, 0 if t1 == t2, 1 if t1 > t2.
 * 
 * @author F. de Coligny - 11.2.2002
 */
public class TreeDbhThenIdComparator implements Comparator, Serializable {
	
	boolean ascending;	// fc - 23.9.2004 - added ascending / descending


	public TreeDbhThenIdComparator (boolean a) {ascending = a;}
	
	public TreeDbhThenIdComparator () {this (true);}

	public int compare (Object o1, Object o2) throws ClassCastException {
		if (!(o1 instanceof Tree)) {
				throw new ClassCastException ("o1 is not a Tree instance: "+o1);}
		if (!(o2 instanceof Tree)) {
				throw new ClassCastException ("o2 is not a Tree instance: "+o2);}
				
		double d1 = ((Tree) o1).getDbh ();
		double d2 = ((Tree) o2).getDbh ();
		int id1 = ((Tree) o1).getId ();
		int id2 = ((Tree) o2).getId ();
		
		if (d1 < d2) {
			return ascending ? -1 : 1;		// t1 < t2
		} else if (d1 > d2) {
			return ascending ? 1 : -1;		// t1 > t2
		} else {
		
			if (id1 < id2) {
				return ascending ? -1 : 1;		// t1 < t2
			} else if (id1 > id2) {
				return ascending ? 1 : -1;		// t1 > t2
			} else {
				return 0;		// t1 == t2
			}
		}
	}

}
