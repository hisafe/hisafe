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

import capsis.defaulttype.Tree;

/**
 * Comparator on height instance variable for Trees. The compare method deals
 * with two instances of Tree. Returns -1 if t1.getHeight () < t2.getHeight (),
 * returns +1 if t1.getHeight () > t2.getHeight (), returns 0 otherwise (==).
 * 
 * @author F. de Coligny - january 2001 / june 2005 : ascending
 */
public class TreeHeightComparator implements Comparator {

	private boolean ascending;

	public TreeHeightComparator() {
		this.ascending = true;
	}

	public TreeHeightComparator(boolean ascending) {
		this.ascending = ascending;
	}

	public int compare(Object o1, Object o2) throws ClassCastException {
		if (!(o1 instanceof Tree)) {
			throw new ClassCastException("Object is not a Tree : " + o1);
		}
		if (!(o2 instanceof Tree)) {
			throw new ClassCastException("Object is not a Tree : " + o2);
		}

		double h1 = ((Tree) o1).getHeight();
		double h2 = ((Tree) o2).getHeight();
		double id1 = ((Tree) o1).getId();
		double id2 = ((Tree) o2).getId();
		
		// fc-6.5.2019 Added these checks for Forceps
		if (Double.isNaN(h1))
			throw new ClassCastException("TreeHeightComparator Error, comparison is impossible, tree "+id1+" has a NaN height");
		if (Double.isNaN(h2))
			throw new ClassCastException("TreeHeightComparator Error, tree "+id2+" has a NaN height");

		// fc-7.9.2015 Error in SVSimple with an Heterofor stand: Comparison
		// method violates its general contract! 
		// -> added the extra tests on ids
		if (h1 < h2) {
			return ascending ? -1 : 1; // asc : t1 < t2
		} else if (h1 > h2) {
			return ascending ? 1 : -1; // asc : t1 > t2
		} else {
			if (id1 < id2) { // fc-7.9.2015
				return ascending ? -1 : 1; // asc : t1 < t2
			} else if (id1 > id2) { // fc-7.9.2015
				return ascending ? 1 : -1; // asc : t1 > t2
			} else {
				return 0; // t1 == t2
			}
		}
	}

}
