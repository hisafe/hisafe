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
 * Comparator on diameter instance variable for Trees.
 * The compare method deals with two instances of Tree.
 * If ascending, returns -1 if t1.getDbh() < t2.getDbh(),
 * returns +1 if t1.getDbh() > t2.getDbh(),
 * returns 0 otherwise (==).
 *
 * @author Ph. Dreyfus - February 2001 (derived from TreeHeightComparator from F. de Coligny - january 2001)
 */
public class TreeDbhComparator implements Comparator {

	/**
	 * True: ascending order, false: descending order. 
	 */
	boolean ascending;

	/**
	 * Default constructor.
	 */
	public TreeDbhComparator() {
		this(true);
	}

	/**
	 * Constructor.
	 */
	public TreeDbhComparator(boolean a) {
		ascending = a;
	}

	@Override
	public int compare(Object o1, Object o2) throws ClassCastException {

		if (!(o1 instanceof Tree)) {
				throw new ClassCastException ("Object is not a Tree: " + o1);}

		if (!(o2 instanceof Tree)) {
				throw new ClassCastException ("Object is not a Tree: " + o2);}

		double d1 = ((Tree) o1).getDbh();
		double d2 = ((Tree) o2).getDbh();

		if (d1 < d2) {
			if (!ascending) return 1;		// for descending order
			else return -1;					// t1 < t2
		} else if (d1 > d2) {
			if (!ascending) return -1;		// for descending order
			else return 1;					// t1 > t2
		} else {
			return 0;						// t1 == t2
		}
	}

}

