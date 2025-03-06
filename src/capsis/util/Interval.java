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


/**
 * Interval contains two double values : a and b.
 * 
 * @author F. de Coligny - july 2000 
 */
public class Interval {
	public double a;
	public double b;

	public Interval (double x, double y) {
		a = x;
		b = y;
	}
	public Interval (int x, int y) {
		a = (double) x;
		b = (double) y;
	}
	public Interval (Double x, Double y) {
		this (x.doubleValue (), y.doubleValue ());
	}
	public Interval (Integer x, Integer y) {
		this (x.intValue (), y.intValue ());	
	}
	public Interval (Interval model) {
		a = model.a;
		b = model.b;
	}

	/**
	 * Modifies a or b to include the value in the Interval.
	 */
	public void insert (int value) {
		insert ((double) value);
	}
	public void insert (double value) {
		if (value < a) {
			a = value;
		}
		if (value > b) {
			b = value;
		}
	}

	/**
	 * Enlarges the Interval with minimum variation between a and b.
	 */
	public void enlarge (int minVariation) {
		enlarge ((double) minVariation);
	}
	public void enlarge (double minVariation) {
		double variation = b - a;
		if (variation < minVariation) {
			double halfVariation = variation / 2;
			double halfMinVariation = minVariation / 2;
			a = a + halfVariation - halfMinVariation;
			b = b - halfVariation + halfMinVariation;
		}
	}

	// Creates a new interval containing the two interval parameters.
	public static Interval adjust (Interval r1, Interval r2) {
		if (r1 == null && r2 == null) {
			return new Interval (0, 1);
		} else {
			if (r1 == null) {
				return r2;
			} else {
				if (r2 == null) {
					return r1;
				} else {
					Interval r = new Interval (Math.min (r1.a, r2.a), Math.max (r1.b, r2.b));
					return r;
				}
			}
		}
	}

	/**
	 * Returns true if a == b.
	 */
	public boolean isNull () {
		if (a == b) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Returns variation.
	 */
	public double getVariation () {
		return b - a;
	}

	/**
	 * Shifts an interval (right or left).
	 */
	public void shift (int value) {
		a += value;
		b += value;
	}
	public void shift (double value) {
		a += value;
		b += value;
	}

	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("[");
		b.append (this.a);
		b.append (", ");
		b.append (this.b);
		b.append ("]");
		
		return b.toString ();
	}
}

