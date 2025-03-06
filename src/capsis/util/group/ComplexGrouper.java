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
package capsis.util.group;

import java.util.Collection;
import java.util.HashSet;

import capsis.kernel.GScene;
import jeeb.lib.util.Log;

/**
 * A complex grouper combines two grouper with a logical AND or OR.
 * 
 * @author F. de Coligny - december 2004
 */
public class ComplexGrouper implements Grouper {

	private boolean g1Not; // if true, apply NOT g1
	private Grouper g1; // grouper 1
	private byte operator; // Group.AND or Group.OR
	private boolean g2Not; // if true, apply NOT g2
	private Grouper g2; // grouper 2

	private boolean not; // fc - 16.4.2007

	public boolean isNot() {
		return not;
	} // fc - 16.4.2007

	public void setNot(boolean not) {
		this.not = not;
	} // fc - 16.4.2007

	/**
	 * Constructor. Tests are made on the two grouper which must especially have
	 * same type.
	 */
	public ComplexGrouper(Grouper g1, byte operator, Grouper g2) throws Exception {
		this(false, g1, operator, false, g2);
	}

	public ComplexGrouper(boolean g1Not, Grouper g1, byte operator, boolean g2Not, Grouper g2) throws Exception {

		if (g1 == null) {
			throw new Exception("g1 == null");
		}
		if (g2 == null) {
			throw new Exception("g2 == null");
		}
		if (operator != Group.AND && operator != Group.OR) {
			throw new Exception("wrong operator, sould be Group.AND or OR");
		}
		if (!g1.getType().equals(g2.getType())) {
			throw new Exception("g1 and g2 must have same type, found " + g1.getType() + " and " + g2.getType());
		}
		this.g1Not = g1Not;
		this.g1 = g1;
		this.operator = operator;
		this.g2Not = g2Not;
		this.g2 = g2;

		not = false; // fc - 16.4.2007
	}

	/**
	 * Returns a copy of this grouper. Groupers may be used in several threads,
	 * copies are needed to avoid concurrence problems.
	 */
	public Grouper getCopy() {
		try {
			return new ComplexGrouper(g1Not, g1.getCopy(), operator, g2Not, g2.getCopy());
		} catch (Exception e) {
			Log.println(Log.ERROR, "ComplexGrouper.getCopy ()", "Could not create a copy of this grouper", e);
			return null;
		}
	}

	/**
	 * Tell if the grouper can be used on this referent. Referent is a
	 * Collection. Parameter is an Object to comply with Capsis general
	 * matchWith () methods design.
	 */
	public boolean matchWith(Object referent) {
		if (!(referent instanceof Collection)) {
			return false;
		}
		return g1.matchWith(referent) && g2.matchWith(referent);
	}

	/**
	 * Apply the grouper on this collection and return the resulting sub
	 * collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply(Collection c) {
		// ~ return apply (c, false);
		return apply(c, not); // fc - 16.4.2007
	}

	/**
	 * Apply the grouper possibly in complementary mode on this collection and
	 * return the resulting sub collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply(Collection c, boolean not) {
		GroupCollection c1 = g1.apply(c, g1Not);
		GroupCollection c2 = g2.apply(c, g2Not);
		Collection result = new HashSet();
		if (operator == Group.AND) {
			result.addAll(c1);
			result.retainAll(c2);
		} else if (operator == Group.OR) {
			result.addAll(c1);
			result.addAll(c2);
		} else {
			Log.println(Log.WARNING, "ComplexGrouper.apply (Collection, boolean)",
					"wrong operator : " + operator + " should be Group.AND or OR");
		}
		if (not) {
			Collection aux = new HashSet();
			aux.addAll(c);
			aux.removeAll(result);
			result = aux;
		}
		return new GroupCollection(result);
	}

	/**
	 * Return the type of the individuals this grouper can handle.
	 */
	public GroupableType getType() {
		return g1.getType(); // g2.getType () is the same
	}

	/**
	 * Name of the grouper.
	 */
	public String getName() {
		boolean g1IsComplex = g1 instanceof ComplexGrouper;
		boolean g2IsComplex = g2 instanceof ComplexGrouper;

		StringBuffer b = new StringBuffer();

		if (g1IsComplex) {
			b.append("(");
		}
		if (g1Not)
			b.append("NOT ");
		b.append(g1.getName());
		if (g1IsComplex) {
			b.append(")");
		}

		b.append(operator == Group.AND ? " AND " : " OR ");

		if (g2IsComplex) {
			b.append("(");
		}
		if (g2Not)
			b.append("NOT ");
		b.append(g2.getName());
		if (g2IsComplex) {
			b.append(")");
		}
		return b.toString();
	}

	/**
	 * Returns the area (m2) of the given scene. Default is scene.getArea () but
	 * the grouper may have a different answer, see Samsa2CoreTreeGrouper.
	 */
	public double getArea(GScene scene) { // fc-2.6.2020

		// If one of the two groupers has a restriction on area, return the
		// smallest area
		double area1 = g1.getArea(scene);
		double area2 = g2.getArea(scene);

		return Math.min(area1, area2);

	}

}
