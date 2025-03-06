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

import capsis.kernel.GScene;
import jeeb.lib.util.Log;

/**
 * DummyGrouper is a grouper. When applied, it returns the complete collection
 * (no effect).
 * 
 * @author F. de Coligny - April 2004
 */
public class DummyGrouper implements Grouper {

	private String name;

	private boolean not; // fc - 16.4.2007

	public boolean isNot() {
		return not;
	} // fc - 16.4.2007

	public void setNot(boolean not) {
		this.not = not;
	} // fc - 16.4.2007

	/**
	 * Constructor.
	 */
	public DummyGrouper() {
	}

	/**
	 * Returns a copy of this grouper. Groupers may be used in several threads,
	 * copies are needed to avoid concurrence problems.
	 */
	public Grouper getCopy() {
		try {
			return new DummyGrouper();
		} catch (Exception e) {
			Log.println(Log.ERROR, "DummyGrouper.getCopy ()", "Could not create a copy of this grouper", e);
			return null;
		}
	}

	/**
	 * Name of the DummyGrouper.
	 */
	public String getName() {
		return "DummyGrouper";
	}

	/**
	 * Tell if the DummyGrouper can be used on this referent.
	 */
	public boolean matchWith(Object referent) {
		return true;
	}

	/**
	 * Apply the DummyGrouper on this collection and return the resulting sub
	 * collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply(Collection c) {
		return new GroupCollection(c);
	}

	/**
	 * Apply the DummyGrouper in complementary mode on this collection and
	 * return the resulting sub collection. Never return null.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply(Collection c, boolean complementary) {
		if (c != null) { // fc - 16.4.2007
			return new GroupCollection(c);
		} else { // fc - 16.4.2007
			return new GroupCollection(); // fc - 16.4.2007
		} // fc - 16.4.2007
	}

	/**
	 * Return the type of the individuals this DummyGrouper can handle.
	 */
	public GroupableType getType() {
		return null;
//		return Group.UNKNOWN;
	}


	/**
	 * Returns the area (m2) of the given scene. Default is scene.getArea () but
	 * the grouper may have a different answer, see Samsa2CoreTreeGrouper.
	 */
	public double getArea(GScene scene) { // fc-2.6.2020
		
		return scene.getArea ();
		
	}
	
}
