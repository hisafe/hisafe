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

import java.io.Serializable;
import java.util.Collection;

import capsis.kernel.GScene;

/**
 * Grouper make groups at runtime.
 * 
 * @author F. de Coligny - march 2004
 */
public interface Grouper extends Serializable {

	/**
	 * Returns a copy of this grouper. Groupers may be used in several threads,
	 * copies are needed to avoid concurrence problems.
	 */
	public Grouper getCopy();

	/**
	 * Name of the grouper.
	 */
	public String getName();

	/**
	 * Tell if the grouper can be used on this referent. Referent is a
	 * Collection. Parameter is an Object to comply with Capsis general
	 * matchWith () methods design.
	 */
	public boolean matchWith(Object referent);

	/**
	 * Apply the grouper on this collection and return the resulting sub
	 * collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply(Collection c);

	/**
	 * Apply the grouper possibly in complementary mode on this collection and
	 * return the resulting sub collection.
	 */
	// fc - 8.9.2005 - introduced GroupCollection
	public GroupCollection apply(Collection c, boolean not);

	/**
	 * Return the type of the individuals this grouper can handle.
	 */
	public GroupableType getType();

	// fc - 16.4.2007 - change result to the complementary
	public boolean isNot(); // fc - 16.4.2007

	public void setNot(boolean not); // fc - 16.4.2007

	/**
	 * Returns the area (m2) of the given scene. Default is scene.getArea () but
	 * the grouper may have a different answer, see Samsa2CoreTreeGrouper.
	 */
	public double getArea(GScene scene); // fc-2.6.2020

}
