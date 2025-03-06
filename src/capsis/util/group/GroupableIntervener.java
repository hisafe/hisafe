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

import capsis.kernel.GScene;

/**
 * Marks an intervener extension as usable with groups of the mentioned type.
 * 
 * @author F. de Coligny - September 2004
 */
public interface GroupableIntervener {

	/**
	 * Returns a GroupableType, e.g. TreeList.GROUP_ALIVE_TREE
	 */
	public GroupableType getGrouperType();

	/**
	 * Tells the intervener a grouper is used to make the collection in its iit
	 * () method (optional, grouper may stay null). Should be called before init
	 * () in case init () calls getSceneArea (scene).
	 */
	public void setGrouper(Grouper grouper); // fc-8.6.2020

	/**
	 * Returns the grouper or null.
	 */
	public Grouper getGrouper(); // fc-8.6.2020

	/**
	 * Returns the area of the scene in m2, or a smaller area if a grouper is
	 * not null and has an area (e.g. a grouper based on a polygon). This area
	 * is to be used for per hectare computation.
	 */
	public double getSceneArea(GScene scene); // fc-8.6.2020

}