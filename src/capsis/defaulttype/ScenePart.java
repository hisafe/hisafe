/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA 
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
package capsis.defaulttype;

import java.util.Collection;
import java.util.List;

import jeeb.lib.maps.geom.Polygon2;

/**
 * An interface for a part of a MultipartScene.
 * 
 * @author F. de Coligny - January 2011
 */
public interface ScenePart {

	/**
	 * Returns name of this ScenePart.
	 */
	public String getName();

	/**
	 * Returns a color for this ScenePart.
	 */
	public int[] getRGB(); // 3 integers in [0,255] range

	/**
	 * Returns the area (m2) of this ScenePart.
	 */
	public double getArea_m2(); // m2

	/**
	 * Returns polygon (or polygons is several) of this ScenePart.
	 */
	public List<Polygon2> getPolygons(); // one or several shape polygons
	
	/**
	 * Optional: the part may return the objects it contains (trees ?) in this
	 * list, may help for selection in GUI viewers.
	 */
	public Collection getObjectsInPart(); // by default, returns
									// Collections.EMPTY_LIST

}
