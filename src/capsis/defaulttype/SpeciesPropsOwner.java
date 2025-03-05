/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2012 INRA 
 * 
 * Author: F. de Coligny
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

import java.util.List;

/**
 * An interface for objects with Species level properties inside. This interface
 * may be used for MultipartScenes made of SceneParts with various species
 * inside. It can help draw diagrams at the species level.
 * 
 * @author F. de Coligny - January 2012
 */
public interface SpeciesPropsOwner {

	/**
	 * Returns the list of species names of this object. E.g. "Spruce", "Fir"
	 */
	public List<String> getSpeciesNames();

	/**
	 * Returns the list of property names this object can handle for the
	 * different species. E.g. "BasalArea", "Number"
	 */
	public List<String> getPropNames();

	/**
	 * Returns the value for the given species and the given property. E.g.
	 * value for "Spruce" of "BasalArea"
	 */
	public double getValue(String speciesName, String propName);

}
