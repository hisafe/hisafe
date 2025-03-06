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
import java.util.Map;

/**
 * A property which takes one out of a list of known possible values. Instances
 * of QualitativeProperty with the same propertyName are linked together, see
 * EnumProperty constructor.
 * 
 * @author F. de Coligny - October 2000
 */
public interface QualitativeProperty extends Cloneable, Serializable {

	/**
	 * Property name, e.g. Species
	 */
	public String getPropertyName();

	/**
	 * Value of the property, e.g. 0
	 */
	public int getValue();

	/**
	 * Name matching the value, e.g. Spruce
	 */
	public String getName();

//	/**
//	 * Platform range unique name (better than name, not ambiguous, to be
//	 * preferred), e.g. Spruce (Samsara2), see EnumProperty subclass
//	 */
//	public String getUniqueName();

	/**
	 * Possible values for all the properties for a given propertyName, value ->
	 * name, e.g. for Species: O -> Spruce, 1 -> Beech
	 */
	public Map getValues();

}
