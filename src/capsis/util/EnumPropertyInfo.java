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
import java.util.HashMap;
import java.util.Map;

/**
 * Information about a set of qualitative properties sharing the same
 * propertyName and built together (see EnumProperty constructor).
 * 
 * @author F. de Coligny - February 2003
 */
public class EnumPropertyInfo implements Serializable {

	protected String propertyName; // e.g. "Species"
	protected Map valueToName; // e.g. 1 - "Beech"
	protected Map valueToProperty; // e.g. 1 - reference to the beech
									// QualitativeProperty

	/**
	 * Constructor
	 */
	public EnumPropertyInfo(String propertyName) {
		this.propertyName = propertyName;
		valueToName = new HashMap();
		valueToProperty = new HashMap();
	}

	/**
	 * Returns the property name
	 */
	public String getPropertyName() {
		return propertyName;
	}

	/**
	 * Returns the Map value -> name
	 */
	public Map getValueToName() {
		return valueToName;
	}

	/**
	 * Retus the Map value -> reference to the property with this value
	 */
	public Map getValueToProperty() {
		return valueToProperty;
	}

}
