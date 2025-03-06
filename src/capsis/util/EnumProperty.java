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

import java.util.Map;

import jeeb.lib.util.Translator;

/**
 * Enum property: has one value among a known set of values. Instances of
 * EnumProperty with the same propertyName are linked together.
 * 
 * @author F. de Coligny - February 2003
 */
public class EnumProperty implements QualitativeProperty {

	// fc-10.1.2019 Added uniqueName, e.g. Spruce (Samsara2)

	// Information shared by all the properties for a given propertyName
	private EnumPropertyInfo info;

	private int value; // current value

	/**
	 * Constructor.
	 * 
	 * <pre>
	 * Example :
	 * EnumProperty beech = new EnumProperty (1, "LoxModel.beech", null, "LoxModel.species");
	 * EnumProperty spruce = new EnumProperty (2, "LoxModel.spruce", beech, null);
	 * EnumProperty fir = new EnumProperty (3, "LoxModel.fir", beech, null);
	 * </pre>
	 */
	public EnumProperty(int v, String name, EnumProperty model, String propertyName) {
		if (model != null) {
			info = model.info; // share common data
		} else {
			info = new EnumPropertyInfo(propertyName);
		}

		setValue(v, name);
	}

	/**
	 * Property name, e.g. Species
	 */
	@Override
	public String getPropertyName() {
		return info.propertyName;
	}

	/**
	 * Value of the property, e.g. 0
	 */
	@Override
	public int getValue() {
		return value;
	}

	/**
	 * Name matching the value, e.g. Spruce
	 */
	@Override
	public String getName() {
		return (String) info.valueToName.get(new Integer(value));
	}

	/**
	 * Platform range name (better than name, not ambiguous, to be preferred),
	 * e.g. Spruce (Samsara2)
	 */
	public String getUniqueName() {
		// fc-10.1.2019 New definition for unique name
		return getUniqueName(getPropertyName(), getName());
	}

	/**
	 * A better unique name for enum properties, e.g. Beech (Samsara2)
	 */
	public static String getUniqueName(String propertyName, String name) {

		// fc+bc-9.1.2019 better option for a unique name, can be used for
		// grouperNames, e.g. Beech (Samsara2)
		StringBuffer grouperName = new StringBuffer(name);
		grouperName.append(' ');

		// fc-28.6.2019 added Translator in case a translation exists (e.g.
		// HetSpecies.species -> species)
		grouperName.append("(" + Translator.swap(propertyName) + ")"); // e.g.:
																		// Beech
																		// (Samsara2)
		// grouperName.append("(" + propertyName + ")"); // e.g.: Beech
		// (Samsara2)

		return "" + grouperName;
	}

	/**
	 * Returns true if the value of this property is equal to the given value
	 */
	public boolean isValue(int value) {
		return this.value == value;
	}

	/**
	 * Possible values for all the properties for a given propertyName, value ->
	 * name, e.g. for Species: O -> Spruce, 1 -> Beech
	 */
	@Override
	public Map getValues() {
		return info.valueToName;
	}

	public EnumPropertyInfo getInfo() {
		return info;
	}

	/**
	 * Set the value and name of the property.
	 */
	protected void setValue(int v, String name) {
		value = v;

		info.valueToName.put(v, name);
		info.valueToProperty.put(v, this);
	}

}
