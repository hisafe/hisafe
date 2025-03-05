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

package capsis.extension.dataextractor.configuration;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import capsis.kernel.AbstractSettings;
import capsis.util.group.GroupableType;
import jeeb.lib.util.DoubleKeyMap;
import jeeb.lib.util.annotation.Param;
import jeeb.lib.util.annotation.RecursiveParam;

/**
 * DataExtractors Settings.
 *
 * @author F. de Coligny - December 2000
 */
public class DESettings extends AbstractSettings {

	// fc-30.8.2021 Fixed the configuration propagation bug for boolean, radio, int,
	// double and string properties, added ListOfConfigProperties, still things to
	// see for set, combo...
	// -> the ParamMap used by ExtensionManager to save the properties for next
	// session did not manage well TreeMaps <-

	// fc-3.9.2019 REMOVED GROUPS RELATED MEMORIZATION. At reopening time, no
	// groups in configuration (too tricky if not same module / same input...)
	// -> removed several @Param below

	// fc+bc-11.1.2019 AbstractDataExtractor.settings was @Param
	// this was removed to @RecursiveParam by fc-3.8.2018
	// but extensions settings memorization was lost from this time
	// Added @Param on the fields to be saved in this class to restore extension
	// params saving

	// Common groupers management.
	// (Individual groupers configuration is stored in DataExtractor -> not
	// memorized by
	// ExtensionManager).

//	@Param // fc-3.9.2019 removed groups related memorization
	public GroupableType c_grouperType; // fc-1.7.2019

	public GroupableType i_grouperType; // fc-1.7.2019

//	@Param // fc-3.9.2019 removed groups related memorization
	public boolean c_grouperMode;

//	@Param // fc-3.9.2019 removed groups related memorization
	public String c_grouperName;

//	@Param // fc-3.9.2019 removed groups related memorization
	public boolean c_grouperNot; // fc-21.4.2004

	// General config properties (moved from DataExtractor - fc-9.10.2003)
	@Param
	public List configProperties = new ArrayList();

	/**
	 * PropertyName: String, value: boolean. A boolean property is a boolean with a
	 * name. Two cases: name: rendered by a single checkBox (name is translated),
	 * group_name: all the properties in same "group" are rendered by checkBoxes in
	 * a bordered panel named "group". E.g. visibleStepsOnly, stand_G, stand_V,
	 * stand_N, thinning_V, thinning_G... See ConfigurableExtractor, DEStandTable.
	 */
	@RecursiveParam
	public ListOfConfigProperties<Boolean> booleanProperties;

	/**
	 * PropertyName: String, value: boolean. Radio properties are represented by
	 * radio buttons. Format : group_name1, group_name2,... group_namen. The buttons
	 * are in a bordered panel named "group" (every name go through the Translator
	 * before being written on the user interface). See ConfigurableExtractor.
	 */
	@RecursiveParam
	public ListOfConfigProperties<Boolean> radioProperties;

	/**
	 * PropertyName: String, value: int. Input management for int values. See
	 * ConfigurableExtractor.
	 */
	@RecursiveParam
	public ListOfConfigProperties<Integer> intProperties;

	/**
	 * PropertyName: String, value: double. Input management for double values. See
	 * ConfigurableExtractor.
	 */
	@RecursiveParam
	public ListOfConfigProperties<Double> doubleProperties;

	/**
	 * PropertyName: String, value: list of candidate Strings. A String value in a
	 * list of possibilities. See ConfigurableExtractor.
	 */
	// fc-10.6.2004 For fg (selection of two groups)
	@Param
	private ListOfConfigProperties<LinkedList<String>> comboProperties;
//	public ListOfConfigProperties<LinkedList<String>> comboProperties; // fc-2.2.2024 searching a bug

	// fc-2.2.2024 searching a bug in compboProperties (A. Guignabert / HetTimePhenology)
	public ListOfConfigProperties<LinkedList<String>> getComboProperties () {
		System.out.println("DESettings "+hashCode()+" someone called getComboProperties ()...");
		return comboProperties;
	}
	
	/**
	 * PropertyName: String, value: String Input management for String values see
	 * ConfigurableExtractor
	 */
	@RecursiveParam
	public ListOfConfigProperties<String> stringProperties;

	/**
	 * PropertyName: String, value: a set of Strings. Input management for sets of
	 * String values.
	 * 
	 * @Ignore below means that possibleValues must not be saved / restored by the
	 *         settings memorization system fc-2.8.2018
	 * @Ignore // @Ignore is not so bad... Trying RecursiveParam to forget possible
	 *         and keep selected in setProperties see ConfigurableExtractor
	 */
	// fc-15.9.2021 This is a map, to be reviewed, ListOfConfigProperties needed ?
	// REVIEW needed: Ignore ? RecursiveParam ? TreeMap ? check memorisation /
	// restoration
	@RecursiveParam
	public Map setProperties; // = new TreeMap();

	/**
	 * Specific properties for custom configuration (e.g. Ecoaf filters). First key
	 * is the extractor reference (or null for properties common to all extractors
	 * in the dataBlock). Second key is the property name.
	 */
	// fc-3.11.2020 Changed specific properties Map for a DoubleKeyMap
	public DoubleKeyMap<ConfigurableExtractor, String, DESpecificProperty> specificProperties; // = new
																								// DoubleKeyMap<>();

	/**
	 * Constructor
	 */
	public DESettings() {

		booleanProperties = new ListOfConfigProperties<Boolean>();
		intProperties = new ListOfConfigProperties<Integer>();
		doubleProperties = new ListOfConfigProperties<Double>();
		radioProperties = new ListOfConfigProperties<Boolean>();
		comboProperties = new ListOfConfigProperties<LinkedList<String>>(); // fc-13.9.2021
		stringProperties = new ListOfConfigProperties<String>();

		// fc-15.9.2021
		setProperties = new TreeMap();
		specificProperties = new DoubleKeyMap<>();

	}

	public String toString() {
		return "- DESettings" + ", booleanProperties: " + booleanProperties + ", radioProperties: " + radioProperties
				+ ", stringProperties: " + stringProperties + ", intProperties: " + intProperties
				+ ", doubleProperties: " + doubleProperties + ", setProperties: " + setProperties
				+ ", comboProperties: " + comboProperties + " end-of-DESettings";
	}

}
