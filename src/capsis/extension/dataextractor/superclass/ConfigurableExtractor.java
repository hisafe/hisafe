/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2021 INRA
 * 
 * Authors: F. de Coligny, N. Beudez, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */
package capsis.extension.dataextractor.superclass;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import capsis.commongui.util.Tools;
import capsis.defaulttype.TreeList;
import capsis.extension.dataextractor.configuration.DEConfigurationPanel;
import capsis.extension.dataextractor.configuration.DEMultiConfPanel;
import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extension.dataextractor.configuration.DESpecificProperty;
import capsis.extension.dataextractor.configuration.propertiesmanager.SetPropertyItemSelector;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.gui.GrouperChooser;
import capsis.kernel.Step;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.configurable.SharedConfigurable;
import capsis.util.group.Group;
import capsis.util.group.GroupableType;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.annotation.RecursiveParam;

/**
 * A superclass containing configuration features, intended to gather all
 * configuration features of a DataExtractor.
 * 
 * @author F. de Coligny - July 2021
 */
public abstract class ConfigurableExtractor implements DataExtractor, Configurable, SharedConfigurable {

	// The constants here are the old configuration properties, still running,
	// see in DESettings for the new framework
	public static final char COMMON = 'c'; // fc-13.9.2004
	public static final char INDIVIDUAL = 'i';
	public static final String STATUS = "STATUS"; // fc-22.3.2004

	// Common tree group for all the extractors in the data block
	public static final String TREE_GROUP = "TREE_GROUP";

	// Individual tree group for this extractor only
	public static final String I_TREE_GROUP = "I_TREE_GROUP";

	// Common cell group for all the extractors in the data block
	public static final String CELL_GROUP = "CELL_GROUP";

	// Individual cell group for this extractor only
	public static final String I_CELL_GROUP = "I_CELL_GROUP";

	// Individual tree id list for this extractor only
	public static final String TREE_IDS = "TREE_IDS";

	// A way to restrict to a given list of trees.
	// This concerns individual configuration: not memorized in Settings.
	// Only SharedConfigurable config options are memorized.
	// @Param // fc-3.8.2018 removed @Param to match the comment just upper
	private List<String> treeIds;

	// Individual groupers management: restrict a given extractor on a group.
	// This concerns individual configuration: not memorized in Settings.
	// Only SharedConfigurable config options are memorized.
	// @Param // fc-3.8.2018 removed @Param to match the comment just upper
	public boolean i_grouperMode;
	// @Param // fc-3.8.2018 removed @Param to match the comment just upper
	public String i_grouperName;
	// @Param // fc-3.8.2018 removed @Param to match the comment just upper
	public boolean i_grouperNot; // fc-21.4.2004

	// A way to restrict on elements with a particular status (TreeList status,
	// e.g. dead trees)
	// This concerns individual configuration: not memorized in Settings.
	// Only SharedConfigurable config options are memorized.
	protected String[] statusSelection; // fc-23.4.2004

	// Configuration properties may be disabled
	private Collection<String> disabledProperties;

	// fc-13.9.2021 Needs a review, should go into the ComboPropertyManager but this
	// implies the latter would be created at first addComboProperty () call time
	// and it only exists at DEMultiConfPanel creation time
	public Map<String, Map<String, String>> comboPropertyTranslationToValue;

	// fc-10.1.2019 list of property names for which user can not change the
	// value.
	// See e.g. CumulatedHarvestedDbhDistributionPerSpecies, we build an
	// extractor containing several extractors synchronized on various species
	// groups, user must not change these groups
	private Set<String> userActionDisabled = new HashSet<>();

	// fc-17.9.2021 Manage the order of presentation to user: order of declaration
	// in setConfigProperties (), only if propertyDeclarationOrderKept is true
	private List<String> propertyDeclarationOrder;
	private boolean propertyDeclarationOrderKept;

	// fc-2.8.2018 added @RecursiveParam, so some fields in settings may be
	// ignored (e.g. setProperties memorization is tricky and specific)
	@RecursiveParam
	protected DESettings settings;

	// fc-8.12.2023 If true, the grouping system based on the firt '_' in the
	// property name will be disabled see setPropertyGroupingInTitledPanelsVeto ()
	protected boolean propertyGroupingInTitledPanelsVeto = false; // default: grouping ok

	// Methods ---------------------------------

	/**
	 * Constructor
	 */
	public ConfigurableExtractor() {
	}

	/**
	 * An extractor may refuse to be added to a block already containing other
	 * extractors with incompatible configuration possibilities (See Cstab, has a
	 * comboProperty with possible values depending on the setup loaded file). This
	 * must be called before init ().
	 */
	@Override
	public boolean acceptsToBeAddedToThisDataBlock(DataBlock db, Step incomingInitStep) {
		return true; // default value
	}

	/**
	 * Called when the extractor is inited on a new Step
	 */
	protected void stepChanged(Step newStep) {

		// Note: This is called from init(GModel, Step)

		// fc-13.9.2021 Needs a review
		comboPropertyTranslationToValue = new HashMap<String, Map<String, String>>();

		setTreeIds(new ArrayList<String>());

	}

	/**
	 * Returns the step the extractor is synchronized on
	 */
	// Needed method (see group properties)
	abstract public Step getStep();

	/**
	 * Tell the extractor is up to date or not (e.g. configuration changed or step
	 * changed and an update is needed).
	 */
	// fc-6.9.2021 Added this to help propagate configuration properties
	abstract public void setUpToDate(boolean upToDate);

	/**
	 * Added this accessor to be able to configure the extractor in script mode.
	 */
	public DESettings getSettings() {
		return settings;
	}

	/**
	 * Returns the data block containing this extractor
	 */
	abstract public DataBlock getDataBlock();

	/**
	 * Return the list of status selected by user (e.g. "dead", "cut"). may return
	 * null if this extractor does not use STATUS property.
	 */
	public String[] getStatusSelection() {
		return statusSelection;
	}

	/**
	 * By default, all added configuration properties can be changed by user in
	 * config panels. This is a way to prevent user to change a configuration
	 * property which name is given.
	 */
	public void setUserActionEnabled(String propertyName, boolean yep) {
		// fc-10.1.2019
		if (yep)
			userActionDisabled.remove(propertyName);
		else
			userActionDisabled.add(propertyName);
	}

	/**
	 * Returns true if user is allowed to change this configuration property
	 */
	public boolean isUserActionEnabled(String propertyName) {
		// fc-10.1.2019
		boolean disabled = userActionDisabled.contains(propertyName);
		return !disabled;
	}

	/**
	 * Used in subclasses to set the config properties for this extractor, i.e. add
	 * configuration properties (addBooleanProperties, addIntProperties...) with
	 * their default value.
	 */
	abstract public void setConfigProperties();

	/**
	 * Used in subclasses to check boolean / radio properties values, returns true
	 * if the property was selected by user.
	 */
	public boolean isSet(String propertyName) {

		try {
			if (settings.booleanProperties.containsKey(propertyName) && isPropertyEnabled(propertyName)
					&& ((Boolean) settings.booleanProperties.get(propertyName)).booleanValue()) {
				return true;
			}

			if (settings.radioProperties.containsKey(propertyName) && isPropertyEnabled(propertyName)
					&& ((Boolean) settings.radioProperties.get(propertyName)).booleanValue()) {
				return true;
			}

		} catch (Exception e) {
			Log.println(Log.WARNING, "ConfigurableExtractor.isSet()",
					"Trouble while calling isSet () on property: " + propertyName + ", returned false", e);
			return false; // In case of trouble, return false
		}

		return false;
	}

	/**
	 * There can be several DataExtractors in the DataBlock. In this case, they will
	 * be drawn on the same graphics. An individual property concerns only one
	 * extractor (e.g. for several height evolution curves). A common property
	 * modifies all the extractors (e.g. per hectare calculation). Properties are
	 * recognized to be individual if their name begins with "i-". Common properties
	 * are managed in a single config panel for all the extractors
	 * (SharedConfiguration). Individual properties are managed in one panel per
	 * extractor (Configuration). Reminder: some properties have a "_" inside their
	 * name. Possible name formats: name, i-name, group_suffix, i-group_suffix.
	 * fc-25.7.2002
	 */
	public boolean isIndividualProperty(String propertyName) {
		if (propertyName.startsWith("i-")) {
			return true;
		}
		return false;
	}

	/**
	 * Used in configuration panels to add needed components.
	 */
	public boolean hasConfigProperty(String propertyName) {

		return settings.configProperties.contains(propertyName) || settings.booleanProperties.containsKey(propertyName)
				|| settings.radioProperties.containsKey(propertyName)
				|| settings.stringProperties.containsKey(propertyName)
				|| settings.intProperties.containsKey(propertyName)
				|| settings.doubleProperties.containsKey(propertyName)
				|| settings.setProperties.containsKey(propertyName)
				|| settings.getComboProperties ().containsKey(propertyName) // fc-14.10.2004
				|| settings.specificProperties.keySet2().contains(propertyName) // fc-4.7.2019, fc-3.11.2020
		;
	}

	// -----------------------------------------------------------
	// ---[ Legacy config properties ...
	// -----------------------------------------------------------

	/**
	 * Used by subclasses to add a config properties.
	 */
	public void addConfigProperty(String propertyName) {

		// fc-1.7.2019 Groups refactoring

		if (propertyName.equals(TREE_GROUP)) { // fc-13.9.2004 Short circuit
												// (see addGroupProperty ())
			addGroupProperty(TreeList.GROUP_ALIVE_TREE, COMMON);
			// addGroupProperty(TreeList.GROUP_ALIVE_TREE, COMMON);
			return;
		}
		if (propertyName.equals(I_TREE_GROUP)) { // fc-13.9.2004
			addGroupProperty(TreeList.GROUP_ALIVE_TREE, INDIVIDUAL);
			// addGroupProperty(TreeList.GROUP_ALIVE_TREE, INDIVIDUAL);
			return;
		}
		if (propertyName.equals(CELL_GROUP)) { // fc-13.9.2004
			addGroupProperty(TreeList.GROUP_PLOT_CELL, COMMON);
			// addGroupProperty(Group.CELL, COMMON);
			return;
		}
		if (propertyName.equals(I_CELL_GROUP)) { // fc-13.9.2004
			addGroupProperty(TreeList.GROUP_PLOT_CELL, INDIVIDUAL);
			// addGroupProperty(Group.CELL, INDIVIDUAL);
			return;
		}

		settings.configProperties.add(propertyName);
	}
	// ... Legacy config properties ]---------------------

	/**
	 * Memorize the property in the propertyDeclarationOrder list, called each time
	 * a property is added (mainly for the properties managed with
	 * AbstractPropertyManager instances, see DEMulticonfPanel).
	 */
	private void memoPropertyDeclarationOrder(String propertyName) {
		// fc-17.9.2021

		if (propertyDeclarationOrder == null)
			propertyDeclarationOrder = new ArrayList<>();

		// The property must be stored only once
		if (propertyDeclarationOrder.contains(propertyName))
			return;

		propertyDeclarationOrder.add(propertyName);

	}

	// -----------------------------------------------------------
	// ---[ Boolean properties ...
	// -----------------------------------------------------------

	/**
	 * Used by subclasses to add boolean properties (JCheckBox). User will be asked
	 * for a choice in some config panel.
	 */
	public void addBooleanProperty(String propertyName) {
		addBooleanProperty(propertyName, false);
	}

	public void addBooleanProperty(String propertyName, boolean defaultValue) {

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName);

		if (!settings.booleanProperties.containsKey(propertyName)) {
			settings.booleanProperties.put(propertyName, new Boolean(defaultValue));

//			memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED
		}

	}

	public void removeBooleanProperty(String propertyName) {
		if (!settings.booleanProperties.containsKey(propertyName))
			return;

		settings.booleanProperties.remove(propertyName);
	}

	// fc-27.8.2021 Added this to set programmatically the value of a boolean
	// property (optional)
	public void setBooleanProperty(String propertyName, boolean value) {

		removeBooleanProperty(propertyName);
		addBooleanProperty(propertyName, value);

	}

	// ... Boolean properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Radio properties ...
	// -----------------------------------------------------------

	/**
	 * A map property name -> group name. In case several groups of radio property
	 * are added with addRadioProperty(), the group information will help group the
	 * radio buttons in separate groups. The group name is built by concatenating
	 * the names of the properties in the group (will not appear on a user
	 * interface).
	 */
	private Map<String, String> radioGroupNames; // fc-22.9.2021

	/**
	 * Memorize a set of exclusive boolean properties (JRadioButton group). User
	 * will be prompted for a choice in some config panel. They can be checked like
	 * the booleanProperties with isSet (propertyName).
	 */
	public void addRadioProperty(String[] tab) {

		String groupName = "";

		for (int i = 0; i < tab.length; i++) {
			String propertyName = tab[i];

			// fc-2.2.2024 MOVED here, must be done even if property was already restored by
			// configuration saving
			memoPropertyDeclarationOrder(propertyName);

			// If common property and already known for this extractor, ignore
			if (!isIndividualProperty(propertyName) && hasConfigProperty(propertyName))
				continue;

			boolean yep = i == 0 ? true : false;
			settings.radioProperties.put(propertyName, new Boolean(yep));

			// fc-22.9.2021 Prepare groupName
			if (groupName.length() > 0)
				groupName += "_";
			groupName += propertyName;

//			memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED

		}

		// fc-22.9.2021
		if (radioGroupNames == null)
			radioGroupNames = new HashMap<>();

		// fc-22.9.2021 Update the map propertyName -> groupName
		for (int i = 0; i < tab.length; i++) {
			String propertyName = tab[i];
			radioGroupNames.put(propertyName, groupName);
		}

	}

	// fc-22.9.2021
	public String getRadioGroupName(String propertyName) {
		return radioGroupNames.get(propertyName);
	}

	// ... Radio properties ]---------------------

	// -----------------------------------------------------------
	// ---[ String properties ...
	// -----------------------------------------------------------

	/**
	 * Provides input for a String property in some config panel (JTextField).
	 */
	public void addStringProperty(String propertyName, String defaultValue) { // fc-12.9.2016

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName);

		if (!settings.stringProperties.containsKey(propertyName)) {
			settings.stringProperties.put(propertyName, defaultValue);

//			memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED
		}

	}

	/**
	 * Can be used to retrieve the value chosen by user in config for this property.
	 */
	public String getStringProperty(String propertyName) { // fc-12.9.2016

		if (!isPropertyEnabled(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getStringProperty ()",
					"Property: " + propertyName + " is not enabled, returned an empty string");
			return "";
		}

		try {
			if (settings.stringProperties.containsKey(propertyName))
				return settings.stringProperties.get(propertyName);

		} catch (Exception e) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getStringProperty()",
					"Error while trying to get property: " + propertyName + ", returned an empty string", e);
			return "";
		}

		return "";
	}

	// ... String properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Int properties ...
	// -----------------------------------------------------------

	/**
	 * Provides input for an int value in some config panel (JTextField).
	 */
	public void addIntProperty(String propertyName, int defaultValue) {

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName);

		if (!settings.intProperties.containsKey(propertyName)) {
			settings.intProperties.put(propertyName, new Integer(defaultValue));

//			memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED
		}

	}

	/**
	 * Can be used to retrieve the value chosen by user in config for this property.
	 */
	public int getIntProperty(String propertyName) {

		if (!isPropertyEnabled(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getIntProperty ()",
					"Property: " + propertyName + " is not enabled, returned 0");
			return 0;
		} // fc-6.2.2004

		try {
			if (settings.intProperties.containsKey(propertyName))
				return ((Integer) settings.intProperties.get(propertyName)).intValue();

		} catch (Exception e) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getIntProperty ()",
					"Property: " + propertyName + " caused an exception, returned 0", e);
			return 0;
		}
		return 0;
	}

	// ... Int properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Double properties ...
	// -----------------------------------------------------------

	/**
	 * Provides input for a double value in some config panel (JTextField).
	 */
	public void addDoubleProperty(String propertyName, double defaultValue) {

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021

		if (!settings.doubleProperties.containsKey(propertyName)) {
			settings.doubleProperties.put(propertyName, new Double(defaultValue));

//			memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED
		}

	}

	/**
	 * Can be used to retrieve the value chosen by user in config for this property.
	 */
	public double getDoubleProperty(String propertyName) {

		if (!isPropertyEnabled(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getDoubleProperty ()",
					"Property: " + propertyName + " is not enabled, returned 0");
			return 0d;
		} // fc-6.2.2004

		try {
			if (settings.doubleProperties.containsKey(propertyName))
				return ((Double) settings.doubleProperties.get(propertyName)).doubleValue();

		} catch (Exception e) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getDoubleProperty ()",
					"Property: " + propertyName + " caused an exception, returned 0", e);
			return 0d;
		}
		return 0d;
	}

	// ... Double properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Set properties ...
	// -----------------------------------------------------------

	/**
	 * Selection of some items inside a list of possible items. Items are Strings
	 * (e.g. "1", "2",... or "Blue", "Red"...). selectedItems can be null or empty.
	 */
	public void addSetProperty(String propertyName, String[] possibleItems, String[] selectedItems) {

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021

		// fc-1.8.2018 If property already there, replace it
		if (settings.setProperties.containsKey(propertyName))
			settings.setProperties.remove(propertyName);

		SetPropertyItemSelector is = new SetPropertyItemSelector(possibleItems, selectedItems);
		settings.setProperties.put(propertyName, is);

//		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED

	}

	/**
	 * Restores a user preference related to setProperties, to be called when
	 * reseting the settings, see comment lower.
	 */
	public void restoreUserSelectionInSetPropertiesIfNeeded() {

		// fc-2.8.2018
		// setProperties memorisation is managed specifically: see if for each
		// property and its set of possible values there was a user selection
		// saved, if so restore it, else do nothing more (the property may have
		// been inited in setConfigProperty with selectedValues =
		// possibleValues). See in sharedConfigure below for the storing of the
		// user chosen values.
		if (settings.setProperties != null) {

			// Iterate on setProperties if any to restore their selected values
			// fc 2.8.2018
			// fc-7.1.2019 iterate on copies to fix the concurrent modification
			// exception, see comment just below
			Iterator keys = new ArrayList(settings.setProperties.keySet()).iterator();
			Iterator values = new ArrayList(settings.setProperties.values()).iterator();

			// fc-7.1.2019 Bug found by T. Bronner and H. Wernsdorfer,
			// concurrent modification exception, see replacement upper
			// Iterator keys = settings.setProperties.keySet().iterator();
			// Iterator values = settings.setProperties.values().iterator();

			while (keys.hasNext() && values.hasNext()) {
				String propertyName = (String) keys.next();

				// ShareConfigure -> shared properties only here
				if (isIndividualProperty(propertyName))
					continue;

				SetPropertyItemSelector is = (SetPropertyItemSelector) values.next();
				List possibleValueList = is.possibleValues;

				// fc-2.8.2018 restore what was selected for this property and
				// this
				// given set of possible values (if nothing found, selected =
				// possible)
				String encodedString = Settings.getProperty(propertyName + Tools.encode(possibleValueList),
						Tools.encode(possibleValueList)); // 2nd arg is default
															// value if not
															// found
				String[] selectedValues = Tools.decode(encodedString);

				// convert List -> String[]
				String[] possibleValues = Tools.decode(Tools.encode(possibleValueList));

				// Which loci do we want to see
				addSetProperty(propertyName, possibleValues, selectedValues);

			}

		}

	}

	/**
	 * This must be used to update the possible values of the property when they can
	 * possibly change. E.g. if the property concerns treeIds, update it when step
	 * changes or when a tree grouper is used.
	 */
	public void updateSetProperty(String propertyName, String[] possibleValues) {

		if (settings.setProperties.containsKey(propertyName)) {
			SetPropertyItemSelector is = (SetPropertyItemSelector) settings.setProperties.get(propertyName);
			is.setPossibleValues(possibleValues);
		}

	}

	// fc-3.7.2018 Changed set by list below (keep order)
	public void updateSetProperty(String propertyName, List possibleValues) {

		if (settings.setProperties.containsKey(propertyName)) {
			SetPropertyItemSelector is = (SetPropertyItemSelector) settings.setProperties.get(propertyName);
			is.possibleValues = possibleValues;
		}

	}

	/**
	 * Can be used to retrieve the value chosen by user in config for this property.
	 */
	public List<String> getSetProperty(String propertyName) {

		// fc-12.6.2019 return a list of string for easier management by callers

		if (!isPropertyEnabled(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getSetProperty ()",
					"Property: " + propertyName + " is not enabled, returned an empty list");
			return new ArrayList<String>();
		}

		try {
			if (settings.setProperties.containsKey(propertyName)) {

				// fc-3.7.2018 setProperties.get(property) now returns a list
				// (keep order)
				List list = ((SetPropertyItemSelector) settings.setProperties.get(propertyName)).selectedValues;

				List<String> listOfString = new ArrayList<String>();
				for (Object o : list)
					listOfString.add((String) o);

				return listOfString;

			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getSetProperty ()",
					"Property: " + propertyName + " caused an exception, returned an empty list", e);
			return new ArrayList<String>();
		}
		return new ArrayList<String>();
	}

	// ... Set properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Specific properties ...
	// -----------------------------------------------------------

	/**
	 * Adds a common level specific configuration element (contains a configuration
	 * dialog, will be openable with a button).
	 */
	// fc-4.7.2019
	public void addSpecificProperty(String propertyName, DESpecificProperty specificProperty) {

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021

		// Common property -> first key is null
		settings.specificProperties.put(null, propertyName, specificProperty);

//		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED

	}

	/**
	 * Adds an individual level specific configuration property for the given
	 * dataExtractor (contains a configuration dialog, will be openable with a
	 * button).
	 */
	// fc-3.11.2020
	public void addSpecificProperty(String propertyName, ConfigurableExtractor ex,
			DESpecificProperty specificProperty) {

		// Individual property -> first key is the individual extractor the property
		// belongs to
		settings.specificProperties.put(ex, propertyName, specificProperty);

	}

	/**
	 * Can be used to retrieve the configuration chosen by user for this common
	 * property.
	 */
	// fc-4.7.2019
	public DESpecificProperty getSpecificProperty(String propertyName) {

		if (!isPropertyEnabled(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getSpecificProperty ()",
					"Property: " + propertyName + " is not enabled, returned null");
			return null;
		}

		try {

			// Common property -> first key is null
			return settings.specificProperties.get(null, propertyName);

		} catch (Exception e) {
			Log.println(Log.ERROR, "ConfigurableExtractor.specificProperties()",
					"Error while trying to get a specific property: " + propertyName, e);
			return null;
		}

	}

	/**
	 * Can be used to retrieve the configuration chosen by user for this property
	 * for the given dataExtractor.
	 */
	// fc-3.11.2020
	public DESpecificProperty getSpecificProperty(String propertyName, ConfigurableExtractor ex) {

		// Individual property -> first key is the individual extractor the property
		// belongs to
		return settings.specificProperties.get(ex, propertyName);

	}

	// ... Specific properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Combo properties ...
	// -----------------------------------------------------------

	/**
	 * Provides input for the choice of a String out of a list of possibilities in a
	 * combo box in some config panel (JTextField). Convention: selected item is
	 * placed in first position.
	 */
	public void addComboProperty(String propertyName, LinkedList<String> values) {

		System.out.println("" + this.getClass().getSimpleName() + " " + getStep().getCaption() + " addComboProperty(): "
				+ propertyName + ", #values: " + values.size());

		// fc-2.2.2024 MOVED here, must be done even if property was already restored by
		// configuration saving
		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021

		// fc+ag-1.2.2024 When changing simulation, an old species list is restored
		if (settings.getComboProperties ().containsKey(propertyName))
			settings.getComboProperties ().remove(propertyName);

		if (!settings.getComboProperties ().containsKey(propertyName)) {
			settings.getComboProperties ().put(propertyName, values);

//			memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021 MOVED

			// fc-13.9.2021 Note: should be reviewed
			// This could be deferred at DEMultiConfPanel creation time or redone each time
			// needed to avoid needing the comboPropertyTranslationToValue map in the
			// configurable extractor.

			// When a translated value is selected, we will return the original
			// untranslated value
			// So we need a map "translated value" to "value"
			Map<String, String> map = new HashMap<String, String>();
			comboPropertyTranslationToValue.put(propertyName, map);

			for (Iterator i = values.iterator(); i.hasNext();) {
				String v = (String) i.next();
				map.put(Translator.swap(v), v);
			}
			// fc-13.9.2021 Note: should be reviewed

		}

	}

	/**
	 * Returns the possible values for the given combo property.
	 */
	public LinkedList<String> getComboPropertyPossibleValues(String propertyName) {
		// fc-11.10.2023 To help manage a tricky configuration problem with Cstab
		if (!settings.getComboProperties ().containsKey(propertyName)) {
			return null;
		} else {
			return settings.getComboProperties ().get(propertyName);
		}

	}

	/**
	 * Remove combo property
	 */
	public void removeComboProperty(String propertyName) {
		if (!settings.getComboProperties ().containsKey(propertyName))
			return;

		settings.getComboProperties ().remove(propertyName);
	}

	/**
	 * Considering the given comboProperty, force its selection by moving the
	 * optionToSelect in first position.
	 */
	public void forceComboProperty(String propertyName, String optionToSelect) {
		// fc-11.10.2023 Cstab extractors configuration is tricky

		if (!settings.getComboProperties ().containsKey(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.forceComboProperty()",
					"Unknown optionToSelect: " + optionToSelect + ", ignored");
			return;
		} else {

			LinkedList<String> values = settings.getComboProperties ().get(propertyName);

			boolean removed = values.remove(optionToSelect);
			if (removed)
				values.addFirst(optionToSelect);
		}

	}

	/**
	 * Can be used to retrieve the value chosen by user in the combo list for this
	 * property. Convention: selected item is placed in first position.
	 */
	@Override
	public String getComboProperty(String propertyName) {

		if (!isPropertyEnabled(propertyName)) {
			Log.println(Log.ERROR, "ConfigurableExtractor.getComboProperty ()",
					"Property: " + propertyName + " is not enabled, returned null");

			return null; // fc-10.6.2004
		}

		try {
			if (settings.getComboProperties ().containsKey(propertyName)) {
				LinkedList<String> values = settings.getComboProperties ().get(propertyName);
				// values contains not translated string -> we return the
				// original not translated value
				// convention : selected item is placed in first position

				if (values == null || values.isEmpty()) {
					Log.println(Log.ERROR, "DataExtractor.getComboProperty ()", "Exception, comboProperty:"
							+ propertyName + " empty values list: " + values + " returned empty string");
					return "";
				}

				// Return the fist value (convention: it's the selected one)
				String value = values.iterator().next();
				if (value == null || value.equals("null"))
					return "";

				return value;
			}
		} catch (Exception e) {
			Log.println(Log.ERROR, "ConfigurableDataExtractor.getComboProperty ()", "Exception", e);
			return "";
		}
		return "";
	}

//	/**
//	 * Selects the option matching the given translated name in the given
//	 * comboProperty. I.e. moves the matching option in fist position in the
//	 * comboProperty list. fc-26.9.2012
//	 */
//	public void selectOptionInComboProperty(String comboPropertyName, String translatedName) {
//		try {
//			LinkedList<String> list = settings.getComboProperties ().get(comboPropertyName);
//			String option = translatedName;
//			try {
//				Map<String, String> map = comboPropertyTranslationToValue.get(comboPropertyName);
//				option = map.get(translatedName); // get the original option
//													// name (combo box
//													// contains translations)
//
//			} catch (Exception e) {
//				// Maybe not an error if missing translation
//				// Log.println (Log.ERROR, "DataExtractor.sharedConfigure ()",
//				// "Exception, comboProperty=" + name
//				// + ", trouble while trying to de-translate " +
//				// f.getSelectedItem (), e);
//			}
//
//			boolean removed = list.remove(option);
//			if (removed)
//				list.addFirst(option);
//
//		} catch (Exception e) {
//			Log.println(Log.ERROR, "DataExtractor.selectOptionInComboProperty ()",
//					"error in combo property selection, passed", e);
//		}
//
//	}

	// ... Combo properties ]---------------------

	// -----------------------------------------------------------
	// ---[ Group properties ...
	// -----------------------------------------------------------

	/**
	 * Used by subclasses to add a group properties. Type is one declared in the
	 * Group class (ex: TREE, CELL, FISH...) Target is either INDIVIDUAL or COMMON.
	 * INDIVIDUAL means that the group is for one single extractor and is set in the
	 * individual tab of DEMulticonfPanel. In this case, different extractors may be
	 * set on different groups. COMMON means that the group is used for all the
	 * extractors in the data block. If COMMON and INDIVIDUAL are added together,
	 * only COMMON will be considered if checked (grouper chooser must be checked).
	 */
	// fc-13.9.2004 Group generalization: FISH...
	public void addGroupProperty(GroupableType type, char target) {

		// fc-3.6.2008 Type is possibly null -> all types compatible with
		// the scene are possible
		if (type == null) {
			Collection possibleTypes = Group.getPossibleTypes(getStep().getScene());
			if (possibleTypes != null && !possibleTypes.isEmpty()) {
				type = (GroupableType) possibleTypes.iterator().next();

			} else {
				type = null;
				// type = Group.UNKNOWN;
			}
		}

		if (target == INDIVIDUAL) {
			settings.i_grouperType = type;
		} else if (target == COMMON) {
			settings.c_grouperType = type;
		} else {
			Log.println(Log.ERROR, "DataExtractor.addGroupProperty ()",
					"unknown target (ignored), should be COMMON or INDIVIDUAL: " + target);
		}

		// No property name, legacy config, to be reviewed
//		memoPropertyDeclarationOrder(propertyName); // fc-17.9.2021

	}

	/**
	 * If not null, type of group requested for individual configuration
	 */
	public GroupableType getIGrouperType() {
		return settings.i_grouperType;
	} // fc-13.9.2004

	/**
	 * If not null, type of group requested for common configuration
	 */
	public GroupableType getCGrouperType() {
		return settings.c_grouperType;
	} // fc-13.9.2004

	/**
	 * Grouper mode is true if one grouper was activated in config panel.
	 * fc-23.3.2004 Common (with memorized config) and individual (forgotten config)
	 * groupers are now separated.
	 */
	public boolean isGrouperMode() {
		return settings.c_grouperMode || i_grouperMode;
	}

	/**
	 * Grouper name is the one selected in the GrouperChooser combo box.
	 * fc-23.3.2004 Common (with memorized config) and individual (forgotten config)
	 * groupers are now separated.
	 */
	public String getGrouperName() {
		return (settings.c_grouperMode) ? settings.c_grouperName : i_grouperName;
	}

	/**
	 * Chosen grouper was in the shared config panel. fc-23.3.2004 Common
	 * (memorized) and individual (forgotten) groupers are now separated.
	 */
	public boolean isCommonGrouper() {
		return settings.c_grouperMode;
	}

	/**
	 * True if common grouper is in NOT mode (complementary).
	 */
	public boolean isCommonGrouperNot() {
		return settings.c_grouperNot;
	} // fc-21.4.2004

	/**
	 * True if individual grouper is in NOT mode (complementary).
	 */
	public boolean isGrouperNot() {
		return i_grouperNot;
	} // fc-21.4.2004

	/**
	 * fc-14.4.2003 Added this for connection with ObjectViewers (not tested yet)
	 * Sandrine Chalon - PhD
	 */
	public void setGrouperName(String grouperName) {
		try {
			// What about settings.useCommonGroup ??? - fc-6.5.2003
			settings.i_grouperType = GrouperManager.getInstance().getGrouper(grouperName).getType(); // fc
																										// -
																										// 13.9.2004

			i_grouperName = grouperName;
			i_grouperMode = !grouperName.equals("");
			grouperChanged(grouperName);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DataExtractor.setGrouperName ()",
					"Exception in extractor " + this.getClass().getName(), e);
		}
	}

	/**
	 * This method is called if user selects or changes groupers. It can be
	 * redefined in subclasses to do things in this case. Ex: updateSetProperty
	 * ("somePropertyKey", someSetOfPossibleValues);
	 */
	abstract public void grouperChanged(String gestGrouperName);

	// ... Group properties ]---------------------

	// ----------------------------------------------------------
	// ---[ treeIds configuration ...
	// -----------------------------------------------------------

	public List<String> getTreeIds() {
		return treeIds;
	}

	public void setTreeIds(List<String> treeIds) {
		this.treeIds = treeIds;
	}
	// ... treeIds configuration ]---------------------

	// ----------------------------------------------------------
	// ---[ Properties disabling ...
	// -----------------------------------------------------------

	/**
	 * Property enabling/disabling
	 */
	// fc-6.2.2004
	public void setPropertyEnabled(String propertyName, boolean enabled) {

		// fc-25.7.2021 moved here
		if (disabledProperties == null)
			disabledProperties = new HashSet<String>();

		if (enabled) {

			// Should not throw an exception if not found
			disabledProperties.remove(propertyName);

		} else {

			disabledProperties.add(propertyName);
		}
	}

	public boolean isPropertyEnabled(String propertyName) {
		// fc-25.7.2021 added check: disabledProperties == null ||
		return disabledProperties == null || !disabledProperties.contains(propertyName);
	}
	// ... Properties disabling ]-----------------------------

	// ------------------------------------------------------------------
	// Shared configuration considerations
	// Reminder: shared configuration concern all the extractors
	// in the data block (e.g. per hectare calculation -> all extractors
	// go per hectare).

	/**
	 * SharedConfigurable interface.
	 */
	public String getSharedConfigLabel() {
		return Translator.swap("Shared.common"); // let's twist again...
	}

	/**
	 * SharedConfigurable interface.
	 */
	public ConfigurationPanel getSharedConfigPanel(Object param) {

		// fc-25.7.2021 Now a local variable
		// Will be also needed in sharedConfigure () below
		DataBlock dataBlock = (DataBlock) param;
		setDataBlock(dataBlock); // fc-25.7.2021 Needed

		// Calibration data (optional)
		Collection<String> modelNames = dataBlock.detectCalibrationData(); // fc-12.10.2004

		if (!modelNames.isEmpty()) {

			LinkedList<String> l = new LinkedList<String>();
			l.add(Translator.swap("Shared.noneFeminine")); // first item in the
															// list will be
															// default selected
			l.addAll(modelNames);

			// If the comboProperty already exists and contains exactly the same
			// names, do nothing to keep the entries order (the first entry will be selected
			// in the combo box)
			boolean doNothing = false;
			if (hasConfigProperty("activateCalibration")) {
				LinkedList<String> existingvalues = (LinkedList<String>) settings.getComboProperties ()
						.get("activateCalibration");
				if (l.containsAll(existingvalues) && existingvalues.containsAll(l))
					doNothing = true;
			}
			if (!doNothing) {
				removeComboProperty("activateCalibration");
				addComboProperty("activateCalibration", l);
			}
		}

		// TRIED to MOVE this code in sharedConfigure () (seems more logical)
		// if (hasConfigProperty ("activateCalibration")) {
		// String modelName = getComboProperty ("activateCalibration");
		// if (!modelName.equals (Translator.swap ("Shared.noneFeminine"))) {
		// dataBlock.addCalibrationExtractor (modelName);
		// } else {
		// dataBlock.removeCalibrationExtractor ();
		// }
		// }

		return new DEMultiConfPanel(this);
	}

	/**
	 * SharedConfigurable interface. In case of both shared-configuration and
	 * single-configuration, sharedConfigure () is called before configure ().
	 */
	public void applySharedConfig(ConfigurationPanel panel) {

		DEMultiConfPanel multiConfPanel = (DEMultiConfPanel) panel;

		// fc-25.7.2021 Now a local variable
		DataBlock dataBlock = multiConfPanel.getDataBlock(); // fc-26.9.2012
		setDataBlock(dataBlock); // fc-25.7.2021 Needed

		// Note: checks have been done in checksAreOk () (empty, wrong type...)

		// Groupers
		try {
			StringBuffer b = new StringBuffer("DE multiconfigure(): p.grouperChooser.isGrouperSelected ()="
					+ multiConfPanel.grouperChooser.isGrouperAvailable() + " ");
			if (multiConfPanel.grouperChooser.isGrouperAvailable()) {
				try {
					settings.c_grouperMode = true;
					settings.c_grouperName = multiConfPanel.grouperChooser.getGrouperName();
					settings.c_grouperNot = multiConfPanel.grouperChooser.isGrouperNot(); // fc-21.4.2004
					settings.c_grouperType = multiConfPanel.grouperChooser.getType(); // fc-3.6.2008

					grouperChanged(settings.c_grouperName);
				} catch (Exception e) {
				}

			} else {
				settings.c_grouperMode = false; // fc-23.3.2004 Added this
												// line
				settings.c_grouperName = ""; // fc-23.3.2004 Added this line
				settings.c_grouperNot = false; // fc-21.4.2004 Added this
												// line
			}
			b.append("settings.c_grouperMode=" + settings.c_grouperMode + " ");
			b.append("settings.c_grouperName=" + settings.c_grouperName + " ");
		} catch (Exception e) {
		}

		// fc-8.9.2021 Calls the applyConfig () method of all configProperties (Boolean,
		// Int...)
		multiConfPanel.applySharedConfig(this);

		// MOVED this code here from getSharedConfiguration upper fc-4.9.2012
		if (hasConfigProperty("activateCalibration")) {
			String modelName = getComboProperty("activateCalibration");
			if (!modelName.equals(Translator.swap("Shared.noneFeminine"))) {
				dataBlock.addCalibrationExtractor(modelName);
			} else {
				dataBlock.removeCalibrationExtractor();
			}
		}

	}

	// ------------------------------------------------------------------
	// Configuration considerations.
	// Concerns only one extractor. The extractors in the same
	// dataBlock may be configured differently. They also may share some
	// configuration parameters, see sharedConfiguration.

	/**
	 * From Configurable interface.
	 */
	public String getConfigLabel() {
		return AmapTools.cutIfTooLong(getCaption(), 50);
	}

	/**
	 * From Configurable interface. Configurable interface allows to pass a
	 * parameter. Here, it is unused.
	 */
	public ConfigurationPanel getConfigPanel(Object parameter) {
		DEConfigurationPanel panel = new DEConfigurationPanel(this);
		return panel;
	}

	/**
	 * From Configurable interface. In case of both multi-configuration and
	 * single-configuration, sharedConfigure () is called before configure ().
	 */
	public void applyConfig(ConfigurationPanel panel) {

		DEConfigurationPanel p = (DEConfigurationPanel) panel;

		// Individual config: tree ids
		try {
			getTreeIds().clear();

			// fc-4.9.2019
			getTreeIds().addAll(p.getTreeIds());

		} catch (Exception e) {
			// fc-4.9.2019
			Log.println(Log.ERROR, "ConfigurableExtractor.applyConfig ()",
					"Error while retrieving the tree ids in DEConfigurationPanel", e);
			MessageDialog.print(panel, Translator.swap("Shared.errorSeeLog"), e);
			return;
		}

		// Individual config: groupers
		// Only if not managed commonly for all the extractors of the data
		// block.
		StringBuffer b = new StringBuffer("DE configure(): isCommonGrouper ()=" + isCommonGrouper() + " ");

		if (!isCommonGrouper() && p.getGrouperChooser() != null) {
			try {
				GrouperChooser grouperChooser = p.getGrouperChooser();
				i_grouperMode = grouperChooser.isGrouperAvailable();

				if (i_grouperMode) {
					i_grouperName = grouperChooser.getGrouperName();
					i_grouperNot = grouperChooser.isGrouperNot(); // fc -
																	// 21.4.2004
					grouperChanged(i_grouperName);
				} else {
					i_grouperName = "";
					i_grouperNot = false; // fc - 21.4.2004
				}

				b.append("settings.i_grouperMode=" + i_grouperMode + " ");
				b.append("settings.i_grouperName=" + i_grouperName + " ");

			} catch (Exception e) {
				// fc-4.9.2019
				Log.println(Log.ERROR, "ConfigurableExtractor.applyConfig ()",
						"Error while managing the INDIVIDUAL grouper mode in DEConfigurationPanel", e);
				MessageDialog.print(panel, Translator.swap("Shared.errorSeeLog"), e);
				return;

			}
		}

		// Individual config: status (alive, dead, cut...)
		if (p.getStatusChooser() != null) {
			statusSelection = p.getStatusChooser().getSelection();
		}

	}

	/**
	 * Configurable and SharedConfigurable interface.
	 */
	abstract public void postConfig();

	/**
	 * A way to check additional things about the configProperties after
	 * configuration has been changed
	 * 
	 * <pre>
	 * if (getIntProperty("classWidth") > 1000) {
	 *     MessageDialog.print(...);	// tell user why it is wrong
	 *     return false; // will abort updating the graph
	 * }
	 * </pre>
	 * 
	 * In case all is ok, return true.
	 */
	public boolean additionalChecks() {

		// Redefine this if needed to run more checks with the configProperties
		return true; // default: ok

	}

	public void setPropertyDeclarationOrderKept() { // fc-27.9.2021
		// optional, but if set, cannot be unset
		propertyDeclarationOrderKept = true;
	}

	public boolean isPropertyDeclarationOrderKept() { // fc-27.9.2021
		return propertyDeclarationOrderKept;
	}

	public List<String> getPropertyDeclarationOrder() { // fc-27.9.2021
		if (propertyDeclarationOrder == null)
			return new ArrayList<String>(); // empty

		return propertyDeclarationOrder; // fc-27.9.2021
	}

	public void setPropertyGroupingInTitledPanelsVeto(boolean propertyGroupingInTitledPanelsVeto) {
		this.propertyGroupingInTitledPanelsVeto = propertyGroupingInTitledPanelsVeto; // fc-8.12.2023
	}

	public boolean isPropertyGroupingInTitledPanelsVeto() {
		return propertyGroupingInTitledPanelsVeto; // fc-8.12.2023
	}

}
