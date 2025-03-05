/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
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

package capsis.extension.dataextractor.configuration;

import java.awt.BorderLayout;
import java.awt.event.MouseWheelEvent;
import java.awt.event.MouseWheelListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;

import capsis.extension.dataextractor.configuration.propertiesmanager.AbstractPropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.BooleanPropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.ComboPropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.DoublePropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.IntPropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.RadioPropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.SetPropertyItemSelector;
import capsis.extension.dataextractor.configuration.propertiesmanager.SetPropertyManager;
import capsis.extension.dataextractor.configuration.propertiesmanager.StringPropertyManager;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import capsis.extensiontype.DataBlock;
import capsis.gui.GrouperChooser;
import capsis.gui.GrouperChooserListener;
import capsis.kernel.GScene;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.GenoSpecies;
import capsis.lib.genetics.Genotypable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.configurable.SharedConfigurable;
import capsis.util.group.GroupableType;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;

/**
 * Panel for a data block common configuration considerations. These
 * configurations apply to each data extractor in the block, all of same type
 * (i.e. same class). E.g. perHectare if true must be true for all the
 * extractors in the same dataBlock (they are in the same renderer, e.g. a
 * graph).
 * 
 * @author F. de Coligny - March 2003
 */
public class DEMultiConfPanel extends ConfigurationPanel /* implements ActionListener */ {

	// fc-8.9.2021 Reviewed configProperties, added the managers
	// (BooleanPropertiesManager...)
	// fc-6.2.2004 - added isPropertyEnabled (name) management for all properties

	// Reference to the extractor being configured
	private ConfigurableExtractor extractor;

	// The data block the extractor belongs to, set at
	// construction time, may contain other extractors of same class, configured
	// differently or synchronised on different simulation steps for comparison in
	// the same graph
	private DataBlock dataBlock;

	// Groupers
	public GrouperChooser grouperChooser;

	// fc-9.9.2021 New properties management framework
	private List<AbstractPropertyManager> propertyManagers;

	// fc-14.9.2021 Removed the ordered properties (complex and low added value)
//	private Collection orderedProperties;
//	public Map booleanPropertiesCheckBoxes;
//	public Map intPropertiesTextFields;
//	public Map doublePropertiesTextFields;
//	public Map comboPropertiesComboBoxes;
//	public Map stringPropertiesTextFields;
	// setProperties not processed by orderedProperties
	// fc-14.9.2021 Removed the ordered properties

	public Map specificPropertiesButtons; // fc-4.7.2019

	/**
	 * Constructor.
	 */
	public DEMultiConfPanel(SharedConfigurable c) {
		super(c);

		extractor = (ConfigurableExtractor) c; // fc-28.3.2003
		dataBlock = extractor.getDataBlock();

		System.out.println("" + getClass().getSimpleName() + " " + extractor.getStep()
				+ " DEMultiConfPanel ()... desettings: " +extractor.getSettings().hashCode()+", "+ extractor.getSettings());

		// Create all propertyManagers
		propertyManagers = new ArrayList<>();

		// For boolean, radio, int... properties in settings
		// The order of the lines below will be the default order of appearance of the
		// various types in the config panel
		propertyManagers.add(new StringPropertyManager());
		propertyManagers.add(new SetPropertyManager());
		propertyManagers.add(new ComboPropertyManager());
		propertyManagers.add(new IntPropertyManager());
		propertyManagers.add(new DoublePropertyManager());
		propertyManagers.add(new BooleanPropertyManager());
		propertyManagers.add(new RadioPropertyManager());
		// specific
		// ordered

		// Init all propertyManagers on the extractor under configuration
		for (AbstractPropertyManager propMan : propertyManagers)
			propMan.init(extractor);

		specificPropertiesButtons = new HashMap();

		ColumnPanel propertyPanel = new ColumnPanel();

		// fc-17.9.2021 Progressing with new properties ordering strategy
//		System.out.println("[DEMultiConfPanel] config properties order for " + extractor.getClass().getName() + "...");
//		System.out.println(
//				"[DEMultiConfPanel] configPropertyDeclarationOrderKept: " + extractor.isPropertyDeclarationOrderKept());
//		for (String propName : extractor.getPropertyDeclarationOrder())
//			System.out.println("[DEMultiConfPanel]   " + propName);

		layoutLegacyProperties(propertyPanel);
		layoutSpecificProperties(propertyPanel);

		if (extractor.isPropertyDeclarationOrderKept()) {

			// fc-17.9.2021 Show the properties in same order than their declaration in
			// ConfigurableExtractor.setConfigProperties ()
			ColumnPanel nullPanel = null;
			Map<String, ColumnPanel> mapOfPanels = new HashMap<>();

			// Ask the propertyManagers to return the property panels in a map and merge
			// them
			for (AbstractPropertyManager propMan : propertyManagers)
				// Sending a nullPanel means 'please return the property panels in a map'
				mapOfPanels.putAll(propMan.layoutProperties(nullPanel));

			// fc-8.10.2021 Do better for panels in sections (i.e. sectionName_propertyName)
			// layoutProperties(nullPanel) above should return a map of panels without their
			// section borders
			// The section borders should be managed just below

			// fc-28.10.2021 for titled panels merging below
			Map<String, ColumnPanel> titledPanelMap = new HashMap<>();

			// Add the property panels in the expected order
			for (String propertyName : extractor.getPropertyDeclarationOrder()) {

				if (mapOfPanels.containsKey(propertyName)) {

					ColumnPanel p = mapOfPanels.get(propertyName);

					// fc-28.10.2021 Merge titled borders if same title
					if (p.getTitle() != null) {

						if (titledPanelMap.containsKey(p.getTitle())) {
							ColumnPanel panelWithSameTitle = titledPanelMap.get(p.getTitle());
							p.setBorder(null); // remove extra titled border
							panelWithSameTitle.add(p);

						} else {
							titledPanelMap.put(p.getTitle(), p);
							propertyPanel.add(p);
						}

					} else {
						// No title, nothing to be merged
						propertyPanel.add(p);
					}

					// fc-28.10.2021
					// If p has a title
					//
					// if (first panel with this title)
					// store p in titledPanelMap
					// add p in propertyPanel
					// else
					// get the first occurrence in titledPanelMap
					// and add p in this first occurence

					// else (if p does not have a title)
					// add it in propertyPanel directly

					mapOfPanels.remove(propertyName);
				}
			}

			// Add remaining panels after if any (security)
			if (!mapOfPanels.isEmpty()) {
//				System.out.println("[DEMultiConfPanel] Adding " + mapOfPanels.size() + " remaining panels...");

				// aux can help for sections (property starting with sectionName_)
				ColumnPanel aux = new ColumnPanel(0, 0);

				// fc+ag-25.1.2024 Loop on a copy to prevent a ConcurrentModificationException
				for (String propertyName : new ArrayList<>(mapOfPanels.keySet())) {
//				for (String propertyName : mapOfPanels.keySet()) {

					// fc-8.10.2021 NOTE: see ConfigPropertyLayoutStrategy:71 and below, prefix
					// management

					propertyPanel.add(mapOfPanels.get(propertyName));
					mapOfPanels.remove(propertyName);
				}

			}

			// fc-17.9.2021

		} else { // Simple case: properties laid out by the property managers, type by type

			// Layout all properties of the propertyManagers i the given propertyPanel
			for (AbstractPropertyManager propMan : propertyManagers)
				propMan.layoutProperties(propertyPanel);

		}

		// Avoid an empty panel
		if (propertyPanel.getComponentCount() == 0) {
			LinePanel l1 = new LinePanel();
			l1.add(new JLabel(Translator.swap("DataExtractor.noCommonConfiguration")));
			l1.addGlue();
			propertyPanel.add(l1);
		}

		// Add property panel
		setLayout(new BorderLayout());
		JPanel aux = new JPanel(new BorderLayout());
		aux.add(propertyPanel, BorderLayout.NORTH);

		JScrollPane scroll = new JScrollPane(aux);

		// fc-22.9.2021 Better wheel scrolling management
		aux.addMouseWheelListener(new MouseWheelListener() {

			public void mouseWheelMoved(MouseWheelEvent event) {
				final JScrollBar scrollBar = scroll.getVerticalScrollBar();
				final int rotation = event.getWheelRotation();
				if (scrollBar != null)
					scrollBar.setValue(scrollBar.getValue() + 20 * rotation);
			}
		});

		add(scroll, BorderLayout.CENTER);

		MemoPanel userMemo = new MemoPanel(Translator.swap("DEMultiConfPanel.commonConfigurationExplanation"));
		add(userMemo, BorderLayout.SOUTH);

	}

	// Specific properties: add a button to open the property's configuration
	// dialog. DEMultiConfPanel deals with the common level properties
	//
	private void layoutSpecificProperties(ColumnPanel master) {

		// Common property -> first key is null (see AbstractDataExtractor)
		Set<String> propertyNames = new TreeSet<String>(extractor.getSettings().specificProperties.keySet2(null));
//		Set<String> propertyNames = new TreeSet<>(ex.getSettings().specificProperties.keySet());

		for (String propertyName : propertyNames) {

			if (!extractor.isUserActionEnabled(propertyName))
				continue;

			DESpecificProperty property = extractor.getSettings().specificProperties.get(null, propertyName);

			boolean buttonEnabled = extractor.isPropertyEnabled(propertyName);
			JPanel l1 = property.getConfigurationLine(this, propertyName, buttonEnabled);

			master.add(l1);

		}

	}

	// Some legacy properties : HECTARE, PERCENTAGE, TREE_GROUP, CELL_GROUP,
	// CLASS_WIDTH
	// + some (old manner) properties for Ripley : INTERVAL_NUMBER,
	// INTERVAL_SIZE, IC_NUMBER_OF_SIMULATIONS, IC_RISK, IC_PRECISION
	// New properties should use the new framework (boolean, radio, set, int and
	// double properties (...)).
	//
	private void layoutLegacyProperties(ColumnPanel master) {

		// Groupers in COMMON context
		//
		if (extractor.getCGrouperType() != null
				&& extractor.isUserActionEnabled(extractor.getCGrouperType().getKey())) {
			// fc-10.1.2019 added isUserActionEnabled ()

			GroupableType type = extractor.getCGrouperType();

			// fc-13.9.2004
			// ~ Object composite = Group.getComposite (ex.getStep ().getStand
			// (), type);

			// NEW...
			boolean checked = extractor.isCommonGrouper() && extractor.isGrouperMode();
			GrouperManager gm = GrouperManager.getInstance();
			String selectedGrouperName = gm.removeNot(extractor.getGrouperName());

			// fc-3.6.2020 Restricted to type
			grouperChooser = new GrouperChooser(extractor.getStep().getScene(), type, Arrays.asList(type),
					selectedGrouperName, extractor.isCommonGrouperNot(), true, checked);

			// fc-13.9.2004 - removed next line
			// ~ grouperChooser.setEnabled (enabled); // fc-6.2.2004

			// Call listener when grouper change
			grouperChooser.addGrouperChooserListener((GrouperChooserListener) extractor);

			LinePanel l8 = new LinePanel();
			l8.add(grouperChooser);
			l8.addStrut0();
			master.add(l8);
		}

	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////:
	// //////////////////////////////////////////// Controls
	// ///////////////////////////////////////////////////////////////////////////////////////////////:
	// ///////////////////////////////////////////////////////////////////////////////////////////////:

	public boolean checksAreOk() {

		// fc-8.9.2021 If a propertiesManager returns false (it has told user), return
		// false (all managers will be asked in sequence)
		for (AbstractPropertyManager propMan : propertyManagers)
			if (!propMan.checksAreOk())
				return false;

		// fc-4.9.2019 Set default value, may be changed below
		dataBlock.setElementRestrictionAtCommonLevel(false, "DETimeConfigPanel");

		// fc-23.3.2004
		if (grouperChooser != null && grouperChooser.isGrouperAvailable()) {

			if (grouperChooser.getGrouperName() == null || grouperChooser.getGrouperName().equals("")) {
				MessageDialog.print(this, Translator.swap("DEMultiConfPanel.wrongGrouperName"));
				return false;
			}

			// fc-4.9.2019 Check individual / common consistency
			if (dataBlock.isElementRestrictionAtIndividualLevel()) {
				MessageDialog.print(this,
						Translator.swap("Shared.errorDuringElementsRestrictionConsistencyManagement"));
				return false;
			} else {
				dataBlock.setElementRestrictionAtCommonLevel(true, "DETimeConfigPanel");
			}
		}

		// fc-14.9.2021 Moved to additionalChecks () when getComboProperty () is
		// available
//		// Add here a control for calibration data : if some model is selected
//		// in the comboProperty, we must be in hectare mode, else, prompt
//		// user and return false;
//		// fc-14.10.2004
//		if (extractor.hasConfigProperty("activateCalibration")) {
//
//			// We can not use here directly :
//			// String modelName = getComboProperty ("activateCalibration");
//			// because this method runs before this value is updated in
//			// DataExtractor.multiConfigure ()
//			// fc-14.10.2004
//			JComboBox cb = (JComboBox) comboPropertiesComboBoxes.get("activateCalibration");
//			String modelName = (String) cb.getSelectedItem();
//
//			if (!modelName.equals(Translator.swap("Shared.noneFeminine"))) {
//
//				// fc-27.8.2021 Removed HECTARE and perHa, now a boolean property
//				if (extractor.hasConfigProperty("perHectare") && !extractor.isSet("perHectare")) {
//
//					MessageDialog.print(this, Translator.swap("DEMultiConfPanel.calibrationRequiresHectareProperty"));
//					return false;
//				}
//			}
//		}

		// -------------------------------------------------
		// fc-13.9.2021 Perhaps this could be moved in the extractor's additionalChecks
		// () method <- to be reviewed
		// -------------------------------------------------
		// Special test for genetics extractors : group must contain Genotypable
		// indivs (only)
		// and selected loci must exist in indivs.
		// fc-23.12.2004
		if (extractor.hasConfigProperty("afLociIds") || extractor.hasConfigProperty("gfLociIds")) {

			// Which loci were selected ?
			List loci = null;
			if (extractor.hasConfigProperty("afLociIds")) {
				loci = ((SetPropertyItemSelector) extractor.getSettings().setProperties
						.get("afLociIds")).selectedValues;
			} else {
				loci = ((SetPropertyItemSelector) extractor.getSettings().setProperties
						.get("gfLociIds")).selectedValues;
			}
			if (loci == null || loci.isEmpty()) {
				MessageDialog.print(this, Translator.swap("Shared.noLociWereSelected"));
				return false;
			}

			// Which individuals to consider ? (group or not)
			GScene stand = extractor.getStep().getScene();
			if (!(stand instanceof GeneticScene)) {
				MessageDialog.print(this, Translator.swap("Shared.standMustBeAGeneticScene"));
				return false;
			}
			GeneticScene scene = (GeneticScene) stand;
			Collection indivs = scene.getGenotypables();

			GroupableType grouperType = extractor.getCGrouperType();

			if (dataBlock.isElementRestrictionAtCommonLevel() && grouperChooser != null
					&& grouperChooser.isGrouperAvailable()) {

				String grouperName = grouperChooser.getGrouperName();
				if (grouperName != null && !grouperName.equals("")) {
					indivs = GrouperManager.getInstance().getGrouper(grouperName).apply(indivs,
							grouperName.toLowerCase().startsWith("not "));
				}
			}

			GenoSpecies prevSpecies = null;
			Genotypable gee = null;
			for (Iterator i = indivs.iterator(); i.hasNext();) {
				Object indiv = i.next();
				if (!(indiv instanceof Genotypable && ((Genotypable) indiv).getGenotype() != null)) {
					MessageDialog.print(this, Translator.swap("Shared.mustBeAllGenotyped"));
					return false;
				}
				gee = (Genotypable) indiv;
				if (prevSpecies == null) {
					prevSpecies = gee.getGenoSpecies();
				} else {
					if (!gee.getGenoSpecies().equals(prevSpecies)) {
						MessageDialog.print(this, Translator.swap("Shared.mustBeAllSameSpecies"));
						return false;
					}
				}

			}
			if (gee == null) {
				MessageDialog.print(this, Translator.swap("Shared.oneGenotypableNeededAtLeast"));
				return false;
			}

		}

		// All checks were run, all is ok
		return true;
	}

	/**
	 * Apply the current user configuration (called by ConfigurableExtractor)
	 */
	public void applySharedConfig(ConfigurableExtractor extractorToBeConfigured) {

		// fc-9.9.2021
		// Call applyConfig () in all propertiesManagerss
		for (AbstractPropertyManager propMan : propertyManagers)
			propMan.applyConfig(extractorToBeConfigured);

	}

	/**
	 * This method is called by containing dialog after calling execute (). If
	 * trouble, the method is responsible for user information (e.g. "classWidth
	 * must be greater than 1").
	 */
	public boolean additionalChecks() {
		// fc-8.9.2021 Can be used to check properties values in expected ranges

		// fc-14.9.2021 activateCalibration is activated when a calibration file is
		// found
		// fc-14.9.2021 Moved here, was called too early in checksAreOk()
		// Add here a control for calibration data : if some model is selected
		// in the comboProperty, we must be in hectare mode, else, prompt
		// user and return false;
		// fc-14.10.2004
		if (extractor.hasConfigProperty("activateCalibration")) {

			// Called after execute: getComboProperty () is available
			String modelName = extractor.getComboProperty("activateCalibration");

			if (!modelName.equals(Translator.swap("Shared.noneFeminine"))) {

				// fc-27.8.2021 Removed HECTARE and perHa, now a boolean property
				if (extractor.hasConfigProperty("perHectare") && !extractor.isSet("perHectare")) {

					MessageDialog.print(this, Translator.swap("DEMultiConfPanel.calibrationRequiresHectareProperty"));
					return false;
				}
			}
		}

		// Redirection to the extractor (optional check, returns true by default)
		return extractor.additionalChecks();

	}

	public DataBlock getDataBlock() {
		return dataBlock;
	}

	/**
	 * This method gives an access to the combo boxes in the ComboPropertyManager,
	 * it should not be used, it is there for compatibility with an old class. Could
	 * be removed in the future.
	 */
	@Deprecated
	public Map<String, JComboBox> getComboPropertiesComboBoxes() {
		ComboPropertyManager cpm = null;
		for (AbstractPropertyManager apm : propertyManagers) {
			if (apm instanceof ComboPropertyManager) {
				cpm = (ComboPropertyManager) apm;
				break;
			}
		}
		if (cpm == null)
			return null;
		return cpm.getComboPropertiesComboBoxes();
	}

}
