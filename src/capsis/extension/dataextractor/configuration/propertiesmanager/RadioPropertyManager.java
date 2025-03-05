package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.awt.BorderLayout;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JLabel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * A manager to display radio properties (radio buttons), check user input and
 * apply his choices.
 * 
 * @author F. de Coligny - September 2021
 */
public class RadioPropertyManager extends AbstractPropertyManager {

	// This is a refactoring of the configuration properties management
	// Related to DESettings (data extractors settings), DEMultiConfPanel (common
	// configuration for all extractors in the same graph) and configurable
	// extractor.

	// Reference to the extractor being configured
	private ConfigurableExtractor extractor;

	// The Settings object of the extractor, containing the properties with their
	// current state
	private DESettings settings;

	// RadioButtons to display the radio properties
	// Key is propertyName, value is the matching radioButton
	public Map<String, JRadioButton> radioPropertiesRadioButtons; // radio button

	// A map containing for each radio group name (name for a group of exclusive
	// radio buttons) the matching buttonGroup
	private Map<String, ButtonGroup> radioButtonGroups; // fc-22.9.2021

	/**
	 * Constructor
	 */
	public RadioPropertyManager() {

		// init () must be called after the constructor

	}

	/**
	 * Inits the manager to show the radio properties of the given data extractor.
	 */
	@Override
	public void init(ConfigurableExtractor extractor) {

		this.extractor = extractor;
		this.settings = extractor.getSettings();

		radioPropertiesRadioButtons = new HashMap<>();

	}

	/**
	 * Returns a propertyPanel to allow user to configure it, e.g. a JCheckBox for a
	 * booleanProperty ; or a JLabel and a JTextField for an intProperty.
	 */
	public ColumnPanel getPropertyPanel(String propertyName) {

		boolean yep = settings.radioProperties.get(propertyName);

		// Create radio buttons
		JRadioButton rb = new JRadioButton(Translator.swap(propertyName), yep);

		radioPropertiesRadioButtons.put(propertyName, rb);

		if (!extractor.isPropertyEnabled(propertyName))
			rb.setEnabled(false);

		// Add the buttons in groups
		String radioGroupName = extractor.getRadioGroupName(propertyName);
		
		// fc-22.9.2021
		if (radioButtonGroups == null)
			radioButtonGroups = new HashMap<> ();
		
		// fc-22.9.2021
		ButtonGroup buttonGroup = radioButtonGroups.get(radioGroupName);
		if (buttonGroup == null) {
			buttonGroup = new ButtonGroup();
			radioButtonGroups.put(radioGroupName, buttonGroup);
		}
		
		buttonGroup.add(rb);

		LinePanel l1 = new LinePanel();
		l1.add(rb);
		l1.addGlue();

		ColumnPanel propertyPanel = new ColumnPanel(0, 0);
		propertyPanel.add(l1);

		return propertyPanel;

	}

	/**
	 * Layout the radio properties in the given panel. Radio properties: add
	 * JRadioButtons in ButtonGroups. If the given propertiesPanel is null, put the
	 * new property panels in the returned map.
	 */
	@Override
	public Map<String, ColumnPanel> layoutProperties(ColumnPanel propertiesPanel) {

		// fc-21.9.2021 Tuning the strategy, will be refactored
		return ConfigPropertyLayoutStrategy.layoutProperties(this, settings.radioProperties.keySet(), propertiesPanel);

		/*
		 * // fc-17.9.2021 If propertiesPanel is null, return the panels in a map
		 * Map<String, JPanel> mapOfPanels = null; if (propertiesPanel == null) {
		 * mapOfPanels = new HashMap<>(); // i.e. 'mapMode' propertiesPanel = new
		 * ColumnPanel(); // Used to feed the map } // fc-17.9.2021
		 * 
		 * // fc-30.8.2012 added TreeSet below, order was changing (CM & TL)... Iterator
		 * keys = new TreeSet(settings.radioProperties.keySet()).iterator(); String
		 * prevRoot = ""; JPanel panel = null; JPanel inside = null; Border etched =
		 * BorderFactory.createEtchedBorder(); ButtonGroup radioGroup = new
		 * ButtonGroup(); String propertyName = null; // fc-17.9.2021
		 * 
		 * while (keys.hasNext()) { propertyName = (String) keys.next();
		 * 
		 * if (!extractor.isUserActionEnabled(propertyName)) // fc-10.1.2019 added
		 * isUserActionEnabled () continue;
		 * 
		 * Boolean yep = (Boolean) settings.radioProperties.get(propertyName); //
		 * fc-30.8.2012
		 * 
		 * if (extractor.isIndividualProperty(propertyName)) continue;
		 * 
		 * String root = ""; try { root = propertyName.substring(0,
		 * propertyName.indexOf("_")); } catch (Exception e) { } String suffix = ""; try
		 * { suffix = propertyName.substring(propertyName.indexOf("_") + 1); } catch
		 * (Exception e) { }
		 * 
		 * // If root changes, write old root's panel if (!root.equals(prevRoot)) { if
		 * (panel != null)
		 * 
		 * addPanelInPropertiesPanelOrMap(propertyName, panel, propertiesPanel,
		 * mapOfPanels); // fc-17.9.2021 // propertiesPanel.add(panel);
		 * 
		 * panel = null; prevRoot = root; radioGroup = new ButtonGroup(); }
		 * 
		 * // Create radio buttons JRadioButton rb = new
		 * JRadioButton(Translator.swap(propertyName), yep.booleanValue());
		 * radioPropertiesRadioButtons.put(propertyName, rb);
		 * 
		 * if (!extractor.isPropertyEnabled(propertyName)) rb.setEnabled(false);
		 * 
		 * // Add the buttons in groups radioGroup.add(rb);
		 * 
		 * if (!"".equals(root)) { if (panel == null) { panel = new JPanel(new
		 * BorderLayout()); Border border = BorderFactory.createTitledBorder(etched,
		 * Translator.swap(root)); panel.setBorder(border);
		 * 
		 * inside = new JPanel(); inside.setLayout(new BoxLayout(inside,
		 * BoxLayout.Y_AXIS)); panel.add(inside, BorderLayout.CENTER); } inside.add(rb);
		 * } else { LinePanel l1 = new LinePanel(); l1.add(rb); l1.addGlue();
		 * 
		 * addPanelInPropertiesPanelOrMap(propertyName, l1, propertiesPanel,
		 * mapOfPanels); // fc-17.9.2021 // propertiesPanel.add(l1); } }
		 * 
		 * // Add last radio property panel if needed if (panel != null)
		 * 
		 * addPanelInPropertiesPanelOrMap(propertyName, panel, propertiesPanel,
		 * mapOfPanels); // fc-17.9.2021 // propertiesPanel.add(panel);
		 * 
		 * // The returned map is not null if the given propertiesPanel was passed null
		 * return mapOfPanels;
		 */

	}

	/**
	 * Runs checks on the current values of the properties, if trouble, tell user
	 * and return false.
	 */
	@Override
	public boolean checksAreOk() {

		// No checks needed for radioProperties (no possible error in a radioButton)
		return true;

	}

	/**
	 * Applies the current user config in the DESettings of the given extractor
	 */
	@Override
	public void applyConfig(ConfigurableExtractor extractorToBeConfigured) {

		// Set radioProperties
		Iterator keys = radioPropertiesRadioButtons.keySet().iterator();
		Iterator values = radioPropertiesRadioButtons.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String name = (String) keys.next();
			JRadioButton rb = (JRadioButton) values.next();
			if (extractor.isIndividualProperty(name))
				continue;

			extractorToBeConfigured.getSettings().radioProperties.remove(name);
			extractorToBeConfigured.getSettings().radioProperties.put(name, new Boolean(rb.isSelected()));
		}

	}

	/**
	 * Accessor to the extractor being configured.
	 */
	public ConfigurableExtractor getExtractor() {
		return extractor;
	}

}
