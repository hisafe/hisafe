package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.awt.BorderLayout;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;

/**
 * A manager to display int properties (textFields), check user input and apply
 * his choices.
 * 
 * @author F. de Coligny - September 2021
 */
public class IntPropertyManager extends AbstractPropertyManager {

	// This is a refactoring of the configuration properties management
	// Related to DESettings (data extractors settings), DEMultiConfPanel (common
	// configuration for all extractors in the same graph) and configurable
	// extractor.

	// Reference to the extractor being configured
	private ConfigurableExtractor extractor;

	// The Settings object of the extractor, containing the properties with their
	// current state
	private DESettings settings;

	// JTextFields to display the int properties
	// Key is propertyName, value is the matching textfield
	public Map<String, JTextField> intPropertiesTextFields;

	/**
	 * Constructor
	 */
	public IntPropertyManager() {

		// init () must be called after the constructor

	}

	/**
	 * Inits the manager to show the int properties of the given data extractor.
	 */
	@Override
	public void init(ConfigurableExtractor extractor) {

		this.extractor = extractor;
		this.settings = extractor.getSettings();

		intPropertiesTextFields = new HashMap<>();

	}
	
	/**
	 * Returns a propertyPanel to allow user to configure it, e.g. a JCheckBox for a
	 * booleanProperty ; or a JLabel and a JTextField for an intProperty.
	 */
	public ColumnPanel getPropertyPanel(String propertyName) {
		
		int value = settings.intProperties.get(propertyName);

		// Create JTextField
		JLabel label = new JWidthLabel(Translator.swap(propertyName) + " :", 190);
		JTextField textField = new JTextField("" + value, 5);
		
		intPropertiesTextFields.put(propertyName, textField);

		if (!extractor.isPropertyEnabled(propertyName))
			textField.setEnabled(false);
		
		LinePanel l1 = new LinePanel();
		l1.add(label);
		l1.add(textField);
		l1.addGlue();

		ColumnPanel propertyPanel = new ColumnPanel (0, 0);
		propertyPanel.add(l1);
		
		return propertyPanel;
		
	}

	/**
	 * Layout the int properties in the given panel. Int properties: add
	 * JTextFields. If the given propertiesPanel is null, put the new property panels
	 * in the returned map.
	 */
	@Override
	public Map<String, ColumnPanel> layoutProperties(ColumnPanel propertiesPanel) {

		// fc-21.9.2021 Tuning the strategy, will be refactored
		return ConfigPropertyLayoutStrategy.layoutProperties(this, settings.intProperties.keySet(),
				propertiesPanel);

		/*
		// fc-17.9.2021 If propertiesPanel is null, return the panels in a map
		Map<String, JPanel> mapOfPanels = null;
		if (propertiesPanel == null) {
			mapOfPanels = new HashMap<>(); // i.e. 'mapMode'
			propertiesPanel = new ColumnPanel(); // Used to feed the map
		}
		// fc-17.9.2021

		// fc-30.8.2012 added TreeSet below, order was changing (CM & TL)...
		Iterator keys = new TreeSet(settings.intProperties.keySet()).iterator();
		String prevRoot = "";
		JPanel panel = null;
		JPanel inside = null;
		Border etched = BorderFactory.createEtchedBorder();
		String propertyName = null; // fc-17.9.2021

		while (keys.hasNext()) {
			propertyName = (String) keys.next();

			if (!extractor.isUserActionEnabled(propertyName))
				// fc-10.1.2019 added isUserActionEnabled ()
				continue;

			Integer value = (Integer) settings.intProperties.get(propertyName); // fc-30.8.2012

			if (extractor.isIndividualProperty(propertyName))
				continue;

			String root = "";
			try {
				root = propertyName.substring(0, propertyName.indexOf("_"));
			} catch (Exception e) {
			}
			String suffix = "";
			try {
				suffix = propertyName.substring(propertyName.indexOf("_") + 1);
			} catch (Exception e) {
			}

			// If root changes, write old root's panel
			if (!root.equals(prevRoot)) {
				if (panel != null)
					
					addPanelInPropertiesPanelOrMap(propertyName, panel, propertiesPanel, mapOfPanels); // fc-17.9.2021
//					propertiesPanel.add(panel);
				
				panel = null;
				prevRoot = root;
			}

			// Create JTextField
			JLabel label = new JWidthLabel(Translator.swap(propertyName) + " :", 190);
			JTextField f = new JTextField("" + value.intValue(), 5);
			intPropertiesTextFields.put(propertyName, f);

			if (!extractor.isPropertyEnabled(propertyName))
				f.setEnabled(false);

			if (!"".equals(root)) {
				if (panel == null) {
					panel = new JPanel(new BorderLayout());
					// Bordered panel name
					Border border = BorderFactory.createTitledBorder(etched, Translator.swap(root));
					panel.setBorder(border);

					inside = new JPanel();
					inside.setLayout(new BoxLayout(inside, BoxLayout.Y_AXIS));
					panel.add(inside, BorderLayout.CENTER);
				}
				LinePanel l1 = new LinePanel();
				l1.add(label);
				l1.add(f);
				l1.addGlue();
				inside.add(l1);
			} else {
				LinePanel l1 = new LinePanel();
				l1.add(label);
				l1.add(f);
				l1.addGlue();
				
				addPanelInPropertiesPanelOrMap(propertyName, l1, propertiesPanel, mapOfPanels); // fc-17.9.2021
//				propertiesPanel.add(l1);
			}
		}

		// Add last property panel if needed
		if (panel != null)
			
			addPanelInPropertiesPanelOrMap(propertyName, panel, propertiesPanel, mapOfPanels); // fc-17.9.2021
//			propertiesPanel.add(panel);

		// The returned map is not null if the given propertiesPanel was passed null
		return mapOfPanels;
*/
		
	}

	/**
	 * Runs checks on the current values of the properties, if trouble, tell user
	 * and return false.
	 */
	@Override
	public boolean checksAreOk() {

		// Check intProperties type (must be int)
		Iterator keys = intPropertiesTextFields.keySet().iterator();
		Iterator values = intPropertiesTextFields.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String propertyName = (String) keys.next();
			JTextField f = (JTextField) values.next();
			if (extractor.isIndividualProperty(propertyName))
				continue;

			String value = f.getText();
			try {
				new Integer(value);
			} catch (Exception e) {
				MessageDialog.print(this,
						Translator.swap(propertyName) + " " + Translator.swap("DataExtractor.mustBeAnInteger"));
				return false;
			}
		}

		return true;
	}

	/**
	 * Applies the current user config in the DESettings of the given extractor
	 */
	@Override
	public void applyConfig(ConfigurableExtractor extractorToBeConfigured) {

		// Set intProperties
		Iterator keys = intPropertiesTextFields.keySet().iterator();
		Iterator values = intPropertiesTextFields.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String propertyName = (String) keys.next();
			JTextField f = (JTextField) values.next();
			if (extractor.isIndividualProperty(propertyName))
				continue;

			extractorToBeConfigured.getSettings().intProperties.remove(propertyName);
			extractorToBeConfigured.getSettings().intProperties.put(propertyName, new Integer(f.getText()));
		}

	}

	/**
	 * Accessor to the extractor being configured.
	 */
	 public ConfigurableExtractor getExtractor () {
		 return extractor;
	 }

}
