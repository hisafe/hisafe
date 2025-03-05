package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.awt.BorderLayout;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.border.Border;

import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * A manager to display combo properties, check user input and apply his
 * choices.
 * 
 * @author F. de Coligny - September 2021
 */
public class ComboPropertyManager extends AbstractPropertyManager {

	// This is a refactoring of the configuration properties management
	// Related to DESettings (data extractors settings), DEMultiConfPanel (common
	// configuration for all extractors in the same graph) and configurable
	// extractor.

	// Reference to the extractor being configured
	private ConfigurableExtractor extractor;

	// The Settings object of the extractor, containing the properties with their
	// current state
	private DESettings settings;

	// JComboBoxes to display the combo properties
	// Key is propertyName, value is the matching comboBox
	private Map<String, JComboBox> comboPropertiesComboBoxes;

	/**
	 * Constructor
	 */
	public ComboPropertyManager() {

		// init () must be called after the constructor

	}

	/**
	 * Inits the manager to show the combo properties of the given data extractor.
	 */
	@Override
	public void init(ConfigurableExtractor extractor) {

		this.extractor = extractor;
		this.settings = extractor.getSettings();

		comboPropertiesComboBoxes = new HashMap<>();

	}
	
	/**
	 * Returns a propertyPanel to allow user to configure it, e.g. a JCheckBox for a
	 * booleanProperty ; or a JLabel and a JTextField for an intProperty.
	 */
	public ColumnPanel getPropertyPanel(String propertyName) {
		
		LinkedList<String> value = settings.getComboProperties ().get(propertyName);
		String selectedItem = value.getFirst();

		// Create JComboBox
		JLabel label = new JWidthLabel(Translator.swap(propertyName) + " :", 190);
		Map<String,String> human2key = new LinkedHashMap<>();
		for (Iterator<String> i = value.iterator(); i.hasNext();) {
			String key = i.next();
			String human = Translator.swap(key);
			human2key.put(human, key);
		}

		// Alphabetical order
		JComboBox combo = new JComboBox<>(new TreeSet<String>(human2key.keySet()).toArray());

		// Select first value in values
		combo.setSelectedItem(Translator.swap(selectedItem));

		comboPropertiesComboBoxes.put(propertyName, combo);

		if (!extractor.isPropertyEnabled(propertyName))
			combo.setEnabled(false);
		
		LinePanel l1 = new LinePanel();
		l1.add(label);
		l1.add(combo);
		l1.addGlue();
		
		ColumnPanel propertyPanel = new ColumnPanel (0, 0);
		propertyPanel.add(l1);
		
		return propertyPanel;
		
	}
	
	/**
	 * Layout the combo properties in the given panel. Combo properties: add
	 * JComboBoxes If the given propertiesPanel is null, put the new property panels in
	 * the returned map.
	 */
	@Override
	public Map<String, ColumnPanel> layoutProperties(ColumnPanel propertiesPanel) {

		// fc-21.9.2021 Tuning the strategy, will be refactored
		return ConfigPropertyLayoutStrategy.layoutProperties(this, settings.getComboProperties ().keySet(),
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
		Iterator keys = new TreeSet(settings.getComboProperties ().keySet()).iterator();
		String prevRoot = "";
		JPanel panel = null;
		JPanel inside = null;
		Border etched = BorderFactory.createEtchedBorder();
		String propertyName = null; // fc-17.9.2021

		while (keys.hasNext()) {
			propertyName = (String) keys.next();

			try {
				if (!extractor.isUserActionEnabled(propertyName))
					// fc-10.1.2019 added isUserActionEnabled ()
					continue;

				if (extractor.isIndividualProperty(propertyName))
					continue;

				LinkedList<String> value = settings.getComboProperties ().get(propertyName);
				Object selectedItem = value.getFirst();

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
//						propertiesPanel.add(panel);

					panel = null;
					prevRoot = root;
				}

				// Create JComboBox
				JLabel label = new JWidthLabel(Translator.swap(propertyName) + " :", 190);
				Map human2key = new LinkedHashMap();
				for (Iterator i = value.iterator(); i.hasNext();) {
					String key = (String) i.next();
					String human = Translator.swap(key);
					human2key.put(human, key);
				}

				// Translate values... maybe we should not translate here ? User
				// could decide
				// to translate or not at building combo property time...
				// Alphabetical order - needed
				JComboBox f = new JComboBox(new TreeSet(human2key.keySet()).toArray());

				// Select first value in values
				f.setSelectedItem(Translator.swap((String) selectedItem));

				comboPropertiesComboBoxes.put(propertyName, f);

				if (!extractor.isPropertyEnabled(propertyName))
					f.setEnabled(false);

				if (!"".equals(root)) {
					if (panel == null) {
						panel = new JPanel(new BorderLayout());
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
//					propertiesPanel.add(l1);
				}
			} catch (Exception e) {
				Log.println(Log.WARNING, "ComboPropertyManager.layoutProperties ()",
						"Error while laying out combo " + propertyName + ", passed", e);
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

		// No checks needed for getComboProperties () (no possible error in a comboBox)
		return true;

	}

	/**
	 * Applies the current user config in the DESettings of the given extractor
	 */
	@Override
	public void applyConfig(ConfigurableExtractor extractorToBeConfigured) {

		// Set getComboProperties ()
		Iterator<String> keys = comboPropertiesComboBoxes.keySet().iterator();
		Iterator<JComboBox> values = comboPropertiesComboBoxes.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String comboPropertyName = keys.next();
			JComboBox f = values.next();

			if (extractor.isIndividualProperty(comboPropertyName))
				continue;

			// Move selected item in first position of the linked list
			selectOptionInComboProperty(extractorToBeConfigured, comboPropertyName, (String) f.getSelectedItem());

		}

	}

	/**
	 * Selects the option matching the given translated name in the given
	 * comboProperty. I.e. moves the matching option in fist position in the
	 * comboProperty list. fc-26.9.2012
	 */
	private void selectOptionInComboProperty(ConfigurableExtractor extractorToBeConfigured, String comboPropertyName,
			String translatedName) {
		try {
			LinkedList<String> list = extractorToBeConfigured.getSettings().getComboProperties ().get(comboPropertyName);
			String option = translatedName;
			try {
				Map<String, String> map = extractor.comboPropertyTranslationToValue.get(comboPropertyName);
				option = map.get(translatedName); // get the original option
													// name (combo box
													// contains translations)

			} catch (Exception e) {
				// Maybe not an error if missing translation
				// Log.println (Log.ERROR, "DataExtractor.sharedConfigure ()",
				// "Exception, comboProperty=" + name
				// + ", trouble while trying to de-translate " +
				// f.getSelectedItem (), e);
			}

			boolean removed = list.remove(option);
			if (removed)
				list.addFirst(option);

		} catch (Exception e) {
			Log.println(Log.ERROR, "ComboPropertyManager.selectOptionInComboProperty ()",
					"error in combo property selection, passed", e);
		}

	}

	/**
	 * This method should not be used, there only for compatibility with an old
	 * class. Could be removed in the future.
	 */
	@Deprecated
	public Map<String, JComboBox> getComboPropertiesComboBoxes() {
		return comboPropertiesComboBoxes;
	}

	/**
	 * Accessor to the extractor being configured.
	 */
	 public ConfigurableExtractor getExtractor () {
		 return extractor;
	 }

}
