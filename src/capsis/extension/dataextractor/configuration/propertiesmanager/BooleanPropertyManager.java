package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JPanel;

import capsis.extension.dataextractor.configuration.ConfigurationPropagator;
import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * A manager to display boolean properties, check user input and apply his
 * choices.
 * 
 * @author F. de Coligny - September 2021
 */
public class BooleanPropertyManager extends AbstractPropertyManager {

	// This is a refactoring of the configuration properties management
	// Related to DESettings (data extractors settings), DEMultiConfPanel (common
	// configuration for all extractors in the same graph) and configurable
	// extractor.

	// Reference to the extractor being configured
	protected ConfigurableExtractor extractor;

	// The Settings object of the extractor, containing the properties with their
	// current state
	private DESettings settings;

	// CheckBoxes to display the boolean properties
	// Key is propertyName, value is the matching checkBox
	protected Map<String, JCheckBox> booleanPropertiesCheckBoxes;

	/**
	 * Constructor
	 */
	public BooleanPropertyManager() {

		// init () must be called after the constructor

	}

	/**
	 * Inits the manager to show the boolean properties of the given data extractor.
	 */
	@Override
	public void init(ConfigurableExtractor extractor) {

		this.extractor = extractor;
		this.settings = extractor.getSettings();

		booleanPropertiesCheckBoxes = new HashMap<>();

	}

	/**
	 * Returns a propertyPanel to allow user to configure it, e.g. a JCheckBox for a
	 * booleanProperty ; or a JLabel and a JTextField for an intProperty.
	 */
	public ColumnPanel getPropertyPanel(String propertyName) {

		// Make the widget and store it
		String translatedName = Translator.swap(propertyName);
		boolean yep = settings.booleanProperties.get(propertyName);

		JCheckBox cb = new JCheckBox(translatedName, yep);

		if (!extractor.isPropertyEnabled(propertyName))
			cb.setEnabled(false);

		booleanPropertiesCheckBoxes.put(propertyName, cb);

		// Propagation panel (with a button added if the user choice can be propagated
		// to other opened extractors having the same property)
		JPanel propertyComponentWithOptions = getPanelWithPropagationFeature(propertyName, cb);

		ColumnPanel propertyPanel = new ColumnPanel (0, 0);
		propertyPanel.add(propertyComponentWithOptions);
		
		return propertyPanel;

	}

	/**
	 * Layout the boolean properties in the given panel. Boolean properties: add
	 * JCheckBoxes. If the given propertiesPanel is null, put the new property
	 * panels in the returned map.
	 */
	@Override
	public Map<String, ColumnPanel> layoutProperties(ColumnPanel propertiesPanel) {

		// fc-21.9.2021 Tuning the strategy, will be refactored
		return ConfigPropertyLayoutStrategy.layoutProperties(this, settings.booleanProperties.keySet(),
				propertiesPanel);

		/*
		 * // fc-17.9.2021 If propertiesPanel is null, return the panels in a map
		 * Map<String, JPanel> mapOfPanels = null; if (propertiesPanel == null) {
		 * mapOfPanels = new HashMap<>(); // i.e. 'mapMode' propertiesPanel = new
		 * ColumnPanel(); // Used to feed the map } // fc-17.9.2021
		 * 
		 * // fc-30.8.2012 added TreeSet below, order was changing (CM & TL)... Iterator
		 * keys = new TreeSet(settings.booleanProperties.keySet()).iterator(); String
		 * prevPrefix = ""; JPanel panel = null; JPanel inside = null; Border etched =
		 * BorderFactory.createEtchedBorder(); String propertyName = null; //
		 * fc-17.9.2021
		 * 
		 * 
		 * String prefix = ""; String suffix = "";
		 * 
		 * while (keys.hasNext()) { propertyName = (String) keys.next();
		 * 
		 * System.out.println("[BooleanPropertyManager], laying out: " + propertyName);
		 * 
		 * if (!extractor.isUserActionEnabled(propertyName)) // fc-10.1.2019 added
		 * isUserActionEnabled () continue;
		 * 
		 * Boolean yep = (Boolean) settings.booleanProperties.get(propertyName); //
		 * fc-30.8.2012
		 * 
		 * if (extractor.isIndividualProperty(propertyName)) continue;
		 * 
		 * 
		 * try { prefix = propertyName.substring(0, propertyName.indexOf("_")); } catch
		 * (Exception e) { } try { suffix =
		 * propertyName.substring(propertyName.indexOf("_") + 1); } catch (Exception e)
		 * { }
		 * 
		 * // If prefix changes, write old prefix's panel if
		 * (!prefix.equals(prevPrefix)) { if (panel != null)
		 * 
		 * addPanelInPropertiesPanelOrMap(propertyName, panel, propertiesPanel,
		 * mapOfPanels); // fc-17.9.2021 // propertiesPanel.add(panel);
		 * 
		 * panel = null; prevPrefix = prefix; }
		 * 
		 * // fc+op-20.3.2008 - wpn_Debardage -> Debardage (suppression du // prefixe,
		 * le name est deja traduit) // Create checkbox String translatedName =
		 * Translator.swap(propertyName); if
		 * (translatedName.equals(Translator.swap(propertyName)) &&
		 * translatedName.indexOf('_') != -1) translatedName = suffix;
		 * 
		 * JCheckBox cb = new JCheckBox(translatedName, yep.booleanValue());
		 * 
		 * if (!extractor.isPropertyEnabled(propertyName)) cb.setEnabled(false); //
		 * fc-6.2.2004
		 * 
		 * booleanPropertiesCheckBoxes.put(propertyName, cb);
		 * 
		 * // Propagation panel call JPanel propertyComponentWithOptions =
		 * getPanelWithPropagationFeature(propertyName, cb);
		 * 
		 * // Manage properties inside panels with a title if (!"".equals(prefix)) { if
		 * (panel == null) { panel = new JPanel(new BorderLayout()); Border border =
		 * BorderFactory.createTitledBorder(etched, Translator.swap(prefix));
		 * panel.setBorder(border);
		 * 
		 * inside = new JPanel(); inside.setLayout(new BoxLayout(inside,
		 * BoxLayout.Y_AXIS)); panel.add(inside, BorderLayout.CENTER); }
		 * inside.add(propertyComponentWithOptions); } else { LinePanel l1 = new
		 * LinePanel(); l1.add(propertyComponentWithOptions); l1.addStrut0(); //
		 * fc-6.9.2021 pcwo now contains glue // l1.addGlue();
		 * 
		 * addPanelInPropertiesPanelOrMap(propertyName, l1, propertiesPanel,
		 * mapOfPanels); // fc-17.9.2021 // propertiesPanel.add(l1); } }
		 * 
		 * // Add last boolean property panel if needed if (panel != null)
		 * 
		 * addPanelInPropertiesPanelOrMap(propertyName, panel, propertiesPanel,
		 * mapOfPanels); // fc-17.9.2021 // propertiesPanel.add(panel);
		 * 
		 * // The returned map is not null if the given propertiesPanel was passed null
		 * return mapOfPanels;
		 */
	}

	/**
	 * Returns a Panel with the given widget inside, and a propagateConfiguration
	 * button if the feature is available (i.e. if other extractors are currently
	 * opened and have the same configuratonProperty).
	 */
	protected JPanel getPanelWithPropagationFeature(String propertyName, JCheckBox propertyComponent) {
		LinePanel l = new LinePanel(0, 4); // O pixel before the first component and after the last one
		l.add(propertyComponent);
		l.addGlue(); // Needed for correct alignment in case there is nothing more added

		// If the feature is possible for this property, add a propagation widget right
		// to the component

		// fc-6.9.2021 Config propagation feature
		ConfigurationPropagator cp = ConfigurationPropagator.getInstance();
		BooleanConfigurationPropagationButton propagate = cp.getConfigPropagationButton(extractor, propertyName,
				propertyComponent);
		if (propagate != null) {
			l.add(propagate);
			l.addStrut0();
		}

		return l;

	}

	/**
	 * Runs checks on the current values of the properties, if trouble, tell user
	 * and return false.
	 */
	@Override
	public boolean checksAreOk() {

		// No checks needed for booleanProperties (no possible error in a checkBox)
		return true;

	}

	/**
	 * Applies the current user config in the DESettings of the given extractor
	 */
	@Override
	public void applyConfig(ConfigurableExtractor extractorToBeConfigured) {

		// Set booleanProperties
		Iterator<String> keys = booleanPropertiesCheckBoxes.keySet().iterator();
		Iterator<JCheckBox> values = booleanPropertiesCheckBoxes.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String name = keys.next();
			JCheckBox cb = values.next();
			if (extractor.isIndividualProperty(name))
				continue;

			extractorToBeConfigured.getSettings().booleanProperties.remove(name);
			extractorToBeConfigured.getSettings().booleanProperties.put(name, new Boolean(cb.isSelected()));
		}

	}

	/**
	 * Accessor to the extractor being configured.
	 */
	public ConfigurableExtractor getExtractor() {
		return extractor;
	}

}
