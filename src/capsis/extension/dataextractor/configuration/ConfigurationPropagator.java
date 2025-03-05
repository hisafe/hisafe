package capsis.extension.dataextractor.configuration;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JCheckBox;

import capsis.extension.dataextractor.configuration.propertiesmanager.BooleanConfigurationPropagationButton;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.extensiontype.DataRenderer;
import capsis.gui.MainFrame;
import capsis.gui.Selector;

/**
 * A tool to propagate a configuration value to all opened data extractors
 * having this configuration available. E.g. propagate perHectare to all opened
 * extractors.
 * 
 * @author F. de Coligny - September 2021
 */
public class ConfigurationPropagator {

	static private ConfigurationPropagator instance;
	private Selector selector;

	/**
	 * Singleton pattern, private constructor.
	 */
	private ConfigurationPropagator() {
		instance = this;
		selector = MainFrame.getInstance().getSelector();
	}

	/**
	 * Returns the unique instance of ConfigurationPropagator.
	 */
	static public ConfigurationPropagator getInstance() {
		if (instance == null)
			instance = new ConfigurationPropagator();
		return instance;
	}

	/**
	 * Returns a propagation button is propagation is possible, or null.
	 */
	public BooleanConfigurationPropagationButton getConfigPropagationButton(DataExtractor extractorUnderConfiguration,
			String propertyName, JCheckBox propertyCheckBox) {

		Map<DataExtractor, DataBlock> map = getOpenedExtractorsWithThisConfigProperty(propertyName);

		// fc-7.9.2021 Do NOT remove the extractor being configured: clicking on the
		// propagation button updates all graphs immediately, interesting for a glimpse
		// before closing the configuration dialog of the extractor under configuration	
//		if (!map.isEmpty()) // Remove the extractor being configured
//			map.remove(extractorUnderConfiguration);

		if (map.size() > 1) { // More entries than the one for the extractorUnderConsideration ?
//		if (!map.isEmpty()) {
			// Propagation to others is possible
			return new BooleanConfigurationPropagationButton(propertyName, propertyCheckBox, map);
		} else {
			return null; // No candidates with the same property -> no button
		}

	}

	/**
	 * Returns the opened extractors. For each extractor, we join the matching
	 * DataBlock (dataBlocks will need an update if the properties are changed)
	 */
	public Map<DataExtractor, DataBlock> getOpenedExtractors() {
		Map<DataExtractor, DataBlock> map = new HashMap<>();
		for (DataBlock db : selector.getOpenedDataBlocks()) {
			for (DataExtractor e : db.getDataExtractors()) {
				// extractor - matching dataBlock
				map.put(e, db);
			}
		}
//		System.out.println("[ConfigurationPropagator] #opened extractors: " + map.size());
		return map;
	}

	/**
	 * Returns the opened configurable extractors having the given config property.
	 * For each extractor, we join the matching DataBlock.
	 */
	public Map<DataExtractor, DataBlock> getOpenedExtractorsWithThisConfigProperty(String propertyName) {
		Map<DataExtractor, DataBlock> map = new HashMap<>();

		Map<DataExtractor, DataBlock> openedExtractors = getOpenedExtractors();

		for (DataExtractor e : openedExtractors.keySet()) {
			DataBlock db = openedExtractors.get(e);
			if (e instanceof ConfigurableExtractor) {
				ConfigurableExtractor ce = (ConfigurableExtractor) e;
				if (ce.hasConfigProperty(propertyName))
					map.put(ce, db);

			}

		}
//		System.out.println("[ConfigurationPropagator] #opened extractors matching " + propertyName + ": " + map.size());
		return map;

	}

	// propagateConfig (List<ConfigurableExtractor> targetExtractors, String
	// propertyName, Object value) {
	//
	// }

}
