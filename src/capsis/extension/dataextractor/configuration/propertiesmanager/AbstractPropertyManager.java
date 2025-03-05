package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.util.Map;

import javax.swing.JComponent;

import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import jeeb.lib.util.ColumnPanel;

/**
 * A superclass for managers to display configuration properties, check user
 * input and apply his choices.
 * 
 * @author F. de Coligny - September 2021
 */
public abstract class AbstractPropertyManager {

	/**
	 * Inits the manager to show the properties of the given data extractor.
	 */
	abstract public void init(ConfigurableExtractor extractor);

	/**
	 * Returns a propertyPanel to allow user to configure it, e.g. a JCheckBox for
	 * a booleanProperty ; or a JLabel and a JTextField for an intProperty.
	 */
	abstract public ColumnPanel getPropertyPanel(String propertyName);

	/**
	 * If the given panel is not null, layout the properties in it, e.g. checkBoxes
	 * for booleanProperties... If the given propertiesPanel is null, returns a map
	 * with for all propertyName (key), the property panel (value), allows a
	 * specific ordering if needed.
	 */
	abstract public Map<String, ColumnPanel> layoutProperties(ColumnPanel propertiesPanel);

	/**
	 * Runs checks on the current values of the properties, if trouble, tell user
	 * and return false.
	 */
	abstract public boolean checksAreOk();

	/**
	 * Applies the current user config in the DESettings of the given extractor
	 */
	abstract public void applyConfig(ConfigurableExtractor extractorToBeConfigured);

	/**
	 * A tool method for the layoutProperties () methods. These methods write in the
	 * given propertiesPanel if not null, else they return a map with all property
	 * panels (makes it possible to reorder the panels afterwards according to some
	 * sort order). If mapOfPanels is not null, we store the given panel in the map,
	 * else we add the given panel in the propertiesPanel.
	 * 
	 * @param propertyName: name of the property
	 * @param panel: config panel for the property (can contain several properties
	 *        if they are grouped with a common title / border)
	 * @param propertiesPanel: if not null, write panel in it
	 * @param mapOfPanels: if not null, add an entry propertyName, panel
	 */
	protected void addPanelInPropertiesPanelOrMap(String propertyName, ColumnPanel panel, ColumnPanel propertiesPanel,
			Map<String, ColumnPanel> mapOfPanels) {

		// fc-28.10.2021 Changed panel and mapOfPanels type from JPanel to ColumnPanel (with a title)
		
		if (mapOfPanels != null) {

			// Store panel in map (for later possible reordering)
			mapOfPanels.put(propertyName, panel);

//			System.out.println("[AbstractPropertyManager] ** added panel in map for property: "+propertyName);		

		} else if (propertiesPanel != null) {

			// Directly add panel in propertiesPanel
			propertiesPanel.add(panel);

		} else
			throw new RuntimeException(
					"Error , AbstractPropertyManager.addPanelInPropertiesPanelOrMap(), both propertiesPanel and mapOfPanels are null");

	}

	/**
	 * Accessor to the extractor being configured.
	 * @return
	 */
	abstract public ConfigurableExtractor getExtractor ();
	
}
