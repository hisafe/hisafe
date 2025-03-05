package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.JPanel;
import javax.swing.border.Border;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.TitledPanel;
import jeeb.lib.util.Translator;

/**
 * A strategy to layout the widgets for a set of config properties. They can be
 * laid out in a given panel, or returned in a map for deferred layout in case a
 * sorting would be needed.
 * 
 * @author F. de Coligny - September 2021
 */
public class ConfigPropertyLayoutStrategy {

	// fc-21.9.2021 Tuning this with BooleanPropertiesManager and other subclasses
	// of AbstractPropertyManager

	/**
	 * If the given propertiesPanel is not null, layout the widgets in it and return
	 * null. Else return a map containing entries for each config property and the
	 * matching widget (a panel) associated.
	 */
	static public Map<String, ColumnPanel> layoutProperties(AbstractPropertyManager propMan, Set<String> propertyNames,
			ColumnPanel propertiesPanel) {

		// If propertiesPanel is null, return the panels in a map
		// fc-28.10.2021 Changed JPanel to ColumnPanel in mapOfPanels
		Map<String, ColumnPanel> mapOfPanels = null;
		if (propertiesPanel == null) {
			mapOfPanels = new HashMap<>(); // i.e. 'mapMode'
			propertiesPanel = new ColumnPanel(); // Used to feed the map
		}

		Iterator keys = new TreeSet(propertyNames).iterator(); // sort propertyNames

		String propertyName = null;
		String memoPropertyName = null; // to store the propertyName for future processing
		String prefix = null;
		String prevPrefix = null;
		Border etched = BorderFactory.createEtchedBorder();

		// This panel will contain a single propertyPanel or several if they have the
		// same prefix
		// fc-28.10.2021 TitledPanel: the prefix if any goes to title
		ColumnPanel groupPanel = new ColumnPanel(0, 0);

//		Border debugTimeBorder = BorderFactory.createLineBorder(Color.BLUE);
//		groupPanel.setBorder(debugTimeBorder);

		while (keys.hasNext()) {
			propertyName = (String) keys.next();

			if (!propMan.getExtractor().isUserActionEnabled(propertyName))
				continue;
			if (propMan.getExtractor().isIndividualProperty(propertyName))
				continue;

			// The panel with the widget of this property
			ColumnPanel propertyPanel = propMan.getPropertyPanel(propertyName);

			// Check if there is a prefix, e.g. production_volume -> production. If so,
			// group the properties with the same prefix in the same panel
			prefix = null;

			// fc-8.12.2023 grouping can be vetoed in setConfigProperties:
			// setPropertyGroupingInTitledPanelsVeto(true);
			boolean groupingVeto = propMan.getExtractor().isPropertyGroupingInTitledPanelsVeto();

			boolean foundAPrefixMark = propertyName.contains("_");
			if (!groupingVeto && foundAPrefixMark) {
				prefix = propertyName.substring(0, propertyName.indexOf("_"));
			} else {
				prefix = propertyName; // changes for each property -> no grouping
			}
			boolean firstIteration = prevPrefix == null;
			boolean prefixChanged = !firstIteration && !prefix.equals(prevPrefix);

			// Check if prefix changed, if yes write the propertyPanel and reset it for the
			// next property (ies)
			if (prefixChanged) {

				propMan.addPanelInPropertiesPanelOrMap(memoPropertyName, groupPanel, propertiesPanel, mapOfPanels);

				// Prepare the new panel for the following properties
				groupPanel = propertyPanel;
//				groupPanel.setBorder(debugTimeBorder);
				memoPropertyName = propertyName; // fc-22.9.2021

			} else {
				// Same prefix, keep the propertyPanel, will be completed and added later (when
				// prefix changes). Note: if added in the map, it will be associated to the last
				// propertyName of the group

				groupPanel.add(propertyPanel);
				memoPropertyName = propertyName; // fc-22.9.2021

			}

			if (!groupingVeto && foundAPrefixMark) {
				// If the propertyName contains a prefix mark, add a title based on prefix
				// translation
				String title = Translator.swap(prefix);
				Border border = BorderFactory.createTitledBorder(etched, title);
				groupPanel.setBorder(border);

				groupPanel.storeTitle(title); // fc-28.10.2021
			}

			prevPrefix = prefix; // for next iteration
		}

		// Write the last pending groupPanel
		if (groupPanel.getComponentCount() != 0)
			propMan.addPanelInPropertiesPanelOrMap(memoPropertyName, groupPanel, propertiesPanel, mapOfPanels);

		return mapOfPanels;
	}

}
