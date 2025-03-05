package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeSet;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.commongui.util.Tools;
import capsis.extension.dataextractor.configuration.DESettings;
import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import capsis.gui.DListSelector;
import capsis.util.Spiable;
import capsis.util.Spy;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * A manager to display set properties, check user input and apply his choices.
 * A set property proposes to user to choose one or more strings among a set of
 * possible strings. This is done with an item selector.
 * 
 * @author F. de Coligny - September 2021
 */
public class SetPropertyManager extends AbstractPropertyManager implements ActionListener {

	// This is a refactoring of the configuration properties management
	// Related to DESettings (data extractors settings), DEMultiConfPanel (common
	// configuration for all extractors in the same graph) and configurable
	// extractor.

	// Reference to the extractor being configured
	private ConfigurableExtractor extractor;

	// The Settings object of the extractor, containing the properties with their
	// current state
	private DESettings settings;

	/**
	 * Constructor
	 */
	public SetPropertyManager() {

		// init () must be called after the constructor

	}

	/**
	 * Inits the manager to show the set properties of the given data extractor.
	 */
	@Override
	public void init(ConfigurableExtractor extractor) {

		this.extractor = extractor;
		this.settings = extractor.getSettings();

//		booleanPropertiesCheckBoxes = new HashMap<>();

	}

	/**
	 * Returns a propertyPanel to allow user to configure it, e.g. a JCheckBox for a
	 * booleanProperty ; or a JLabel and a JTextField for an intProperty.
	 */
	public ColumnPanel getPropertyPanel(String propertyName) {

		SetPropertyItemSelector SetPropertyItemSelector = (SetPropertyItemSelector) settings.setProperties
				.get(propertyName); // fc-30.8.2012

		// Create JTextField and select button
		JLabel label = new JWidthLabel(Translator.swap(propertyName) + " :", 120);

		// When something happens in SetPropertyItemSelector, f will know (and clear
		// itself)
		SpyJTextField textField = new SpyJTextField(SetPropertyItemSelector,
				Tools.toString(SetPropertyItemSelector.selectedValues), 5);
		textField.setEditable(false);

		JButton selectButton = createSelectButton(propertyName, SetPropertyItemSelector, textField);

		if (!extractor.isPropertyEnabled(propertyName)) { // fc-6.2.2004
			textField.setEnabled(false);
			selectButton.setEnabled(false);
		}

		LinePanel l1 = new LinePanel();
		l1.add(label);
		l1.add(textField);
		l1.add(selectButton);
		l1.addGlue();

		ColumnPanel propertyPanel = new ColumnPanel(0, 0);
		propertyPanel.add(l1);

		return propertyPanel;

	}

	/**
	 * Layout the set properties in the given panel. Set properties: label,
	 * textField and button to open an item selector. If the given propertiesPanel
	 * is null, put the new property panels in the returned map.
	 */
	@Override
	public Map<String, ColumnPanel> layoutProperties(ColumnPanel propertiesPanel) {

		// fc-21.9.2021 Tuning the strategy, will be refactored
		return ConfigPropertyLayoutStrategy.layoutProperties(this, settings.setProperties.keySet(),
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
		Iterator keys = new TreeSet(settings.setProperties.keySet()).iterator();
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

			SetPropertyItemSelector SetPropertyItemSelector = (SetPropertyItemSelector) settings.setProperties
					.get(propertyName); // fc-30.8.2012

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

			// Create JTextField and select button
			JLabel label = new JWidthLabel(Translator.swap(propertyName) + " :", 120);

			// When something happens in SetPropertyItemSelector, f will know (and clear
			// itself)
			SpyJTextField f = new SpyJTextField(SetPropertyItemSelector,
					Tools.toString(SetPropertyItemSelector.selectedValues), 5);
			f.setEditable(false);

			JButton b = createSelectButton(propertyName, SetPropertyItemSelector, f);

			if (!extractor.isPropertyEnabled(propertyName)) { // fc-6.2.2004
				f.setEnabled(false);
				b.setEnabled(false);
			}

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

				l1.add(b);

				l1.addGlue();
				inside.add(l1);
			} else {
				LinePanel l1 = new LinePanel();
				l1.add(label);
				l1.add(f);

				l1.add(b);

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
	 * A select button to open a listSelector related to a setProperty
	 */
	private JButton createSelectButton(String propertyName, SetPropertyItemSelector SetPropertyItemSelector,
			JTextField textField) {
		JButton select = new SetPropertyItemSelectorButton(Translator.swap("Shared.select"), propertyName,
				SetPropertyItemSelector, textField);
		select.addActionListener(this);
		return select;
	}

	/**
	 * Selection processing
	 */
	public void processSelection(SetPropertyItemSelectorButton SetPropertyItemSelectorButton) {
		String propertyName = SetPropertyItemSelectorButton.propertyName;

		// fc-3.7.2018 changed sets by lists below (keep order)
		List possibleValues = SetPropertyItemSelectorButton.itemSelector.possibleValues;
		List selectedValues = SetPropertyItemSelectorButton.itemSelector.selectedValues;
		JTextField textField = SetPropertyItemSelectorButton.textField;

		// fc-17.6.2019 removed "" around propertyName below, was a mistake
		boolean singleSelection = false;

		// Below, null means we do not pass an additional text to explain the selection,
		// the propertyName in the title is enough
		String additionalExplanationText = null;
		DListSelector dlg = new DListSelector(Translator.swap(propertyName), additionalExplanationText, possibleValues,
				selectedValues, singleSelection);

		// DListSelector dlg = new DListSelector(Translator.swap(propertyName),
		// Translator.swap("DataExtractor.selectionText"), new
		// Vector(possibleValues));

		if (dlg.isValidDialog()) {
			List selection = dlg.getSelectedItems();
			textField.setText(Tools.toString(selection)); // update textfield
			SetPropertyItemSelectorButton.itemSelector.selectedValues = selection; // remember
			// selection
		}
		dlg.dispose();
	}

	// Button events processing
	//
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource() instanceof SetPropertyItemSelectorButton) {
			processSelection((SetPropertyItemSelectorButton) evt.getSource());
		}
	}

	/**
	 * Runs checks on the current values of the properties, if trouble, tell user
	 * and return false.
	 */
	@Override
	public boolean checksAreOk() {

		// Check setProperties type (must be a list of string separated by ',')
		Iterator keys = extractor.getSettings().setProperties.keySet().iterator();
		Iterator values = extractor.getSettings().setProperties.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String propertyName = (String) keys.next();
			SetPropertyItemSelector is = (SetPropertyItemSelector) values.next();

			if (extractor.isIndividualProperty(propertyName))
				continue;

			// Check the selected values
			// If trouble, the method tells the user, then return false
			boolean ok = is.checkSelectedValues();
			if (!ok)
				return false; // report the trouble to caller

		}

		return true;
	}

	/**
	 * Applies the current user config in the DESettings of the given extractor
	 */
	@Override
	public void applyConfig(ConfigurableExtractor extractorToBeConfigured) {

		// fc-13.9.2021 This code was in ConfigurableExtractor

		// fc-2.8.2018
		// setProperties memorisation is managed specifically:
		// iterate on setProperties (if any) to remember their values for next
		// time opening
		// fc 2.8.2018
		Iterator keys = extractorToBeConfigured.getSettings().setProperties.keySet().iterator();
		Iterator values = extractorToBeConfigured.getSettings().setProperties.values().iterator();
		while (keys.hasNext() && values.hasNext()) {
			String propertyName = (String) keys.next();

			// ShareConfigure -> shared properties only here
			if (extractor.isIndividualProperty(propertyName))
				continue;

			SetPropertyItemSelector is = (SetPropertyItemSelector) values.next();

			// fc-2.8.2018 Memorize what was selected for this property and the
			// current set of possible values
			Settings.setProperty(propertyName + Tools.encode(is.possibleValues), Tools.encode(is.selectedValues));
		}

	}

	// This text field can be cleared if the spied object notifies it to do so.
	// It is used here with SetPropertyItemSelector.
	//
	private class SpyJTextField extends JTextField implements Spy {

		public SpyJTextField(Spiable o, String text, int columns) {
			super(text, columns);
			o.setSpy(this); // he he
		}

		public void action(Spiable m, Object sth) {
			setText("");
		}
	}

	/**
	 * Accessor to the extractor being configured.
	 */
	public ConfigurableExtractor getExtractor() {
		return extractor;
	}

}
