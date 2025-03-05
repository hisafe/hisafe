package capsis.extension.dataextractor.configuration.propertiesmanager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.swing.ImageIcon;
import javax.swing.JCheckBox;

import capsis.extension.dataextractor.superclass.ConfigurableExtractor;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.IconButton;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**
 * A button to be added in a configuration panel to propagate a given value for
 * a property to opened extractors having the same property.
 * 
 * @author F. de Coligny - September 2021
 */
public class BooleanConfigurationPropagationButton extends IconButton implements ActionListener {

	private static ImageIcon propagationIcon;
	private static ImageIcon pressedIcon;

	static {
		Translator.addBundle("capsis.extension.dataextractor.configuration.ConfigurationLabels");
		IconLoader.addPath("capsis/images");
		propagationIcon = IconLoader.getIcon("wi-fi_14.png");
		pressedIcon = IconLoader.getIcon("wi-fi_12.png");

	}

	private String propertyName;
	private JCheckBox propertyCheckBox;
	private Map<DataExtractor, DataBlock> extractorCandidateForChange;

	/**
	 * Constructor. The extractorCandidateForChange map contains for each extractor
	 * the matching dataBlock (dataBlocks will need an update if we change the
	 * extractors configuration).
	 */
	public BooleanConfigurationPropagationButton(String propertyName, JCheckBox propertyCheckBox,
			Map<DataExtractor, DataBlock> extractorsCandidateForChange) {
		super(propagationIcon, pressedIcon);

		this.propertyName = propertyName;
		this.propertyCheckBox = propertyCheckBox;
		this.extractorCandidateForChange = extractorsCandidateForChange;

		addActionListener(this);

		setToolTipText(Translator.swap("ConfigurationPropagationButton.propagateThisConfigurationToOtherOpenedGraphs"));

		// fc-22.9.2021 Under Windows L&F, a focus mark is painted too small when user
		// clicks on the button, this is not very clean, try to remove this
		setFocusPainted(false);
	}

	/**
	 * Propagates the property value to all opened extractors and updates their
	 * renderers.
	 */
	@Override
	public void actionPerformed(ActionEvent evt) {
		// Click on the button: do the propagation

		// This gathers the dataBlocks to be updated a the end without duplicates.
		Set<DataBlock> dataBlocksToBeUpdated = new HashSet<>(extractorCandidateForChange.values());

		// Apply the new configuration to all candidates
		for (DataExtractor e : extractorCandidateForChange.keySet()) {
			if (e instanceof ConfigurableExtractor) {
				ConfigurableExtractor ce = (ConfigurableExtractor) e;

				ce.setBooleanProperty(propertyName, propertyCheckBox.isSelected());

				// This may record the config for next re-opening
				ce.postConfig();

				// fc-7.9.2021 Needed to force all doExtraction () methods to rerun
				ce.setUpToDate(false);

			}

		}

		// Update the dataBlocks (updates the renderers)
		for (DataBlock db : dataBlocksToBeUpdated) {

			db.updateExtractors();

		}

		// Tell user what was done (in the Capsis main status bar)
		// Each dataBlock has one renderer
		int nGraphsUpdated = dataBlocksToBeUpdated.size();

		String propertyValue = Translator.swap("ConfigurationPropagationButton.true");
		if (!propertyCheckBox.isSelected())
			propertyValue = Translator.swap("ConfigurationPropagationButton.false");

		StatusDispatcher.print(Translator.swap(propertyName) + " : " + propertyValue + " "
				+ Translator.swap("ConfigurationPropagationButton.wasPropagatedIn") + " " + nGraphsUpdated + " "
				+ Translator.swap("ConfigurationPropagationButton.graphs"));

	}

}
