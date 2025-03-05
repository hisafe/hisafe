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

package capsis.extension.datarenderer;

import java.awt.BorderLayout;
import java.awt.Component;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import capsis.extension.AbstractDiagram;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.extensiontype.DataRenderer;
import capsis.gui.DUserConfiguration;
import capsis.gui.Pilot;
import capsis.gui.Positioner;
import capsis.gui.Repositionable;
import capsis.util.DummyConfigurationPanel;
import capsis.util.MuteConfigurationPanel;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.configurable.SharedConfigurable;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;

/**
 * An abstract panel data renderer. Extractors and renderers configuration
 * panels merging and processing is managed here.
 * 
 * @author Samuel Dufour-Kowalski - 2010
 */
abstract public class AbstractPanelDataRenderer extends AbstractDiagram
		implements DataRenderer, Repositionable, Configurable {

	// fc-9.12.2015 added AbstractDiagram for better positioner management
	// (order)

	public final static int COMMON_FIRST = 1;
	public final static int INDIVIDUAL_FIRST = 2;
	public final static int LAST_OPENED_FIRST = 3;

	protected DataBlock dataBlock;

	@Override
	public void init(DataBlock db) {
		setLayout(new BorderLayout());
		dataBlock = db;
		reposition();
	}

	/**
	 * If the diagram is a StandViewer, this is the standviewer class name. If the
	 * diagram is a DataRenderer, this is the matching DataExtractor className. Used
	 * by the positioners.
	 */
	@Override
	public String getDiagramClassName() {
		return dataBlock.getExtractorType();
	}

	/**
	 * Allow configuration of the configurable extractors connected to this
	 * renderer. Also manage renderer own configuration.
	 */
	public void openConfigure() {
		openConfigure(LAST_OPENED_FIRST);
	}

	public void openConfigure(int order) {

		List<ConfigurationPanel> configPanels = new ArrayList<ConfigurationPanel>();

		// 1. Shared configuration panel (common config for for all extractors)
		ConfigurationPanel multiPan = null;
		Collection<DataExtractor> v = dataBlock.getDataExtractors();

		boolean isMultiConfigurable = false;
		if (!v.isEmpty()) {
			DataExtractor extr = (DataExtractor) v.iterator().next();
			if (extr instanceof SharedConfigurable) {
				multiPan = ((SharedConfigurable) extr).getSharedConfigPanel(dataBlock);

				// Security: if empty panel, do not add
				if (multiPan != null && !multiPan.isEmpty()) {
					isMultiConfigurable = true;

					// fc-27.11.2014 Trying to get better rendering (size)
					multiPan.setPreferredSize(new Dimension(500, 350));
				}
			}
		}

		// 2. Individual configuration panel (distinct config for each
		// extractor)
		MuteConfigurationPanel individualTab = null;
		String individualTabTitle = Translator.swap("DataRenderer.individual");

		ConfigurationPanel pan = null;
		for (DataExtractor ex : v) {

			// Look if extractor is configurable -> merge their config panel if
			// several found
			if (ex instanceof Configurable) {
				pan = ((Configurable) ex).getConfigPanel(null);

				if (pan != null && !pan.isEmpty()) { // security: if empty
														// panel, do not add
					if (individualTab == null) {
						individualTab = new MuteConfigurationPanel(individualTabTitle);
					}

					// Individual config panels are merged in one single panel
					individualTab.add(AmapTools.cutIfTooLong(ex.getCaption(), 50), ex.getColor(), pan);
				}
			}

			// Extractor registration for multiconfig if needed (see higher)
			if (isMultiConfigurable)
				multiPan.addSharedConfigurable((SharedConfigurable) ex);

		}
		if (individualTab != null) {

			MemoPanel userMemo = new MemoPanel(
					Translator.swap("DEConfigurationPanel.individualConfigurationExplanation"));
			individualTab.add(userMemo, BorderLayout.SOUTH); // fc-27.11.2014

			individualTab.addGlue();
		}

		// 3. DataRenderer's Configuration (rendering config)
		ConfigurationPanel drPan = this.getConfigPanel(null);

		// 3. Extractor property page
		// propertyPan moved in DataRendererPopop class. nb-01.08.2018
		// ConfigurationPanel propertyPan = null; nb-01.08.2018
		DataExtractor ex = null;
		if (!v.isEmpty()) {
			ex = (DataExtractor) v.iterator().next();
			// propertyPan = new DEPropertyPage(ex, dataBlock); nb-01.08.2018
		}

		// Main config panel creation: up to 4 tabs
		if (isMultiConfigurable)
			configPanels.add(multiPan);

		if (individualTab != null)
			configPanels.add(individualTab);

		if (drPan != null)
			configPanels.add(drPan);

		// Moved in DataRendererPopup class. nb-01.08.2018
		// if (propertyPan != null)
		// configPanels.add(propertyPan);

		// fc-31.1.2024 The property is be modifiable in Capsis preferences
		JPanel inspectorPanel = null;
		if (Settings.getProperty("AbstractPanelDataRenderer.addInpectorInConfigPanel", false)) {
			// The dataBlock contains all the extractors in this data renderer
			inspectorPanel = AmapTools.getIntrospectionPanel(dataBlock);
			configPanels.add(new DummyConfigurationPanel("Debug", inspectorPanel));
		}
		// fc-31.1.2024

		// Which tab is selected?
		int selectedIndex = 0;
		String extractorType = getDataBlock().getExtractorType();

		if (order == INDIVIDUAL_FIRST && individualTab != null) {
			if (isMultiConfigurable)
				selectedIndex = 1;

			if (!isMultiConfigurable)
				selectedIndex = 0;

		} else if (order == COMMON_FIRST) {
			selectedIndex = 0;

		} else { // default: LAST_OPENED_FIRST
			selectedIndex = Settings.getProperty(extractorType + ".last.config.panel", 0);

		}

		// This dialog box shows all the configuration panels
		DUserConfiguration dlg = new DUserConfiguration(AmapTools.getWindow(this), dataBlock.getName(), ex,
				configPanels, selectedIndex);
		Settings.setProperty(extractorType + ".last.config.panel", "" + dlg.getCurrentIndex());

		// fc-11.10.2021 if user cancelled, do not update extractors (bug by G. Ligot)
		if (!dlg.isValidDialog()) {
			dlg.dispose();
			return;
		}

		dlg.dispose();

		try {
			dataBlock.updateExtractors();
		} catch (Exception e) {
		}

	}

	/**
	 * Something failed, write a message on the renderer
	 */
	public void security() {
		this.removeAll();
		setLayout(new BorderLayout());
		JLabel l = new JLabel(Translator.swap("Error, please check the Log"));
		LinePanel l1 = new LinePanel();
		l1.add(l);
		l1.addGlue();
		add(l1, BorderLayout.NORTH);
	}

	/**
	 * Configurable interface
	 */
	@Override
	public String getConfigLabel() {
		return getName(); // fc-3.6.2020
//		return ExtensionManager.getName(this); // deprecated
	}

	/**
	 * Configurable interface
	 */
	@Override
	public ConfigurationPanel getConfigPanel(Object param) {
		return null;
	}

	/**
	 * Configurable interface
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
	}

	/**
	 * Configurable interface
	 */
	@Override
	public void postConfig() {
	}

	/**
	 * Positioner
	 */
	@Override
	public void reposition() {
		Pilot.getPositioner().layOut(this);

	}

	@Override
	public void setLayout(Positioner p) {
		p.layoutComponent(this);
	}

	@Override
	public void update() {
	}

	@Override
	public void setUpdating() {
	}

	@Override
	public void setUpdated() {
	}

	@Override
	public Component getPanel() {
		return this;
	}

	@Override
	public DataBlock getDataBlock() {
		return dataBlock;
	}

	@Override
	public void dispose() {
		dataBlock.dispose();

	}

	@Override
	public void close() {
		Pilot.getPositioner().remove(this); // Important
	}

}
