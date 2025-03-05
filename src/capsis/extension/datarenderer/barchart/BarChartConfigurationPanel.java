/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.extension.datarenderer.barchart;

import java.awt.BorderLayout;

import javax.swing.JOptionPane;
import javax.swing.JTextField;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.configuration.DRConfigurationPanel;
import capsis.util.configurable.Configurable;

/**
 * Configuration panel for BarChart renderer.
 * 
 * @author F. de Coligny - december 2004
 */
public class BarChartConfigurationPanel extends DRConfigurationPanel {
	private BarChart source;
	private BarChartSettings settings;
	public JTextField columnWidth;

	/**
	 * Constructor
	 */
	public BarChartConfigurationPanel(Configurable obj) {
		super(obj);
		source = (BarChart) getConfigurable();
		settings = source.getSettings();

		LinePanel l1 = new LinePanel();
		l1.add(new JWidthLabel(Translator.swap("BarChart.columnWidth") + " :", 120));
		columnWidth = new JTextField(5);
		columnWidth.setText("" + settings.columnWidth);
		l1.add(columnWidth);
		l1.addGlue();

		ColumnPanel master = new ColumnPanel();
		// ~ master.add (l1); // fc - 17.3.2004 Now unused

		mainContent.add(master); // fc-25.11.2014

	}

	public int getColumnWidth() {
		return new Integer(columnWidth.getText()).intValue();
	}

	public boolean checksAreOk() {
		if (!Check.isInt(columnWidth.getText())) {
			JOptionPane.showMessageDialog(this, Translator.swap("BarChart.columnWidthMustBeAnInteger"),
					Translator.swap("Shared.warning"), JOptionPane.WARNING_MESSAGE);
			return false;
		}
		return true;
	}

}
