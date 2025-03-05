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
package capsis.extension.modeltool.woodqualityworkshop;

import javax.swing.JComboBox;

import capsis.kernel.GModel;
import jeeb.lib.util.LinePanel;
import repicea.simulation.treelogger.TreeLogger;
import repicea.simulation.treelogger.TreeLoggerDescription;

/**
 * This LoggerChooserPanel class provides a panel with a JComboBox containing
 * the different TreeLogger objects that are compatible with a specified
 * GModel object. The getTreeLogger() method instantiates the selected logger.
 * @author Mathieu Fortin - June 2010
 */
@SuppressWarnings("serial")
public class LoggerChooserPanel extends LinePanel { // fc-11.12.2023 was JPanel

	private JComboBox combo;

	/**
	 * This panel shows the tree loggers available for a particular model.
	 */
	public LoggerChooserPanel(GModel model) {
		combo = new JComboBox(CapsisTreeLoggerDescription.getMatchingTreeLoggers(model));
		combo.setSelectedIndex(0);
		createUI();
	}
	
	private void createUI() {
		add(combo);
		addStrut0 (); // fc-11.12.2023 better layout
	}
	
	/**
	 * This method returns an instance of the selected tree logger.
	 * @return a TreeLogger instance
	 */
	public TreeLogger getTreeLogger() {
		TreeLoggerDescription treeLoggerDescription = (TreeLoggerDescription) combo.getSelectedItem();
		return treeLoggerDescription.instantiateTreeLogger(false);		// not in script mode
	}
	
	/**
	 * This method return the name of the selected logger or an empty String
	 * if no logger were selected.
	 * @return String name of the selected logger or an empty String
	 */
	public String getSelectedLoggerName() {
		return combo.getSelectedItem().toString();
	}
	
	
	
}
