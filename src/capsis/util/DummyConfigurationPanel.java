/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2014  Francois de Coligny
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

package capsis.util;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;

import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.configurable.SharedConfigurable;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;

/**
 * A dummy configuration panel, e.g. for filters without configuration needed.
 * 
 * @author F. de Coligny - January 2014
 */
public class DummyConfigurationPanel extends ConfigurationPanel
		implements Configurable /* ActionListener, Pilotable */ {

	// fc-31.1.2024 Added second constructor to display a given panel
	// fc-31.1.2024 Added Configurable interface just upper

	// fc-31.1.2024
	// If first constructor, configurable is set
	// If second constructor, configurable is this dummy panel
	private Configurable configurable;
	
	// fc-31.1.2024 Only id second constructor, else null
	private String specificConfigLabel;

	/**
	 * Constructor
	 */
	public DummyConfigurationPanel(Configurable c, String userMessage) {
		super(c);

		this.configurable = configurable; // fc-31.1.2024

		setLayout(new BorderLayout());

		ColumnPanel master = new ColumnPanel();

		// Show a simple message to user
		LinePanel l0 = new LinePanel();
		JLabel lab1 = new JLabel(userMessage);
		l0.add(lab1);
		l0.addGlue();

		master.add(l0);

		master.addGlue();

		add(master, BorderLayout.NORTH);

	}

	/**
	 * Constructor. The panel is only shown for user information, no configurable
	 * here.
	 */
	public DummyConfigurationPanel(String specificConfigLabel, JPanel panel) {
		// fc-31.1.2024
		// This was built to add a debug mode inspector on the dataBlock / extrators in
		// the configuration panels depending on a configuration variable

		super(); // no configurable here

		this.specificConfigLabel = specificConfigLabel; // fc-31.1.2024
		this.configurable = this; // fc-31.1.2024
		
//		panel.setPreferredSize (null);
		
		setLayout(new BorderLayout());
		add(new JScrollPane(panel), BorderLayout.CENTER);

	}

//	/**
//	 * Events processing
//	 */
//	public void actionPerformed(ActionEvent e) {
//	}

//	/**
//	 * From Pilotable interface
//	 */
//	public JComponent getPilot() {
//
//		// No toolbar
//
//		JToolBar toolbar = new JToolBar();
//		toolbar.setVisible(true);
//
//		return toolbar;
//	}

	/**
	 * From ConfigurationPanel
	 */
	public boolean checksAreOk() {

		// No checks

		return true;
	}

	/**
	 * Command to trigger the configuration, once panel modified. See checksAreOk
	 * ().
	 */
	public void execute() {

		// No execution

	}

	/**
	 * Write the total number of trees as information
	 */
	private void preset() {

		// No preset

	}

	/**
	 * Return the configurable if single mode.
	 */
	public Configurable getConfigurable() {
		return configurable; // fc-31.1.2024 See constructors
//		return (Configurable) configurables.iterator().next(); // first element
	}

	/**
	 * Return the shared configurable if shared mode.
	 */
	public SharedConfigurable getSharedConfigurable() {
		return null;
		// return (SharedConfigurable) configurables.iterator().next(); // first
		// element
	}

	/**
	 * Add a SharedConfigurable to the list. They will all be reconfigured with the
	 * panel.
	 */
	public void addSharedConfigurable(SharedConfigurable c) {
		// Do nothing
	}

	@Override
	public String getConfigLabel() {
		if (configurable == this)
			return specificConfigLabel;
		else
			return configurable.getConfigLabel();
	}

	@Override
	public ConfigurationPanel getConfigPanel(Object param) {
		return this;
	}

	@Override
	public void applyConfig(ConfigurationPanel panel) {
		// Do nothing
	}

	@Override
	public void postConfig() {
		// Do nothing
	}

}
