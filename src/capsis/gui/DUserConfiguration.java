/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.extensiontype.DataExtractor;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.configurable.SharedConfigurable;

/**
 * DUserConfiguration is a dialog box to show and execute configurationPanel
 * instances.
 * 
 * @author F. de Coligny - October 2000, April 2010, November 2011
 */
public class DUserConfiguration extends AmapDialog implements ActionListener, ChangeListener {

	// fc-8.9.2021 Reviewed, added additionalChecks () management after the calls to
	// execute (), see ConfigurableExtractor and its configurationProperties
	// (BooleanPropertiesManager...)

	private String dialogTitle;
	private List<ConfigurationPanel> configPanels;
	private int selectedIndex;
	private int currentIndex;

	private JButton ok;
	private JButton cancel;
	private JButton help;
	private Object helpSource;

	/**
	 * Constructor.
	 */
	public DUserConfiguration(Window parent, String title, Object helpSource, List<ConfigurationPanel> configPans) {
		this(parent, title, helpSource, configPans, 0);
	}

	/**
	 * Constructor 2.
	 */
	public DUserConfiguration(Window parent, String title, Object helpSource, List<ConfigurationPanel> configPans,
			int selectedIndex) {

		super(parent);

		// Help source
		if (helpSource != null)
			this.helpSource = helpSource;
		else
			this.helpSource = this;

		this.selectedIndex = selectedIndex;
		this.currentIndex = selectedIndex;

		dialogTitle = title; // must have been translated
		configPanels = configPans;

		createUI();

		activateSizeMemorization(DUserConfiguration.class.getName());
		// fc-11.1.2021 Added title to the memorization key for finer management
		activateLocationMemorization(DUserConfiguration.class.getName() + "." + title);
//		activateLocationMemorization(DUserConfiguration.class.getName());
		pack();

		show();
	}

	/**
	 * Action on Ok Button
	 */
	private void okAction() {

		// Checks each config panel (e.g. for types: an int was expected...)
		for (ConfigurationPanel configPanel : configPanels) {
			boolean ok = configPanel.checksAreOk();
			if (!ok)
				// In case of trouble, the panel is responsible to tell user with a MesageDialog
				// before returning true
				return; // user was told, abort
		}

		// fc-8.9.2021 Reorganised this
//		// Checks the config panels
//		boolean validCheks = true;
//		Iterator<ConfigurationPanel> i = configPanels.iterator();
//		while (i.hasNext() && validCheks) {
//			ConfigurationPanel configPanel = i.next();
//
//			if (!configPanel.checksAreOk())
//				validCheks = false;
//
//		}
//
//		// If checks are not all ok, do not execute
//		if (!validCheks)
//			return;

		// For each panel, configure associated Configurable with execute()
		for (ConfigurationPanel configPanel : configPanels) {
			configPanel.execute();
		}

		// fc-8.9.2021 Added additionalChecks () call after execute()
		// For each panel, run the additional checks after execute () (e.g. value in
		// expected range, min > max...)
		for (ConfigurationPanel configPanel : configPanels) {
			boolean stillOkAfterExecute = configPanel.additionalChecks();
			if (!stillOkAfterExecute)
				// In case of trouble, the panel is responsible to tell user with a MesageDialog
				// before returning true
				return; // user was told, abort
		}

		setValidDialog(true);
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(helpSource);
		}
	}

	/**
	 * User just changed tabs.
	 */
	public void stateChanged(ChangeEvent e) {

		JTabbedPane pane = (JTabbedPane) e.getSource();
		currentIndex = pane.getSelectedIndex();
	}

	/**
	 * Initialize the GUI.
	 */
	private void createUI() {

		JTabbedPane part1 = new JTabbedPane(JTabbedPane.TOP);
		part1.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

		for (ConfigurationPanel panel : configPanels) {

			if (panel == null)
				continue;

			Color cardColor = null;
			String cardTitle = "";

			if (panel.isSharedConfPanel()) {
				cardTitle = ((SharedConfigurable) panel.getSharedConfigurable()).getSharedConfigLabel();

			} else {
				cardTitle = panel.getConfigurable().getConfigLabel();

				// Color ?
				if (panel.getConfigurable() instanceof DataExtractor) {

					cardColor = ((DataExtractor) panel.getConfigurable()).getColor();
				}
			}

			// part1.addTab(cardTitle, null, panel, null); // fc-25.11.2014
			JPanel aux = new JPanel(new BorderLayout());
			aux.add(panel, BorderLayout.CENTER);
			part1.addTab(cardTitle, null, aux, null);

			// Color !
			if (cardColor != null) {
				int index = part1.indexOfComponent(panel);
				part1.setBackgroundAt(index, cardColor);
			}

		}

		try {
			part1.setSelectedIndex(selectedIndex);
		} catch (Exception e) {
		} // no matter

		part1.addChangeListener(this);

		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		ImageIcon icon = IconLoader.getIcon("ok_16.png");
		ok.setIcon(icon);

		cancel = new JButton(Translator.swap("Shared.cancel"));
		icon = IconLoader.getIcon("cancel_16.png");
		cancel.setIcon(icon);

		help = new JButton(Translator.swap("Shared.help"));
		icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);

		controlPanel.add(ok);
		controlPanel.add(cancel);
		controlPanel.add(help);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);

		// Sets ok as default (see AmapDialog)
		setDefaultButton(ok);
		// ok.setDefaultCapable(true);
		// getRootPane().setDefaultButton(ok);

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(part1, BorderLayout.CENTER);
		getContentPane().add(controlPanel, BorderLayout.SOUTH);

		setTitle(dialogTitle);

		setModal(true);
	}

	public int getCurrentIndex() {
		return currentIndex;
	}

}
