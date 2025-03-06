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
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.HashMap;
import java.util.Map;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;
import javax.swing.UIManager;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColoredButton;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import jeeb.lib.util.gui.NorthPanel;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.util.Helper;
import capsis.util.JSmartFileChooser;

/**
 * CapsisPreferences is a dialog box to change the general options. This box
 * does not dispose () itself on Ok or Cancel. The user must chek its end status
 * with CapsisPreferences.isValid (). The input variables are accessible through
 * access methods like CapsisPreferences.getXxx () (checks are done by the box).
 * When all is retrieved, the CapsisPreferences.dispose () method must be
 * invoked by the caller.
 *
 * @author F. de Coligny - august 1999 - may 2002
 */
public class CapsisPreferences extends AmapDialog implements ActionListener, TreeSelectionListener {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	static {
		Translator.addBundle("capsis.gui.CapsisPreferences");
	}
	private JTree tree;
	private DefaultMutableTreeNode onStartupTreeNode;
	private DefaultMutableTreeNode userInterfaceTreeNode;
	private DefaultMutableTreeNode externalToolsTreeNode;
	private DefaultMutableTreeNode rememberPathsTreeNode;
	private DefaultMutableTreeNode appearanceTreeNode;

	private JPanel part1;

	private ColumnPanel onStartup;
	private JCheckBox showWelcomeToCapsisDialog;
//	private JTextField downloadHelpFreq;

	private ColumnPanel userInterface;
	// ~ private JCheckBox dialogsAreAlwaysResizable;
	// ~ private JCheckBox dialogsReopenAtSamePlace;
	private JCheckBox mainMenuButtonsBorders;
	// private JCheckBox useSmartInternalFrames;

	// fc-16.11.2020 Restored (see lower)
	private JCheckBox openGraphsWithNewRenderers; // fc-29.7.2016, removed fc-12.1.2018

	private JCheckBox addLegacyRenderersInGraphPopup;
	private JCheckBox compactStep;

	// fc-22.12.2020 Step button size customisation
	private JRadioButton stepSize_60x20;
	private JRadioButton stepSize_60x25;
	private JRadioButton stepSize_75x20;
	private JRadioButton stepSize_75x25;
	private JRadioButton stepSize_100x25;
	private ButtonGroup group1;

	private JCheckBox starsAreVisible;
	private JCheckBox reverseProjectsOrder; // project.manager.reverse.order

	// fc-12.1.2018 number of desktops (pages)
	private JTextField reportPositionerNbDesktops;
	private int reportPositionerNbDesktops_initValue; // to detect user change

	private JTextField internalFrameBorderSize;
	private JTextField toolTipTextField;
	private ColoredButton projectSelectionColor;
	// The capsis.commongui.ProjectManager does not propose background color for
	// selection
	// private ButtonGroup rdGroup1;
	// private JRadioButton selectBorder;
	// private JRadioButton selectBackground;
	
	// fc-31.1.2024 To help debugging, see AbstractPanelDataRenderer
	private JCheckBox addInpectorInConfigPanel;

	private ColumnPanel externalTools;
	private ButtonGroup group2;
	private JRadioButton integratedBrowser;
	private JRadioButton customBrowser;
	private JTextField customBrowserCommand;
	private JButton browserBrowse;
	private JButton browserTest;

	// Removed this option
	// private JTextField defaultEditor;
	// private JButton editorBrowse;

//	private ColumnPanel rememberPaths;
//	private JTextField sessionDirectory;
//	private JTextField scenarioDirectory;
//	private JTextField inventoryDirectory;
//	private JTextField exportDirectory;
//	private JButton sessionBrowse;
//	private JButton scenarioBrowse;
//	private JButton inventoryBrowse;
//	private JButton exportBrowse;

	private ColumnPanel appearance;
	private JTextField appearanceField;
	private Map<String, String> appearanceMap;
	private JList appearanceList;
	private JCheckBox smallTheme;
	private JButton selectAppearance;

	private JButton ok;
	private JButton cancel;
	private JButton help;

	/**
	 * Default constructor.
	 */
	public CapsisPreferences(JFrame frame) {
		super(frame);

		createUI();
		setPreferredSize(new Dimension(700, 500));

		activateSizeMemorization(getClass().getName());

		pack();
		setVisible(true);
	}

	/** Choose session directory */
//	private void sessionBrowseAction() {
//
//		JFileChooser chooser = new JSmartFileChooser(Translator.swap("CapsisPreferences.sessionDirectory"),
//				Translator.swap("CapsisPreferences.select"), Translator.swap("CapsisPreferences.select"),
//				sessionDirectory.getText(), true); // DIRECTORIES_ONLY=true
//		new FileAccessory(chooser);
//
//		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
//																			// :
//																			// approveButton
//																			// text
//																			// was
//																			// already
//																			// set
//
//		if (returnVal == JFileChooser.APPROVE_OPTION) {
//			String selected = chooser.getSelectedFile().toString();
//			sessionDirectory.setText(selected);
//		}
//	}

	/** Choose scenario directory */
//	private void scenarioBrowseAction() {
//
//		JFileChooser chooser = new JSmartFileChooser(Translator.swap("CapsisPreferences.scenarioDirectory"),
//				Translator.swap("CapsisPreferences.select"), Translator.swap("CapsisPreferences.select"),
//				scenarioDirectory.getText(), true); // DIRECTORIES_ONLY=true
//		new FileAccessory(chooser);
//
//		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
//																			// :
//																			// approveButton
//																			// text
//																			// was
//																			// already
//																			// set
//
//		if (returnVal == JFileChooser.APPROVE_OPTION) {
//			String selected = chooser.getSelectedFile().toString();
//			scenarioDirectory.setText(selected);
//		}
//	}

	/** Choose inventory directory */
//	private void inventoryBrowseAction() {
//
//		JFileChooser chooser = new JSmartFileChooser(Translator.swap("CapsisPreferences.inventoryDirectory"),
//				Translator.swap("CapsisPreferences.select"), Translator.swap("CapsisPreferences.select"),
//				inventoryDirectory.getText(), true); // DIRECTORIES_ONLY=true
//		new FileAccessory(chooser);
//
//		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
//																			// :
//																			// approveButton
//																			// text
//																			// was
//																			// already
//																			// set
//
//		if (returnVal == JFileChooser.APPROVE_OPTION) {
//			String selected = chooser.getSelectedFile().toString();
//			inventoryDirectory.setText(selected);
//		}
//	}

	/** Choose browser file */
//	private void exportBrowseAction() {
//
//		JFileChooser chooser = new JSmartFileChooser(Translator.swap("CapsisPreferences.exportDirectory"),
//				Translator.swap("CapsisPreferences.select"), Translator.swap("CapsisPreferences.select"),
//				exportDirectory.getText(), true); // DIRECTORIES_ONLY=true
//		new FileAccessory(chooser);
//
//		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
//																			// :
//																			// approveButton
//																			// text
//																			// was
//																			// already
//																			// set
//
//		if (returnVal == JFileChooser.APPROVE_OPTION) {
//			String selected = chooser.getSelectedFile().toString();
//			exportDirectory.setText(selected);
//		}
//	}

	/** Choose editor file */
	// private void editorBrowseAction() {
	//
	// JFileChooser chooser = new JSmartFileChooser (
	// Translator.swap ("CapsisPreferences.defaultEditor"),
	// Translator.swap ("CapsisPreferences.select"),
	// Translator.swap ("CapsisPreferences.select"),
	// exportDirectory.getText (),
	// false); // DIRECTORIES_ONLY=false
	// new FileAccessory (chooser);
	// int returnVal = chooser.showDialog (MainFrame.getInstance (), null); //
	// null : approveButton text was already set
	//
	// if(returnVal == JFileChooser.APPROVE_OPTION) {
	// String selected = chooser.getSelectedFile ().toString ();
	// defaultEditor.setText (selected);
	// }
	// }

	/** Check Values */
	boolean checkData() {

		// Border size
		if (!Check.isInt(internalFrameBorderSize.getText().trim())) {
			selectTreeNode(userInterfaceTreeNode);
			MessageDialog.print(this, Translator.swap("CapsisPreferences.userInterface") + ": "
					+ Translator.swap("CapsisPreferences.internalFrameBorderSizeShouldBeAnInt"));
			return false;
		}

		int size = Check.intValue(internalFrameBorderSize.getText().trim());
		if (size < 0 || size > 10) {
			selectTreeNode(userInterfaceTreeNode);
			MessageDialog.print(this, Translator.swap("CapsisPreferences.userInterface") + ": "
					+ Translator.swap("CapsisPreferences.internalFrameBorderSizeShouldBeBetween0And10"));
			return false;
		}

		// fc-8.8.2018 DEPRECATED, not working properly
//		// Download Frequency
//		if (!Check.isInt(downloadHelpFreq.getText().trim())) {
//			selectTreeNode(onStartupTreeNode);
//			MessageDialog.print(
//					this,
//					Translator.swap("CapsisPreferences.onStartup") + ": "
//							+ Translator.swap("CapsisPreferences.downloadHelpFreqShouldBeAnInt"));
//			return false;
//		}

		return true;
	}

	/**
	 * If all is correct, loads the new capsisOptions to Settings.getProperties,
	 * then, writes capsisOptions to disk (capsis.root/etc/capsis.options) in order
	 * to reload it when launching capsis again in interactive mode. The process is
	 * described in Options: first Settings.getProperties, overriden by
	 * capsisProperties (capsis.root/etc/capsis.properties) and capsisOptions if it
	 * can be found. Always access to the properties with Settings.getProperty
	 * ("prop", null).
	 */
	private void okAction() {

		if (!checkData())
			return;

		// *** At this point, everything was checked and is guaranteed ok
		// On startup
		Settings.setProperty("capsis.what.do.you.want.to.do", "" + showWelcomeToCapsisDialog.isSelected());

//		Settings.setProperty("capsis.download.offline.help.freq", downloadHelpFreq.getText().trim());

		// User interface
		// ~ Settings.setProperty ("capsis.dialogs.reopen.at.same.place",
		// ~ "" + dialogsReopenAtSamePlace.isSelected ());

		Settings.setProperty("project.manager.compact.mode", "" + compactStep.isSelected());

		// fc-22.12.2020
		if (stepSize_60x20.isSelected())
			Settings.setProperty("step.button.size", "60x20");
		else if (stepSize_60x25.isSelected())
			Settings.setProperty("step.button.size", "60x25");
		else if (stepSize_75x20.isSelected())
			Settings.setProperty("step.button.size", "75x20");
		else if (stepSize_75x25.isSelected())
			Settings.setProperty("step.button.size", "75x25");
		else if (stepSize_100x25.isSelected())
			Settings.setProperty("step.button.size", "100x25");

		Settings.setProperty("Project.interventionStepsAreAlwaysVisible", "" + starsAreVisible.isSelected());

		Settings.setProperty("project.manager.reverse.order", "" + reverseProjectsOrder.isSelected());

		// fc-12.1.2018 reportPositionerNbDesktops must be int and positive
		if (!Check.isInt(reportPositionerNbDesktops.getText().trim())) {
			MessageDialog.print(this, Translator.swap("CapsisPreferences.reportPositionerNbDesktopsMustBeAnInt"));
			return;
		}
		int nb = Check.intValue(reportPositionerNbDesktops.getText().trim());
		if (nb <= 0) {
			MessageDialog.print(this, Translator.swap("CapsisPreferences.reportPositionerNbDesktopsMustBeAnInt"));
			return;
		}
		if (nb != reportPositionerNbDesktops_initValue) {
			// Store new value and update positioner if needed
			Settings.setProperty("ReportPositioner.preferred.nb.desktops", nb);

			Positioner previousPos = Pilot.getPositioner();
			if (previousPos instanceof ReportPositioner) {
				// Code from ReportOrg command
				Pilot.getInstance();
				Pilot.setPositioner(new ReportPositioner(MainFrame.getInstance(), previousPos));
			}

		}

		Settings.setProperty("main.menu.buttons.borders", "" + mainMenuButtonsBorders.isSelected());
		MainFrame.getInstance().updateMainToolBarAppearance();

		// Settings.setProperty ("use.smart.internal.frames",
		// "" + useSmartInternalFrames.isSelected ());

		// fc-16.11.20220 Restored (see lower)
		// fc-12.1.2018
		Settings.setProperty("open.graphs.with.new.renderers", "" + openGraphsWithNewRenderers.isSelected());

		Settings.setProperty("add.legacy.renderers.in.graph.popup", "" + addLegacyRenderersInGraphPopup.isSelected());

		// Border Size
		String sizestr = internalFrameBorderSize.getText().trim();
		int size = Check.intValue(sizestr);
		UIManager.put("InternalFrame.border", BorderFactory.createLineBorder(Color.gray, size));
		Settings.setProperty("internal.frame.border.size", sizestr);

		// ToolTip dismiss delay - nb-01.12.2016
		String toolTipDismissDelayStr = toolTipTextField.getText().trim();
		if (!Check.isInt(toolTipDismissDelayStr)) {
			MessageDialog.print(this,
					Translator.swap("CapsisPreferences.tooltipDismissDelayMustBeAPositiveNonZeroInteger"));
			return;
		}
		int delayInSeconds = Check.intValue(toolTipDismissDelayStr);
		if (delayInSeconds <= 0) {
			MessageDialog.print(this,
					Translator.swap("CapsisPreferences.tooltipDismissDelayMustBeAPositiveNonZeroInteger"));
			return;
		}
		ToolTipManager.sharedInstance().setDismissDelay(delayInSeconds * 1000);
		Settings.setProperty("capsis.tooltip.dismiss.delay", toolTipDismissDelayStr);
		
		// fc-31.1.2024
		Settings.setProperty("AbstractPanelDataRenderer.addInpectorInConfigPanel", addInpectorInConfigPanel.isSelected());
		
		// // Select Border
		// if (selectBorder.isSelected ()) {
		// Settings.setProperty ("project.manager.selection.mode", "border");
		// } else {
		// Settings.setProperty ("project.manager.selection.mode",
		// "background");
		// }
		// project selection color ?

		// External tools
		Settings.setProperty("capsis.use.custom.browser", "" + customBrowser.isSelected());
		Settings.setProperty("capsis.custom.browser.command", customBrowserCommand.getText().trim());
		// Settings.setProperty ("capsis.default.editor", defaultEditor.getText
		// ());

//		// Directories options
//		Settings.setProperty("capsis.session.path", sessionDirectory.getText());
//		Settings.setProperty("capsis.project.path", scenarioDirectory.getText());
//		Settings.setProperty("capsis.inventory.path", inventoryDirectory.getText());
//		Settings.setProperty("capsis.export.path", exportDirectory.getText());

//		ensurePathsValidity();

		// some sm options might have changed
		try {
			ProjectManager.getInstance().update();
		} catch (Exception e) {
		} // maybe it does not exist yet

		// Look and feel
		// applyNewAppearance();

		setValidDialog(true);
	}

//	/**
//	 * Ensures these paths are valid.
//	 */
//	public static void ensurePathsValidity() {
//
//		Settings.setProperty("capsis.session.path",
//				Tools.correctFileSeparators(Settings.getProperty("capsis.session.path", "")));
//		Settings.setProperty("capsis.project.path",
//				Tools.correctFileSeparators(Settings.getProperty("capsis.project.path", "")));
//		Settings.setProperty("capsis.inventory.path",
//				Tools.correctFileSeparators(Settings.getProperty("capsis.inventory.path", "")));
//		Settings.setProperty("capsis.export.path",
//				Tools.correctFileSeparators(Settings.getProperty("capsis.export.path", "")));
//	}

	/** Actions on buttons */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(projectSelectionColor)) {
			Color newColor = JColorChooser.showDialog(this, Translator.swap("CapsisPreferences.chooseAColor"),
					projectSelectionColor.getColor());
			if (newColor != null) {
				projectSelectionColor.colorize(newColor);
				Settings.setProperty("capsis.project.selection.color", newColor/*
																				 * . getRGB ( )
																				 */);
			}
//		} else if (evt.getSource().equals(sessionBrowse)) {
//			sessionBrowseAction();
//		} else if (evt.getSource().equals(scenarioBrowse)) {
//			scenarioBrowseAction();
//		} else if (evt.getSource().equals(inventoryBrowse)) {
//			inventoryBrowseAction();
//		} else if (evt.getSource().equals(exportBrowse)) {
//			exportBrowseAction();
			// } else if (evt.getSource ().equals (editorBrowse)) {
			// editorBrowseAction ();
		} else if (evt.getSource().equals(selectAppearance)) { // fc - 10.9.2003
			applyNewAppearance(); // looks directly in list

		} else if (evt.getSource().equals(customBrowser)) {
			synchroBrowserWidgets();
		} else if (evt.getSource().equals(integratedBrowser)) {
			synchroBrowserWidgets();

		} else if (evt.getSource().equals(browserBrowse)) {
			browserBrowseAction();
		} else if (evt.getSource().equals(browserTest)) {
			browserTestAction();

		} else if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			setValidDialog(false);
		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);
		}
	}

	/** Search browser in filesystem */
	private void browserBrowseAction() {

		JFileChooser chooser = new JSmartFileChooser(Translator.swap("Shared.HTMLBrowser"),
				Translator.swap("CapsisPreferences.select"), Translator.swap("CapsisPreferences.select"),
				PathManager.getInstallDir(), false); // DIRECTORIES_ONLY=false
//		JFileChooser chooser = new JSmartFileChooser(Translator.swap("Shared.HTMLBrowser"),
//				Translator.swap("CapsisPreferences.select"), Translator.swap("CapsisPreferences.select"),
//				sessionDirectory.getText(), false); // DIRECTORIES_ONLY=false
		new FileAccessory(chooser);

		int returnVal = chooser.showDialog(MainFrame.getInstance(), null); // null
																			// :
																			// approveButton
																			// text
																			// was
																			// already
																			// set

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String selected = chooser.getSelectedFile().toString();
			customBrowserCommand.setText(selected);
		}
	}

	/** Test current browser command */
	private void browserTestAction() {
		// Save current values
		String savedUseCustomBrowser = Settings.getProperty("capsis.use.custom.browser", "");
		String savedCustomBrowserCommand = Settings.getProperty("capsis.custom.browser.command", "");

		// Change values for test duration
		Settings.setProperty("capsis.use.custom.browser", "" + customBrowser.isSelected());
		Settings.setProperty("capsis.custom.browser.command", customBrowserCommand.getText().trim());

		try {
			// Multilingual document (french / english...)
			String testUrl = Settings.getProperty("capsis.url", "");
			StatusDispatcher.print(Translator.swap("Shared.launchingBrowser"));
			Helper.showPage(testUrl);
		} catch (Exception e) {
			MessageDialog.print(this, Translator.swap("Shared.commandFailed"), e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
		}

		// Restore saved values
		Settings.setProperty("capsis.use.custom.browser", savedUseCustomBrowser);
		Settings.setProperty("capsis.custom.browser.command", savedCustomBrowserCommand);
	}

	// Apply the selected look and feel (currently selected in laf list)
	private void applyNewAppearance() {

		String lafName = (String) appearanceList.getSelectedValue();

		// No selection -> return
		if (lafName == null) {
			return;
		}

		String lafClassName = (String) appearanceMap.get(lafName);
		try {
			// Reduced theme if needed - fc - 17.11.2003
			//
			if (smallTheme.isSelected()) {
				javax.swing.plaf.metal.MetalLookAndFeel.setCurrentTheme(new capsis.gui.CapsisSmallTheme());
			} else {
				javax.swing.plaf.metal.MetalLookAndFeel.setCurrentTheme(
						// ~ new javax.swing.plaf.metal.DefaultMetalTheme());
						new javax.swing.plaf.metal.OceanTheme()); // fc -
																	// 9.12.2004
			}
			Settings.setProperty("capsis.small.theme", "" + smallTheme.isSelected());

			UIManager.setLookAndFeel(lafClassName);

			// Update complete gui
			SwingUtilities.updateComponentTreeUI(MainFrame.getInstance());
			SwingUtilities.updateComponentTreeUI(this);
			this.validate();

			// ~ pack (); // fc - 6.1.2004 - resize dialog if needed

			// Memo current laf for next boot
			appearanceField.setText(lafClassName);
			Settings.setProperty("capsis.last.lookandfeel", lafClassName);

		} catch (Exception e) {
			StatusDispatcher.print(lafName + " : " + Translator.swap("CapsisPreferences.unavailableForThisPlatform"));
			Log.println(Log.ERROR, "CapsisPreferences.applyNewAppearance ()",
					"could not set new look and feel " + lafName, e);
			try {
				UIManager.setLookAndFeel("javax.swing.plaf.metal.MetalLookAndFeel");
			} catch (Exception e2) { // ...should always be available
				System.out.println("Look and feel error " + e2);
			}
		}
	}

	// Synchronize browser textfield with radio buttons
	//
	private void synchroBrowserWidgets() {
		customBrowserCommand.setEnabled(customBrowser.isSelected());
		browserBrowse.setEnabled(customBrowser.isSelected());
	}

	public void valueChanged(TreeSelectionEvent e) {
		// Returns the last path element of the selection.
		// This method is useful only when the selection model allows a single
		// selection.
		DefaultMutableTreeNode node = (DefaultMutableTreeNode) tree.getLastSelectedPathComponent();

		if (node == null) {
			return;
		}

		Object userObject = node.getUserObject();

		try {
			int treeSelectedRow = tree.getSelectionRows()[0];
			Settings.setProperty("capsis.options.last.panel.selected", "" + treeSelectedRow);
		} catch (Exception exc) {
		} // does not matter if trouble here, just for comfort

		display((JPanel) userObject);
		// ~ if (node.isLeaf()) {
		// ~ BookInfo book = (BookInfo)nodeInfo;
		// ~ displayURL(book.bookURL);
		// ~ } else {
		// ~ displayURL(helpURL);
		// ~ }
	}

	private void selectTreeNode(DefaultMutableTreeNode node) {
		tree.setSelectionPath(new TreePath(node.getPath()));
	}

	private void display(JPanel p) {
		part1.removeAll();
		part1.add(new NorthPanel(p));
		part1.validate();

	}

	private JTree createTree() {
		DefaultMutableTreeNode top = new DefaultMutableTreeNode(Translator.swap("CapsisPreferences.preferences"));

		onStartupTreeNode = new DefaultMutableTreeNode(onStartup);
		top.add(onStartupTreeNode);

		userInterfaceTreeNode = new DefaultMutableTreeNode(userInterface);
		top.add(userInterfaceTreeNode);

		externalToolsTreeNode = new DefaultMutableTreeNode(externalTools);
		top.add(externalToolsTreeNode);

		// fc-8.8.2018 DEPRECATED
//		rememberPathsTreeNode = new DefaultMutableTreeNode(rememberPaths);
//		top.add(rememberPathsTreeNode);

		appearanceTreeNode = new DefaultMutableTreeNode(appearance);
		top.add(appearanceTreeNode);

		JTree tree = new JTree(top);
		return tree;
	}

	// Initialize the GUI
	//
	private void createUI() {

		// On Startup
		onStartup = new ColumnPanel() {
			public String toString() {
				return Translator.swap("CapsisPreferences.onStartup");
			}
		};
		onStartup.setMargin(5);
		onStartup.add(LinePanel.getTitle1(Translator.swap("CapsisPreferences.onStartup")));

		LinePanel l51 = new LinePanel();
		showWelcomeToCapsisDialog = new JCheckBox(Translator.swap("CapsisPreferences.showWelcomeToCapsisDialog"));
		boolean yep = Settings.getProperty("capsis.what.do.you.want.to.do", true);
		showWelcomeToCapsisDialog.setSelected(yep);
		l51.add(showWelcomeToCapsisDialog);
		l51.addGlue();

		// fc-8.8.2018 offline doc downloading was deprecated, does not work properly
//		LinePanel l52 = new LinePanel();
//		l52.add(new JLabel(Translator.swap("CapsisPreferences.downloadOfflineHelpDataFreq") + " : "));
//		downloadHelpFreq = new JTextField();
//		downloadHelpFreq.setText(Settings.getProperty("capsis.download.offline.help.freq", ""));
//		l52.add(downloadHelpFreq);
//		l52.addStrut0();

		onStartup.add(l51);
//		onStartup.add(l52);
		onStartup.addGlue();

		// User Interface
		userInterface = new ColumnPanel() {
			public String toString() {
				return Translator.swap("CapsisPreferences.userInterface");
			}
		};
		userInterface.setMargin(5);
		userInterface.add(LinePanel.getTitle1(Translator.swap("CapsisPreferences.userInterface")));

		// ~ LinePanel l61 = new LinePanel ();
		// ~ dialogsAreAlwaysResizable = new JCheckBox (Translator.swap
		// ("CapsisPreferences.dialogsAreAlwaysResizable"));
		// ~ yep = "true".equals (Settings.getProperty
		// ("capsis.dialogs.always.resizable"), null);
		// ~ dialogsAreAlwaysResizable.setSelected (yep);
		// ~ l61.add (dialogsAreAlwaysResizable);
		// ~ l61.addGlue ();
		// ~ userInterface.add (l61);

		// ~ LinePanel l62 = new LinePanel ();
		// ~ dialogsReopenAtSamePlace = new JCheckBox (Translator.swap
		// ("CapsisPreferences.dialogsReopenAtSamePlace"));
		// ~ yep = Settings.getProperty ("capsis.dialogs.reopen.at.same.place",
		// true);

		// ~ dialogsReopenAtSamePlace.setSelected (yep);
		// ~ l62.add (dialogsReopenAtSamePlace);
		// ~ l62.addGlue ();
		// ~ userInterface.add (l62);

		LinePanel l64 = new LinePanel();
		compactStep = new JCheckBox(Translator.swap("CapsisPreferences.compactStep"));
		yep = Settings.getProperty("project.manager.compact.mode", true);
		compactStep.setSelected(yep);
		l64.add(compactStep);
		l64.addGlue();
		userInterface.add(l64);

		// fc-22.12.2020 Step button size customisation
		LinePanel l64a = new LinePanel();
		l64a.add(new JLabel(Translator.swap("CapsisPreferences.stepButtonSize") + " : "));
		stepSize_60x20 = new JRadioButton(Translator.swap("CapsisPreferences.stepSize_60x20"));
		l64a.add(stepSize_60x20);
		stepSize_60x25 = new JRadioButton(Translator.swap("CapsisPreferences.stepSize_60x25"));
		l64a.add(stepSize_60x25);
		stepSize_75x20 = new JRadioButton(Translator.swap("CapsisPreferences.stepSize_75x20"));
		l64a.add(stepSize_75x20);
		stepSize_75x25 = new JRadioButton(Translator.swap("CapsisPreferences.stepSize_75x25"));
		l64a.add(stepSize_75x25);
		stepSize_100x25 = new JRadioButton(Translator.swap("CapsisPreferences.stepSize_100x25"));
		l64a.add(stepSize_100x25);
		l64a.addGlue();
		userInterface.add(l64a);

		group1 = new ButtonGroup();
		group1.add(stepSize_60x20);
		group1.add(stepSize_60x25);
		group1.add(stepSize_75x20);
		group1.add(stepSize_75x25);
		group1.add(stepSize_100x25);

		// Current setting
		String buttonSize = Settings.getProperty("step.button.size", "75x20");
		if (buttonSize.equals("60x20"))
			stepSize_60x20.setSelected(true);
		else if (buttonSize.equals("60x25"))
			stepSize_60x25.setSelected(true);
		else if (buttonSize.equals("75x20"))
			stepSize_75x20.setSelected(true);
		else if (buttonSize.equals("75x25"))
			stepSize_75x25.setSelected(true);
		else if (buttonSize.equals("100x25"))
			stepSize_100x25.setSelected(true);

		LinePanel l64b = new LinePanel();
		starsAreVisible = new JCheckBox(Translator.swap("CapsisPreferences.starsAreVisible"));
		yep = Settings.getProperty("Project.interventionStepsAreAlwaysVisible", false);
		starsAreVisible.setSelected(yep);
		l64b.add(starsAreVisible);
		l64b.addGlue();
		userInterface.add(l64b);

		LinePanel l65 = new LinePanel();
		reverseProjectsOrder = new JCheckBox(Translator.swap("CapsisPreferences.reverseProjectsOrder"));
		yep = Settings.getProperty("project.manager.reverse.order", true);
		reverseProjectsOrder.setSelected(yep);
		l65.add(reverseProjectsOrder);
		l65.addGlue();
		userInterface.add(l65);

		// fc-12.1.2018
		LinePanel l69 = new LinePanel();
		l69.add(new JLabel(Translator.swap("CapsisPreferences.reportPositionerNbDesktops") + " : "));
		reportPositionerNbDesktops = new JTextField();
		reportPositionerNbDesktops_initValue = ReportPositioner.getNbDesktops();
		reportPositionerNbDesktops.setText("" + reportPositionerNbDesktops_initValue);
		l69.add(reportPositionerNbDesktops);
		l69.addStrut0();
		userInterface.add(l69);

		LinePanel l100 = new LinePanel();
		mainMenuButtonsBorders = new JCheckBox(Translator.swap("CapsisPreferences.mainMenuButtonsBorders"));
		yep = Settings.getProperty("main.menu.buttons.borders", true);
		mainMenuButtonsBorders.setSelected(yep);
		l100.add(mainMenuButtonsBorders);
		l100.addGlue();
		userInterface.add(l100);

		// REMOVED, too hard to tune (see DockedPositioner /
		// SmartInternalFrames)
		// LinePanel l102 = new LinePanel ();
		// useSmartInternalFrames = new JCheckBox (Translator.swap
		// ("CapsisPreferences.useSmartInternalFrames"));
		// yep = Settings.getProperty ("use.smart.internal.frames", false);
		// useSmartInternalFrames.setSelected (yep);
		// l102.add (useSmartInternalFrames);
		// l102.addGlue ();
		// userInterface.add (l102);

		// fc-16.11.2020 RESTORED: when false, PhD's charts will reopen with the legacy
		// renderers
		// -> PhD likes the legacy renderers, adds them in the popup, chooses them to
		// see his graphs, then at reopening time, the new renderers came back.
		// Restoring open graph wuth new renderer makes it possible to disable it and
		// keep the legacy renderers at reopening time
		// (was fc-12.1.2018 REMOVED, see addLegacyRenderersInGraphPopup feature)
		LinePanel l101a = new LinePanel(); // fc-29.7.2016
		openGraphsWithNewRenderers = new JCheckBox(Translator.swap("CapsisPreferences.openGraphsWithNewRenderers"));
		yep = Settings.getProperty("open.graphs.with.new.renderers", true);
		openGraphsWithNewRenderers.setSelected(yep);
		l101a.add(openGraphsWithNewRenderers);
		l101a.addGlue();
		userInterface.add(l101a);

		LinePanel l101 = new LinePanel();
		addLegacyRenderersInGraphPopup = new JCheckBox(
				Translator.swap("CapsisPreferences.addLegacyRenderersInGraphPopup"));
		yep = Settings.getProperty("add.legacy.renderers.in.graph.popup", false);
		addLegacyRenderersInGraphPopup.setSelected(yep);
		l101.add(addLegacyRenderersInGraphPopup);
		l101.addGlue();
		userInterface.add(l101);

		LinePanel l63 = new LinePanel();
		l63.add(new JLabel(Translator.swap("CapsisPreferences.internalFrameBorderSize") + " : "));
		internalFrameBorderSize = new JTextField();
		internalFrameBorderSize.setText("" + Settings.getProperty("internal.frame.border.size", 1));
		l63.add(internalFrameBorderSize);
		l63.addStrut0();
		userInterface.add(l63);

		// nb-01.12.2016
		LinePanel toolTipLinePanel = new LinePanel();
		toolTipLinePanel.add(new JLabel(Translator.swap("CapsisPreferences.toolTipDismissDelay") + " : "));
		toolTipTextField = new JTextField();
		int currentToolTipDelayInSeconds = ToolTipManager.sharedInstance().getDismissDelay() / 1000;
		toolTipTextField
				.setText("" + Settings.getProperty("capsis.tooltip.dismiss.delay", currentToolTipDelayInSeconds));
		toolTipLinePanel.add(toolTipTextField);
		toolTipLinePanel.addStrut0();
		userInterface.add(toolTipLinePanel);

		userInterface.add(LinePanel.getTitle2(Translator.swap("CapsisPreferences.projectSelection")));

		LinePanel l66 = new LinePanel();
		l66.add(new JLabel(Translator.swap("CapsisPreferences.projectSelectionColor") + " : "));
		Color selColor = Settings.getProperty("capsis.project.selection.color", new Color(164, 78, 238));

		projectSelectionColor = new ColoredButton(selColor);
		// ~ projectSelectionColor.setBackground (selColor);
		projectSelectionColor.addActionListener(this);
		l66.add(projectSelectionColor);
		l66.addGlue();
		userInterface.add(l66);

		
		// fc-31.1.2024
		LinePanel l99 = new LinePanel();
		addInpectorInConfigPanel = new JCheckBox(
				Translator.swap("CapsisPreferences.addInpectorInConfigPanel"));
		yep = Settings.getProperty("AbstractPanelDataRenderer.addInpectorInConfigPanel", false);
		addInpectorInConfigPanel.setSelected(yep);
		l99.add(addInpectorInConfigPanel);
		l99.addGlue();
		userInterface.add(l99);
		
		
		// LinePanel l67 = new LinePanel ();
		// l67.add (new JWidthLabel (Translator.swap
		// ("CapsisPreferences.projectSelection")+" : ", 150));
		// selectBorder = new JRadioButton (Translator.swap
		// ("CapsisPreferences.selectBorder"));
		// selectBorder.addActionListener (this);
		// l67.add (selectBorder);
		// l67.addGlue ();
		// userInterface.add (l67);
		//
		// LinePanel l68 = new LinePanel ();
		// l68.add (new JWidthLabel ("", 150));
		// selectBackground = new JRadioButton (Translator.swap
		// ("CapsisPreferences.selectBackground"));
		// selectBackground.addActionListener (this);
		// l68.add (selectBackground);
		// l68.addGlue ();
		// userInterface.add (l68);
		//
		// rdGroup1 = new ButtonGroup ();
		// rdGroup1.add (selectBorder);
		// rdGroup1.add (selectBackground);
		// if ("background".equals (Settings.getProperty
		// ("project.manager.selection.mode", ""))) {
		// rdGroup1.setSelected (selectBackground.getModel (), true);
		// } else {
		// rdGroup1.setSelected (selectBorder.getModel (), true);
		// }
		userInterface.addGlue();

		// External tools
		externalTools = new ColumnPanel() {
			public String toString() {
				return Translator.swap("CapsisPreferences.externalTools");
			}
		};
		externalTools.setMargin(5);
		externalTools.add(LinePanel.getTitle1(Translator.swap("CapsisPreferences.externalTools")));

		LinePanel l71 = new LinePanel();
		l71.add(new JWidthLabel(Translator.swap("Shared.HTMLBrowser") + " : ", 120));
		integratedBrowser = new JRadioButton(Translator.swap("CapsisPreferences.integratedBrowserWithoutFrameSupport"));
		integratedBrowser.addActionListener(this);
		l71.add(integratedBrowser);
		l71.addGlue();
		externalTools.add(l71);

		LinePanel l72 = new LinePanel();
		l72.add(new JWidthLabel("", 120));
		customBrowser = new JRadioButton(Translator.swap("CapsisPreferences.customBrowser"));
		customBrowser.addActionListener(this);
		l72.add(customBrowser);
		customBrowserCommand = new JTextField(5);
		customBrowserCommand.setText(Settings.getProperty("capsis.custom.browser.command", ""));
		browserBrowse = new JButton("...");
		browserBrowse.addActionListener(this);
		l72.add(customBrowserCommand);
		l72.add(browserBrowse);
		l72.addGlue();
		externalTools.add(l72);

		group2 = new ButtonGroup();
		group2.add(integratedBrowser);
		group2.add(customBrowser);
		if (Settings.getProperty("capsis.use.custom.browser", false)) {
			group2.setSelected(customBrowser.getModel(), true);
		} else {
			group2.setSelected(integratedBrowser.getModel(), true);
		}
		synchroBrowserWidgets();

		LinePanel l73 = new LinePanel();
		l73.add(new JWidthLabel("", 120));
		browserTest = new JButton(Translator.swap("Shared.test"));
		browserTest.addActionListener(this);
		l73.add(browserTest);
		l73.addGlue();
		externalTools.add(l73);

		externalTools.add(new JLabel(" "));

		// Editor
		// LinePanel l75 = new LinePanel ();
		// l75.add (new JWidthLabel (Translator.swap
		// ("CapsisPreferences.defaultEditor")+" : ", 80));
		// defaultEditor = new JTextField (5);
		// defaultEditor.setText (Settings.getProperty ("capsis.default.editor",
		// ""));
		// editorBrowse = new JButton ("...");
		// editorBrowse.addActionListener (this);
		// l75.add (defaultEditor);
		// l75.add (editorBrowse);
		// l75.addGlue ();
		// externalTools.add (l75);
		externalTools.addGlue();

		// fc-8.8.2018 DEPRECATED, not useful, managed differently at each directory
		// or file user choice in a CheckingFileChooser in Capsis
//		// Directories
//		rememberPaths = new ColumnPanel() {
//			public String toString() {
//				return Translator.swap("CapsisPreferences.rememberPaths");
//			}
//		};
//		rememberPaths.setMargin(5);
//		rememberPaths.add(LinePanel.getTitle1(Translator.swap("CapsisPreferences.rememberPaths")));
//
//		LinePanel l85 = new LinePanel();
//		l85.add(new JWidthLabel(Translator.swap("CapsisPreferences.session") + " : ", 80));
//		sessionDirectory = new JTextField(5);
//		sessionDirectory.setText(Settings.getProperty("capsis.session.path", ""));
//		sessionBrowse = new JButton("...");
//		sessionBrowse.addActionListener(this);
//		l85.add(sessionDirectory);
//		l85.add(sessionBrowse);
//		l85.addGlue();
//		rememberPaths.add(l85);
//
//		LinePanel l86 = new LinePanel();
//		l86.add(new JWidthLabel(Translator.swap("CapsisPreferences.scenario") + " : ", 80));
//		scenarioDirectory = new JTextField(5);
//		scenarioDirectory.setText(Settings.getProperty("capsis.project.path", ""));
//		scenarioBrowse = new JButton("...");
//		scenarioBrowse.addActionListener(this);
//		l86.add(scenarioDirectory);
//		l86.add(scenarioBrowse);
//		l86.addGlue();
//		rememberPaths.add(l86);
//
//		LinePanel l87 = new LinePanel();
//		l87.add(new JWidthLabel(Translator.swap("CapsisPreferences.inventory") + " : ", 80));
//		inventoryDirectory = new JTextField(5);
//		inventoryDirectory.setText(Settings.getProperty("capsis.inventory.path", PathManager.getDir("data")));
//		inventoryBrowse = new JButton("...");
//		inventoryBrowse.addActionListener(this);
//		l87.add(inventoryDirectory);
//		l87.add(inventoryBrowse);
//		l87.addGlue();
//		rememberPaths.add(l87);
//
//		LinePanel l88 = new LinePanel();
//		l88.add(new JWidthLabel(Translator.swap("CapsisPreferences.export") + " : ", 80));
//		exportDirectory = new JTextField(5);
//		exportDirectory.setText(Settings.getProperty("capsis.export.path", ""));
//		exportBrowse = new JButton("...");
//		exportBrowse.addActionListener(this);
//		l88.add(exportDirectory);
//		l88.add(exportBrowse);
//		l88.addGlue();
//		rememberPaths.add(l88);
//		rememberPaths.addGlue();

		// Appearance
		appearance = new ColumnPanel() {
			public String toString() {
				return Translator.swap("CapsisPreferences.appearance");
			}
		};
		appearance.setMargin(5);
		appearance.add(LinePanel.getTitle1(Translator.swap("CapsisPreferences.appearance")));

		LinePanel l91 = new LinePanel();
		l91.add(new JWidthLabel(Translator.swap("CapsisPreferences.currentAppearance") + " :", 80));
		appearanceField = new JTextField(15);
		String currentLookAndFeel = Settings.getProperty("capsis.last.lookandfeel", "");
		appearanceField.setText(currentLookAndFeel);
		l91.add(appearanceField);
		l91.addStrut0();
		appearance.add(l91);

		LinePanel l92 = new LinePanel();
		l92.add(new JLabel(Translator.swap("CapsisPreferences.availableAppearances") + " : "));
		l92.addGlue();
		appearance.add(l92);

		LinePanel l93 = new LinePanel();
		UIManager.LookAndFeelInfo[] lafs = UIManager.getInstalledLookAndFeels();
		appearanceMap = new HashMap<String, String>();
		String entryToBeSelected = null;
		for (int i = 0; i < lafs.length; i++) {
			String lafName = lafs[i].getName();
			String lafClassName = lafs[i].getClassName();
			if (lafClassName.equals(currentLookAndFeel)) {
				entryToBeSelected = lafName;
			}
			appearanceMap.put(lafName, lafClassName);
		}
		appearanceList = new JList(new Vector<String>(appearanceMap.keySet()));
		appearanceList.setSelectedValue(entryToBeSelected, true); // shouldScroll
																	// is true
		l93.add(new JScrollPane(appearanceList));
		l93.addStrut0();
		appearance.add(l93);

		LinePanel l94 = new LinePanel();
		selectAppearance = new JButton(Translator.swap("CapsisPreferences.applyAppearance"));
		selectAppearance.addActionListener(this);
		l94.add(selectAppearance);
		boolean small = Settings.getProperty("capsis.small.theme", false);
		smallTheme = new JCheckBox(Translator.swap("CapsisPreferences.smallTheme"), small);
		l94.add(smallTheme);
		l94.addGlue();
		appearance.add(l94);
		appearance.addGlue();

		part1 = new JPanel();
		part1.setLayout(new GridLayout(1, 1));

		// The lateral JTree
		tree = createTree();
		tree.setRootVisible(false);
		// ~ tree.setShowsRootHandles (false);
		tree.getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
		tree.addTreeSelectionListener(this);
		int row = Settings.getProperty("capsis.options.last.panel.selected", 0);
		tree.setSelectionRow(row);

		// Main control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		ok = new JButton(Translator.swap("Shared.ok"));
		cancel = new JButton(Translator.swap("Shared.cancel"));
		help = new JButton(Translator.swap("Shared.help"));
		pControl.add(ok);
		pControl.add(cancel);
		pControl.add(help);
		ok.addActionListener(this);
		cancel.addActionListener(this);
		help.addActionListener(this);

		// sets ok as default (see AmapDialog)
		setDefaultButton(ok);

		ColumnPanel left = new ColumnPanel(10, 0);
		left.setMargin(10);
		left.add(new JScrollPane(tree));
		left.addStrut0();

		ColumnPanel right = new ColumnPanel(10, 0);
		right.add(part1);
		right.addStrut0();

		getContentPane().setLayout(new BorderLayout());
		getContentPane().add(left, BorderLayout.WEST);
		getContentPane().add(right, BorderLayout.CENTER);
		getContentPane().add(pControl, BorderLayout.SOUTH);

		setTitle(Translator.swap("CapsisPreferences.option"));

		setModal(true);
	}

}
