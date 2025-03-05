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

package capsis.commongui.command;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Map;
import java.util.TreeMap;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.JViewport;
import javax.swing.table.TableColumn;

import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.kernel.IdCard;
import capsis.kernel.Project;
import capsis.kernel.Step;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * Configure a Project.
 *
 * @author F. de Coligny - October 2002
 */
public class ProjectConfigurationDialog extends AmapDialog implements ActionListener {

	private JTabbedPane tabs;

	public JTextField projectName;
	private JButton renameProject;

	// fc-4.10.2021 Removed Cancel, now Close
//	private String projectOriginalName;

	private JViewport viewport;

	private Icon infoIcon = IconLoader.getIcon("information_16.png");
	private Icon licenseIcon = IconLoader.getIcon("license_16.png");
	private JButton info;
	private JButton license;

	public JCheckBox dateCorrected;
	// public JTextField dateCorrection;
	public JTextField stepCorrection;

	private JComboBox stepValue;
	private static Map<String, Integer> stepValues;

	private Project project; // project being configured

	// fc-4.10.2021 Close is better here
	private JButton close;
//	private JButton ok;
//	private JButton cancel;
	private JButton help;

	static {
		stepValues = new TreeMap<String, Integer>();
		stepValues.put(Translator.swap("ProjectConfigurationDialog.proposals"), new Integer(1));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.disabled"), new Integer(1));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.year-6months"), new Integer(2));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.year-4months"), new Integer(3));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.year-3months"), new Integer(4));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.year-month"), new Integer(12));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.year-week"), new Integer(52));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.year-day"), new Integer(365));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.month-week"), new Integer(4));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.month-day"), new Integer(30));
		stepValues.put(Translator.swap("ProjectConfigurationDialog.week-day"), new Integer(7));
	}

	/**
	 * Constructor.
	 */
	public ProjectConfigurationDialog(JFrame frame, Project project) {
		super(frame);

		// fc-2.12.2019
		activateLocationMemorization(this.getClass().getName());
		activateSizeMemorization(this.getClass().getName());

		this.project = project;
//		projectOriginalName = project.getName();

		createUI();

		// setPreferredSize (new Dimension (500, 500));
		setModal(true);

		activateSizeMemorization(getClass().getName());

		pack();
		setVisible(true);
	}

	// Someone clicked on info.
	//
	private void infoAction() {
		String mpn = project.getModel().getIdCard().getModelPackageName();
		Helper.helpFor(mpn);
	}

	// Someone clicked on license.
	//
	private void licenseAction() {
		String mpn = project.getModel().getIdCard().getModelPackageName();
		Helper.licenseFor(mpn);
	}

	/**
	 * The Close button was hit
	 */
	private void closeAction() {
//	private void okAction() {

		// fc-4.10.2021 No checks needed in this dialog

		setValidDialog(true);
	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(renameProject)) {

			ProjectNameInput dlg = new ProjectNameInput(project.getName());
			if (dlg.isValidDialog()) {
				project.setName(dlg.getProjectName());

				// fc-4.101.2021 Managed at time of Project addition in the Session and when a
				// Project of the Session is renamed (here)
				project.ensureNameIsUniqueInTheSession();

				viewport.setView(buildProjectTable());

				// fc-28.9.2021 In case diagrams would be open, update their captions

				Current.getInstance().forceUpdate(); // Selector listens

				// Use the stepButtons color refresh system to propagate the event
//				Step currentStep = Current.getInstance().getStep();
//				StepButton sb = ProjectManager.getInstance().getStepButton(currentStep);
//				ButtonColorer.getInstance().changeColor(sb, sb.getColor()); // same color

			}
			dlg.dispose();

		} else if (evt.getSource().equals(dateCorrected)) {
			synchronizeDateTextFields();

		} else if (evt.getSource().equals(stepValue)) {
			stepCorrection.setText("" + stepValues.get(stepValue.getSelectedItem()));
			stepCorrection.setCaretPosition(0);

		} else if (evt.getSource().equals(info)) {
			infoAction();

		} else if (evt.getSource().equals(license)) {
			licenseAction();

		} else if (evt.getSource().equals(close)) {
			closeAction(); // fc-4.10.2021

//		} else if (evt.getSource().equals(ok)) {
//			okAction();
//
//		} else if (evt.getSource().equals(cancel)) {
//			project.setName(projectOriginalName);
//			setValidDialog(false);

		} else if (evt.getSource().equals(help)) {
			Helper.helpFor(this);

		}

	}

	private void synchronizeDateTextFields() {
		// dateCorrection.setEnabled (dateCorrected.isSelected ());
		stepCorrection.setEnabled(dateCorrected.isSelected());
		stepValue.setEnabled(dateCorrected.isSelected());
	}

	// Unused
//	public boolean checksAreOk() {
//
//		return true;
//	}

	protected JComponent buildProjectTable() {
		IdCard idCard = project.getModel().getIdCard();

		Object[][] rows = new Object[][] {
				{ Translator.swap("ProjectConfigurationDialog.projectName"), project.getName() },
				{ Translator.swap("IdCard.model"), idCard.getModelName() },
				{ Translator.swap("IdCard.version"), idCard.getModelVersion() },
				{ Translator.swap("IdCard.type"), idCard.getModelType() },
				{ Translator.swap("IdCard.author"), idCard.getModelAuthor() },
				{ Translator.swap("IdCard.institute"), idCard.getModelInstitute() } };
		Object[] columnNames = new Object[] { Translator.swap("ProjectConfigurationDialog.property"),
				Translator.swap("ProjectConfigurationDialog.value") };

		JTable table = new JTable(rows, columnNames);

		// Columns size
		TableColumn column = null;
		for (int i = 0; i < 2; i++) {
			column = table.getColumnModel().getColumn(i);
			if (i == 0) {
				column.setPreferredWidth(50);
			} else {
				column.setPreferredWidth(100);
			}
		}

		return table;

	}

	protected void createUI() {

		// Main tabbed pane
		tabs = new JTabbedPane();

		// 1. General
		ColumnPanel part1 = new ColumnPanel(Translator.swap("ProjectConfigurationDialog.general"));

		JComponent pan = buildProjectTable();
		JScrollPane panModel = new JScrollPane(pan);
		panModel.setPreferredSize(new Dimension(300, 200));
		viewport = panModel.getViewport();

		// button bar
		renameProject = new JButton(Translator.swap("ProjectConfigurationDialog.renameProject"));
		renameProject.addActionListener(this);

		info = new JButton(Translator.swap("ProjectConfigurationDialog.documentation"), infoIcon);
		info.addActionListener(this);

		license = new JButton(Translator.swap("ProjectConfigurationDialog.license"), licenseIcon);
		license.addActionListener(this);

		// Documentation and License for the current model
		LinePanel buttons = new LinePanel();
		buttons.addGlue();
		buttons.add(renameProject);
		buttons.add(info);
		buttons.add(license);
		buttons.addGlue();

		part1.add(panModel);
		part1.add(buttons);
		part1.addStrut0();

		// 2. Steps visibility
		// ColumnPanel part4 = new ColumnPanel (Translator.swap
		// ("ProjectConfigurationDialog.stepsVisibility"));
		// part4.addStrut0 ();

		// 3. Memorizers
		ColumnPanel part3 = new ColumnPanel(Translator.swap("ProjectConfigurationDialog.memory"));

		LinePanel l4 = new LinePanel();
		// fc-17.8.2018 Moved the label in MemorizerPanel for better appearance
		// l4.add(new NorthPanel(new
		// JWidthLabel(Translator.swap("ProjectConfigurationDialog.Memorizer") +
		// " : ", 150)));
		l4.add(new MemorizerPanel(project));
		l4.addStrut0();

		part3.add(l4);
		part3.addStrut0();

		// 4. Date correction
		JPanel aux = new JPanel(new BorderLayout());
		aux.add(part1, BorderLayout.CENTER);
		aux.add(part3, BorderLayout.SOUTH);
		// if (part5 != null) {aux.add (part5);} // date correction is optionnal

		// setLayout (new BorderLayout ());
		// add (aux, BorderLayout.NORTH);
		// add (part4, BorderLayout.CENTER);

		tabs.addTab(Translator.swap("ProjectConfigurationDialog.project"), /* new NorthPanel (aux) */ aux);

		// Second tab
		JComponent c = Tools.getIntrospectionPanel(project.getModel().getSettings());
		tabs.addTab(Translator.swap("ProjectConfigurationDialog.settings"), c);

		// Tabbed pane at the center
		setLayout(new BorderLayout());
		add(tabs, BorderLayout.CENTER);

		// Control panel
		LinePanel cp = new LinePanel();
		cp.addGlue();

		close = new JButton(Translator.swap("Shared.close"));
		close.addActionListener(this);
//		ImageIcon icon = IconLoader.getIcon("close_16.png");
//		close.setIcon(icon);
		cp.add(close);

//		ok = new JButton(Translator.swap("Shared.ok"));
//		ok.addActionListener(this);
//		ImageIcon icon = IconLoader.getIcon("ok_16.png");
//		ok.setIcon(icon);
//		cp.add(ok);
//
//		cancel = new JButton(Translator.swap("Shared.cancel"));
//		cancel.addActionListener(this);
//		icon = IconLoader.getIcon("cancel_16.png");
//		cancel.setIcon(icon);
//		cp.add(cancel);

		help = new JButton(Translator.swap("Shared.help"));
		help.addActionListener(this);
		ImageIcon icon = IconLoader.getIcon("help_16.png");
		help.setIcon(icon);
		cp.add(help);

		cp.addStrut0();

		// Control panel at the bottom
		add(cp, BorderLayout.SOUTH);

		// Dialog title
		setTitle(Translator.swap("ProjectConfigurationDialog.configureProject") + " - [" + project.getName() + "]");

	}

}
