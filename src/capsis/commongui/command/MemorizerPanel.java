/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2018 INRA 
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
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.JTextPane;

import capsis.app.CapsisExtensionManager;
import capsis.extension.memorizer.CompactMemorizer;
import capsis.extension.memorizer.DefaultMemorizer;
import capsis.extension.memorizer.FlexibleMemorizer;
import capsis.extension.memorizer.FlexibleMemorizerPanel;
import capsis.extension.memorizer.FrequencyMemorizer;
import capsis.extension.memorizer.FrequencyMemorizerPanel;
import capsis.extension.memorizer.InterventionMemorizer;
import capsis.kernel.MemorizerFactory;
import capsis.kernel.Project;
import capsis.kernel.extensiontype.Memorizer;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * Configuration panel for project memory options.
 *
 * @author F. de Coligny - October 2002
 */
public class MemorizerPanel extends JPanel
		implements ActionListener /* ItemListener */ {

	// fc-17.8.2018 reviewed, added Memorizer property panel

	private Project project;

	// private JComboBox combo;

	// fc-20.5.2020
	private JTextField currentMemorizer;
	private JButton changeForDefaultMemorizer;
	private JButton changeForCompactMemorizer;
	private JButton changeForFrequencyMemorizer;
	private JButton changeForFlexibleMemorizer;
	private JButton changeForInterventionMemorizer; // fa-22.02.2023

	private JComponent witness;
	private String lastSelectedOption = "";
	private Map<String, String> class_label;
	private Map<String, String> label_class;

	private LinePanel comboLine; // fc-17.8.2018
	private JScrollPane scroll; // fc-17.8.2018

	/**
	 * Constructor.
	 */
	public MemorizerPanel(Project project) {
		super();
		this.project = project;

		makeOptions();
		createUI();
	}

	/**
	 * Prepare possible options.
	 */
	private void makeOptions() {
		// Memorizer name - className
		class_label = new HashMap<String, String>();

		Collection<String> classNames = CapsisExtensionManager.getInstance()
				.getExtensionClassNames(CapsisExtensionManager.MEMORIZER, project.getModel());

		for (Iterator<String> i = classNames.iterator(); i.hasNext();) {
			String className = (String) i.next();

			String classLittleName = AmapTools.getClassSimpleName(className);

			// fc-15.3.2019 get extension name the good way
			class_label.put(classLittleName, ExtensionManager.getName(className));

			// class_label.put(classLittleName,
			// Translator.swap(classLittleName));
		}

		label_class = new HashMap<String, String>();
		Iterator<String> c = class_label.keySet().iterator();
		Iterator<String> l = class_label.values().iterator();
		while (c.hasNext() && l.hasNext()) {
			label_class.put((String) l.next(), (String) c.next());
		}
	}

	/**
	 * Called when an item is selected in combo.
	 */
	@Override
	public void actionPerformed(ActionEvent e) {

		// fc-17.8.2018

		if (e.getSource().equals(changeForDefaultMemorizer)) {
			optionChangeAction("DefaultMemorizer");

		} else if (e.getSource().equals(changeForCompactMemorizer)) {
			optionChangeAction("CompactMemorizer");

		} else if (e.getSource().equals(changeForFrequencyMemorizer)) {
			optionChangeAction("FrequencyMemorizer");

		} else if (e.getSource().equals(changeForFlexibleMemorizer)) {
			optionChangeAction("FlexibleMemorizer");
			
		// fa-22.02.2023
		} else if (e.getSource().equals(changeForInterventionMemorizer)) {
			optionChangeAction("InterventionMemorizer");

			// } else if (e.getSource().equals(combo)) {
			//
			// String option = (String) combo.getSelectedItem();
			//
			// if (!(option.equals(lastSelectedOption))) {
			// lastSelectedOption = option;
			// optionChangeAction(option);
			// }

		}

	}

	// fc-17.8.2018
	// /**
	// * Called when an item is selected in combo.
	// */
	// public void itemStateChanged(ItemEvent evt) {
	// if (evt.getSource().equals(combo)) {
	//
	// // fc-17.8.2018 Combo sends 2 events: selection and deselection (we
	// // need only one)
	// int state = evt.getStateChange();
	// if (state != ItemEvent.SELECTED)
	// return;
	//
	// System.out.println("MemorizerPanel " + evt.getItem() + " " +
	// evt.getStateChange());
	//
	// Object o = evt.getItem();
	// String option = (String) o;
	// if (!(option.equals(lastSelectedOption))) {
	// lastSelectedOption = option;
	// optionChangeAction(option);
	// }
	// }
	// }

	/**
	 * Action done on combo choice.
	 */
	// private void optionChangeAction(String option) {
	//
	// String className = (String) label_class.get(option);

	private void optionChangeAction(String className) {

		// fc-25.5.2020
		Memorizer currentMemorizer = project.getMemorizer();

		if (className.equals("DefaultMemorizer")) {
			this.project.setMemorizer(MemorizerFactory.createDefaultMemorizer());

		} else if (className.equals("FrequencyMemorizer")) {

			// fc-20.5.2020
			FrequencyMemorizer baseMemorizer = new FrequencyMemorizer();
			// Base on the current if possible
			if (project.getMemorizer() instanceof FrequencyMemorizer)
				baseMemorizer = (FrequencyMemorizer) currentMemorizer;

			FrequencyMemorizerPanel dlg = new FrequencyMemorizerPanel(AmapTools.getWindow(this), baseMemorizer);

			if (dlg.isValidDialog())
				project.setMemorizer(dlg.getFrequencyMemorizer());

		} else if (className.equals("FlexibleMemorizer")) {

			// fc-20.5.2020
			String modelName = project.getModel().getIdCard().getModelName();
			FlexibleMemorizer baseMemorizer = new FlexibleMemorizer(); 

			// Base on the current if possible
			if (project.getMemorizer() instanceof FlexibleMemorizer)
				baseMemorizer = (FlexibleMemorizer) project.getMemorizer();

			Window parentWindow = AmapTools.getWindow(this);
			FlexibleMemorizerPanel dlg = new FlexibleMemorizerPanel(parentWindow, modelName, baseMemorizer);

			if (dlg.isValidDialog())
				try {
					project.setMemorizer(dlg.getFlexibleMemorizer());
				} catch (Exception e) {
					MessageDialog.print(this, Translator.swap("MemorizerPanel.couldNotCreateAFlexibleMemorizer"), e);
					return;

				}

		} else if (className.equals("CompactMemorizer")) {
			this.project.setMemorizer(MemorizerFactory.createCompactMemorizer());

		} else if (className.equals("MinimalMemorizer")) {

			// Minimal memorizer is not functional

			this.project.setMemorizer(MemorizerFactory.createMinimalMemorizer());

		} else if (className.equals("InterventionMemorizer")) { // fa-22.02.2023

			this.project.setMemorizer(MemorizerFactory.createInterventionMemorizer());

		}

		// fc-20.5.2020
		update();

	}

	/**
	 * Choice may have a witness component to show current parameter.
	 */
	private void showWitness(String selectedOption) {

		// // fc-17.8.2018 Always remove witness, then see if a new must be
		// added
		// try {
		// comboLine.remove(witness);
		// } catch (Exception e) {
		// // Maybe no witness to be removed
		// }
		//
		// Memorizer m = this.project.getMemorizer();
		//
		// if (m instanceof FrequencyMemorizer) {
		// // if (selectedOption.equals("FrequencyMemorizer")) {
		//
		// FrequencyMemorizer fm = (FrequencyMemorizer) m;
		//
		// witness = new JTextField(5); // do not add directly
		// ((JTextField) witness).setEditable(false);
		//
		// // FrequencyMemorizer m = (FrequencyMemorizer)
		// // this.project.getMemorizer();
		//
		// ((JTextField) witness).setText(fm.getCaption()); // fc-15.3.2019
		// comboLine.add(witness);
		//
		// // } else {
		// // try {
		// // remove(witness);
		// // } catch (Exception e) {
		// // }
		// }
		//
		// // fc-17.8.2018 Added this property panel for user
		// updateMemorizerPropertyPanel();
		//
		// revalidate();

	}

	// fc-17.8.2018
	private void updateMemorizerPropertyPanel() {

		String className = this.project.getMemorizer().getClass().getName();
		JTextPane area = new JTextPane();
		CapsisExtensionManager.getInstance().getPropertyPanel(className, area);

		scroll.setViewportView(area);

	}

	private void update() {
		Memorizer m = this.project.getMemorizer();

		currentMemorizer.setText(m.getCaption());

		// fc-17.8.2018 Added this property panel for user
		updateMemorizerPropertyPanel();

		// showWitness(className);
	}

	/**
	 * Create the gui.
	 */
	private void createUI() {

		// Find current Memorizer for the project
		ColumnPanel aux = new ColumnPanel();

		LinePanel l1 = new LinePanel();
		l1.add(new JLabel(Translator.swap("MemorizerPanel.currentMemorizer") + " : "));
		currentMemorizer = new JTextField();
		currentMemorizer.setEditable(false);
		l1.add(currentMemorizer);
		l1.addStrut0();
		aux.add(l1);

		// fc-20.5.2020
		LinePanel l2 = new LinePanel(Translator.swap("MemorizerPanel.changeMemorizer"));
		l2.addGlue();

		changeForDefaultMemorizer = new JButton(new DefaultMemorizer().getName());
		changeForDefaultMemorizer.addActionListener(this);
		l2.add(changeForDefaultMemorizer);

		changeForCompactMemorizer = new JButton(new CompactMemorizer().getName());
		changeForCompactMemorizer.addActionListener(this);
		l2.add(changeForCompactMemorizer);

		changeForFrequencyMemorizer = new JButton(new FrequencyMemorizer().getName());
		changeForFrequencyMemorizer.addActionListener(this);
		l2.add(changeForFrequencyMemorizer);

		changeForFlexibleMemorizer = new JButton(new FlexibleMemorizer().getName());
		changeForFlexibleMemorizer.addActionListener(this);
		l2.add(changeForFlexibleMemorizer);
		
		// fa-22.02.2023
		changeForInterventionMemorizer = new JButton(new InterventionMemorizer().getName());
		changeForInterventionMemorizer.addActionListener(this);
		l2.add(changeForInterventionMemorizer);

		l2.addGlue();
		aux.add(l2);

		// fc-17.8.2018
		scroll = new JScrollPane();
		scroll.setMinimumSize(new Dimension(300, 150));

		setLayout(new BorderLayout());
		add(aux, BorderLayout.NORTH);
		add(scroll, BorderLayout.CENTER);

		update();

	}

}
