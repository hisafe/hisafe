/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2003  Francois de Coligny
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
package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JColorChooser;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.defaulttype.TreeList;
import capsis.gui.GrouperChooser;
import capsis.kernel.Step;
import capsis.util.ExportComponent;
import capsis.util.SmartFlowLayout;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColoredButton;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * SVMaid option panel.
 * 
 * @author F. de Coligny - January 2005
 */
public class SVMaidPanel extends JPanel implements ActionListener {
	private JTabbedPane tabs;

	private JCheckBox ckShowAxisNames;
	private JCheckBox ckShowPreviousStep;
	private JCheckBox ckZeroOnX;
	private JCheckBox ckShowGirth;

	private GrouperChooser grouperChooser;
	private JCheckBox ckSelectUnderlyingTrees;
	private JCheckBox ckPerHectare;

	private JCheckBox ckAggregate;

	private ButtonGroup rdGroup2;
	private JRadioButton rdAggregateClassWidth;

	private JRadioButton rdAggregateClassNumber;
	private JTextField aggregateClassWidth;
	private JTextField aggregateClassNumber;
	private JCheckBox ckAggregateMinThreshold;
	private JCheckBox ckAggregateMaxThreshold;
	private JTextField aggregateMinThreshold;
	private JTextField aggregateMaxThreshold;
	private JCheckBox ckEnlargeBars;

	private ColoredButton selectionColorButton; // for the selected bar
	private ColoredButton color2Button; // for the previous step histogram

	private JButton exportButton;
	private Container embedder; // fc - 23.7.2004

	private SVMaid svmaid; // fc - 6.1.2005
	private SVMaidSettings settings; // fc - 6.1.2005
	private Step step1; // fc - 6.1.2005
	private JPanel optionPanel; // fc - 6.1.2005

	/**
	 * Constructor
	 */
	public SVMaidPanel(SVMaid svmaid, SVMaidSettings settings, Step step1, JPanel optionPanel) {
		super();
		this.svmaid = svmaid;
		this.settings = settings;
		this.step1 = step1;
		this.optionPanel = optionPanel;

		setLayout(new BorderLayout());

		Border etched = BorderFactory.createEtchedBorder();

		LinePanel part1 = new LinePanel();

		ColumnPanel part2 = new ColumnPanel();

		ColumnPanel part3 = new ColumnPanel();

		// 1. Common
		JPanel p1 = new JPanel();
		p1.setAlignmentX(Component.LEFT_ALIGNMENT);
		Border b1 = BorderFactory.createTitledBorder(etched, Translator.swap("SVMaid.common"));
		p1.setLayout(new BoxLayout(p1, BoxLayout.Y_AXIS));

		p1.setBorder(b1);

		ckShowAxisNames = new JCheckBox(Translator.swap("SVMaid.showAxisNames"), settings.showAxisNames);
		ckShowPreviousStep = new JCheckBox(Translator.swap("SVMaid.showPreviousStep"), settings.showPreviousStep);
		ckZeroOnX = new JCheckBox(Translator.swap("SVMaid.zeroOnX"), settings.zeroOnX);
		ckShowGirth = new JCheckBox(Translator.swap("SVMaid.showGirth"), settings.showGirth);

		ckSelectUnderlyingTrees = new JCheckBox(Translator.swap("SVMaid.selectUnderlyingTrees"),
				settings.selectUnderlyingTrees);
		ckPerHectare = new JCheckBox(Translator.swap("SVMaid.perHectare"), settings.perHectare);

		exportButton = new JButton(Translator.swap("Shared.export"));
		exportButton.addActionListener(this);

		// NEW...
		boolean checked = settings.grouperMode;
		boolean not = settings.grouperModeNot; // fc - 21.4.2004
		String selectedGrouperName = settings.grouperName;

		// fc-3.6.2020 Restricted to GROUP_ALIVE_TREE types
		grouperChooser = new GrouperChooser(step1.getScene(), TreeList.GROUP_ALIVE_TREE,
				Arrays.asList(TreeList.GROUP_ALIVE_TREE), selectedGrouperName, not, true, checked);

		JPanel l6 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l16 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l14 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l15 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l7 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l17 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l18 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l19 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));

		l6.add(ckShowAxisNames);
		l16.add(ckShowPreviousStep);
		l14.add(ckZeroOnX);
		l15.add(ckShowGirth);

		l7.add(grouperChooser);
		l17.add(ckSelectUnderlyingTrees);
		l18.add(ckPerHectare);
		l19.add(exportButton);

		p1.add(l6);
		p1.add(l16);
		p1.add(l14);
		p1.add(l15);
		p1.add(l7);
		p1.add(l17);
		p1.add(l18);
		p1.add(l19);

		p1.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
		part2.add(p1);
		part2.addStrut0();

		// 2. Stand variables
		JPanel p4 = new JPanel();
		p4.setAlignmentX(Component.LEFT_ALIGNMENT);
		Border b4 = BorderFactory.createTitledBorder(etched, Translator.swap("SVMaid.standVariables"));
		p4.setLayout(new BoxLayout(p4, BoxLayout.Y_AXIS));
		p4.setBorder(b4);

		// 3. Aggregate
		JPanel p3 = new JPanel();
		p3.setAlignmentX(Component.LEFT_ALIGNMENT);
		Border b3 = BorderFactory.createTitledBorder(etched, Translator.swap("SVMaid.aggregation"));
		p3.setLayout(new BoxLayout(p3, BoxLayout.Y_AXIS));
		p3.setBorder(b3);

		ckAggregate = new JCheckBox(Translator.swap("SVMaid.aggregate"), settings.aggregate);
		ckAggregate.addActionListener(this);
		JPanel l20 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));

		l20.add(ckAggregate);

		// Radio buttons for aggregate mode selection
		rdAggregateClassWidth = new JRadioButton(Translator.swap("SVMaid.aggregateClassWidth") + " :");

		rdAggregateClassWidth.addActionListener(this);
		rdAggregateClassNumber = new JRadioButton(Translator.swap("SVMaid.aggregateClassNumber") + " :");

		rdAggregateClassNumber.addActionListener(this);
		rdGroup2 = new ButtonGroup();
		rdGroup2.add(rdAggregateClassWidth);
		rdGroup2.add(rdAggregateClassNumber);

		aggregateClassWidth = new JTextField(5);
		aggregateClassWidth.setText("" + settings.aggregateClassWidth);
		aggregateClassNumber = new JTextField(5);
		aggregateClassNumber.setText("" + settings.aggregateClassNumber);

		switch (settings.aggregateMode) {
		case SVMaidSettings.AGGREGATE_CLASS_WIDTH:
			rdGroup2.setSelected(rdAggregateClassWidth.getModel(), true);
			break;
		case SVMaidSettings.AGGREGATE_CLASS_NUMBER:
			rdGroup2.setSelected(rdAggregateClassNumber.getModel(), true);
			break;
		}

		rdAggregateAction(); // enables /disables the radio text fields
		JPanel l21 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		JPanel l22 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		l21.add(new JWidthLabel("", 20));
		l21.add(rdAggregateClassWidth);
		l21.add(aggregateClassWidth);
		l22.add(new JWidthLabel("", 20));
		l22.add(rdAggregateClassNumber);
		l22.add(aggregateClassNumber);

		// min threshold
		boolean minThreshold = settings.isAggregateMinThreshold;
		ckAggregateMinThreshold = new JCheckBox(Translator.swap("SVMaid.aggregateMinThreshold") + " :", minThreshold);
		ckAggregateMinThreshold.addActionListener(this);
		aggregateMinThreshold = new JTextField(5);
		aggregateMinThreshold.setText("" + settings.aggregateMinThreshold);
		JPanel l23 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		l23.add(new JWidthLabel("", 20));
		l23.add(ckAggregateMinThreshold);
		l23.add(aggregateMinThreshold);

		// max threshold
		boolean maxThreshold = settings.isAggregateMaxThreshold;
		ckAggregateMaxThreshold = new JCheckBox(Translator.swap("SVMaid.aggregateMaxThreshold") + " :", maxThreshold);
		ckAggregateMaxThreshold.addActionListener(this);
		aggregateMaxThreshold = new JTextField(5);
		aggregateMaxThreshold.setText("" + settings.aggregateMaxThreshold);
		JPanel l24 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		l24.add(new JWidthLabel("", 20));
		l24.add(ckAggregateMaxThreshold);
		l24.add(aggregateMaxThreshold);

		ckAggregateMinThresholdAction(); // enables / disables the checkbox's
											// text field
		ckAggregateMaxThresholdAction(); // enables / disables the checkbox's
											// text field

		enableAgregatePanel(ckAggregate.isSelected());
		p3.add(l20);
		p3.add(l21);
		p3.add(l22);
		p3.add(l23);
		p3.add(l24);

		// all JComponents in BoxLayout must have max value maximum size to
		// allow their X extension
		p3.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
		part3.add(p3);

		// 4. Colors
		JPanel p2 = new JPanel();
		p2.setAlignmentX(Component.LEFT_ALIGNMENT);
		Border b2 = BorderFactory.createTitledBorder(etched, Translator.swap("SVMaid.appearance"));
		p2.setLayout(new BoxLayout(p2, BoxLayout.Y_AXIS));
		p2.setBorder(b2);

		// To enlarge histo bars
		JPanel l26 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT));
		ckEnlargeBars = new JCheckBox(Translator.swap("SVMaid.enlargeBars"), settings.enlargeBars);
		l26.add(ckEnlargeBars);

		selectionColorButton = new ColoredButton(settings.selectionColor);
		selectionColorButton.addActionListener(this);

		JPanel l1 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT, 0, 3));
		l1.add(new JWidthLabel(Translator.swap("SVMaid.selectionColor") + " :", 150));
		l1.add(selectionColorButton);

		color2Button = new ColoredButton(settings.color2);
		color2Button.addActionListener(this);

		JPanel l3 = new JPanel(new SmartFlowLayout(FlowLayout.LEFT, 0, 1));
		l3.add(new JWidthLabel(Translator.swap("SVMaid.color2") + " :", 150));
		l3.add(color2Button);

		p2.add(l26);
		p2.add(l1);
		p2.add(l3);

		// all JComponents in BoxLayout must have max value maximum size to
		// allow their X extension
		p2.setMaximumSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE));
		part3.add(p2);
		part3.addStrut0();

		// 5. Correct layout
		part1.add(part2);
		part1.add(part3);
		part1.addStrut0();

		JPanel worker = new JPanel(new BorderLayout());
		worker.add(part1, BorderLayout.CENTER);

		if (optionPanel.getComponentCount() != 0) {
			tabs = new JTabbedPane();
			tabs.addTab(Translator.swap("SVMaid.general"), null, worker);
			tabs.addTab(Translator.swap("SVMaid.options"), null, optionPanel);
			add(tabs, BorderLayout.NORTH); // if 2 panels, put them in a tabbed
											// pane in SVMaidPanel
		} else {
			add(worker, BorderLayout.NORTH); // else, put directly the only
												// panel in SVMaidPanel.
		}

	}

	/**
	 * Actions management
	 */
	public void actionPerformed(ActionEvent evt) {

		if (evt.getSource().equals(selectionColorButton)) {
			Color newColor = JColorChooser.showDialog(this, Translator.swap("SVMaid.chooseAColor"),
					selectionColorButton.getColor());
			selectionColorButton.colorize(newColor);

		} else if (evt.getSource().equals(color2Button)) {
			Color newColor = JColorChooser.showDialog(this, Translator.swap("SVMaid.chooseAColor"),
					color2Button.getColor());
			color2Button.colorize(newColor);

		} else if (evt.getSource().equals(ckAggregate)) {
			enableAgregatePanel(ckAggregate.isSelected());

		} else if (evt.getSource().equals(rdAggregateClassWidth)) {
			rdAggregateAction();

		} else if (evt.getSource().equals(rdAggregateClassNumber)) {
			rdAggregateAction();

		} else if (evt.getSource().equals(ckAggregateMinThreshold)) {
			ckAggregateMinThresholdAction();

		} else if (evt.getSource().equals(ckAggregateMaxThreshold)) {
			ckAggregateMaxThresholdAction();

		} else if (evt.getSource().equals(exportButton)) { // fc - 23.7.2004

			if (embedder instanceof JFrame) {
				new ExportComponent(svmaid, (JFrame) embedder);
			} else {
				new ExportComponent(svmaid, (JDialog) embedder);
			}

		}
	}

	/**
	 * Aggregate panel enabling / disabling
	 */
	private void enableAgregatePanel(boolean yep) {

		rdAggregateClassWidth.setEnabled(yep);
		rdAggregateClassNumber.setEnabled(yep);
		ckAggregateMinThreshold.setEnabled(yep);
		ckAggregateMaxThreshold.setEnabled(yep);

		if (yep) {
			rdAggregateAction();
			ckAggregateMinThresholdAction();
			ckAggregateMaxThresholdAction();

		} else {
			aggregateClassWidth.setEnabled(false);
			aggregateClassNumber.setEnabled(false);
			aggregateMinThreshold.setEnabled(false);
			aggregateMaxThreshold.setEnabled(false);
		}

	}

	/**
	 * To be called on action of each of the aggregate radio buttons
	 */
	private void rdAggregateAction() {
		boolean itsMin = rdGroup2.getSelection().equals(rdAggregateClassWidth.getModel());
		aggregateClassWidth.setEnabled(itsMin);
		aggregateClassNumber.setEnabled(!itsMin);
	}

	/**
	 * On min checkbox selection / deselection
	 */
	private void ckAggregateMinThresholdAction() {
		aggregateMinThreshold.setEnabled(ckAggregateMinThreshold.isSelected());
	}

	/**
	 * On max checkbox selection / deselection
	 */
	private void ckAggregateMaxThresholdAction() {
		aggregateMaxThreshold.setEnabled(ckAggregateMaxThreshold.isSelected());
	}

	/**
	 * GrouperChooser accessor
	 */
	public GrouperChooser getGrouperChooser() {
		return grouperChooser;
	} // fc - 21.4.2004

	/**
	 * Disposal.
	 */
	public void dispose() {
		try {
			tabs.removeAll();
		} catch (Exception e) {
		}
	}

	public JCheckBox getCkShowAxisNames() {
		return ckShowAxisNames;
	}

	public JCheckBox getCkShowPreviousStep() {
		return ckShowPreviousStep;
	}

	public JCheckBox getCkZeroOnX() {
		return ckZeroOnX;
	}

	public JCheckBox getCkShowGirth() {
		return ckShowGirth;
	}

	public JCheckBox getCkSelectUnderlyingTrees() {
		return ckSelectUnderlyingTrees;
	}

	public JCheckBox getCkPerHectare() {
		return ckPerHectare;
	}

	public boolean isAggregate() {
		return ckAggregate.isSelected();
	}

	public double getAggregateClassWidth() {
		return Check.doubleValue(aggregateClassWidth.getText());
	}

	public int getAggregateClassNumber() {
		return Check.intValue(aggregateClassNumber.getText());
	}

	public boolean isAggregateMinThreshold() {
		return ckAggregateMinThreshold.isSelected();
	}

	public boolean isAggregateMaxThreshold() {
		return ckAggregateMaxThreshold.isSelected();
	}

	public double getAggregateMinThreshold() {
		return Check.doubleValue(aggregateMinThreshold.getText());
	}

	public double getAggregateMaxThreshold() {
		return Check.doubleValue(aggregateMaxThreshold.getText());
	}

	public boolean isAggregateClassWidth() {
		return rdGroup2.getSelection().equals(rdAggregateClassWidth.getModel());
	}

	public boolean isAggregateClassNumber() {
		return rdGroup2.getSelection().equals(rdAggregateClassNumber.getModel());
	}

	public JCheckBox getCkEnlargeBars() {
		return ckEnlargeBars;
	}

	public Color getSelectionColor() {
		return selectionColorButton.getColor();
	}

	public Color getColor2() {
		return color2Button.getColor();
	}

	public String getTitle() {
		return Translator.swap(this.getClass().getSimpleName());
	}

}
