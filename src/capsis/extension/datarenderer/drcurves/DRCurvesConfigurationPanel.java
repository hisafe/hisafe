/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.extension.datarenderer.drcurves;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.configuration.DRConfigurationPanel;
import capsis.util.configurable.Configurable;

/**
 * Configuration Panel for DRCurves.
 * 
 * @author F. de Coligny - september 2003
 */
public class DRCurvesConfigurationPanel extends DRConfigurationPanel implements ActionListener {

	private DRCurves source;
	private String extractorType;
	private JCheckBox enlargedMode;
	private JCheckBox linesAsked;
	private JCheckBox alignRightLabelsOnData; // fc-21.11.2014

	private JCheckBox forcedEnabled; // fc - 13.3.2006

	private JTextField forcedXMin; // fc - 21.3.2005
	private JTextField forcedXMax; // fc - 21.3.2005
	private JTextField forcedYMin; // fc - 21.3.2005
	private JTextField forcedYMax; // fc - 21.3.2005

	private double forcedXMinValue;
	private double forcedXMaxValue;
	private double forcedYMinValue;
	private double forcedYMaxValue;

	/**
	 * Constructor
	 */
	public DRCurvesConfigurationPanel(Configurable obj, String extractorType) {
		super(obj);
		source = (DRCurves) getConfigurable();
		this.extractorType = extractorType;

		boolean memoForcedEnabled = Settings.getProperty(extractorType + ".forcedEnabled", false);
		double memoForcedXMin = Settings.getProperty(extractorType + ".forcedXMin", Double.MIN_VALUE);
		double memoForcedXMax = Settings.getProperty(extractorType + ".forcedXMax", Double.MAX_VALUE);
		double memoForcedYMin = Settings.getProperty(extractorType + ".forcedYMin", Double.MIN_VALUE);
		double memoForcedYMax = Settings.getProperty(extractorType + ".forcedYMax", Double.MAX_VALUE);

		LinePanel l1 = new LinePanel();
		enlargedMode = new JCheckBox(Translator.swap("DRCurves.enlargedMode"), source.enlargedMode);
		l1.add(enlargedMode);
		l1.addGlue();

		LinePanel l2 = new LinePanel();
		linesAsked = new JCheckBox(Translator.swap("DRCurves.linesAsked"), source.linesAsked);
		l2.add(linesAsked);
		l2.addGlue();

		LinePanel l3 = new LinePanel();
		alignRightLabelsOnData = new JCheckBox(Translator.swap("DRCurves.alignRightLabelsOnData"),
				source.alignRightLabelsOnData);
		l3.add(alignRightLabelsOnData);
		l3.addGlue();

		// fc - 21.3.2005
		// Panel to force some axes min / max values
		ColumnPanel forced = new ColumnPanel();
		Border etched = BorderFactory.createEtchedBorder();
		Border b = BorderFactory.createTitledBorder(etched, Translator.swap("DRCurves.forcedEdges"));
		forced.setBorder(b);

		// fc - 13.3.2006
		LinePanel l4 = new LinePanel();
		forcedEnabled = new JCheckBox(Translator.swap("DRCurves.forcedEnabled"), memoForcedEnabled);
		forcedEnabled.addActionListener(this);
		l4.add(forcedEnabled);
		l4.addGlue();
		forced.add(l4);

		LinePanel l5 = new LinePanel();
		l5.add(new JWidthLabel(Translator.swap("DRCurves.forcedXMin") + " : ", 60));
		forcedXMin = new JTextField(1);
		if (memoForcedXMin != Double.MIN_VALUE) {
			forcedXMin.setText("" + memoForcedXMin);
		}
		l5.add(forcedXMin);
		// ~ l5.addGlue ();
		// ~ forced.add (l5);

		// ~ LinePanel l6 = new LinePanel ();
		l5.add(new JWidthLabel(Translator.swap("DRCurves.forcedXMax") + " : ", 60));
		forcedXMax = new JTextField(1);
		if (memoForcedXMax != Double.MAX_VALUE) {
			forcedXMax.setText("" + memoForcedXMax);
		}
		l5.add(forcedXMax);
		l5.addGlue();
		forced.add(l5);

		LinePanel l7 = new LinePanel();
		l7.add(new JWidthLabel(Translator.swap("DRCurves.forcedYMin") + " : ", 60));
		forcedYMin = new JTextField(1);
		if (memoForcedYMin != Double.MIN_VALUE) {
			forcedYMin.setText("" + memoForcedYMin);
		}
		l7.add(forcedYMin);
		// ~ l7.addGlue ();
		// ~ forced.add (l7);

		// ~ LinePanel l8 = new LinePanel ();
		l7.add(new JWidthLabel(Translator.swap("DRCurves.forcedYMax") + " : ", 60));
		forcedYMax = new JTextField(1);
		if (memoForcedYMax != Double.MAX_VALUE) {
			forcedYMax.setText("" + memoForcedYMax);
		}
		l7.add(forcedYMax);
		l7.addGlue();
		forced.add(l7);

		LinePanel l9 = new LinePanel();
		l9.add(new JLabel(Translator.swap("DRCurves.specifyOnlyTheValuesToBeForced")));
		forced.add(l9);

		synchroniseOptions();

		ColumnPanel master = new ColumnPanel();
		master.add(l1);
		master.add(l2);
		master.add(l3);
		master.add(forced);

		mainContent.add(master); // fc-25.11.2014

	}

	public void actionPerformed(ActionEvent e) {
		synchroniseOptions();
	}

	public void synchroniseOptions() {
		forcedXMin.setEnabled(forcedEnabled.isSelected());
		forcedXMax.setEnabled(forcedEnabled.isSelected());
		forcedYMin.setEnabled(forcedEnabled.isSelected());
		forcedYMax.setEnabled(forcedEnabled.isSelected());
	}

	public boolean checksAreOk() {

		forcedXMinValue = Double.MIN_VALUE;
		forcedXMaxValue = Double.MAX_VALUE;
		forcedYMinValue = Double.MIN_VALUE;
		forcedYMaxValue = Double.MAX_VALUE;

		// X min & max
		String aux = forcedXMin.getText().trim();
		if (!Check.isEmpty(aux)) {
			if (!Check.isDouble(aux)) {
				MessageDialog.print(this, Translator.swap("DRCurves.xMinMustBeANumber"));
				return false;
			}
			forcedXMinValue = Check.doubleValue(aux);
		}

		aux = forcedXMax.getText().trim();
		if (!Check.isEmpty(aux)) {
			if (!Check.isDouble(aux)) {
				MessageDialog.print(this, Translator.swap("DRCurves.xMaxMustBeANumber"));
				return false;
			}
			forcedXMaxValue = Check.doubleValue(aux);
		}

		if (forcedXMinValue >= forcedXMaxValue) {
			MessageDialog.print(this, Translator.swap("DRCurves.xMinMustBeStrictlyLowerThanXMax"));
			return false;
		}

		// Y min & max
		aux = forcedYMin.getText().trim();
		if (!Check.isEmpty(aux)) {
			if (!Check.isDouble(aux)) {
				MessageDialog.print(this, Translator.swap("DRCurves.yMinMustBeANumber"));
				return false;
			}
			forcedYMinValue = Check.doubleValue(aux);
		}

		aux = forcedYMax.getText().trim();
		if (!Check.isEmpty(aux)) {
			if (!Check.isDouble(aux)) {
				MessageDialog.print(this, Translator.swap("DRCurves.yMaxMustBeANumber"));
				return false;
			}
			forcedYMaxValue = Check.doubleValue(aux);
		}

		if (forcedYMinValue >= forcedYMaxValue) {
			MessageDialog.print(this, Translator.swap("DRCurves.yMinMustBeStrictlyLowerThanYMax"));
			return false;
		}

		return true; // everything's ok
	}

	public boolean isEnlargedMode() {
		return enlargedMode.isSelected();
	}

	public boolean isLinesAsked() {
		return linesAsked.isSelected();
	}

	public boolean isAlignRightLabelsOnData() {
		return alignRightLabelsOnData.isSelected();
	}

	public boolean isForcedEnabled() {
		return forcedEnabled.isSelected();
	}

	public double getForcedXMin() {
		return forcedXMinValue;
	}

	public double getForcedXMax() {
		return forcedXMaxValue;
	}

	public double getForcedYMin() {
		return forcedYMinValue;
	}

	public double getForcedYMax() {
		return forcedYMaxValue;
	}
}
