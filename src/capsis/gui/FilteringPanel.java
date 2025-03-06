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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JPanel;
import javax.swing.JSlider;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;

/**
 * Filtering panel. Allows to set display filters on tree dbh. 
 * (1) a detail threshold (in pixels): trees with dbh > (1) are more 
 *     detailled (closer than observer). 
 * (2) a low and a high threshold (in user coordinates, here cm.): trees with 
 *     dbh < lT or dbh > hT are not drawn.
 * 
 * @author F. de Coligny - march 2001
 */
public class FilteringPanel extends JPanel implements ChangeListener, ActionListener {
	final public static int PIXEL_MIN = 0;
	final public static int PIXEL_MAX = 10;		// pixels
	final public static int ICON_SIZE = 24;
	
	private FilteringPanelSettings s;

	private JSlider detailSlider;
	private JSlider lowSlider;
	private JSlider highSlider;

	private JCheckBox cbDetail;
	private JCheckBox cbLow;
	private JCheckBox cbHigh;

	private JTextField detailField;
	private JTextField lowField;
	private JTextField highField;

	private JButton disableDetail;
	private JButton disableLow;
	private JButton disableHigh;

	/**
	 * UserMin and userMax are the min & max diameters of trees to be drawn.
	 * Detail, low and high thresholds are the current values (low and high
	 * must have been calibrated to be in user interval).
	 */
	public FilteringPanel (FilteringPanelSettings settings) {
		super ();
		
		s = settings;
		checkSecurity ();
		createUI ();
		
	}

	private void checkSecurity () {
		// s : the current settings
		if (s.detailValue < PIXEL_MIN) {s.detailValue = PIXEL_MIN;}
		if (s.detailValue > PIXEL_MAX) {s.detailValue = PIXEL_MAX;}
		
		boolean shouldBeSecured = false;
		
		if (s.isMinimumSet) {
			if (s.minimumValue > s.userMaximumValue 
					|| s.minimumValue < s.userMinimumValue ) {
				shouldBeSecured = true;
			}
		}
		if (s.isMaximumSet) {
			if (s.maximumValue > s.userMaximumValue 
					|| s.maximumValue < s.userMinimumValue) {
				shouldBeSecured = true;
			}
		}
		if (s.isMinimumSet && s.isMaximumSet) {
			if (s.minimumValue >= s.maximumValue) {
				shouldBeSecured = true;
			}
		}
		if (s.userMinimumValue >= s.userMaximumValue) {
			shouldBeSecured = true;
		}
		
		if (shouldBeSecured) {
			s.secure ();	// ensure a consistent mode (everything disabled)
		}
	}
	
	public void stateChanged (ChangeEvent evt) {
		if (evt.getSource ().equals (detailSlider)) {
			detailField.setText (""+detailSlider.getValue ());
		} else if (evt.getSource ().equals (lowSlider)) {
			lowField.setText (""+lowSlider.getValue ());
		} else if (evt.getSource ().equals (highSlider)) {
			highField.setText (""+highSlider.getValue ());
		} 
	}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (disableDetail)) {
			detailSlider.setValue (0);
		} else if (evt.getSource ().equals (disableLow)) {
			lowSlider.setValue (0);
		} else if (evt.getSource ().equals (disableHigh)) {
			highSlider.setValue (Integer.MAX_VALUE);
		} else if (evt.getSource ().equals (cbDetail)) {
			enableDetail (cbDetail.isSelected ());
		} else if (evt.getSource ().equals (cbLow)) {
			enableLow (cbLow.isSelected ());
		} else if (evt.getSource ().equals (cbHigh)) {
			enableHigh (cbHigh.isSelected ());
		}
		
	}

	private void enableDetail (boolean yep) {
		detailSlider.setEnabled (yep);
		detailField.setEnabled (yep);
		disableDetail.setEnabled (yep);
	}

	private void enableLow (boolean yep) {
		lowSlider.setEnabled (yep);
		lowField.setEnabled (yep);
		disableLow.setEnabled (yep);
	}

	private void enableHigh (boolean yep) {
		highSlider.setEnabled (yep);
		highField.setEnabled (yep);
		disableHigh.setEnabled (yep);
	}

	public void dispose () {}

	public FilteringPanelSettings getSettings () {
		s.isDetailSet = cbDetail.isSelected ();
		s.isMinimumSet = cbLow.isSelected ();
		s.isMaximumSet = cbHigh.isSelected ();
		
		s.detailValue = new Integer (detailField.getText ()).intValue ();
		s.minimumValue = new Integer (lowField.getText ()).intValue ();
		s.maximumValue = new Integer (highField.getText ()).intValue ();
		
		return s;
	}

	private void createUI () {
		Border etched = BorderFactory.createEtchedBorder ();
		
		// Detail
		ColumnPanel part1 = new ColumnPanel ();
		
		LinePanel f1 = new LinePanel ();
		cbDetail = new JCheckBox (Translator.swap ("FilteringPanel.detailThreshold"), s.isDetailSet);
		cbDetail.addActionListener (this);
		f1.add (cbDetail);
		f1.addGlue ();
		
		LinePanel l1 = new LinePanel ();
		detailSlider = new JSlider (PIXEL_MIN, PIXEL_MAX, s.detailValue);
		detailSlider.addChangeListener (this);
		detailField = new JTextField (3);
		detailField.setHorizontalAlignment (JTextField.RIGHT);
		ImageIcon icon = IconLoader.getIcon ("stop_16.png");
		disableDetail = new JButton (icon);
		Tools.setSizeExactly (disableDetail);
		//~ disableDetail = new JButton (icon) {
			//~ public Dimension getPreferredSize () {
				//~ return new Dimension (ICON_SIZE, ICON_SIZE);
			//~ }
		//~ };
		disableDetail.setToolTipText (Translator.swap ("Shared.disable"));
		disableDetail.addActionListener (this);
		l1.add (detailSlider);
		l1.add (detailField);
		l1.add (disableDetail);
		l1.addStrut0 ();
		enableDetail (cbDetail.isSelected ());
		
		part1.add (f1);
		part1.add (l1);
		part1.addStrut0 ();
		
		// Minimum
		ColumnPanel part2 = new ColumnPanel ();
		LinePanel f2 = new LinePanel ();
		cbLow = new JCheckBox (Translator.swap ("FilteringPanel.lowThreshold"), s.isMinimumSet);
		cbLow.addActionListener (this);
		f2.add (cbLow);
		f2.addGlue ();
		
		LinePanel l2 = new LinePanel ();
		lowSlider = new JSlider (s.userMinimumValue, s.userMaximumValue, s.minimumValue);
		lowSlider.addChangeListener (this);
		lowField = new JTextField (3);
		lowField.setHorizontalAlignment (JTextField.RIGHT);
		disableLow = new JButton (icon);
		Tools.setSizeExactly (disableLow);
		//~ disableLow = new JButton (icon) {
			//~ public Dimension getPreferredSize () {
				//~ return new Dimension (ICON_SIZE, ICON_SIZE);
			//~ }
		//~ };
		disableLow.setToolTipText (Translator.swap ("Shared.disable"));
		disableLow.addActionListener (this);
		l2.add (lowSlider);
		l2.add (lowField);
		l2.add (disableLow);
		l2.addStrut0 ();
		enableLow (cbLow.isSelected ());
		
		part2.add (f2);
		part2.add (l2);
		part2.addStrut0 ();
		
		// Maximum
		ColumnPanel part3 = new ColumnPanel ();
		LinePanel f3 = new LinePanel ();
		cbHigh = new JCheckBox (Translator.swap ("FilteringPanel.highThreshold"), s.isMaximumSet);
		cbHigh.addActionListener (this);
		f3.add (cbHigh);
		f3.addGlue ();
		
		LinePanel l3 = new LinePanel ();
		highSlider = new JSlider (s.userMinimumValue, s.userMaximumValue, s.maximumValue);
		highSlider.addChangeListener (this);
		highField = new JTextField (3);
		highField.setHorizontalAlignment (JTextField.RIGHT);
		disableHigh = new JButton (icon);
		Tools.setSizeExactly (disableHigh);
		//~ disableHigh = new JButton (icon) {
			//~ public Dimension getPreferredSize () {
				//~ return new Dimension (ICON_SIZE, ICON_SIZE);
			//~ }
		//~ };
		disableHigh.setToolTipText (Translator.swap ("Shared.disable"));
		disableHigh.addActionListener (this);
		l3.add (highSlider);
		l3.add (highField);
		l3.add (disableHigh);
		l3.addStrut0 ();
		enableHigh (cbHigh.isSelected ());
		
		part3.add (f3);
		part3.add (l3);
		part3.addStrut0 ();
		
		ColumnPanel master = new ColumnPanel ();
		
		master.add (part1);
		master.add (part2);
		master.add (part3);
		
		setLayout (new BorderLayout ());
		add (master, BorderLayout.NORTH);
		
		// init fields
		detailField.setText (""+detailSlider.getValue ());
		lowField.setText (""+lowSlider.getValue ());
		highField.setText (""+highSlider.getValue ());
		
	}

}

