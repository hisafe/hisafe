/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.commongui.util.Helper;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * MecaDConstraints - Interface for constraints data.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDConstraints extends AmapDialog implements ActionListener {
//checked for c4.1.1_08 - fc - 3.2.2003

	private JRadioButton meanHeight;
	private JRadioButton dominantHeight;
	private ButtonGroup rdGroupHeight;
	private JRadioButton windAtH;
	private JRadioButton windAt10m;
	private ButtonGroup rdGroupWindLevel;
	private JRadioButton stand;
	private JRadioButton edge;
	private ButtonGroup rdGroupLocation;

	private JTextField windSpeedEdgeAtH;
	private JTextField windSpeedEdgeAt10m;

	private JButton reset;
	private JButton ok;
	private JButton cancel;
	private JButton help;

	private MecaConstraints constraints;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDConstraints");
	}


	/**
	 * Constructor.
	 */
	public MecaDConstraints (AmapDialog parent, MecaConstraints constraints, double hMean, double hDom) {
		super (parent);
		this.constraints = constraints;

		NumberFormat nf1 = NumberFormat.getInstance ();
		nf1.setMinimumFractionDigits (1);
		nf1.setMaximumFractionDigits (1);
		nf1.setGroupingUsed (false);
		String hM = "" + nf1.format (hMean);
		String hD = "" + nf1.format (hDom);

		createUI (hM, hD);
		
		setModal (true);
		pack ();
		show ();
	}

	// Radio buttons synchronisation.
	//
	private void rdGroupHeightAction () {
		boolean height = rdGroupHeight.getSelection ().equals (meanHeight.getModel ());
		if (height) {
			constraints.standHeight = "mean";
		}
		height = rdGroupHeight.getSelection ().equals (dominantHeight.getModel ());
		if (height) {
			constraints.standHeight = "dom";
		}
	}

	// Radio buttons synchronisation.
	//
	private void rdGroupWindLevelAction () {
		boolean level = rdGroupWindLevel.getSelection ().equals (windAtH.getModel ());
		if (level) {
			constraints.windAt10m = false;
		}
		level = rdGroupWindLevel.getSelection ().equals (windAt10m.getModel ());
		if (level) {
			constraints.windAt10m = true;
		}
	}

	// Radio buttons synchronisation.
	//
	private void rdGroupLocationAction () {
		boolean location = rdGroupLocation.getSelection ().equals (edge.getModel ());
		if (location) {
			constraints.location = "edge";
		}
		location = rdGroupLocation.getSelection ().equals (stand.getModel ());
		if (location) {
			constraints.location = "stand";
		}
	}

	/**
	 * Reset settings to default settings.
	 */
	private void resetAction () {
		if (constraints.STAND_HEIGHT.equals ("mean")) {
			rdGroupHeight.setSelected (meanHeight.getModel (), true);
		} else if (constraints.STAND_HEIGHT.equals ("dom")) {
			rdGroupHeight.setSelected (dominantHeight.getModel (), true);
		}
		rdGroupHeightAction ();

		if (constraints.WIND_AT_10M) {
			rdGroupWindLevel.setSelected (windAt10m.getModel (), true);
		} else {
			rdGroupWindLevel.setSelected (windAtH.getModel (), true);
		}
		rdGroupWindLevelAction ();

		windSpeedEdgeAtH.setText (""+constraints.WIND_SPEED_EDGE_AT_H);
		windSpeedEdgeAt10m.setText (""+constraints.WIND_SPEED_EDGE_AT_10M);

		if (constraints.LOCATION.equals ("edge")) {
			rdGroupLocation.setSelected (edge.getModel (), true);
		} else if (constraints.LOCATION.equals ("stand")) {
			rdGroupLocation.setSelected (stand.getModel (), true);
		}
		rdGroupLocationAction ();

		repaint();
	}

	/**
	 * Change settings to collected settings.
	 */
	private void okAction () {
		// 1. checks...

		boolean level = rdGroupWindLevel.getSelection ().equals (windAtH.getModel ());
		if (level) {
			if (Check.isEmpty (windSpeedEdgeAtH.getText ()) || !Check.isDouble (windSpeedEdgeAtH.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaDConstraints.windSpeedIsNotANumber"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
		}

		level = rdGroupWindLevel.getSelection ().equals (windAt10m.getModel ());
		if (level) {
			if (Check.isEmpty (windSpeedEdgeAt10m.getText ()) || !Check.isDouble (windSpeedEdgeAt10m.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaDConstraints.windSpeedIsNotANumber"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
		}

		// 2. retrieve the collected data and overide settings attributes
		//    (initialy set to default values) with them
		constraints.windSpeedEdgeAtH = Check.doubleValue (windSpeedEdgeAtH.getText ());
		constraints.windSpeedEdgeAt10m = Check.doubleValue (windSpeedEdgeAt10m.getText ());
		setValidDialog (true);
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (reset)) {
			resetAction ();
		} else if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		} else if (evt.getSource ().equals (meanHeight) || evt.getSource ().equals (dominantHeight)) {
			rdGroupHeightAction ();
		} else if (evt.getSource ().equals (windAtH) || evt.getSource ().equals (windAt10m)) {
			rdGroupWindLevelAction ();
		} else if (evt.getSource ().equals (edge) || evt.getSource ().equals (stand)) {
			rdGroupLocationAction ();
		}
	}

	/**
	 * User interface definition.
	 */
	private void createUI (String hM, String hD) {
		Border etched = BorderFactory.createEtchedBorder ();

		// 1. Lines parameters
		JPanel p1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		Border bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraints.title"));
		p1.setBorder (bor);

		Box part1 = Box.createVerticalBox ();

		JPanel p2 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		bor = BorderFactory.createTitledBorder (etched,
				Translator.swap ("MecaDConstraints.giveStandHeight1") + " " + hM + " (m) " +
				Translator.swap ("MecaDConstraints.giveStandHeight2") + " " + hD + " (m)");
		p2.setBorder (bor);

		Box part2 = Box.createVerticalBox ();

		rdGroupHeight = new ButtonGroup ();
		JPanel p3 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		//~ p3.add (new JWidthLabel (Translator.swap ("MecaDConstraints.chooseStandHeight"), 10));
		p3.add (new JWidthLabel (Translator.swap ("MecaDConstraints.chooseStandHeight"), 5));
		meanHeight = new JRadioButton (Translator.swap ("MecaDConstraints.meanHeight"));
		meanHeight.addActionListener (this);
		rdGroupHeight.add (meanHeight);
		p3.add (meanHeight);
		dominantHeight = new JRadioButton (Translator.swap ("MecaDConstraints.dominantHeight"));
		dominantHeight.addActionListener (this);
		rdGroupHeight.add (dominantHeight);
		p3.add (dominantHeight);
		if (constraints.standHeight.equals ("mean")) {
			rdGroupHeight.setSelected (meanHeight.getModel (), true);
		} else if (constraints.standHeight.equals ("dom")) {
			rdGroupHeight.setSelected (dominantHeight.getModel (), true);
		}
		rdGroupHeightAction ();

		part2.add (p3);
		p2.add (part2);
		part1.add (p2);


		JPanel phf = new JPanel (new FlowLayout (FlowLayout.CENTER));
		bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraints.windAt"));
		phf.setBorder (bor);

		Box part22 = Box.createVerticalBox ();

		rdGroupWindLevel = new ButtonGroup ();
		JPanel ph = new JPanel (new FlowLayout (FlowLayout.LEFT));
		windAtH = new JRadioButton (" " + Translator.swap ("MecaDConstraints.windAtH") + " = ");
		windAtH.addActionListener (this);
		rdGroupWindLevel.add (windAtH);
		ph.add (windAtH);
		windSpeedEdgeAtH = new JTextField (5);
		ph.add (windSpeedEdgeAtH);
		windSpeedEdgeAtH.setText (""+constraints.windSpeedEdgeAtH);
		ph.add (new JWidthLabel (" (m/s)", 10));

		JPanel pf = new JPanel (new FlowLayout (FlowLayout.LEFT));
		windAt10m = new JRadioButton (" " + Translator.swap ("MecaDConstraints.windAt10m") + " = ");
		windAt10m.addActionListener (this);
		rdGroupWindLevel.add (windAt10m);
		pf.add (windAt10m);
		windSpeedEdgeAt10m = new JTextField (5);
		pf.add (windSpeedEdgeAt10m);
		windSpeedEdgeAt10m.setText (""+constraints.windSpeedEdgeAt10m);
		pf.add (new JWidthLabel (" (m/s)", 10));

		if (constraints.windAt10m) {
			rdGroupWindLevel.setSelected (windAt10m.getModel (), true);
		} else {
			rdGroupWindLevel.setSelected (windAtH.getModel (), true);
		}
		rdGroupWindLevelAction ();

		part22.add (ph);
		part22.add (pf);
		phf.add (part22);
		part1.add (phf);


		JPanel p5 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraints.chooseLocation"));
		p5.setBorder (bor);

		Box part3 = Box.createVerticalBox ();

		rdGroupLocation = new ButtonGroup ();
		JPanel p6 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		edge = new JRadioButton (Translator.swap ("MecaDConstraints.edge"));
		edge.addActionListener (this);
		rdGroupLocation.add (edge);
		p6.add (edge);
		p6.add (new JWidthLabel ("", 40));
		stand = new JRadioButton (Translator.swap ("MecaDConstraints.stand"));
		stand.addActionListener (this);
		rdGroupLocation.add (stand);
		p6.add (stand);
		if (constraints.location.equals ("edge")) {
			rdGroupLocation.setSelected (edge.getModel (), true);
		} else if (constraints.location.equals ("stand")) {
			rdGroupLocation.setSelected (stand.getModel (), true);
		}
		rdGroupLocationAction ();
		part3.add (p6);

		JPanel p7 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraints.withinStand"));
		p7.setBorder (bor);

		Box part4 = Box.createVerticalBox ();
		JPanel p9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p9.add (new JWidthLabel (Translator.swap ("MecaDConstraints.logProfileStand"), 10));
		JPanel p10 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p10.add (new JWidthLabel (Translator.swap ("MecaDConstraints.linProfileStand"), 10));
		JPanel p11 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p11.add (new JWidthLabel (Translator.swap ("MecaDConstraints.expProfileStand"), 10));
		JPanel p8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p8.add (new JWidthLabel (Translator.swap ("MecaDConstraints.conProfileStand"), 10));

		part4.add (p9);
		part4.add (p10);
		part4.add (p11);
		part4.add (p8);
		p7.add (part4);
		part3.add (p7);

		JPanel p14 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraints.edgeStand"));
		p14.setBorder (bor);

		Box part5 = Box.createVerticalBox ();

		JPanel p15 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p15.add (new JWidthLabel (Translator.swap ("MecaDConstraints.logProfileEdge"), 10));

		part5.add (p15);
		p14.add (part5);
		part3.add (p14);
		p5.add (part3);
		part1.add (p5);
		p1.add (part1);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		reset = new JButton (Translator.swap ("MecaDConstraints.reset"));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (reset);
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		reset.addActionListener (this);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (p1, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);

		setTitle (Translator.swap ("MecaDConstraints"));
		
		setModal (true);
	}

	public static void main (String[] args) {
		MecaConstraints c = new MecaConstraints ();
		JFrame f = new JFrame ();
		new MecaDConstraints (f, c, 10, 11);
	}

	public MecaDConstraints (JFrame f, MecaConstraints constraints, double hMean, double hDom) {
		super (f);
		this.constraints = constraints;

		NumberFormat nf1 = NumberFormat.getInstance ();
		nf1.setMinimumFractionDigits (1);
		nf1.setMaximumFractionDigits (1);
		nf1.setGroupingUsed (false);
		String hM = "" + nf1.format (hMean);
		String hD = "" + nf1.format (hDom);

		createUI (hM, hD);
		
		setModal (true);
		pack ();
		show ();
	}

}


