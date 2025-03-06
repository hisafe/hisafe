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

//import capsis.kernel.*;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import capsis.commongui.util.Helper;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * MecaDConstraintsSensi - Interface for constraints data.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDConstraintsSensi extends AmapDialog implements ActionListener {
//checked for c4.1.1_08 - fc - 3.2.2003

	private JTextField windSpeedEdgeAt10m;
	private JButton ok;
	private JButton cancel;
	private JButton help;
	private MecaConstraints constraints;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDConstraintsSensi");
	}

	/**
	 * Constructor.
	 */
	public MecaDConstraintsSensi (AmapDialog parent, MecaConstraints constraints) {
		super (parent);
		this.constraints = constraints;

		createUI ();
		
		setModal (true);
		pack ();
		show ();
	}

	/**
	 * Change settings to collected settings.
	 */
	private void okAction () {
		// 1. checks...

		if (Check.isEmpty (windSpeedEdgeAt10m.getText ()) || !Check.isDouble (windSpeedEdgeAt10m.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDConstraintsSensi.windSpeedIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		// 2. retrieve the collected data and overide settings attributes
		//    (initialy set to default values) with them
		constraints.windSpeedEdgeAt10m = Check.doubleValue (windSpeedEdgeAt10m.getText ());

		setValidDialog (true);
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		Border etched = BorderFactory.createEtchedBorder ();

		// 1. Lines parameters
		JPanel p1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		Border bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraintsSensi.title"));
		p1.setBorder (bor);

		Box part1 = Box.createVerticalBox ();

		JPanel p2 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		p2.add (new JWidthLabel (Translator.swap ("MecaDConstraintsSensi.windAt"), 10));
		JPanel p3 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		p3.add (new JWidthLabel (" " + Translator.swap ("MecaDConstraintsSensi.windAt10m") + " = ", 10));
		windSpeedEdgeAt10m = new JTextField (5);
		p3.add (windSpeedEdgeAt10m);
		windSpeedEdgeAt10m.setText (""+constraints.windSpeedEdgeAt10m);
		p3.add (new JWidthLabel (" (m/s)", 10));

		part1.add (p2);
		part1.add (p3);

		JPanel p7 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		bor = BorderFactory.createTitledBorder (etched, Translator.swap ("MecaDConstraintsSensi.withinStand"));
		p7.setBorder (bor);

		Box part4 = Box.createVerticalBox ();
		JPanel p9 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p9.add (new JWidthLabel (Translator.swap ("MecaDConstraintsSensi.logProfileStand"), 10));
		JPanel p10 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p10.add (new JWidthLabel (Translator.swap ("MecaDConstraintsSensi.linProfileStand"), 10));
		JPanel p11 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p11.add (new JWidthLabel (Translator.swap ("MecaDConstraintsSensi.expProfileStand"), 10));
		JPanel p8 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p8.add (new JWidthLabel (Translator.swap ("MecaDConstraintsSensi.conProfileStand"), 10));

		part4.add (p9);
		part4.add (p10);
		part4.add (p11);
		part4.add (p8);
		p7.add (part4);
		part1.add (p7);
		p1.add (part1);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (p1, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);

		setTitle (Translator.swap ("MecaDConstraintsSensi"));
		
		setModal (true);
	}

	public static void main (String[] args) {
		MecaConstraints c = new MecaConstraints ();
		JFrame f = new JFrame ();
		new MecaDConstraintsSensi (f, c);
	}

	public MecaDConstraintsSensi (JFrame f, MecaConstraints constraints) {
		super (f);
		this.constraints = constraints;

		createUI ();
		
		setModal (true);
		pack ();
		show ();
	}

}


