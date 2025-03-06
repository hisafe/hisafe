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

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Translator;

/**
 * MecaDSettings - Interface for trees biomechanics data.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDSettings extends AmapDialog implements ActionListener {
//checked for c4.1.1_08 - fc - 3.2.2003

	private JTextField woodDensity;
	private JTextField youngModulus;
	//private JTextField crownDensity;
	private JTextField crownStemRatio;
	private JTextField crownDragCoefficient;
	//private JTextField cdcVariation;

	private JButton reset;
	private JButton ok;
	private JButton cancel;

	private MecaSettings settings;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDSettings");
	}


	/**
	 * Constructor.
	 */
	public MecaDSettings (AmapDialog parent, MecaSettings settings) {
		super (parent);
		this.settings = settings;

		createUI ();
		// location is set by AmapDialog
		
		setModal (true);
		pack ();
		show ();
	}

	/**
	 * Reset settings to default settings.
	 */
	private void resetAction () {
		woodDensity.setText (""+settings.WOOD_DENSITY);
		youngModulus.setText (""+settings.YOUNG_MODULUS);
		//crownDensity.setText (""+settings.CROWN_DENSITY);
		crownStemRatio.setText (""+settings.CROWN_STEM_RATIO);
		crownDragCoefficient.setText (""+settings.CROWN_DRAG_COEFFICIENT);
		//cdcVariation.setText (""+settings.CDC_VARIATION);
	}

	/**
	 * Change settings to collected settings.
	 */
	private void okAction () {
		// 1. Checks...
		if (Check.isEmpty (woodDensity.getText ()) || !Check.isDouble (woodDensity.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDSettings.woodDensityIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		if (Check.isEmpty (youngModulus.getText ()) || !Check.isDouble (youngModulus.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDSettings.youngModulusIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		/*if (Check.isEmpty (crownDensity.getText ()) || !Check.isDouble (crownDensity.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDSettings.crownDensityIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}*/

		if (Check.isEmpty (crownStemRatio.getText ()) || !Check.isDouble (crownStemRatio.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDSettings.crownStemRatioIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		if (Check.isEmpty (crownDragCoefficient.getText ()) || !Check.isDouble (crownDragCoefficient.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDSettings.crownDragCoefficientIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		/*if (Check.isEmpty (cdcVariation.getText ()) || !Check.isDouble (cdcVariation.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("MecaDSettings.cdcVariationIsNotANumber"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}*/

		// 2. retrieve the collected data and overide settings attributes
		//    (initialy set to default values) with them
		settings.woodDensity = Check.doubleValue (woodDensity.getText ());
		settings.youngModulus = Check.doubleValue (youngModulus.getText ());
		//settings.crownDensity = Check.doubleValue (crownDensity.getText ());
		settings.crownStemRatio = Check.doubleValue (crownStemRatio.getText ());
		settings.crownDragCoefficient = Check.doubleValue (crownDragCoefficient.getText ());
		//settings.cdcVariation = Check.doubleValue (cdcVariation.getText ());

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
		}
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		Box part1 = Box.createVerticalBox ();

		// 1. Lines parameters
		JPanel l1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		woodDensity = new JTextField (5);
		l1.add (new JWidthLabel (Translator.swap ("MecaDSettings.woodDensity")+" :", 230));
		l1.add (woodDensity);

		JPanel l2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		youngModulus = new JTextField (5);
		l2.add (new JWidthLabel (Translator.swap ("MecaDSettings.youngModulus")+" :", 230));
		l2.add (youngModulus);

		/*JPanel l3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		crownDensity = new JTextField (5);
		crownDensity.setEnabled (false);
		l3.add (new JWidthLabel (Translator.swap ("MecaDSettings.crownDensity")+" :", 230));
		l3.add (crownDensity);*/

		JPanel l4 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		crownStemRatio = new JTextField (5);
		l4.add (new JWidthLabel (Translator.swap ("MecaDSettings.crownStemRatio")+" :", 230));
		l4.add (crownStemRatio);

		JPanel l5 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		crownDragCoefficient = new JTextField (5);
		l5.add (new JWidthLabel (Translator.swap ("MecaDSettings.crownDragCoefficient")+" :", 230));
		l5.add (crownDragCoefficient);
		/*cdcVariation = new JTextField (5);
		l5.add (new JWidthLabel (" +/- :", 10));
		l5.add (cdcVariation);*/

		woodDensity.setText (""+settings.woodDensity);
		youngModulus.setText (""+settings.youngModulus);
		//crownDensity.setText (""+settings.crownDensity);
		crownStemRatio.setText (""+settings.crownStemRatio);
		crownDragCoefficient.setText (""+settings.crownDragCoefficient);
		//cdcVariation.setText (""+settings.cdcVariation);

		part1.add (l1);
		part1.add (l2);
		//part1.add (l3);
		part1.add (l4);
		part1.add (l5);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		reset = new JButton (Translator.swap ("MecaDSettings.reset"));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		pControl.add (reset);
		pControl.add (ok);
		pControl.add (cancel);
		reset.addActionListener (this);
		ok.addActionListener (this);
		cancel.addActionListener (this);

		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (part1, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);

		setTitle (Translator.swap ("MecaDSettings"));
		
		setModal (true);
	}

}


