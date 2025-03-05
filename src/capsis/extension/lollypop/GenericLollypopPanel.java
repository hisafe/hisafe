/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.lollypop;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JCheckBox;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.util.ConfigPanel;

/**	GenericLollypopPanel: a configuration panel
*	for GenericLollypop
*
*	@author F. de Coligny - march 2006
*/
public class GenericLollypopPanel extends ConfigPanel implements ActionListener {

	private JCheckBox labelEnabled;
	private JCheckBox labelFrequencyEnabled;
	private JTextField labelFrequency;
	private JRadioButton labelId;
	private JRadioButton labelDbh;

	private ButtonGroup group1;

	private JCheckBox trunkEnabled;
	private JTextField trunkMagnifyFactor;

	private JCheckBox crownEnabled;
	private JRadioButton crownOutline;
	private JRadioButton crownFilled;
	private JRadioButton crownFilledFlat;
	private JRadioButton crownFilledLight;
	private JRadioButton crownFilledTransparent;
	private JTextField crownAlphaValue;	// related to transparency

	private ButtonGroup group2;
	private ButtonGroup group3;

	private Color labelColor;
	private Color trunkColor;
	private Color crownColor;
	private Color selectionColor;

	private GenericLollypopStarter s;


	/**	Constructor.
	*/
	public GenericLollypopPanel (GenericLollypop subject) {
		super (subject);
		s = (GenericLollypopStarter)
				((GenericLollypop) subject).getStarter ();
		createUI ();
	}

	/**	ConfigPanel interface.
	*/
	public boolean everythingCorrect () {
		// 1. controls
		if (!Check.isInt (labelFrequency.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("GenericLollypop.labelFrequencyShouldBeAnInt"));
			return false ;
		}

		if (!Check.isInt (crownAlphaValue.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("GenericLollypop.crownAlphaValueShouldBeAnInt"));
			return false ;
		}
		int av = Check.intValue (crownAlphaValue.getText ().trim ());
		if (av < 0 || av > 255) {
			MessageDialog.print (this, Translator.swap ("GenericLollypop.crownAlphaValueShouldBeAnInt"));
			return false ;
		}

		if (!Check.isDouble (trunkMagnifyFactor.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("GenericLollypop.trunkMagnifyFactorShouldBeADouble"));
			return false ;
		}

		// 2. all controls ok, report new configuration
		s.labelEnabled = labelEnabled.isSelected ();
		s.labelFrequencyEnabled = labelFrequencyEnabled.isSelected ();
		s.labelFrequency = Check.intValue (labelFrequency.getText ().trim ());
		s.labelId = labelId.isSelected ();
		s.labelDbh = labelDbh.isSelected ();

		s.trunkEnabled = trunkEnabled.isSelected ();
		s.trunkMagnifyFactor = Check.doubleValue (trunkMagnifyFactor.getText ().trim ());

		s.crownEnabled = crownEnabled.isSelected ();
		s.crownOutline = crownOutline.isSelected ();
		s.crownFilled = crownFilled.isSelected ();
		s.crownFilledFlat = crownFilledFlat.isSelected ();
		s.crownFilledLight = crownFilledLight.isSelected ();
		s.crownFilledTransparent = crownFilledTransparent.isSelected ();
		s.crownAlphaValue = Check.intValue (crownAlphaValue.getText ().trim ());

		s.labelColor = new Color (51, 0, 102);
		s.trunkColor = Color.BLACK;
		s.crownColor = new Color (0, 102, 0);
		s.cellColor = Color.GRAY;
		s.selectionColor = new Color (207, 74, 7);

		return true;
	}

	/**	Called when something changes in config
	*	(ex: a check box was changed...)
	*	It will notify the Drawer listener.
	*/
	public void actionPerformed (ActionEvent e) {
		synchronizeOptions ();
		super.actionPerformed (e);
	}

	/**	Synchronize the radio buttons / check box
	*/
	private void synchronizeOptions () {
		labelFrequencyEnabled.setEnabled (labelEnabled.isSelected ());
		labelFrequency.setEnabled (labelEnabled.isSelected ());
		labelId.setEnabled (labelEnabled.isSelected ());
		labelDbh.setEnabled (labelEnabled.isSelected ());

		crownFilledFlat.setEnabled (crownFilled.isSelected ());
		crownFilledLight.setEnabled (crownFilled.isSelected ());
		crownFilledTransparent.setEnabled (crownFilled.isSelected ());
		crownAlphaValue.setEnabled (crownFilled.isSelected () && crownFilledTransparent.isSelected ());
	}

	/**	Initializes the GUI.
	*/
	private void createUI () {
		ColumnPanel part1 = new ColumnPanel (0, 0);
		Border etched = BorderFactory.createEtchedBorder ();
		/*Border b = BorderFactory.createTitledBorder (etched, Translator.swap ("GenericLollypop"));
		part1.setBorder (b);*/

		// label
		ColumnPanel p1 = new ColumnPanel (0, 0);
		Border b = BorderFactory.createTitledBorder (etched, Translator.swap ("GenericLollypop.labels"));
		p1.setBorder (b);

		LinePanel l1 = new LinePanel ();
		labelEnabled = new JCheckBox (Translator.swap ("GenericLollypop.labelEnabled"));
		labelEnabled.setSelected (s.labelEnabled);
		labelEnabled.addActionListener (this);
		l1.add (labelEnabled);

		labelId = new JRadioButton (Translator.swap ("GenericLollypop.labelId"));
		labelId.setSelected (s.labelId);
		labelId.addActionListener (this);
		labelDbh = new JRadioButton (Translator.swap ("GenericLollypop.labelDbh"));
		labelDbh.setSelected (s.labelDbh);
		labelDbh.addActionListener (this);
		group1 = new ButtonGroup ();
		group1.add (labelId);
		group1.add (labelDbh);
		l1.add (labelId);
		l1.add (labelDbh);

		l1.addGlue ();
		p1.add (l1);

		LinePanel l2 = new LinePanel ();
		l2.add (new JWidthLabel ("", 10));
		labelFrequencyEnabled = new JCheckBox (Translator.swap ("GenericLollypop.labelFrequencyEnabled"));
		labelFrequencyEnabled.setSelected (s.labelFrequencyEnabled);
		labelFrequencyEnabled.addActionListener (this);
		l2.add (labelFrequencyEnabled);
		labelFrequency = new JTextField ();
		labelFrequency.setText (""+s.labelFrequency);
		labelFrequency.addActionListener (this);
		l2.add (labelFrequency);
		l2.addStrut0 ();
		p1.add (l2);

		part1.add (p1);

		// trunk
		ColumnPanel p3 = new ColumnPanel (0, 0);
		b = BorderFactory.createTitledBorder (etched, Translator.swap ("GenericLollypop.trunks"));
		p3.setBorder (b);

		LinePanel l20 = new LinePanel ();
		trunkEnabled = new JCheckBox (Translator.swap ("GenericLollypop.trunkEnabled"));
		trunkEnabled.setSelected (s.trunkEnabled);
		trunkEnabled.addActionListener (this);
		l20.add (trunkEnabled);
		l20.addGlue ();
		p3.add (l20);

		LinePanel l9 = new LinePanel ();
		l9.add (new JWidthLabel (Translator.swap ("GenericLollypop.trunkMagnifyFactor")+" :", 120));
		trunkMagnifyFactor = new JTextField ();
		trunkMagnifyFactor.setText (""+s.trunkMagnifyFactor);
		trunkMagnifyFactor.addActionListener (this);
		l9.add (trunkMagnifyFactor);
		l9.addStrut0 ();
		p3.add (l9);

		part1.add (p3);

		// crown
		ColumnPanel p2 = new ColumnPanel (0, 0);
		b = BorderFactory.createTitledBorder (etched, Translator.swap ("GenericLollypop.crowns"));
		p2.setBorder (b);

		LinePanel l21 = new LinePanel ();
		crownEnabled = new JCheckBox (Translator.swap ("GenericLollypop.trunkEnabled"));
		crownEnabled.setSelected (s.crownEnabled);
		crownEnabled.addActionListener (this);
		l21.add (crownEnabled);
		l21.addGlue ();
		p2.add (l21);

		LinePanel l4 = new LinePanel ();
		crownOutline = new JRadioButton (Translator.swap ("GenericLollypop.crownOutline"));
		crownOutline.setSelected (s.crownOutline);
		crownOutline.addActionListener (this);
		l4.add (crownOutline);
		l4.addGlue ();
		p2.add (l4);

		LinePanel l5 = new LinePanel ();
		crownFilled = new JRadioButton (Translator.swap ("GenericLollypop.crownFilled"));
		crownFilled.setSelected (s.crownFilled);
		crownFilled.addActionListener (this);
		l5.add (crownFilled);
		l5.addGlue ();
		p2.add (l5);

		group2 = new ButtonGroup ();
		group2.add (crownOutline);
		group2.add (crownFilled);

		LinePanel l7 = new LinePanel ();
		l7.add (new JWidthLabel ("", 10));

		crownFilledFlat = new JRadioButton (Translator.swap ("GenericLollypop.crownFilledFlat"));
		crownFilledFlat.setSelected (s.crownFilledFlat);
		crownFilledFlat.addActionListener (this);
		l7.add (crownFilledFlat);

		crownFilledLight = new JRadioButton (Translator.swap ("GenericLollypop.crownFilledLight"));
		crownFilledLight.setSelected (s.crownFilledLight);
		crownFilledLight.addActionListener (this);
		l7.add (crownFilledLight);
		l7.addGlue ();
		p2.add (l7);

		LinePanel l8 = new LinePanel ();

		l8.add (new JWidthLabel ("", 10));
		crownFilledTransparent = new JRadioButton (Translator.swap ("GenericLollypop.crownFilledTransparent"));
		crownFilledTransparent.setSelected (s.crownFilledTransparent);
		crownFilledTransparent.addActionListener (this);
		l8.add (crownFilledTransparent);

		crownAlphaValue = new JTextField ();
		crownAlphaValue.setText (""+s.crownAlphaValue);
		crownAlphaValue.addActionListener (this);
		l8.add (crownAlphaValue);
		l8.addStrut0 ();
		p2.add (l8);

		group3 = new ButtonGroup ();
		group3.add (crownFilledFlat);
		group3.add (crownFilledLight);
		group3.add (crownFilledTransparent);

		part1.add (p2);


		//~ LinePanel l10 = new LinePanel ();
		//~ l10.add (new JLabel ("add colors here..."));
		//~ l10.addGlue ();
		//~ part1.add (l10);

		setLayout (new BorderLayout ());
		add (part1, BorderLayout.NORTH);

		synchronizeOptions ();

	}

}

