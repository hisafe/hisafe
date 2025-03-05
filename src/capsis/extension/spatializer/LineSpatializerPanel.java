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

package capsis.extension.spatializer;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.InstantPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**	LineSpatializerPanel: a configuration panel
*	for LineSpatializer
*
*	@author F. de Coligny - july 2006
*/
public class LineSpatializerPanel extends InstantPanel {

	private JTextField x0Border;
	private JTextField x1Border;
	private JTextField y0Border;
	private JTextField y1Border;

	private JRadioButton interXEnabled;
	private JTextField interX;

	private JRadioButton interYEnabled;
	private JTextField interY;

	private ButtonGroup group1;
	
	private LineSpatializerStarter s;


	/**	Constructor.
	*/
	public LineSpatializerPanel (LineSpatializer subject) {
		super (subject);
		s = (LineSpatializerStarter)
				((LineSpatializer) subject).getStarter ();
		createUI ();
	}

	/**	ConfigPanel interface.
	*/
	public boolean isCorrect () {
		// 1. controls
		if (!Check.isDouble (x0Border.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("LineSpatializerPanel.x0BorderShouldBeADouble"));
			return false ;
		}

		if (!Check.isDouble (x1Border.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("LineSpatializerPanel.x1BorderShouldBeADouble"));
			return false ;
		}

		if (!Check.isDouble (y0Border.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("LineSpatializerPanel.y0BorderShouldBeADouble"));
			return false ;
		}

		if (!Check.isDouble (y1Border.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("LineSpatializerPanel.y1BorderShouldBeADouble"));
			return false ;
		}

		if (interXEnabled.isSelected ()) {
			if (!Check.isDouble (interX.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("LineSpatializerPanel.interXShouldBeADouble"));
				return false ;
			}
		}

		if (interYEnabled.isSelected ()) {
			if (!Check.isDouble (interY.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("LineSpatializerPanel.interYShouldBeADouble"));
				return false ;
			}
		}

		// 2. all controls ok, report new configuration
		s.x0Border = Check.intValue (x0Border.getText ().trim ());
		s.x1Border = Check.intValue (x1Border.getText ().trim ());
		s.y0Border = Check.intValue (y0Border.getText ().trim ());
		s.y1Border = Check.intValue (y1Border.getText ().trim ());

		s.interXEnabled = interXEnabled.isSelected ();
		s.interX = Check.doubleValue (interX.getText ().trim ());
		
		s.interYEnabled = interYEnabled.isSelected ();
		s.interY = Check.doubleValue (interY.getText ().trim ());
		
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
		interX.setEnabled (interXEnabled.isSelected ());
		interY.setEnabled (interYEnabled.isSelected ());
	}

	/**	Initializes the GUI.
	*/
	private void createUI () {
		ColumnPanel part1 = new ColumnPanel (0, 0);
		Border etched = BorderFactory.createEtchedBorder ();
		/*Border b = BorderFactory.createTitledBorder (etched, Translator.swap ("LineSpatializer"));
		part1.setBorder (b);*/
		
		// label
		ColumnPanel p1 = new ColumnPanel (0, 0);
		Border b = BorderFactory.createTitledBorder (etched, Translator.swap ("LineSpatializer.borders"));
		p1.setBorder (b);

		LinePanel l2 = new LinePanel ();
		l2.add (new JWidthLabel (Translator.swap ("LineSpatializer.x0Border")+" :", 70));
		x0Border = new JTextField ();
		x0Border.setText (""+s.x0Border);
		x0Border.addActionListener (this);
		l2.add (x0Border);
		l2.addStrut0 ();
		p1.add (l2);

		LinePanel l3 = new LinePanel ();
		l3.add (new JWidthLabel (Translator.swap ("LineSpatializer.x1Border")+" :", 70));
		x1Border = new JTextField ();
		x1Border.setText (""+s.x1Border);
		x1Border.addActionListener (this);
		l3.add (x1Border);
		l3.addStrut0 ();
		p1.add (l3);

		LinePanel l4 = new LinePanel ();
		l4.add (new JWidthLabel (Translator.swap ("LineSpatializer.y0Border")+" :", 70));
		y0Border = new JTextField ();
		y0Border.setText (""+s.y0Border);
		y0Border.addActionListener (this);
		l4.add (y0Border);
		l4.addStrut0 ();
		p1.add (l4);

		LinePanel l5 = new LinePanel ();
		l5.add (new JWidthLabel (Translator.swap ("LineSpatializer.y1Border")+" :", 70));
		y1Border = new JTextField ();
		y1Border.setText (""+s.y1Border);
		y1Border.addActionListener (this);
		l5.add (y1Border);
		l5.addStrut0 ();
		p1.add (l5);

		part1.add (p1);


		ColumnPanel p2 = new ColumnPanel (0, 0);
		b = BorderFactory.createTitledBorder (etched, Translator.swap ("LineSpatializer.intervals"));
		p2.setBorder (b);

		LinePanel l10 = new LinePanel ();
		interXEnabled = new JRadioButton (Translator.swap ("LineSpatializer.interXEnabled"));
		interXEnabled.addActionListener (this);
		l10.add (interXEnabled);
		l10.add (new JWidthLabel (Translator.swap ("LineSpatializer.interX")+" :", 70));
		interX = new JTextField ();
		interX.setText (""+s.interX);
		interX.addActionListener (this);
		l10.add (interX);
		l10.addStrut0 ();
		p2.add (l10);

		LinePanel l11 = new LinePanel ();
		interYEnabled = new JRadioButton (Translator.swap ("LineSpatializer.interYEnabled"));
		interYEnabled.addActionListener (this);
		l11.add (interYEnabled);
		l11.add (new JWidthLabel (Translator.swap ("LineSpatializer.interY")+" :", 70));
		interY = new JTextField ();
		interY.setText (""+s.interY);
		interY.addActionListener (this);
		l11.add (interY);
		l11.addStrut0 ();
		p2.add (l11);

		group1 = new ButtonGroup ();
		group1.add (interXEnabled);
		group1.add (interYEnabled);

		interXEnabled.setSelected (s.interXEnabled);
		interYEnabled.setSelected (s.interYEnabled);

		part1.add (p2);


		setLayout (new BorderLayout ());
		add (part1, BorderLayout.NORTH);

		synchronizeOptions ();

	}

}



