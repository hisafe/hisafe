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
 * Configuration Panel for DRTable.
 * 
 * @author F. de Coligny - may 2011
 */
public class DRTableConfigurationPanel extends DRConfigurationPanel /* implements ActionListener */ {

	private DRTable drtable;

	private JTextField nDecimalDigits;
	
	/**
	 * Constructor
	 */
	public DRTableConfigurationPanel (Configurable obj) {
		super (obj);

		// The object being configured
		drtable = (DRTable) getConfigurable ();

		int n = Settings.getProperty ("drtable.nDecimalDigits", 2);

		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel (Translator.swap ("DRTable.nDecimalDigits")+" : ", 100));
		nDecimalDigits = new JTextField ();
		nDecimalDigits.setText ("" + n);
		l1.add (nDecimalDigits);
		l1.addGlue ();

		// Layout
		ColumnPanel master = new ColumnPanel ();
		master.add (l1);

		mainContent.add (master); // fc-25.11.2014


	}

//	public void actionPerformed (ActionEvent e) {
//		synchroniseOptions ();
//	}

//	public void synchroniseOptions () {
//		forcedXMin.setEnabled (forcedEnabled.isSelected ());
//	}

	public boolean checksAreOk () {

		if (!Check.isInt (nDecimalDigits.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("DRTable.nDecimalDigitsMustBeAPositiveInteger"));
			return false;
		}
		int n = getNDecimalDigits ();
		if (n < 1) {
			MessageDialog.print (this, Translator.swap ("DRTable.nDecimalDigitsMustBeAPositiveInteger"));
			return false;
		}

		// Memorize the current value
		Settings.setProperty ("drtable.nDecimalDigits", n);
		
		return true; // everything's ok
	}

	public int getNDecimalDigits () {
		// Was checked in checksAreOk ()
		return Check.intValue (nDecimalDigits.getText ().trim ());
	}

}
