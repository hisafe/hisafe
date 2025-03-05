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

package capsis.extension.intervener;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.Locale;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.defaulttype.Tree;

/**
 * This dialog box is used to set DHAThinner parameters in interactive context.
 * 
 * @author F. de Coligny - march 2002
 */
public class DHAThinnerDialog extends AmapDialog implements ActionListener {

	// 24.6.2002 - fc - bug correction (phd) : getMin () and getMax () returned int values, now they
	// return float. DHAThinner version passed to 1.1.

	private NumberFormat nf;

	private DHAThinner thinner;

	private ButtonGroup group1;
	private JRadioButton dbh;
	private JRadioButton height;
	private JRadioButton age;
	private JTextField min;
	private JTextField max;

	protected JButton ok;
	protected JButton cancel;
	protected JButton help;

	
	
	/**	
	 * Constructor
	 */
	public DHAThinnerDialog (DHAThinner thinner) {
		super ();

		this.thinner = thinner;

		// To show numbers in a nice way
		nf = NumberFormat.getInstance (Locale.ENGLISH);
		nf.setGroupingUsed (false);
		nf.setMaximumFractionDigits (3);

		createUI ();
		presetMinMax ();
		
		// fc-31.8.2018
		setTitle (thinner.getName());
//		setTitle (Translator.swap ("DHAThinnerDialog"));

		setModal (true);

		// location is set by the AmapDialog superclass
		pack (); // uses component's preferredSize
		show ();

	}

	/**
	 * Accessor for context.
	 */
	public int getContext () {
		if (dbh.isSelected ()) {
			return DHAThinner.DBH;
		} else if (height.isSelected ()) {
			return DHAThinner.HEIGHT;
		} else {
			return DHAThinner.AGE;
		}
	}

	/**
	 * Accessor for min value.
	 */
	public float getMin () {
		if (min.getText ().trim ().length () == 0) { return 0; }
		return (float) Check.doubleValue (min.getText ().trim ());
	}

	/**
	 * Accessor for max value.
	 */
	public float getMax () {
		if (max.getText ().trim ().length () == 0) { return Float.MAX_VALUE; }
		return (float) Check.doubleValue (max.getText ().trim ());
	}

	/**
	 * Action on ok button.
	 */
	private void okAction () {

		boolean minIsEmpty = Check.isEmpty (min.getText ().trim ());
		boolean maxIsEmpty = Check.isEmpty (max.getText ().trim ());

		// Checks...
		if (minIsEmpty && maxIsEmpty) {
			MessageDialog.print (this, Translator.swap ("DHAThinnerDialog.someValueIsNeeded"));
			return;
		}

		// Age must be an int
		if (age.isSelected ()) {
			if (!minIsEmpty && !Check.isInt (min.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("DHAThinnerDialog.ageMustBeAnInteger"));
				return;
			}
			if (!maxIsEmpty && !Check.isInt (max.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("DHAThinnerDialog.ageMustBeAnInteger"));
				return;
			}
			
		// Dbh and height must be doubles
		} else {
			if (!minIsEmpty && !Check.isDouble (min.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("DHAThinnerDialog.bothValuesMustBeNumbers"));
				return;
			}
			if (!maxIsEmpty && !Check.isDouble (max.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("DHAThinnerDialog.bothValuesMustBeNumbers"));
				return;
			}
			
		}

		// Min must be lower than max
		if (!minIsEmpty && !maxIsEmpty) {
			if (Check.doubleValue (min.getText ().trim ()) > Check.doubleValue (max.getText ().trim ())) {
				MessageDialog.print (this, Translator.swap ("DHAThinnerDialog.minMustBeLowerThanMax"));
				return;
			}
		}

		// All has been checked successfully, set the dialog invisible 
		// and go back to caller (will check for validity and dispose the dialog)
		setValidDialog (true);
	}

	/**
	 * Action on cancel button.
	 */
	private void cancelAction () {
		// Set the dialog invisible 
		// and go back to caller (will check for validity and dispose the dialog)
		setValidDialog (false);
	}

	/**
	 * Someone hit a button.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource () instanceof JRadioButton) {
			presetMinMax ();
		} else if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			cancelAction ();
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}

	/**
	 * Writes the available min max values in the min and max textfields as an information
	 * for the user.
	 */
	private void presetMinMax () {
		if (thinner.concernedTrees == null || thinner.concernedTrees.isEmpty ()) { return; }

		double m = Double.MAX_VALUE;
		double M = -Double.MAX_VALUE;

		double v = 0;
		for (Tree t : thinner.concernedTrees) {
			if (dbh.isSelected ()) {
				v = t.getDbh ();

			} else if (height.isSelected ()) {
				v = t.getHeight ();

			} else if (age.isSelected ()) {
				v = t.getAge ();

			}

			m = Math.min (m, v);
			M = Math.max (M, v);

		}
		min.setText (nf.format (m));
		max.setText (nf.format (M));

	}

	/**
	 * Creates the dialog box user interface.
	 */
	private void createUI () {

		// All lines will be inserted in this main column
		ColumnPanel panel = new ColumnPanel ();

		// Choose the mode: diameter, height OR age
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (Translator.swap ("DHAThinnerDialog.cutTreesWith") + " :"));
		l1.addGlue ();
		panel.add (l1);

		// Radio buttons
		LinePanel l10 = new LinePanel ();
		LinePanel l11 = new LinePanel ();
		LinePanel l12 = new LinePanel ();

		dbh = new JRadioButton (Translator.swap ("DHAThinnerDialog.dbh"));
		dbh.addActionListener (this);
		height = new JRadioButton (Translator.swap ("DHAThinnerDialog.height"));
		height.addActionListener (this);
		age = new JRadioButton (Translator.swap ("DHAThinnerDialog.age"));
		age.addActionListener (this);

		// Add the radio buttons in their group
		group1 = new ButtonGroup ();
		group1.add (dbh);
		group1.add (height);
		group1.add (age);
		// Choose the default selection
		group1.setSelected (dbh.getModel (), true);

		l10.add (dbh);
		l10.addGlue ();
		l11.add (height);
		l11.addGlue ();
		l12.add (age);
		l12.addGlue ();
		panel.add (l10);
		panel.add (l11);
		panel.add (l12);

		// Min and max fields on a single line
		LinePanel l21 = new LinePanel ();
		min = new JTextField (5);
		max = new JTextField (5);
		l21.add (new JLabel (Translator.swap ("DHAThinnerDialog.between") + " : "));
		l21.add (min);
		l21.add (new JLabel (" " + Translator.swap ("DHAThinnerDialog.and") + " : "));
		l21.add (max);
		l21.addStrut0 ();
		panel.add (l21);
		panel.addGlue ();

		// Put the main panel at the top (north) of the user interface
		getContentPane ().setLayout (new BorderLayout ());
		getContentPane ().add (panel, BorderLayout.NORTH);

		// Control panel (Ok Cancel Help)
		LinePanel controlPanel = new LinePanel ();
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		
		controlPanel.addGlue ();  // adding glue first -> the buttons will be right justified
		controlPanel.add (ok);
		controlPanel.add (cancel);
		controlPanel.add (help);
		controlPanel.addStrut0 ();
		
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);
		
		// Put the control panel at the bottom (south)
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);

		// Set Ok as default (see AmapDialog)
		setDefaultButton (ok);

	}

}
