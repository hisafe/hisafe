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

package capsis.extension.filter.gtree;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.text.NumberFormat;
import java.util.Locale;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JTextField;
import javax.swing.JToolBar;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.util.Pilotable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for FBiggestTrees.
 * 
 * @author F. de Coligny - may 2002
 */
public class FBiggestTreesDialog extends ConfigurationPanel 
		implements ActionListener, Pilotable {
	private static final int BUTTON_SIZE = 23;
	
	private FBiggestTrees filter;		// this filter is being configured
	protected JTextField number;
	private JButton availableValues;
	
	private JButton helpButton;
	private NumberFormat formater;
	
	
	/**	Constructor
	*/
	protected FBiggestTreesDialog (Configurable c) {
		super (c);
		
		filter = (FBiggestTrees) c;
		
		setLayout (new BorderLayout ());
		
		formater = NumberFormat.getInstance (Locale.ENGLISH);
		formater.setGroupingUsed (false);
		formater.setMaximumFractionDigits (0);
		
		ColumnPanel master = new ColumnPanel ();
		
		// line 0
		LinePanel l0 = new LinePanel ();
		JLabel lab1 = new JLabel (Translator.swap ("FBiggestTrees.selectTheBiggestTreesAccordingToDbh"));
		l0.add (lab1);
		l0.addGlue ();
		
		master.add (l0);
		
		// line 1
		LinePanel l1 = new LinePanel ();
		JLabel lab2 = new JWidthLabel (Translator.swap ("FBiggestTrees.numberOfTrees")+" :", 150);
		number = new JTextField (5);
		number.setText (""+filter.number);
		l1.add (lab2);
		l1.add (number);
		l1.addStrut0 ();
		master.add (l1);
		
		LinePanel l2 = new LinePanel ();
		availableValues = new JButton (Translator.swap("FBiggestTrees.availableValues"));
		availableValues.addActionListener (this);
		l2.addGlue ();
		l2.add(availableValues);
		l2.addStrut0();
		master.add(l2);
		
		master.addGlue ();
		
		add (master, BorderLayout.NORTH);
		
//		preset (); // NO: would prevent the filter to be customized, only on demand, see below
	}

	/**	Events processing
	*/
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (availableValues)) {
			preset (); // Only on demand
		} else if (e.getSource ().equals (helpButton)) {
			Helper.helpFor (this);
		}
	}

	/**	From Pilotable interface
	*/
	public JComponent getPilot () {
		
		ImageIcon icon = IconLoader.getIcon ("help_16.png");
		helpButton = new JButton (icon);
		Tools.setSizeExactly (helpButton, BUTTON_SIZE, BUTTON_SIZE);
		helpButton.setToolTipText (Translator.swap ("Shared.help"));
		helpButton.addActionListener (this);
		
		JToolBar toolbar = new JToolBar ();
		toolbar.add (helpButton);
		toolbar.setVisible (true);
		
		return toolbar;
	}
	
	/**	From ConfigurationPanel
	*/
	public boolean checksAreOk () {
		
		String name = ExtensionManager.getName(filter.getClass().getName());
		
		if (Check.isEmpty (number.getText ()) || !Check.isInt (number.getText ())) {
			JOptionPane.showMessageDialog (this, 
				Translator.swap ("FBiggestTrees.numberMustBeAnInteger"),
				name ,
				JOptionPane.WARNING_MESSAGE );
			return false;
		}
		
		int n = Check.intValue (number.getText ());
		if (n < 1) {
			JOptionPane.showMessageDialog (this, 
				Translator.swap ("FBiggestTrees.numberMustBeGreaterThanOne"),
				name,
				JOptionPane.WARNING_MESSAGE );
			return false;
		}
		
		return true;
	}

	
	/**	Write the total number of trees as information
	*/
	private void preset () {
		
		int n = filter.candidates.size ();
		number.setText (""+n);
		
	}
	
	/**	Return number of biggest trees
	*/
	protected int getNumber () {
		return Check.intValue (number.getText ());
	}

	
}
	
