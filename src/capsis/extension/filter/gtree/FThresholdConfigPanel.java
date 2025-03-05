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
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Locale;
import java.util.Map;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComboBox;
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
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.util.Pilotable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for FThreshold.
 * 
 * @author F. de Coligny - may 2002
 */
public class FThresholdConfigPanel extends ConfigurationPanel implements ActionListener, Pilotable {
	private static final int BUTTON_SIZE = 23;
	
	private FThreshold filter;		// filter under configuration
	
	private JButton helpButton;
	
	protected JComboBox mode;
	protected JTextField lowValue;
	protected JTextField highValue;
	protected Map humanKey_mode;

	private JButton availableValues;
	
	private NumberFormat formater;
	private NumberFormat nf2;
	
	
	/**	Constructor
	*/
	protected FThresholdConfigPanel (Configurable c) {
		super (c);
		
		filter = (FThreshold) c;
		
		setLayout (new BorderLayout ());
		
		formater = NumberFormat.getInstance (Locale.ENGLISH);
		formater.setGroupingUsed (false);
		if (filter.mode == FThreshold.AGE) {
			formater.setMaximumFractionDigits (0);
		}
		
		nf2 = NumberFormat.getInstance (Locale.ENGLISH);
		nf2.setGroupingUsed (false);
		nf2.setMaximumFractionDigits (3);
		
		ColumnPanel master = new ColumnPanel ();
		
		humanKey_mode = new Hashtable ();
		humanKey_mode.put (Translator.swap ("FThreshold.dbh"), new Integer (FThreshold.DBH));
		humanKey_mode.put (Translator.swap ("FThreshold.height"), new Integer (FThreshold.HEIGHT));
		humanKey_mode.put (Translator.swap ("FThreshold.age"), new Integer (FThreshold.AGE));
		
		Vector v = new Vector ();
		for (Iterator i = new TreeSet (humanKey_mode.keySet ()).iterator (); i.hasNext ();) {
			String s = (String) i.next ();
			v.add (s);
		}
		
		// line 0
		LinePanel l0 = new LinePanel ();
		JLabel lab1 = new JLabel (Translator.swap ("FThreshold.selectTrees"));
		l0.add (lab1);
		l0.addGlue ();
		master.add (l0);
		
		// line 1
		LinePanel l1 = new LinePanel ();
		JLabel lab20 = new JWidthLabel (Translator.swap ("FThreshold.whose")+" :", 150);
		mode = new JComboBox (v);
		mode.addActionListener (this);
		
		for (int i = 0; i< mode.getItemCount (); i++) {
			String k = (String) mode.getItemAt (i);
			if (humanKey_mode.get (k).equals (new Integer ((filter.mode)))) {
				mode.setSelectedItem (k);
				break;
			}
		}
		
		l1.add (lab20);
		l1.add (mode);
		l1.addStrut0 ();
		master.add (l1);
		
		// line 2
		LinePanel l2 = new LinePanel ();
		JLabel lab2 = new JWidthLabel (Translator.swap ("FThreshold.between")+" :", 150);
		lowValue = new JTextField (5);
		if (filter.lowValue == Double.MIN_VALUE 
				|| (filter.lowValue == 0 && filter.highValue == 0)) {
			lowValue.setText ("");
		} else {
			lowValue.setText (formater.format (filter.lowValue));
		}
		JLabel lab22 = new JWidthLabel (Translator.swap ("FThreshold.included"), 100);
		l2.add (lab2);
		l2.add (lowValue);
		l2.add (lab22);
		l2.addStrut0 ();
		master.add (l2);
		
		// line 3
		LinePanel l3 = new LinePanel ();
		JLabel lab3 = new JWidthLabel (Translator.swap ("FThreshold.and")+" :", 150);
		highValue = new JTextField (5);
		if (filter.highValue == Double.MAX_VALUE 
				|| (filter.lowValue == 0 && filter.highValue == 0)) {
			highValue.setText ("");
		} else {
			highValue.setText (formater.format (filter.highValue));
		}
		JLabel lab23 = new JWidthLabel (Translator.swap ("FThreshold.excluded"), 100);
		l3.add (lab3);
		l3.add (highValue);
		l3.add (lab23);
		l3.addStrut0 ();
		master.add (l3);
		
		LinePanel l4 = new LinePanel ();
		availableValues = new JButton (Translator.swap("FThreshold.availableValues"));
		availableValues.addActionListener(this);
		l4.addGlue ();
		l4.add(availableValues);
		l4.addStrut0();
		master.add (l4);
		
		master.addGlue ();
		
		add (master, BorderLayout.NORTH);
		
//		presetMinMax (); // NO: prevents groups customization (changes the previous values)
		
	}

	/**	Events processing
	*/
	public void actionPerformed (ActionEvent e) {
		if (e.getSource () instanceof JComboBox) {
//			presetMinMax (); // NO: prevents groups customization (changes the previous values)
			
		} else if (e.getSource ().equals (availableValues)) {
			presetMinMax(); // Only on demand
			
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
		int t = 0;
		try {
			String s = (String) mode.getSelectedItem ();
			t =  ((Integer) humanKey_mode.get (s)).intValue ();
		} catch (Exception e) {}
		
		// fc-23.12.2020
		String name = filter.getName();
//		String name = ExtensionManager.getName(filter);
		
		// age (int needed)
		if (t == FThreshold.AGE) {
			
			if (!Check.isEmpty (lowValue.getText ()) && !Check.isInt (lowValue.getText ())) {
				JOptionPane.showMessageDialog (this, 
					Translator.swap ("FThreshold.ageLowValueMustBeInteger"),
					name,
					JOptionPane.WARNING_MESSAGE );
				return false;
			}
			
			if (!Check.isEmpty (highValue.getText ()) && !Check.isInt (highValue.getText ())) {
				JOptionPane.showMessageDialog (this, 
					Translator.swap ("FThreshold.ageHighValueMustBeInteger"),
					name, 
					JOptionPane.WARNING_MESSAGE );
				return false;
			}
			
			if (!Check.isEmpty (lowValue.getText ()) && !Check.isEmpty (highValue.getText ())) {
				if (Check.intValue (lowValue.getText ()) >= Check.intValue (highValue.getText ())) {
					JOptionPane.showMessageDialog (this, 
						Translator.swap ("FThreshold.lowMustBeLessThanHigh"),
						name, 
						JOptionPane.WARNING_MESSAGE );
					return false;
				}
			}
			
		// Diameter or Height (double needed)
		} else {
			
			if (!Check.isEmpty (lowValue.getText ()) && !Check.isDouble (lowValue.getText ())) {
				JOptionPane.showMessageDialog (this, 
					Translator.swap ("FThreshold.LowValueMustBeDouble"),
					name, 
					JOptionPane.WARNING_MESSAGE );
				return false;
			}
			
			if (!Check.isEmpty (highValue.getText ()) && !Check.isDouble (highValue.getText ())) {
				JOptionPane.showMessageDialog (this, 
					Translator.swap ("FThreshold.HighValueMustBeDouble"),
					name, 
					JOptionPane.WARNING_MESSAGE );
				return false;
			}
			
			if (!Check.isEmpty (lowValue.getText ()) && !Check.isEmpty (highValue.getText ())) {
				if (Check.doubleValue (lowValue.getText ()) >= Check.doubleValue (highValue.getText ())) {
					JOptionPane.showMessageDialog (this, 
						Translator.swap ("FThreshold.lowMustBeLessThanHigh"),
						name, 
						JOptionPane.WARNING_MESSAGE );
					return false;
				}
			}
		}
		
		return true;
	}	// no possible error on a check box

	
	/**	Write the accurate min and max values as information
	*/
	private void presetMinMax () {
		// Early call, ignore
		if (lowValue == null || highValue == null) {return;}
		
		double m = Double.MAX_VALUE;
		double M = -Double.MAX_VALUE;
		
		double v = 0;
		for (Object o : filter.candidates) {
			if (!(o instanceof Tree)) {continue;}
			Tree t = (Tree) o;
			
			if (mode.getSelectedItem() .equals (Translator.swap ("FThreshold.height"))) {
				v = t.getHeight ();
				
			} else if (mode.getSelectedItem() .equals (Translator.swap ("FThreshold.dbh"))) {
				v = t.getDbh ();
				
			} else {  // Translator.swap ("FThreshold.age")
				v = t.getAge ();
				
			}
			
			m = Math.min (m, v);
			M = Math.max (M, v);
		}
		
		lowValue.setText (nf2.format (m));
		highValue.setText (nf2.format (M));
		
	}
	
	
	/**	Return lowValue
	*/
	protected double getLowValue () {
		String low = lowValue.getText ().trim ();
		if (low.length () == 0) {
			return Double.MIN_VALUE;
		} else {
			return Check.doubleValue (low);
		}
	}

	
	/**	Return highValue
	*/
	protected double getHighValue () {
		String high = highValue.getText ().trim ();
		if (high.length () == 0) {
			return Double.MAX_VALUE;
		} else {
			return Check.doubleValue (high);
		}
	}

	
	/**	Retrieve mode
	*/
	protected int getMode () {
		int r = 0;
		try {
			String s = (String) mode.getSelectedItem ();
			r =  ((Integer) humanKey_mode.get (s)).intValue ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "FThresholdConfigPanel.getMode ()", "Exception caught: ", e);
		}	// see checksAreOk ()
		return r;
	}
	
	
}
	
