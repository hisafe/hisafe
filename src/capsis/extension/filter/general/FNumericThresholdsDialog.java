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

package capsis.extension.filter.general;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JToolBar;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.util.EnablableTextFieldCouple;
import capsis.util.Pilotable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for numeric thresholds filter.
 * 
 * @author F. de Coligny - september 2004
 */
public class FNumericThresholdsDialog extends ConfigurationPanel implements Pilotable, ActionListener {
	
	private FNumericThresholds filter;		// filter under configuration
	private Collection candidates;		// the collection of individuals for configuration time
	private Collection numericMethods;	// the numericMethods to enable / disable on which thresholds can be set
	
	private boolean panelDisabled;
	
	private Map textFields2method;
	
	private JButton helpButton;
	

	/**	Constructor
	*/
	protected FNumericThresholdsDialog (Configurable c) {
		super (c);
		filter = (FNumericThresholds) c;
		this.candidates = filter.candidates;
		this.numericMethods = filter.searchCommonNumericMethods (this.candidates);
		textFields2method = new HashMap ();
		createUI ();
	}

	/**	Rebuild a methodName2acceptedValues map from the checkBoxes
	*/
	protected Map getMethodName2acceptedValues () {
		Map methodName2acceptedValues = new HashMap ();
		for (Iterator i = textFields2method.keySet ().iterator (); i.hasNext ();) {
			EnablableTextFieldCouple f = (EnablableTextFieldCouple) i.next ();
			if (f.isEnabled ()) {
				Method m = (Method) textFields2method.get (f);
				
				double[] values = new double[2];
				String v1 = f.getValue1 ();
				String v2 = f.getValue2 ();
				values[0] = Check.isEmpty (v1) ? Double.MIN_VALUE : Check.doubleValue (v1);
				values[1] = Check.isEmpty (v2) ? Double.MAX_VALUE : Check.doubleValue (v2);
				
				methodName2acceptedValues.put (m.getName (), values);
			}
		}
		return methodName2acceptedValues;
	}
	
	/**	From Pilotable interface.
	*/
	public JComponent getPilot () {
		
		
		ImageIcon icon = IconLoader.getIcon ("help_16.png");
		helpButton = new JButton (icon);
		Tools.setSizeExactly (helpButton, 23, 23);
		helpButton.setToolTipText (Translator.swap ("Shared.help"));
		helpButton.addActionListener (this);
		
		JToolBar toolbar = new JToolBar ();
		toolbar.add (helpButton);
		toolbar.setVisible (true);
		
		return toolbar;
	}
	
	/**	Events processing
	*/
	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (helpButton)) {
			Helper.helpFor (this);
		}
	}
	
	/**	Check that all values are numerics.
	*/
	public boolean checksAreOk () {
		
		for (Iterator i = textFields2method.keySet ().iterator (); i.hasNext ();) {
			EnablableTextFieldCouple f = (EnablableTextFieldCouple) i.next ();
			if (f.isEnabled ()) {
				String value1 = f.getValue1 ();
				if (!Check.isEmpty (value1) && !Check.isDouble (value1)) {
					MessageDialog.print (
							this, Translator.swap ("FNumericThresholdsDialog.lowValueShouldBeANumber")
							+" : "+f.getName ());
					return false;
				}
				String value2 = f.getValue2 ();
				if (!Check.isEmpty (value2) && !Check.isDouble (value2)) {
					MessageDialog.print (
							this, Translator.swap ("FNumericThresholdsDialog.highValueShouldBeANumber")
							+" : "+f.getName ());
					return false;
				}
			}
		}
		
		return true;
	}
	
	//	Create the user interface.
	//
	private void createUI () {
		setLayout (new BorderLayout ());
		
		ColumnPanel main = new ColumnPanel (2, 0);
		Set comps = new TreeSet ();	// sorted
		
		JLabel notice = new JLabel (Translator.swap ("FNumericThresholdsDialog.enableTheChosenPropertiesAndSetTheirThresholds"));
		LinePanel l1 = new LinePanel ();
		l1.add (notice);
		l1.addGlue ();
		main.add (l1);
		
		for (Iterator i = numericMethods.iterator (); i.hasNext ();) {
			Method m = (Method) i.next ();
			
			boolean enabled = filter.methodName2acceptedValues.keySet ().contains (m.getName ());
			String value1 = "";
			String value2 = "";
			if (enabled) {
				double[] values = (double[]) filter.methodName2acceptedValues.get (m.getName ());
				value1 = values[0] == Double.MIN_VALUE ? "" : ""+values[0];
				value2 = values[1] == Double.MAX_VALUE ? "" : ""+values[1];
			}
			
			EnablableTextFieldCouple f = new EnablableTextFieldCouple (enabled, 
					Translator.swap ("FNumericThresholdsDialog.between"), value1, 
					Translator.swap ("FNumericThresholdsDialog.and"), value2, 
					Tools.setFirstLetterUpperCase (Tools.removeAccessorPrefix (m.getName ())), 
					m.getName ());
			textFields2method.put (f, m);
			
			comps.add (f);
		}
		
		// add components to panel in sorted order
		//
		for (Iterator k = comps.iterator (); k.hasNext ();) {
			EnablableTextFieldCouple f = (EnablableTextFieldCouple) k.next ();
			main.add (f);
		}
		
		main.addGlue ();
		
		add (main, BorderLayout.NORTH);
	}
	
	
}
	
