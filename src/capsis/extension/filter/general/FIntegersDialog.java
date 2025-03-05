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
import capsis.util.EnablableTextField;
import capsis.util.Pilotable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for ints filter.
 * 
 * @author F. de Coligny - september 2004
 */
public class FIntegersDialog extends ConfigurationPanel implements Pilotable, ActionListener {
	
	private FIntegers filter;		// filter under configuration
	private Collection candidates;	// the collection of individuals for configuration time
	private Collection intMethods;	// the intMethods to enable / disable and set
	
	private boolean panelDisabled;
	
	private Map textField2method;
	
	private JButton helpButton;
	

	/**	Constructor
	*/
	protected FIntegersDialog (Configurable c) {
		super (c);
		filter = (FIntegers) c;
		this.candidates = filter.candidates;
		this.intMethods = filter.searchCommonIntMethods (this.candidates);
		textField2method = new HashMap ();
		createUI ();
	}

	/**	Rebuild a methodName2acceptedValue map from the checkBoxes
	*/
	protected Map getMethodName2acceptedValue () {
		Map methodName2acceptedValue = new HashMap ();
		for (Iterator i = textField2method.keySet ().iterator (); i.hasNext ();) {
			EnablableTextField f = (EnablableTextField) i.next ();
			if (f.isEnabled ()) {
				Method m = (Method) textField2method.get (f);
				methodName2acceptedValue.put (m.getName (), new Integer (f.getValue ()));
			}
		}
		return methodName2acceptedValue;
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
	
	/**	Check that all values are ints.
	*/
	public boolean checksAreOk () {
		
		for (Iterator i = textField2method.keySet ().iterator (); i.hasNext ();) {
			EnablableTextField f = (EnablableTextField) i.next ();
			if (f.isEnabled ()) {
				String value = f.getValue ();
				if (Check.isEmpty (value) || !Check.isInt (value)) {
					MessageDialog.print (
							this, Translator.swap ("FIntegersDialog.valueShouldBeAnInteger")
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
		
		JLabel notice = new JLabel (Translator.swap ("FIntegersDialog.enableTheChosenPropertiesAndSetTheirValues"));
		LinePanel l1 = new LinePanel ();
		l1.add (notice);
		l1.addGlue ();
		main.add (l1);
		
		for (Iterator i = intMethods.iterator (); i.hasNext ();) {
			Method m = (Method) i.next ();
			
			boolean enabled = filter.methodName2acceptedValue.keySet ().contains (m.getName ());
			String value = "";
			if (enabled) {
				value = ""+((Integer) filter.methodName2acceptedValue.get (m.getName ())).intValue ();
			}
			
			EnablableTextField f = new EnablableTextField (enabled, value, 
					Tools.setFirstLetterUpperCase (Tools.removeAccessorPrefix (m.getName ())), 
					m.getName ());
			textField2method.put (f, m);
			comps.add (f);
		}
		
		// add components to panel in sorted order
		//
		for (Iterator k = comps.iterator (); k.hasNext ();) {
			EnablableTextField f = (EnablableTextField) k.next ();
			main.add (f);
		}
		
		main.addGlue ();
		
		add (main, BorderLayout.NORTH);
	}
	
	
}
	
