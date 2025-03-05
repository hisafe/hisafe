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

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.util.EnablableCheckBox;
import capsis.util.Pilotable;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for booleans filter.
 * 
 * @author F. de Coligny - september 2004
 */
public class FBooleansDialog extends ConfigurationPanel implements Pilotable, ActionListener {
	
	private FBooleans filter;		// filter under configuration
	private Collection candidates;	// the collection of individuals for configuration time
	private Collection booleanMethods;	// the booleanMethods to enable / disable and check
	
	private boolean panelDisabled;
	
	private Map checkBox2method;
	
	private JButton helpButton;
	

	/**	Constructor
	*/
	protected FBooleansDialog (Configurable c) {
		super (c);
		filter = (FBooleans) c;
		this.candidates = filter.candidates;
		this.booleanMethods = filter.searchCommonBooleanMethods (this.candidates);
		checkBox2method = new HashMap ();
		createUI ();
	}

	/**	Rebuild a methodName2acceptedValue map from the checkBoxes
	*/
	protected Map getMethodName2acceptedValue () {
		Map methodName2acceptedValue = new HashMap ();
		for (Iterator i = checkBox2method.keySet ().iterator (); i.hasNext ();) {
			EnablableCheckBox ck = (EnablableCheckBox) i.next ();
			if (ck.isEnabled ()) {
				Method m = (Method) checkBox2method.get (ck);
				methodName2acceptedValue.put (m.getName (), new Boolean (ck.isChecked ()));
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
		
	public boolean checksAreOk () {return true;}	// no possible error on check boxes
	
	private void createUI () {
		setLayout (new BorderLayout ());
		
		ColumnPanel main = new ColumnPanel (2, 0);
		Set comps = new TreeSet ();	// sorted
		
		JLabel notice = new JLabel (Translator.swap ("FBooleans.enableTheChosenPropertiesAndSelectTrueOrFalse"));
		LinePanel l1 = new LinePanel ();
		l1.add (notice);
		l1.addGlue ();
		main.add (l1);
		
		for (Iterator i = booleanMethods.iterator (); i.hasNext ();) {
			Method m = (Method) i.next ();
			
			boolean enabled = filter.methodName2acceptedValue.keySet ().contains (m.getName ());
			boolean checked = false;
			if (enabled) {
				checked = ((Boolean) filter.methodName2acceptedValue.get (m.getName ())).booleanValue ();
			}
			
			EnablableCheckBox ck = new EnablableCheckBox (enabled, checked, 
					Tools.setFirstLetterUpperCase (Tools.removeAccessorPrefix (m.getName ())));
			checkBox2method.put (ck, m);
			
			comps.add (ck);
		}
		
		// add components to panel in sorted order
		//
		for (Iterator k = comps.iterator (); k.hasNext ();) {
			EnablableCheckBox ck = (EnablableCheckBox) k.next ();
			main.add (ck);
		}
		
		main.addGlue ();
		
		add (main, BorderLayout.NORTH);
	}
	
	
}
	
