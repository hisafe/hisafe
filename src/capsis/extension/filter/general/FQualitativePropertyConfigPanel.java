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
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import javax.swing.BoxLayout;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JToolBar;

import jeeb.lib.util.IconLoader;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.util.Pilotable;
import capsis.util.QualitativeProperty;
import capsis.util.SmartFlowLayout;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration panel for qualitative property filter.
 * 
 * @author F. de Coligny - may 2002
 */
public class FQualitativePropertyConfigPanel extends ConfigurationPanel implements Pilotable, ActionListener {
	
	private FQualitativeProperty mum;	// mummy is being configured
	private Map propertyKey_checkBox;
	private JButton helpButton;
	
	// One PropertyKey per check box: the class of the property and the concerned value
	private class PropertyKey {
		public Class klass;
		public Integer value;
		public PropertyKey (Class klass, Integer value) {
			this.klass = klass;
			this.value = value;
		}
	}


	/**	Rebuild a class_validValues map from the checkBoxes
	*/
	protected Map getClass_validValues () {
		Map map = new Hashtable ();
		
		Iterator keys = propertyKey_checkBox.keySet ().iterator ();
		Iterator values = propertyKey_checkBox.values ().iterator ();
		while (keys.hasNext () && values.hasNext ()) {
			PropertyKey propertyKey = (PropertyKey) keys.next ();
			JCheckBox ck = (JCheckBox) values.next ();

			Class klass = propertyKey.klass;
			Integer value = propertyKey.value;
			boolean selected = ck.isSelected ();
			
			if (selected) {
				Collection c = (Collection) map.get (klass);
				if (c == null) {
					//c = new Vector ();
					c = new HashSet ();	// better performance on contains operation: O(1)
					map.put (klass, c);
				}
				c.add (value);
			}
		}
		//System.out.println ("FQualProp: getClass_validValues (): map="+map);

		return map;
	}
	
	/**	Constructor
	*/
	protected FQualitativePropertyConfigPanel (Configurable c) {
		super (c);
		
		mum = (FQualitativeProperty) c;
		
		propertyKey_checkBox = new Hashtable ();
		
		// Consider first element in Filtrable.
		// Create one JTabbedPane in this.
		// Create one card for each Qualitative Property found in element
		// Each Card has the name of the property and contains
		// one checkbox per possible value
		// User can check all the boxes he wants on each card.
		//~ Object elt = null;
		//~ for (Iterator i = mum.filtrable.iterator (); i.hasNext ();) {
			//~ elt = i.next ();
		//~ }
		Object elt = mum.candidates.iterator ().next ();
		
		// If no element, message "empty..."
		if (elt == null) {
			JPanel l3 = new JPanel (new SmartFlowLayout ());
			JLabel lab3 = new JWidthLabel (Translator.swap ("FQualitativeProperty.emptyList"), 150);
			l3.add (lab3);
			getContentPane ().add (l3);
			getContentPane ().revalidate ();
			//getContentPane ().setBorder (BorderFactory.createTitledBorder (
			//		Translator.swap ("FQualitativeProperty")));
			repaint ();
			return;
		}
		
		// Looking for Property accessors...
		JTabbedPane tabbedPane = new JTabbedPane ();
		Collection acc = Tools.getPublicAccessors (elt.getClass ());	
		
		// Visual: try to select the first panel with something checked
		// fc - 5.9.2003
		//
		JPanel panelToBeSelected = null;	
		
		for (Iterator j = acc.iterator (); j.hasNext ();) {
			Method m = (Method) j.next ();
			
			// Card creation: one per qualitative value
			if (Tools.returnsType (m, QualitativeProperty.class)) {
				// One property accessor found...
				QualitativeProperty p = null;
				try {
					p = (QualitativeProperty) m.invoke (elt);	// fc - 2.12.2004 - varargs
				} catch (Exception e) {
					Log.println (Log.WARNING, "FQualitativeProperty.FQualitativePropertyConfigPanel ()", 
							"Exception during dynamic method invocation (get...())"
							+" to retrieve a Qualitative Property on object "+elt, e);
					//System.out.println ("error while invoking method on object: "+e);
				}
				
				JPanel pan = new JPanel ();
				pan.setLayout (new BoxLayout (pan, BoxLayout.Y_AXIS));
				
				// Exploring possible values and creating checkboxes
				Map desc = p.getValues ();
				
				Iterator keys = desc.keySet ().iterator ();
				Iterator values = desc.values ().iterator ();
				while (keys.hasNext () && values.hasNext ()) {
					Integer key = (Integer) keys.next ();
					String value = (String) values.next ();
					
					//String propertyKey = p.getPropertyName ()+"."+key;
					boolean selected = false;
					try {
						
						//System.out.println ("======> p.getClass ()="+p.getClass ());
						
						Collection validValues = (Collection) mum.class_validValues.get 
								(p.getClass ());
						//System.out.println ("======> validValues  ="+validValues);
						selected = validValues.contains (key);
					} catch (Exception e) {
						selected = false;
					}
						
					//System.out.println ("======> key="+key+", value="+value+", selected="+selected);
					
					JCheckBox ck = new JCheckBox (Translator.swap (value), selected);
					propertyKey_checkBox.put (new PropertyKey (p.getClass (), key), ck);
					pan.add (ck);
					
					if (panelToBeSelected == null && selected) {panelToBeSelected = pan;}	// fc - 5.9.2003
					
				}
				pan.add (new JWidthLabel ("", 150));		// to format cool width...
				tabbedPane.addTab (Translator.swap (p.getPropertyName ()), pan);
			}
		}
		
		if (panelToBeSelected != null) {tabbedPane.setSelectedComponent (panelToBeSelected);}	// fc - 5.9.2003
		
		setLayout (new BorderLayout ());
		add (new JScrollPane (tabbedPane), BorderLayout.CENTER);
		
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
	
}
	
