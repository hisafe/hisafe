/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.gui;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Configuration panel for Status choice (Ex: alive / cut / dead / windfall...).
 *
 * @author F. de Coligny - march 2004
 */
public class StatusChooser extends JPanel implements ActionListener {
	
	private Set<String> keys;				// entries available in statusMap
	private Map<JCheckBox, String> checkBoxToLabel;
	private Set<String> selection;			// selected keys
	private boolean locked;			// if single key or trouble, disable chooser
	
	/**	Constructor. */
	public StatusChooser (Set<String> statusKeys, String[] selection) {
		super ();
		
		if (statusKeys == null || statusKeys.isEmpty ()) {
			Log.println (Log.ERROR, "StatusChooser.c ()", 
					"Wrong parameters: statusKeys=" + statusKeys);
			createSecurityUI ();
			return;
		}
		
		keys = statusKeys;
		locked = false;
		
		this.selection = new HashSet<String> ();
		if (keys.size () == 1) {	// single key => select it
			this.selection.add (keys.iterator ().next ());
			locked = true;
			
		} else if (selection == null) {		// try to select "alive" (always present)
			this.selection.add ("alive");
			
		} else {		// more than 1 selection: memorize them
			for (int i = 0; i < selection.length; i++) {
				if (!keys.contains (selection[i])) {	// if one key is unknown, select only "alive"
					this.selection.clear ();
					this.selection.add ("alive");
					break;
				}
				this.selection.add (selection[i]);
			}
		}
		
		checkBoxToLabel = new HashMap<JCheckBox, String> ();
		createUI ();
	}
	
	
	/**	Return the array of status keys which are currently selected.
	*	Example: ["alive" "windfall"].
	*/
	public String[] getSelection () {
		Collection<String> selectedKeys = new ArrayList<String> ();
		for (JCheckBox c : checkBoxToLabel.keySet ()) {
			 
			if (c.isSelected ()) {
				String key = (String) checkBoxToLabel.get (c);
				selectedKeys.add (key);
			}
		}
		if (selectedKeys.size () == 0) {return null;}
		
		String[] result = new String[selectedKeys.size ()];
		int k = 0;
		for (String s : selectedKeys) {
			result[k++] = s;
		}
		return result;
	}
	
	
	/**	Check if status chooser is valid : at least one status must be checked.	*/
	public boolean isChooserValid () {
		for (JCheckBox cb : checkBoxToLabel.keySet ()) {
				if (cb.isSelected ()) {return true;}
		}
		return false;
	}


	/**	Actions on components.
	*/
	public void actionPerformed (ActionEvent evt) {}
	
	
	//	Create a security user interface in case of trouble.
	//
	private void createSecurityUI () {
		setLayout (new BorderLayout ());
		
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (Translator.swap ("Shared.status")));
		l1.add (new JLabel (Translator.swap ("Shared.error")));
		l1.addGlue ();
		add (l1, BorderLayout.NORTH);
	}
	
	
	//	Create the user interface.
	//
	private void createUI () {
		
		setLayout (new BorderLayout ());
		
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (Translator.swap ("Shared.status")));
		
		Iterator<String> k = keys.iterator ();
		while (k.hasNext ()) {
			String label = k.next ();
			
			JCheckBox c = new JCheckBox (Translator.swap (label), selection.contains (label));
			c.setEnabled (!locked);
			checkBoxToLabel.put (c, label);
			l1.add (c);
		}
		
		add (l1, BorderLayout.NORTH);
	}
	
	
	//--------------------------------------------- static methods
	//
	
	/**	Given a statusMap and an array of keys, return a name (ex: "(cut+windfall)").
	*	If statusMap and keys are not compatible, return "" (for "alive").
	*/
	static public String getName (String[] selection) {
		
		// selection == null => ""
		if (selection == null) {
			return "";
			
		// Only "alive" -> do not mention it
		} else if (selection.length == 1 && selection[0].equals ("alive")) {
			return "";
			
		} else {
			StringBuffer b = new StringBuffer (" (");
			for (int i = 0; i < selection.length; i++) {
				String key = selection[i];
				
				// fc - 23.4.2004 - all status keys must appear in statusChooser name (!)
				b.append (Translator.swap (key));
				if (i < selection.length-1) {
					b.append ('+');
				}
			}
			b.append (')');
			return b.toString ();
		}
	}
	
	
}

