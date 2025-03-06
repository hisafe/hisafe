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

package capsis.util;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JTextField;

import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * EnablableTextFieldCouple is a couple of JTextField (with some labels arround) 
 *  which can be enabled / disabled.
 * 
 * @author F. de Coligny - september 2004
 */
public class EnablableTextFieldCouple extends LinePanel 
		implements ActionListener, Comparable {
	private JCheckBox enabled;
	private JTextField value1;
	private JTextField value2;
	private String label;
	private String name;
	
	/**	Constructor. Label may be different then name.
	*/
	public EnablableTextFieldCouple (boolean enabled, 
			String label1, String value1, 
			String label2, String value2, 
			String label, String name) {
		super ();
		this.enabled = new JCheckBox ("", enabled);
		this.enabled.addActionListener (this);
		this.value1 = new JTextField (value1, 2);
		this.value2 = new JTextField (value2, 2);
		this.label = label;
		this.name = name;
		
		add (this.enabled);
		add (new JWidthLabel (Translator.swap (label), 140));	// name may be translated or not
		add (new JLabel (Translator.swap (label1)));
		add (this.value1);
		add (new JLabel (Translator.swap (label2)));
		add (this.value2);
		addGlue ();
		synchronize ();
	}

	private void synchronize () {
		value1.setEnabled (enabled.isSelected ());
		value2.setEnabled (enabled.isSelected ());
	}

	public boolean isEnabled () {return enabled.isSelected ();}
	public String getValue1 () {return value1.getText ();}
	public String getValue2 () {return value2.getText ();}
	public String getLabel () {return label;}
	public String getName () {return name;}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (enabled)) {
			synchronize ();
		}
	}

	/**	Can be used to sort EnablableTextFieldCouple instances in a TreeSet.
	*/
	public int compareTo (Object o) {
		EnablableTextFieldCouple other = (EnablableTextFieldCouple) o;
		return label.compareTo (other.getLabel ());
	}

}