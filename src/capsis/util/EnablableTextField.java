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
import javax.swing.JTextField;

import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * EnablableTextField is a (JLabel + JTextField) with can be enabled / disabled
 * 
 * @author F. de Coligny - september 2004
 */
public class EnablableTextField extends LinePanel 
		implements ActionListener, Comparable {
	private JCheckBox enabled;
	private JTextField value;
	private String label;
	private String name;
	
	/**	Constructor. Label may be different then name.
	*/
	public EnablableTextField (boolean enabled, String value, String label, String name) {
		super ();
		this.enabled = new JCheckBox ("", enabled);
		this.enabled.addActionListener (this);
		this.value = new JTextField (value);
		this.label = label;
		this.name = name;
		
		add (this.enabled);
		add (new JWidthLabel (Translator.swap (label), 150));	// label may be translated or not
		add (this.value);
		addGlue ();
		synchronize ();
	}

	private void synchronize () {
		value.setEnabled (enabled.isSelected ());
	}

	public boolean isEnabled () {return enabled.isSelected ();}
	public String getValue () {return value.getText ();}
	public String getLabel () {return label;}
	public String getName () {return name;}

	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (enabled)) {
			synchronize ();
		}
	}

	/**	Can be used to sort EnablableTextField instances in a TreeSet.
	*/
	public int compareTo (Object o) {
		EnablableTextField other = (EnablableTextField) o;
		return label.compareTo (other.getLabel ());
	}

}