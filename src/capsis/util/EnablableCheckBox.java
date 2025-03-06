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

import jeeb.lib.util.LinePanel;

/**
 * EnablableCheckBox. is a JCheckBox with can be enabled / disabled
 * 
 * @author F. de Coligny - september 2004
 */
public class EnablableCheckBox extends LinePanel 
		implements ActionListener, Comparable {
	private JCheckBox enabled;
	private JCheckBox checked;
	private String label;
	
	public EnablableCheckBox (boolean enabled, boolean checked, String label) {
		super ();
		this.enabled = new JCheckBox ("", enabled);
		this.checked = new JCheckBox (label, checked);
		this.enabled.addActionListener (this);
		this.label = label;
		
		add (this.enabled);
		add (this.checked);
		addGlue ();
		synchronize ();
	}
	
	private void synchronize () {
		checked.setEnabled (enabled.isSelected ());
	}

	public boolean isEnabled () {return enabled.isSelected ();}
	public boolean isChecked () {return checked.isSelected ();}
	public String getLabel () {return label;}
	
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (enabled)) {
			synchronize ();
		}
	}

	/**	Can be used to sort EnableCheckBox instances in a TreeSet.
	*/
	public int compareTo (Object o) {
		EnablableCheckBox other = (EnablableCheckBox) o;
		return label.compareTo (other.getLabel ());
	}
	
}