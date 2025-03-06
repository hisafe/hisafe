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

import java.awt.Color;
import java.awt.Component;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;

import jeeb.lib.util.ColoredIcon;
import jeeb.lib.util.LinePanel;

/**
 * A renderer for checkable JList items.
 * From CheckListExample2.java (codeguru.developer.com).
 *
 * @author Nobuo Tamemasa
 */
public class CheckListRenderer implements ListCellRenderer {	// fc - 4.5.2005
//~ public class CheckListRenderer extends DefaultListCellRenderer {	// fc - 4.5.2005
	
	
	public CheckListRenderer() {
	
	}

	public Component getListCellRendererComponent (JList list, Object value,
			int index, boolean isSelected, boolean hasFocus) {
		
		LinePanel line = new LinePanel (0, 0);
		line.setOpaque (true);
		line.setBackground (Color.WHITE);
		
		final CheckableItem item = (CheckableItem) value;
		
		final JCheckBox cb = new JCheckBox ("", item.isSelected ());
		cb.setOpaque (true);
		cb.setBackground (Color.WHITE);
		cb.setEnabled (list.isEnabled ());	// fc - 4.5.2005
		line.add (cb);
		
		// modified on 4.5.2005 - fc - null color allowed -> no icon
		if (item.getColor () == null) {
			JLabel label = new JLabel (item.getCaption (), null, JLabel.LEFT);
			label.setEnabled (list.isEnabled ());	// fc - 4.5.2005
			line.add (label);
		
		} else {
			ColoredIcon icon = new ColoredIcon (item.getColor ());
			JLabel label = new JLabel (item.getCaption (), icon, JLabel.LEFT);
			label.setEnabled (list.isEnabled ());	// fc - 4.5.2005
			line.add (label);
		}
		
		return line;
	}
	
}


