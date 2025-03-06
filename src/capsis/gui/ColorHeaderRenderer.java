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

import java.awt.Color;
import java.awt.Component;

import javax.swing.BorderFactory;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.SwingConstants;
import javax.swing.table.TableCellRenderer;

/**
 * Colored JLabel for table headers.
 * 
 * @author F. de Coligny - june 2000
 */
public class ColorHeaderRenderer extends JLabel implements TableCellRenderer {
	private Color backgroundColor;
	private Color foregroundColor;

	public void setBackgroundColor (Color c) {
		backgroundColor = c;
	}
	public void setForegroundColor (Color c) {
		foregroundColor = c;
	}

	public Component getTableCellRendererComponent (JTable table,
														Object value,
														boolean isSelected,
														boolean hasFocus,
														int row,
														int column) {
		setOpaque (true);
		setBorder (BorderFactory.createRaisedBevelBorder ());
		
		if (backgroundColor == null) {
			setBackground (Color.lightGray);
		} else {
			setBackground (backgroundColor);
		}
		
		if (foregroundColor == null) {
			setForeground (Color.black);
		} else {
			setForeground (foregroundColor);
		}
		setHorizontalAlignment (SwingConstants.CENTER);
		setText ((String) value);
		return this;
	}


}