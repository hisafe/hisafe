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

import java.awt.Component;
import java.awt.Font;
import java.awt.Insets;

import javax.swing.JTable;
import javax.swing.JTextArea;
import javax.swing.table.TableCellRenderer;

/**	MultiLineTableCellRenderer is a Cell renderer for multiline strings in a JTable
* 
*	@author F. de Coligny - february 2006
*/
public class MultiLineTableCellRenderer extends JTextArea
		implements TableCellRenderer {

	public MultiLineTableCellRenderer (Font font, int tabSize) {
		setFont (font);
		setLineWrap (false);
		setTabSize (tabSize);
		setMargin (new Insets (0, 10, 0, 0));
	}

	public Component getTableCellRendererComponent (JTable table,
			Object object, boolean isSelected, boolean hasFocus, int row,
			int col) {

		setText ((String) object);
		return this;
	}
}
