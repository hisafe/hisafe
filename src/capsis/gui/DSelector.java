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

import java.util.Collection;

/**
 * DSelector contains static methods to select things with dialogs.
 * 
 * @author F. de Coligny - august 2003
 */
public class DSelector {
	
	// Usage (open a graphical dialog box) : 
	// Collection selectedIds = DSelector.selectCellIds (cells, alreadySelectedCellIds);
	//
	static public Collection selectCellIds (Collection cells, Collection alreadySelectedCellIds) {
		CellSelectionDialog dlg = new CellSelectionDialog (cells, alreadySelectedCellIds);
		return dlg.getSelectedCellIds ();
	}

}



