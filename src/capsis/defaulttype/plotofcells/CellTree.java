/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA 
 * 
 * Authors: F. de Coligny 
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
package capsis.defaulttype.plotofcells;

import jeeb.lib.util.Identifiable;

/**	An interface for trees that are located in ground cells.
 *	@author F. de Coligny - march 2011
 */
public interface CellTree extends Identifiable {

	/**	Returns id of the Tree
	 */
	public int getId ();

	/**	Returns the ground cell containing this tree
	 */
	public TreeListCell getCell ();
	
	/**	Sets the ground cell containing this tree
	 */
	public void setCell (TreeListCell cell);
	
	
}
