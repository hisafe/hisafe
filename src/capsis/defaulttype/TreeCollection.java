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

package capsis.defaulttype;

import java.util.Collection;


/**	Interface for a collection of Trees. 
 *	@author F. de Coligny - december 2000, reviewed january 2011
 */
public interface TreeCollection {

	/**	Adds a tree in the TreeCollection.
	 * 	May throw an UnsupportedOperationException in case the 
	 * 	TreeCollection is not modifiable. 
	 */
	public boolean addTree (Tree t);
	
	/**	Removes a tree from the TreeCollection.
	 * 	May throw an UnsupportedOperationException in case the 
	 * 	TreeCollection is not modifiable. 
	 */
	public void removeTree (Tree t);
	
	/**	Removes all trees from the TreeCollection.
	 * 	May throw an UnsupportedOperationException in case the 
	 * 	TreeCollection is not modifiable. 
	 */
	public void clearTrees ();
	
	/**	Returns all the trees.
	 */
	public Collection<? extends Tree> getTrees ();
	
	/**	Returns the tree with the given id.
	 * 	The tree ids should be unique within the TreeCollection.
	 */
	public Tree getTree (int id);
	
	/**	Returns all the tree ids.
	 * 	The tree ids should be unique within the TreeCollection.
	 */
	public Collection<Integer> getTreeIds ();
	

}

