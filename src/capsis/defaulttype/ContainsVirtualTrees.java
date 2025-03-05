/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2012 INRA 
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
 */
package capsis.defaulttype;

import java.util.List;

/**
 * An interface for Scenes which contain target trees.
 * 
 * @author F. Andre - January 2023
 */
public interface ContainsVirtualTrees {

	/**
	 * Returns true if the given tree is a virtual tree
	 */
	public boolean isVirtualTree(Tree t);
	
	/**
	 * Returns the list of Ids of trees which are virtual copies of the targeted tree t
	 */
	public List<Integer> getVirtualTreeIdsBasedOnMe(Tree t);

}
