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

/**
 * An interface for Scenes which contain target trees.
 * 
 * @author G. Ligot, F. de Coligny - January 2012
 */
public interface ContainsTargetTrees {

	/**
	 * Returns true if the given tree is a target tree (may be excluded from
	 * thinnings).
	 */
	public boolean isTargetTree(Tree t);

}
