/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2012  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.defaulttype;

import java.util.List;

/**
 * A spatializer for a treeList: returns an avatar per tree to be drawn. E.g.
 * for a list of 100 individual trees, return 100 avatars. For a list of 2
 * Numberable trees with numbers equal to 12 and 25 resp., returns 37 individual
 * avatars to be drawn.
 * 
 * @author F. de Coligny - February 2012
 */
public interface Spatializer {

	/**
	 * Returns a list of individual spatialized avatars for the trees
	 * (individuals or class widths) in the given treeList.
	 */
	public List<TreeAvatar> getAvatars(TreeList treeList);

}
