/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
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
package capsis.util.methodprovider;

import capsis.defaulttype.Tree;
import capsis.kernel.MethodProvider;

/**
 * Tree diameter class
 *
 * @author B.Courbaud - August 2003
 */
public interface TreeDiameterClassProvider extends MethodProvider {

	/**
	 * Tree diameter class. There are 6 categories. In french : G (Gaule), P
	 * (Perche), PB (Petit Bois), BM (Bois Moyen), GB (Gros Bois) and TGB (Très Gros
	 * Bois). In english : XXS, XS, S, M, L, XL.
	 * 
	 * For Samsara2, the 5 limit diameters are : 7.5, 17.5, 27.5, 42.5, 62.5.
	 * 
	 * See Samsa2Tree.calculateDiameterClass ().
	 */
	public int getTreeDiameterClass(Tree t);

}
