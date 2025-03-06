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

/**	Radius of the tree at the given height under or over bark.
*	@author F. de Coligny, F. Mothe - january 2006
*/
public interface TreeRadius_cmProvider extends MethodProvider {

	/**	
	 * This method returns the radius of a cross section at any height along the tree bole.
	 * @param t the Tree instance that serves as subject
	 * @param h the height of the cross section
	 * @param overBark true to obtain the overbark radius or false otherwise
	 * @return the radius of the cross section (cm)
	 */
	public double getTreeRadius_cm (Tree t, double h, boolean overBark);
	
}


