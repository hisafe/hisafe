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


/**	Tree volume, with name of the volume.
*	@author R. Schneider, F. de Coligny - may 2008
*/
public interface VProviderWithName extends VProvider {

	/**	Compute the volume of the given trees. Note : some
	*	trees may be Numberable, i.e. represent several trees.
	*/
	//~ public double getV (GStand stand, Collection trees);
	
	/**	Returns the name of volume.
	*/
	public String getVolumeName ();
	
}


