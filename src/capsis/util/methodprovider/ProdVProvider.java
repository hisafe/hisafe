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

import java.util.Collection;

import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.Flag;

/**	Total production volume since the beginning of the scenario.
*
*	@author F. de Coligny - apr 2001 / S. Perret - jan 2004 / fc - sept 2005
*/
public interface ProdVProvider extends MethodProvider {

	/**	ProdV: total production volume (m3) since the beginning of the scenario 
	*	(must be cumulated), including mortality and thinnings (cut trees).
	*	
	*	Parameters: ProdV must be calculated for the given stand. If trees is null, 
	*	ignore it. If not null, ProdV must be calculated for these trees (a group).   
	*	
	*	Flag possible values: correct (0), s == null (-1), 
	*	trees is a group and implementation does not support groups (-2).
	*	
	*	If flag is 0, returns ProdV else returns -1.
	*/
	public double getProdV (GScene stand, Collection trees, Flag flag);

}


