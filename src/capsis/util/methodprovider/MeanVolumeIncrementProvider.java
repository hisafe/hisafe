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

import capsis.kernel.MethodProvider;
import capsis.kernel.Step;

/**	MeanVolumeIncrementProvider.
*	@author Christine Deleuze, Olivier Pain - septembre 2007
*/
public interface MeanVolumeIncrementProvider  extends MethodProvider {

	/**	Compute the mean basal area increment("Surface terriere")
	*	from the root step to the given step (m2)
	*/
	public double getMeanVolumeIncrement (Step step);

}


