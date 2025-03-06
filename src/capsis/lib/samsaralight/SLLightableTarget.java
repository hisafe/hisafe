/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
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

package capsis.lib.samsaralight;

import capsis.defaulttype.plotofcells.SquareCell;

/**
 * SLLightableTarget - a cell or a sensor with a SLTargetLightResult light
 * properties object.
 * 
 * @author B. Courbaud, N. Dones, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public interface SLLightableTarget { // a cell, a sensor...

	// fc+fa-1.8.2017 reviewed and refactored

	public SLTargetLightResult getLightResult();

	public void setLightResult(SLTargetLightResult c);

	/**
	 * Returns the square cell matching this lightable target. If the target is
	 * a cell, return itself. If the target is a sensor, return the cell below
	 * the sensor...
	 */
	public SquareCell getMatchingCell();


}
