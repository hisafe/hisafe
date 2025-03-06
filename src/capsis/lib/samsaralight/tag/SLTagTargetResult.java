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

package capsis.lib.samsaralight.tag;

import java.io.Serializable;

/**
 * Remaining energy in MJ/m2 of a beam arriving on a target (cell, sensor...) after having been
 * intercepted by trees.
 * 
 * @author F. de Coligny, Frédéric André - April 2017
 */
public class SLTagTargetResult implements Serializable {

	// Default: SLTag.LEGACY, or User tag with free name
	public SLTag beamTag;

	// SLTag.DIRECT, DIFFUSE
	public SLTag energyTag;
	
	// fa-08.01.2018: SLTag.HORIZONTAL, SLOPE
	public SLTag orientationTag;
	
	public double energy_MJm2; // MJ/m2

	// fc+fa-28.4.2017
	// Tentative: add slope/horizontal diffuse/direct energy in tag result
	public double slopeDirectEnergy_MJm2; // MJ/m2
	public double slopeDiffuseEnergy_MJm2; // MJ/m2
	public double horizontalDirectEnergy_MJm2; // MJ/m2
	public double horizontalDiffuseEnergy_MJm2; // MJ/m2

	/**
	 * Constructor
	 */
	public SLTagTargetResult(SLTag beamTag, SLTag energyTag, SLTag orientationTag, double energy_MJm2) { //fa-08.01.2018: added orientationTag
		this.beamTag = beamTag;
		this.energyTag = energyTag;
		this.orientationTag = orientationTag; // fa-08.01.2018
		this.energy_MJm2 = energy_MJm2;

	}

	@Override
	public String toString() {
		return "SLTagTargetResult, beamTag: " + beamTag + ", energyTag: " + energyTag + ", orientationTag: " + orientationTag + ", energy_MJm2: "
				+ energy_MJm2;
	}

}
