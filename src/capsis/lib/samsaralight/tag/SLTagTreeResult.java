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
 * A result of an interception of a beam by a tree part, tagged with a beam tag
 * and an energy tag.
 * 
 * @author F. de Coligny, Frédéric André - April 2017
 */
public class SLTagTreeResult implements Serializable {

	// Default: SLTag.LEGACY, or User tag with free name
	public SLTag beamTag;

	// SLTag.TRUNK, CROWN
	public SLTag interceptionTag;

	// SLTag.DIRECT, DIFFUSE, CROWN_POTENTIAL, LOWER_CROWN_POTENTIAL
	public SLTag energyTag;

	public double energy_MJ; // MJ

	/**
	 * Constructor
	 */
	public SLTagTreeResult(SLTag beamTag, SLTag interceptionTag, SLTag energyTag, double energy_MJ) {
		this.beamTag = beamTag;
		this.interceptionTag = interceptionTag;
		this.energyTag = energyTag;
		this.energy_MJ = energy_MJ;

	}

	@Override
	public String toString() {
		return "SLTagTreeResult, beamTag: " + beamTag + ", energyTag: " + energyTag + ", interceptionTag: "
				+ interceptionTag + ", energy_MJ: " + energy_MJ;
	}

}
