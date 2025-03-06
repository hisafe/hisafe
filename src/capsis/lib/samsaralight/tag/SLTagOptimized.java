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


/**
 * A superclass for tags in the SamsaraLight library. Tag mode is optional,if
 * activated, detailed results at tag level are provided by the library in the
 * lightable objects (trees, cells). E.g. if some beams are tagged
 * "BUD_BURST_DATE" and tag mode is activated, it will be possible to get the
 * results directly associated with these beams.
 * 
 * It is possible to build subclasses of this Tag with extra properties, e.g.
 * name, doy, hour.
 * 
 * This subclass is for the tag optimized process with only one diffuse beam
 * list per keydoy + complete leaf development period.
 * 
 * @author F. de Coligny, Frédéric André - August 2017
 */
public class SLTagOptimized extends SLTag {

	private double[] diffuseIncidentEnergies;
//	private double 
	
	/**
	 * Constructor.
	 */
	public SLTagOptimized(String name, double[] diffuseIncidentEnergies) {
		super (name);
		this.diffuseIncidentEnergies = diffuseIncidentEnergies;
	}

	public String getName() {
		return name;
	}

	public double[] getDiffuseIncidentEnergies() {
		return diffuseIncidentEnergies;
	}
	
	@Override
	public String toString() {
		return "SLTagOptimized " + name;
	}
}
