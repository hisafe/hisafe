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

import java.io.Serializable;

import jeeb.lib.util.Record;

/**
 * A monthly irradiation record. Can be used to calculate a BeamSet.
 * 
 * @author B. Courbaud, N. Donès, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLMonthlyRecord extends Record implements Serializable {

	public int month; // [1,12]
	public double global; // >= 0, MJ/m2 // fc+mj+lw-18.10.2016
	public double diffuseToGlobalRatio; // [0,1]

	/**
	 * Default constructor
	 */
	public SLMonthlyRecord() {
		super();
	}

	/**
	 * Constructor
	 */
	public SLMonthlyRecord(String line) throws Exception {
		super(line);
	}

	public String toString() {
		return "SLHourlyRecord" + " month: " + month + " global: " + global + " diffuseToGlobalRatio: "
				+ diffuseToGlobalRatio;
	}

}
