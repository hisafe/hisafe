/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2003 Francois Goreaud.
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

package capsis.lib.spatial;

import capsis.kernel.AbstractSettings;
import jeeb.lib.util.Automatable;

/**
 * VirtualParametersMixedStand - List of parameters for a mixed stand virtual stand simulation
 * May have defaults or not. May be modified by user action during 
 * initialization process in interactive mode.
 *
 * @author F. Goreaud - 23/01/06
 */
public class VirtualParametersMixedStand extends AbstractSettings implements Automatable {

	// Default values
	// Here we add all the parameters used to simulate virtual mixed stands.

	public boolean virtualStand = true;
	public int numberOfPopulation = 1;
	public VirtualParameters[] param;
	public double virtualStandXmin = 0;
	public double virtualStandXmax = 50;
	public double virtualStandYmin = 0;
	public double virtualStandYmax = 50;

}



