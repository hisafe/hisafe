/* 
 * Spatial library for Capsis4.
 * 
 * Copyright (C) 2001-2006 Francois Goreaud.
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


/**
 * VirtualStands - List of the characteristics of a simulated virtual stand.
 * Allows communication between VirtualStandSimulator and modules .
 * 
 * @author F. Goreaud - 10/7/02 -> 23/1/06
 */
public class VirtualStand {
//checked for c4.1.1_09 - fc - 5.2.2003

	public int treeNumber = 0;
	public double[] d;
	public double[] h;
	public double[] x;
	public double[] y;
	public int[] e;
	public int[] p;
	
	public VirtualStand() {
		treeNumber = 0;
	}
}



