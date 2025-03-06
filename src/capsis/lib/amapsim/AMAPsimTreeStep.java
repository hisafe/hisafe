/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2001  Francois de Coligny
 * 
 * This program is free software; you can redistribute it and/or modify 
 * it under the terms of the GNU General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or 
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, 
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation, 
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA. 
 */

package capsis.lib.amapsim;

import java.io.Serializable;
import java.text.NumberFormat;
import java.util.Collection;
import java.util.Locale;

/**
 * AMAPsimTreeStep : tree at step i, info returned by AMAPsim
 * 
 * @author F. de Coligny - january 2004
 */
public class AMAPsimTreeStep implements Serializable {

	public int age;			// 20.1.2004 - float -> int
	
	public float dbh;
	public float trunkDiameter260;		// (cm) - 19.1.2004
	public float mediumDiameter;		// (cm) - 19.1.2004
	
	public float height;
	public float heightDiameter7;		// (m) - 19.1.2004
	public float heightDiameter2;		// (m) - 19.1.2004
	public float mediumHeight;			// (m) - 19.1.2004
	
	public float trunkVolume;
	public float trunkVolumeDplus20;		// (m3) - 19.1.2004
	public float trunkVolumeD20to7;			// (m3) - 19.1.2004
	public float trunkVolumeD7to4;			// (m3) - 19.1.2004
	public float trunkVolumeD4to0;			// (m3) - 19.1.2004
	public float trunkVolume260;			// (m3) - 19.1.2004
	public float trunkVolumeDplus7;			// (m3) - 19.1.2004
	public float trunkVolumeD7to2;			// (m3) - 19.1.2004

	public float branchVolume;
	public float branchVolumeDplus20;		// (m3) - 19.1.2004
	public float branchVolumeD20to7;		// (m3) - 19.1.2004
	public float branchVolumeD7to4;			// (m3) - 19.1.2004
	public float branchVolumeD4to0;			// (m3) - 19.1.2004
	public float branchVolumeOrder2;		// (m3) - 19.1.2004
	public float branchVolumeOrder3;		// (m3) - 19.1.2004
	public float branchVolumeOrdern;		// (m3) - 19.1.2004
	
	public float leafSurface;
	
	public int numberOfBranches;
	public Collection branches;
	
	public int numberOfLayers;
	public Collection layers;
	
	public int numberOfUTs;		// 19.1.2004 - UT = Topological Unit
	public Collection UTs;		// 19.1.2004
	
	public int numberOfUCs;		// 19.1.2004 - UC = Growth Unit
	public Collection UCs;		// 19.1.2004
	
	
	public String toString () {	// AMAPsimTreeStep age=12
		StringBuffer b = new StringBuffer ();
		b.append ("AMAPsimTreeStep age=");
		b.append (style.format (age));
		return b.toString ();
	}

	private static NumberFormat style = NumberFormat.getNumberInstance (Locale.ENGLISH);
	static {
		style.setMaximumFractionDigits (2);
		style.setGroupingUsed (false);
	}
	
}





