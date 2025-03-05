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

package capsis.extension.modeltool.rivermaker;

import jeeb.lib.util.Vertex3d;
import jeeb.lib.util.Node;
import capsis.util.WatershedNode;

/**	Weir : "seuil" - threshold in the river
*
*	@author B. Parisi - march 2006
*/
public class Weir extends Node implements WatershedNode {

	private int id;
	private byte upPass;
	private byte downPass;
	private Vertex3d origin;
	private String address;		// fc - 3.6.2004

	public Weir (int id,
			String address,
			byte upPass,
			byte downPass,
			Vertex3d origin
			) {
		this.id = id;
		this.address = address;
		this.upPass = upPass;
		this.downPass = downPass;
		this.origin = origin;
	}

	// accessors

	public int getId () {return id;}
	public String getAddress () {return address;}
	public byte getUpPass () {return upPass;}
	public byte getDownPass () {return downPass;}
	public Vertex3d getOrigin () {return origin;}
	public Vertex3d getEnd () {return null;}	// also

	public void setOrigin (Vertex3d v) {origin = v;}
	public void setEnd (Vertex3d v) {}	// unused for weir

	// Modifiers for instance variables

	public void setUpPass (byte l) {upPass = l;}
	public void setDownPass (byte l) {downPass = l;}

	public String toString () {return "weir "+id;}

}



