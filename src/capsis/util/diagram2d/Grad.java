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

package capsis.util.diagram2d;


/**
 * A complete prepared graduation (not computed)
 */
public class Grad {
	// Position of the graduation
	public double anchor;
	
	// Label of the graduation
	public String label;
	
	public Grad (double anchor, String label) {
		this.anchor = anchor;
		this.label = label;
	}
	
	public String toString () {
		return "("+anchor+" "+label+")";
	}
	
	public static void main (String[] a) {
		System.out.println ("0 % 1 = "+0 % 1);
		System.out.println ("0 % 2 = "+0 % 2);
		System.out.println ();
		System.out.println ("1 % 1 = "+1 % 1);
		System.out.println ("1 % 2 = "+1 % 2);
		System.out.println ();
		System.out.println ("3 % 1 = "+3 % 1);
		System.out.println ("3 % 2 = "+3 % 2);
	}
	
}


