/* 
* The Genetics library for Capsis4
* 
* Copyright (C) 2002-2004  Ingrid Seynave, Christian Pichot
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
package capsis.lib.genetics;

import java.io.Serializable;

/**
 * Comment needed here.
 *
 * @author I. Seynave - June 2002, F. de Coligny - November 2004
 */
public class Kinship implements Serializable {
	
	private double [][] initialPhiArray;
	private double defaultPhi;

	
	/**	Constructor for new Kinship.
	*/
	public Kinship (double[][] t, double p) {
		initialPhiArray = t;
		defaultPhi = p;
	}

	public double[][] getInitialPhiArray () {return initialPhiArray;}
	public double getDefaultPhi () {return defaultPhi;}

}
