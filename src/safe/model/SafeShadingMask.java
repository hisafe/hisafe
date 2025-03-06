/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */
package safe.model;

import java.io.Serializable;
import java.util.Vector;

import jeeb.lib.util.Vertex3d;


/**
 * A collection of SafeShadingNeighbour (used for light interception by crops)
 *
 * @see SafeShadingNeighbour and SafeBeam
 * @author G.TALBOT	- INRAE Montpellier France- May 2007
 */


public class SafeShadingMask implements Serializable {

private static final long serialVersionUID = 1L;
protected Vector<SafeShadingNeighbour> shadingNeighbours;	// relative coordinates (Points)
private Vertex3d impact;

	public SafeShadingMask(Vertex3d imp) {
		impact = imp;
		shadingNeighbours = new Vector<SafeShadingNeighbour> ();
	}

	public void addShadingNeighbour (SafeShadingNeighbour c) {
		shadingNeighbours.add (c);
	}

	public Vector<SafeShadingNeighbour> getShadingNeighbours () {
		return shadingNeighbours;
	}

	public Vertex3d getImpact (){
		return impact;
	}
}

