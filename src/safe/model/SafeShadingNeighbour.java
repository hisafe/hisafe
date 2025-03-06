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

import java.awt.Point;

import capsis.defaulttype.ShiftItem;

/**
 * Used for light interception by crops
 * Contains : a neigbour cell, its relative coordinates, and informations about the path of a
 * beam above this cell (distance to impact point of entrance and exit points)
 *
 * @see SafeShadingMask
 * @author G.TALBOT	- INRAE Montpellier France- May 2007
 */

public class SafeShadingNeighbour {
	Point p;		//cell coordinates (x,y) relatively to target cell
	double lOut; 	//distance entre le point de sortie de la cellule concurrente et le point d'impact
	double lIn;
	SafeCell cell;
	ShiftItem shift;

	/*
	Constructor
	*/

	public SafeShadingNeighbour (Point e, double lin, double lout){
		p = e;
		lOut = lout;
		lIn = lin;
	}

	/*
	accessors
	*/

	public void setLOut (double l) {lOut = l;}
	public double getLOut () {return lOut;}
	public void setLIn (double l) {lIn = l;}
	public double getLIn () {return lIn;}
	public SafeCell getCell() {return cell;}
	public ShiftItem getShift() {return shift;}
	public void setCell (SafeCell c) {cell = c;}
	public void setShift (ShiftItem s) {shift = s;}
	public Point getRelCoordinates () {return p;}


}

