/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
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
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
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

