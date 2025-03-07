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

import capsis.lib.samsaralight.SLBeamSet;

/**
 * A light beam set composed of direct or diffuse beams.
 *
 * @see SafeBeam
 * @author B. Courbaud CEMAGREF Grenoble - January 2000 - Benoit.Courbaud@grenoble.cemagref.fr
 */
public class SafeBeamSet<T> extends SLBeamSet {

	private static final long serialVersionUID = 1L;
	//sky mask by slope
	private double skyDiffuseMask;
	private double skyDirectMask;
	private double skyInfraRedMask;

	public SafeBeamSet () {
		super ();
		skyDiffuseMask = 1;
		skyDirectMask = 1;
	}

	public float getSize () {return  getBeams().size();}
	public double getSkyDiffuseMask () {return  skyDiffuseMask;}
	public double getSkyDirectMask () {return  skyDirectMask;}
	public double getSkyInfraRedMask () {return skyInfraRedMask;}

	public void setSkyDiffuseMask (double e) {skyDiffuseMask =  e;}
	public void setSkyDirectMask(double e) {skyDirectMask =  e;}
	public void setSkyInfraRedMask(double e) {skyInfraRedMask =  e;}

}