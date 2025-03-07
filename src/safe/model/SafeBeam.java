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

import java.text.NumberFormat;
import java.util.Vector;
import capsis.lib.samsaralight.SLBeam;


/**
 * A light beam (direct or diffuse used in the tree light interception process )
 *
 * @see SafeBeamSet
 * @author B. Courbaud CEMAGREF Grenoble - January 2000 - Benoit.Courbaud@grenoble.cemagref.fr
 */
public class SafeBeam extends SLBeam {
 
	private static final long serialVersionUID = 1L;
	private float diffuseEnergy;	//=(fraction of diffuse radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)
	private float directEnergy;		//=(fraction of direct radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)
	private float infraRedEnergy;	//=(fraction of infra-red radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)

	private Vector<SafeShadingMask> shadingMasks;		//for each CellImpact, a mask of potentially shading neighbor crops.

	public SafeBeam (double a, double h, float difE, float dirE, float ire, float convFactor) {

		super(a, h, convFactor, false);
		
		//lightening a surface of unit projection on horizontal and energy of a beam lightening a unit
		//horizontal surface (=1 when no slope)
		diffuseEnergy=difE;
		directEnergy=dirE;
		infraRedEnergy=ire;
		shadingMasks = new Vector<SafeShadingMask>();

	}

	public float getDiffuseEnergy () {
		return diffuseEnergy;
	}

	public float getDirectEnergy () {
		return directEnergy;
	}

	public void setDirectEnergy (float dirE) {
		directEnergy=dirE;
	}

	public float getInfraRedEnergy () {
		return infraRedEnergy;
	}

	public void setInfraRedEnergy (float ire) {
		infraRedEnergy=ire;
	}

	public void resetDirectEnergy (){
		directEnergy=0;
	}

	public void addShadingMask (SafeShadingMask mask){	
		shadingMasks.add(mask);
	}

	public Vector<SafeShadingMask> getShadingMasks() {	
		return shadingMasks;
	}

	public void removeShadingMasks (){
		shadingMasks.removeAllElements();
	}

	/**
	 * Clear the neighbourCells collection
	 */
	public void removeAllNeighbourCell () {
		sites.clear ();
	}

	public String toString(){
		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(0);

		String str = super.toString ()
			+ " azimut="+nf.format(Math.toDegrees(this.getAzimut_rad()))
			+ " heightAngle="+nf.format(Math.toDegrees(this.getHeightAngle_rad()));
		return str;
	}
	
}