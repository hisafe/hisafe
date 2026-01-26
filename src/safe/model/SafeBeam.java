/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
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
 * A light BEAM (direct or diffuse used in the tree light interception process )
 *
 * @see SafeBeamSet
 * @author : B. Courbaud CEMAGREF Grenoble - January 2000 - Benoit.Courbaud@grenoble.cemagref.fr
 */
public class SafeBeam extends SLBeam {
 
	/** fraction of diffuse radiation allocated to this beam (m-2 of beam section) */
	private float diffuseEnergy;
	/** fraction of direct radiation allocated to this beam (m-2 of beam section) */
	private float directEnergy;	
	/** fraction of infra red radiation allocated to this beam (m-2 of beam section) */
	private float infraRedEnergy;	
	/** for each CellImpact, a mask of potentially shading neighbor cells */
	private Vector<SafeShadingMask> shadingMasks;	

	/**
	 * Constructor
	 *
	 * @param azimuth beam azimuth in radiant
	 * @param height beam heightAngle in radiant
	 * @param diffuseEnergy beam diffuse energy in m-2 of beam section
	 * @param directEnergy beam direct energy in m-2 of beam section
	 * @param infraRedEnergy beam infraRed energy in m-2 of beam section
	 * @param reativelEnergy beam relative energy 
	 */
	public SafeBeam (double azimuth, double height, float diffuseEnergy, float directEnergy, float infraRedEnergy, float reativelEnergy) {

		super(azimuth, height, reativelEnergy, false);
		
		//lightening a surface of unit projection on horizontal and energy of a beam lightening a unit
		//horizontal surface (=1 when no slope)
		this.diffuseEnergy=diffuseEnergy;
		this.directEnergy=directEnergy;
		this.infraRedEnergy=infraRedEnergy;
		shadingMasks = new Vector<SafeShadingMask>();
	}
	/**
	 * Return the beam diffuse energy (m-2 of beam section)
	 **/
	public float getDiffuseEnergy () {return diffuseEnergy;}
	/**
	 * Return the beam direct energy (m-2 of beam section)
	 **/
	public float getDirectEnergy () {return directEnergy;}
	/**
	 * Return the beam infra red energy (m-2 of beam section)
	 **/
	public float getInfraRedEnergy () {return infraRedEnergy;}
	/**
	 * Set beam diffuse energy (m-2 of beam section)
	 **/
	public void setDiffuseEnergy (float dirE) {directEnergy=dirE;}
	/**
	 * Set beam direct energy (m-2 of beam section)
	 **/
	public void setDirectEnergy (float dirE) {directEnergy=dirE;}
	/**
	 * Set beam infra red energy (m-2 of beam section)
	 **/
	public void setInfraRedEnergy (float ire) {infraRedEnergy=ire;}
	/**
	 * Return the ShadingMark collection (a mask of potentially shading neighbor cells)
	 **/
	public Vector<SafeShadingMask> getShadingMasks() {	return shadingMasks;}
	/**
	 * Remove all elements of the ShadingMark collection (a mask of potentially shading neighbor cells)
	 **/
	public void removeShadingMasks (){shadingMasks.removeAllElements();}
	/**
	 * Add an elements to the ShadingMark collection (a mask of potentially shading neighbor cells)
	 * @param mask The SafeShadingMask object to add to the collection 
	 **/
	public void addShadingMask (SafeShadingMask mask){shadingMasks.add(mask);}
	/**
	 * Clear the neighbourCells collection
	 */
	public void removeAllNeighbourCell () {sites.clear ();}

	public String toString(){
		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(0);

		String str = super.toString ()
			+ " azimut="+nf.format(Math.toDegrees(this.getAzimut_rad()))
			+ " heightAngle="+nf.format(Math.toDegrees(this.getHeightAngle_rad()));
		return str;
	}
	
}