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

import capsis.lib.samsaralight.SLBeamSet;

/**
 * A light BEAM SET composed of direct or diffuse beams.
 *
 * @see SafeBeam
 * @author : B. Courbaud CEMAGREF Grenoble - January 2000 - Benoit.Courbaud@grenoble.cemagref.fr
 */
public class SafeBeamSet<T> extends SLBeamSet {

	/**  proportion of diffuse radiation reaching the scene */
	private double skyDiffuseMask;
	/**  proportion of direct radiation reaching the scene */
	private double skyDirectMask;
	/**  proportion of infra red radiation reaching the scene */
	private double skyInfraRedMask;

	/**
	 * Constructor
	 */
	public SafeBeamSet () {
		super ();
		skyDiffuseMask = 1;
		skyDirectMask = 1;
	}
	/**
	 * Return the beam set size 
	 */
	public float getSize () {return  getBeams().size();}
	/**
	 * Return the proportion of diffuse radiation reaching the scene
	 */
	public double getSkyDiffuseMask () {return  skyDiffuseMask;}
	/**
	 * Return the proportion of direct radiation reaching the scene
	 */	
	public double getSkyDirectMask () {return  skyDirectMask;}
	/**
	 * Return the proportion of infra red radiation reaching the scene
	 */	
	public double getSkyInfraRedMask () {return skyInfraRedMask;}
	/**
	 * Set the proportion of diffuse radiation reaching the scene
	 */	
	public void setSkyDiffuseMask (double e) {skyDiffuseMask =  e;}
	/**
	 * Set the proportion of direct radiation reaching the scene
	 */		
	public void setSkyDirectMask(double e) {skyDirectMask =  e;}
	/**
	 * Set the proportion of infra red radiation reaching the scene
	 */	
	public void setSkyInfraRedMask(double e) {skyInfraRedMask =  e;}

}