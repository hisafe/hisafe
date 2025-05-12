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

import java.io.Serializable;

/**
 * Crop species parameters Hi-sAFe specific
 * STICS usual parameters are in safe.stics.SafeSticsCrop
 *
 * @author : Isabelle Lecomte - INRA (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeCropSpecies implements Serializable, Cloneable {

	/** Crop species name  */
	private String name; 								
	/** File species name  */
	private String fileName; 							
	/** Crop root diameter (cm)  */
	private double cropRootDiameter;				
	/** Root axial conductance (cm cm-1) 
	* (1/resistance involved in water transport inside the root per unit gradient in water potential and per unit path-length) */
	private double cropRootConductivity;			
	/** Parameter for calculating the transpiration reduction factor following Campbell (unitless) */
	private double cropAlpha;
	/** Minimum crop transpiration potential (cm) */
	private double cropMinTranspirationPotential;		
	/** Maximum crop transpiration potential (cm) */
	private double cropMaxTranspirationPotential;
	/** Potential drop needed to enter the root expressed as a % of soil water potential (cm)*/
	private double cropBufferPotential;				
	/** Longitudinal resistance factor for root sap (mm cm-1 m-1) */
	private double cropLongitudinalResistantFactor;		
	/** Relative influence of dry voxels on the calculation of the averaged soil water potential perceived by the plant
	* When = 1, we use a harmonic average (unitless) */
	public  double cropHarmonicWeightedMean ;
	/** Store value P_stamflax from STICS to avoid lost of value when chaining years */
	public float[] P_stamflax;   
	/** Store value P_stlevamf from STICS to avoid lost of value when chaining years */
	public float[] P_stlevamf;   
	/** Store value P_stlevdrp from STICS to avoid lost of value when chaining years */
	public float[] P_stlevdrp;     
	/** Store value P_stflodrp from STICS to avoid lost of value when chaining years */
	public float[] P_stflodrp;      
	/** Store value P_stlaxsen from STICS to avoid lost of value when chaining years */
	public float[] P_stlaxsen;     
	/** Store value P_stsenlan from STICS to avoid lost of value when chaining years */
	public float[] P_stsenlan;    
	/** Store value P_stdrpmat from STICS to avoid lost of value when chaining years */
	public float[] P_stdrpmat;   
	/** Store value P_stdrpdes from STICS to avoid lost of value when chaining years */
	public float[] P_stdrpdes;      
	
	/**	Constructor
	*/
	public SafeCropSpecies ()   {
		P_stamflax= new float[30];      
		P_stlevamf= new float[30];     
		P_stlevdrp= new float[30];      
		P_stflodrp= new float[30];      
		P_stlaxsen= new float[30];      
		P_stsenlan= new float[30];     
		P_stdrpmat= new float[30];          
		P_stdrpdes= new float[30]; 
	}

	/**
	 * return Campbell factor (dimensionless) - ICRAF method
	 */
	public double getCampbellFactorIcraf() {
		return (2 * Math.log (cropAlpha / (1 - cropAlpha))
				/ Math.log (cropMaxTranspirationPotential / cropMinTranspirationPotential));
	}
	
	/**
	 * return Campbell factor (dimensionless) - NOT USED
	 * @param plantWaterPotential Plant water potential
	 */
	public double getCampbellFactor (double plantWaterPotential) {
		double halfCurrWaterPotential= getHalfCurrWaterPotential();
		double a = getA();
		return 1.0/(1.0+Math.pow(plantWaterPotential/halfCurrWaterPotential,a));
	}
	
	/**
	*  return water potential where tranpiration demand is half of its potential - ICRAF method (mm)
	*/
	public double getHalfCurrWaterPotentialIcraf() {
			return (cropMaxTranspirationPotential * Math.pow ((1 - cropAlpha) / cropAlpha, 1 / getCampbellFactorIcraf()));
	}

	/**
	 * return water potential where tranpiration demand is half of its potential - NOT USED (mm)
	 */
	public double getHalfCurrWaterPotential() {
			return -Math.sqrt (cropMaxTranspirationPotential * cropMinTranspirationPotential);
	}
	

	public double getA() {
			return (2.0 * Math.log (cropAlpha / (1 - cropAlpha))
					   / Math.log (cropMaxTranspirationPotential / cropMinTranspirationPotential));
	}
	/**
	 * return crop species name
	 */
	public String getName () {return name;}
	/**
	 * return crop species file name
	 */	
	public String getFileName () {return fileName;}
	/**
	 * return crop species root diameter (cm)
	 */
	public double getCropRootDiameter() {return cropRootDiameter;}
	/**
	 * return crop species parameter for transpiration reduction factor following Campbell (unitless)
	 */
	public double getCropAlpha() {return cropAlpha;}
	/**
	 * return crop species root axial conductivity (cm cm-1) 
	 */
	public double getCropRootConductivity() {return cropRootConductivity;}
	/**
	 * return crop species max transpiration coefficient (cm)
	 */
	public double getCropMaxTranspirationPotential() {return cropMaxTranspirationPotential;}
	/**
	 * return crop species min transpiration coefficient (cm) 
	 */
	public double getCropMinTranspirationPotential() {return cropMinTranspirationPotential;}
	/**
	 * return crop species buffer potential (%)
	 */
	public double getCropBufferPotential() {return cropBufferPotential;}
	/**
	 * return crop species longitudinal resistance factor (unitless)
	 */
	public double getCropLongitudinalResistantFactor() {return cropLongitudinalResistantFactor;}
	/**
	 * return crop species harmonic weighted mean (unitless)
	 */
	public double getCropHarmonicWeightedMean() {return cropHarmonicWeightedMean;}
	/**
	 * Set the crop species name
	 * @param value The crop species name
	 */
	public void setName (String value) {name = value;}
	/**
	 * Set the crop species file name
	 * @param value The crop species file name
	 */
	public void setFileName (String value) {fileName = value;}
	/**
	 * Set the crop root diameter
	 * @param value The crop root diameter (cm) 
	 */
	public void setCropRootDiameter (double value) {cropRootDiameter = value;}
	/**
	 * Set the crop species parameter for transpiration reduction factor following Campbell
	 * @param value The crop species parameter for transpiration reduction factor following Campbell (unitless)
	 */
	public void setCropAlpha (double value) {cropAlpha = value;}
	/**
	 * Set the crop species root conductivity parameter
	 * @param value The crop species root conductivity parameter (cm day-1)
	 */
	public void setCropRootConductivity(double value) {cropRootConductivity = value;}
	/**
	 * Set the crop species maximum transpiration potential
	 * @param value The crop species maximum transpiration potential (cm)
	 */
	public void setCropMaxTranspirationPotential (double value) {cropMaxTranspirationPotential = value;}
	/**
	 * Set the crop species minimum transpiration potential 
	 * @param value The crop species minimum transpiration potential (cm)
	 */
	public void setCropMinTranspirationPotential (double value) {cropMinTranspirationPotential = value ;}
	/**
	 * Set the crop species potential drop needed to enter the root expressed as a % of soil water potential 
	 * @param value The crop species potential drop needed to enter the root expressed as a % of soil water potential (%)
	 */
	public void setCropBufferPotential(double value) {cropBufferPotential = value;}
	/**
	 * Set the crop species longitudinal resistance factor 
	 * @param value The crop species longitudinal resistance factor (mm cm-1 m-1)
	 */
	public void setCropLongitudinalResistantFactor(double value) {cropLongitudinalResistantFactor = value;}
	/**
	 * Set the crop species harmonic weighted mean (unitless)
	 * @param value The crop species harmonic weighted mean (unitless)
	 */
	public void setCropHarmonicWeightedMean(double value) {cropHarmonicWeightedMean = value;}

}


