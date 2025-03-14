/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
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
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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
 * Crop species parameters HISAFE specific
 * STICS usual parameters are in safe.stics.SafeSticsCrop
 *
 * @author Isabelle Lecomte - INRAE Montpellier - July 2002
 */
public class SafeCropSpecies implements Serializable, Cloneable {	// fc - 29.7.2004 - EnumProperty

	private static final long serialVersionUID = 1L;

	private String name; 								//name of crop species
	private String fileName; 							//name of crop species file name

	private double cropRootDiameter;					//cm

	//For calculating the transpiration reduction factor following Campbell
	private double cropAlpha;
	private double cropMinTranspirationPotential;		//cm
	private double cropMaxTranspirationPotential;		//cm

	//Root axial conductance (1/resistance involved in water transport inside the root per unit gradient in water potential and per unit path-length)
	//Unit should be here kg s-1 cm-1, or if the flux is expressed in cm, cm cm-1
	//According to Tyree, root axial conductance is higher for large roots
	private double cropRootConductivity;				//cm cm-1

	//Potential drop needed to enter the root expressed as a % of soil water potential
	private double cropBufferPotential;					//cm

	//Longitudinal resistance factor for root sap
	private double cropLongitudinalResistantFactor;		//mm.cm-1.m-1

	// This parameter indicates the relative influence of dry voxels on the calculation
	// of the averaged soil water potential perceived by the plant
	// When = 1, we use a harmonic average
	public  double cropHarmonicWeightedMean ;
	
	//to SAVE and RESTORE some STICS values that can be erased in perenial chaining years
	public float[] P_stamflax;      // PARAMETER // Sum of development units between the stages AMF and LAX // degree.days // PARPLT // 1
	public float[] P_stlevamf;      // PARAMETER // Sum of development units between the stages LEV and AMF // degree.days // PARPLT // 1
	public float[] P_stlevdrp;      // PARAMETER // Sum of development units between the stages LEV and DRP // degree.days // PARPLT // 1
	public float[] P_stflodrp;      // PARAMETER // phasic duration between FLO and DRP (only for indication) // degr�s.jours // PARPLT // 1
	public float[] P_stlaxsen;      // PARAMETER // Sum of development units between the stages LAX and SEN // degree.days // PARPLT // 1
	public float[] P_stsenlan;      // PARAMETER // Sum of development units between the stages SEN et LAN // degree.days // PARPLT // 1
	public float[] P_stdrpmat;      // PARAMETER // Sum of development units between the stages DRP and MAT // degree.days // PARPLT // 1
	public float[] P_stdrpdes;      // PARAMETER // phasic duration between the DRP stage and the beginning of the water fruit dynamics  // degree.days // PARPLT // 1

	
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

	public String getName () {return name;}
	public String getFileName () {return fileName;}
	public double getCropRootDiameter() {return cropRootDiameter;}
	public double getCropAlpha() {return cropAlpha;}
	public double getCropRootConductivity() {return cropRootConductivity;}
	public double getCropMaxTranspirationPotential() {return cropMaxTranspirationPotential;}
	public double getCropMinTranspirationPotential() {return cropMinTranspirationPotential;}
	public double getCropBufferPotential() {return cropBufferPotential;}
	public double getCropLongitudinalResistantFactor() {return cropLongitudinalResistantFactor;}
	public double getCropHarmonicWeightedMean() {return cropHarmonicWeightedMean;}
	
	public void setName (String v) {name = v;}
	public void setFileName (String v) {fileName = v;}
	public void setCropRootDiameter (double v) {cropRootDiameter = v;}
	public void setCropAlpha (double v) {cropAlpha = v;}
	public void setCropRootConductivity(double v) {cropRootConductivity = v;}
	public void setCropMaxTranspirationPotential (double v) {cropMaxTranspirationPotential = v;}
	public void setCropMinTranspirationPotential (double v) {cropMinTranspirationPotential = v ;}
	public void setCropBufferPotential(double v) {cropBufferPotential = v;}
	public void setCropLongitudinalResistantFactor(double v) {cropLongitudinalResistantFactor = v;}
	public void setCropHarmonicWeightedMean(double v) {cropHarmonicWeightedMean = v;}
	

	/**
	 * return Campbell factor  (dimensionless)
	 * ICRAF method
	 */
	public double getCampbellFactorIcraf() {
		return (2 * Math.log (cropAlpha / (1 - cropAlpha))
				/ Math.log (cropMaxTranspirationPotential / cropMinTranspirationPotential));
	}
	
	/**
	 * return Campbell factor (dimensionless) 
	 * NOT USED
	 */
	public double getCampbellFactor (double plantWaterPotential) {
		double halfCurrWaterPotential= getHalfCurrWaterPotential();
		double a = getA();
		return 1.0/(1.0+Math.pow(plantWaterPotential/halfCurrWaterPotential,a));
	}
	
	/**
	*  return water potential where tranpiration demand is half of its potential
	*  ICRAF method
	*/
	public double getHalfCurrWaterPotentialIcraf() {
			return (cropMaxTranspirationPotential * Math.pow ((1 - cropAlpha) / cropAlpha, 1 / getCampbellFactorIcraf()));
	}

	/**
	 * return water potential where tranpiration demand is half of its potential 
	 * NOT USED
	 */
	public double getHalfCurrWaterPotential() {
			return -Math.sqrt (cropMaxTranspirationPotential * cropMinTranspirationPotential);
	}
	

	public double getA() {
			return (2.0 * Math.log (cropAlpha / (1 - cropAlpha))
					   / Math.log (cropMaxTranspirationPotential / cropMinTranspirationPotential));
	}

}


