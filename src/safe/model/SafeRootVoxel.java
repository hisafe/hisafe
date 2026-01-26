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

import java.io.Serializable;


/**
 * Temporary object to store and sort data attached to a voxel and a plant
 * This is used by waterRepartion ICRAF module
 *
 * @author Isabelle Lecomte - INRAE Montpellier France - November 2004
 */
public class SafeRootVoxel implements Serializable, Comparable   {

	private static final long serialVersionUID = 1L;
	private SafePlantRoot plantRoots;				//plant (tree or crop) fine root reference address
	private SafeVoxel voxel;					//rooted voxel reference address
	private double rootDensity;					//plant (tree or crop) root density (m m-3)
	private double waterRhizospherePotential;	//Voxel rhizosphere water potential (cm)
	private double phiPf;						//cm2 day-1
	private double waterUptakePotential;			//liters
	private double nitrogenShareUptake;			//ND
	private double nitrogenZeroSinkPotential;	//g
	private double nitrogenUptakePotential;		//g

	public SafeRootVoxel (SafePlantRoot root, SafeVoxel voxel, double rootDensity, double rhizospherePotential, double phiPf) {
		this.plantRoots = root;
		this.voxel = voxel;
		this.rootDensity =  rootDensity;
		this.waterRhizospherePotential= rhizospherePotential;
		this.phiPf= phiPf;
		this.waterUptakePotential = 0;
		this.nitrogenShareUptake = 0;
		this.nitrogenZeroSinkPotential  = 0;
		this.nitrogenUptakePotential = 0;
	}

	public SafePlantRoot getPlantRoots () {return plantRoots;}
	public SafeVoxel getVoxel () {return voxel;}
	public double getRootDensity () {return  rootDensity;}
	public double getWaterRhizospherePotential () {return waterRhizospherePotential;}
	public double getPhiPf () {return (double) phiPf;}
	public double getWaterUptakePotential () {return waterUptakePotential;}
	public void setWaterUptakePotential (double v) {waterUptakePotential =  v;}
	public void addWaterUptakePotential (double v) {waterUptakePotential +=  v;}


	public double getNitrogenShareUptake() {return nitrogenShareUptake;}
	public double getNitrogenZeroSinkPotential () {return  nitrogenZeroSinkPotential;}
	public double getNitrogenUptakePotential () {return  nitrogenUptakePotential;}
	public void setNitrogenShareUptake (double v) {nitrogenShareUptake =   v;}
	public void setNitrogenZeroSinkPotential (double v) {nitrogenZeroSinkPotential =   v;}
	public void addNitrogenUptakePotential (double v) {nitrogenUptakePotential +=   v;}


	public int compareTo (Object other) {
	  double nombre1 = ((SafeRootVoxel) other).getPhiPf();
	  double nombre2 = this.getPhiPf();
	  if (nombre1 > nombre2)  return -1;		// gt - 12.11.2009 - ">" instead of "<" : sort from low phipf to large phipf instead of the contrary
	  else if(nombre1 == nombre2) return 0;
	  else return 1;
	}

	public String toString(){
		String str = "";
		str = "RootVoxel  voxel="+voxel.getId()+" z="+voxel.getZ()+" fR="+rootDensity+" phiPf="+phiPf+" Rhizo="+waterRhizospherePotential;
		return str;
	}
	
}
