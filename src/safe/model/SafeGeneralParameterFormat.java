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

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;
import safe.stics.*;

/**
 * GENERAL parameters format for reading in a file 
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeGeneralParameterFormat extends RecordSet {

	/**
	 * Construtor
	 * @param filePath Path to the file containing the general parameters
	 */
	public SafeGeneralParameterFormat (String filePath) throws Exception {
		prepareImport (filePath);}

	/**
	 * Load general parameters
	 * @param generalParameters Reference to the SafeGeneralParameters object containing general parameters
	 * @param sticsStation Reference to the SafeSticsStation object containing stics weather station parameters
	 */
	public  void load (SafeGeneralParameters generalParameters, SafeSticsStation sticsStation) throws Exception {

		Set<String> requiredParameters = new HashSet<>();

		requiredParameters.add("diffuseCoeffA");
		requiredParameters.add("diffuseCoeffB");
		requiredParameters.add("SOC");
		requiredParameters.add("UOC");
		requiredParameters.add("turtleOption");
		requiredParameters.add("timeStep");
		requiredParameters.add("nbTimeStepMax");
		requiredParameters.add("diffuseAngleStep");
		requiredParameters.add("declinationThreshold");
		requiredParameters.add("leafAreaThreshold");
		requiredParameters.add("nbImpactMultiplication");
		requiredParameters.add("parGlobalCoefficient");
		requiredParameters.add("molesParCoefficient");
		requiredParameters.add("aangst");
		requiredParameters.add("bangst");
		requiredParameters.add("priestleyTaylorCoeff");
		requiredParameters.add("stefanBoltzmanConstant");
		requiredParameters.add("psychrometricConstant");
		requiredParameters.add("integrationStep");
		requiredParameters.add("maxPhiPF");
		requiredParameters.add("maxTempSnow");
		requiredParameters.add("minTempSnow");
		requiredParameters.add("maxDailySnowMelt");
		requiredParameters.add("maxTempSnowMelt");	
		requiredParameters.add("minTempSnowMelt");							
		requiredParameters.add("nitrogenDiffusionConstant");													
		requiredParameters.add("nitrogenEffectiveDiffusionA0");												
		requiredParameters.add("nitrogenEffectiveDiffusionA1");										
		requiredParameters.add("no3AbsorptionConstant");
		requiredParameters.add("nh4AbsorptionConstant");													
		requiredParameters.add("no3Fraction");
		requiredParameters.add("fmin1");
		requiredParameters.add("fmin2");
		requiredParameters.add("fmin3");
		requiredParameters.add("zr");														
		requiredParameters.add("NH3ref");												
		requiredParameters.add("patm");												
		requiredParameters.add("aclim");												
		requiredParameters.add("codeetp");												
		requiredParameters.add("codeclichange");												
		requiredParameters.add("codaltitude");											
		requiredParameters.add("altistation");												
		requiredParameters.add("gradtn");												
		requiredParameters.add("gradtx");												
		requiredParameters.add("altinversion");												
		requiredParameters.add("gradtninv");												
		requiredParameters.add("cielclair");												
		requiredParameters.add("codadret");												
		requiredParameters.add("ombragetx");												
		requiredParameters.add("ra");												
		requiredParameters.add("albveg");												
		requiredParameters.add("corecTrosee");												
		requiredParameters.add("codecaltemp");												
		requiredParameters.add("codernet");												
		requiredParameters.add("coefdevil");												
		requiredParameters.add("aks");												
		requiredParameters.add("bks");												
		requiredParameters.add("cvent");												
		requiredParameters.add("phiv0");												
		requiredParameters.add("coefrnet");	
		requiredParameters.add("leavesResiduesSpreading");
		requiredParameters.add("branchesResiduesSpreading");
		requiredParameters.add("sticsWaterExtractionForCrop");
		requiredParameters.add("waterStressMin");	
		requiredParameters.add("nitrogenStressMin");
		
		
		
		for (Iterator<Record> i = this.iterator (); i.hasNext ();) {
			Record record =  i.next ();

		 	if (record instanceof SafeGeneralParameterFormat.KeyRecord) {

		 		SafeGeneralParameterFormat.KeyRecord r = (SafeGeneralParameterFormat.KeyRecord) record;

				if (r.key.equals ("diffuseCoeffA")) {
					generalParameters.diffuseCoeffA = r.getDoubleValue ();
					requiredParameters.remove("diffuseCoeffA");
					
				} else if  (r.key.equals ("diffuseCoeffB")) {
					generalParameters.diffuseCoeffB  = r.getDoubleValue ();
					requiredParameters.remove("diffuseCoeffB");
					
				} else if  (r.key.equals ("parGlobalCoefficient")) {
					generalParameters.parGlobalCoefficient  = r.getDoubleValue ();
					requiredParameters.remove("parGlobalCoefficient");
					
				} else if  (r.key.equals ("molesParCoefficient")) {
					generalParameters.molesParCoefficient  = r.getDoubleValue ();
					requiredParameters.remove("molesParCoefficient");
					
				} else if  (r.key.equals ("aangst")) {
					generalParameters.aangst  = r.getDoubleValue ();
					sticsStation.P_aangst = (float) generalParameters.aangst;
					requiredParameters.remove("aangst");
					
				} else if  (r.key.equals ("bangst")) {
					generalParameters.bangst  = r.getDoubleValue ();
					sticsStation.P_bangst = (float) generalParameters.bangst;
					requiredParameters.remove("bangst");
					
				} else if  (r.key.equals ("priestleyTaylorCoeff")) {
					generalParameters.priestleyTaylorCoeff  = r.getDoubleValue ();
					sticsStation.P_alphapt = (float) generalParameters.priestleyTaylorCoeff;
					requiredParameters.remove("priestleyTaylorCoeff");
					
				} else if  (r.key.equals ("stefanBoltzmanConstant")) {
					generalParameters.stefanBoltzmanConstant  = r.getDoubleValue ();
					requiredParameters.remove("stefanBoltzmanConstant");
					
				} else if (r.key.equals ("psychrometricConstant")) {
					generalParameters.psychrometricConstant  = r.getDoubleValue ();	
					requiredParameters.remove("psychrometricConstant");
								
				} else if (r.key.equals ("timeStep")) {
					generalParameters.timeStep  = r.getDoubleValue ();
					requiredParameters.remove("timeStep");
					
				} else if (r.key.equals ("nbTimeStepMax")) {
					generalParameters.nbTimeStepMax  = r.getIntValue ();	
					requiredParameters.remove("nbTimeStepMax");
					
				} else if (r.key.equals ("diffuseAngleStep")) {
					generalParameters.diffuseAngleStep  = r.getDoubleValue ();
					requiredParameters.remove("diffuseAngleStep");
					
				} else if (r.key.equals ("declinationThreshold")) {
					generalParameters.declinationThreshold  = r.getDoubleValue ();
					requiredParameters.remove("declinationThreshold");
					
				} else if (r.key.equals ("leafAreaThreshold")) {
					generalParameters.leafAreaThreshold  = r.getDoubleValue ();	
					requiredParameters.remove("leafAreaThreshold");
					
				} else if (r.key.equals ("nbImpactMultiplication")) {
					generalParameters.nbImpactMultiplication  = r.getIntValue ();
					requiredParameters.remove("nbImpactMultiplication");
						
				} else if (r.key.equals("SOC")) {
					int ri = r.getIntValue();
					generalParameters.SOC = true;
					if (ri == 0) generalParameters.SOC = false;
					if (ri == 1) generalParameters.SOC = true;
					requiredParameters.remove("SOC");
					
				} else if (r.key.equals("UOC")) {
					int ri = r.getIntValue();
					generalParameters.UOC = false;
					if (ri == 0) generalParameters.UOC = false;
					if (ri == 1) generalParameters.UOC = true;	
					requiredParameters.remove("UOC");
					
				} else if (r.key.equals("turtleOption")) {
					int ri = r.getIntValue();
					generalParameters.turtleOption = false;
					if (ri == 0) generalParameters.turtleOption = false;	
					if (ri == 1) generalParameters.turtleOption = true;	
					requiredParameters.remove("turtleOption");
					
				} else if (r.key.equals("hisafeLightMethodForCrop")) {
					int ri = r.getIntValue();
					generalParameters.hisafeLightMethodForCrop = false;
					if (ri == 0) generalParameters.hisafeLightMethodForCrop = false;
					if (ri == 1) generalParameters.hisafeLightMethodForCrop = true;
					
					
				} else if  (r.key.equals ("integrationStep")) {
					generalParameters.integrationStep  = r.getDoubleValue ();
					requiredParameters.remove("integrationStep");
					
				} else if  (r.key.equals ("maxPhiPF")) {
					generalParameters.maxPhiPF  = r.getDoubleValue ();
					requiredParameters.remove("maxPhiPF");
				
				} else if  (r.key.equals ("maxTempSnow")) {
					generalParameters.maxTempSnow  = r.getDoubleValue ();
					requiredParameters.remove("maxTempSnow");
					
				} else if  (r.key.equals ("minTempSnow")) {
					generalParameters.minTempSnow  = r.getDoubleValue ();
					requiredParameters.remove("minTempSnow");
					
				} else if  (r.key.equals ("maxDailySnowMelt")) {
					generalParameters.maxDailySnowMelt  = r.getDoubleValue ();
					requiredParameters.remove("maxDailySnowMelt");
					
				} else if  (r.key.equals ("maxTempSnowMelt")) {
					generalParameters.maxTempSnowMelt  = r.getDoubleValue ();
					requiredParameters.remove("maxTempSnowMelt");
					
				} else if  (r.key.equals ("minTempSnowMelt")) {
					generalParameters.minTempSnowMelt  = r.getDoubleValue ();
					requiredParameters.remove("minTempSnowMelt");
					
				} else if (r.key.equals("sticsWaterExtractionForCrop")) {
					int ri = r.getIntValue();
					generalParameters.sticsWaterExtractionForCrop = false;
					if (ri == 1) generalParameters.sticsWaterExtractionForCrop = true;	
					requiredParameters.remove("sticsWaterExtractionForCrop");

				} else if  (r.key.equals ("waterStressMin")) {
					generalParameters.waterStressMin  = r.getDoubleValue ();
					requiredParameters.remove("waterStressMin");

				} else if  (r.key.equals ("nitrogenStressMin")) {
					generalParameters.nitrogenStressMin  = r.getDoubleValue ();
					requiredParameters.remove("nitrogenStressMin");
				}

				else if (r.key.equals ("codecaltemp")) {
					sticsStation.P_codecaltemp = r.getIntValue ();
					requiredParameters.remove("codecaltemp");
					
				} else if (r.key.equals ("codernet")) {
					sticsStation.P_codernet = r.getIntValue ();	
					requiredParameters.remove("codernet");
					
				} else if (r.key.equals ("codeclichange")) {
					sticsStation.P_codeclichange = r.getIntValue ();	
					requiredParameters.remove("codeclichange");
					
				} else if (r.key.equals ("zr")) {
					sticsStation.P_zr = r.getFloatValue ();
					requiredParameters.remove("zr");
					
				} else if (r.key.equals ("ra")) {
					sticsStation.P_ra = r.getFloatValue ();
					requiredParameters.remove("ra");
					
				} else if (r.key.equals ("NH3ref")) {
					sticsStation.P_NH3ref = r.getFloatValue ();
					requiredParameters.remove("NH3ref");

				} else if (r.key.equals ("coefdevil")) {
					sticsStation.P_coefdevil = r.getFloatValue ();
					requiredParameters.remove("coefdevil");
					
				} else if (r.key.equals ("albveg")) {
					sticsStation.P_albveg = r.getFloatValue ();
					requiredParameters.remove("albveg");
					
				} else if (r.key.equals ("altistation")) {
					sticsStation.P_altistation = r.getFloatValue ();
					requiredParameters.remove("altistation");
					
				} else if (r.key.equals ("gradtn")) {
					sticsStation.P_gradtn = r.getFloatValue ();
					requiredParameters.remove("gradtn");
					
				} else if (r.key.equals ("gradtx")) {
					sticsStation.P_gradtx = r.getFloatValue ();
					requiredParameters.remove("gradtx");
					
				} else if (r.key.equals ("altinversion")) {
					sticsStation.P_altinversion = r.getFloatValue ();
					requiredParameters.remove("altinversion");
					
				} else if (r.key.equals ("gradtninv")) {
					sticsStation.P_gradtninv = r.getFloatValue ();
					requiredParameters.remove("gradtninv");
					
				} else if (r.key.equals ("cielclair")) {
					sticsStation.P_cielclair = r.getFloatValue ();
					requiredParameters.remove("cielclair");
					
				} else if (r.key.equals ("ombragetx")) {
					sticsStation.P_ombragetx = r.getFloatValue ();
					requiredParameters.remove("ombragetx");
					
				} else if (r.key.equals ("aks")) {
					sticsStation.P_aks = r.getFloatValue ();
					requiredParameters.remove("aks");
					
				} else if (r.key.equals ("bks")) {
					sticsStation.P_bks = r.getFloatValue ();
					requiredParameters.remove("bks");
					
				} else if (r.key.equals ("cvent")) {
					sticsStation.P_cvent = r.getFloatValue ();
					requiredParameters.remove("cvent");
					
				} else if (r.key.equals ("phiv0")) {
					sticsStation.P_phiv0 = r.getFloatValue ();
					requiredParameters.remove("phiv0");
					
				} else if (r.key.equals ("coefrnet")) {
					sticsStation.P_coefrnet = r.getFloatValue ();	
					requiredParameters.remove("coefrnet");
					
				} else if (r.key.equals ("patm")) {
					sticsStation.P_patm = r.getFloatValue ();
					requiredParameters.remove("patm");
					
				} else if (r.key.equals ("corecTrosee")) {
					sticsStation.P_corecTrosee = r.getFloatValue ();
					requiredParameters.remove("corecTrosee");
					
				} else if (r.key.equals ("codeetp")) {
					sticsStation.P_codeetp = r.getIntValue ();
					requiredParameters.remove("codeetp");
			
				} else if (r.key.equals ("codaltitude")) {
					sticsStation.P_codaltitude = r.getIntValue ();
					requiredParameters.remove("codaltitude");
					
				} else if (r.key.equals ("codadret")) {
					sticsStation.P_codadret = r.getIntValue ();
					requiredParameters.remove("codadret");
					
				} else if (r.key.equals ("aclim")) {
					sticsStation.P_aclim = r.getFloatValue ();					
					requiredParameters.remove("aclim");	
				
				} else if (r.key.equals ("nitrogenDiffusionConstant")) {
					generalParameters.nitrogenDiffusionConstant = r.getDoubleValue ();
					requiredParameters.remove("nitrogenDiffusionConstant");
					
				} else if (r.key.equals ("nitrogenEffectiveDiffusionA0")) {
					generalParameters.nitrogenEffectiveDiffusionA0 = r.getDoubleValue ();
					requiredParameters.remove("nitrogenEffectiveDiffusionA0");
					
				} else if (r.key.equals ("nitrogenEffectiveDiffusionA1")) {
					generalParameters.nitrogenEffectiveDiffusionA1 = r.getDoubleValue ();
					requiredParameters.remove("nitrogenEffectiveDiffusionA1");
					
				} else if (r.key.equals ("no3AbsorptionConstant")) {
					generalParameters.no3AbsorptionConstant = r.getDoubleValue ();
					requiredParameters.remove("no3AbsorptionConstant");
					
				} else if (r.key.equals ("nh4AbsorptionConstant")) {
					generalParameters.nh4AbsorptionConstant = r.getDoubleValue ();
					requiredParameters.remove("nh4AbsorptionConstant");
					
				} else if (r.key.equals ("no3Fraction")) {
					generalParameters.no3Fraction = r.getDoubleValue ();	
					requiredParameters.remove("no3Fraction");
					
				} else if (r.key.equals ("fmin1")) {
					generalParameters.fmin1 = r.getDoubleValue ();
					requiredParameters.remove("fmin1");
					
				} else if (r.key.equals ("fmin2")) {
					generalParameters.fmin2 = r.getDoubleValue ();
					requiredParameters.remove("fmin2");
					
				} else if (r.key.equals ("fmin3")) {
					generalParameters.fmin3 = r.getDoubleValue ();
					requiredParameters.remove("fmin3");
					
				} else if (r.key.equals ("leavesResiduesSpreading")) {
					generalParameters.leavesResiduesSpreading =  r.getIntValue ();
					requiredParameters.remove("leavesResiduesSpreading");
					
				} else if (r.key.equals ("branchesResiduesSpreading")) {
					generalParameters.branchesResiduesSpreading =  r.getIntValue ();
					requiredParameters.remove("branchesResiduesSpreading");
				}
		 	}
		}
		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing general parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort

		}

	}
}
