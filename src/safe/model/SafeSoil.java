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
import java.util.Arrays;
import java.util.Collection;

/**
 * Soil general description
 *
 * @author Isabelle Lecomte - INRAE Montpellier France - July 2002
 */
public class SafeSoil implements Serializable {

	private static final long serialVersionUID = 1L;
	//STICS parametres
	private double humificationDepth;	// equivalent depth of humification (m) between depth of tillage and 0.6 m
	private double organicNitrogen;		// organic nitrogen content (%) in moisture soil horizon
	private double albedo;				// albedo of bare soil in dry state (0-1)
	private double evaporationValue;	// evaporation value (mm) at the end of maximum evaporation stage
	private double rainRunOffFraction;	// rainwater run-off (compared with total rainfall) under bare soil conditions
	private double cropRootObstruction;	// Obstruction to crop roots (m)
	private double minNh4Concentration;	// minimum soil concentration in NH3 (kgN ha-1 mm-1)
	private double ph;					// ph

	//Option: Capillary uptake
	private boolean capillary;
	private double capillaryUptake;				// (mm j-1)
	private double capillaryUptakeMinWater;		// min water to activate capillary uptake (g water/g soil)

	//Option: Drainage
	private boolean artificialDrainage;			
	private double impermeableLayerDepth;		// m
	private double drainagePipesSpacing; 		// m
	private double drainagePipesDepth;			// m
	private double waterConductivity; 			// water conductivity to saturation for water transport towards drainage pipes (cm j-1)

	//Other options
	private boolean swellingClaySoil;    //option: creation of supplementary compartment in water balance for swelling clay soils
	private boolean macroporosity;
	private boolean nitrification;
	private boolean denitrification;


	//Stics V8 new parameters
	private double soilCrustRainMin;  				//minimal rain quantity for the crust occurrence (pluiebat) // mm day-1 
	private double soilCrustDepth;   				// mulch depth from which a crust occurs (mulchbat) // 0 
	private double evaporationMaxDepth;  			// maximal depth of soil affected by soil evaporation (zesx) // cm 
	private double evaporationDepthContribution;    // soil contribution to evaporation as a function of depth (cfes)  // SD
	private double roughnessLength;  				// roughness length of bare soil (z0solnu) // cm 
	private double denitrificationRate;   			// potential rate of denitrification per 1 cm soil layer (vpotdenit)// kg ha-1 j-1 cm-1  
	private double denitrificationDepth;  			// soil depth on which denitrification is active with the appropriate option (profdenit) // cm 
	private double soilHumusCN;   					// Initial C to N ratio of soil humus (CsurNsol) // SD 
	private double runOffCoefPlantMulch;    		// runoff coefficient taking account for plant mulch (penterui) // SD  
		
	//Layers
	private SafeLayer [] layers;		//layer for soil properties
	private int nbVoxels;				//number of soil voxels per cell (in case it is different of layer number)
	private double depth;				//total soil depth (m)
	private double volume;				//total soil volume (m3)

	//Water Table
	private boolean waterTable;			//is there a water table ?


	public SafeSoil (SafeGeneralParameters settings, SafePlotSettings plotSettings) {

		this.humificationDepth = plotSettings.humificationDepth;
		this.organicNitrogen = plotSettings.organicNitrogen;
		this.albedo = plotSettings.albedo;
		this.evaporationValue = plotSettings.evaporationValue;
		this.rainRunOffFraction = plotSettings.rainRunOffFraction;
		this.cropRootObstruction = plotSettings.cropRootObstruction;
		this.minNh4Concentration =  plotSettings.minNh4Concentration;
		this.ph =  plotSettings.ph;
		this.capillary = plotSettings.capillary;
		this.capillaryUptake =  plotSettings.capillaryUptake;
		this.capillaryUptakeMinWater =  plotSettings.capillaryUptakeMinWater;
		this.artificialDrainage = plotSettings.artificialDrainage;
		this.impermeableLayerDepth = plotSettings.impermeableLayerDepth;
		this.drainagePipesSpacing = plotSettings.drainagePipesSpacing;
		this.drainagePipesDepth = plotSettings.drainagePipesDepth;
		this.waterConductivity = plotSettings.waterConductivity;
		this.swellingClaySoil = plotSettings.swellingClaySoil;
		this.macroporosity = plotSettings.macroporosity;
		this.nitrification = plotSettings.nitrification;
		this.denitrification = plotSettings.denitrification;
        this.soilCrustRainMin = plotSettings.soilCrustRainMin;
        this.soilCrustDepth  = plotSettings.soilCrustDepth;
        this.evaporationMaxDepth = plotSettings.evaporationMaxDepth;    
        this.evaporationDepthContribution   = plotSettings.evaporationDepthContribution;
        this.roughnessLength =  plotSettings.roughnessLength;
        this.denitrificationRate  =  plotSettings.denitrificationRate;
        this.denitrificationDepth = plotSettings.denitrificationDepth;
        this.soilHumusCN = plotSettings.soilHumusCN;
        this.runOffCoefPlantMulch = plotSettings.runOffCoefPlantMulch;
		this.waterTable = plotSettings.waterTable;

		// layer of soil MAX and flexible number of voxels
		layers = new SafeLayer[settings.NB_LAYER_MAX];
	}


	public int getNbVoxels () {return nbVoxels;}
	public double getHumificationDepth () {return humificationDepth;}
	public double getOrganicNitrogen  () {return organicNitrogen;}
	public double getAlbedo () {return albedo;}
	public double getEvaporationValue () {return evaporationValue;}
	public double getRainRunOffFraction() {return rainRunOffFraction;}
	public double getCropRootObstruction () {return cropRootObstruction;}
	public double getMinNh4Concentration () {return minNh4Concentration;}
	public double getPh () {return ph;}
	public double getDepth () {return depth;}
	public boolean getCapillary () {return capillary;}
	public double getCapillaryUptake () {return capillaryUptake;}
	public double getCapillaryUptakeMinWater () {return capillaryUptakeMinWater;}
	public boolean getArtificialDrainage () {return artificialDrainage;}
	public double getImpermeableLayerDepth () {return impermeableLayerDepth;}
	public double getDrainagePipesSpacing () {return drainagePipesSpacing;}
	public double getDrainagePipesDepth() {return drainagePipesDepth;}
	public double getWaterConductivity() {return waterConductivity;}
	public boolean getSwellingClaySoil () {return swellingClaySoil;}
	public boolean getMacroporosity () {return macroporosity;}
	public boolean getNitrification () {return nitrification;}
	public boolean getDenitrification () {return denitrification;}
	public double getSoilCrustRainMin () {return soilCrustRainMin;}
	public double getSoilCrustDepth () {return soilCrustDepth;}
	public double getEvaporationMaxDepth () {return evaporationMaxDepth;}
	public double getEvaporationDepthContribution() {return evaporationDepthContribution;}
	public double getRoughnessLength () {return roughnessLength;}
	public double getDenitrificationRate () {return denitrificationRate;}
	public double getDenitrificationDepth() {return denitrificationDepth;}
	public double getSoilHumusCN () {return soilHumusCN;}
	public double getRunOffCoefPlantMulch () {return runOffCoefPlantMulch;}
	public boolean isWaterTable () {return waterTable;}
	public SafeLayer getLayer  (int i) {return  layers[i];}
	public Collection getLayers  () {return  Arrays.asList(layers);}	//Just for testing
	public void putLayer (int i, SafeLayer layer) {layers[i] = layer;}
	public void addDepth (double v) {depth +=  v;}
	public void setVolume (double v) {volume = v;}
	public double getVolume () {return volume;}
	public void setNbVoxels (int i) {nbVoxels = i;}
}