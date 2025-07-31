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
import java.util.Arrays;
import java.util.Collection;

/**
 * SOIL general description
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeSoil implements Serializable {

	/** Layers table  */
	private SafeLayer [] layers;		
	/** number of soil voxels per cell  */
	private int nbVoxels;				
	/** Total soil depth (m) */
	private double depth;				
	/** Total soil volume (m3)  */
	private double volume;			
	/** Voxel thickness max (m) */
	private double voxelThicknessMax;	
	
	/** Depth of humification (m)  */
	private double humificationDepth;	
	/** Organic nitrogen content (%) in moisture soil horizon   */
	private double organicNitrogen;		
	/** Albedo of bare soil in dry state   */
	private double albedo;				
	/** Evaporation value (mm) at the end of maximum evaporation stage   */
	private double evaporationValue;	// 
	/** Rainwater run-off (compared with total rainfall) under bare soil conditions   */
	private double rainRunOffFraction;	
	/** Obstruction to crop roots (m)  */
	private double cropRootObstruction;	
	/** Minimum soil concentration in NH3 (kgN ha-1 mm-1)   */
	private double minNh4Concentration;	
	/** PH of mixing soil + organic amendments  */
	private double ph;			
	/** Minimal rain quantity for the crust occurrence (mm day-1)  */
	private double soilCrustRainMin;  				
	/** Mulch depth from which a crust occurs (cm)  */
	private double soilCrustDepth;   			
	/** Maximal depth of soil affected by soil evaporation (cm)   */
	private double evaporationMaxDepth;  		
	/** Soil contribution to evaporation as a function of depth  */
	private double evaporationDepthContribution;   
	/** Roughness length of bare soil (cm)   */
	private double roughnessLength;  			
	/** Potential rate of denitrification per 1 cm soil layer (kg ha-1 j-1 cm-1)  */
	private double denitrificationRate;   			
	/** Soil depth on which denitrification is active with the appropriate option (cm)   */
	private double denitrificationDepth;  			
	/** Initial C to N ratio of soil humus   */
	private double soilHumusCN;   				
	/** runoff coefficient taking account for plant mulch   */
	private double runOffCoefPlantMulch;    	
			
	//OPTIONS
	/** Option to activate capillary rise   */
	private boolean capillary;
	/** Capillary rise value upward water flux (mm d-1)   */
	private double capillaryUptake;				
	/** Min water to activate capillary uptake (g water/g soil)   */
	private double capillaryUptakeMinWater;		
	/** Option to activate artificial drainage   */
	private boolean artificialDrainage;			
	/** Upper depth of the impermeable layer (from the soil surface) (m)  */
	private double impermeableLayerDepth;		
	/** In between drains distance (m)   */
	private double drainagePipesSpacing; 		
	/** Drain depth (m)   */
	private double drainagePipesDepth;		
	/**  water conductivity to saturation for water transport towards drainage pipes (cm j-1)   */
	private double waterConductivity; 		
	/** Option to activate swelling clay soils   */
	private boolean swellingClaySoil;    
	/** Option to activate water flux in the macroporosity of soils to estimate water excess and drip by  overflowing  */
	private boolean macroporosity;
	/** Option to activate calculation of nitrification   */
	private boolean nitrification;
	/** Option to activate calculation of denitrification   */
	private boolean denitrification;
	/** Option to activate water table   */
	private boolean waterTable;		
	/** Default no3 concentration in water table (g l-1)  */
	private double no3ConcentrationInWaterTable;		
	/** Default nh4 concentration in water table (g l-1) */
	private double nh4ConcentrationInWaterTable;
	
	public SafeSoil () {}


	public int getNbVoxels () {return nbVoxels;}
	public void setHumificationDepth (double v) {humificationDepth = v;}
	public double getHumificationDepth () {return humificationDepth;}
	public void setOrganicNitrogen (double v) {organicNitrogen = v;}
	public double getOrganicNitrogen  () {return organicNitrogen;}
	public void setAlbedo(double v) {albedo = v;}
	public double getAlbedo () {return albedo;}
	public void setEvaporationValue(double v) {evaporationValue = v;}
	public double getEvaporationValue () {return evaporationValue;}
	public void setRainRunOffFraction(double v) {rainRunOffFraction = v;}
	public double getRainRunOffFraction() {return rainRunOffFraction;}
	public void setCropRootObstruction(double v) {cropRootObstruction = v;}
	public double getCropRootObstruction () {return cropRootObstruction;}
	public void setMinNh4Concentration(double v) {minNh4Concentration = v;}
	public double getMinNh4Concentration () {return minNh4Concentration;}
	public void setPh(double v) {ph = v;}
	public double getPh () {return ph;}
	public void setDepth(double v) {depth = v;}
	public double getDepth () {return depth;}
	public void setCapillary(boolean v) {capillary = v;}
	public boolean getCapillary () {return capillary;}
	public void setCapillaryUptake(double v) {capillaryUptake = v;}
	public double getCapillaryUptake () {return capillaryUptake;}
	public void setCapillaryUptakeMinWater(double v) {capillaryUptakeMinWater = v;}
	public double getCapillaryUptakeMinWater () {return capillaryUptakeMinWater;}
	public void setArtificialDrainage(boolean v) {artificialDrainage = v;}
	public boolean getArtificialDrainage () {return artificialDrainage;}
	public void setImpermeableLayerDepth(double v) {impermeableLayerDepth = v;}
	public double getImpermeableLayerDepth () {return impermeableLayerDepth;}
	public void setDrainagePipesSpacing(double v) {drainagePipesSpacing = v;}
	public double getDrainagePipesSpacing () {return drainagePipesSpacing;}
	public void setDrainagePipesDepth(double v) {drainagePipesDepth = v;}
	public double getDrainagePipesDepth() {return drainagePipesDepth;}
	public void setWaterConductivity(double v) {waterConductivity = v;}
	public double getWaterConductivity() {return waterConductivity;}
	public void setSwellingClaySoil(boolean v) {swellingClaySoil = v;}
	public boolean getSwellingClaySoil () {return swellingClaySoil;}
	public void setMacroporosity(boolean v) {macroporosity = v;}
	public boolean getMacroporosity () {return macroporosity;}
	public void setNitrification(boolean v) {nitrification = v;}
	public boolean getNitrification () {return nitrification;}
	public void setDenitrification(boolean v) {denitrification = v;}
	public boolean getDenitrification () {return denitrification;}
	public void setSoilCrustRainMin(double v) {soilCrustRainMin = v;}
	public double getSoilCrustRainMin () {return soilCrustRainMin;}
	public void setSoilCrustDepth(double v) {soilCrustDepth = v;}
	public double getSoilCrustDepth () {return soilCrustDepth;}
	public void setEvaporationMaxDepth(double v) {evaporationMaxDepth = v;}
	public double getEvaporationMaxDepth () {return evaporationMaxDepth;}
	public void setEvaporationDepthContribution(double v) {evaporationDepthContribution = v;}
	public double getEvaporationDepthContribution() {return evaporationDepthContribution;}
	public void setRoughnessLength(double v) {roughnessLength = v;}
	public double getRoughnessLength () {return roughnessLength;}
	public void setDenitrificationRate(double v) {denitrificationRate = v;}
	public double getDenitrificationRate () {return denitrificationRate;}
	public void setDenitrificationDepth(double v) {denitrificationDepth = v;}
	public double getDenitrificationDepth() {return denitrificationDepth;}
	public void setSoilHumusCN(double v) {soilHumusCN = v;}
	public double getSoilHumusCN () {return soilHumusCN;}
	public void setRunOffCoefPlantMulch(double v) {runOffCoefPlantMulch = v;}
	public double getRunOffCoefPlantMulch () {return runOffCoefPlantMulch;}
	public void setWaterTable(boolean v) {waterTable = v;}
	public boolean isWaterTable () {return waterTable;}
	
	public void setNo3ConcentrationInWaterTable(double v) {no3ConcentrationInWaterTable = v;}
	public double getNo3ConcentrationInWaterTable () {return no3ConcentrationInWaterTable;}
	
	public void setNh4ConcentrationInWaterTable(double v) {nh4ConcentrationInWaterTable = v;}
	public double getNh4ConcentrationInWaterTable () {return nh4ConcentrationInWaterTable;}
	
	public void setVoxelThicknessMax(double v) {voxelThicknessMax = v;}
	public double getVoxelThicknessMax() {return voxelThicknessMax;}
	
	public void setLayers (SafeLayer[] layer) {layers = layer;}
	
	public SafeLayer getLayer  (int i) {return  layers[i];}
	public int getNbLayers  () {return  layers.length;}
	public Collection getLayers  () {return  Arrays.asList(layers);}	//Just for testing
	public void putLayer (int i, SafeLayer layer) {layers[i] = layer;}
	public void addDepth (double v) {depth +=  v;}
	public void setVolume (double v) {volume = v;}
	public double getVolume () {return volume;}
	public void setNbVoxels (int i) {nbVoxels = i;}
}