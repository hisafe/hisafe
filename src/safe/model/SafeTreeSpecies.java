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

import capsis.defaulttype.Species;

/**
 * TREE SPECIES parameters
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */

public class SafeTreeSpecies implements  Species, Serializable, Cloneable {	// fc - 29.7.2004 + fc - 15.11.2004 - Species
	
	/** SafeTreeSpecies name */
	private String name; 						//name of tree species name
	/** SafeTreeSpecies file name */
	private String fileName; 					//name of tree species file name
	
	//ALLOMETRY
	/** Crown shape 1=ellipsoid 2=paraboloid */
	private int crownShape;						
	/** Ratio for truncated ellipsoid (0=full ellipsoid) */
	private double ellipsoidTruncationRatio;	
	/** Height/Dbh allometric relationship coefficient */
	private double heightDbhAllometricCoeffA;	
	/** Height/Dbh allometric relationship coefficient */
	private double heightDbhAllometricCoeffB;
	/** Crown/Dbh allometric relationship coefficient */
	private double crownDbhAllometricCoeffA;
	/** Crown/Dbh allometric relationship coefficient */
	private double crownDbhAllometricCoeffB;
	/** Stem/Dbh allometric relationship coefficient */
	private double stemDbhAllometricCoeffA;		
	/** Stem/Dbh allometric relationship coefficient */
	private double stemDbhAllometricCoeffB;
	/** Stem/Dbh allometric relationship coefficient */
	private double stemDbhAllometricCoeffC;
	/** Dbc/Dbh allometric relationship coefficient */
	private double dcbFromDbhAllometricCoeff;
	/** Stump/Stem biomass ratio */
	private double stumpToStemBiomassRatio;
	/** Coarse root area to fine root length ratio */
	private double cRAreaToFRLengthRatio;
	/** Initial leaf to FineRoots ratio  */
	private double initialTargetLfrRatio;	
	/** LeafArea/CrownVol allometric relationship coefficient */
	private double leafAreaCrownVolCoefA;
	/** LeafArea/CrownVol allometric relationship coefficient */
	private double leafAreaCrownVolCoefB;
	/** Leaf dry mass per unit leaf area (kg m-2) */
	private double leafMassArea;		// 
	/** Leaf carbon content (g C g total dry biomass) */
	private double leafCarbonContent;	//
	/** Average branch and stem density (kg m-3) */
	private double woodDensity; 		//
	/** Assuming a fixed ratio of branch volume to crown volume. (cm3 cm-3) */
	private double branchVolumeRatio;	//
	/** proportion of C in wood dry yield % */
	private double woodCarbonContent;   
	
	/** Maximum daily height increment (m)*/
	public  double maxHeightInc; 
	/** Maximum daily dbh increment (m) */
	public  double maxDbhInc;
	/** Maximum daily crown radius increment (m) */
	public  double maxCrownRadiusInc;
	
	//PHENOLOGY
	/** Phenology type 1=cold deciduous 2=evergreen  */
	private int phenologyType;						
	/** Number of leaves cohorts for evergreen trees  */
	private int nbCohortMax;							
	/** Date to start accumulation of temperature for budburst (julian day)  */
	private int budBurstTempAccumulationDateStart;
	/** Threshold of effective temperature for cumulating budburst degree day (degrees)  */
	private double budBurstTempThreshold;	 		
	/** Accumulated temperature to trigger budburst (degrees) */
	private double budBurstTriggerTemp;				
	/** Duration of leaf expansion (in no stress condition)  (days) */
	private int leafExpansionDuration;				
	/** Duration between budburst and leaf fall (days) */
	private int budBurstToLeafFallDuration;			 
	/** Duration of leaf fall (days) */
	private int leafFallDuration;				
	/** Thresold for frost sensibility (degrees) */
	private double leafFallFrostThreshold;			
	/** Leaf senescence rate (ratio) */
	private double leafSenescenceRate;			
	/** Activation of cold requirement phenology  */
	private boolean coldRequirement;				
	/** Date to start accumulation of cold temperature for budburst (julian day) */
	private int coldTempAccumulationDateStart;	    
	/** Threshold of effective temperature for cumulating cold requierement  (degrees) */
	private double coldTempThreshold;	 			
	/** Accumulated cold temperature to trigger budburst (degrees) */
	private double coldBudBurstTriggerTemp;			
	/** Chilling unit calculation parameter */
	private double coldBudBurstTriggerParamA;	
	/** Chilling unit calculation parameter */
	private double coldBudBurstTriggerParamB;		
	/** Chilling unit calculation parameter */
	private double coldBudBurstTriggerParamC;	
	/** Chilling unit calculation parameter */
	private double coldBudBurstTriggerParamE;		

	// LIGHT MODULE 
	/** Leaf absorption coefficient for PAR radiation */
	private double leafParAbsorption;		// no unit
	/** Leaf absorption coefficient for near infra-red radiation  */
	private double leafNirAbsorption;
	/** Virtual leaf area density for winter light interception by tree branches (m2 m-3) */
	private double woodAreaDensity;			
	/** Correction parameter for leaf clumping  */
	private double clumpingCoef; 		

	// TRANSPIRATION
	/** Scaling factor for transpiration */
	private double transpirationCoefficient;	
	/** Coefficient of variation of the stemflow with the LAI */
	private double stemFlowCoefficient;		
	/** Maximum fraction of rain going to stemflow (ratio) */
	private double stemFlowMax; 				
	/** Wettability of leaves, for calculating interception of rain by tree canopy  (mm lai-1) */
	private double wettability; 				

	// CALLOCATION MODULE PARAMETERS
	/**Maximum potential light use efficiency (g C MJ-1) */
	private double leafLueMax;				
	/** Leaf age for lue max (days) */
	private int leafAgeForLueMax;			 
	/** Used to compute LUE according to time after budburst */
	private double leafPhotosynthesisEfficiencyTimeConstant;
	/**  Min of light competition index to stop tree growth */
	private double lightCompetitionIndexMin;	
	/**  Target non-structural carbon pool as a fraction of the tree woody carbon pool  */
	private double targetNSCFraction;		
	/**  Maximum daily fraction of non-structural carbon (NSC) that can be reallocated from the NSC pool */
	private double maxNSCUseFraction;		
	/**  Maximum daily fraction of non-structural carbon (NSC) that can be reallocated from the NSC pool to foliage */
	private double maxNSCUseFoliageFraction;			
	/**  Level of above ground imbalance above which remobilisation of reserves is triggered */
	private double imbalanceThreshold;  
		

	//NITROGEN Balance
	//Functional optimum N/C concentrations
	// arbitrary value see physiology of woody plants p 304 etc. for some estimates of total N per plant part
	/**  Functional optimum N/C ratio in stem */
	private double  optiNCStem;
	/**  Functional optimum N/C ratio in stump */
	private double  optiNCStump;
	/**  Functional optimum N/C ratio in branches */
	private double  optiNCBranch;
	/**  Functional optimum N/C ratio in foliage */
	private double  optiNCFoliage;	
	/**  Functional optimum N/C ratio in coarse roots */
	private double  optiNCCoarseRoot;
	/**  Functional optimum N/C ratio in fine roots */
	private double  optiNCFineRoot;
	/**  Functional optimum N/C ratio in fruits */
	private double  optiNCFruit;
	/**  Coefficient applied to optimum N content to define target N content */
	private double  targetNCoefficient; 		
	/**  Coefficient applied to optimum N content to define maximum N content  */
	private double  luxuryNCoefficient; 		
	/**  Fraction of leaf nitrogen content recovered from dying leaves each day during senesence */
	private double  leafNRemobFraction;			
	/**  Fraction of fine root nitrogen content recovered from dying fine roots each day during senesence */
	private double  rootNRemobFraction;		

	//ROOT GROWTH MODULE
	/** Fine root length per unit dry mass (m g-1 of dry matter) */
	private double specificRootLength;			 
	/** Threshold for root colonisation (m m-3) */
	private double colonisationThreshold;		 	
	/** Fraction of carbon allocated  for root colonisation (ratio) */
	private double colonisationFraction;		 	 //
	/**  Horizontal preference in root colonisation process (ratio) */
	private double horizontalPreference;		
	/**  Governs the fraction of vertical root colonisation to upward vs. downward voxels (ratio) */
	private double geotropismFactor;	
	/**  Effect of source sink distance for root proliferation (dimensionless) */
	private double sinkDistanceEffect;				
	/**  Effect of water efficiency for root proliferation (dimensionless) */
	private double localWaterUptakeFactor;		 	
	/**  Effect of nitrogen efficiency for root proliferation  (dimensionless) */
	private double localNitrogenUptakeFactor;	
	/**  Mean lifespan of fine roots NOT in anoxic voxel for senescence calculation (days) */
	private double fineRootLifespan;	 		
	/**  Mean lifespan of fine roots in anoxic voxel for senescence calculations (days) */
	private double fineRootAnoxiaLifespan;	 		
	/** Coarse root topology shape at initialization 1: surface then down; 2: taproot, 3: ray from stump */
	private int    coarseRootTopologyType;		
	/** After this number of days of saturation in a voxel, anoxia will kill the coarse root and all downstream roots. */
	private int coarseRootAnoxiaResistance;
	
	
	// WATER EXTRACTION MODULE
	/** Fine roots diameters (cm) */
	private double treeRootDiameter;						
	/** Parameter for transpiration reduction factor following Campbell */
	private double treeAlpha;
	/** Minimum tree transpiration potential (cm) */
	private double treeMinTranspirationPotential;			
	/** Maximum tree transpiration potential (cm) */
	private double treeMaxTranspirationPotential;			
	/** Fine root axial conductance of water  (cm cm-1) */
	private double treeRootConductivity;		
	/** Potential drop needed to enter the root expressed as a % of soil water potential (%) */
	private double treeBufferPotential;		
	/** Longitudinal resistance factor for water flow in coarse roots from voxel to stem base  (mm.cm-1.m-1) */
	private double treeLongitudinalResistantFactor;		
	/** The relative influence of dry voxels on the calculation of the averaged soil water potential perceived by the plant  */
	public  double treeHarmonicWeightedMean ;
	
	// FRUIT MODULE 
	/** Activation of fruit compartment  */
	private boolean fruitCompartment;				
	/** Date to start accumulation of temperature for flowering (julian day) */
	private int floweringTempAccumulationDateStart;	
	/** Threshold of effective temperature for cumulating flowering degree day (degrees) */
	private double floweringTempThreshold;			
	/** Accumulated temperature to trigger flowering (degrees) */
	private double floweringTriggerTemp;		   
	/** Accumulated temperature to trigger fruit setting (degrees)*/
	private double fruitSettingTriggerTemp;		
	/** Accumulated temperature to trigger fruit growth (degrees) */
	private double fruitGrowthTriggerTemp;			
	/** Accumulated temperature to trigger fruit veraison (degrees) */
	private double fruitVeraisonTriggerTemp;	
	/** Temperature above witch fruit production is affected by heat (degrees) */
	private double fruitHeatStressTemperatureMin;	
	/** Temperature above witch fruit production is stopped by heat (degrees) */
	private double fruitHeatStressTemperatureMax;	
	/** Temperature below witch fruit production is affected by frost  (degrees) */
	private double fruitFrostStressTemperatureMin;		
	/** Temperature below witch fruit production is stopped by fros (degrees) */
	private double fruitFrostStressTemperatureMax;		
	
	/** Maximum daily fruit growth (g DM) */
	private double fruitMaxDryMatterAllocation;			
	/** Above ground carbon fraction allocated to fruit (ratio) */
	private double fruitAllocationFraction;			
	/** Date to start accumulation of fruit carbon stress */
	private int fruitCarbonStressDateStart;				
	/** Conversion rate from fruit dry to fresh matter  */
	private double fruitDryToFreshMatterWeight;			
	/** Fruit dry matter density (m3 / tonnes DM) */
	private double fruitDryMatterDensity;			
	/** Fruit fresh matter weight to oil concentration conversion parameter */
	private double fruitOilConversionCoeffA;			//
	/** Fruit fresh matter weight to oil concentration conversion parameter */
	private double fruitOilConversionCoeffB;
	/** Fruit fresh matter weight to oil concentration conversion parameter */
	private double fruitOilConversionCoeffC;
	/** Fruit oil density (ratio) */
	private double fruitOilDensity;					
	/** First year of the tree species fruiting  */
	private int fruitFirstYear;						
	/** Fruit number related to leaf area in m2  */
	private double fruitLeafArea;					
	/** ratio C labile / C lignus organs */
	private double fruitingConfortThreshold;			
	/** Ratio of fruitingConfortThreshold inhibate flowering */
	private double fruitingTotalStressThreshold;		


	// NITROGEN FIXATION MODULE 
	/** Activation of nitrogen fixation BNF  */
	private boolean nitrogenFixation;				
	/** Date to start accumulation of temperature for BNF (julian day) */
	private int bnfTempAccumulationDateStart;		
	/** Threshold of effective temperature for cumulating degree day for BNF (degrees) */
	private double bnfTempThreshold;	 		    
	/** Accumulated temperature to trigger BNF start (degrees) */
	private double bnfStartTriggerTemp;			   	
	/** Duration of BNF expansion (days) */
	private int bnfExpansionDuration;			
	/** Duration of BNF start to end (days) */
	private int bnfStartToEndDuration;	
	/**  Maximum depth for nitrogen fixation (m) */
	private double bnfMaxDepth;//
	/**Param1 for trapezoidal function for bnf temperature stress calculation (degrees)*/
	private double bnfCardinalTemp1;
	/**Param2 for trapezoidal function for bnf temperature stress calculation (degrees)*/
	private double bnfCardinalTemp2;	
	/**Param3 for trapezoidal function for bnf temperature stress calculation (degrees)*/
	private double bnfCardinalTemp3;		 
	/**Param4 for trapezoidal function for bnf temperature stress calculation (degrees)*/
	private double bnfCardinalTemp4;	
	/**Saturation inhibiting BNF linear function param Max (g m-3)*/
	private double bnfFullNoduleActivityThreshold;
	/**Saturation inhibiting BNF linear function param Min (g m-3)*/
	private double bnfNullNoduleActivityThreshold;
	/** Threshold under whitch BNF nodulation Inhibition is activated (g m-3))*/
	private double bnfNodulationInhibitionThreshold;	
	/** Air temperature threshold for BNF potential activity to be increased by air temperature (degree) */
	private double bnfAirTemperatureThreshold; 	
	/**The optimal difference to bnfAirTemperatureThreshold for optimal activity (degree)*/
	private double bnfOptimalTemperatureDifference;  
	/**A parameter describing the mass of nitrogen fixed per g of produced vegetative dry matter.*/
	private double bnfFixMaxVeg;
	/**A parameter describing the mass of nitrogen fixed per g of produced reproductive organ dry matter (flowers + fruits). */
	private double bnfFixMaxRepro;
	
	//STRESSES EFFECT ON Root Shoot 
	/** Activation of effect of below ground stress (water and nitrogen) in shoot root allocation  */
	private boolean rsBelowGroundStressActivation ;		//
	/** Activation of effect of light competition in shoot root allocation  */
	private boolean rsLightStressActivation;			
	/** Activation of effect of nitrogen excess in shoot root allocation  */
	private boolean rsNitrogenExcessStressActivation ;	
	/** Root allocation stress calculation method 1=rsWaterStress*rsNitrogenStress 2=Min(rsWaterStress,rsNitrogenStress) */
	private int rsBelowGroundStressMethod;			
	/** Governs amplitude of response in shoot-root allocation when there is no stress */
	private double rsNoStressResponsiveness;		
	/** Governs amplitude of response in shoot root allocation to water stress */
	private double rsWaterStressResponsiveness;			
	/** Governs amplitude of response in shoot root allocation to nitrogen stress */
	private double rsNitrogenStressResponsiveness;	
	/** Maximum daily change (positive or negative) in the target leaf-fine root ratio */
	private double maxTargetLfrRatioDailyVariation;
	/** Target daily upwards drift in the target leaf-fine root ratio */
	private double targetLfrRatioUpperDrift;
	/** Minimum target leaf-fine root ratio */
	private double minTargetLfrRatio;
	/** Maximum target leaf-fine root ratio */
	private double maxTargetLfrRatio;	
	
	//STRESSES EFFECT ON LUE
	/** LUE stress calculation method 1=lueWaterStress*lueNitrogenStress 2=Min(lueWaterStress,lueNitrogenStress) */
	private int lueStressMethod;					
	/** Governs amplitude of response in LUE to water stress */
	private double lueWaterStressResponsiveness;		
	/** Governs amplitude of response in LUE to nitrogen stress */
	private double lueNitrogenStressResponsiveness;	
	/** Temperature below which temperature stress affecting lue is = 0  (degrees) */
	private double lueTemperatureStressTMin;		
	/** Temperature above which temperature stress affecting lue is = 0 (degrees) */
	private double lueTemperatureStressTMax;		
	/** Temperature above which temperature stress affecting lue is optimal (degrees) */
	private double lueTemperatureStressTOptMin;			
	/** Temperature below which temperature stress affecting lue is optimal (degrees) */
	private double lueTemperatureStressTOptMax;			
	
	//OTHER STRESS EFFECT
	/** Governs amplitude of response in SENESCENCE to water stress */
	private double senWaterStressResponsiveness;	
	/** Governs amplitude of response in SENESCENCE to nitrogen stress */
	private double senNitrogenStressResponsiveness;
	/** Temperature below which leaf area is not affected by frost (degrees) */
	private double leafFrostStressTemperatureMin;
	/** Temperature below which leaf area is totally affected by frost (defoliation) (degrees) */
	private double leafFrostStressTemperatureMax;	

	//CO2 EFFECT
	/** CO2 Effect on LUE activation  */
	private boolean co2EffectOnLueActivation;				
	/** CO2 Effect on WUE activation  */
	private boolean co2EffectOnWueActivation;				
	/** CO2 Effect on LUE (Light use efficiency) : half saturation constant (ppm)  */
	private double co2EffectOnLueHalfSaturationConstant;	
	/** CO2 intrinsic effect on WUE (Water use efficiency) sensitivity  */
	private double co2EffectIntrinsicWueSensitivity;		
	/** CO2 concentration reference value (ppm)  */
	private double co2ReferenceValue;					
 
	// SELF PRUNING PARAMETERS 
	/** Self pruning effet activation  */
	private boolean selfPruningEffet;						
	/** Light Competition index threshold for self pruning  */
	private double  selfPruningLCIThreshold;				
	/** Proportion of Self-pruning canopy height */
	private double  selfPruningHeightRatio;						
	/** Number of days of shade to trigger self pruning  */
	private int     selfPruningNbrDaysShade;					
	/** Number of year for full decay of self pruning branches */
	private int     selfPruningNbrYearsForBranchesFullDecay;	

	//TROPICAL SPECIES MODULE (FAIDHERBIA)
	//ADDED  BY LEA TRESCH 01/2026
	/**Delay from last budburst to trigger upcoming budburst*/
	private int budburstDelayFromLastBudburst;
	/**Delay from last minimum water table to trigger upcoming budburst*/
	private int budburstDelayFromMinWaterTable;
	
	public SafeTreeSpecies () {}
	
	public String getName () {return name;}
	public String getFileName () {return fileName;}
	public void setFileName (String v) {fileName = v;}
	public int getValue() {return 0;}
	public int getCrownShape () {return crownShape;}
	public double getEllipsoidTruncationRatio() {return ellipsoidTruncationRatio;}

	public double getHeightDbhAllometricCoeffA() {return heightDbhAllometricCoeffA;}
	public double getHeightDbhAllometricCoeffB() {return heightDbhAllometricCoeffB;}
	public double getStemDbhAllometricCoeffA() {return stemDbhAllometricCoeffA;}
	public double getStemDbhAllometricCoeffB() {return stemDbhAllometricCoeffB;}
	public double getStemDbhAllometricCoeffC() {return stemDbhAllometricCoeffC;}
	public double getCrownDbhAllometricCoeffA () {return crownDbhAllometricCoeffA;}
	public double getCrownDbhAllometricCoeffB () {return crownDbhAllometricCoeffB;}
	public double getDcbFromDbhAllometricCoeff () {return dcbFromDbhAllometricCoeff;}
	
	public double getStumpToStemBiomassRatio () {return stumpToStemBiomassRatio;}

	public double getCRAreaToFRLengthRatio() {return cRAreaToFRLengthRatio;}
	public double getInitialTargetLfrRatio() {return initialTargetLfrRatio;}
	

	public double getLeafAreaCrownVolCoefA() {return leafAreaCrownVolCoefA;}
	public double getLeafAreaCrownVolCoefB() {return leafAreaCrownVolCoefB;}
	public double getWoodAreaDensity() {return woodAreaDensity;}
	public double getLeafParAbsorption() {return leafParAbsorption;}
	public double getLeafNirAbsorption() {return leafNirAbsorption;}
	public double getClumpingCoef() {return clumpingCoef;}

	public int getPhenologyType () {return phenologyType;}
	public int getNbCohortMax () {return nbCohortMax;}
	public int getBudBurstTempAccumulationDateStart () {return budBurstTempAccumulationDateStart;}
	public double getBudBurstTempThreshold () {return budBurstTempThreshold;}
	public int getLeafExpansionDuration () {return leafExpansionDuration;}
	public int getBudBurstToLeafFallDuration () {return budBurstToLeafFallDuration;}
	public int getLeafFallDuration () {return leafFallDuration;}
	public double getLeafFallFrostThreshold() {return leafFallFrostThreshold;}
	public double getBudBurstTriggerTemp () {return budBurstTriggerTemp;}

	public boolean getColdRequirement () {return coldRequirement;}
	public int getColdTempAccumulationDateStart() {return coldTempAccumulationDateStart;}
	
	public double getColdTempThreshold() {return coldTempThreshold;}
	public double getColdBudBurstTriggerTemp() {return coldBudBurstTriggerTemp;}
	public double getColdBudBurstTriggerParamA() {return coldBudBurstTriggerParamA;}
	public double getColdBudBurstTriggerParamB() {return coldBudBurstTriggerParamB;}
	public double getColdBudBurstTriggerParamC() {return coldBudBurstTriggerParamC;}
	public double getColdBudBurstTriggerParamE() {return coldBudBurstTriggerParamE;}
	

	
	public boolean getFruitCompartment () {return fruitCompartment;}
	public int getFloweringTempAccumulationDateStart () {return floweringTempAccumulationDateStart;}
	public double getFloweringTempThreshold() {return floweringTempThreshold;}
	
	
	public double getFloweringTriggerTemp() {return floweringTriggerTemp;}
	public double getFruitSettingTriggerTemp() {return fruitSettingTriggerTemp;}
	public double getFruitGrowthTriggerTemp() {return fruitGrowthTriggerTemp;}
	public double getFruitVeraisonTriggerTemp() {return fruitVeraisonTriggerTemp;}

	
	public boolean getNitrogenFixation () {return nitrogenFixation;}
	public int getBnfTempAccumulationDateStart () {return bnfTempAccumulationDateStart;}
	public double getBnfTempThreshold() {return bnfTempThreshold;}
	public double getBnfStartTriggerTemp() {return bnfStartTriggerTemp;}
	public int getBnfExpansionDuration() {return bnfExpansionDuration;}
	public int getBnfStartToEndDuration() {return bnfStartToEndDuration;}
	
	
	public double getStemFlowCoefficient () {return stemFlowCoefficient;}
	public double getStemFlowMax () {return stemFlowMax;}
	public double getWettability () {return wettability;}

	public double getTranspirationCoefficient () {return transpirationCoefficient;}

	public double getLeafLueMax () {return leafLueMax;}
	public int getLeafAgeForLueMax () {return leafAgeForLueMax;}
	public double getLeafPhotosynthesisEfficiencyTimeConstant() {return leafPhotosynthesisEfficiencyTimeConstant;}
	public double getLightCompetitionIndexMin() {return lightCompetitionIndexMin;}
	
	public double getLeafCarbonContent () {return leafCarbonContent;}
	public double getLeafMassArea() {return leafMassArea;}
	public double getLuxuryNCoefficient() {return luxuryNCoefficient;}
	public double getTargetNCoefficient() {return targetNCoefficient;}
	
	public double getRootNRemobFraction () {return rootNRemobFraction;}
	public double getLeafNRemobFraction () {return leafNRemobFraction;}
	public double getOptiNCBranch() {return optiNCBranch;}
	public double getOptiNCCoarseRoot() {return optiNCCoarseRoot;}
	public double getOptiNCFineRoot() {return optiNCFineRoot;}
	public double getOptiNCFoliage() {return optiNCFoliage;}
	public double getOptiNCStem () {return optiNCStem;}
	public double getOptiNCStump () {return optiNCStump;}
	public double getOptiNCFruit () {return optiNCFruit;}
	
	public double getTargetNSCFraction () {return targetNSCFraction;}
	public double getMaxNSCUseFraction() {return maxNSCUseFraction;}
	public double getMaxNSCUseFoliageFraction () {return maxNSCUseFoliageFraction;}
	public double getRsNoStressResponsiveness () {return rsNoStressResponsiveness;}
	

	public int getRsBelowGroundStressMethod () {return rsBelowGroundStressMethod;}
	public int getLueStressMethod () {return lueStressMethod;}
	public double getRsWaterStressResponsiveness () {return rsWaterStressResponsiveness;}
	public double getRsNitrogenStressResponsiveness () {return rsNitrogenStressResponsiveness;}
	public boolean getRsLightStressActivation () {return rsLightStressActivation;}
	public boolean getRsNitrogenExcessStressActivation () {return rsNitrogenExcessStressActivation;}
	public boolean getRsBelowGroundStressActivation () {return rsBelowGroundStressActivation;}
	
	public double getLueWaterStressResponsiveness () {return lueWaterStressResponsiveness;}
	public double getLueNitrogenStressResponsiveness () {return lueNitrogenStressResponsiveness;}
	public double getSenWaterStressResponsiveness () {return senWaterStressResponsiveness;}
	public double getSenNitrogenStressResponsiveness () {return senNitrogenStressResponsiveness;}
	
	
	
	

	
	public boolean getCo2EffectOnLueActivation() {return co2EffectOnLueActivation;}
	public boolean getCo2EffectOnWueActivation () {return co2EffectOnWueActivation;}
	public double getCo2EffectOnLueHalfSaturationConstant () {return co2EffectOnLueHalfSaturationConstant;}
	public double getCo2EffectIntrinsicWueSensitivity() {return co2EffectIntrinsicWueSensitivity;}
	public double getCo2ReferenceValue () {return co2ReferenceValue;}
	

	
	
	
	public double getMaxTargetLfrRatioDailyVariation () {return maxTargetLfrRatioDailyVariation;}
	public double getTargetLfrRatioUpperDrift() {return targetLfrRatioUpperDrift;}
	public double getMinTargetLfrRatio() {return minTargetLfrRatio;}
	public double getMaxTargetLfrRatio() {return maxTargetLfrRatio;}


	
	
	public double getLeafFrostStressTemperatureMin() {return leafFrostStressTemperatureMin;}
	public double getLeafFrostStressTemperatureMax() {return leafFrostStressTemperatureMax;}
	
	public double getLueTemperatureStressTMin() {return lueTemperatureStressTMin;}
	public double getLueTemperatureStressTMax() {return lueTemperatureStressTMax;}
	public double getLueTemperatureStressTOptMin() {return lueTemperatureStressTOptMin;}
	public double getLueTemperatureStressTOptMax() {return lueTemperatureStressTOptMax;}
	



	public double getWoodDensity() {return woodDensity;}
	public double getBranchVolumeRatio() {return branchVolumeRatio;}
	public double getLeafSenescenceRate() {return leafSenescenceRate;}
	public double getImbalanceThreshold() {return imbalanceThreshold;}
	public double getWoodCarbonContent() {return woodCarbonContent;}
	public double getMaxCrownRadiusInc() {return maxCrownRadiusInc;}
	public double getMaxHeightInc() {return maxHeightInc;}
	public double getMaxDbhInc() {return maxDbhInc;}
	


/** FINE ROOT GROWTH PARAMETERS **/
	public double getSpecificRootLength() {return specificRootLength;}
	public int getCoarseRootAnoxiaResistance(){return coarseRootAnoxiaResistance;}
	public double getColonisationThreshold() {return colonisationThreshold;}
	public double getColonisationFraction() {return colonisationFraction;}
	
	
	public double getFineRootLifespan() {return fineRootLifespan;}
	public double getFineRootAnoxiaLifespan() {return fineRootAnoxiaLifespan;}

	public double getHorizontalPreference() {return horizontalPreference;}
	public double getGeotropismFactor() {return geotropismFactor;}
	public double getLocalWaterUptakeFactor() {return localWaterUptakeFactor;}
	public double getSinkDistanceEffect() {return sinkDistanceEffect;}
	public double getLocalNitrogenUptakeFactor() {return localNitrogenUptakeFactor;}


/** COARSE ROOT GROWTH INITIALISATION **/
	public int getCoarseRootTopologyType() {return coarseRootTopologyType;}


	public double getTreeRootDiameter() {return treeRootDiameter;}
	public double getTreeAlpha() {return treeAlpha;}
	public double getTreeRootConductivity() {return treeRootConductivity;}
	public double getTreeMaxTranspirationPotential() {return treeMaxTranspirationPotential;}
	public double getTreeMinTranspirationPotential() {return treeMinTranspirationPotential;}
	public double getTreeBufferPotential() {return treeBufferPotential;}
	public double getTreeLongitudinalResistantFactor() {return treeLongitudinalResistantFactor;}
	public double getTreeHarmonicWeightedMean() {return treeHarmonicWeightedMean;}

	//fruit module
	public double getFruitHeatStressTemperatureMin() {return fruitHeatStressTemperatureMin;}
	public double getFruitHeatStressTemperatureMax() {return fruitHeatStressTemperatureMax;}
	
	public double getFruitFrostStressTemperatureMin() {return fruitFrostStressTemperatureMin;}
	public double getFruitFrostStressTemperatureMax() {return fruitFrostStressTemperatureMax;}
	

	
	public double getFruitMaxDryMatterAllocation() {return fruitMaxDryMatterAllocation;}
	public double getFruitAllocationFraction() {return fruitAllocationFraction;}
	public int getFruitCarbonStressDateStart() {return fruitCarbonStressDateStart;}
	
	
	
	public double getFruitDryToFreshMatterWeight() {return fruitDryToFreshMatterWeight;}
	public double getFruitDryMatterDensity() {return fruitDryMatterDensity;}
	public double getFruitOilConversionCoeffA() {return fruitOilConversionCoeffA;}
	public double getFruitOilConversionCoeffB() {return fruitOilConversionCoeffB;}
	public double getFruitOilConversionCoeffC() {return fruitOilConversionCoeffC;}
	public double getFruitOilDensity() {return fruitOilDensity;}
	
	
	public int getFruitFirstYear() {return fruitFirstYear;}
	public double getFruitLeafArea() {return fruitLeafArea;}
	public double getFruitingConfortThreshold() {return fruitingConfortThreshold;}
	public double getFruitingTotalStressThreshold() {return fruitingTotalStressThreshold;}

	//BNF module
	public double getBnfMaxDepth() {return bnfMaxDepth;}
	public double getBnfNodulationInhibitionThreshold() {return bnfNodulationInhibitionThreshold;}
	public double getBnfCardinalTemp1() {return bnfCardinalTemp1;}
	public double getBnfCardinalTemp2() {return bnfCardinalTemp2;}
	public double getBnfCardinalTemp3() {return bnfCardinalTemp3;}
	public double getBnfCardinalTemp4() {return bnfCardinalTemp4;}
	public double getBnfFullNoduleActivityThreshold() {return bnfFullNoduleActivityThreshold;}
	public double getBnfNullNoduleActivityThreshold() {return bnfNullNoduleActivityThreshold;}
	
	
	
	public double getBnfAirTemperatureThreshold() {return bnfAirTemperatureThreshold;}
	public double getBnfOptimalTemperatureDifference() {return bnfOptimalTemperatureDifference;}
	
	public double getBnfFixMaxVeg() {return bnfFixMaxVeg;}
	public double getBnfFixMaxRepro() {return bnfFixMaxRepro;}
	
	public boolean getSelfPruningEffet()  {return selfPruningEffet;}
	public double getSelfPruningLCIThreshold() {return selfPruningLCIThreshold;}
	public double getSelfPruningHeightRatio() {return selfPruningHeightRatio;}
	public int getSelfPruningNbrDaysShade() {return selfPruningNbrDaysShade;}
	public int getSelfPruningNbrYearsForBranchesFullDecay() {return selfPruningNbrYearsForBranchesFullDecay;}


	public int getBudburstDelayFromLastBudburst() {return budburstDelayFromLastBudburst;}
	public int getBudburstDelayFromMinWaterTable() {return budburstDelayFromMinWaterTable;}
	

	
	/**
	 * return Campbell factor  (dimensionless)
	 * ICRAF method
	 */
	public double getCampbellFactorIcraf() {
		return (2 * Math.log (treeAlpha / (1 - treeAlpha))
				/ Math.log (treeMaxTranspirationPotential / treeMinTranspirationPotential));
	}
	
	/**
	 * return Campbell factor  (dimensionless)
	 * INOT USED
	 */
	public double getCampbellFactor (double plantWaterPotential) {
		double halfCurrWaterPotential = getHalfCurrWaterPotential();
		double a = getA();
		return 1.0 / (1.0 + Math.pow(plantWaterPotential/halfCurrWaterPotential,a));

	}
	
	/**
	*  return water potential where tranpiration demand is half of its potential
	*  ICRAF method
	*/
	public double getHalfCurrWaterPotentialIcraf() {
			return (treeMaxTranspirationPotential * Math.pow ((1 - treeAlpha) / treeAlpha, 1 / getCampbellFactorIcraf()));
	}

	/**
	*  return water potential where tranpiration demand is half of its potential
	*  NOT USED
	*/
	public double getHalfCurrWaterPotential() {
			return -Math.sqrt (treeMaxTranspirationPotential * treeMinTranspirationPotential);
	}

	public double getA() {
			return (2.0 * Math.log (treeAlpha / (1 - treeAlpha))
					   / Math.log (treeMaxTranspirationPotential / treeMinTranspirationPotential));
	}

	/**
	*  To add parameters to a existing species
	*  read in a specific parameter file.
	*/

	public void updateSpecies  (String treeSpeciesName, int crownShape,
								double ellipsoidTruncationRatio, 
								double heightDbhAllometricCoeffA, double heightDbhAllometricCoeffB,
								double crownDbhAllometricCoeffA, double crownDbhAllometricCoeffB,
								double stemDbhAllometricCoeffA, double stemDbhAllometricCoeffB, double stemDbhAllometricCoeffC, 
								double dcbFromDbhAllometricCoeff,
								double stumpToStemBiomassRatio,
								double cRAreaToFRLengthRatio, 
								double initialTargetLfrRatio,
								double leafAreaCrownVolCoefA, double leafAreaCrownVolCoefB,
								double woodAreaDensity, double leafParAbsorption, double leafNirAbsorption, double clumpingCoef,
								
								int phenologyType, int nbCohortMax, 
								int budBurstTempAccumulationDateStart, 
								double budBurstTempThreshold,
								double budBurstTriggerTemp,
								int leafExpansionDuration,
								int budBurstToLeafFallDuration,
								int leafFallDuration,
								double leafFallFrostThreshold,
								double stemFlowCoefficient,
								double stemFlowMax,
								double wettability,
								double transpirationCoefficient,
								double leafLueMax,
								int leafAgeForLueMax,
								double leafPhotosynthesisEfficiencyTimeConstant,
								double lightCompetitionIndexMin,
								
								double leafCarbonContent,
								double leafMassArea,
								double luxuryNCoefficient,
								double targetNCoefficient,
								double rootNRemobFraction,
								double leafNRemobFraction,	
								double targetNSCFraction,
								double maxNSCUseFraction,
								double maxNSCUseFoliageFraction,	
								boolean rsBelowGroundStressActivation,
								boolean rsLightStressActivation, 
								boolean rsNitrogenExcessStressActivation, 
								int rsBelowGroundStressMethod,
								double rsNoStressResponsiveness,
								double rsWaterStressResponsiveness, double rsNitrogenStressResponsiveness,
								double maxTargetLfrRatioDailyVariation,double targetLfrRatioUpperDrift,
								double minTargetLfrRatio, double maxTargetLfrRatio,
								

								int lueStressMethod,
								double lueWaterStressResponsiveness, double lueNitrogenStressResponsiveness,
								double lueTemperatureStressTMin, double lueTemperatureStressTMax,
								double lueTemperatureStressTOptMin, double lueTemperatureStressTOptMax,
								double senWaterStressResponsiveness, double senNitrogenStressResponsiveness,
								
								double leafFrostStressTemperatureMin, double leafFrostStressTemperatureMax,
								
								boolean co2EffectOnLueActivation, boolean co2EffectOnWueActivation,
								double co2EffectOnLueHalfSaturationConstant, double co2EffectIntrinsicWueSensitivity, double co2ReferenceValue,

								double optiNCBranch,
								double optiNCCoarseRoot,
								double optiNCFineRoot,
								double optiNCFoliage,
								double optiNCStem,
								double optiNCStump,
								double optiNCFruit,
								
								double woodDensity,
								double branchVolumeRatio,
								double woodCarbonContent,
								double maxCrownRadiusInc,
								double maxHeightInc,	
								double maxDbhInc,
								
								double imbalanceThreshold,
								double leafSenescenceRate,
								int coarseRootAnoxiaResistance,
								double specificRootLength,
								double colonisationThreshold,
								double colonisationFraction,
								double fineRootLifespan,
								double fineRootAnoxiaLifespan,
								double horizontalPreference,
								double geotropismFactor,
								double localWaterUptakeFactor,
								double sinkDistanceEffect,
								double localNitrogenUptakeFactor,
								int    coarseRootTopologyType,
								double rootDiameter, double rootConductivity,
								double alpha,
								double minTranspirationPotential,double maxTranspirationPotential,
								double bufferPotential, double longitudinalResistantFactor,
								double harmonicWeightedMean,
								
								//SELF PRUNING 
								boolean selfPruningEffet,
								double selfPruningLCIThreshold,
								double selfPruningHeightRatio,
								int    selfPruningNbrDaysShade,
								int 	selfPruningNbrYearsForBranchesFullDecay
								

								) {

		this.name = treeSpeciesName;
		this.crownShape = crownShape;
		this.ellipsoidTruncationRatio = ellipsoidTruncationRatio;
		this.heightDbhAllometricCoeffA = heightDbhAllometricCoeffA;
		this.heightDbhAllometricCoeffB = heightDbhAllometricCoeffB;
		this.crownDbhAllometricCoeffA = crownDbhAllometricCoeffA;
		this.crownDbhAllometricCoeffB = crownDbhAllometricCoeffB;
		this.stemDbhAllometricCoeffA  = stemDbhAllometricCoeffA;
		this.stemDbhAllometricCoeffB  = stemDbhAllometricCoeffB;
		this.stemDbhAllometricCoeffC  = stemDbhAllometricCoeffC;
		this.dcbFromDbhAllometricCoeff = dcbFromDbhAllometricCoeff;
		this.stumpToStemBiomassRatio  = stumpToStemBiomassRatio;
		this.cRAreaToFRLengthRatio = cRAreaToFRLengthRatio;
		this.initialTargetLfrRatio = initialTargetLfrRatio; 
		this.leafAreaCrownVolCoefA = leafAreaCrownVolCoefA;
		this.leafAreaCrownVolCoefB = leafAreaCrownVolCoefB;
		this.woodAreaDensity = woodAreaDensity;
		this.leafParAbsorption = leafParAbsorption;
		this.leafNirAbsorption = leafNirAbsorption;
		this.clumpingCoef = clumpingCoef;
		
		this.phenologyType = phenologyType;
		this.nbCohortMax = nbCohortMax; 
		this.budBurstTempAccumulationDateStart = budBurstTempAccumulationDateStart;
		this.budBurstTempThreshold = budBurstTempThreshold;
		this.budBurstTriggerTemp = budBurstTriggerTemp;	
		this.leafExpansionDuration = leafExpansionDuration;
		this.budBurstToLeafFallDuration = budBurstToLeafFallDuration;
		this.leafFallDuration = leafFallDuration;
		this.leafFallFrostThreshold = leafFallFrostThreshold;

		this.stemFlowCoefficient = stemFlowCoefficient;
		this.stemFlowMax = stemFlowMax;
		this.wettability = wettability;
		this.transpirationCoefficient = transpirationCoefficient;
		this.leafLueMax = leafLueMax;
		this.leafAgeForLueMax = leafAgeForLueMax;
		this.leafPhotosynthesisEfficiencyTimeConstant = leafPhotosynthesisEfficiencyTimeConstant;
		this.lightCompetitionIndexMin = lightCompetitionIndexMin;
		this.leafCarbonContent = leafCarbonContent;
		this.leafMassArea = leafMassArea;
		this.luxuryNCoefficient = luxuryNCoefficient;
		this.targetNCoefficient = targetNCoefficient;
		this.rootNRemobFraction = rootNRemobFraction;
		this.leafNRemobFraction = leafNRemobFraction;
		this.targetNSCFraction = targetNSCFraction;
		this.maxNSCUseFraction = maxNSCUseFraction;
		this.maxNSCUseFoliageFraction = maxNSCUseFoliageFraction;
		this.rsBelowGroundStressActivation = rsBelowGroundStressActivation;
		this.rsLightStressActivation = rsLightStressActivation;
		this.rsNitrogenExcessStressActivation = rsNitrogenExcessStressActivation;
		this.rsBelowGroundStressMethod = rsBelowGroundStressMethod;
		this.lueStressMethod = lueStressMethod;
		this.rsNoStressResponsiveness = rsNoStressResponsiveness;
		this.rsWaterStressResponsiveness = rsWaterStressResponsiveness;
		this.rsNitrogenStressResponsiveness = rsNitrogenStressResponsiveness;
		this.lueWaterStressResponsiveness = lueWaterStressResponsiveness;
		this.lueNitrogenStressResponsiveness = lueNitrogenStressResponsiveness;
		this.senWaterStressResponsiveness = senWaterStressResponsiveness;
		this.senNitrogenStressResponsiveness = senNitrogenStressResponsiveness;
		this.leafFrostStressTemperatureMin = leafFrostStressTemperatureMin;
		this.leafFrostStressTemperatureMax = leafFrostStressTemperatureMax;
		this.co2EffectOnLueActivation = co2EffectOnLueActivation;
		this.co2EffectOnWueActivation = co2EffectOnWueActivation;
		this.co2EffectOnLueHalfSaturationConstant = co2EffectOnLueHalfSaturationConstant;
		this.co2EffectIntrinsicWueSensitivity = co2EffectIntrinsicWueSensitivity;
		this.co2ReferenceValue = co2ReferenceValue;
		this.lueTemperatureStressTMin = lueTemperatureStressTMin;
		this.lueTemperatureStressTMax = lueTemperatureStressTMax;
		this.lueTemperatureStressTOptMin = lueTemperatureStressTOptMin;
		this.lueTemperatureStressTOptMax = lueTemperatureStressTOptMax;
		this.maxTargetLfrRatioDailyVariation = maxTargetLfrRatioDailyVariation;
		this.targetLfrRatioUpperDrift = targetLfrRatioUpperDrift;
		this.minTargetLfrRatio = minTargetLfrRatio;
		this.maxTargetLfrRatio = maxTargetLfrRatio;
		this.optiNCBranch = optiNCBranch;
		this.optiNCCoarseRoot = optiNCCoarseRoot;
		this.optiNCFineRoot =  optiNCFineRoot;
		this.optiNCFoliage =  optiNCFoliage;
		this.optiNCStem =  optiNCStem;
		this.optiNCStump =  optiNCStump;
		this.optiNCFruit =  optiNCFruit;

		this.woodDensity = woodDensity;
		this.branchVolumeRatio = branchVolumeRatio;
		this.woodCarbonContent = woodCarbonContent;	
		this.maxCrownRadiusInc = maxCrownRadiusInc;
		this.maxHeightInc = maxHeightInc;
		this.maxDbhInc = maxDbhInc;
		this.leafSenescenceRate = leafSenescenceRate;
		this.imbalanceThreshold= imbalanceThreshold;
		this.specificRootLength = specificRootLength;
		this.coarseRootAnoxiaResistance=coarseRootAnoxiaResistance;
		this.colonisationThreshold = colonisationThreshold;
		this.colonisationFraction = colonisationFraction;
		this.fineRootLifespan= fineRootLifespan;
		this.fineRootAnoxiaLifespan = fineRootAnoxiaLifespan;
		this.horizontalPreference= horizontalPreference;
		this.geotropismFactor= geotropismFactor;
		this.localWaterUptakeFactor = localWaterUptakeFactor;
		this.sinkDistanceEffect = sinkDistanceEffect;
		this.localNitrogenUptakeFactor = localNitrogenUptakeFactor;
		this.coarseRootTopologyType = coarseRootTopologyType;
		this.treeRootDiameter = rootDiameter;
		this.treeRootConductivity = rootConductivity;
		this.treeAlpha = alpha;
		this.treeMinTranspirationPotential = minTranspirationPotential;
		this.treeMaxTranspirationPotential = maxTranspirationPotential;
		this.treeBufferPotential = bufferPotential;
		this.treeLongitudinalResistantFactor = longitudinalResistantFactor;
		this.treeHarmonicWeightedMean = harmonicWeightedMean;

		this.selfPruningEffet = selfPruningEffet;
		this.selfPruningLCIThreshold = selfPruningLCIThreshold;
		this.selfPruningHeightRatio = selfPruningHeightRatio;
		this.selfPruningNbrDaysShade = selfPruningNbrDaysShade;
		this.selfPruningNbrYearsForBranchesFullDecay = selfPruningNbrYearsForBranchesFullDecay;

		
	}
	
	public void updateFruitSpecies  (

			boolean  fruitCompartment,
			int floweringTempAccumulationDateStart,
			double floweringTempThreshold,
			double floweringTriggerTemp,
			double fruitSettingTriggerTemp,
			double fruitGrowthTriggerTemp,
			double fruitVeraisonTriggerTemp,
			double fruitHeatStressTemperatureMin, double fruitHeatStressTemperatureMax,
			double fruitFrostStressTemperatureMin, double fruitFrostStressTemperatureMax,
			double fruitMaxDryMatterAllocation, double fruitAllocationFraction, int fruitCarbonStressDateStart,
			double fruitDryToFreshMatterWeight, double fruitDryMatterDensity,
			double fruitOilConversionCoeffA, double fruitOilConversionCoeffB, double fruitOilConversionCoeffC,
			double fruitOilDensity,
			int fruitFirstYear,
			double fruitLeafArea, 
			double fruitingConfortThreshold,
			double fruitingTotalStressThreshold,
			boolean coldRequirement,
			int coldTempAccumulationDateStart,
			double coldTempThreshold,
			double coldBudBurstTriggerTemp,
			double coldBudBurstTriggerParamA,
			double coldBudBurstTriggerParamB,
			double coldBudBurstTriggerParamC,
			double coldBudBurstTriggerParamE

			) {

				this.fruitCompartment = fruitCompartment;
				this.floweringTempAccumulationDateStart = floweringTempAccumulationDateStart;
				this.floweringTempThreshold = floweringTempThreshold; 
				this.floweringTriggerTemp = floweringTriggerTemp;
				this.fruitSettingTriggerTemp = fruitSettingTriggerTemp;
				this.fruitGrowthTriggerTemp = fruitGrowthTriggerTemp;
				this.fruitVeraisonTriggerTemp = fruitVeraisonTriggerTemp;
				this.fruitHeatStressTemperatureMin = fruitHeatStressTemperatureMin;
				this.fruitHeatStressTemperatureMax = fruitHeatStressTemperatureMax;
				this.fruitFrostStressTemperatureMin = fruitFrostStressTemperatureMin;
				this.fruitFrostStressTemperatureMax = fruitFrostStressTemperatureMax;
				this.fruitMaxDryMatterAllocation = fruitMaxDryMatterAllocation;
				this.fruitAllocationFraction = fruitAllocationFraction;
				this.fruitCarbonStressDateStart = fruitCarbonStressDateStart;
				this.fruitDryToFreshMatterWeight = fruitDryToFreshMatterWeight;
				this.fruitDryMatterDensity = fruitDryMatterDensity; 
				this.fruitOilConversionCoeffA = fruitOilConversionCoeffA;
				this.fruitOilConversionCoeffB = fruitOilConversionCoeffB;
				this.fruitOilConversionCoeffC = fruitOilConversionCoeffC;
				this.fruitOilDensity = fruitOilDensity;
				this.fruitFirstYear = fruitFirstYear;
				this.fruitLeafArea = fruitLeafArea;
				this.fruitingConfortThreshold = fruitingConfortThreshold;
				this.fruitingTotalStressThreshold = fruitingTotalStressThreshold;

				//phenology : cold temperature 			
				this.coldRequirement = coldRequirement;
				this.coldTempAccumulationDateStart = coldTempAccumulationDateStart;
				this.coldTempThreshold = coldTempThreshold;
				this.coldBudBurstTriggerTemp = coldBudBurstTriggerTemp;
				this.coldBudBurstTriggerParamA = coldBudBurstTriggerParamA;
				this.coldBudBurstTriggerParamB = coldBudBurstTriggerParamB;
				this.coldBudBurstTriggerParamC = coldBudBurstTriggerParamC;
				this.coldBudBurstTriggerParamE = coldBudBurstTriggerParamE;
				
				
	}


	public void updateBnfSpecies  (

								boolean nitrogenFixation,
								int bnfTempAccumulationDateStart,
								double bnfTempThreshold,
								double bnfStartTriggerTemp,
								int bnfExpansionDuration,
								int bnfStartToEndDuration,
								double bnfMaxDepth,
								double bnfNodulationInhibitionThreshold,
								double bnfCardinalTemp1, 
								double bnfCardinalTemp2,	
								double bnfCardinalTemp3,		 
								double bnfCardinalTemp4,	
								double bnfFullNoduleActivityThreshold,
								double bnfNullNoduleActivityThreshold,								
								double bnfAirTemperatureThreshold,
								double bnfOptimalTemperatureDifference,								
								double bnfFixMaxVeg,
								double bnfFixMaxRepro							

								) {

			this.nitrogenFixation = nitrogenFixation;
			this.bnfTempAccumulationDateStart = bnfTempAccumulationDateStart;
			this.bnfTempThreshold = bnfTempThreshold;
			this.bnfStartTriggerTemp = bnfStartTriggerTemp;
			this.bnfExpansionDuration = bnfExpansionDuration;
			this.bnfStartToEndDuration = bnfStartToEndDuration;
			this.bnfMaxDepth = bnfMaxDepth;
			this.bnfNodulationInhibitionThreshold = bnfNodulationInhibitionThreshold;
			this.bnfCardinalTemp1 = bnfCardinalTemp1;
			this.bnfCardinalTemp2 = bnfCardinalTemp2;	
			this.bnfCardinalTemp3 = bnfCardinalTemp3;	 
			this.bnfCardinalTemp4 = bnfCardinalTemp4;
			this.bnfFullNoduleActivityThreshold = bnfFullNoduleActivityThreshold;
			this.bnfNullNoduleActivityThreshold = bnfNullNoduleActivityThreshold;		
			this.bnfAirTemperatureThreshold = bnfAirTemperatureThreshold;
			this.bnfOptimalTemperatureDifference = bnfOptimalTemperatureDifference;
			this.bnfFixMaxVeg = bnfFixMaxVeg;
			this.bnfFixMaxRepro = bnfFixMaxRepro;		

	}	
	
	public void updateTropicalSpecies  (
			int budburstDelayFromLastBudburst,
			int budburstDelayFromMinWaterTable
			) {

		this.budburstDelayFromLastBudburst = 	budburstDelayFromLastBudburst;
		this.budburstDelayFromMinWaterTable = 	budburstDelayFromMinWaterTable;

	}	
		
}
