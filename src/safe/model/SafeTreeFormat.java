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

import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * TREE SPECIES parameters format for reading in a file 
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeTreeFormat extends RecordSet {

	/** SafeTreeSpecies name */
	public String treeSpecies;

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
	/** Phenology type 1=cold deciduous 2=evergreen 3=tropical(faidherbia) */
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
	/**  Maximum daily fraction of non-structural carbon (NSC) that can be reallocated from the NSC pool and towards leaf expansion*/
	private double maxNSCUseFraction;		
	/**  Limits daily amount of non-structural carbon (NSC) that can be reallocated from the NSC pool and towards leaf expansion */
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
	/** Date to start accumulation of fruit carbon stress (julian day) */
	private int fruitCarbonStressDateStart;				
	/** Conversion rate from fruit dry to fresh matter  */
	private double fruitDryToFreshMatterWeight;			
	/** Fruit dry mater density (m2 / tonnes DM) */
	private double fruitDryMaterDensity;			
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
	private double bnfNodulationInhibitionThreshold;	
	private double bnfCardinalTemp1;
	private double bnfCardinalTemp2;	
	private double bnfCardinalTemp3;		 
	private double bnfCardinalTemp4;	
	private double bnfFullNoduleActivityThreshold;
	private double bnfNullNoduleActivityThreshold;
	/** Air temperature threshold for BNF potential activity to be increased by air temperature (degree) */
	private double bnfAirTemperatureThreshold; 		
	private double bnfOptimalTemperatureDifference;  //degrees 
	private double bnfFixMaxVeg;
	private double bnfFixMaxRepro;
	
	//STRESSES EFFECT ON Root Shoot 
	/** Activation of effect of below ground stress (water and nitrogen) in shoot root allocation s */
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
	/** CO2 reference value (ppm)  */
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
	
	/** 
	 * Constructor
	 * @param fileName The file name to read to create the tree species parameters
	*/
	public SafeTreeFormat (String fileName) throws Exception {
		prepareImport (fileName);
	}

	/** 
	 * Get julian days from a date MM-DD
	 * @param dateMMDD Date format MM-DD
	 * @return Julian day (0-365)
	*/
	public int getJulianDay (String dateMMDD)
	{
		if (dateMMDD.equals ("999")) return 999;
		if (dateMMDD.equals ("0")) return 0;
		int year = 1999;	//it could be any year 
		String [] part1 = dateMMDD.split("-");
		GregorianCalendar date = new GregorianCalendar();
		date.set(year,Integer.parseInt(part1[0])-1,Integer.parseInt(part1[1]) );
		int jul =  date.get(GregorianCalendar.DAY_OF_YEAR);
		return jul;
	}
	
	/**
	 * Load RecordSet for updating SafeTreeSpecies
	 * @param species Reference to SafeTreeSpecies object
	 */
	public  void load (SafeTreeSpecies species) throws Exception {

		Set<String> requiredParameters = new HashSet<>();
		requiredParameters.add("treeSpecies");
		requiredParameters.add("crownShape");
		requiredParameters.add("heightDbhAllometricCoeffA");
		requiredParameters.add("heightDbhAllometricCoeffB");
		requiredParameters.add("crownDbhAllometricCoeffA");
		requiredParameters.add("crownDbhAllometricCoeffB");
		requiredParameters.add("stemDbhAllometricCoeffA");
		requiredParameters.add("stemDbhAllometricCoeffB");
		requiredParameters.add("stemDbhAllometricCoeffC");
		requiredParameters.add("dcbFromDbhAllometricCoeff");
		requiredParameters.add("leafAreaCrownVolCoefA");
		requiredParameters.add("leafAreaCrownVolCoefB");
		requiredParameters.add("stumpToStemBiomassRatio");
		requiredParameters.add("maxCrownRadiusInc");
		requiredParameters.add("maxHeightInc");
		requiredParameters.add("maxDbhInc");
		requiredParameters.add("phenologyType");
		requiredParameters.add("nbCohortMax");

		requiredParameters.add("leafExpansionDuration");
		requiredParameters.add("budBurstToLeafFallDuration");
		requiredParameters.add("leafFallDuration");
		requiredParameters.add("leafFallFrostThreshold");
		requiredParameters.add("fruitCompartment");
		requiredParameters.add("coldRequirement");
		requiredParameters.add("nitrogenFixation");
		requiredParameters.add("woodAreaDensity");
		requiredParameters.add("leafParAbsorption");
		requiredParameters.add("leafNirAbsorption");
		requiredParameters.add("clumpingCoef");
		requiredParameters.add("stemFlowCoefficient");
		requiredParameters.add("stemFlowMax");
		requiredParameters.add("wettability");
		requiredParameters.add("transpirationCoefficient");
		requiredParameters.add("leafLueMax");
		requiredParameters.add("lightCompetitionIndexMin");
		requiredParameters.add("leafAgeForLueMax");
		requiredParameters.add("leafPhotosynthesisEfficiencyTimeConstant");
		requiredParameters.add("woodCarbonContent");
		requiredParameters.add("leafCarbonContent");
		requiredParameters.add("leafMassArea");
		requiredParameters.add("woodDensity");
		requiredParameters.add("branchVolumeRatio");
		requiredParameters.add("imbalanceThreshold");
		requiredParameters.add("rsBelowGroundStressMethod");
		requiredParameters.add("lueStressMethod");
		requiredParameters.add("rsNoStressResponsiveness");
		requiredParameters.add("rsWaterStressResponsiveness");
		requiredParameters.add("rsNitrogenStressResponsiveness");
		requiredParameters.add("rsLightStressActivation");
		requiredParameters.add("rsNitrogenExcessStressActivation");
		requiredParameters.add("rsBelowGroundStressActivation");
		requiredParameters.add("lueWaterStressResponsiveness");
		requiredParameters.add("lueNitrogenStressResponsiveness");
		requiredParameters.add("senWaterStressResponsiveness");
		requiredParameters.add("senNitrogenStressResponsiveness");	
		requiredParameters.add("leafFrostStressTemperatureMin");		
		requiredParameters.add("leafFrostStressTemperatureMax");
		requiredParameters.add("lueTemperatureStressTMin");	
		requiredParameters.add("lueTemperatureStressTMax");	
		requiredParameters.add("lueTemperatureStressTOptMin");	
		requiredParameters.add("lueTemperatureStressTOptMax");	
		requiredParameters.add("co2EffectOnLueActivation");
		requiredParameters.add("co2EffectOnWueActivation");		
		requiredParameters.add("co2EffectOnLueHalfSaturationConstant");	
		requiredParameters.add("co2EffectIntrinsicWueSensitivity");	
		requiredParameters.add("co2ReferenceValue");	
		requiredParameters.add("maxTargetLfrRatioDailyVariation");
		requiredParameters.add("targetLfrRatioUpperDrift");
		requiredParameters.add("minTargetLfrRatio");
		requiredParameters.add("maxTargetLfrRatio");
		requiredParameters.add("optiNCBranch");
		requiredParameters.add("optiNCFruit");
		requiredParameters.add("optiNCCoarseRoot");
		requiredParameters.add("optiNCFineRoot");
		requiredParameters.add("optiNCFoliage");
		requiredParameters.add("optiNCStem");
		requiredParameters.add("optiNCStump");
		requiredParameters.add("targetNCoefficient");
		requiredParameters.add("luxuryNCoefficient");
		requiredParameters.add("maxNSCUseFoliageFraction");
		requiredParameters.add("maxNSCUseFraction");
		requiredParameters.add("targetNSCFraction");
		requiredParameters.add("leafNRemobFraction");
		requiredParameters.add("rootNRemobFraction");
		requiredParameters.add("leafSenescenceRate");
		requiredParameters.add("cRAreaToFRLengthRatio");
		requiredParameters.add("initialTargetLfrRatio");
		requiredParameters.add("coarseRootAnoxiaResistance");
		requiredParameters.add("specificRootLength");
		requiredParameters.add("fineRootLifespan");
		requiredParameters.add("fineRootAnoxiaLifespan");
		requiredParameters.add("colonisationThreshold");
		requiredParameters.add("colonisationFraction");
		requiredParameters.add("horizontalPreference");
		requiredParameters.add("geotropismFactor");
		requiredParameters.add("localWaterUptakeFactor");
		requiredParameters.add("sinkDistanceEffect");
		requiredParameters.add("localNitrogenUptakeFactor");
		requiredParameters.add("coarseRootTopologyType");
		requiredParameters.add("treeRootDiameter");
		requiredParameters.add("treeRootConductivity");
		requiredParameters.add("treeAlpha");
		requiredParameters.add("treeMinTranspirationPotential");
		requiredParameters.add("treeMaxTranspirationPotential");
		requiredParameters.add("treeBufferPotential");
		requiredParameters.add("treeLongitudinalResistantFactor");
		requiredParameters.add("treeHarmonicWeightedMean");
		requiredParameters.add("selfPruningEffet");
		requiredParameters.add("selfPruningLCIThreshold");
		requiredParameters.add("selfPruningHeightRatio");
		requiredParameters.add("selfPruningNbrDaysShade");
		requiredParameters.add("selfPruningNbrYearsForBranchesFullDecay");
		

		for (Iterator<Record> i = this.iterator (); i.hasNext ();) {
			Record record = i.next ();

		 	if (record instanceof SafeTreeFormat.KeyRecord) {

				SafeTreeFormat.KeyRecord r = (SafeTreeFormat.KeyRecord) record;

				if (r.key.equals ("treeSpecies")) {
					treeSpecies = r.value;
					requiredParameters.remove("treeSpecies");
					
				} else if (r.key.equals ("crownShape")) {
					crownShape = r.getIntValue ();
					requiredParameters.remove("crownShape");
					
				} else if  (r.key.equals ("ellipsoidTruncationRatio")) {
					ellipsoidTruncationRatio = r.getDoubleValue ();			
					requiredParameters.remove("ellipsoidTruncationRatio");
					
				} else if  (r.key.equals ("crownDbhAllometricCoeffA")) {
					crownDbhAllometricCoeffA = r.getDoubleValue ();
					requiredParameters.remove("crownDbhAllometricCoeffA");
					
				} else if  (r.key.equals ("crownDbhAllometricCoeffB")) {
					crownDbhAllometricCoeffB = r.getDoubleValue ();
					requiredParameters.remove("crownDbhAllometricCoeffB");
					
				} else if  (r.key.equals ("heightDbhAllometricCoeffA")) {
					heightDbhAllometricCoeffA = r.getDoubleValue ();
					requiredParameters.remove("heightDbhAllometricCoeffA");
					
				} else if  (r.key.equals ("heightDbhAllometricCoeffB")) {
					heightDbhAllometricCoeffB = r.getDoubleValue ();
					requiredParameters.remove("heightDbhAllometricCoeffB");
					
				} else if  (r.key.equals ("stemDbhAllometricCoeffA")) {
					stemDbhAllometricCoeffA = r.getDoubleValue ();
					requiredParameters.remove("stemDbhAllometricCoeffA");
					
				} else if  (r.key.equals ("stemDbhAllometricCoeffB")) {
					stemDbhAllometricCoeffB = r.getDoubleValue ();
					requiredParameters.remove("stemDbhAllometricCoeffB");
					
				} else if  (r.key.equals ("stemDbhAllometricCoeffC")) {
					stemDbhAllometricCoeffC = r.getDoubleValue ();
					requiredParameters.remove("stemDbhAllometricCoeffC");
					
				} else if  (r.key.equals ("stumpToStemBiomassRatio")) {
					stumpToStemBiomassRatio = r.getDoubleValue ();	
					requiredParameters.remove("stumpToStemBiomassRatio");
					
				} else if  (r.key.equals ("dcbFromDbhAllometricCoeff")) {
					dcbFromDbhAllometricCoeff = r.getDoubleValue ();	
					requiredParameters.remove("dcbFromDbhAllometricCoeff");
					
				} else if  (r.key.equals ("cRAreaToFRLengthRatio")) {
					cRAreaToFRLengthRatio = r.getDoubleValue ();
					requiredParameters.remove("cRAreaToFRLengthRatio");
					
				} else if  (r.key.equals ("initialTargetLfrRatio")) {
					initialTargetLfrRatio = r.getDoubleValue ();
					requiredParameters.remove("initialTargetLfrRatio");					
	
				} else if (r.key.equals ("leafAreaCrownVolCoefA")) {
					leafAreaCrownVolCoefA = r.getDoubleValue ();
					requiredParameters.remove("leafAreaCrownVolCoefA");
					
				} else if (r.key.equals ("leafAreaCrownVolCoefB")) {
					leafAreaCrownVolCoefB = r.getDoubleValue ();
					requiredParameters.remove("leafAreaCrownVolCoefB");
					
				} else if (r.key.equals ("woodAreaDensity")) {
					woodAreaDensity = r.getDoubleValue ();
					requiredParameters.remove("woodAreaDensity");
					
				} else if  (r.key.equals ("leafParAbsorption")) {
					leafParAbsorption = r.getDoubleValue ();
					requiredParameters.remove("leafParAbsorption");
					
				} else if  (r.key.equals ("leafNirAbsorption")) {
					leafNirAbsorption = r.getDoubleValue ();
					requiredParameters.remove("leafNirAbsorption");
					
				} else if  (r.key.equals ("clumpingCoef")) {
					clumpingCoef = r.getDoubleValue ();
					requiredParameters.remove("clumpingCoef");
					
				} else if  (r.key.equals ("phenologyType")) {
					phenologyType = r.getIntValue ();
					requiredParameters.remove("phenologyType");
					
					//tropical tree (faidherbia) 
					if(phenologyType==3){
						requiredParameters.add("budburstDelayFromLastBudburst");
						requiredParameters.add("budburstDelayFromMinWaterTable");
					}
					//cold deciduous or evergreen
					else {
						requiredParameters.add("budBurstTempAccumulationDateStart");
						requiredParameters.add("budBurstTriggerTemp");
						requiredParameters.add("budBurstTempThreshold");
					}
	
				} else if  (r.key.equals ("nbCohortMax")) {
					nbCohortMax = r.getIntValue ();
					requiredParameters.remove("nbCohortMax");				
					
				} else if  (r.key.equals ("budBurstTempAccumulationDateStart")) {
					//julian days replaced by MM-JJ (IL 25/05/2023)
					budBurstTempAccumulationDateStart = getJulianDay (r.value);
					requiredParameters.remove("budBurstTempAccumulationDateStart");

				} else if  (r.key.equals ("budBurstTempThreshold")) {
					budBurstTempThreshold = r.getDoubleValue ();
					requiredParameters.remove("budBurstTempThreshold");

				} else if  (r.key.equals ("budBurstTriggerTemp")) {
					budBurstTriggerTemp = r.getDoubleValue ();
					requiredParameters.remove("budBurstTriggerTemp");
					
				} else if  (r.key.equals ("leafExpansionDuration")) {
					leafExpansionDuration = r.getIntValue ();
					requiredParameters.remove("leafExpansionDuration");
					
				} else if  (r.key.equals ("budBurstToLeafFallDuration")) {
					budBurstToLeafFallDuration=r.getIntValue(); // gt - 09.10.2009
					requiredParameters.remove("budBurstToLeafFallDuration");
					
				} else if  (r.key.equals ("leafFallDuration")) {
					leafFallDuration = r.getIntValue ();
					requiredParameters.remove("leafFallDuration");
					
				} else if  (r.key.equals ("leafFallFrostThreshold")) {
					leafFallFrostThreshold = r.getDoubleValue ();
					requiredParameters.remove("leafFallFrostThreshold");

				} else if  (r.key.equals ("stemFlowCoefficient")) {
					stemFlowCoefficient = r.getDoubleValue ();
					requiredParameters.remove("stemFlowCoefficient");
					
				} else if  (r.key.equals ("stemFlowMax")) {
					stemFlowMax = r.getDoubleValue ();
					requiredParameters.remove("stemFlowMax");
					
				} else if  (r.key.equals ("wettability")) {
					wettability = r.getDoubleValue ();
					requiredParameters.remove("wettability");
					
				} else if  (r.key.equals ("transpirationCoefficient")) {
					transpirationCoefficient = r.getDoubleValue ();
					requiredParameters.remove("transpirationCoefficient");
					
				} else if  (r.key.equals ("leafLueMax")) {
					leafLueMax = r.getDoubleValue ();
					requiredParameters.remove("leafLueMax");
					
				} else if  (r.key.equals ("lightCompetitionIndexMin")) {
					lightCompetitionIndexMin = r.getDoubleValue ();
					requiredParameters.remove("lightCompetitionIndexMin");					
					
				} else if  (r.key.equals("leafAgeForLueMax")){
					leafAgeForLueMax = r.getIntValue();
					requiredParameters.remove("leafAgeForLueMax");
					
				} else if  (r.key.equals("leafPhotosynthesisEfficiencyTimeConstant")){
					leafPhotosynthesisEfficiencyTimeConstant = r.getDoubleValue();
					requiredParameters.remove("leafPhotosynthesisEfficiencyTimeConstant");
					
				} else if  (r.key.equals ("leafCarbonContent")) {
					leafCarbonContent = r.getDoubleValue ();
					requiredParameters.remove("leafCarbonContent");
					
				} else if  (r.key.equals ("leafMassArea")) {
					leafMassArea = r.getDoubleValue ();
					requiredParameters.remove("leafMassArea");
					
				} else if  (r.key.equals ("luxuryNCoefficient")) {
					luxuryNCoefficient = r.getDoubleValue ();
					requiredParameters.remove("luxuryNCoefficient");
					
				} else if  (r.key.equals ("targetNCoefficient")) {
					targetNCoefficient = r.getDoubleValue ();
					requiredParameters.remove("targetNCoefficient");
					
				} else if  (r.key.equals ("maxNSCUseFoliageFraction")) {
					maxNSCUseFoliageFraction = r.getDoubleValue ();
					requiredParameters.remove("maxNSCUseFoliageFraction");
					
				} else if  (r.key.equals ("rootNRemobFraction")) {
					rootNRemobFraction = r.getDoubleValue ();
					requiredParameters.remove("rootNRemobFraction");
					
				} else if  (r.key.equals ("leafNRemobFraction")) {
					leafNRemobFraction = r.getDoubleValue ();
					requiredParameters.remove("leafNRemobFraction");
					
				} else if  (r.key.equals ("targetNSCFraction")) {
					targetNSCFraction = r.getDoubleValue ();
					requiredParameters.remove("targetNSCFraction");
					
				} else if  (r.key.equals ("maxNSCUseFraction")) {
					maxNSCUseFraction = r.getDoubleValue ();
					requiredParameters.remove("maxNSCUseFraction");
					
				} else if  (r.key.equals ("rsBelowGroundStressMethod")) {
					rsBelowGroundStressMethod = r.getIntValue ();
					requiredParameters.remove("rsBelowGroundStressMethod");
					
				} else if  (r.key.equals ("lueStressMethod")) {
					lueStressMethod = r.getIntValue ();
					requiredParameters.remove("lueStressMethod");	
					
				} else if  (r.key.equals ("rsNoStressResponsiveness")) {
					rsNoStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("rsNoStressResponsiveness");
					
				} else if  (r.key.equals ("rsWaterStressResponsiveness")) {
					rsWaterStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("rsWaterStressResponsiveness");
					
				} else if  (r.key.equals ("rsNitrogenStressResponsiveness")) {
					rsNitrogenStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("rsNitrogenStressResponsiveness");

				} else if  (r.key.equals ("rsBelowGroundStressActivation")) {
					rsBelowGroundStressActivation = false;
					int b  = r.getIntValue ();
					if (b==0) rsBelowGroundStressActivation= false;
					if (b==1) rsBelowGroundStressActivation= true;
					requiredParameters.remove("rsBelowGroundStressActivation");	
					
				} else if  (r.key.equals ("rsLightStressActivation")) {
					rsLightStressActivation = false;
					int b  = r.getIntValue ();
					if (b==0) rsLightStressActivation= false;
					if (b==1) rsLightStressActivation= true;
					requiredParameters.remove("rsLightStressActivation");					
					
				} else if  (r.key.equals ("rsNitrogenExcessStressActivation")) {
					rsNitrogenExcessStressActivation  = false;
					int b  = r.getIntValue ();
					if (b==0) rsNitrogenExcessStressActivation = false;
					if (b==1) rsNitrogenExcessStressActivation = true;
					requiredParameters.remove("rsNitrogenExcessStressActivation");						
					
				} else if  (r.key.equals ("lueWaterStressResponsiveness")) {
					lueWaterStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("lueWaterStressResponsiveness");
					
				} else if  (r.key.equals ("lueNitrogenStressResponsiveness")) {
					lueNitrogenStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("lueNitrogenStressResponsiveness");
					
				} else if  (r.key.equals ("senWaterStressResponsiveness")) {
					senWaterStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("senWaterStressResponsiveness");
					
				} else if  (r.key.equals ("senNitrogenStressResponsiveness")) {
					senNitrogenStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("senNitrogenStressResponsiveness");					
					
				} else if  (r.key.equals ("leafFrostStressTemperatureMin")) {
					leafFrostStressTemperatureMin = r.getDoubleValue ();
					requiredParameters.remove("leafFrostStressTemperatureMin");					
				} else if  (r.key.equals ("leafFrostStressTemperatureMax")) {
					leafFrostStressTemperatureMax = r.getDoubleValue ();
					requiredParameters.remove("leafFrostStressTemperatureMax");
					
				} else if  (r.key.equals ("lueTemperatureStressTMin")) {
					lueTemperatureStressTMin = r.getDoubleValue ();
					requiredParameters.remove("lueTemperatureStressTMin");		
					
				} else if  (r.key.equals ("lueTemperatureStressTMax")) {
					lueTemperatureStressTMax = r.getDoubleValue ();
					requiredParameters.remove("lueTemperatureStressTMax");
					
				} else if  (r.key.equals ("lueTemperatureStressTOptMin")) {
					lueTemperatureStressTOptMin = r.getDoubleValue ();
					requiredParameters.remove("lueTemperatureStressTOptMin");	
					
				} else if  (r.key.equals ("lueTemperatureStressTOptMax")) {
					lueTemperatureStressTOptMax = r.getDoubleValue ();
					requiredParameters.remove("lueTemperatureStressTOptMax");

				} else if  (r.key.equals ("co2EffectOnLueActivation")) {
					co2EffectOnLueActivation  = false;
					int b  = r.getIntValue ();
					if (b==0) co2EffectOnLueActivation = false;
					if (b==1) co2EffectOnLueActivation = true;
					requiredParameters.remove("co2EffectOnLueActivation");	
					
				} else if  (r.key.equals ("co2EffectOnWueActivation")) {
					co2EffectOnWueActivation  = false;
					int b  = r.getIntValue ();
					if (b==0) co2EffectOnWueActivation = false;
					if (b==1) co2EffectOnWueActivation = true;
					requiredParameters.remove("co2EffectOnWueActivation");	

				} else if  (r.key.equals ("co2EffectOnLueHalfSaturationConstant")) {
					co2EffectOnLueHalfSaturationConstant = r.getDoubleValue ();
					requiredParameters.remove("co2EffectOnLueHalfSaturationConstant");
					
				} else if  (r.key.equals ("co2EffectIntrinsicWueSensitivity")) {
					co2EffectIntrinsicWueSensitivity = r.getDoubleValue ();
					requiredParameters.remove("co2EffectIntrinsicWueSensitivity");
					
				} else if  (r.key.equals ("co2ReferenceValue")) {
					co2ReferenceValue = r.getDoubleValue ();
					requiredParameters.remove("co2ReferenceValue");
					
				} else if  (r.key.equals ("maxTargetLfrRatioDailyVariation")) {
					maxTargetLfrRatioDailyVariation = r.getDoubleValue ();
					requiredParameters.remove("maxTargetLfrRatioDailyVariation");
					
				} else if (r.key.equals ("minTargetLfrRatio")){
					minTargetLfrRatio = r.getDoubleValue ();
					requiredParameters.remove("minTargetLfrRatio");
					
				} else if (r.key.equals ("maxTargetLfrRatio")){
					maxTargetLfrRatio = r.getDoubleValue ();
					requiredParameters.remove("maxTargetLfrRatio");
					
				} else if (r.key.equals ("targetLfrRatioUpperDrift")){
					targetLfrRatioUpperDrift = r.getDoubleValue ();		
					requiredParameters.remove("targetLfrRatioUpperDrift");
					
				} else if  (r.key.equals ("optiNCBranch")) {
					optiNCBranch = r.getDoubleValue ();
					requiredParameters.remove("optiNCBranch");

				} else if  (r.key.equals ("optiNCFruit")) {
					optiNCFruit = r.getDoubleValue ();
					requiredParameters.remove("optiNCFruit");
					
				} else if  (r.key.equals ("optiNCCoarseRoot")) {
					optiNCCoarseRoot = r.getDoubleValue ();
					requiredParameters.remove("optiNCCoarseRoot");
					
				} else if  (r.key.equals ("optiNCFineRoot")) {
					optiNCFineRoot = r.getDoubleValue ();
					requiredParameters.remove("optiNCFineRoot");
					
				} else if  (r.key.equals ("optiNCFoliage")) {
					optiNCFoliage = r.getDoubleValue ();
					requiredParameters.remove("optiNCFoliage");
					
				} else if  (r.key.equals ("optiNCStem")) {
					optiNCStem = r.getDoubleValue ();
					requiredParameters.remove("optiNCStem");
					
				} else if  (r.key.equals ("optiNCStump")) {
					optiNCStump = r.getDoubleValue ();
					requiredParameters.remove("optiNCStump");
					
				} else if  (r.key.equals ("woodDensity")) {
					woodDensity = r.getDoubleValue ();
					requiredParameters.remove("woodDensity");
					
				} else if  (r.key.equals ("branchVolumeRatio")) {
					branchVolumeRatio = r.getDoubleValue ();
					requiredParameters.remove("branchVolumeRatio");
					
				} else if  (r.key.equals ("woodCarbonContent")) {
					woodCarbonContent = r.getDoubleValue ();
					requiredParameters.remove("woodCarbonContent");
					
				} else if  (r.key.equals ("maxCrownRadiusInc")) {
					maxCrownRadiusInc  = r.getDoubleValue ();
					requiredParameters.remove("maxCrownRadiusInc");
					
					
				} else if  (r.key.equals ("maxDbhInc")) {
					maxDbhInc  = r.getDoubleValue ();
					requiredParameters.remove("maxDbhInc");				
					
					
				} else if  (r.key.equals ("maxHeightInc")) {
					maxHeightInc  = r.getDoubleValue ();	
					requiredParameters.remove("maxHeightInc");
					
				} else if  (r.key.equals ("leafSenescenceRate")) {
					leafSenescenceRate = r.getDoubleValue ();
					requiredParameters.remove("leafSenescenceRate");

				} else if  (r.key.equals ("imbalanceThreshold")) {
					imbalanceThreshold = r.getDoubleValue ();
					requiredParameters.remove("imbalanceThreshold");
					
				} else if  (r.key.equals ("coarseRootAnoxiaResistance")) {
					coarseRootAnoxiaResistance = r.getIntValue ();
					requiredParameters.remove("coarseRootAnoxiaResistance");
					
				} else if  (r.key.equals ("specificRootLength")) {
					specificRootLength = r.getDoubleValue ();
					requiredParameters.remove("specificRootLength");
					
				} else if  (r.key.equals ("colonisationThreshold")) {
					colonisationThreshold = r.getDoubleValue ();
					requiredParameters.remove("colonisationThreshold");
					
				} else if  (r.key.equals ("colonisationFraction")) {
					colonisationFraction = r.getDoubleValue ();
					requiredParameters.remove("colonisationFraction");			
					
				} else if  (r.key.equals ("fineRootLifespan")) {
					fineRootLifespan = r.getDoubleValue ();
					requiredParameters.remove("fineRootLifespan");
					
				} else if  (r.key.equals ("fineRootAnoxiaLifespan")) {
					fineRootAnoxiaLifespan = r.getDoubleValue ();
					requiredParameters.remove("fineRootAnoxiaLifespan");
					
				} else if  (r.key.equals ("horizontalPreference")) {
					horizontalPreference = r.getDoubleValue ();
					requiredParameters.remove("horizontalPreference");
					
				} else if  (r.key.equals ("geotropismFactor")) {
					geotropismFactor = r.getDoubleValue ();
					requiredParameters.remove("geotropismFactor");
					
				} else if  (r.key.equals ("localWaterUptakeFactor")) {
					localWaterUptakeFactor = r.getDoubleValue ();
					requiredParameters.remove("localWaterUptakeFactor");
					
				} else if  (r.key.equals ("sinkDistanceEffect")) {
					sinkDistanceEffect = r.getDoubleValue ();
					requiredParameters.remove("sinkDistanceEffect");
					
				} else if  (r.key.equals ("localNitrogenUptakeFactor")) {
					localNitrogenUptakeFactor = r.getDoubleValue ();
					requiredParameters.remove("localNitrogenUptakeFactor");
					
				} else if  (r.key.equals ("coarseRootTopologyType")) {
					coarseRootTopologyType = r.getIntValue ();
					requiredParameters.remove("coarseRootTopologyType");
					
				} else if  (r.key.equals ("treeRootDiameter")) {
					treeRootDiameter = r.getDoubleValue ();
					requiredParameters.remove("treeRootDiameter");
					
				} else if  (r.key.equals ("treeRootConductivity")) {
					treeRootConductivity = r.getDoubleValue ();
					requiredParameters.remove("treeRootConductivity");
					
				} else if  (r.key.equals ("treeAlpha")) {
					treeAlpha = r.getDoubleValue ();
					requiredParameters.remove("treeAlpha");
					
				} else if  (r.key.equals ("treeMinTranspirationPotential")) {
					treeMinTranspirationPotential = r.getDoubleValue ();
					requiredParameters.remove("treeMinTranspirationPotential");
					
				} else if  (r.key.equals ("treeMaxTranspirationPotential")) {
					treeMaxTranspirationPotential = r.getDoubleValue ();
					requiredParameters.remove("treeMaxTranspirationPotential");
					
				} else if  (r.key.equals ("treeBufferPotential")) {
					treeBufferPotential = r.getDoubleValue ();
					requiredParameters.remove("treeBufferPotential");
					
				} else if  (r.key.equals ("treeLongitudinalResistantFactor")) {
					treeLongitudinalResistantFactor = r.getDoubleValue ();
					requiredParameters.remove("treeLongitudinalResistantFactor");
				
				} else if  (r.key.equals ("treeHarmonicWeightedMean")) {
					treeHarmonicWeightedMean = r.getDoubleValue ();
					requiredParameters.remove("treeHarmonicWeightedMean");

	//COLD TEMPERATURE 				
				} else if  (r.key.equals ("coldRequirement")) {
					int j = r.getIntValue ();
					if (j==0) coldRequirement=false;
					else {
						coldRequirement=true;	
						requiredParameters.add("coldTempAccumulationDateStart");
						requiredParameters.add("coldTempThreshold");
						requiredParameters.add("coldBudBurstTriggerTemp");
						requiredParameters.add("coldBudBurstTriggerParamA");
						requiredParameters.add("coldBudBurstTriggerParamB");
						requiredParameters.add("coldBudBurstTriggerParamC");
						requiredParameters.add("coldBudBurstTriggerParamE");
					}
					requiredParameters.remove("coldRequirement");
					
				} else if  (r.key.equals ("coldTempAccumulationDateStart")) {
					//julian days replaced by MM-JJ (IL 25/05/2023)
					coldTempAccumulationDateStart = getJulianDay (r.value);
					requiredParameters.remove("coldTempAccumulationDateStart");		
				} else if  (r.key.equals ("coldTempThreshold")) {
					coldTempThreshold = r.getDoubleValue ();
					requiredParameters.remove("coldTempThreshold");
				} else if  (r.key.equals ("coldBudBurstTriggerTemp")) {
					coldBudBurstTriggerTemp = r.getDoubleValue ();
					requiredParameters.remove("coldBudBurstTriggerTemp");
				} else if  (r.key.equals ("coldBudBurstTriggerParamA")) {
					coldBudBurstTriggerParamA = r.getDoubleValue ();
					requiredParameters.remove("coldBudBurstTriggerParamA");
				} else if  (r.key.equals ("coldBudBurstTriggerParamB")) {
					coldBudBurstTriggerParamB = r.getDoubleValue ();
					requiredParameters.remove("coldBudBurstTriggerParamB");
				} else if  (r.key.equals ("coldBudBurstTriggerParamC")) {
					coldBudBurstTriggerParamC = r.getDoubleValue ();
					requiredParameters.remove("coldBudBurstTriggerParamC");
				} else if  (r.key.equals ("coldBudBurstTriggerParamE")) {
					coldBudBurstTriggerParamE = r.getDoubleValue ();
					requiredParameters.remove("coldBudBurstTriggerParamE");					
	//FRUITS 				
				} else if  (r.key.equals ("fruitCompartment")) {
					int j = r.getIntValue ();
					if (j==0) fruitCompartment=false;
					else {
						fruitCompartment=true;	
						requiredParameters.add("floweringTempAccumulationDateStart");
						requiredParameters.add("floweringTriggerTemp");
						requiredParameters.add("floweringTempThreshold");						
						requiredParameters.add("fruitSettingTriggerTemp");
						requiredParameters.add("fruitGrowthTriggerTemp");
						requiredParameters.add("fruitVeraisonTriggerTemp");
						requiredParameters.add("fruitHeatStressTemperatureMin");		
						requiredParameters.add("fruitHeatStressTemperatureMax");
						requiredParameters.add("fruitFrostStressTemperatureMin");		
						requiredParameters.add("fruitFrostStressTemperatureMax");
						requiredParameters.add("fruitMaxDryMatterAllocation");		
						requiredParameters.add("fruitAllocationFraction");
						requiredParameters.add("fruitCarbonStressDateStart");
						requiredParameters.add("fruitDryToFreshMatterWeight");
						requiredParameters.add("fruitDryMaterDensity");
						requiredParameters.add("fruitOilConversionCoeffA");
						requiredParameters.add("fruitOilConversionCoeffB");
						requiredParameters.add("fruitOilConversionCoeffC");
						requiredParameters.add("fruitOilDensity");		
						requiredParameters.add("fruitFirstYear");
						requiredParameters.add("fruitLeafArea");
						requiredParameters.add("fruitingConfortThreshold");
						requiredParameters.add("fruitingTotalStressThreshold");

	
					}
					requiredParameters.remove("fruitCompartment");

				} else if  (r.key.equals ("floweringTempAccumulationDateStart")) {
					//julian days replaced by MM-JJ (IL 25/05/2023)
					floweringTempAccumulationDateStart = getJulianDay (r.value);
					requiredParameters.remove("floweringTempAccumulationDateStart");
					
				} else if  (r.key.equals ("floweringTempThreshold")) {
					floweringTempThreshold = r.getDoubleValue ();
					requiredParameters.remove("floweringTempThreshold");
					
				} else if  (r.key.equals ("floweringTriggerTemp")) {
					floweringTriggerTemp = r.getDoubleValue ();	
					requiredParameters.remove("floweringTriggerTemp");
					
				} else if  (r.key.equals ("fruitSettingTriggerTemp")) {
					fruitSettingTriggerTemp = r.getDoubleValue ();	
					requiredParameters.remove("fruitSettingTriggerTemp");
					
				} else if  (r.key.equals ("fruitGrowthTriggerTemp")) {
					fruitGrowthTriggerTemp = r.getDoubleValue ();	
					requiredParameters.remove("fruitGrowthTriggerTemp");
					
				} else if  (r.key.equals ("fruitVeraisonTriggerTemp")) {
					fruitVeraisonTriggerTemp = r.getDoubleValue ();	
					requiredParameters.remove("fruitVeraisonTriggerTemp");
					
				} else if  (r.key.equals ("fruitHeatStressTemperatureMin")) {
					fruitHeatStressTemperatureMin = r.getDoubleValue ();
					requiredParameters.remove("fruitHeatStressTemperatureMin");					
				} else if  (r.key.equals ("fruitHeatStressTemperatureMax")) {
					fruitHeatStressTemperatureMax = r.getDoubleValue ();
					requiredParameters.remove("fruitHeatStressTemperatureMax");

				} else if  (r.key.equals ("fruitFrostStressTemperatureMin")) {
					fruitFrostStressTemperatureMin = r.getDoubleValue ();
					requiredParameters.remove("fruitFrostStressTemperatureMin");					
				} else if  (r.key.equals ("fruitFrostStressTemperatureMax")) {
					fruitFrostStressTemperatureMax = r.getDoubleValue ();
					requiredParameters.remove("fruitFrostStressTemperatureMax");
					
				} else if  (r.key.equals ("fruitMaxDryMatterAllocation")) {
					fruitMaxDryMatterAllocation = r.getDoubleValue ();
					requiredParameters.remove("fruitMaxDryMatterAllocation");
					
				} else if  (r.key.equals ("fruitAllocationFraction")) {
					fruitAllocationFraction = r.getDoubleValue ();
					requiredParameters.remove("fruitAllocationFraction");
					
				} else if  (r.key.equals ("fruitCarbonStressDateStart")) {
					//julian days replaced by MM-JJ (IL 25/05/2023)
					fruitCarbonStressDateStart = getJulianDay (r.value);
					requiredParameters.remove("fruitCarbonStressDateStart");												
					
				} else if  (r.key.equals ("fruitDryToFreshMatterWeight")) {
					fruitDryToFreshMatterWeight = r.getDoubleValue ();
					requiredParameters.remove("fruitDryToFreshMatterWeight");
					
				} else if  (r.key.equals ("fruitDryMaterDensity")) {
					fruitDryMaterDensity = r.getDoubleValue ();
					requiredParameters.remove("fruitDryMaterDensity");					
					
				} else if  (r.key.equals ("fruitOilConversionCoeffA")) {
					fruitOilConversionCoeffA = r.getDoubleValue ();
					requiredParameters.remove("fruitOilConversionCoeffA");					

				} else if  (r.key.equals ("fruitOilConversionCoeffB")) {
					fruitOilConversionCoeffB = r.getDoubleValue ();
					requiredParameters.remove("fruitOilConversionCoeffB");	
					
				} else if  (r.key.equals ("fruitOilConversionCoeffC")) {
					fruitOilConversionCoeffC = r.getDoubleValue ();
					requiredParameters.remove("fruitOilConversionCoeffC");	
					
				} else if  (r.key.equals ("fruitOilDensity")) {
					fruitOilDensity = r.getDoubleValue ();
					requiredParameters.remove("fruitOilDensity");	
					
				} else if  (r.key.equals ("fruitFirstYear")) {
					fruitFirstYear = r.getIntValue ();
					requiredParameters.remove("fruitFirstYear");
				} else if  (r.key.equals ("fruitLeafArea")) {
					fruitLeafArea = r.getDoubleValue ();
					requiredParameters.remove("fruitLeafArea");
				} else if  (r.key.equals ("fruitingConfortThreshold")) {
					fruitingConfortThreshold = r.getDoubleValue ();
					requiredParameters.remove("fruitingConfortThreshold");					
				} else if  (r.key.equals ("fruitingTotalStressThreshold")) {
					fruitingTotalStressThreshold = r.getDoubleValue ();
					requiredParameters.remove("fruitingTotalStressThreshold");					

					
//BNF		
				} else if  (r.key.equals ("nitrogenFixation")) {
					int j = r.getIntValue ();
					if (j==0) nitrogenFixation=false;
					else {
						nitrogenFixation=true;	
						requiredParameters.add("bnfTempAccumulationDateStart");
						requiredParameters.add("bnfTempThreshold");
						requiredParameters.add("bnfStartTriggerTemp");
						requiredParameters.add("bnfExpansionDuration");
						requiredParameters.add("bnfStartToEndDuration");
						requiredParameters.add("bnfMaxDepth");
						requiredParameters.add("bnfNodulationInhibitionThreshold");
						requiredParameters.add("bnfCardinalTemp1");
						requiredParameters.add("bnfCardinalTemp2");
						requiredParameters.add("bnfCardinalTemp3");
						requiredParameters.add("bnfCardinalTemp4");
						requiredParameters.add("bnfFullNoduleActivityThreshold");
						requiredParameters.add("bnfNullNoduleActivityThreshold");
						requiredParameters.add("bnfAirTemperatureThreshold");
						requiredParameters.add("bnfOptimalTemperatureDifference");
						requiredParameters.add("bnfFixMaxVeg");
						requiredParameters.add("bnfFixMaxRepro");
						requiredParameters.add("selfPruningLightCompetitionIndexThreshold");
						requiredParameters.add("selfPruningHeightProportion");
						requiredParameters.add("selfPruningTriggerDays");

					}
					requiredParameters.remove("nitrogenFixation");	

				} else if  (r.key.equals ("bnfTempAccumulationDateStart")) {
					//julian days replaced by MM-JJ (IL 25/05/2023)
					bnfTempAccumulationDateStart = getJulianDay (r.value);
					requiredParameters.remove("bnfTempAccumulationDateStart");
					
				} else if  (r.key.equals ("bnfTempThreshold")) {
					bnfTempThreshold = r.getDoubleValue ();
					requiredParameters.remove("bnfTempThreshold");
					
				} else if  (r.key.equals ("bnfStartTriggerTemp")) {
					bnfStartTriggerTemp = r.getDoubleValue ();
					requiredParameters.remove("bnfStartTriggerTemp");
									
				} else if  (r.key.equals ("bnfExpansionDuration")) {
					bnfExpansionDuration = r.getIntValue ();
					requiredParameters.remove("bnfExpansionDuration");
					
				} else if  (r.key.equals ("bnfStartToEndDuration")) {
					bnfStartToEndDuration = r.getIntValue ();
					requiredParameters.remove("bnfStartToEndDuration");

				} else if  (r.key.equals ("bnfMaxDepth")) {
					bnfMaxDepth = r.getDoubleValue ();
					requiredParameters.remove("bnfMaxDepth");
					
				} else if  (r.key.equals ("bnfNodulationInhibitionThreshold")) {
					bnfNodulationInhibitionThreshold = r.getDoubleValue ();
					requiredParameters.remove("bnfNodulationInhibitionThreshold");
					
				} else if  (r.key.equals ("bnfCardinalTemp1")) {
					bnfCardinalTemp1 = r.getDoubleValue ();
					requiredParameters.remove("bnfCardinalTemp1");
					
				} else if  (r.key.equals ("bnfCardinalTemp2")) {
					bnfCardinalTemp2 = r.getDoubleValue ();
					requiredParameters.remove("bnfCardinalTemp2");
					
				} else if  (r.key.equals ("bnfCardinalTemp3")) {
					bnfCardinalTemp3 = r.getDoubleValue ();
					requiredParameters.remove("bnfCardinalTemp3");
					
				} else if  (r.key.equals ("bnfCardinalTemp4")) {
					bnfCardinalTemp4 = r.getDoubleValue ();
					requiredParameters.remove("bnfCardinalTemp4");
					
				} else if  (r.key.equals ("bnfFullNoduleActivityThreshold")) {
					bnfFullNoduleActivityThreshold = r.getDoubleValue ();
					requiredParameters.remove("bnfFullNoduleActivityThreshold");
					
				} else if  (r.key.equals ("bnfNullNoduleActivityThreshold")) {
					bnfNullNoduleActivityThreshold = r.getDoubleValue ();
					requiredParameters.remove("bnfNullNoduleActivityThreshold");

				} else if  (r.key.equals ("bnfAirTemperatureThreshold")) {
					bnfAirTemperatureThreshold = r.getDoubleValue ();
					requiredParameters.remove("bnfAirTemperatureThreshold");
					
				} else if  (r.key.equals ("bnfOptimalTemperatureDifference")) {
					bnfOptimalTemperatureDifference = r.getDoubleValue ();
					requiredParameters.remove("bnfOptimalTemperatureDifference");

				} else if  (r.key.equals ("bnfFixMaxVeg")) {
					bnfFixMaxVeg = r.getDoubleValue ();
					requiredParameters.remove("bnfFixMaxVeg");
	
				} else if  (r.key.equals ("bnfFixMaxRepro")) {
					bnfFixMaxRepro = r.getDoubleValue ();
					requiredParameters.remove("bnfFixMaxRepro");

				} else if  (r.key.equals ("selfPruningEffet")) {
					int j = r.getIntValue ();
					if (j==0) selfPruningEffet = false;
					else selfPruningEffet = true;
					requiredParameters.remove("selfPruningEffet");
					
				} else if  (r.key.equals ("selfPruningLCIThreshold")) {
					selfPruningLCIThreshold = r.getDoubleValue ();
					requiredParameters.remove("selfPruningLCIThreshold");
					
				} else if  (r.key.equals ("selfPruningHeightRatio")) {
					selfPruningHeightRatio = r.getDoubleValue ();
					requiredParameters.remove("selfPruningHeightRatio");
					
				} else if  (r.key.equals ("selfPruningNbrDaysShade")) {
					selfPruningNbrDaysShade = r.getIntValue ();
					requiredParameters.remove("selfPruningNbrDaysShade");
					
				} else if  (r.key.equals ("selfPruningNbrYearsForBranchesFullDecay")) {
					selfPruningNbrYearsForBranchesFullDecay = r.getIntValue ();
					requiredParameters.remove("selfPruningNbrYearsForBranchesFullDecay");

				//add tropical tree (faidherbia)  (IL 21/01/2026)	

				} else if  (r.key.equals ("budburstDelayFromLastBudburst")) {
					budburstDelayFromLastBudburst =  r.getIntValue ();
					requiredParameters.remove("budburstDelayFromLastBudburst");
				} else if  (r.key.equals ("budburstDelayFromMinWaterTable")) {
					budburstDelayFromMinWaterTable = r.getIntValue ();
					requiredParameters.remove("budburstDelayFromMinWaterTable");
			 	}
		 	}
		}


		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing tree species parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort

		}
		else {
			//updating directly the tree species object
			species.updateSpecies (treeSpecies, crownShape, ellipsoidTruncationRatio,
						heightDbhAllometricCoeffA, heightDbhAllometricCoeffB,
						crownDbhAllometricCoeffA, crownDbhAllometricCoeffB,
						stemDbhAllometricCoeffA, stemDbhAllometricCoeffB, stemDbhAllometricCoeffC, 
						dcbFromDbhAllometricCoeff,
						stumpToStemBiomassRatio,
						cRAreaToFRLengthRatio,
						initialTargetLfrRatio,
						leafAreaCrownVolCoefA, leafAreaCrownVolCoefB,
						woodAreaDensity, leafParAbsorption, leafNirAbsorption, clumpingCoef,
						phenologyType, nbCohortMax, 
						budBurstTempAccumulationDateStart, 						
						budBurstTempThreshold, 
						budBurstTriggerTemp,
						leafExpansionDuration, 
						budBurstToLeafFallDuration, leafFallDuration,
						leafFallFrostThreshold,
						stemFlowCoefficient, stemFlowMax, wettability,
						transpirationCoefficient,
						leafLueMax,
						leafAgeForLueMax,					
						leafPhotosynthesisEfficiencyTimeConstant,
						lightCompetitionIndexMin,
						leafCarbonContent,
						leafMassArea,
						luxuryNCoefficient,
						targetNCoefficient,
						rootNRemobFraction, leafNRemobFraction,
						targetNSCFraction,
						maxNSCUseFraction, 
						maxNSCUseFoliageFraction,						
						rsBelowGroundStressActivation,
						rsLightStressActivation, 
						rsNitrogenExcessStressActivation, 
						rsBelowGroundStressMethod,
						rsNoStressResponsiveness,
						rsWaterStressResponsiveness,rsNitrogenStressResponsiveness,
						maxTargetLfrRatioDailyVariation,targetLfrRatioUpperDrift,
						minTargetLfrRatio,maxTargetLfrRatio,
						lueStressMethod,
						lueWaterStressResponsiveness,lueNitrogenStressResponsiveness,
						lueTemperatureStressTMin, lueTemperatureStressTMax,
						lueTemperatureStressTOptMin, lueTemperatureStressTOptMax,
						senWaterStressResponsiveness,senNitrogenStressResponsiveness,
						leafFrostStressTemperatureMin, leafFrostStressTemperatureMax,
						co2EffectOnLueActivation, co2EffectOnWueActivation,
						co2EffectOnLueHalfSaturationConstant,	co2EffectIntrinsicWueSensitivity,co2ReferenceValue,
						optiNCBranch, optiNCCoarseRoot,	optiNCFineRoot,	optiNCFoliage, optiNCStem, optiNCStump, optiNCFruit, 
						woodDensity, branchVolumeRatio, woodCarbonContent, 
						maxCrownRadiusInc, maxHeightInc, maxDbhInc,
						imbalanceThreshold,
						leafSenescenceRate,
						coarseRootAnoxiaResistance,
						specificRootLength,
						colonisationThreshold,						
						colonisationFraction,					
						fineRootLifespan, fineRootAnoxiaLifespan, horizontalPreference, geotropismFactor,
						localWaterUptakeFactor, sinkDistanceEffect,
						localNitrogenUptakeFactor, 
						coarseRootTopologyType,
						treeRootDiameter, treeRootConductivity,
						treeAlpha,
						treeMinTranspirationPotential, treeMaxTranspirationPotential,
						treeBufferPotential, treeLongitudinalResistantFactor,
						treeHarmonicWeightedMean,
						selfPruningEffet,
						selfPruningLCIThreshold,
						selfPruningHeightRatio,
						selfPruningNbrDaysShade,
						selfPruningNbrYearsForBranchesFullDecay
						);


			//updating  the fruit species object
			if (fruitCompartment) {
				species.updateFruitSpecies (
							fruitCompartment,
							floweringTempAccumulationDateStart,
							floweringTempThreshold,
							floweringTriggerTemp,
							fruitSettingTriggerTemp,
							fruitGrowthTriggerTemp,
							fruitVeraisonTriggerTemp,
							fruitHeatStressTemperatureMin, fruitHeatStressTemperatureMax,
							fruitFrostStressTemperatureMin, fruitFrostStressTemperatureMax,					
							fruitMaxDryMatterAllocation, fruitAllocationFraction,fruitCarbonStressDateStart,						
							fruitDryToFreshMatterWeight, fruitDryMaterDensity,
							fruitOilConversionCoeffA,fruitOilConversionCoeffB,fruitOilConversionCoeffC,
							fruitOilDensity,
							fruitFirstYear,
							fruitLeafArea, 
							fruitingConfortThreshold,
							fruitingTotalStressThreshold,
							coldRequirement,
							coldTempAccumulationDateStart,
							coldTempThreshold,
							coldBudBurstTriggerTemp,
							coldBudBurstTriggerParamA,
							coldBudBurstTriggerParamB,
							coldBudBurstTriggerParamC,
							coldBudBurstTriggerParamE
							);
			}
			
			//updating  the BNF species object
			if (nitrogenFixation) {
				species.updateBnfSpecies (
						nitrogenFixation,
						bnfTempAccumulationDateStart,
						bnfTempThreshold,
						bnfStartTriggerTemp,
						bnfExpansionDuration,
						bnfStartToEndDuration,					
						bnfMaxDepth,
						bnfNodulationInhibitionThreshold,			
						bnfCardinalTemp1, 
						bnfCardinalTemp2,	
						bnfCardinalTemp3,		 
						bnfCardinalTemp4,	
						bnfFullNoduleActivityThreshold,
						bnfNullNoduleActivityThreshold,					
						bnfAirTemperatureThreshold,
						bnfOptimalTemperatureDifference,					
						bnfFixMaxVeg,
						bnfFixMaxRepro			
						);
				}
			
			//update tropical tree species
			if (phenologyType ==3) {
				species.updateTropicalSpecies(
						budburstDelayFromLastBudburst, 
						budburstDelayFromMinWaterTable);
			}
		}
	}
}
