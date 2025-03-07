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

import capsis.defaulttype.Species;

/**
 * Properties for tree species.
 *
 * @author : Isabelle LECOMTE  -  INRAE Montpellier France - (July 2002)
 */

public class SafeTreeSpecies implements  Species, Serializable, Cloneable {	// fc - 29.7.2004 + fc - 15.11.2004 - Species

	private static final long serialVersionUID = 1L;
	
	/*------------- CROWN SHAPE AND ALLOMETRIC PARAMETERS -------------------------*/
	private String name; 						//name of tree species name
	private String fileName; 					//name of tree species file name
	private int crownShape;						// 1=elipsoid 2=paraboloid
	private double ellipsoidTruncationRatio;	// for truncated ellipsoid (0=full ellipsoid)
	private double heightDbhAllometricCoeffA;	//height = a * dbh^b
	private double heightDbhAllometricCoeffB;
	private double crownDbhAllometricCoeffA;	//crownRadius = a * dbh + b (if crownShape = 1 ellipsoid)
	private double crownDbhAllometricCoeffB;
	private double stemDbhAllometricCoeffA;		//Ln(stemvolume) = a * ln(dbh) - b * ln(height) + c
	private double stemDbhAllometricCoeffB;
	private double stemDbhAllometricCoeffC;
	private double dcbFromDbhAllometricCoeff;
	private double stumpToStemBiomassRatio;
	
	/*-------------  ALLOMETRIC PARAMETERS FOR ROOTS C ALLOCATION -------------------------*/
	private double 	cRAreaToFRLengthRatio;	//Coarse Root Section Area To Fine Root Length Ratio (m2.m-1)
	private double  initialTargetLfrRatio;	//initial Leaf to FineRoots ratio

	/*---------------------PHENOLOGY VEGETATIVE (1 or 2) --------------------------------*/
	private int phenologyType;						//1=cold deciduous 2=evergreen
	private int nbCohortMax;						//number of leaves cohort for evergreen trees
	private int budBurstTempAccumulationDateStart;	//date to start accumulation of temperature for budburst (julian day)
	private double budBurstTempThreshold;	 		// threshold of effective temperature for cumulating degree day (in degree)
	private double budBurstTriggerTemp;			    //Accumulated temperature to trigger budburst (degree-day)
	private int leafExpansionDuration;				//date of end of leaf expansion (in no stress condition) (julian day)
	private int budBurstToLeafFallDuration;			// number of day to compute mean T to trigger leaf fall (day)	// gt - 09.10.2009
	private int leafFallDuration;					//usual duration of leaf fall (days)
	private double leafFallFrostThreshold;			//threshold for frost sensibility (degrees)
	//private int budBurstDelayAfterPollarding;		//number of days  to delay budburst if pollarding (nbr day)

	/*--------------------- PHENOLOGY COLD REQUIREMENT ---------------------------*/
	private boolean coldRequirement;				//if true : cold requiremnt phenology 
	private int coldTempAccumulationDateStart;	    	//date to start accumulation of cold temperature for budburst (julian day)
	private double coldTempThreshold;	 				//Threshold of effective temperature for cumulating cold requierement  (in degree)
	private double coldBudBurstTriggerTemp;			    //Accumulated cold temperature to trigger budburst (degree-day)
	private double coldBudBurstTriggerParamA;			//Parameter A to calculate the chilling unit
	private double coldBudBurstTriggerParamB;			//Parameter B to calculate the chilling unit
	private double coldBudBurstTriggerParamC;			//Parameter C to calculate the chilling unit
	private double coldBudBurstTriggerParamE;			//Parameter E to calculate the chilling unit

	/*---------------------FRUIT PHENOLOGY HEAT REQUIREMENT ---------------------------*/
	private boolean fruitCompartment;				//if true : fruit compartment 
	private int floweringTempAccumulationDateStart;	//date to start accumulation of temperature for flowering (julian day)
	private double floweringTempThreshold;			// threshold of effective temperature for cumulating degree day (in degree)
	private double floweringTriggerTemp;			//accumulated temperature to trigger flowering (degree-day)
	private double fruitSettingTriggerTemp;			//accumulated temperature to trigger fruit setting (degree-day)
	private double fruitGrowthTriggerTemp;			//accumulated temperature to trigger fruit growth (degree-day)
	private double fruitVeraisonTriggerTemp;		//accumulated temperature to trigger fruit veraison (degree-day)
	
	/*---------------------BNF PHENOLOGY ---------------------------------------------*/
	private boolean nitrogenFixation;				//if true : yes
	private int bnfTempAccumulationDateStart;		//date to start accumulation of temperature for BNF (julian day)
	private double bnfTempThreshold;	 		    //threshold of effective temperature for cumulating degree day for BNF (in degree)
	private double bnfStartTriggerTemp;			   	//accumulated temperature to trigger BNF start (degree-day)
	private int bnfExpansionDuration;			//duration of BNF expension (days)
	private int bnfStartToEndDuration;			//duration of BNF start to end (days)
	
	/*---------------------- LIGHT MODULE PARAMETERS ----------------------------------*/
	private double woodAreaDensity;	// virtual lad for winter interception by tree branches m2 m-3
	private double leafParAbsorption;	// no unit : absorption coefficient for Par radiation
	private double leafNirAbsorption;	// no unit : absorption coefficient for near infra-red radiation
	private double clumpingCoef;		// no unit : correction parameter to account for leaf clumping

	/*---------------- MICROCLIMATE PARAMETERS ----------------------------------------*/
	private double stemFlowCoefficient;	    //Coefficient of variation of the stemflow with the LAI
	private double stemFlowMax; 			//Maximum stemflow fraction
	private double wettability; 			//Wettability  (mm lai-1)

	/*---------------------- TRANSPIRATION PARAMETERS ----------------------------*/
	private double transpirationCoefficient;	

	/*---------------------- CALLOCATION MODULE PARAMETERS ----------------------------*/
	/* G.Vincent  VERSION */

	//LEAF Light use efficiency MAX
	private double leafLueMax;				// gr C MJ-1 (PAR)
	private int leafAgeForLueMax;
	private double leafPhotosynthesisEfficiencyTimeConstant;	// these two parameters are use for computing LUE according to time from budburst (formalism from the model "noyer formel")
	private double lightCompetitionIndexMin;	// Min of LCI to stop tree growth
	
	// the target leaf area depends on crown volume : leafArea = a*crownVolume^b
	private double leafAreaCrownVolCoefA;
	private double leafAreaCrownVolCoefB;

	private double leafMassArea;				// Leaf dry mass per unit leaf area (kg m-2)
	private double leafSenescenceRate;			//senescence
	
	// To limit tree daily increment in meters
	public  double maxCrownRadiusInc; 
	public  double maxHeightInc; 
	public  double maxDbhInc;
	
	//carbon conversion
	private double leafCarbonContent;	//g C g total dry biomass
	private double woodDensity; 		//Average branch and stem density (arbitrarily set to 500 kg per cubic meter) (kg*m-3)
	private double branchVolumeRatio;	//Assuming a fixed ratio of branch volume to crown volume. (cm3 cm-3)	
	private double woodCarbonContent;   //proportion of C in wood dry yield %
	


	//NITROGEN Balance
	//Functional optimum N/C concentrations
	// arbitrary value see physiology of woody plants p 304 etc. for some estimates of total N per plant part
	private double  optiNCBranch;
	private double  optiNCCoarseRoot;
	private double  optiNCFineRoot;
	private double  optiNCFoliage;
	private double  optiNCStem;
	private double  optiNCStump;
	private double  optiNCFruit;
	private double  targetNCoefficient; 		//coefficient applied to optimum to defined target concentration
	private double  luxuryNCoefficient; 		//coefficient applied to optimum to defined maximum  concentration
	private double  leafNRemobFraction;			//Fraction of Nitrogen recovered from dying leaves
	private double  rootNRemobFraction;			//Fraction of Nitrogen recovered from dying fine roots

	//others
	private double maxNSCUseFoliageFraction;		//Relative pool size, arbitrary
	private double maxNSCUseFraction;				//parameter to smoothen variation in NSC and avoid NSC to become 0
	private double targetNSCFraction;
	
	private double imbalanceThreshold;      	//level of imbalance above which remobilisation of reserves is triggered

	
	//STRESSES EFFECT ON Root Shoot 
	private boolean rsBelowGroundStressActivation ;		//effet of below ground stress (water and nitrogen) in shoot root allocation 0=no 1=yes
	private boolean rsLightStressActivation;			//effet of light competition in shoot root allocation 0=no 1=yes
	private boolean rsNitrogenExcessStressActivation ;	//effet of nitrogen excess in shoot root allocation 0=no 1=yes
	private int rsBelowGroundStressMethod;				//1=rsWaterStress * rsNitrogenStress   2=Min(rsWaterStress,rsNitrogenStress)
	private double rsNoStressResponsiveness;			//values between 0 and 1
	private double rsWaterStressResponsiveness;			//governs amplitude of response in shoot root allocation to water stress
	private double rsNitrogenStressResponsiveness;		//governs amplitude of response in shoot root allocation to nitrogen stress
	private double maxTargetLfrRatioDailyVariation;
	private double targetLfrRatioUpperDrift;
	private double minTargetLfrRatio;
	private double maxTargetLfrRatio;	
	
	//STRESSES EFFECT ON LUE
	private int lueStressMethod;						//1=lueWaterStress * lueNitrogenStress 2=Min(lueWaterStress,lueNitrogenStress)
	private double lueWaterStressResponsiveness;		//governs amplitude of response in LUE to water stress
	private double lueNitrogenStressResponsiveness;		//governs amplitude of response in LUE to nitrogen stress
	private double lueTemperatureStressTMin;			//temp below which temperature stress affecting lue is = 0  (degree)
	private double lueTemperatureStressTMax;			//temp above which temperature stress affecting lue is = 0 (degree)
	private double lueTemperatureStressTOptMin;			//temp above which temperature stress affecting lue is optimal (degree)
	private double lueTemperatureStressTOptMax;			//temp below which temperature stress affecting lue is optimal (degree)
	
	//OTHER STRESS EFFECT
	private double senWaterStressResponsiveness;		//governs amplitude of response in SENESCENCE to water stress
	private double senNitrogenStressResponsiveness;		//governs amplitude of response in SENESCENCE to nitrogen stress	
	private double leafFrostStressTemperatureMin;		//temp below which leaf area is not affected by frost (degree)
	private double leafFrostStressTemperatureMax;		//temp below which leaf area is totally affected by frost (defoliation) (degree)

	//CO2 effect
	private boolean co2EffectOnLueActivation;				//CO2 Effect on LUE activation (True/False) 
	private boolean co2EffectOnWueActivation;				//CO2 Effect on WUE activation (True/False) 
	private double co2EffectOnLueHalfSaturationConstant;	//CO2 Effect on LUE (Light use efficiency) : half saturation constant (ppm) 
	private double co2EffectIntrinsicWueSensitivity;		//CO2 intrinsic effect on WUE (Water use efficiency): Sensitivity 
	private double co2ReferenceValue;						//CO2 Effect : CO2 reference value (ppm) 
 


	/*---------------------- FINE ROOT GROWTH MODULE PARAMETERS ----------------------*/
	private int coarseRootAnoxiaResistance;			// days for coarse root deth in saturation
	private double specificRootLength;			 	//m g-1 of dry matter
	private double fineRootLifespan;	 			//number of days for senescence calculation
	private double fineRootAnoxiaLifespan;	 		//number of days for senescence in case of anoxia
	private double colonisationThreshold;		 	//alpha : Threshold for root colonisation (m m-3)
	private double colonisationFraction;		 	//Fraction of carbon allocated  for root colonisation (0-1)
	private double horizontalPreference;			//lambda : horizontal preference in root colonisation process (dimensionless)
	private double geotropismFactor;				//eta : geotropism factor (dimensionless)
	private double localWaterUptakeFactor;		 	//phi : weighting factor for local water uptakes  (dimensionless)
	private double sinkDistanceEffect;	 		 	//rho : effect of source sink distance 
	private double localNitrogenUptakeFactor;		//phi2 : weighting factor for local nitrogen uptakes  (dimensionless)

	/*---------------------- COARSE ROOT TOPOLOGY INITIALISATION ----------------------*/

	private int    coarseRootTopologyType;			//1-2-3

	/*-------------- WATER EXTRACTION PROPERTIES-------------------------------*/
	private double treeRootDiameter;						//cm

	//For calculating the transpiration reduction factor following Campbell
	private double treeAlpha;
	private double treeMinTranspirationPotential;			//cm
	private double treeMaxTranspirationPotential;			//cm

	//Root axial conductance (1/resistance involved in water transport inside the root per unit gradient in water potential and per unit path-length)
	//Unit should be here kg s-1 cm-1, or if the flux is expressed in cm, cm cm-1
	//According to Tyree, root axial conductance is higher for large roots
	private double treeRootConductivity;			//cm cm-1

	//Potential drop needed to enter the root expressed as a % of soil water potential
	private double treeBufferPotential;						//cm

	//Longitudinal resistance factor for root sap
	private double treeLongitudinalResistantFactor;						//cm.mm-1.m-1

	// This parameter indicates the relative influence of dry voxels on the calculation
	// of the averaged soil water potential perceived by the plant
	// When = 1, we use a harmonic average
	public  double treeHarmonicWeightedMean ;
	
	
	/*---------------------- FRUIT ALLOCATION CARBON  PARAMETERS ----------------------*/
	private double fruitHeatStressTemperatureMin;		//temperature above witch fruit production is affected by heat (degree)
	private double fruitHeatStressTemperatureMax;		//temperature above witch fruit production is stopped by heat(degree)	
	private double fruitFrostStressTemperatureMin;		//temperature below witch fruit production is affected by frost (degree)
	private double fruitFrostStressTemperatureMax;		//temperature below witch fruit production is stopped by frost (degree)	
	private double fruitMaxDryMatterAllocation;			//maximum daily fruit growth (g DM/fruit/day)
	private double fruitAllocationFraction;				//above ground carbon fraction allocated to fruit 
	private int fruitCarbonStressDateStart;				//carbon stress calculation date start (n-1 to n)
	private double fruitDryToFreshMatterWeight;			//conversion rate from fruit dry to fresh matter 
	private double fruitDryMaterDensity;				//m2 / tonnes DM 
	private double fruitOilConversionCoeffA;			//conversion parameter from fruit fresh matter weight to oil concentration 
	private double fruitOilConversionCoeffB;
	private double fruitOilConversionCoeffC;
	private double fruitOilDensity;						//fruit oil density
	private int fruitFirstYear;							//first year of fruiting 
	private double fruitLeafArea;						//fruit number related to leaf area in m2 
	private double fruitingConfortThreshold;			//ratio C labile / C lignus organs			
	private double fruitingTotalStressThreshold;		//ratio of fruitingConfortThreshold inhibate flowering
	private double fruitLueMax;							// Light use efficiency MAX gr C MJ-1 (PAR)
	private int fruitAgeForLueMax;	 

	/*---------------------- NITROGEN FIXATION PARAMETERS ----------------------*/
	private double bnfMaxDepth;							// maximum depth for nitrogen fixation (m) 
	private double bnfNodulationInhibitionThreshold;	// Nodulation inhibition threshold (g m-3) 
	private double bnfCardinalTemp1;					// Parameter 1 of the trapezoidal optimum function for nodule activity (degrees) 
	private double bnfCardinalTemp2;					// Parameter 2 of the trapezoidal optimum function for nodule activity (degrees) 
	private double bnfCardinalTemp3;		 			// Parameter 3  of the trapezoidal optimum function for nodule activity (degrees) 
	private double bnfCardinalTemp4;					// Parameter 4  of the trapezoidal optimum function for nodule activity (degrees) 
	private double bnfFullNoduleActivityThreshold;		// Parameters (full) of the linear threshold function for nitrogen staturation control  (g m-3)
	private double bnfNullNoduleActivityThreshold;      // Parameters (null) of the linear threshold function for nitrogen staturation control  (g m-3)
	private double bnfAirTemperatureThreshold; 		    // TCMIN stics : air temperature threshold for BNF potential activity to be increased by air temp (degrees) 
	private double bnfOptimalTemperatureDifference;     // the optimal difference to bnfAirTemperatureThreshold for optimal activity. (degrees) 
	
	//V1
	private double bnfFixMaxVeg;		//mass of nitrogen fixed per KG of produced vegetative dry matter.  (g.g-1 dry biomass) 
	private double bnfFixMaxRepro;		//mass of nitrogen fixed per KG of produced reproductive organ dry matter (flowers + fruits).   (g.g-1 dry biomass)

	/*---------------------- SELF PRUNING  PARAMETERS ----------------------*/
	private boolean selfPruningEffet;							//Self pruning effet activation 0=no 1=yes
	private double  selfPruningLCIThreshold;					//Light Competiton index threshold for self pruning (value in the [0;1] range)
	private double  selfPruningHeightRatio;						//Proportion of Self-pruning canopy height 
	private int     selfPruningNbrDaysShade;					//Number of days of shade to trigger self pruning 
	private int  	selfPruningNbrYearsForBranchesFullDecay;	//Number of year for full decay of self pruning branches
	

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
	//public int getBudBurstDelayAfterPollarding () {return budBurstDelayAfterPollarding;}
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
	public double getFruitDryMaterDensity() {return fruitDryMaterDensity;}
	public double getFruitOilConversionCoeffA() {return fruitOilConversionCoeffA;}
	public double getFruitOilConversionCoeffB() {return fruitOilConversionCoeffB;}
	public double getFruitOilConversionCoeffC() {return fruitOilConversionCoeffC;}
	public double getFruitOilDensity() {return fruitOilDensity;}
	
	
	public int getFruitFirstYear() {return fruitFirstYear;}
	public double getFruitLeafArea() {return fruitLeafArea;}
	public double getFruitingConfortThreshold() {return fruitingConfortThreshold;}
	public double getFruitingTotalStressThreshold() {return fruitingTotalStressThreshold;}

	public double getFruitLueMax () {return fruitLueMax;}
	public int getFruitAgeForLueMax () {return fruitAgeForLueMax;}
	

	
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
		//this.budBurstDelayAfterPollarding = budBurstDelayAfterPollarding;

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
			double fruitDryToFreshMatterWeight, double fruitDryMaterDensity,
			double fruitOilConversionCoeffA, double fruitOilConversionCoeffB, double fruitOilConversionCoeffC,
			double fruitOilDensity,
			int fruitFirstYear,
			double fruitLeafArea, 
			double fruitingConfortThreshold,
			double fruitingTotalStressThreshold,
			double fruitLueMax,
			int fruitAgeForLueMax,
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
				this.fruitDryMaterDensity = fruitDryMaterDensity; 
				this.fruitOilConversionCoeffA = fruitOilConversionCoeffA;
				this.fruitOilConversionCoeffB = fruitOilConversionCoeffB;
				this.fruitOilConversionCoeffC = fruitOilConversionCoeffC;
				this.fruitOilDensity = fruitOilDensity;
				this.fruitFirstYear = fruitFirstYear;
				this.fruitLeafArea = fruitLeafArea;
				this.fruitingConfortThreshold = fruitingConfortThreshold;
				this.fruitingTotalStressThreshold = fruitingTotalStressThreshold;
				this.fruitLueMax = fruitLueMax;
				this.fruitAgeForLueMax = fruitAgeForLueMax;

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
		
}
