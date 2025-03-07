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
 * @author : Isabelle LECOMTE  - INRAE Montpellier - March 2003
 */
public class SafeTreeFormat extends RecordSet {

	// Generic keyword record is described in superclass RecordSet : key = value
	// We use only key records  here

	private static final long serialVersionUID = 1L;

	public String treeSpecies;

	/*------------- CROWN SHAPE AND ALLOMETRIC PARAMETERS -------------------------*/
	private int crownShape;						//1=elipsoid 2=paraboloid
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
	/*------------- ROOT SHAPE AND ALLOMETRIC PARAMETERS -------------------------*/

	private double cRAreaToFRLengthRatio;
	private double initialTargetLfrRatio;	//initial Leaf to FineRoots ratio 

	/*---------------------PHENOLOGY VEGETATIVE (1 or 2) --------------------------------*/
	private int phenologyType;						//1=cold deciduous 2=evergreen
	private int nbCohortMax;							//number of leaves cohort for evergreen trees
	
	
	private int budBurstTempAccumulationDateStart;	//date to start accumulation of temperature for budburst (MM-DD)
	private double budBurstTempThreshold;	 		// threshold of effective temperature for cumulating budburst degree day (in degree)
	private double budBurstTriggerTemp;				// accumulated temperature to trigger budburst (degree-day)
	private int leafExpansionDuration;				//duration of leaf expansion (in no stress condition)  (nbr day)
	private int budBurstToLeafFallDuration;			// duration between budburst and leaf fall
	private int leafFallDuration;					//usual duration of leaf fall (days)
	private double leafFallFrostThreshold;			//thresold for frost sensibility (degrees)
	//private int budBurstDelayAfterPollarding;		//number of days to delay budburst if pollarding (nbr day)
	private double leafSenescenceRate;				//leaf senescence rate 
	
		
	/*--------------------- PHENOLOGY COLD REQUIREMENT ---------------------------*/
	private boolean coldRequirement;				//if true : cold requiremnt phenology 
	private int coldTempAccumulationDateStart;	    //date to start accumulation of cold temperature for budburst (MM-DD)
	private double coldTempThreshold;	 			//Threshold of effective temperature for cumulating cold requierement  (in degree)
	private double coldBudBurstTriggerTemp;			//Accumulated cold temperature to trigger budburst (degree-day)
	private double coldBudBurstTriggerParamA;		//Parameter A to calculate the chilling unit
	private double coldBudBurstTriggerParamB;		//Parameter B to calculate the chilling unit
	private double coldBudBurstTriggerParamC;		//Parameter C to calculate the chilling unit
	private double coldBudBurstTriggerParamE;		//Parameter E to calculate the chilling unit

	
	/*---------------------FRUIT PHENOLOGY ---------------------------------------------*/
	private boolean fruitCompartment;				//if true : fruit compartment 
	private int floweringTempAccumulationDateStart;	//date to start accumulation of temperature for flowering (MM-DD)
	private double floweringTempThreshold;			// threshold of effective temperature for cumulating flowering degree day (in degree)
	private double floweringTriggerTemp;		    // accumulated temperature to trigger flowering (degree-day)
	private double fruitSettingTriggerTemp;			// accumulated temperature to trigger fruit setting (degree-day)
	private double fruitGrowthTriggerTemp;			// accumulated temperature to trigger fruit growth (degree-day)
	private double fruitVeraisonTriggerTemp;		// accumulated temperature to trigger fruit veraison (degree-day)
	
	
	/*---------------------BNF PHENOLOGY ---------------------------------------------*/
	private boolean nitrogenFixation;				//if true : yes
	private int bnfTempAccumulationDateStart;		//date to start accumulation of temperature for BNF (MM-DD)
	private double bnfTempThreshold;	 		    //threshold of effective temperature for cumulating degree day for BNF (in degree)
	private double bnfStartTriggerTemp;			   	//accumulated temperature to trigger BNF start (degree-day)
	private int bnfExpansionDuration;				//duration of BNF expension (days)
	private int bnfStartToEndDuration;				//duration of BNF start to end (days)
	
	
	/*---------------------- LIGHT MODULE PARAMETERS ----------------------------------*/

	private double leafParAbsorption;		// no unit
	private double leafNirAbsorption;
	private double woodAreaDensity;			// m2 m-3 (for winter branches interception)
	private double clumpingCoef; 			// no unit : correction parameter to account for leaf clumping

	/*---------------- MICROCLIMATE PARAMETERS ----------------------------------------*/
	private double stemFlowCoefficient;		//Coefficient of variation of the stemflow with the LAI
	private double stemFlowMax; 				//Maximum stemflow fraction
	private double wettability; 				//Wettability  (mm lai-1)

	/*------------------ TRANSPIRATION MODULE PARAMETERS ----------------------------*/
	private double transpirationCoefficient;

	/*---------------------- CALLOCATION MODULE PARAMETERS ----------------------------*/
	/* G.Vincent  VERSION */

	//LEAF Light use efficiency MAX
	private double leafLueMax;					// gr C MJ-1 (PAR)
	// these two parameters are use for computing LUE according to time from budburst (formalism from the model "noyer formel")
	private int leafAgeForLueMax;			 	// nbr day
	private double leafPhotosynthesisEfficiencyTimeConstant;
	private double lightCompetitionIndexMin;	// Min of LCI to stop tree growth

		
	//Leaf area calculation
	private double leafAreaCrownVolCoefA;
	private double leafAreaCrownVolCoefB;
	private double leafMassArea;		// Leaf dry mass per unit leaf area (kg m-2)

	private double leafCarbonContent;	//g C g total dry biomass
	private double woodDensity; 		//Average branch and stem density (arbitrarily set to 500 kg per cubic meter) (kg*m-3)
	private double branchVolumeRatio;	//Assuming a fixed ratio of branch volume to crown volume. (cm3 cm-3)
	private double woodCarbonContent;   //proportion of C in wood dry yield %
	
	// To limit tree daily increment in meters
	public  double maxCrownRadiusInc;
	public  double maxHeightInc; 
	public  double maxDbhInc;
	


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
	private double  leafNRemobFraction;			// Fraction of Nitrogen recovered from dying leaves
	private double  rootNRemobFraction;			// Fraction of Nitrogen recovered from dying fine roots

	//others
	
	private double targetNSCFraction;		//comment greg ?
	private double maxNSCUseFraction;			//parameter to smoothen variation in NSC and avoid NSC to become 0
	private double maxNSCUseFoliageFraction;				// Relative pool size, arbitrary
	
	private double imbalanceThreshold;      //level of imbalance above which remobilisation of reserves is triggered

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
	private double senWaterStressResponsiveness;	//governs amplitude of response in SENESCENCE to water stress
	private double senNitrogenStressResponsiveness;	//governs amplitude of response in SENESCENCE to nitrogen stress	
	private double leafFrostStressTemperatureMin;	//temp below which leaf area is not affected by frost (degree)
	private double leafFrostStressTemperatureMax;	//temp below which leaf area is totally affected by frost (defoliation) (degree)

	//CO2 effect
	private boolean co2EffectOnLueActivation;				//CO2 Effect on LUE activation (True/False) 
	private boolean co2EffectOnWueActivation;				//CO2 Effect on WUE activation (True/False) 
	private double co2EffectOnLueHalfSaturationConstant;	//CO2 Effect on LUE (Light use efficiency) : half saturation constant (ppm) 
	private double co2EffectIntrinsicWueSensitivity;		//CO2 intrinsic effect on WUE (Water use efficiency): Sensitivity 
	private double co2ReferenceValue;						//CO2 Effect : CO2 reference value (ppm) 
 


	/*---------------------- FINE ROOT GROWTH MODULE PARAMETERS ----------------------*/
	private int coarseRootAnoxiaResistance;
	private double specificRootLength;			 	//m g-1 of dry matter
	
	private double fineRootLifespan;	 			//number of days for senescence calculation
	private double fineRootAnoxiaLifespan;	 		//number of days for senescence in case of anoxia

	private double colonisationThreshold;		 	//alpha : Threshold for root colonisation (m m-3)
	private double colonisationFraction;		 	 //Fraction of carbon allocated  for root colonisation (0-1)
	private double horizontalPreference;			//lambda : horizontal preference in root colonisation process (dimensionless)
	private double geotropismFactor;				//eta : geotropism factor (dimensionless)
	private double localWaterUptakeFactor;		 	//phi : weighting factor for local water uptakes  (dimensionless)
	private double sinkDistanceEffect;				//rho : effect of source sink distance for water effect (dimensionless)
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
	
	private double treeBufferPotential;		//Potential drop needed to enter the root expressed as a % of soil water potential	//cm
	private double treeLongitudinalResistantFactor;		//Longitudinal resistance factor for root sap	//mm.cm-1.m-1
	
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
	private int fruitCarbonStressDateStart;				//date to start accumulation of fruit carbon stress (MM-DD)
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
	
	private double fruitLueMax;				// Light use efficiency MAX gr C MJ-1 (PAR)
	private int fruitAgeForLueMax;

	
	/*---------------------- NITROGEN FIXATION PARAMETERS ----------------------*/
	private double bnfMaxDepth;// maximum depth for nitrogen fixation 
	private double bnfNodulationInhibitionThreshold;	
	private double bnfCardinalTemp1;// Nodule activity cardinal temperature 
	private double bnfCardinalTemp2;	
	private double bnfCardinalTemp3;		 
	private double bnfCardinalTemp4;	
	private double bnfFullNoduleActivityThreshold;
	private double bnfNullNoduleActivityThreshold;

	private double bnfAirTemperatureThreshold; 		//TCMIN stics : air temperature threshold for BNF potential activity to be increased by air temp
	private double bnfOptimalTemperatureDifference;  //degrees 
	
	//V1
	private double bnfFixMaxVeg;
	private double bnfFixMaxRepro;
	
	/*---------------------- SELF PRUNING  PARAMETERS ----------------------*/
	private boolean selfPruningEffet;							//Self pruning effet activation 0=no 1=yes
	private double  selfPruningLCIThreshold;					//Light Competiton index threshold for self pruning (value in the [0;1] range)
	private double  selfPruningHeightRatio;						//Proportion of Self-pruning canopy height 
	private int     selfPruningNbrDaysShade;					//Number of days of shade to trigger self pruning 
	private int     selfPruningNbrYearsForBranchesFullDecay;	//Number of year for full decay of self pruning branches

	
	public SafeTreeFormat (String fileName) throws Exception {
		prepareImport (fileName);
	}

	//julian days replaced by MM-JJ (IL 25/05/2023)
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
	 * Load RecordSet -> updating SafeTreeSpecies
	 */
	public  void load (SafeTreeSpecies s) throws Exception {


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
		requiredParameters.add("budBurstTempAccumulationDateStart");
		requiredParameters.add("budBurstTriggerTemp");
		requiredParameters.add("budBurstTempThreshold");
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
					
			/*	} else if  (r.key.equals ("budBurstDelayAfterPollarding")) {
					budBurstDelayAfterPollarding = r.getIntValue ();
					requiredParameters.remove("budBurstDelayAfterPollarding");*/

				
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

						requiredParameters.add("fruitLueMax");
						requiredParameters.add("fruitAgeForLueMax");
						
	
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
					

				} else if  (r.key.equals ("fruitLueMax")) {
					fruitLueMax = r.getDoubleValue ();
					requiredParameters.remove("fruitLueMax");
					
				} else if  (r.key.equals("fruitAgeForLueMax")){
					fruitAgeForLueMax = r.getIntValue();
					requiredParameters.remove("fruitAgeForLueMax");	
					


					
					
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
			s.updateSpecies (treeSpecies, crownShape, ellipsoidTruncationRatio,
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
			s.updateFruitSpecies (
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
						fruitLueMax,
						fruitAgeForLueMax,
						coldRequirement,
						coldTempAccumulationDateStart,
						coldTempThreshold,
						coldBudBurstTriggerTemp,
						coldBudBurstTriggerParamA,
						coldBudBurstTriggerParamB,
						coldBudBurstTriggerParamC,
						coldBudBurstTriggerParamE
						);
			
			//updating  the BNF species object
			s.updateBnfSpecies (
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
	}
}
