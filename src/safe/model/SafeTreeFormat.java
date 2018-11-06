package safe.model;


import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * Records description for tree species parameters
 *
 * @author : Isabelle LECOMTE  - INRA SYSTEM Montpellier (March 2003)
 */
public class SafeTreeFormat extends RecordSet {

	// Generic keyword record is described in superclass RecordSet : key = value
	// We use only key records  here

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

	/*---------------------PHENOLOGY------------------------------------------------------*/
	private int phenologyType;						//1=cold deciduous 2=evergreen
	private int budBurstTempAccumulationDateStart;	//date to start accumulation of temperature for budburst (julian day)
	private double budBurstTempThreshold;	 	   	//threshold of effective temperature for cumulating degree day (in degree)
	private double budBurstAccumulatedTemp;			//threshold of accumulated temperature to trigger budburst (degree-day)
	private int leafExpansionDuration;				//duration of leaf expansion (in no stress condition) (julian day)
	private int budBurstToLeafFallDuration;			// duration between budburst and leaf fall
	private int leafFallDuration;					//usual duration of leaf fall (days)
	private double leafFallFrostThreshold;			//thresold for frost sensibility (degrees)
	private int budBurstDelayMinAfterPollaring;		//number of days min to delay budburst if pollading (nbr day)
	private int budBurstDelayMaxAfterPollaring;		//number of days max to delay budburst if pollading (nbr day)
	
	/*---------------------- LIGHT MODULE PARAMETERS ----------------------------------*/

	private double leafParAbsorption;		// no unit
	private double leafNirAbsorption;
	private double woodAreaDensity;		// m2 m-3 (for winter branches interception)
	private double clumpingCoef; 			// no unit : correction parameter to account for leaf clumping

	/*---------------- MICROCLIMATE PARAMETERS ----------------------------------------*/
	private double stemFlowCoefficient;		//Coefficient of variation of the stemflow with the LAI
	private double stemFlowMax; 				//Maximum stemflow fraction
	private double wettability; 				//Wettability  (mm lai-1)

	/*------------------ TRANSPIRATION MODULE PARAMETERS ----------------------------*/
	private double transpirationCoefficient;

	/*---------------------- CALLOCATION MODULE PARAMETERS ----------------------------*/
	/* G.Vincent  VERSION */

//Light use efficiency MAX
	private double lueMax;				// gr C MJ-1 (PAR)
	private int leafAgeForLueMax;
	private double leafSenescenceTimeConstant;

	//Leaf area calculation
	private double leafAreaCrownVolCoefA;
	private double leafAreaCrownVolCoefB;
	private double leafMassArea;		// Leaf dry mass per unit leaf area (kg m-2)

	private double leafCarbonContent;	//g C g total dry biomass
	private double woodDensity; 		//Average branch and stem density (arbitrarily set to 500 kg per cubic meter) (kg*m-3)
	private double branchVolumeRatio;	//Assuming a fixed ratio of branch volume to crown volume. (cm3 cm-3)
	private double woodCarbonContent;   //proportion of C in wood dry yield %
	
	// To limit tree daily increment
	public  double maxCrownRadiusInc; // meters
	public  double maxHeightInc; 
	
	//senescence
	private double leafSenescenceRate;

	//NITROGEN Balance
	//Functional optimum N/C concentrations
	// arbitrary value see physiology of woody plants p 304 etc. for some estimates of total N per plant part
	private double  optiNCBranch;
	private double  optiNCCoarseRoot;
	private double  optiNCFineRoot;
	private double  optiNCFoliage;
	private double  optiNCStem;
	private double  optiNCStump;
	private double  targetNCoefficient; 		//coefficient applied to optimum to defined target concentration
	private double  luxuryNCoefficient; 		//coefficient applied to optimum to defined maximum  concentration
	private double  leafNRemobFraction;			// Fraction of Nitrogen recovered from dying leaves
	private double  rootNRemobFraction;			// Fraction of Nitrogen recovered from dying fine roots

	//others
	
	private double targetNSCFraction;		//comment greg ?
	private double maxNSCUseFraction;			//parameter to smoothen variation in NSC and avoid NSC to become 0
	private double maxNSCUseFoliageFraction;				// Relative pool size, arbitrary
	
	private double imbalanceThreshold;      //level of imbalance above which remobilisation of reserves is triggered

	//Effect of stress on LUE and Root Shoot 
	private int rsStressMethod;						//1=rsWaterStress * rsNitrogenStress   2=Min(rsWaterStress,rsNitrogenStress)
	private int lueStressMethod;					//1=lueWaterStress * lueNitrogenStress 2=Min(lueWaterStress,lueNitrogenStress)
	private double rsNoStressResponsiveness;		//values between 0 and 1
	private double rsWaterStressResponsiveness;		//governs amplitude of response in shoot root allocation to water stress
	private double rsNitrogenStressResponsiveness;	//governs amplitude of response in shoot root allocation to nitrogen stress
	private double lueWaterStressResponsiveness;	//governs amplitude of response in LUE to water stress
	private double lueNitrogenStressResponsiveness;	//governs amplitude of response in LUE to nitrogen stress
	private double maxTargetLfrRatioDailyVariation;
	private double targetLfrRatioUpperDrift;
	private double minTargetLfrRatio;
	private double maxTargetLfrRatio;


	/*---------------------- FINE ROOT GROWTH MODULE PARAMETERS ----------------------*/
	private double coarseRootAnoxiaResistance;
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


	public SafeTreeFormat (String fileName) throws Exception {
		createRecordSet (fileName);
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
		requiredParameters.add("phenologyType");
		requiredParameters.add("budBurstTempAccumulationDateStart");
		requiredParameters.add("budBurstTempThreshold");
		requiredParameters.add("budBurstAccumulatedTemp");
		requiredParameters.add("budBurstDelayMinAfterPollaring");
		requiredParameters.add("budBurstDelayMaxAfterPollaring");
		requiredParameters.add("leafExpansionDuration");
		requiredParameters.add("budBurstToLeafFallDuration");
		requiredParameters.add("leafFallDuration");
		requiredParameters.add("leafFallFrostThreshold");
		requiredParameters.add("woodAreaDensity");
		requiredParameters.add("leafParAbsorption");
		requiredParameters.add("leafNirAbsorption");
		requiredParameters.add("clumpingCoef");
		requiredParameters.add("stemFlowCoefficient");
		requiredParameters.add("stemFlowMax");
		requiredParameters.add("wettability");
		requiredParameters.add("transpirationCoefficient");
		requiredParameters.add("lueMax");
		requiredParameters.add("leafAgeForLueMax");
		requiredParameters.add("leafSenescenceTimeConstant");
		requiredParameters.add("woodCarbonContent");
		requiredParameters.add("leafCarbonContent");
		requiredParameters.add("leafMassArea");
		requiredParameters.add("woodDensity");
		requiredParameters.add("branchVolumeRatio");
		requiredParameters.add("imbalanceThreshold");
		requiredParameters.add("rsStressMethod");
		requiredParameters.add("lueStressMethod");
		requiredParameters.add("rsNoStressResponsiveness");
		requiredParameters.add("rsWaterStressResponsiveness");
		requiredParameters.add("rsNitrogenStressResponsiveness");
		requiredParameters.add("lueWaterStressResponsiveness");
		requiredParameters.add("lueNitrogenStressResponsiveness");
		requiredParameters.add("maxTargetLfrRatioDailyVariation");
		requiredParameters.add("targetLfrRatioUpperDrift");
		requiredParameters.add("minTargetLfrRatio");
		requiredParameters.add("maxTargetLfrRatio");
		requiredParameters.add("optiNCBranch");
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

		
		String treeSpecies ="";

		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();

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
					
				} else if  (r.key.equals ("budBurstTempAccumulationDateStart")) {
					budBurstTempAccumulationDateStart = r.getIntValue ();
					requiredParameters.remove("budBurstTempAccumulationDateStart");
					
				} else if  (r.key.equals ("budBurstTempThreshold")) {
					budBurstTempThreshold = r.getDoubleValue ();
					requiredParameters.remove("budBurstTempThreshold");
					
				} else if  (r.key.equals ("budBurstAccumulatedTemp")) {
					budBurstAccumulatedTemp = r.getDoubleValue ();
					requiredParameters.remove("budBurstAccumulatedTemp");
					
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
					
				} else if  (r.key.equals ("budBurstDelayMinAfterPollaring")) {
					budBurstDelayMinAfterPollaring = r.getIntValue ();
					requiredParameters.remove("budBurstDelayMinAfterPollaring");
					
				} else if  (r.key.equals ("budBurstDelayMaxAfterPollaring")) {
					budBurstDelayMaxAfterPollaring = r.getIntValue ();	
					requiredParameters.remove("budBurstDelayMaxAfterPollaring");
					
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
					
				} else if  (r.key.equals ("lueMax")) {
					lueMax = r.getDoubleValue ();
					requiredParameters.remove("lueMax");
					
				} else if  (r.key.equals("leafAgeForLueMax")){
					leafAgeForLueMax = r.getIntValue();
					requiredParameters.remove("leafAgeForLueMax");
					
				} else if  (r.key.equals("leafSenescenceTimeConstant")){
					leafSenescenceTimeConstant = r.getDoubleValue();
					requiredParameters.remove("leafSenescenceTimeConstant");
					
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

					
				} else if  (r.key.equals ("rsStressMethod")) {
					rsStressMethod = r.getIntValue ();
					requiredParameters.remove("rsStressMethod");
					
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

				} else if  (r.key.equals ("lueWaterStressResponsiveness")) {
					lueWaterStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("lueWaterStressResponsiveness");
					
				} else if  (r.key.equals ("lueNitrogenStressResponsiveness")) {
					lueNitrogenStressResponsiveness = r.getDoubleValue ();
					requiredParameters.remove("lueNitrogenStressResponsiveness");
					
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
					coarseRootAnoxiaResistance = r.getDoubleValue ();
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
						phenologyType,
						budBurstTempAccumulationDateStart, budBurstTempThreshold, budBurstAccumulatedTemp,
						leafExpansionDuration, 
						budBurstToLeafFallDuration, leafFallDuration,
						leafFallFrostThreshold,
						budBurstDelayMinAfterPollaring, budBurstDelayMaxAfterPollaring,
						stemFlowCoefficient, stemFlowMax, wettability,
						transpirationCoefficient,
						lueMax,
						leafAgeForLueMax,
						leafSenescenceTimeConstant,
						leafCarbonContent,
						leafMassArea,
						luxuryNCoefficient,
						targetNCoefficient,
						
						rootNRemobFraction, leafNRemobFraction,
						targetNSCFraction,
						maxNSCUseFraction, 
						maxNSCUseFoliageFraction,
						
						rsStressMethod,
						lueStressMethod,
						
						rsNoStressResponsiveness,
						rsWaterStressResponsiveness,rsNitrogenStressResponsiveness,
						lueWaterStressResponsiveness,lueNitrogenStressResponsiveness,
						maxTargetLfrRatioDailyVariation,targetLfrRatioUpperDrift,
						minTargetLfrRatio,maxTargetLfrRatio,
						optiNCBranch, optiNCCoarseRoot,	optiNCFineRoot,	optiNCFoliage, optiNCStem, optiNCStump,
						woodDensity, branchVolumeRatio, woodCarbonContent, maxCrownRadiusInc, maxHeightInc, imbalanceThreshold,
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
						treeBufferPotential, treeLongitudinalResistantFactor
						);
		}
	}
}
