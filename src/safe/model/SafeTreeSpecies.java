package safe.model;

import java.io.Serializable;

import capsis.defaulttype.Species;

/**
 * Properties for tree species.
 *
 * @author : Isabelle LECOMTE  - INRA SYSTEM Montpellier (July 2002)
 */

public class SafeTreeSpecies implements  Species, Serializable, Cloneable {	// fc - 29.7.2004 + fc - 15.11.2004 - Species


	/*------------- CROWN SHAPE AND ALLOMETRIC PARAMETERS -------------------------*/
	private String name; 						//name of tree species name
	private String fileName; 					//name of tree species file name
	private int crownShape;						// 1=elipsoid 2=paraboloid
	private double ellipsoidTruncationRatio;		// for truncated ellipsoid (0=full ellipsoid)
	
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

	/*---------------------PHENOLOGY------------------------------------------------------*/
	private int phenologyType;						//1=cold deciduous 2=evergreen
	private int budBurstTempAccumulationDateStart;	//date to start accumulation of temperature for budburst (julian day)
	private double budBurstTempThreshold;	 	   	//threshold of effective temperature for cumulating degree day (in degree)
	private double budBurstAccumulatedTemp;			//threshold of accumulated temperature to trigger budburst (degree-day)
	private int leafExpansionDuration;				//date of end of leaf expansion (in no stress condition) (julian day)
	private int budBurstToLeafFallDuration;			// number of day to compute mean T to trigger leaf fall (day)	// gt - 09.10.2009
	private int leafFallDuration;					//usual duration of leaf fall (days)
	private double leafFallFrostThreshold;			//threshold for frost sensibility (degrees)
	private int budBurstDelayMinAfterPollaring;		//number of days min to delay budburst if pollading (nbr day)
	private int budBurstDelayMaxAfterPollaring;		//number of days max to delay budburst if pollading (nbr day)
	
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

	//Light use efficiency MAX
	private double lueMax;				// gr C MJ-1 (PAR)
	private int leafAgeForLueMax;
	private double leafSenescenceTimeConstant;	// these two parameters are use for computing LUE according to time from budburst (formalism from the model "noyer formel")

	// the target leaf area depends on crown volume : leafArea = a*crownVolume^b
	private double leafAreaCrownVolCoefA;
	private double leafAreaCrownVolCoefB;

	private double leafMassArea;		// Leaf dry mass per unit leaf area (kg m-2)

	
	// To limit tree daily increment
	public  double maxCrownRadiusInc; // meters
	public  double maxHeightInc; 
	
	//carbon conversion
	private double leafCarbonContent;	//g C g total dry biomass
	private double woodDensity; 		//Average branch and stem density (arbitrarily set to 500 kg per cubic meter) (kg*m-3)
	private double branchVolumeRatio;	//Assuming a fixed ratio of branch volume to crown volume. (cm3 cm-3)	
	private double woodCarbonContent;   //proportion of C in wood dry yield %
	
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
	private double  leafNRemobFraction;			//Fraction of Nitrogen recovered from dying leaves
	private double  rootNRemobFraction;			//Fraction of Nitrogen recovered from dying fine roots

	//others
	private double maxNSCUseFoliageFraction;		//Relative pool size, arbitrary
	private double maxNSCUseFraction;				//parameter to smoothen variation in NSC and avoid NSC to become 0
	private double targetNSCFraction;
	
	private double imbalanceThreshold;      	//level of imbalance above which remobilisation of reserves is triggered

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
	
	//AQ ajout du paramètre constante de mineralisation des racines:



	/*---------------------- FINE ROOT GROWTH MODULE PARAMETERS ----------------------*/
	private double coarseRootAnoxiaResistance;		// days for coarse root deth in saturation
	private double specificRootLength;			 	//m g-1 of dry matter
	private double fineRootLifespan;	 			 	//number of days for senescence calculation
	private double fineRootAnoxiaLifespan;	 			//number of days for senescence in case of anoxia

	private double colonisationThreshold;		 	 //alpha : Threshold for root colonisation (m m-3)
	private double colonisationFraction;		 	 //Fraction of carbon allocated  for root colonisation (0-1)
	private double horizontalPreference;			 //lambda : horizontal preference in root colonisation process (dimensionless)
	private double geotropismFactor;				 //eta : geotropism factor (dimensionless)
	private double localWaterUptakeFactor;		 	 //phi : weighting factor for local water uptakes  (dimensionless)
	private double sinkDistanceEffect;	 		 	 //rho : effect of source sink distance 
	private double localNitrogenUptakeFactor;		 //phi2 : weighting factor for local nitrogen uptakes  (dimensionless)

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


	/**	Constructor
	*/
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
	public int getBudBurstTempAccumulationDateStart () {return budBurstTempAccumulationDateStart;}
	public double getBudBurstTempThreshold () {return budBurstTempThreshold;}
	public double getBudBurstAccumulatedTemp () {return budBurstAccumulatedTemp;}
	public int getLeafExpansionDuration () {return leafExpansionDuration;}
	public int getBudBurstToLeafFallDuration () {return budBurstToLeafFallDuration;}
	public int getLeafFallDuration () {return leafFallDuration;}
	public double getLeafFallFrostThreshold() {return leafFallFrostThreshold;}
	public int getBudBurstDelayMinAfterPollaring () {return budBurstDelayMinAfterPollaring;}
	public int getBudBurstDelayMaxAfterPollaring () {return budBurstDelayMaxAfterPollaring;}
	
	
	
	public double getStemFlowCoefficient () {return stemFlowCoefficient;}
	public double getStemFlowMax () {return stemFlowMax;}
	public double getWettability () {return wettability;}

	public double getTranspirationCoefficient () {return transpirationCoefficient;}

	public double getLueMax () {return lueMax;}
	public int getLeafAgeForLueMax () {return leafAgeForLueMax;}
	public double getLeafSenescenceTimeConstant() {return leafSenescenceTimeConstant;}
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
	public double getTargetNSCFraction () {return targetNSCFraction;}
	public double getMaxNSCUseFraction() {return maxNSCUseFraction;}
	public double getMaxNSCUseFoliageFraction () {return maxNSCUseFoliageFraction;}
	public double getRsNoStressResponsiveness () {return rsNoStressResponsiveness;}
	

	public int getRsStressMethod () {return rsStressMethod;}
	public int getLueStressMethod () {return lueStressMethod;}
	public double getRsWaterStressResponsiveness () {return rsWaterStressResponsiveness;}
	public double getRsNitrogenStressResponsiveness () {return rsNitrogenStressResponsiveness;}
	public double getLueWaterStressResponsiveness () {return lueWaterStressResponsiveness;}
	public double getLueNitrogenStressResponsiveness () {return lueNitrogenStressResponsiveness;}
	
	public double getMaxTargetLfrRatioDailyVariation () {return maxTargetLfrRatioDailyVariation;}
	public double getTargetLfrRatioUpperDrift() {return targetLfrRatioUpperDrift;}
	public double getMinTargetLfrRatio() {return minTargetLfrRatio;}
	public double getMaxTargetLfrRatio() {return maxTargetLfrRatio;}

	public void setRsStressMethod (int i) {rsStressMethod = i;}
	public void setLueStressMethod (int i) {lueStressMethod = i;}
	public void setRsWaterStressResponsiveness(double w){this.rsWaterStressResponsiveness=w;}
	public void setRsNitrogenStressResponsiveness(double w){this.rsNitrogenStressResponsiveness=w;}
	public void setLueWaterStressResponsiveness(double w){this.lueWaterStressResponsiveness=w;}
	public void setLueNitrogenStressResponsiveness(double w){this.lueNitrogenStressResponsiveness=w;}
	
	public void setRsNoStressResponsiveness(double w){this.rsNoStressResponsiveness=w;}
	
	public void setMaxTargetLfrRatioDailyVariation(double w){this.maxTargetLfrRatioDailyVariation=w;}
	public void setTargetLfrRatioUpperDrift(double w){this.targetLfrRatioUpperDrift=w;}
	public void setMinTargetLfrRatio(double w){this.minTargetLfrRatio=w;}
	public void setMaxTargetLfrRatio(double w){this.maxTargetLfrRatio=w;}
	public void setColonisationThreshold(double c){this.colonisationThreshold=c;}
	public void setColonisationFraction(double c){this.colonisationFraction=c;}
	
	
	public void setHorizontalPreference(double h){this.horizontalPreference=h;}
	public void setGeotropismFactor(double g){this.geotropismFactor=g;}
	public void setLocalWaterUptakeFactor(double l){this.localWaterUptakeFactor=l;}
	public void setSinkDistanceEffect(double s){this.sinkDistanceEffect=s;}


	public double getWoodDensity() {return woodDensity;}
	public double getBranchVolumeRatio() {return branchVolumeRatio;}
	public double getLeafSenescenceRate() {return leafSenescenceRate;}
	public double getImbalanceThreshold() {return imbalanceThreshold;}
	public double getWoodCarbonContent() {return woodCarbonContent;}
	public double getMaxCrownRadiusInc() {return maxCrownRadiusInc;}
	public double getMaxHeightInc() {return maxHeightInc;}


/** FINE ROOT GROWTH PARAMETERS **/
	public double getSpecificRootLength() {return specificRootLength;}
	public double getCoarseRootAnoxiaResistance(){return coarseRootAnoxiaResistance;}
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
								int phenologyType,
								int budBurstTempAccumulationDateStart,
								double budBurstTempThreshold,
								double budBurstAccumulatedTemp,
								int leafExpansionDuration,
								int budBurstToLeafFallDuration,
								int leafFallDuration,
								double leafFallFrostThreshold,
								int budBurstDelayMinAfterPollaring,
								int budBurstDelayMaxAfterPollaring,
								double stemFlowCoefficient,
								double stemFlowMax,
								double wettability,
								double transpirationCoefficient,
								double lueMax,
								int leafAgeForLueMax,
								double leafSenescenceTimeConstant,
								double leafCarbonContent,
								double leafMassArea,
								double luxuryNCoefficient,
								double targetNCoefficient,
								
								double rootNRemobFraction,
								double leafNRemobFraction,
								
								double targetNSCFraction,
								double maxNSCUseFraction,
								double maxNSCUseFoliageFraction,
								
								int rsStressMethod,
								int lueStressMethod,
								
								double rsNoStressResponsiveness,
								double rsWaterStressResponsiveness,
								double rsNitrogenStressResponsiveness,
								double lueWaterStressResponsiveness,
								double lueNitrogenStressResponsiveness,								
								double maxTargetLfrRatioDailyVariation,
								double targetLfrRatioUpperDrift,
								double minTargetLfrRatio,
								double maxTargetLfrRatio,
								double optiNCBranch,
								double optiNCCoarseRoot,
								double optiNCFineRoot,
								double optiNCFoliage,
								double optiNCStem,
								double optiNCStump,
								double woodDensity,
								double branchVolumeRatio,
								double woodCarbonContent,
								double maxCrownRadiusInc,
								double maxHeightInc,						
								double imbalanceThreshold,
								double leafSenescenceRate,
								double coarseRootAnoxiaResistance,
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
								double bufferPotential, double longitudinalResistantFactor) {

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
		this.budBurstTempAccumulationDateStart = budBurstTempAccumulationDateStart;
		this.budBurstTempThreshold = budBurstTempThreshold;
		this.budBurstAccumulatedTemp = budBurstAccumulatedTemp;
		this.leafExpansionDuration = leafExpansionDuration;
		this.budBurstToLeafFallDuration = budBurstToLeafFallDuration;
		this.leafFallDuration = leafFallDuration;
		this.leafFallFrostThreshold = leafFallFrostThreshold;
		this.budBurstDelayMinAfterPollaring = budBurstDelayMinAfterPollaring;
		this.budBurstDelayMaxAfterPollaring = budBurstDelayMaxAfterPollaring;
		this.stemFlowCoefficient = stemFlowCoefficient;
		this.stemFlowMax = stemFlowMax;
		this.wettability = wettability;
		this.transpirationCoefficient = transpirationCoefficient;
		this.lueMax = lueMax;
		this.leafAgeForLueMax = leafAgeForLueMax;
		this.leafSenescenceTimeConstant = leafSenescenceTimeConstant;
		this.leafCarbonContent = leafCarbonContent;
		this.leafMassArea = leafMassArea;
		this.luxuryNCoefficient = luxuryNCoefficient;
		this.targetNCoefficient = targetNCoefficient;
		this.rootNRemobFraction = rootNRemobFraction;
		this.leafNRemobFraction = leafNRemobFraction;
		this.targetNSCFraction = targetNSCFraction;
		this.maxNSCUseFraction = maxNSCUseFraction;
		this.maxNSCUseFoliageFraction = maxNSCUseFoliageFraction;
		this.rsNoStressResponsiveness = rsNoStressResponsiveness;
		this.rsWaterStressResponsiveness = rsWaterStressResponsiveness;
		this.rsNitrogenStressResponsiveness = rsNitrogenStressResponsiveness;
		this.lueWaterStressResponsiveness = lueWaterStressResponsiveness;
		this.lueNitrogenStressResponsiveness = lueNitrogenStressResponsiveness;
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
		this.woodDensity = woodDensity;
		this.branchVolumeRatio = branchVolumeRatio;
		this.woodCarbonContent = woodCarbonContent;	
		this.maxCrownRadiusInc = maxCrownRadiusInc;
		this.maxHeightInc = maxHeightInc;
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
	}

}
