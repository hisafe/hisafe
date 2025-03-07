/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
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
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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

import java.awt.Color;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Species;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.kernel.GScene;


/**
 * Safe tree object
 * @author Isabelle Lecomte - INRA Montpellier France - July 2002 
 * @author Tristan GERAULT  - INRA Montpellier France  - April 2021 : add nitrogen SIMPLE fixation module (V1) 
 * @author Nicolas BARBAULT - INRA Montpellier France  - October 2021 : add fruit module 
 */

public class SafeTree extends SpatializedTree implements Speciable, SimpleCrownDescription {

	// WARNING: It's a GMaddTree, see the super class for other states variables
	// dbh is in meter !!!!!!!!!!!!!!!!!!!!!!!
	private static final long serialVersionUID = 1L;
	private SafeTreeSpecies treeSpecies;	// refer to the species parameters
	private SafePlantRoot 	plantRoots;		// refer to fine root object
	private SafeTreeItk 	treeItk;		// refer to itk
	
	//SIZEf
	private double 	dBaseMeters;  			// diameter at stem base (m) 
	private double 	dbhMeters;  			// diameter at breast height (m) (generic dbh is in cm)
	private double 	crownBaseHeight;  		// m
	private double 	crownRadiusTreeLine; 	// m (Y axix)
	private double 	crownRadiusInterRow; 	// m (X axix)
	private double 	crownRadiusVertical; 	// m (Z axis)
	private double 	crownParameter;			// no unit (used only for paraboloide shape)
	private double 	crownVolume;			// m3
	private double 	stemVolume;				// m3
	private double 	leafArea[];				// leaf area by cohort m2 (dimension is given by species.nbCohortMax) 
	private int   	nbCellsBellow;			// number of cells bellow the tree crown
	private double 	laiAboveCells;			// m2/m2 lai on cell basis
	private boolean isTopped;				// true if the tree has been topped 
 
	//PLANTING
	private boolean planted;				//true if the tree is planted 
	private int 	plantingYear;			//planting year 1= first year 
	private int 	plantingDay;			//planting day = julian day
	private int    	age;					//in days
	private int  	leafAge[];				//leaf area age in julian day by cohort  (dimension is given by species.nbCohortMax)
	private double 	leafLue[];				//leaf LUE by cohort  (dimension is given by species.nbCohortMax) g C MJ-1
	
	//PLANTING
	private boolean harvested;				//true if the tree is harvested 
	private int 	harvestingYear;			//harvesting year 1= first year 
	private int 	harvestingDay;			//harvesting day julian day
	
	//VEGETATIVE PHENOLOGY
	private int 	phenologicalStage;					//0:before budburst ; 1:during leaf expansion ; 2:before leaf fall ; 3:during leaf fall ; 4:after leaf fall
	private boolean	firstYearStarted;					//set to true when budburst start the first year (never reset)
	private boolean budburstAccumulatedTemperatureStarted;	//true if budburstAccumulatedTemperature is started
	private double	budburstAccumulatedTemperature;		//day degrees
	private boolean budburstAccumulatedColdTemperatureStarted;	//true if budburstAccumulatedTemperature is started
	private double	budburstAccumulatedColdTemperature;		//day degrees
	private int		budburstDate;						//julian day
	private int		leafExpansionEndingDate;			//julian day
	private int		leafFallStartingDate;				//julian day
	private int		leafFallEndingDate;					//julian day

	//FRUIT PHENOLOGY
	private int 	fruitPhenologicalStage;				//0:dormancy ; 1: end of dormancy; 2:flowering ; 3:fruit setting ; 4:fruit growth ; 5:veraison; 6:harvest
	private boolean heatAccumulatedTemperatureStarted;	//true if heatAccumulatedTemperature is started
	private double	heatAccumulatedTemperature;			//day degrees
	private int		floweringDate;						//julian day
	private int		fruitSettingDate;					//julian day
	private int		fruitGrowthDate;					//julian day
	private int		veraisonDate;						//julian day
	private int     floweringDuration;					//nbr days 
	private int 	fruitHarvestDate;					//julian days

	//BNF PHENOLOGY
	private int 	bnfPhenologicalStage;				//0:dormancy ; 1: bnf start; 2:bnf steady state ; 3:bnf end 
	private int		bnfStartDate;						//julian day
	private int		bnfSteadyStateDate;					//julian day
	private int		bnfEndingDate;						//julian day
	private double	bnfAccumulatedTemperature;			//day degrees
	private boolean bnfAccumulatedTemperatureStarted;	//true if bnfAccumulatedTemperature is started
	
	//FRUIT MODULE
	private double 	flowerNbrPotentialReducer;			//%
	private int 	flowerNbrPotentialFromLeafArea;
	private int 	flowerNbrPotential;
	private int 	flowerNbrPotentialDaily; 
	private int 	flowerNbr;
	private int 	fruitNbr;							//fruit and flowers (reproductive organs) 
	private int 	fruitThinning;						//fruit automatic thinning

	//EFFECT OF STRESS ON FRUIT PRODUCTION  
	private double 	floweringHeatStress;				// 0-1
	private double 	floweringFrostStress;				// 0-1
	private double  fruitCarbonStressIndex;				// 0-1

	
	//COURBAUD LIGHT MODEL CALCULATIONS
	private double	captureFactorForDiffusePar;		// m2 d-1
	private double	captureFactorForDirectPar;		// m2 d-1
	private double	captureFactorForInfraRed;			// m2 d-1
	private double	captureFactorForDirectNir;		// m2 d-1
	private double	captureFactorForDiffuseNir;		// m2 d-1
	private double	directParIntercepted;				// Moles PAR d-1
	private double	diffuseParIntercepted;			// Moles PAR d-1
	private double	globalRadIntercepted;				// MJ d-1
	private double	infraRedIntercepted;				// Watts
	private double	cFdirectLonelyTree;				// capture factor for direct Par if the tree were alone
	private double	cFdiffuseLonelyTree;				// idem for diffuse Par														//(used for computing the light competition index)
	private double	lightCompetitionIndex;			// ratio between Par intercepted and Par intercepted by the same tree alone on the field
	private double 	lastLeafArea;						// in m2 (leaf area used for lighting model triggering)

	//MICROCLIMATE CALCULATION
	private double 	interceptedRain; 					// l  d-1
	private double 	storedRain; 						// l  d-1
	private double 	evaporatedRain; 					// l  d-1
	private double 	stemflow; 							// l  d-1

	//TRANSPIRATION
	private double 	stomatalConductance;				//
	private double 	waterDemand;						// l  
	private double 	waterDemandReduced;					// l  
	private double 	waterUptake;						// l  
	private double 	waterStress;						// 0-1
	private double 	waterStressSpring;					// 0-1
	private double 	waterStressSummer;					// 0-1

	//NITROGEN
	private double 	nitrogenDemandBeforeFixation;		// kg N
	private double 	nitrogenDemandAfterFixation;		// kg N
	private double 	nitrogenUptake;						// from soil kg N
	private double 	nitrogenAvailable;					// uptake from soil + bnf kg N
	private double 	nitrogenStress;						// 0-1
	private double 	nitrogenStressSpring;				// 0-1
	private double 	nitrogenStressSummer;				// 0-1	
	private double 	nitrogenSaturation;					// 0-1
	private double 	nitrogenSatisfaction;				// 0-1
	
	//IN SATURATION
	private double 	waterUptakeInSaturation;			// l  
	private double 	nitrogenUptakeInSaturation;			// kg N

	//Stress effect on Light Use Efficiency 
	private double lueWaterStress; 			//stress effect of water on LUE
	private double lueNitrogenStress; 		//stress effect of nitrogen on LUE
	private double lueStress; 				//stress effect of water+nitrogen on LUE
	private double lueTemperatureStress; 	//stress effect of temperature on LUE

	//Stress effect on  Root Shoots 
	private double rsBelowGroundStressEffect;//Below Ground stress effect on shoot root allocation
	private double rsNitrogenExcessEffect;	//nitrogen excess effect on shoot root allocation
	private double rsLightStressEffect;		//Light stress effect on shoot root allocation
	
	//Stress effect on  Leaves 
	private double leafAgeStress;			//stress effect of leaf age (senescence) 
	private double leafFrostStress;			//stress effect of frost (leave senescence) 
	private double leafWaterNitrogenStress;	//effect of W and N stress (leave senescence) 
	
	//CO2 effect
	private double co2LueEffect; 		//stress of CO2 on LUE (Light Use efficiency) 
	private double co2WueEffect; 		//stress of CO2 on WUE (Water Use efficiency) 
	
	//CARBON POOLS (kg C) 	
	private double 	carbonStem;
	private double 	carbonFoliage[];
	private double 	carbonBranches;
	private double 	carbonCoarseRoots;
	private double 	carbonFineRoots;
	private double 	carbonLabile;
	private double 	carbonStump;
	private double 	carbonFruit;
	private double 	carbonTotalBefore;		//sum of the total carbon pool for budget

	//CARBON TARGET (kg C)
	private double targetCarbonStem; 
	private double targetStemVolume;
	private double targetHeight;
	private double targetDbh;
	private double targetCrownDepth;
	private double targetCrownVolume;
	private double targetCarbonBranches;
	private double targetCarbonFoliage;
	
	//CARBON Sinks (kg C) 
	private double 	aboveGroundSink;
	private double 	belowGroundSink;
	private double 	carbonStemSink;
	private double 	carbonBranchesSink;
	private double 	carbonFoliageSink;
	private double 	carbonStumpSink;
	private double 	carbonFineRootsSink;
	private double 	carbonCoarseRootsSink;

	// CARBON INCREMENT (kg C) 
	private double 	carbonLabileIncrement; 
	private double 	carbonCoarseRootsIncrement;  
	private double 	carbonFineRootsIncrement; 
	private double 	carbonFoliageIncrement;  
	private double 	carbonStemIncrement; 
	private double 	carbonBranchesIncrement; 
	private double 	carbonStumpIncrement; 
	private double 	carbonFruitIncrement; 

	// CARBON SENESCENCE (kg C) 
	private double 	carbonFoliageSen;
	private double 	carbonFineRootsSen;
	private double 	carbonCoarseRootsSen;
	private double 	carbonBranchesSen;
	private double 	carbonFruitSen;
	private double 	carbonFineRootsSenAnoxia;
	private double 	carbonCoarseRootsSenAnoxia;

	//CARBON LITER (KG C)
	private double 	carbonFoliageLitterAllPlot;	
	private double 	carbonBranchesLitterAllPlot;
	private double 	carbonFruitLitterAllPlot;
	private double 	carbonFoliageLitterUnderTree;
	private double 	carbonBranchesLitterUnderTree;
	private double 	carbonFruitLitterUnderTree;
	
	//CARBON DEAD ON TREE  (kg C) 
	private HashMap<Integer, Double> 	carbonBranchesDeadOnTree;
	
	//CARBON EXPORTED  (kg C) 
	private double 	carbonFoliageExported;
	private double 	carbonStemExported;
	private double 	carbonStumpExported;
	private double 	carbonBranchesExported;
	private double 	carbonFruitExported;

	//Daily carbon allocated  for  roots growth (kg C) 
	private double  aboveGroundImbalance;
	private double 	belowGroundGrowth;
	private double 	aboveGroundGrowth;
	private double 	carbonIncrement[];
	private double  NSCExchange;
	private double  carbonSavingFraction;
	private double  branchImbalance;
	private double carbonCoarseRootsTarget; 	//target coarse roots for C allocation module
	
	//N POOLS
	//Structural Nitrogen in tree pools  (kg N)
	private double 	nitrogenStem;
	private double 	nitrogenStump;
	private double 	nitrogenFoliage[];
	private double 	nitrogenBranches;
	private double 	nitrogenCoarseRoots;
	private double 	nitrogenFineRoots;
	private double 	nitrogenLabile;
	private double 	nitrogenFruit;

	//NITROGEN SINKS
	private double 	nitrogenStemSink;
	private double 	nitrogenStumpSink;
	private double 	nitrogenFoliageSink;
	private double 	nitrogenBranchesSink;
	private double 	nitrogenCoarseRootsSink;
	private double 	nitrogenFineRootsSink;
	private double 	nitrogenFruitSink;
	
	//NITROGEN INCREMENT
	private double 	nitrogenCoarseRootsIncrement;  
	private double 	nitrogenFineRootsIncrement; 
	private double 	nitrogenFoliageIncrement;  
	private double 	nitrogenStemIncrement; 
	private double 	nitrogenBranchesIncrement; 
	private double 	nitrogenStumpIncrement; 
	private double 	nitrogenFruitIncrement; 

	
	//NITROGEN EXPORTATION (kg N)
	private double 	nitrogenFoliageExported;
	private double 	nitrogenStemExported;
	private double 	nitrogenStumpExported;
	private double 	nitrogenBranchesExported;
	private double 	nitrogenFruitExported;
	
	
	//NITROGEN SENESCNCE (kg N)
	private double 	nitrogenFoliageSen;
	private double 	nitrogenBranchesSen;
	private double 	nitrogenFruitSen;
	private double 	nitrogenFineRootsSen;
	private double 	nitrogenCoarseRootsSen;
	private double 	nitrogenFineRootsSenAnoxia;
	private double 	nitrogenCoarseRootsSenAnoxia;	

	
	//NITROGEN LITER
	private double 	nitrogenFoliageLitterAllPlot;
	private double 	nitrogenBranchesLitterAllPlot;
	private double 	nitrogenFruitLitterAllPlot;
	private double 	nitrogenFoliageLitterUnderTree;
	private double 	nitrogenBranchesLitterUnderTree;
	private double 	nitrogenFruitLitterUnderTree;
	
	//NITROGEN DEAD ON TREE  (kg N) 
	private HashMap<Integer, Double>	nitrogenBranchesDeadOnTree;

	
	//NIROGEN FIXATION V1  
	//Tristan GERAULT 22/04/2021
	private double bnfNitrogenFixationPotential;// kg N
	private double bnfNitrogenFixation;			// kg N	
	private double bnfPhenologyCoefficient;			
	private double bnfMaxNitrogenFixationPotential;			
	private double bnfAnoxStress;
	private double bnfWaterStress; 
	private double bnfTemperatureStress;
	private double bnfNodulationInhibition; 
	private double bnfSoilNitrateExcess;
	

	//Fraction of total Cavailable which is allocated to different organs
	private double carbonAllocToGrowth; 		//Amount of available C allocated to growth (kg C)
	private double aboveGroundCFraction;		//fraction of Carbon total allocated to above ground
	private double targetLfrRatio;				//target value for carbonFoliage/(carbonFineRoots+carbonFoliage)
	private double targetLfrDrift;				//target drift value for carbonFoliage/(carbonFineRoots+carbonFoliage)

	
	//new data for allocation fraction
	private double 	aboveGroundAllocFrac;
	private double 	stemAllocFrac;
	private double 	branchAllocFrac;
	private double 	foliageAllocFrac;
	private double 	belowGroundAllocFrac;
	private double 	stumpAllocFrac;
	private double 	fineRootsAllocFrac;
	private double 	coarseRootsAllocFrac;

	//To count number of days sincelast irrigation
	private int nbrDaysSinceLastIrrigation;
	private int nbrDaysSinceLastFertilization;
	
	//To count number of days under shade threshold 
	private int nbrDaysInShade;

	
	//Annual summaries
	private double 	leafAreaMax;					//  max for the year m2
	private double 	carbonFoliageMax;				//  max for the year kg
	private double 	waterUptakeAnnual;				// l  d-1
	private double 	nitrogenUptakeAnnual;
	private double 	parInterceptedAnnual;			// Moles PAR d-1
	private double 	interceptedRainAnnual; 			// l  d-1
	private double 	carbonAllocToGrowthAnnual; 		//Amount of available C allocated to growth (kg C)
	private double 	carbonFoliageSenAnnual;
	private double 	carbonFineRootsSenAnnual;
	private double 	carbonCoarseRootsSenAnnual;	
	private double 	carbonFineRootsSenAnoxiaAnnual;
	private double 	carbonCoarseRootsSenAnoxiaAnnual;
	

	/**
	 * Constructor for new SafeTree.
	 * @see SpatializedTree
	 */
	public SafeTree (	GScene 	stand,
						int 	id, 					
						String	treeSpeciesName,
						double	x, 						
						double	y, 						
						double  z,			//GT 2007 slope
						SafeGeneralParameters	safeSettings)  throws Exception {

		super (id, stand,  0, 0, 0, false, x, y, z);		//z = height 

		//Initialise tree species
		//on cherche en priorite les fichiers parametres dans le repertoire de simulation
		//sinon on prend le reperoire pas defaut safe/data
		
		try {
			
			String dataPath = safeSettings.dataPath + "/treeSpecies/"+treeSpeciesName+".tree";

			SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
			new SafeTreeFormat (dataPath).load (treeSpecies);
			this.treeSpecies = treeSpecies;
			System.out.println("TREE SPECIES PARAMETERS "+treeSpeciesName+" OK");

		} catch (CancellationException e) {
			throw e;	
		} catch (Exception e) {

			
			try {
			
				String dataPath = safeSettings.dataOriginalPath + "/treeSpecies/"+treeSpeciesName+".tree";
				SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
				new SafeTreeFormat (dataPath).load (treeSpecies);
				this.treeSpecies = treeSpecies;
				System.out.println("WARNING TREE SPECIES PARAMETERS "+treeSpeciesName+" read in folder "+safeSettings.dataOriginalPath);

			} catch (Exception e2) {
				System.out.println(e2.getMessage());
				System.out.println("TREE SPECIES PARAMETERS "+treeSpeciesName+" reading problem  ... simulation is canceled !");
				System.exit(1);
				
			}
			
		}

		this.treeSpecies.setFileName(treeSpeciesName);
		
		//Initialise roots informations
		this.plantRoots = new SafePlantRoot (this);

		//create leaf area cohort 
		this.leafArea 			= new double[this.getTreeSpecies().getNbCohortMax()];
		this.leafAge 			= new int[this.getTreeSpecies().getNbCohortMax()];
		this.leafLue 			= new double[this.getTreeSpecies().getNbCohortMax()];
		this.carbonIncrement 	= new double[this.getTreeSpecies().getNbCohortMax()];
		this.carbonFoliage 		= new double[this.getTreeSpecies().getNbCohortMax()];
		this.nitrogenFoliage 	= new double[this.getTreeSpecies().getNbCohortMax()];
		
		this.carbonBranchesDeadOnTree = new LinkedHashMap<Integer, Double> ();
		this.nitrogenBranchesDeadOnTree = new LinkedHashMap<Integer, Double> (); 
		
		
		//leaf area and leaf age
		razLeafArea ();

		//Initialise phenology		
		setFirstYearStarted (false);
		setBudburstAccumulatedTemperature (0);
		setBudburstAccumulatedColdTemperature (0);
		setHeatAccumulatedTemperature (0);
		setHeatAccumulatedTemperatureStarted (false);
		setBudburstAccumulatedTemperatureStarted (false);
		setBudburstAccumulatedColdTemperatureStarted (false);
		setPhenologicalStage(0);
		setFruitPhenologicalStage(0);
		setBnfPhenologicalStage(0);
	
		setLaiAboveCells (0);
		setNbCellsBellow (0);

		//Inititalise daily values
		setWaterStress (1);
		setNitrogenStress (1);
		setNitrogenSatisfaction(1);
		setNitrogenSaturation (0);
		setWaterDemand (0);
		setWaterDemandReduced(0);
		setNitrogenDemandAfterFixation (0);
		setNitrogenDemandBeforeFixation(0);
		setWaterUptake (0);
		setWaterUptakeInSaturation (0);
		setCarbonFruitIncrement (0);
		setNitrogenUptake(0);
		setNitrogenAvailable (0);
		setNitrogenUptakeInSaturation (0);
		setEvaporatedRain (0);
		setStoredRain (0);
		setStemflow (0);
		

		setFloweringHeatStress(1); 
		setFloweringFrostStress(1); 
		setLightCompetitionIndex(1); 
		setCaptureFactorForDiffusePar(0);
		setCaptureFactorForDirectPar(0);
		setCaptureFactorForInfraRed(0);
		setCaptureFactorForDirectNir(0);
		setCaptureFactorForDiffuseNir(0);
		setCFdirectLonelyTree(0);
		setCFdiffuseLonelyTree(0);
		setDirectParIntercepted(0);
		setDiffuseParIntercepted(0);
		setGlobalRadIntercepted(0);
		setInfraRedIntercepted(0);

		// initialisation of carbon senescence value	
		setCarbonFoliageSen(0);
		setCarbonFineRootsSen(0);
		setCarbonCoarseRootsSen(0);
		setCarbonFineRootsSenAnoxia(0);
		setCarbonCoarseRootsSenAnoxia(0);		
		setNitrogenFineRootsSen(0);
		setNitrogenCoarseRootsSen(0);
		setNitrogenFineRootsSenAnoxia(0);
		setNitrogenCoarseRootsSenAnoxia(0);
		setCarbonFineRootsIncrement(0);
		setCarbonCoarseRootsTarget(0); 
		setCarbonBranchesSen(0);
		setCarbonFruitSen(0);
		setNitrogenFoliageSen(0);
		setNitrogenBranchesSen(0);
		setNitrogenFruitSen(0);	
		
		//litters
		setCarbonFoliageLitterUnderTree (0);
		setCarbonBranchesLitterUnderTree (0);
		setCarbonFruitLitterUnderTree (0);
		setNitrogenFoliageLitterUnderTree (0);
		setNitrogenBranchesLitterUnderTree (0);
		setNitrogenFruitLitterUnderTree (0);
		
		setCarbonFoliageLitterAllPlot(0);
		setCarbonBranchesLitterAllPlot(0);
		setCarbonFruitLitterAllPlot (0);
		setNitrogenFoliageLitterAllPlot (0);
		setNitrogenBranchesLitterAllPlot (0);
		setNitrogenFruitLitterAllPlot(0);
		
		
		
		setBnfAnoxStress(1);
		setBnfWaterStress(1);
		setBnfTemperatureStress(1);
		setBnfNodulationInhibition(1);
		setBnfSoilNitrateExcess(1);
		setCo2LueEffect(1);
		setCo2WueEffect(1);

		
	}

	/**
	 * Clone a SafeTree: first calls super.clone (), then clone the SafeTree instance variables.
	 */
	public Object clone () {

		SafeTree newTree = (SafeTree) super.clone ();

		return newTree;
	}
	
	/**
	 * Clone a SafeTree: first calls super.clone (), then clone the SafeTree instance variables.
	 */
	public void dailyRaz () {

		this.setDirectParIntercepted (0);
		this.setDiffuseParIntercepted (0);
		this.setGlobalRadIntercepted (0);
		this.setInfraRedIntercepted (0);
		this.setWaterDemand (0);
		this.setWaterUptake (0);		
		this.setWaterUptakeInSaturation (0);
		this.setFloweringHeatStress(1); 
		this.setFloweringFrostStress(1); 
		this.setNitrogenDemandAfterFixation (0);
		this.setNitrogenDemandBeforeFixation(0);
		this.setNitrogenUptake(0);
		this.setNitrogenAvailable (0);
		this.setNitrogenUptakeInSaturation (0);
		this.setInterceptedRain (0);	
		this.setStemflow (0);		
		this.setEvaporatedRain (0);	
		this.setCarbonFineRootsIncrement(0);
		this.setCarbonFoliageSen(0);
		this.setCarbonBranchesSen(0);
		this.setCarbonFruitSen(0);
		this.setNitrogenFoliageSen(0);
		this.setNitrogenBranchesSen(0);
		this.setNitrogenFruitSen(0);
		this.setCarbonFineRootsSen (0);
		this.setCarbonCoarseRootsSen (0);
		this.setNitrogenFineRootsSen (0);
		this.setNitrogenCoarseRootsSen (0);	
		this.setCarbonFineRootsSenAnoxia (0);
		this.setCarbonCoarseRootsSenAnoxia (0);
		this.setNitrogenFineRootsSenAnoxia (0);
		this.setNitrogenCoarseRootsSenAnoxia (0);
		this.setCarbonStemExported (0);
		this.setCarbonStumpExported (0);
		this.setCarbonFoliageExported (0);
		this.setCarbonBranchesExported (0);
		this.setCarbonFruitExported (0);		 
		this.setNitrogenStemExported (0);
		this.setNitrogenStumpExported (0);
		this.setNitrogenFoliageExported (0);
		this.setNitrogenBranchesExported (0);	
		this.setNitrogenFruitExported (0);	
		this.setNbCellsBellow (0);
		this.setFruitThinning(0);
		
		//litters
		setCarbonFoliageLitterUnderTree (0);
		setCarbonBranchesLitterUnderTree (0);
		setCarbonFruitLitterUnderTree (0);
		setNitrogenFoliageLitterUnderTree (0);
		setNitrogenBranchesLitterUnderTree (0);
		setNitrogenFruitLitterUnderTree (0);
		
		setCarbonFoliageLitterAllPlot(0);
		setCarbonBranchesLitterAllPlot(0);
		setCarbonFruitLitterAllPlot (0);
		setNitrogenFoliageLitterAllPlot (0);
		setNitrogenBranchesLitterAllPlot (0);
		setNitrogenFruitLitterAllPlot(0);
		

		Arrays.fill(this.leafLue, 0);
		
		
		this.nbrDaysSinceLastIrrigation = this.nbrDaysSinceLastIrrigation + 1;
		this.nbrDaysSinceLastFertilization = this.nbrDaysSinceLastFertilization + 1;

		//store total carbon Pool J-1
		this.carbonTotalBefore = this.getTotalCarbonFoliage()+ 
									carbonBranches + 
									carbonStem + 
									carbonStump + 
									carbonFruit+
									carbonCoarseRoots + 
									carbonFineRoots+ 
									carbonLabile; 
		
		if (plantRoots != null) plantRoots.dailyRaz();
		
	}
	/**
	 * Reload the tree species parameter file
	 * used when we reopen a project and changing some tree parameters 
	 */
	public void reloadSpecies (SafeEvolutionParameters ep, SafeGeneralParameters safeSettings, String treeSpeciesName) throws Exception {

		//Initialise tree species
		//on cherche en priorite les fichiers parametres dans le repertoire de simulation
		//s'il n'existe pas on cherche ensuite dans le repertoire safe/data/simSettings	
		//en dernier lieu on prend le repertoire pas defaut safe/data

		try {
			
			String dataPath = ep.simulationDir + "/treeSpecies/"+treeSpeciesName+".tree";
			SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
			new SafeTreeFormat (dataPath).load (treeSpecies);
			this.treeSpecies = treeSpecies;
			
		} catch (CancellationException e) {
			throw e;	
		} catch (Exception e) {



				try {
				
					String dataPath = safeSettings.dataOriginalPath + "/treeSpecies/"+treeSpeciesName+".tree";
					SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
					new SafeTreeFormat (dataPath).load (treeSpecies);
					this.treeSpecies = treeSpecies;
					System.out.println("WARNING treeSpecies read in folder "+safeSettings.dataOriginalPath);
	
				} catch (Exception e2) {
					System.out.println("Probleme loading treeSpecies parameter file "+e2);
					throw e2;					
				}
			
		}
		
	
		this.treeSpecies.setFileName(treeSpeciesName);

		

	}
	
	/**
	 * Load the tree intervention parameter file
	 */
	public void loadItk (SafeEvolutionParameters ep, SafeGeneralParameters safeSettings) throws Exception {

		//Initialise tree interventions
		//on cherche en priorite les fichiers parametres dans le repertoire de simulation
		//s'il n'existe pas on cherche ensuite dans le repertoire safe/data/simSettings	
		//en dernier lieu on prend le repertoire pas defaut safe/data
		int i = 0;
		String treeItkName = ep.treeTecList.get(i);
		if (this.getId() > 1 && ep.treeTecList.size()>1) {
			treeItkName = ep.treeTecList.get(this.getId()-1);
		}
		 
		try {
			
			String dataPath = ep.simulationDir + "/treeInterventions/"+treeItkName;

			SafeTreeItk treeItk	= new SafeTreeItk ();
			new SafeTreeItkFormat (dataPath).load (treeItk, ep);
			this.treeItk = treeItk;
			
		} catch (CancellationException e) {
			System.out.println("TREE ITK FILE "+treeItkName+" PROBLEM in folder "+ep.simulationDir + "/treeInterventions/");
			throw e;	
		} catch (Exception e) {

			throw e;
		}

	}
	/**
	 * Planting a SafeTree.
	 */
	public void plant (SafeStand stand, boolean debug) throws Exception {


		SafeCell cell = (SafeCell) this.getCell();
		if (cell.getIdTreePlanted()>0) {
			System.out.println("TREE already PLANTED in this CELL");
			throw new CancellationException();
			
		}
		//Plant the tree
		setPlanted (true);

		//INIT C et N POOLS
		double woodDensity	 	 = this.getTreeSpecies ().getWoodDensity ();
		double woodCarbonContent = this.getTreeSpecies ().getWoodCarbonContent();
		double aLeafArea		 = this.getTreeSpecies ().getLeafAreaCrownVolCoefA();
		double bLeafArea		 = this.getTreeSpecies ().getLeafAreaCrownVolCoefB();
		double leafMassArea  	 = this.getTreeSpecies ().getLeafMassArea ();
		double leafCarbonContent = this.getTreeSpecies ().getLeafCarbonContent();
		double initialTargetLfrRatio = this.getTreeSpecies ().getInitialTargetLfrRatio();
		
		//Initialisation of tree dimension
		setHeight (this.treeItk.plantingHeight);
		setCrownBaseHeight (this.treeItk.plantingCrownBaseHeight);
		setAge(this.treeItk.plantingAge);
	
		for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
			setLeafAge(this.getTreeItk().plantingCohortAge.get(index), index);
		}

		//Tree dimensions are linked through the following allometric relationships : 
		// 1) height=aTree * dbh^bTree
		double aTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		double truncationRatio = this.getTreeSpecies ().getEllipsoidTruncationRatio ();
		
		setDbhMeters (Math.pow (((1/aTree) * getHeight()) , (1/bTree)));		//m
		setDbh (getDbhMeters() * 100);									//cm
		setDBaseMeters(getDbhMeters() * (getHeight()/(getHeight() - 1.3)));	//m 
	
		//Ellipsoid crown shape (after G.Vincent 2003)
		int crownShape 	= this.getTreeSpecies ().getCrownShape ();
		double aCrown	= this.getTreeSpecies ().getCrownDbhAllometricCoeffA ();
		double bCrown 	= this.getTreeSpecies ().getCrownDbhAllometricCoeffB ();
		if (crownShape == 1) {
			//if truncated ellipsoid
			if (truncationRatio == 1) {		//100%
				setCrownRadiusVertical (0);				//METTRE ICI UN MESSAGE ERREUR !!!!!
			}
			else
			{
				setCrownRadiusVertical (((getHeight()-getCrownBaseHeight())/(1-truncationRatio))/2);
			}
			double crownArea = Math.PI*Math.pow(this.treeItk.plantingCrownRadius,2);		// gt - 5.10.2009 - initial crown radius is now given in the pld input file
			//crown radius calcultation limited by tree line and inter-row distance
			double newRadius [] = new double [2];
			newRadius = computeRadiiDeformation (crownArea , stand);
			setCrownRadiusTreeLine   (Math.max(this.getCrownRadiusTreeLine(),newRadius[0]));
			setCrownRadiusInterRow   (Math.max(this.getCrownRadiusInterRow(),newRadius[1]));

			//Elipsoide crown volume with trucature ratio
			setCrownVolume (Math.PI  * this.getCrownRadiusTreeLine() * this.getCrownRadiusInterRow() * this.getCrownRadiusVertical() *
			 				(2 * (1-truncationRatio) - (1d/3d * (1 - Math.pow (2 * truncationRatio-1,3)))));
		}

	
		//Paraboloid of revolution crown shape (after B.Courbeaud 2000)
		if (crownShape == 2) {
			setCrownParameter ((aCrown * Math.pow ((this.getDbhMeters () / getHeight()), bCrown)));
			setCrownRadiusTreeLine (Math.sqrt (this.getCrownParameter() * (getHeight() - getCrownBaseHeight())));
			setCrownRadiusInterRow (Math.sqrt (this.getCrownParameter() * (getHeight() - getCrownBaseHeight())));
			setCrownRadiusVertical ((getHeight()-getCrownBaseHeight())/2);
			setCrownVolume (1d/2d * Math.PI * this.getCrownParameter ()
							* Math.pow (getHeight()-getCrownBaseHeight(),2));
		}
				
		//At initialisation step, the stem volume needs to be computed from
		//tree dimensions while later the inverse calculation is made as stem volume is
		//determined by stem carbon pool.
		//Here we compute the volume of the solid of revolution using the species specific stem profile equation :
		//Ln(V) = a + b*Ln(dbh) + c*Ln(Height)
		double aStemVolume 			= this.getTreeSpecies ().getStemDbhAllometricCoeffA ();
		double bStemVolume 			= this.getTreeSpecies ().getStemDbhAllometricCoeffB ();
		double cStemVolume 			= this.getTreeSpecies ().getStemDbhAllometricCoeffC ();

		setStemVolume (Math.exp (aStemVolume) * Math.pow (getDbhMeters (),bStemVolume)
		               * Math.pow (getHeight(),cStemVolume));		
		
		
		setTargetLfrRatio (initialTargetLfrRatio);
		
		setCarbonStem (getStemVolume()			// m3
						* woodDensity 			// kg m-3
						* woodCarbonContent);	// %
						
		//deciduous tree (single cohort) 
		if (this.getTreeSpecies().getPhenologyType() == 1) {
			if (!((this.getPhenologicalStage()==0)||(this.getPhenologicalStage()==4))){
				setCarbonFoliage  (aLeafArea*Math.pow(getCrownVolume(),bLeafArea)		// m2 
									* leafMassArea		// kg m-2
									* leafCarbonContent,0);	// %
			} else setCarbonFoliage (0,0);

		}
		//evergreen (leave at planting) 
		//carbonFoliage depending of CrownVolume
		else {
			 
			double carbonFoliageTotal = (aLeafArea*Math.pow(getCrownVolume(),bLeafArea)		// m2 
										* leafMassArea										// kg m-2
										* leafCarbonContent);								// %
			int nbCohortWithLeaves = 0;
			for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
				if (leafAge[index] > 0) {
					nbCohortWithLeaves++;
				};
			}
			if (nbCohortWithLeaves>0) {
				double carbonCohort = carbonFoliageTotal/nbCohortWithLeaves;
				for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
					if (leafAge[index] > 0) setCarbonFoliage (carbonCohort, index);
					else  setCarbonFoliage (0, index);
				}
			}

		}

		//BranchVolume is set to a fixed fraction of total crown volume (to be refined later based on AMAP architectural models)
		setCarbonBranches(getBranchVolume() * woodDensity * woodCarbonContent);
		setCarbonStump(getCarbonStem()*this.getTreeSpecies().getStumpToStemBiomassRatio());

		double targetCarbonFoliage = aLeafArea*Math.pow(getCrownVolume(),bLeafArea)		// m2
								* leafMassArea		// kg m-2
								* leafCarbonContent;	// %
		double carbonFineRoot = targetCarbonFoliage*(1-initialTargetLfrRatio)/initialTargetLfrRatio;
		
		setCarbonFineRoots (carbonFineRoot);
		
		// nitrogen pools are set to optimum level at planting
		// optimum level being defined as the level below which the nitrogen stress
		// factor will be different from 1
		double optiNCStem 	 	= this.getTreeSpecies ().getOptiNCStem ();
		double optiNCStump 	 	= this.getTreeSpecies ().getOptiNCStump ();
		double optiNCFoliage 	= this.getTreeSpecies ().getOptiNCFoliage ();
		double optiNCBranch 	= this.getTreeSpecies ().getOptiNCBranch ();
		double optiNCFineRoot 	= this.getTreeSpecies ().getOptiNCFineRoot ();

		setNitrogenStem (getCarbonStem() * optiNCStem);
		setNitrogenStump(getCarbonStump() * optiNCStump);
		setNitrogenBranches	(getCarbonBranches() * optiNCBranch) ;
		setNitrogenFineRoots  (carbonFineRoot * optiNCFineRoot);
		
		for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) 
			setNitrogenFoliage (getCarbonFoliageCohort(index) * optiNCFoliage, index);
		
		setCarbonFruit(0); 
		setNitrogenFruit(0); 
		
		//cold temperature accumulation have to be set to trigger temperature 
		//otherwise budburst will not be triggered the first year 
		if (this.getTreeSpecies ().getColdRequirement ()) {
			setBudburstAccumulatedColdTemperatureStarted(true);
			setBudburstAccumulatedColdTemperature(this.getTreeSpecies ().getColdBudBurstTriggerTemp());
		}

		// initialisation of carbon senescence value	
		setCarbonFoliageSen(0);
		setNitrogenFoliageSen(0);
		setCarbonFineRootsSen(0);
		setCarbonCoarseRootsSen(0);
		setCarbonFineRootsSenAnoxia(0);
		setCarbonCoarseRootsSenAnoxia(0);		
		setNitrogenFineRootsSen(0);
		setNitrogenCoarseRootsSen(0);
		setNitrogenFineRootsSenAnoxia(0);
		setNitrogenCoarseRootsSenAnoxia(0);
		setCarbonFineRootsIncrement(0);
		setNbrDaysInShade(0);
		

		cell.setIdTreePlanted(this.getId()); 

	}

	/**
	 * Harvesting a SafeTree.
	 */
	public void harvest (SafePlot initPlot, int year, int day, SafeGeneralParameters settings) {

		SafeCell cellPlanted = (SafeCell) this.getCell();
		cellPlanted.setIdTreePlanted(0); 
		
		//Harvest the tree
		this.setHarvested(true);
		this.setHarvestingYear (year);
		this.setHarvestingDay  (day);

		//Keep carbon pool values exported in the tree
		this.setCarbonStemExported(this.getCarbonStem());
		this.setCarbonFoliageExported(this.getTotalCarbonFoliage());
		this.setCarbonBranchesExported(this.getCarbonBranches());
		this.setCarbonFruitExported(this.getCarbonFruit());
		this.setNitrogenStemExported(this.getNitrogenStem());
		this.setNitrogenFoliageExported(this.getTotalNitrogenFoliage());
		this.setNitrogenBranchesExported(this.getNitrogenBranches());
		this.setNitrogenFruitExported(this.getNitrogenFruit());

		//stump is removed or not 
		this.setCarbonStumpExported(this.getCarbonStump());
		this.setNitrogenStumpExported(this.getNitrogenStump());

		this.setCarbonStem(0);
		this.setCarbonBranches(0);
		this.setCarbonFruit(0);
		this.setNitrogenStem(0);
		this.setNitrogenBranches(0);
		this.setNitrogenFruit(0);
		this.setCarbonStump(0);
		this.setNitrogenStump(0);
		
		this.setHeight(0);
		this.setDbh(0);
		this.setCrownBaseHeight(0);
		this.setCrownRadiusInterRow(0);
		this.setCrownRadiusTreeLine(0);
		this.setCrownRadiusVertical(0);
		this.setCrownVolume(0);
		
		for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
			this.setLeafArea(0, index);
			this.setCarbonFoliage(0, index);
			this.setNitrogenFoliage(0, index);
		}
		
		// root senescence 
		int treeIndex = this.getId()-1;
		for (Iterator c = initPlot.getCells ().iterator (); c.hasNext ();) {
			
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel voxels[] = cell.getVoxels();
			//FOR EACH VOXEL
			for (int i = 0; i < voxels.length; i++) {
				double voxelBottom = voxels[i].getZ()+(voxels[i].getThickness()/2);
		
				voxels[i].addTreeCarbonFineRootsSen (treeIndex, voxels[i].getTheTreeCarbonFineRoots(treeIndex));
				voxels[i].setTreeCarbonFineRoots(treeIndex, 0);
				voxels[i].addTreeCarbonCoarseRootsSen (treeIndex, voxels[i].getTheTreeCarbonCoarseRoots(treeIndex));
				voxels[i].setTreeCarbonCoarseRoots(treeIndex, 0);			
				
				//for deep roots mineralisation 
				if (voxelBottom > initPlot.getSoil().getHumificationDepth()) {
					voxels[i].addCumulatedTreeNitrogenRootsSen (voxels[i].getTheTreeNitrogenFineRoots(treeIndex));//kg 
					voxels[i].addCumulatedTreeNitrogenRootsSen (voxels[i].getTheTreeNitrogenCoarseRoots(treeIndex));//kg 
				}
				
				voxels[i].addTreeNitrogenFineRootsSen (treeIndex, voxels[i].getTheTreeNitrogenFineRoots(treeIndex));
				voxels[i].setTreeNitrogenFineRoots(treeIndex, 0);
				voxels[i].addTreeNitrogenCoarseRootsSen (treeIndex, voxels[i].getTheTreeNitrogenCoarseRoots(treeIndex));
				voxels[i].setTreeNitrogenCoarseRoots(treeIndex, 0);	

				voxels[i].addTreeRootsDensitySen(treeIndex, 	voxels[i].getTheTreeRootsDensity(treeIndex));
				voxels[i].setTreeRootsDensity(treeIndex, 0);
				

				
			}	
		}
		this.plantRoots = null;
		this.plantRoots = new SafePlantRoot (this);	
		this.setCarbonFineRootsSen(this.getCarbonFineRoots());
		this.setNitrogenFineRootsSen(this.getNitrogenFineRoots());
		this.setCarbonCoarseRootsSen(this.getCarbonCoarseRoots());
		this.setNitrogenCoarseRootsSen(this.getNitrogenCoarseRoots());
		this.setCarbonFineRoots(0);
		this.setCarbonCoarseRoots(0);
		this.setNitrogenFineRoots(0);
		this.setNitrogenCoarseRoots(0);
	
	}
	/**
	 * Calculate totals for annual Export 
	 */	
	public void processTotal () {

		if (this.getTotalLeafArea() > this.leafAreaMax) this.leafAreaMax = this.getTotalLeafArea(); 
		if (this.getTotalCarbonFoliage() > this.carbonFoliageMax) this.carbonFoliageMax = this.getTotalCarbonFoliage(); 
		this.waterUptakeAnnual += this.getWaterUptake();
		this.nitrogenUptakeAnnual += this.getNitrogenUptake();
		this.parInterceptedAnnual +=  this.directParIntercepted + this.diffuseParIntercepted;
		this.interceptedRainAnnual +=  this.interceptedRain;	
		this.carbonAllocToGrowthAnnual +=  this.carbonAllocToGrowth;
		this.carbonFoliageSenAnnual +=  this.carbonFoliageSen;
		this.carbonFineRootsSenAnnual +=  this.carbonFineRootsSen;
		this.carbonCoarseRootsSenAnnual +=  this.carbonCoarseRootsSen;
		this.carbonFineRootsSenAnoxiaAnnual +=  this.carbonFineRootsSenAnoxia;
		this.carbonCoarseRootsSenAnoxiaAnnual +=  this.carbonCoarseRootsSenAnoxia;
	}
	/**
	 * RAZ annual totals for export
	 */
	public void razTotalAnnual() {

		waterStressSpring = 1 ;
		waterStressSummer = 1 ;
		nitrogenStressSpring = 1 ;
		nitrogenStressSummer = 1 ;
		waterUptakeAnnual = 0; 
		nitrogenUptakeAnnual = 0; 
		leafAreaMax= 0;
		carbonFoliageMax = 0;
		parInterceptedAnnual = 0;
		interceptedRainAnnual = 0;	
		carbonAllocToGrowthAnnual = 0;
		carbonFoliageSenAnnual = 0;
		carbonFineRootsSenAnnual = 0;
		carbonCoarseRootsSenAnnual = 0;
		carbonFineRootsSenAnoxiaAnnual = 0;
		carbonCoarseRootsSenAnoxiaAnnual = 0;
		

		if (isPlanted()) {
			this.addYear();
			//for evergreen trees 
			//each year cohort N go to cohort N+1 and first cohort is set to 0 
			if (this.getTreeSpecies ().getPhenologyType() == 2)  {
				int index = this.getTreeSpecies().getNbCohortMax()-1; 
	        	this.carbonFoliageSen = this.carbonFoliage[index];
	        	this.nitrogenFoliageSen = this.nitrogenFoliage[index];
		        while (index > 0) {
		        	this.leafArea[index] = this.leafArea[index-1];
		        	this.leafAge[index] = this.leafAge[index-1];
		        	this.carbonFoliage[index] = this.carbonFoliage[index-1];
		        	this.nitrogenFoliage[index] = this.nitrogenFoliage[index-1];
		        	index --;
		        }
		        this.leafArea[0] = 0;
		        this.leafAge[0] = 0;
		        this.leafLue[0] = 0;
		        this.carbonFoliage[0] = 0;
		        this.nitrogenFoliage[0] = 0;
		        setNbrDaysInShade(0);
			}
		}
		
		this.setPhenologicalStage(0); 	
		setBudburstAccumulatedTemperatureStarted(false);
		setBudburstAccumulatedTemperature(0);			
		this.setFruitPhenologicalStage(0);
		setHeatAccumulatedTemperatureStarted(false);

		
		
	}

	
	
	/**
	* Tree fine roots initialisation
	* Done before coarse Root
	*/
	public void fineRootsInitialisation (SafePlot plot, 
										SafeGeneralParameters settings, 
										 SafePlotSettings plotSettings, 
										 SafeEvolutionParameters evolutionParameters) {

		int treeIndex = this.getId() - 1;		//index of this tree
		
		//IL 02/01/2023 (https://github.com/hisafe/hisafe/issues/42) 
		//the tree Z has to be 0 (in case of slope) 
		//to avoid to have roots above horizon and not in the soil
		//Vertex3d treeCoordinate = new Vertex3d(this.getX(),this.getY(),this.getZ());	
		Vertex3d treeCoordinate = new Vertex3d(this.getX(),this.getY(),0);
		int treeInitIndex = treeIndex;			//index of initialisation values

		//retrieve carbon pool calculated for fine root
		//and convert in root lenght (m)
		double carbonToDryMatter= 1d / this.getTreeSpecies ().getWoodCarbonContent ();
		double totalTreeRootLength =  this.getCarbonFineRoots()
									* carbonToDryMatter 									//convert C to dry matter
									* this.getTreeSpecies().getSpecificRootLength() 		//convert grammes of dry matter to m
									* 1000;													//convert kg to gr

		// Available equations for root shape are :
		//  1- Spherical 2- Elipsoidal 3- Conic		
		
		int rootShape = treeItk.plantingRootShape;
		double shapeParam1=0;
		double shapeParam2=0;
		double shapeParam3=0;

		// Available equations for carbon repartition are :
		//  1- Uniform  2- Reverse to the tree distance 3- Negative exponential
		int rootRepartition = treeItk.plantingRootRepartition;

		if (rootShape == 1) {	//sphere
			shapeParam1 = treeItk.plantingRootShapeParam1;
		}
		if (rootShape == 2) {	//elipsoide
			shapeParam1 = treeItk.plantingRootShapeParam1;
			shapeParam2 = treeItk.plantingRootShapeParam2;
			shapeParam3 = treeItk.plantingRootShapeParam3;
		}
		if (rootShape == 3) {	//cone
			shapeParam1 = treeItk.plantingRootShapeParam1;
			shapeParam2 = treeItk.plantingRootShapeParam2;
		}

		//for each voxel of the plot, find if it is rooted or not
		//comparing tree-voxel distance with a criteria depending of root shape
		double totalCoefficient = 0;
		double plotWidth = plot.getXSize();
		double plotHeight = plot.getYSize();
		for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			SafeVoxel [] voxel = cell.getVoxels();
			for (int i=0; i<voxel.length; i++) {

				//Compute this voxel gravity center distance with the tree origin
				voxel[i].computeTreeDistance (treeIndex, treeCoordinate);

				//Check if the voxel is rooted according to the criteria
				double coefficient = 0;

				if (voxel[i].isVoxelInShape (rootShape, treeCoordinate, shapeParam1, shapeParam2, shapeParam3,
				 								plotWidth, plotHeight, evolutionParameters)) {
					coefficient = voxel[i].computeRepartitionCoefficient (treeIndex, rootRepartition);

					if (coefficient > 0) totalCoefficient += coefficient;
				}
			}
		}

		//Compute carbon repartition on rooted voxels
		if (totalCoefficient > 0) {	//if some voxel are rooted
			
			double totalRootDensity = 0; 
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeVoxel [] voxel = cell.getVoxels();
				for (int i=0; i<voxel.length; i++) {
				
					if (voxel[i].isVoxelInShape (rootShape, treeCoordinate, shapeParam1, shapeParam2,shapeParam3,
												plotWidth, plotHeight, evolutionParameters)) {

						//compute the fine root density value
						totalRootDensity += voxel[i].computeTreeFineRootsRepartition (treeIndex, rootRepartition,
																totalTreeRootLength, totalCoefficient);
						
						//ADD empty topology to be filled by coarse root initialisation	
						if (!this.getPlantRoots().getRootTopology().containsKey(voxel[i]))
							this.getPlantRoots().addEmptyRootTopology (voxel[i]);
					}						
				}
			}

			//compute carbon in fine root in each voxels (proportional to total FR)
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeVoxel [] voxel = cell.getVoxels();
				for (int i=0; i<voxel.length; i++) {				
					if (voxel[i].getTheTreeRootsDensity (treeIndex) > 0) {
					
						double carbon = getCarbonFineRoots() * (voxel[i].getTheTreeRootsDensity (treeIndex) / totalRootDensity);
						voxel[i].setTreeCarbonFineRoots (treeIndex, carbon);
						
						double nitrogen = getNitrogenFineRoots() * (voxel[i].getTheTreeRootsDensity (treeIndex) / totalRootDensity);
						voxel[i].setTreeNitrogenFineRoots (treeIndex, nitrogen);
						
					}						
				}
			}
		}
	}
	
	/**
	* Tree coarse roots initialisation
	*/
	public void coarseRootsInitialisation (	SafePlot plot, 
											SafeGeneralParameters settings, 
											SafePlotSettings plotSettings) throws Exception  {		

		PlotOfCells plotc = (PlotOfCells) plot; 
				
		//Coarse root topology type defines priority to parent determination
		double  coarseRootTopologyType 	= this.getTreeSpecies().getCoarseRootTopologyType();

		///Search the voxel reference where the tree is planted
		int treeId = this.getId() - 1 ;
		SafeCell plantedCell = (SafeCell) (this.getCell ());
		SafeVoxel [] voxel = plantedCell.getVoxels();
		SafeVoxel plantedVoxel = voxel[0];
		plantedVoxel.setColonisationDirection(treeId, 4);
		
		//Init TREE coarse root node
		SafeRootNode plantedNode = this.getPlantRoots().getRootTopology (plantedVoxel);
		this.getPlantRoots().setFirstRootNode (plantedNode);

		//for each voxel of the PLOT
		for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			SafeVoxel [] voxels = cell.getVoxels();
			for (int i=0; i<voxels.length; i++) {

				//if this voxel is rooted by this TREE
				if (voxels[i].getTheTreeRootsDensity (treeId) > 0) {
					SafeVoxel parentVoxel = null;
					int direction = 0;
					int directionSide = 0;
					
					//if the voxel is not the voxel reference where the tree is planted
					if (voxels[i] != plantedVoxel) {

						//Looking for the 4th neighbourgs (left, right, front, back)
						SafeVoxel [] neighbourgs = new SafeVoxel[4];

						//RIGHT voxel   (X positive)
						SafeCell rightNeighbourg = (SafeCell) (plotc.getCell (cell.getCellIdRight()));
						if (rightNeighbourg != null) {	//can be null if toric symetry is OFF of Xp
							SafeVoxel [] rightVoxels = rightNeighbourg.getVoxels();
							if (voxels[i] != rightVoxels[i])
								neighbourgs[0] = rightVoxels[i];
						}

						//LEFT voxel  (X negative)
						SafeCell leftNeighbourg = (SafeCell) (plotc.getCell (cell.getCellIdLeft()));
						if (leftNeighbourg != null) {	//can be null if toric symetry is OFF of Xn
							SafeVoxel [] leftVoxels = leftNeighbourg.getVoxels();
							if (voxels[i] != leftVoxels[i])
								neighbourgs[1] = leftVoxels[i];
						}

						//BACK voxel (Y positif)
						SafeCell backNeighbourg = (SafeCell) (plotc.getCell (cell.getCellIdBack()));
						if (backNeighbourg != null) {	//can be null if toric symetry is OFF of Yp
							SafeVoxel [] backVoxels = backNeighbourg.getVoxels();
							if (voxels[i] != backVoxels[i])
								neighbourgs[2] = backVoxels[i];
						}

						//FRONT voxel (Y negative)
						SafeCell frontNeighbourg = (SafeCell) (plotc.getCell (cell.getCellIdFront()));
						if (frontNeighbourg != null) {	//can be null if toric symetry is OFF of Yn
							SafeVoxel [] frontVoxels = frontNeighbourg.getVoxels();
							if (voxels[i] != frontVoxels[i])
								neighbourgs[3] = frontVoxels[i];
						}
						

						//ABOVE voxel (Z negative)
						//NO BELLOW voxel (impossible to have a bigger density bellow)
						SafeVoxel topVoxel = null;
						if (i > 0) {
							topVoxel = voxels[i-1];
						}

						SafeVoxel sideVoxel = null;
						double densityMax = 0;
						double distanceMin = 0;
						boolean hightestRootedVoxel = false;

						//priority to vertical
						if (coarseRootTopologyType == 1) {
							if (topVoxel != null) {
								densityMax = topVoxel.getTheTreeRootsDensity (treeId);
							}
							//explore other voxel in case of highest density
							for (int k=0; k<4; k++) {
								if (neighbourgs[k] != null) {
									if (neighbourgs[k].getTheTreeRootsDensity (treeId) > densityMax) {
										densityMax  = neighbourgs[k].getTheTreeRootsDensity (treeId);
										distanceMin = neighbourgs[k].getTheTreeDistance (treeId);
										sideVoxel   = neighbourgs[k];
										hightestRootedVoxel = true;
										directionSide = k;
									}
								}
							}
							//between the same density, choose the one with samellest distance
							if (hightestRootedVoxel) {
								for (int k=0; k<4; k++) {
									if (neighbourgs[k] != null) {
										if (neighbourgs[k].getTheTreeRootsDensity (treeId) == densityMax) {
											if (neighbourgs[k].getTheTreeDistance (treeId) < distanceMin) {
												distanceMin = neighbourgs[k].getTheTreeDistance (treeId);
												densityMax  = neighbourgs[k].getTheTreeRootsDensity (treeId);
												sideVoxel   = neighbourgs[k];
												directionSide = k;
											}
										}
									}
								}
							}
							if (topVoxel != null) {
								parentVoxel = topVoxel;
								direction = 4;
								//one voxel on the side have a highest root density
								if (sideVoxel != null) {
									parentVoxel = sideVoxel;
									direction = directionSide;
								}
							}
							else {
								parentVoxel = sideVoxel;
								direction = directionSide;
							}

						}
						//priority to horizontal
						else {

							for (int k=0; k<4; k++) {
								if (neighbourgs[k] != null) {
								//the father will be the neightbourg voxel with highest root density
									if (neighbourgs[k].getTheTreeRootsDensity (treeId) > densityMax) {
										distanceMin = neighbourgs[k].getTheTreeDistance (treeId);
										densityMax  = neighbourgs[k].getTheTreeRootsDensity (treeId);
										parentVoxel = neighbourgs[k];
										direction = k;
									}
									//in CASE of equal density
									else {
									//the father will be the closest to the tree collar
										if (neighbourgs[k].getTheTreeDistance (treeId) < distanceMin) {
											parentVoxel = neighbourgs[k];
											distanceMin = neighbourgs[k].getTheTreeDistance (treeId);
											direction = k;
										}
									}
								}
							}
							//top voxel is the father only if density > densityMax
							if (topVoxel != null) {
								if (topVoxel.getTheTreeRootsDensity (treeId) > densityMax) {
									parentVoxel = topVoxel;		
									direction = 4;
								}
							}
						}
					}

					//UPDATE empty topology already created in fine root initialisation
					if (this.getPlantRoots().getRootTopology ().containsKey(voxels[i])) {
						SafeRootNode node = this.getPlantRoots().getRootTopology (voxels[i]);
  						if (parentVoxel != null) {
								SafeRootNode fatherNode = this.getPlantRoots().getRootTopology (parentVoxel);
								node.setNodeParent(fatherNode);
								fatherNode.addNodeColonised (node);									
								node.computeDistanceFromFather(direction);	//calculate distance from father
								node.computeEffDist(this);
								voxels[i].setColonisationDirection(treeId, direction);
						}
  						node.setFineRootsDensity (voxels[i].getTheTreeRootsDensity (treeId));
					}
				}
			}
		}
		
		//recursive algorithme for updating distances and carbon
		if (plantedNode != null) {
			plantedNode.setDistances(0);
			//Initialise carbon coarse root 
			this.carbonCoarseRootsInitialisation (plot, settings, plotSettings);	
		}
		else {
			System.out.println("Tree planting with no roots ");

			throw new CancellationException();	// abort
		}
	
	}
	
	/**
	 * Coarse roots carbon initialisation
	 * @author Rachmat MULIA (ICRAF) - August 2004
	 * updated by G.Talbot and I.Lecomte - September 2008
	 */
	public void carbonCoarseRootsInitialisation (SafePlot plot, SafeGeneralParameters settings, SafePlotSettings plotSettings) {
		
		//if this tree  has fine roots !!!!
		if (this.getPlantRoots().getFirstRootNode() != null) {


			//compute coarse roots target  proportionnally to total tree fine roots
			this.setCarbonCoarseRootsTarget(0);
			SafeRootNode firstNode = this.getPlantRoots().getFirstRootNode () ;
			firstNode.computeCarbonCoarseRootsTarget (this,	
														null,
														this.getTreeSpecies().getWoodDensity(),
														this.getTreeSpecies ().getWoodCarbonContent (),
														this.getTreeSpecies().getCRAreaToFRLengthRatio());

			this.setCarbonCoarseRoots (this.getCarbonCoarseRootsTarget());

			double optiNCCoarseRoot = this.getTreeSpecies ().getOptiNCCoarseRoot ();
			double nitrogenCoarseRootsTarget   = this.getCarbonCoarseRootsTarget() * optiNCCoarseRoot;
			double totalNodeImbalance = this.getCarbonCoarseRootsTarget(); 
			
			//calculation of coarse root  for each root node
			firstNode.computeCarbonCoarseRoots (this, 
												this.getCarbonCoarseRootsTarget(), 
												nitrogenCoarseRootsTarget,
												totalNodeImbalance);

			//recalculation of Nitrogen pools
			setAboveGroundCFraction ((getCarbonBranches() + getCarbonStem())
						/ (getCarbonBranches() + getCarbonStem() + getCarbonStump()+ getCarbonCoarseRoots ()));

			setCarbonLabile((getCarbonStem()+getCarbonBranches()+getCarbonCoarseRoots ()+getCarbonStump())
						*this.getTreeSpecies().getTargetNSCFraction());

			double targetNCoefficient = this.getTreeSpecies ().getTargetNCoefficient ();
			setNitrogenCoarseRoots  (getCarbonCoarseRoots () * optiNCCoarseRoot);
			setNitrogenLabile(targetNCoefficient*getTotalOptiN() - getTotalN());

		}
	}	
	/**
	 * Process growth part I (before water repartition module)
	 */
	public boolean processGrowth1 (SafeStand stand,
									SafeGeneralParameters safeSettings,
									SafePlotSettings plotSettings,
									SafeDailyClimat dayClimat,
									SafeMacroClimat macroClimat,
									boolean debug) {	
		boolean visibleStep = false;

		// Tree vegetative phenology 
		this.vegetativePhenology (macroClimat, dayClimat, debug);

		// Tree fruit phenology 
		if (this.getTreeSpecies ().getFruitCompartment())
			this.fruitPhenology (macroClimat, dayClimat);
		
		// Tree BNF phenology 
		if (this.getTreeSpecies ().getNitrogenFixation())
			this.bnfPhenology (macroClimat, dayClimat);
		
		this.waterStress = 1;
		this.nitrogenStress = 1;
		
		// Potential transpiration
		if (this.getTotalLeafArea() != 0) {		// gt - 01.10.2009 - added phenological stage != : if leafFall began, no more water demand

			double demand = 0;
			double evaporation = 0;
			
			//COLD DECIDUOUS TREES
			//water demand is computed ONLY if there is leaves AND leaves fall is NOT begin
			if (this.getTreeSpecies ().getPhenologyType() == 1) {			
				if ((this.getPhenologicalStage() != 3) && (this.getPhenologicalStage() != 4))
					 demand = this.computeWaterDemand (dayClimat);
			}
			
			
			//EVERGREEN trees water demand is computed all days 
			if ((this.getTreeSpecies ().getPhenologyType() == 2) && (this.getTotalLeafArea() > 0)) { 
				demand = this.computeWaterDemand (dayClimat);
			}
			

			evaporation = this.computeEvaporation (demand);

			if (demand> 0)
				this.setWaterDemand (Math.max(demand - (evaporation*0.99),0));
			
			//Nitrogen demand calculation
			double nitrogenDemand = this.computeNitrogenDemand ();
			setNitrogenDemandBeforeFixation (nitrogenDemand);
			
			//BNF V1 
			//Reduice demand with Nitrogen fixation
			if (this.getTreeSpecies().getNitrogenFixation()) {
				setNitrogenDemandBeforeFixation (nitrogenDemand);
				if (getBnfNitrogenFixation() <= nitrogenDemand) {
					setNitrogenDemandAfterFixation ( nitrogenDemand - getBnfNitrogenFixation());
				}
				else {
					setNitrogenDemandAfterFixation(0);					
				}		
			}
			else 
				setNitrogenDemandAfterFixation (nitrogenDemand);


			//Nitrogen stress calculation
			double nitrogenStress = this.computeNitrogenStress ();
			setNitrogenStress (nitrogenStress);	

			//Nitrogen sink strength with nitrogen demand of the day before
			if (this.getNitrogenDemandAfterFixation () > 0) 
					this.getPlantRoots().calculateNitrogenSinkStrength (safeSettings, this.getNitrogenDemandAfterFixation ());
			
			
			if (this.getTreeSpecies().getNitrogenFixation()) {

				//BNF FIXATION V1 		
				setBnfAnoxStress(1);
				setBnfWaterStress(1);
				setBnfTemperatureStress(1);
				setBnfNodulationInhibition(1);
				setBnfSoilNitrateExcess(1);
				
				if (this.getBnfPhenologicalStage() == 1 || this.getBnfPhenologicalStage() == 2) {
					computeNitrogenFixationLimitationsV1 (stand);
					computeNitrogenFixationV1 (dayClimat); 
				}
				else {
					setBnfNitrogenFixationPotential(0);
					setBnfNitrogenFixation(0);
					setBnfPhenologyCoefficient(0);
					setBnfMaxNitrogenFixationPotential(0);
				}

			}

		} 
		else
		{
			setWaterDemand (0);
			setWaterDemandReduced (0);
			setWaterUptake (0);
			setWaterStress (1);
			setNitrogenDemandBeforeFixation (0);
			setNitrogenDemandAfterFixation (0);
			setNitrogenUptake (0);
			setNitrogenAvailable (0);
			setNitrogenStress (1);
			setLaiAboveCells (0);
			setStoredRain (0); //no transpiration all stored rain is evaporated by atmosphere
		}

		return visibleStep;
	}
	/**
	 * Process growth part II (after water repartition)
	 */
	public void processGrowth2 (SafeStand stand,
								SafeGeneralParameters safeSettings,
								SafePlotSettings plotSettings,
								SafeEvolutionParameters ep,
								SafeDailyClimat dayClimat,
								int yearIndex,
								int simulationDay) {

		setCarbonFruitIncrement (0);
		
		// J=365 , the tree is one year older
		int julianDay = dayClimat.getJulianDay ();
		float tmean = dayClimat.getMeanTemperature();
		
		// Water stress calculation
		if ((getWaterDemand () > 0) /*&& getWaterUptake () > 0*/){	// gt 30.07.2009
			if(getWaterUptake() <= 0){
				setWaterStress (0.0001d);
			} else {
				double waterStress = Math.min (getWaterUptake () / getWaterDemand (), 1);				
				waterStress = Math.max (waterStress, 0.0001d);
				setWaterStress (waterStress);	

			}
		}
		else {
			setWaterStress (1);
		}

		//Compute spring and summer stress
		if (julianDay >= 91 && julianDay < 181) {
			this.waterStressSpring = this.waterStressSpring + (1-waterStress); 
		}
		if (julianDay > 181 && julianDay < 273) {
			this.waterStressSummer = this.waterStressSummer + (1-waterStress);
		}	
		

		//Compute spring and summer stress
		if (julianDay >= 91 && julianDay < 181) {
			nitrogenStressSpring = nitrogenStressSpring + (1-nitrogenStress); 
		}
		if (julianDay > 181 && julianDay < 273) {
			nitrogenStressSummer = nitrogenStressSummer + (1-nitrogenStress); 
		}	
		
		//compute heat stress effect on flowering  
		setFloweringHeatStress(1);
		if (this.getTreeSpecies ().getFruitCompartment()) {
			double THSMin = this.getTreeSpecies().getFruitHeatStressTemperatureMin(); 
			double THSMax = this.getTreeSpecies().getFruitHeatStressTemperatureMax();
			double tmax = dayClimat.getMaxTemperature();
			if (tmax < THSMin) {
				setFloweringHeatStress(1);
			}
			else if (tmax <= THSMax) {
				setFloweringHeatStress((THSMax-tmax)/(THSMax-THSMin));
			}
			else {
				setFloweringHeatStress(0);
			}
		}
		
		
		//compute frost stress effect on flowering  
		setFloweringFrostStress(1);
		if (this.getTreeSpecies ().getFruitCompartment()) {
			double TFSMin = this.getTreeSpecies().getFruitFrostStressTemperatureMin(); 
			double TFSMax = this.getTreeSpecies().getFruitFrostStressTemperatureMax();
			double tmin = dayClimat.getMinTemperature();
			if (tmin > TFSMax) {
				setFloweringFrostStress(1);
			}
			else if (tmin < TFSMin) {				
				setFloweringFrostStress(0);
			}
			else {				
				setFloweringFrostStress((tmin-TFSMin)/(TFSMax-TFSMin));
			}
		}
		
		//STRESSES ON LUE
		double lueWaterStress = 1;
		double lueNitrogenStress = 1;
		double lueStress = 1;
		double lueTemperatureStress = 1;
		
		//EFFECT OF CO2
		double co2LueEffect = 1;
		double co2WueEffect = 1;
			
		Arrays.fill(this.carbonIncrement, 0);


		//Conversion of light intercepted (Moles PAR) to carbon (kg)
		// gt - 01.10.2009 - added phenological stage != : if leafFall began, no more photosynthesis
		if (((this.getTreeSpecies ().getPhenologyType() == 1) && (this.getTotalLeafArea() > 0) && (this.getPhenologicalStage() !=3 ) && (this.getPhenologicalStage() != 4)) 
		||
		// nb - 24/03/2021 evergreen trees
		((this.getTreeSpecies ().getPhenologyType() == 2) && (this.getTotalLeafArea() > 0)))
		{								

			//WATER AND NITROGEN STRESS	
			double lueStressMethod = this.getTreeSpecies().getLueStressMethod ();
			double lueWaterStressResponsiveness    = this.getTreeSpecies().getLueWaterStressResponsiveness ();
			double lueNitrogenStressResponsiveness = this.getTreeSpecies().getLueNitrogenStressResponsiveness ();
			
			lueWaterStress     = Math.pow (getWaterStress(),lueWaterStressResponsiveness);
			lueNitrogenStress  = Math.pow (getNitrogenSatisfaction(), lueNitrogenStressResponsiveness);

			if (lueStressMethod == 1)        
				lueStress = lueWaterStress * lueNitrogenStress;
			else 
				lueStress = Math.min(lueWaterStress, lueNitrogenStress);

			
			//TEMPERATURE STRESS
			double Tmin = this.getTreeSpecies().getLueTemperatureStressTMin();
			double Tmax = this.getTreeSpecies().getLueTemperatureStressTMax();
			double Topt1 = this.getTreeSpecies().getLueTemperatureStressTOptMin();
			double Topt2 = this.getTreeSpecies().getLueTemperatureStressTOptMax();

			if (tmean < Tmin || tmean > Tmax) lueTemperatureStress = 0;
			else {
				if (tmean < Topt1) 
					lueTemperatureStress = 1 - Math.pow ((tmean - Topt1)/(Tmin - Topt1),2);
					
				if ((Topt1<=tmean) && (tmean<=Topt2)) 
					lueTemperatureStress = 1;
					
		
				if (tmean > Topt2) 
					lueTemperatureStress = 1 -  Math.pow((tmean - Topt2)/(Tmax - Topt2),2);
			}

			//EFFECT OF CO2
			boolean co2EffectOnLueActivation    = this.getTreeSpecies().getCo2EffectOnLueActivation ();
			boolean co2EffectOnWueActivation = this.getTreeSpecies().getCo2EffectOnWueActivation ();
			double co2ReferenceValue = this.getTreeSpecies().getCo2ReferenceValue();
			
			if (co2EffectOnLueActivation) {
				double co2EffectOnLueHalfSaturationConstant = this.getTreeSpecies().getCo2EffectOnLueHalfSaturationConstant();
				co2LueEffect = dayClimat.getCO2Concentration() 
							/ (co2EffectOnLueHalfSaturationConstant + dayClimat.getCO2Concentration())
							/ (co2ReferenceValue / (co2EffectOnLueHalfSaturationConstant + co2ReferenceValue));
				
			}
			if (co2EffectOnWueActivation) {
				double co2EffectIntrinsicWueSensitivity = this.getTreeSpecies().getCo2EffectIntrinsicWueSensitivity();
				co2WueEffect = 1 + (co2EffectIntrinsicWueSensitivity 
							 		* (dayClimat.getCO2Concentration()  - co2ReferenceValue) 
							 		/ co2ReferenceValue);
				
			}

			//LEAF AGE EFFECT
			double lueMax = this.getTreeSpecies ().getLeafLueMax();		//gr C MJ-1 (PAR)
			for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
			
				if (this.getLeafAgeCohort(index) > 0 && this.getLeafAreaCohort(index) > 0) {


					
					int ageMax = this.getLeafAgeCohort(index) - this.getTreeSpecies ().getLeafAgeForLueMax() - 1;	
					
					leafLue[index] = Math.max(lueMax*(1-	this.getTreeSpecies().getLeafPhotosynthesisEfficiencyTimeConstant()*Math.pow(ageMax,2)),0);

					
					carbonIncrement[index] =   
							((getDiffuseParIntercepted () + getDirectParIntercepted ()) * leafArea[index] / this.getTotalLeafArea())
							* safeSettings.molesParCoefficient		 //Conversion from Moles PAR to MJoules
							* lueStress
							* lueTemperatureStress
							* co2LueEffect
							* leafLue[index]						// gr C MJ-1 (PAR)
							/ 1000;									// gr to kg		

					
				}
			}
		}


		setLueStress (lueStress);
		setLueWaterStress (lueWaterStress);
		setLueNitrogenStress (lueNitrogenStress);
		setLueTemperatureStress(lueTemperatureStress);

		setCo2LueEffect(co2LueEffect);
		setCo2WueEffect(co2WueEffect);
		
		// COARSE ROOTS ANOXIA IF WATER TABLE
		if (((SafeSoil) stand.getPlot().getSoil()).isWaterTable()) 
			getPlantRoots().getFirstRootNode().computeCoarseRootsAnoxia(this, safeSettings, stand.getPlot().getSoil().getHumificationDepth());
		
		
		//FINE ROOT SENESCENCE AFTER BUDBURST ONLY
		if (isFirstYearStarted ()) {
			if (getPlantRoots().getFirstRootNode() != null) {
				
				this.setCarbonCoarseRootsTarget(0);
				getPlantRoots().getFirstRootNode().computeCarbonCoarseRootsTarget(this, null, 
																this.getTreeSpecies().getWoodDensity() , 
																this.getTreeSpecies ().getWoodCarbonContent () , 
																this.getTreeSpecies().getCRAreaToFRLengthRatio());

				getPlantRoots().getFirstRootNode().computeFineRootsSenescence(this, safeSettings, stand.getPlot().getSoil().getHumificationDepth());

			}			
		}


		//CARBON FOLIAGE SENESCENCE
		if (this.getTreeSpecies ().getPhenologyType() == 1) {
			
			// gt - 14.01.2009 - the last day of leaf fall, all remaining leaves die
			if ((this.phenologicalStage==0) || (this.phenologicalStage==4)) {
				addCarbonFoliageSen(getCarbonFoliageCohort(0));
				addNitrogenFoliageSen(getNitrogenFoliageCohort(0));
				setCarbonFoliage(0,0);
				setNitrogenFoliage(0,0);
			}
		}
	
		if (this.getTotalCarbonFoliage() != 0) this.computeCarbonFoliageSenescence(julianDay, dayClimat, safeSettings);


		//CARBON ALLOCATION 
		this.computeCarbonAllocation (stand, safeSettings, plotSettings, simulationDay,
								getTotalCarbonIncrement(),
								this.getNitrogenUptake());	

		//ALLOMETRIC GROWTH
		this.computeAllometricGrowth (stand, safeSettings);

		//FINE ROOTS GROWTH
		if (this.getCarbonFineRootsIncrement() > 0) {
			this.fineRootGrowth(safeSettings, plotSettings, simulationDay, this.getCarbonFineRootsIncrement(), true);
		}
		
		//Sometimes nitrogen is allocated to fine root BUT NO carbon
		//This nitrogen will be affected to the first rooted voxel (it is rare and very small values) 
		else if (this.getNitrogenFineRootsIncrement() > 0) {
			this.nitrogenVoxelAllocation(this.getNitrogenFineRootsIncrement());
		}

		//COARSE ROOTS GROWTH
		if (this.getCarbonCoarseRootsIncrement () > 0) {
			this.coarseRootGrowth(this.getCarbonCoarseRootsIncrement(), this.getNitrogenCoarseRootsIncrement());
		}

	}


	/**
	 * Phenology module for cold deciduous and evergreen SafeTree
	 * @author Christian DUPRAZ (INRA SYSTEM) - July 2003
	 * updated by Gregoire Talbot on 29/04/2009
	 */
	private void vegetativePhenology (SafeMacroClimat climat, SafeDailyClimat dayClimat, boolean debug) {

		
		int julianDay = dayClimat.getJulianDay ();
		int dayStart = this.getTreeSpecies ().getBudBurstTempAccumulationDateStart ();	// date to start temperature cumul (julian day)
	
		int dayTest = julianDay;
		if (climat.isLeapYear(dayClimat.getYear())) {
			if (julianDay>366) dayTest = julianDay - 366;
		}
		else {
			if (julianDay>365) dayTest = julianDay - 365;
		}
		
		//Cold temperature accumulation option 
		if (this.getTreeSpecies ().getColdRequirement () && (dayTest == this.getTreeSpecies ().getColdTempAccumulationDateStart ())) {
			setBudburstAccumulatedColdTemperatureStarted(true);
			setBudburstAccumulatedColdTemperature(0);
		}
		if (getBudburstAccumulatedColdTemperatureStarted()) {
			
			float meanTemperature = dayClimat.getMeanTemperature ();

			if (meanTemperature < this.getTreeSpecies ().getColdTempThreshold()) {
				
				double A = this.getTreeSpecies ().getColdBudBurstTriggerParamA ();
				double B = this.getTreeSpecies ().getColdBudBurstTriggerParamB ();
				double C = this.getTreeSpecies ().getColdBudBurstTriggerParamC ();
				double E = this.getTreeSpecies ().getColdBudBurstTriggerParamE ();
				
				double puissance = A * Math.pow(meanTemperature-C ,2) + (B * (meanTemperature - C));
				double ajout = 1d /(1+ Math.pow(E, puissance));
				this.budburstAccumulatedColdTemperature += ajout ;
			}
		}

		
		switch(this.getPhenologicalStage()){
		

		case 0 : // before budburst

			if (dayTest == dayStart) {
				setBudburstAccumulatedTemperatureStarted(true);
				setBudburstAccumulatedTemperature(0);
				setBudburstDate (0);
				if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************Date="+julianDay+" BudburstAccumulatedTemperatureStarted");
			}
			
			
			//here we need ONLY heat temperature accumulation
			if (!this.getTreeSpecies ().getColdRequirement ()) {

				if (getBudburstAccumulatedTemperatureStarted()) {
					
					double budburstTempThreshold = this.getTreeSpecies ().getBudBurstTempThreshold ();

					float meanTemperature = dayClimat.getMeanTemperature ();

					this.budburstAccumulatedTemperature += Math.max(meanTemperature-budburstTempThreshold,0);

					
					if (budburstAccumulatedTemperature > this.getTreeSpecies ().getBudBurstTriggerTemp()) {
						this.setPhenologicalStage(1); 	//budburst
						this.setFruitPhenologicalStage(1);
						setBudburstDate (julianDay);
						setFirstYearStarted (true);
						setLeafAge(1,0);
						if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************BudburstDate="+julianDay+" temp="+budburstAccumulatedTemperature);
					}
				}		
			}
			//here we need cold temperature accumulation AND heat temperature accumulation
			else {

				if (getBudburstAccumulatedTemperatureStarted()) {
					
					double budburstTempThreshold = this.getTreeSpecies ().getBudBurstTempThreshold ();
					float meanTemperature = dayClimat.getMeanTemperature ();
					this.budburstAccumulatedTemperature += Math.max(meanTemperature-budburstTempThreshold,0);

				}
				
				if ((budburstAccumulatedTemperature > this.getTreeSpecies ().getBudBurstTriggerTemp())
						&& (budburstAccumulatedColdTemperature > this.getTreeSpecies ().getColdBudBurstTriggerTemp())) {
					this.setPhenologicalStage(1); 	//budburst
					this.setFruitPhenologicalStage(1);
					setBudburstDate (julianDay);
					setFirstYearStarted (true);
					setLeafAge(1,0);

					if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************BudburstDate="+julianDay+" temp="+budburstAccumulatedTemperature+ " cold="+budburstAccumulatedColdTemperature);
					setBudburstAccumulatedColdTemperatureStarted(false);
					setBudburstAccumulatedColdTemperature(0);

				}					
			}

				
				
		break;
			case 1 : // budburst started
				int leafExpansionDuration = this.getTreeSpecies ().getLeafExpansionDuration ();		// date for the end of leaf expansion (julian day)

	
				if(julianDay-this.getBudburstDate()>leafExpansionDuration){
					this.setPhenologicalStage(2); 	// end of leaf expansion
					setLeafExpansionEndingDate (julianDay);
					if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************LeafExpansionEndingDate="+julianDay+" leafExpansionDuration="+leafExpansionDuration);
				}
				addLeafAge(0);
				//a reactiver lorsque'onaura les bons paramètres
				//if (this.getTreeSpecies ().getFruitCompartment()) fruitColdRequirement = true;
			break;
			case 2 : // leaf expansion finished
				int budBurstToLeafFallDuration = this.getTreeSpecies().getBudBurstToLeafFallDuration();		// date for the end of leaf expansion (julian day)
				double leafFallFrostThreshold = this.getTreeSpecies().getLeafFallFrostThreshold(); //threshold of daily temperature that trigger leaf fall
				
				int ageTest = this.getLeafAgeCohort(this.getTreeSpecies().getNbCohortMax()-1);
				if(ageTest>budBurstToLeafFallDuration){

					this.setPhenologicalStage(3); 	// beginning of leaf fall
					setLeafFallStartingDate (julianDay);
					if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************LeafFallStartingDate="+julianDay+" budBurstToLeafFallDuration="+budBurstToLeafFallDuration);
				} else {
					if(dayClimat.getMinTemperature() < leafFallFrostThreshold){
						this.setPhenologicalStage(3); 	// beginning of leaf fall
						setLeafFallStartingDate (julianDay);
						if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************LeafFallStartingDate="+julianDay+" budBurstToLeafFallDuration"+budBurstToLeafFallDuration);
					}						
				}
				addLeafAge(0);
			break;
			case 3 :	// leaf fall began
				int leafFallDuration = this.getTreeSpecies ().getLeafFallDuration ();
				if((julianDay>=365)||(julianDay>=(this.getLeafFallStartingDate()+leafFallDuration))){
					this.setPhenologicalStage(4); 	// end of leaf fall
					setLeafFallEndingDate (julianDay);
					if (debug) System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************LeafFallEndingDate="+julianDay+" leafFallDuration="+leafFallDuration);
				}
				addLeafAge(0);
			break;
			case 4 :	// leaf fall finished
				this.setPhenologicalStage(0); 	
				setBudburstAccumulatedTemperatureStarted(false);
				setBudburstAccumulatedTemperature(0);
				addLeafAge(0);
				//cold deciduous = shade accumulation is reset 
				if (this.getTreeSpecies().getPhenologyType()==1) setNbrDaysInShade(0);

			break;
		}

		//on ajoute 1 à tous les ages des feuilles		
		if (this.getTreeSpecies().getNbCohortMax()>1) {
			for (int index = 1; index < this.getTreeSpecies().getNbCohortMax(); index++) {
				if (this.getLeafAreaCohort(index)>0) addLeafAge(index);
			}
		}

	}

	

	
	/**
	 * FRUIT Phenology module
	 * @author Nicolas BARBAULT 10/03/2021 (ADD evergreen trees) 
	 */
	private void fruitPhenology (SafeMacroClimat climat, SafeDailyClimat dayClimat) {

		int julianDay = dayClimat.getJulianDay ();
		float meanTemperature = dayClimat.getMeanTemperature();

		int dayStart = this.getTreeSpecies ().getFloweringTempAccumulationDateStart ();	// date to start flowering temp cumulation (julian day)
		double floweringTempThreshold = this.getTreeSpecies ().getFloweringTempThreshold ();
		
		if (climat.isLeapYear(dayClimat.getYear())) {
			if (julianDay>366) julianDay = julianDay - 366;
		}
		else {
			if (julianDay>365) julianDay = julianDay - 365;
		}
		
		
		if (julianDay == dayStart) {
			setHeatAccumulatedTemperatureStarted(true);
			setHeatAccumulatedTemperature(0);
			setFloweringDate (0);
			setFruitSettingDate (0);
			setFruitGrowthDate (0);
			setVeraisonDate (0);
			setFloweringDuration (0);
		}
		
		//1er avril on calcul l'indice de stress carboné pour la floraison 
		int fruitCarbonStressDateStart = this.getTreeSpecies ().getFruitCarbonStressDateStart ();
		if (julianDay == fruitCarbonStressDateStart){
			setFruitCarbonStressIndex (this.getCarbonLabile() 
									/ (this.getCarbonBranches()+ this.getCarbonStem() + this.getCarbonStump() + this.getCarbonCoarseRoots()));
		}

	
		if (getHeatAccumulatedTemperatureStarted()) {	
			
			if (meanTemperature > floweringTempThreshold ) heatAccumulatedTemperature+= (meanTemperature-floweringTempThreshold); 
		}


		switch(this.getFruitPhenologicalStage()){

			case 1 : // budburst started

				if (heatAccumulatedTemperature > this.getTreeSpecies ().getFloweringTriggerTemp()) {
					this.setFruitPhenologicalStage(2);
					setFloweringDate (julianDay);
					
					//nbr flower potential calculation
					if (this.getAge() > this.getTreeSpecies().getFruitFirstYear()) {					
						flowerPotentialCalculation();
					}
					else {
						setFlowerNbrPotential(0);
					}
					
					//calculation of flowering duration in anticipation 
					floweringDurationAnticipation (climat, dayClimat, heatAccumulatedTemperature, floweringTempThreshold);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************FloweringDate="+julianDay+" temp="+heatAccumulatedTemperature);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************FloweringDuration="+getFloweringDuration());
				}
				
			break;
			case 2 : // flowering
			
				if (heatAccumulatedTemperature > this.getTreeSpecies ().getFruitSettingTriggerTemp()) {
					this.setFruitPhenologicalStage(3);
					setFruitSettingDate (julianDay);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************FruitSetting="+julianDay+" temp="+heatAccumulatedTemperature);
				}
					
			break;
			case 3 :	// fruit setting

				if (heatAccumulatedTemperature > this.getTreeSpecies ().getFruitGrowthTriggerTemp()) {
					this.setFruitPhenologicalStage(4);
					setFruitGrowthDate (julianDay);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************FruitGrowthDate="+julianDay+" temp="+heatAccumulatedTemperature);
				}
			break;
			case 4 :	// fruit growth

				if (heatAccumulatedTemperature > this.getTreeSpecies ().getFruitVeraisonTriggerTemp()) {
					this.setFruitPhenologicalStage(5);
					setVeraisonDate (julianDay);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************VeraisonDate="+julianDay+" temp="+heatAccumulatedTemperature);
				}
			break;
			
			//case 5 :	// harvest
			//tree fruit harvest intervention 
			
			case 6 :	// reset

				this.setFruitPhenologicalStage(0);
				setHeatAccumulatedTemperatureStarted(false);			
			break;

		}

	}
	/**
	 * Flower potential  calculation 
	 * @author Nicolas BARBAULT 15/06/2021 
	 */
	private void flowerPotentialCalculation () {
		

		//calcul de la pente entre le point A et B pour la diminution du nombre de fleurs 
		double ya = this.getTreeSpecies().getFruitingConfortThreshold();
		double yb = (this.getTreeSpecies().getFruitingConfortThreshold() - 
					(this.getTreeSpecies().getFruitingConfortThreshold() * this.getTreeSpecies().getFruitingTotalStressThreshold())) / 2.0 
				  + (this.getTreeSpecies().getFruitingConfortThreshold() * this.getTreeSpecies().getFruitingTotalStressThreshold());
		//seuil de stress total 
		double xa = 100; 
		double xb = this.getTreeSpecies().getFruitingTotalStressThreshold() * 100;
		double penteM = (yb - ya) / (xb - xa);
		
		setFlowerNbrPotentialFromLeafArea((int)(this.getTreeSpecies().getFruitLeafArea() * this.getTotalLeafArea()));

		if (getFruitCarbonStressIndex() >= this.getTreeSpecies().getFruitingConfortThreshold() ) {
			setFlowerNbrPotentialReducer(100);
		}
		else {
			if (getFruitCarbonStressIndex() <  ( this.getTreeSpecies().getFruitingConfortThreshold() * this.getTreeSpecies().getFruitingTotalStressThreshold())) {
				setFlowerNbrPotentialReducer(0);
			}
			else {
				
				setFlowerNbrPotentialReducer(
						 (getFruitCarbonStressIndex() - 
						( this.getTreeSpecies().getFruitingConfortThreshold() * this.getTreeSpecies().getFruitingTotalStressThreshold())) 					
						/ penteM);
			}
		}

		setFlowerNbrPotential((int)(getFlowerNbrPotentialFromLeafArea()*getFlowerNbrPotentialReducer())/100);
		
	}
	/**
	 * Flowering duration anticipation
	 * @author Nicolas BARBAULT 10/03/2021 
	 */
	private void floweringDurationAnticipation (SafeMacroClimat climat, SafeDailyClimat dayClimat, double heatTemp, double floweringTempThreshold) {
		
		
		// Get Daily climat 
		int climatDay = dayClimat.getJulianDay ();
		int climatYear = dayClimat.getYear();

		// leap year
		int nbDayMax = 365;
		if (climat.isLeapYear(climatYear)) {
			nbDayMax = 366;
		}
		
		while (heatTemp < this.getTreeSpecies ().getFruitSettingTriggerTemp()) {
			climatDay++; 
			if (climatDay > nbDayMax) {
				climatDay = climatDay - nbDayMax;
				climatYear = climatYear + 1;
			}

			SafeDailyClimat nextClimat = climat.getDailyWeather(climatYear, climatDay);
			float meanTemperature = nextClimat.getMeanTemperature();
			if (meanTemperature > floweringTempThreshold ) heatTemp += (meanTemperature-floweringTempThreshold);
			addFloweringDuration (1);
		}

	}
	/**
	 * Phenology module for biological nitrogen fixation (BNF) 
	 * @author Tristan GERAULT (22/04/2021)
	 */
	private void bnfPhenology (SafeMacroClimat climat, SafeDailyClimat dayClimat) {

		int julianDay = dayClimat.getJulianDay ();
		int dayStart = this.getTreeSpecies ().getBnfTempAccumulationDateStart ();	// date to start temperature cumul (julian day)
		double bnfTempThreshold = this.getTreeSpecies ().getBnfTempThreshold ();

		float meanTemperature = dayClimat.getMeanTemperature ();

		switch(this.getBnfPhenologicalStage()){
			case 0 : // before start
				
				int dayTest = julianDay;
				if (climat.isLeapYear(dayClimat.getYear())) {
					if (julianDay>366) dayTest = julianDay - 366;
				}
				else {
					if (julianDay>365) dayTest = julianDay - 365;
				}

				if (dayTest == dayStart) {
					setBnfAccumulatedTemperatureStarted(true);
					setBnfAccumulatedTemperature(0);
					setBnfStartDate (0);
				}
				
				if (getBnfAccumulatedTemperatureStarted()) {

					this.bnfAccumulatedTemperature += Math.max(meanTemperature-bnfTempThreshold,0);
					
					if (bnfAccumulatedTemperature > this.getTreeSpecies ().getBnfStartTriggerTemp()) {
						this.setBnfPhenologicalStage(1); 	//start
						setBnfStartDate (julianDay);
						//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************BnfDate="+julianDay);
					}
				}
					
					
			break;
			case 1 : // bnf is started

				int bnfExpansionDuration = this.getTreeSpecies ().getBnfExpansionDuration();		// date for the end of BNF expansion (julian day)

	
				if(julianDay-this.getBnfStartDate()>bnfExpansionDuration){
					this.setBnfPhenologicalStage(2); 	// end of leaf expansion
					setBnfSteadyStateDate (julianDay);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************BnfSteadyStateDate="+julianDay);
	
				}
				
				
			break;
			case 2 : // bnf is steady state 
				
				
				int bnfStartToEndDuration = this.getTreeSpecies().getBnfStartToEndDuration();		// date for the end of leaf expansion (julian day)
			
				
				if(julianDay-this.getBnfStartDate()>bnfStartToEndDuration){

					this.setBnfPhenologicalStage(3); 	
					setBnfEndingDate (julianDay);
					//System.out.println(dayClimat.getDay()+"/"+ dayClimat.getMonth()+"*************BnfEndingDate="+julianDay);
				}
				

					
			break;

			case 3 :	// bnf finished
				this.setBnfPhenologicalStage(0); 	
				setBnfAccumulatedTemperatureStarted(false);
				setBnfAccumulatedTemperature(0);
			break;
		}

	}
	
	/**
	 * Tree potential transpiration module
	 * @author Gregoire Talbot 2010
	 * according to Pereira 2006, waterDemand = ETP*leafArea/2.88 for a 30m2 leaf area walnut
	 * to account for size, shade and shelter effects, we replaced leafArea by param*CaptureFactorForDiffusePar (m2)
	 */
	 private double computeWaterDemand (SafeDailyClimat dayClimat) {

		double transpirationCoefficient = this.getTreeSpecies().getTranspirationCoefficient();
		// GT 23 sept 2010 tentative
		// 2.86 permet d'avoir une transpiration coherente avec Pereira 2006 
		// 1/0.35, ou 0.35 est la valeur du captureFactor/leafArea pour un arbres de 30m2 de surface foliaire.
		double transpirationDemand = dayClimat.getEtpPenman()
									*(this.getGlobalRadIntercepted()/dayClimat.getGlobalRadiation())
									* (this.getCo2LueEffect()/this.getCo2WueEffect()) //CO2 effect
									*transpirationCoefficient;		

		
		return Math.max(transpirationDemand,0);
	}
	 
	/**
	 * Tree leaves evaporation
	 * If leaves are falling, the tree transpiration is null so we have to evaporated ALL rain stored on leaves
	 */
	 private double computeEvaporation (double transpirationDemand) {

		double evaporation = 0;
		if (this.getStoredRain() > 0) {
			
			//cold deciduous we check phenology
			if ((this.getTreeSpecies ().getPhenologyType() == 1) && (this.getPhenologicalStage() == 3)) {
				evaporation = this.getStoredRain ();
			}
			//evergreen there is always transpirationDemand
			//cold deciduous there is  transpirationDemand only if PhenologicalStage != 3 
			else {
				evaporation = Math.min(this.getStoredRain(), transpirationDemand);
			}
			
			
			setStoredRain (this.getStoredRain () - evaporation);
			setEvaporatedRain (evaporation);
		}
		
		return (evaporation);
	}	 
	 
	 /**
	 * Nitrogen demand calculation
	 */
	public double computeNitrogenDemand () {

		// luxuryNCoefficient stands for the proportion of N(relative to optimum level)
		// which may be accumulated as NSN before N absorption ceases completely
		double luxuryNCoefficient = this.getTreeSpecies ().getLuxuryNCoefficient();

		//target nitrogen level is set above optimum
		// due to large intra-annual fluctuation in labile Nitrogen
		double targetNCoefficient = this.getTreeSpecies ().getTargetNCoefficient();

		//Optimum level of (structural) N for whole plant
		double totalOptiN = getTotalOptiN();
		double totalN = getTotalN();
		double nitrogenDemand = 0;

		if (totalN > totalOptiN * targetNCoefficient) {

			setNitrogenSaturation (Math.min ((totalN-(totalOptiN * targetNCoefficient))
											/(totalOptiN * targetNCoefficient * (luxuryNCoefficient-1))
								  , 1));
			
			setNitrogenSatisfaction(1);

			//Minimal demand below Nmax = totalOptiN * targetNCoefficient * luxuryNCoefficient
			//GV+IL 6/4/06
			if (getNitrogenSaturation () < 1)
				nitrogenDemand  = (((totalOptiN * targetNCoefficient) - totalOptiN) * 0.01);

		}
		else {

			//IL 19/06/2018 (if set to 0, nitrogenMark can be 0 and finerootIncrement set to 0 also (see fineRootGrowth code) 
			//setNitrogenSaturation (0);
			setNitrogenSaturation (0.0001d);
			
			setNitrogenSatisfaction(Math.min ((totalN/(totalOptiN * targetNCoefficient)), 1));
			

			//to ensure monotonic decrease of demand with increasing Ncontent
			//GV+IL 6/4/06
			nitrogenDemand =  (
				   (((totalOptiN * targetNCoefficient) - totalN) * 0.99)
				 + (((totalOptiN * targetNCoefficient) - totalOptiN) * 0.01));

		}
		
		return nitrogenDemand;
	}


	/*
	 * *BNF VERSION 1
	 * 	Calculation of environmental control for BNF
	 * Anoxie / Water strees / N Saturation / temperature /nodule inhibition 
	*/
	public void computeNitrogenFixationLimitationsV1 (SafeStand stand) {

		double bnfMaxDepth = this.getTreeSpecies ().getBnfMaxDepth () * 100;		// maximum depth for nitrogen fixation 

		double bnfNodulationInhibitionThreshold = this.getTreeSpecies ().getBnfNodulationInhibitionThreshold();		

		double bnfCardinalTemp1 = this.getTreeSpecies ().getBnfCardinalTemp1 ();		// Nodule activity cardinal temperature 
		double bnfCardinalTemp2 = this.getTreeSpecies ().getBnfCardinalTemp2 ();		
		double bnfCardinalTemp3 = this.getTreeSpecies ().getBnfCardinalTemp3 ();		 
		double bnfCardinalTemp4 = this.getTreeSpecies ().getBnfCardinalTemp4 ();		

		double bnfFullNoduleActivityThreshold = this.getTreeSpecies ().getBnfFullNoduleActivityThreshold ();
		double bnfNullNoduleActivityThreshold = this.getTreeSpecies ().getBnfNullNoduleActivityThreshold ();

		double sumHUR = 0;
		double sumNit = 0; 

		int rootedMiniCoucheNumber = 0;
		double sumWater = 0; 
		double sumTemp = 0; 
		double sumAnox = 0;
		
		SafePlot plot = (SafePlot) stand.getPlot();

		for (Iterator c = plot.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeCrop crop = cell.getCrop();
			SafeVoxel voxels [] = cell.getVoxels();


			//FOR EACH VOXEL
			for (int i = 0; i < voxels.length; i++) {
				if (voxels[i].getTheTreeRootsDensity(this.getId()-1) >0) {

					int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
					int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel
					int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	   //number    miniCouches  for current voxel

					for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
						if (z<=bnfMaxDepth) {

							sumHUR 	+= crop.sticsCommun.HUR[z];			    // voxel soil humidity 	%	
							sumNit 	+= crop.sticsSoil.nit[z+1];				// voxel soil no3 kg N ha-1		

							//stress by anoxia
							sumAnox += crop.sticsCommun.anox[z];

							//water stress
							if (crop.sticsCommun.HUR[z] > crop.sticsCommun.HUMIN[z]) sumWater ++;

							//temperature stress
							if (crop.sticsCommun.tsol[z] <= bnfCardinalTemp1) 
								sumTemp += 0.0;

							if (crop.sticsCommun.tsol[z] > bnfCardinalTemp1 && crop.sticsCommun.tsol[z] <= bnfCardinalTemp2) 
								sumTemp += (crop.sticsCommun.tsol[z]-bnfCardinalTemp1) / (bnfCardinalTemp2-bnfCardinalTemp1);

							if (crop.sticsCommun.tsol[z] > bnfCardinalTemp2 && crop.sticsCommun.tsol[z] <= bnfCardinalTemp3) 
								sumTemp += 1.0;

							if (crop.sticsCommun.tsol[z] > bnfCardinalTemp3 && crop.sticsCommun.tsol[z] <= bnfCardinalTemp4) 
								sumTemp += (crop.sticsCommun.tsol[z]-bnfCardinalTemp4) / (bnfCardinalTemp3-bnfCardinalTemp4);

							if (crop.sticsCommun.tsol[z] > bnfCardinalTemp4) 
								sumTemp += 0.0;


							rootedMiniCoucheNumber++;

						}	
					}
				}
			}
		}
		

		if (rootedMiniCoucheNumber>0) {
			setBnfWaterStress(sumWater / rootedMiniCoucheNumber);
			setBnfAnoxStress(1 - (sumAnox / rootedMiniCoucheNumber));
			setBnfTemperatureStress (sumTemp / rootedMiniCoucheNumber);
		}
		
		if ((sumNit / sumHUR) < bnfNodulationInhibitionThreshold) 
			setBnfNodulationInhibition(1);
		else
			setBnfNodulationInhibition(0);

		//nitrogenSaturation stress  
		if ((sumNit / sumHUR) < bnfFullNoduleActivityThreshold) 
			setBnfSoilNitrateExcess(1);
		else
			setBnfSoilNitrateExcess(1.0 / (bnfNullNoduleActivityThreshold - bnfFullNoduleActivityThreshold) 
									* (bnfNullNoduleActivityThreshold - (sumNit / sumHUR)));

		setBnfSoilNitrateExcess(Math.max(getBnfSoilNitrateExcess(), 0));
	}	

	
	private void computeNitrogenFixationV1 (SafeDailyClimat climat) {

		double tmeanAir = climat.getMeanTemperature();
		double bnfAirTemperatureThreshold = this.getTreeSpecies().getBnfAirTemperatureThreshold(); 		//TCMIN stics : air temperature threshold for BNF potential activity to be increased by air temp
		double bnfOptimalTemperatureDifference = this.getTreeSpecies().getBnfOptimalTemperatureDifference();  //degrees 
		double bnfFixMaxVeg = this.getTreeSpecies().getBnfFixMaxVeg();						
		double bnfFixMaxRepro = this.getTreeSpecies().getBnfFixMaxRepro();					
		double bnfExpansionDuration = this.getTreeSpecies().getBnfExpansionDuration();
		
	
		//phase d'expension
		if (this.getBnfPhenologicalStage() == 1 || this.getBnfPhenologicalStage() == 2) {
		
			setBnfPhenologyCoefficient (Math.min(getBnfPhenologyCoefficient() + (1.0/bnfExpansionDuration 
										* ((tmeanAir - bnfAirTemperatureThreshold)/bnfOptimalTemperatureDifference) * getBnfNodulationInhibition()),1));

			setBnfMaxNitrogenFixationPotential ((bnfFixMaxVeg * (getTotalCarbonIncrement() - getCarbonFruitIncrement())) 
										+ (bnfFixMaxRepro * getCarbonFruitIncrement())); 
			
		}
		
		setBnfNitrogenFixationPotential (getBnfPhenologyCoefficient() * getBnfMaxNitrogenFixationPotential());
		
		setBnfNitrogenFixation (getBnfNitrogenFixationPotential() 
				* getBnfAnoxStress() 
				* Math.min(getBnfWaterStress(), Math.min(getBnfSoilNitrateExcess(), 1 - getNitrogenSaturation()) ) 
				* getBnfTemperatureStress());

		
	}

	/**
	 * C allocation module
	 * @author Gregoire VINCENT (IRD) - August 2003
	 */
	private void computeCarbonAllocation (SafeStand stand,
									SafeGeneralParameters safeSettings,
									SafePlotSettings plotSettings,
									int simulationDay,
									double carbonIncrement,
									double nitrogenUptake) {



		
		double carbonAllocated = carbonIncrement; 

		
		double woodCarbonContent = this.getTreeSpecies ().getWoodCarbonContent ();
		
		//Tree specific parameters for allometry
		SafeTreeSpecies treeSpecies = this.getTreeSpecies ();
		double	aTree			= treeSpecies.getHeightDbhAllometricCoeffA ();
		double	bTree			= treeSpecies.getHeightDbhAllometricCoeffB ();
		double	aCrown			= treeSpecies.getCrownDbhAllometricCoeffA ();
		double	bCrown			= treeSpecies.getCrownDbhAllometricCoeffB ();
		double	aStemVolume		= treeSpecies.getStemDbhAllometricCoeffA ();
		double	bStemVolume		= treeSpecies.getStemDbhAllometricCoeffB ();
		double	cStemVolume		= treeSpecies.getStemDbhAllometricCoeffC ();
		double  dcbFromDbh		= treeSpecies.getDcbFromDbhAllometricCoeff ();
		
		double	stumpToStemBiomassRatio = treeSpecies.getStumpToStemBiomassRatio ();
		double	branchVolumeRatio	= treeSpecies.getBranchVolumeRatio ();
		double	woodDensity			= treeSpecies.getWoodDensity ();
		int		crownShape 	 		= treeSpecies.getCrownShape ();
		double	aLeafArea		 	= treeSpecies.getLeafAreaCrownVolCoefA();
		double	bLeafArea			= treeSpecies.getLeafAreaCrownVolCoefB();
		double	leafMassArea		= treeSpecies.getLeafMassArea ();
		double	leafCarbonContent	= treeSpecies.getLeafCarbonContent();
		double  maxCrownRadiusInc   = treeSpecies.getMaxCrownRadiusInc();
		double  maxHeightInc 	    = treeSpecies.getMaxHeightInc();

		
		//Tree specific parameters for Non-structural carbon
		double targetNSCFraction		= treeSpecies.getTargetNSCFraction ();
		double maxNSCUseFraction		= treeSpecies.getMaxNSCUseFraction ();
		double maxNSCUseFoliageFraction = treeSpecies.getMaxNSCUseFoliageFraction();

		//arbitrary level of imbalance above which remobilisation of reserves is triggered
		double imbalanceThreshold 	= treeSpecies.getImbalanceThreshold ();

		//add parameters for fruits compartment
		
		/*
		* Calculation of above ground imbalance
		* Deduction of carbon exchange with NSC and total carbon allocation to growth
		*/
		NSCExchange = 0;
		aboveGroundImbalance = 0;
		
		double starCarbonFoliage = aLeafArea*Math.pow(getCrownVolume(),bLeafArea)*leafCarbonContent*leafMassArea;	// gt - 26/10/2010 : targetCarbonFoliage
		aboveGroundImbalance = Math.max (1 - getTotalCarbonFoliage ()/ starCarbonFoliage, 0);	// gt - 27.01.2009

		/*
		double respi = 0;//0.0002*(getCarbonBranches()+getCarbonCoarseRoots()+getCarbonStem()+getCarbonFineRoots()+getCarbonFoliage());
		//double respi=0.00357*getCarbonFoliage()+0.000854*getCarbonFineRoots()+0.00014*(getCarbonBranches()+getCarbonCoarseRoots()+getCarbonFineRoots());
										
		if(this.getPhenologicalStage() == 1){
			if(getCarbonLabile()>=respi){
				setCarbonLabile (getCarbonLabile () - respi);
			} else {
				carbonAllocated -=(respi-getCarbonLabile ());
				setCarbonLabile(0);
			}
		} else {
			if(carbonAllocated>=respi){
				carbonAllocated -=respi;
			} else {
				setCarbonLabile (Math.max(0,getCarbonLabile () - (respi-carbonAllocated)));
				carbonAllocated = 0;
			}
		}
			carbonAllocated=Math.max(carbonAllocated,0);
			*/	
	
	

		
		if ((aboveGroundImbalance > imbalanceThreshold) && (this.getPhenologicalStage() ==1)){
			// gt - 26/10/2010 : getTotalStructuralCarbon replaced by starCarbonFoliage to avoid a too fast budburst
			//NSCExchange = Math.min (maxNSCFraction * getCarbonLabile(), getTotalStructuralCarbon () * maxDailyNSC);
			NSCExchange = Math.min (getCarbonLabile() * maxNSCUseFraction, starCarbonFoliage * maxNSCUseFoliageFraction);	
		}

		setCarbonLabileIncrement(getCarbonLabile ());
		double NSCDesat = (getCarbonLabile()-NSCExchange)/ ((getCarbonBranches() + getCarbonStem()+ getCarbonStump()+ getCarbonCoarseRoots())*targetNSCFraction);
		double nsSink ;
		if(NSCDesat>=2){
			nsSink=0;
		} else {			
			nsSink = Math.exp((1/NSCDesat-1/(2-NSCDesat)));
		}

		carbonSavingFraction = nsSink/(nsSink+1);
		

		carbonAllocated += NSCExchange;

		setCarbonLabile (getCarbonLabile () - NSCExchange+carbonAllocated*carbonSavingFraction);

		setCarbonLabileIncrement(getCarbonLabile ()-getCarbonLabileIncrement());

		carbonAllocated*=(1-carbonSavingFraction);

		setCarbonAllocToGrowth (carbonAllocated);

		
		/*
		* Effect of stress and  on targetLfrRatio (ratio between Leaf and Fine root carbon repartion) 
		* 
		A positive daily drift is added when there is no water and nitrogen stress (the 0.98 threshold for the belowground stress applies)
	    The negative impact of Water and Nitrogen stress
	    The positive impact of Nitrogen Excess is added
	    The positive impact of Light Deficit is added
		*/

		rsBelowGroundStressEffect = 0;
		rsNitrogenExcessEffect = 0;
		rsLightStressEffect = 0;
		targetLfrDrift = 0;

		if ((((this.getTreeSpecies ().getPhenologyType() == 1) && ((this.getPhenologicalStage() == 1)||(this.getPhenologicalStage() == 2))))
		|| (this.getTreeSpecies ().getPhenologyType() == 2))
		{ 

			//Belowground stress effect 
			//effect of water and nitrogen
			double rsWNDeficitStress = 1;
			double rsWaterDeficitStress    = Math.pow (getWaterStress(),treeSpecies.getRsWaterStressResponsiveness ());
			double rsNitrogenDeficitStress = Math.pow (getNitrogenSatisfaction(),treeSpecies.getRsNitrogenStressResponsiveness ());
			if (treeSpecies.getRsBelowGroundStressMethod () == 1)        
				rsWNDeficitStress = rsWaterDeficitStress * rsNitrogenDeficitStress;
			else 
				rsWNDeficitStress = Math.min(rsWaterDeficitStress, rsNitrogenDeficitStress); 
			
			if (rsWNDeficitStress > 0.98) {
				

				targetLfrDrift = treeSpecies.getTargetLfrRatioUpperDrift () 
								* treeSpecies.getMaxTargetLfrRatioDailyVariation ();
				
				rsLightStressEffect += (1-getLightCompetitionIndex()) 
										* treeSpecies.getMaxTargetLfrRatioDailyVariation ();
			
				if ( getNitrogenSaturation() <= 0.0001d) {
					rsNitrogenExcessEffect = 0;
				}
				else  {
					rsNitrogenExcessEffect =  Math.min( getNitrogenSaturation() 
														* treeSpecies.getMaxTargetLfrRatioDailyVariation ()
														/ 0.2d
											,treeSpecies.getMaxTargetLfrRatioDailyVariation ());
				}
			}
			else {
			
				rsBelowGroundStressEffect = -(1-rsWNDeficitStress) 
											* treeSpecies.getMaxTargetLfrRatioDailyVariation ();	
			}

			double targetVariation = targetLfrDrift;
					
			if (treeSpecies.getRsBelowGroundStressActivation()) 
				targetVariation += rsBelowGroundStressEffect;
			if (treeSpecies.getRsNitrogenExcessStressActivation()) 
				targetVariation += rsNitrogenExcessEffect;
			if (treeSpecies.getRsLightStressActivation()) 
				targetVariation += rsLightStressEffect;
			
			//drift CANNOT be > MaxTargetLfrRatioDailyVariation
			targetVariation =  Math.min( targetVariation,
										treeSpecies.getMaxTargetLfrRatioDailyVariation ());
	

			targetLfrRatio += targetVariation;


		}
		
		setTargetLfrRatio (Math.max(treeSpecies.getMinTargetLfrRatio (), Math.min(targetLfrRatio, treeSpecies.getMaxTargetLfrRatio ())));



		
		/*
		* Calculation of aboveground allocation fraction :
		* 	- The allocation ratio is calculated to reach a target value for the
		*		ratio leaf/fineRoots  (targetLfrRatio) during leaf expansion
		*	-  When leaf expansion if finished, aboveGroundAllocFraction is aboveGroundCfraction
		*/
		this.aboveGroundAllocFrac = 0;
		this.belowGroundAllocFrac = 0;

		if (carbonAllocated > 0){
				double rStar = this.getTargetLfrRatio();
				double epsiA = this.getCarbonAboveGroundEff();
				double epsiB = getCarbonBelowGroundEff();
				double cFr = getCarbonFineRoots();
				double cFl = getTotalCarbonFoliage();	

				if(this.getPhenologicalStage() != 1){
					//11/02/2021 add fruit carbon pool (IL + NB) 
					epsiA = cFl/(cFl+getCarbonStem()+getCarbonBranches()+getCarbonFruit());
				}
			
				aboveGroundAllocFrac = (rStar*(cFr+cFl+epsiB*carbonAllocated)- cFl) / (carbonAllocated*(epsiA+rStar*(epsiB-epsiA)));
				aboveGroundAllocFrac = Math.max(0,Math.min(1,aboveGroundAllocFrac));
				belowGroundAllocFrac = 1 - aboveGroundAllocFrac;
		}
		/*
		* Computation of sink forces of fine roots and coarse roots using the explicit
		* topological rooting system
		*	- a priori allocation of all carbon to fine roots
		*	- calculation of total coarse root  needed to support fine root system
		*	- sink forces computation
		*/

		this.belowGroundGrowth = carbonAllocated * belowGroundAllocFrac;			
		this.fineRootsAllocFrac=0;
		this.coarseRootsAllocFrac=0;
		this.carbonFineRootsSink = 0;
		this.carbonCoarseRootsSink=0;
		this.belowGroundSink=0;
		double fineRootsGrowth=0;
		double coarseRootsGrowth=0;


		if (this.belowGroundGrowth > 0){

			
			carbonFineRootsSink = this.belowGroundGrowth;
			
			// cellular automata is called just for FR/CR allocation computation
			// the method computing the needed coarse root  is called within
			// the cellular automata
			fineRootGrowth (safeSettings, plotSettings, simulationDay,carbonFineRootsSink,false);

			// 2) sink force computation
			carbonCoarseRootsSink = Math.max(getCarbonCoarseRootsTarget()-getCarbonCoarseRoots(),0);

			
			if((carbonCoarseRootsSink+carbonFineRootsSink)>this.belowGroundGrowth){
				belowGroundSink = carbonCoarseRootsSink + carbonFineRootsSink;
				fineRootsAllocFrac = carbonFineRootsSink/belowGroundSink;
				fineRootsGrowth = this.belowGroundGrowth * fineRootsAllocFrac;
				setCarbonFineRootsIncrement (fineRootsGrowth);	
				coarseRootsAllocFrac = 1-fineRootsAllocFrac;
				coarseRootsGrowth =  this.belowGroundGrowth * coarseRootsAllocFrac;
				setCarbonCoarseRootsIncrement (coarseRootsGrowth);
			} else {
				fineRootsGrowth=carbonFineRootsSink;
				fineRootsAllocFrac=fineRootsGrowth/this.belowGroundGrowth;				
				setCarbonFineRootsIncrement (fineRootsGrowth);
				coarseRootsGrowth = this.belowGroundGrowth-fineRootsGrowth;
				setCarbonCoarseRootsIncrement (coarseRootsGrowth);
			}

			addCarbonFineRoots (fineRootsGrowth);
			addCarbonCoarseRoots (coarseRootsGrowth);

			
		} else {
			setCarbonFineRootsIncrement(0);
			setCarbonCoarseRootsIncrement(0);
		}


		/*
		* Computation of sink forces for aboveground carbon pools
		* 	- a priori allocation proportionnal to actuel pools
		*	- sink forces calculation according to allometries and phenology
		*/
				
		this.stemAllocFrac = 0;
		this.branchAllocFrac = 0;
		this.foliageAllocFrac = 0;
		this.stumpAllocFrac = 0;

		double stemGrowth = 0;
		double stumpGrowth = 0;
		double branchGrowth = 0;
		double foliageGrowth = 0;
		double fruitGrowth = 0;

		this.aboveGroundGrowth = carbonAllocated * aboveGroundAllocFrac;
	
		if (this.aboveGroundGrowth == 0) {
			this.aboveGroundSink = 0;
			this.carbonStemSink = 0;
			this.carbonBranchesSink = 0;
			this.carbonFoliageSink = 0;
			this.carbonStumpSink = 0;
		}
		if (this.aboveGroundGrowth > 0) {

			
			//ajouté pour ne pas ecraser les valeurs de la veille
			double newAboveGroundSink = 0;
			double newCarbonStemSink = 0;
			double newCarbonBranchesSink = 0;
			double newCarbonFoliageSink = 0;
			double newCarbonStumpSink = 0;
			
			//01/06/2021 add fruit carbon pool (IL + NB)
			//Fruit allocation has priority 
			if (this.getTreeSpecies().getFruitCompartment()) {
				

				//before flowering
				if (this.getFruitPhenologicalStage() < 2) {
					setFlowerNbrPotentialDaily(0); 
					setFlowerNbr(0);
					setFruitNbr(0);
				}
				//flowering affected by heat and frost stress
				else if (this.getFruitPhenologicalStage() == 2) {
					if (getFloweringDuration() >0) setFlowerNbrPotentialDaily(getFlowerNbrPotential() / getFloweringDuration());
					else setFlowerNbrPotentialDaily(0);
					setFlowerNbr( (int) (getFlowerNbrPotentialDaily() * getFloweringHeatStress() * getFloweringFrostStress()));
					setFruitNbr( getFruitNbr() + getFlowerNbr());
				}
				//other stages
				else {
					setFlowerNbrPotentialDaily(0); 
					setFlowerNbr(0);
				}
				
				fruitGrowth = Math.min(getFruitCarbonTarget(), getFruitDemandTarget());
				setCarbonFruit(getCarbonFruit() + fruitGrowth);

			}
			else {
				fruitGrowth = 0;
			}
			
			setCarbonFruitIncrement(fruitGrowth); 
			
			double restAboveGroundGrowth = this.aboveGroundGrowth - fruitGrowth;

			//Tree growth
			// Compute approximate tree dimensions following incremental growth
			// those would-be compartment sizes are used to determine the sink forces
			// of the various compartments
			targetCarbonStem = getCarbonStem()+restAboveGroundGrowth;
			targetStemVolume = targetCarbonStem /(woodCarbonContent*woodDensity);
			
			
			//1/ We assume the following relationship is always correct
			//   stemVolume=exp(aStemVolume) * dbh^bStemVolume * height^cStemVolume
			//2/ As long as the tree is shorter than expected only growth in height is allowed
			//3/ If tree is balanced then both DBH and Height need to be updated
			//4/ When foliage growth has ceased only dbh may increase
			//5/ Sink forces are further constrained through limiting radial crown expansion to a species specific (?)
			//   currently hardcoded value, set to 0.01 (m d-1) corresponding to the maximum daily increment;

			// gt 17.02.2009
			
			//BRIDAGE DU TARGET AVEC maxHeightInc ???
			//Ou bien est ce que l'on ne doit brider que la hauteur FINALE ???
			//Est ce que le carbon economisÃ© par le bridage est bien redistribuÃ© dans les autres POOLS ???
			
			if(getIsShort() && (this.getPhenologicalStage() == 1)){		// if tree is short, additional stem sink to grow in height
				targetHeight = Math.max(computeNewHeight (safeSettings, targetStemVolume, getDbhMeters()), aTree * Math.pow (getDbhMeters(),bTree));// gt - 26/10/2010
				targetHeight = Math.min((this.getHeight()+maxHeightInc),targetHeight);
				targetDbh = Math.max(getDbhMeters(),computeNewDbh(targetStemVolume,targetHeight));// gt - 26/10/2010
				targetStemVolume = Math.exp(aStemVolume)*Math.pow(targetDbh,bStemVolume)*Math.pow(targetHeight,cStemVolume);
				targetCarbonStem = targetStemVolume*(woodCarbonContent*woodDensity); // gt - 26/10/2010

			} else {
				targetHeight = computeNewHeight (safeSettings, targetStemVolume, getDbhMeters());// gt - 26/10/2010
				targetHeight = Math.min((this.getHeight()+maxHeightInc),targetHeight);
				targetDbh = computeNewDbh (targetStemVolume, getHeight());// gt - 26/10/2010
			}
	

			//We then need to compute associated crown volume to estimate branch and leaves sinks
			targetCrownDepth  = targetHeight - getCrownBaseHeight ();
			targetCrownVolume = this.getCrownVolume();
			
			//crown radial expansion (as well as growth in height is stopped once leaf expansion is over
			if(this.getPhenologicalStage() == 1 ){
				// complete ellipsoid crown shape (after G.Vincent 2003); no truncation
				if (crownShape == 1) {
					//max crown radii length limited by planting pattern and maximum daily increment
					double dcbMeters = targetDbh*Math.min(1,Math.pow(1+(1.3-getCrownBaseHeight())/targetHeight,1/dcbFromDbh));
					double targetCrownArea = aCrown * Math.pow(dcbMeters,bCrown);
					double targetNewRadii [] = new double [2];
					targetNewRadii = computeRadiiDeformation (targetCrownArea, (SafeStand) stand);
					targetNewRadii[0]=Math.max(getCrownRadiusTreeLine(),Math.min(getCrownRadiusTreeLine()+maxCrownRadiusInc,targetNewRadii[0]));
					targetNewRadii[1]=Math.max(getCrownRadiusInterRow(),Math.min(getCrownRadiusInterRow()+maxCrownRadiusInc,targetNewRadii[1]));
					targetCrownVolume = (4d/3d * Math.PI * targetNewRadii[0] * targetNewRadii[1]
											  * (targetCrownDepth/2));
	
				}
				//Paraboloide of revolution crown shape (after B.Courbeaud 2000)
				if (crownShape == 2) {
					double targetCrownParameter = aCrown * Math.pow ((targetDbh / targetHeight), bCrown);
					targetCrownVolume = (1d/2d * Math.PI * targetCrownParameter * Math.pow(targetHeight - getCrownBaseHeight(),2));
				}
			}
			
		
			//Absolute sink of branch assuming allocation to stem of available C is proportional
			// to relative stem size
			double targetBranchVolume =  targetCrownVolume * branchVolumeRatio;
			targetCarbonBranches = (targetBranchVolume * woodDensity * woodCarbonContent);
			newCarbonBranchesSink = Math.max (targetCarbonBranches - getCarbonBranches(),0);
			
			// Expected Leaf C given stem volume and height of crown base (kg)
			targetCarbonFoliage = aLeafArea*Math.pow(targetCrownVolume,bLeafArea) * leafMassArea * leafCarbonContent;
			if (this.getPhenologicalStage() == 1){
				newCarbonFoliageSink = Math.max ((targetCarbonFoliage - getTotalCarbonFoliage()),0);
				if(aboveGroundImbalance!=1) newCarbonFoliageSink/=(1-aboveGroundImbalance);
			}			

			newCarbonStemSink = Math.max (targetCarbonStem - getCarbonStem(),0);
			newCarbonStumpSink = Math.max (targetCarbonStem*stumpToStemBiomassRatio - getCarbonStump(),0);
			newAboveGroundSink = newCarbonBranchesSink + newCarbonFoliageSink + newCarbonStemSink + newCarbonStumpSink;

			if (newAboveGroundSink > 0) {
				
				carbonStemSink = newCarbonStemSink;
				carbonStumpSink = newCarbonStumpSink;
				carbonBranchesSink = newCarbonBranchesSink;
				carbonFoliageSink = newCarbonFoliageSink;
				aboveGroundSink = carbonBranchesSink + carbonFoliageSink + carbonStemSink + carbonStumpSink;
			}

			
			if (aboveGroundSink > 0)  {
				stemAllocFrac = carbonStemSink / aboveGroundSink;
				stumpAllocFrac = carbonStumpSink / aboveGroundSink;
				branchAllocFrac = carbonBranchesSink / aboveGroundSink;
				foliageAllocFrac = carbonFoliageSink / aboveGroundSink;
			}
			
			stemGrowth = restAboveGroundGrowth * stemAllocFrac;
			stumpGrowth = restAboveGroundGrowth * stumpAllocFrac;
			branchGrowth = restAboveGroundGrowth * branchAllocFrac;
			foliageGrowth = restAboveGroundGrowth * foliageAllocFrac;

			
		}
		

		
		setCarbonFoliageIncrement(foliageGrowth);  // kg C
		setCarbonStemIncrement(stemGrowth); 
		setCarbonBranchesIncrement(branchGrowth); 
		setCarbonStumpIncrement(stumpGrowth); 


		// UPDATE Structural C in aboveground compartments (kg C)
		setCarbonBranches (getCarbonBranches () + branchGrowth);
		setCarbonStem (getCarbonStem () + stemGrowth);
		setCarbonStump(getCarbonStump()+stumpGrowth);
		//Only first cohort has carbon Increment
		setCarbonFoliage (getCarbonFoliageCohort (0) + foliageGrowth, 0);
		
		setAboveGroundCFraction ((getCarbonBranches() + getCarbonStem())
								/ (getCarbonBranches() + getCarbonStem() + getCarbonStump() + getCarbonCoarseRoots ()));

		/*
		* Nitrogen allocation
		* computation of N remobilisation from leaf and fine root senescence
		*/
		double optiNCStem 	 		= this.getTreeSpecies().getOptiNCStem ();
		double optiNCStump 	 		= this.getTreeSpecies().getOptiNCStump ();
		double optiNCFoliage 		= this.getTreeSpecies().getOptiNCFoliage ();
		double optiNCBranch 		= this.getTreeSpecies().getOptiNCBranch ();
		double optiNCFruit 			= this.getTreeSpecies().getOptiNCFruit ();
		double optiNCCoarseRoot 	= this.getTreeSpecies().getOptiNCCoarseRoot ();
		double optiNCFineRoot 		= this.getTreeSpecies().getOptiNCFineRoot ();

		//Nitrogen allocation sub-routine; nitrogen is apportioned to the various
		//compartments based on compartment size and target N content*/

		//Absolute deficit of N in compartment occurring after growth of the various C //compartments
		nitrogenBranchesSink = 0;
		nitrogenCoarseRootsSink = 0;
		nitrogenFineRootsSink = 0;
		nitrogenFoliageSink = 0;
		nitrogenFruitSink = 0;
		nitrogenStemSink = 0;
		nitrogenStumpSink = 0;

		if(this.getPhenologicalStage() == 1 || this.getPhenologicalStage() == 2){
			nitrogenBranchesSink = Math.max(optiNCBranch-(getNitrogenBranches()/getCarbonBranches()),0)
						* getCarbonBranches();
			nitrogenCoarseRootsSink = Math.max(optiNCCoarseRoot-(getNitrogenCoarseRoots()/getCarbonCoarseRoots()),0)
						* getCarbonCoarseRoots();
			nitrogenStemSink = Math.max(optiNCStem-(getNitrogenStem()/getCarbonStem()),0)
						* getCarbonStem();
			nitrogenStumpSink = Math.max(optiNCStump-(getNitrogenStump()/getCarbonStump()),0)
					* getCarbonStump();

			if (getCarbonFineRoots() > 0)
				nitrogenFineRootsSink = Math.max(optiNCFineRoot-(getNitrogenFineRoots()/getCarbonFineRoots()),0)
							 * getCarbonFineRoots();
			if (getTotalCarbonFoliage() > 0)
				nitrogenFoliageSink = Math.max(optiNCFoliage-(getTotalNitrogenFoliage()/getTotalCarbonFoliage()),0)
							 * getTotalCarbonFoliage();

			if (getCarbonFruit() > 0)
				nitrogenFruitSink = Math.max(optiNCFruit-(getNitrogenFruit()/getCarbonFruit()),0)
							 * getCarbonFruit();
		}
		//Total N sink following growth of C compartments
		double totalNSink = nitrogenBranchesSink+nitrogenCoarseRootsSink+nitrogenFineRootsSink+nitrogenFoliageSink
							+nitrogenStemSink+nitrogenStumpSink+nitrogenFruitSink;

		//relative sink of foliage and subsequent N flow
		double RelNFoliageSink = 0;
		double RelNbranchSink = 0;
		double RelNstemSink = 0;
		double RelNstumpSink = 0;
		double RelNFineRSink = 0;
		double RealNCoarseRSink = 0;
		double RealNFruitSink = 0;

		if (totalNSink>0) {
			RelNFoliageSink = nitrogenFoliageSink	/ totalNSink;
			RelNbranchSink  = nitrogenBranchesSink	/ totalNSink;
			RelNstemSink    = nitrogenStemSink		/ totalNSink;
			RelNstumpSink   = nitrogenStumpSink	/ totalNSink;
			RelNFineRSink   = nitrogenFineRootsSink	/ totalNSink;
			RealNCoarseRSink = nitrogenCoarseRootsSink	/ totalNSink;
			RealNFruitSink = nitrogenFruitSink	/ totalNSink;
		}

		double totalNitrogenAllocated = Math.min(totalNSink,getNitrogenLabile()+nitrogenUptake);
		
		nitrogenFoliageIncrement		= totalNitrogenAllocated * RelNFoliageSink;
		nitrogenBranchesIncrement		= totalNitrogenAllocated * RelNbranchSink;
		nitrogenStemIncrement			= totalNitrogenAllocated * RelNstemSink;
		nitrogenStumpIncrement			= totalNitrogenAllocated * RelNstumpSink;
		nitrogenFineRootsIncrement		= totalNitrogenAllocated * RelNFineRSink;
		nitrogenCoarseRootsIncrement	= totalNitrogenAllocated * RealNCoarseRSink;
		nitrogenFruitIncrement			= totalNitrogenAllocated * RealNFruitSink;

		// UPDATE Structural N content of compartment (kg)
		setNitrogenLabile 		(getNitrogenLabile()
									+nitrogenUptake-totalNitrogenAllocated);
				
		setNitrogenBranches  	(getNitrogenBranches ()		+ nitrogenBranchesIncrement);
		setNitrogenStem 		(getNitrogenStem ()			+ nitrogenStemIncrement);
		setNitrogenStump 		(getNitrogenStump ()		+ nitrogenStumpIncrement);
		setNitrogenFineRoots 	(getNitrogenFineRoots ()	+ nitrogenFineRootsIncrement);
		setNitrogenCoarseRoots 	(getNitrogenCoarseRoots ()	+ nitrogenCoarseRootsIncrement);
		setNitrogenFruit		(getNitrogenFruit ()		+ nitrogenFruitIncrement);
		
		//Only first cohort has nitrogen Increment
		setNitrogenFoliage 		(getNitrogenFoliageCohort(0)+ nitrogenFoliageIncrement, 0);
	}


	/**
	 * Allometric growth (dbh, height, crowndiameter)
	 */
	public void computeAllometricGrowth (SafeStand stand, SafeGeneralParameters safeSettings) {

		//Tree specific parameters
		int    crownShape 	 	 = this.getTreeSpecies ().getCrownShape ();
		double aCrown 	 		 = this.getTreeSpecies ().getCrownDbhAllometricCoeffA ();
		double bCrown	 		 = this.getTreeSpecies ().getCrownDbhAllometricCoeffB ();
		double woodDensity 		 = this.getTreeSpecies ().getWoodDensity ();
		double woodCarbonContent = this.getTreeSpecies ().getWoodCarbonContent ();
		double leafMassArea  	 = this.getTreeSpecies ().getLeafMassArea ();
		double leafCarbonContent = this.getTreeSpecies ().getLeafCarbonContent();
		double dcbFromDbh		 = this.getTreeSpecies ().getDcbFromDbhAllometricCoeff ();
		double  maxHeightInc 	 = this.getTreeSpecies ().getMaxHeightInc();
		double  maxDbhInc 	 	 = this.getTreeSpecies ().getMaxDbhInc();

		//UPDATE tree dimensions
		if (getCarbonAllocToGrowth() > 0 ) {

			setStemVolume (getCarbonStem () / (woodCarbonContent * woodDensity));

			double newHeight = computeNewHeight (safeSettings, getStemVolume(), getDbhMeters());// gt - 26/10/2010
			double newDbhMeter    = computeNewDbh (getStemVolume(), getHeight());// gt - 26/10/2010
			double dbhMeterBefore = getDbhMeters();

			setHeight (Math.max(getHeight(), Math.min(getHeight()+maxHeightInc, newHeight)));
			setDbhMeters (Math.max(getDbhMeters(), Math.min(getDbhMeters()+maxDbhInc, newDbhMeter)));
			setDbh (getDbhMeters() * 100); //cm
			
			//after topping dbase is growing like dbh
			//VOIR COMMENT ON REPASSE A LA NORMALE ????
			if (this.isTopped()) {
				double ratio = getDbhMeters()/dbhMeterBefore;
				setDBaseMeters(getDBaseMeters()*ratio);
				//setTopped(false);
			}
			//before topping dbase is normal
			else {
				setDBaseMeters(getDbhMeters() * (getHeight()/(getHeight() - 1.3)));	//m 
			}		
		}

		//UPDATE leaf area if growing season	
		for (int index = 0; index < this.treeSpecies.getNbCohortMax(); index++) {
			setLeafArea (Math.max((getCarbonFoliageCohort(index)/leafCarbonContent)*(1/leafMassArea),0.001),index);	
		}
		if (this.getTreeSpecies ().getPhenologyType() == 1) {
			if (this.getPhenologicalStage() == 0 || this.getPhenologicalStage() == 4)  setLeafArea(0,0);
		}

		
		//UPDATE TREE CROWN SIZE 
		//crown radial expansion (as well as growth in height is stopped once leaf expansion is over
		if ((this.getPhenologicalStage() == 1) /*& (branchImbalance>=0.95)*/) {		// gt - 11.12.2009 - if branch biomass does'nt fill crown volume : crown radius are not allowed to grow

			double truncationRatio = this.getTreeSpecies().getEllipsoidTruncationRatio();
			//double branchVolumeRatio = this.getTreeSpecies().getBranchVolumeRatio();
			branchImbalance = getCarbonBranches()/(getBranchVolume() * woodDensity * woodCarbonContent);
			double maxCrownRadiusInc =  this.getTreeSpecies().getMaxCrownRadiusInc();
			
			//Ellipsoid crown shape (after G.Vincent 2003)
			if (crownShape == 1) {

				//crown radius calcultation limited by tree line and inter-row distance
				maxCrownRadiusInc = maxCrownRadiusInc*Math.min(1,branchImbalance);
				
				double dcbMeters = getDbhMeters()*Math.min(1,Math.pow(1+(1.3-getCrownBaseHeight())/getHeight(),1/dcbFromDbh));

				double crownArea = aCrown * Math.pow(dcbMeters,bCrown);		// gt - 27.01.2009
				
				double newRadii [] = new double [2];
				newRadii = computeRadiiDeformation (crownArea , stand);
				
				setCrownRadiusTreeLine(Math.max(getCrownRadiusTreeLine(),Math.min(getCrownRadiusTreeLine()+maxCrownRadiusInc,newRadii[0])));
				setCrownRadiusInterRow(Math.max(getCrownRadiusInterRow(),Math.min(getCrownRadiusInterRow()+maxCrownRadiusInc,newRadii[1])));

				//truncation ratio may only be altered by pruning
				//if truncated ellipsoid
				setCrownRadiusVertical (((height-crownBaseHeight)/(1-truncationRatio))/2);

				setCrownVolume (Math.PI  * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical() *
						(2 * (1-truncationRatio) - (1d/3d * ( 1 - Math.pow (2*truncationRatio-1,3)))));

			}

			//Paraboloide of revolution crown shape (after B.Courbeaud 2000)
			if (crownShape == 2) {
				setCrownParameter ((aCrown * Math.pow ((getDbhMeters () / getHeight ()), bCrown)));
				setCrownRadiusTreeLine (Math.sqrt (getCrownParameter() * (getHeight () - getCrownBaseHeight ())));
				setCrownRadiusInterRow (Math.sqrt (getCrownParameter() * (getHeight () - getCrownBaseHeight ())));

				setCrownVolume (1d/2d * Math.PI * getCrownParameter()
								* Math.pow(getHeight () - getCrownBaseHeight(),2));
			}

		}


	}
	/**
	 * return Boolean, tracking stem "compression"
	 */
	public boolean getIsShort()  {

		//To allow the tree to become tall when it is close 98% to the allometric  height
		if (getHeight()*1.001 < getAllometricHeight ()) {			
			return (true);
		}
		else {
			return (false);
		}
	}
	/**
	* return tree height
	*/
	public double computeNewHeight (SafeGeneralParameters safeSettings, double newStemVolume, double newDbhMeters) {

		double aTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		double aStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffA ();
		double bStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffB ();
		double cStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffC ();

		if (this.getPhenologicalStage() == 1) {
			//tree height growth and crown expansion is possible
		   if (this.getIsShort()) {

			   
				// new potential height is estimated assuming no additional dbh increment
				return (Math.pow (
						   newStemVolume * Math.exp(-aStemVolume) * Math.pow(newDbhMeters,-bStemVolume),
						   (1/cStemVolume)));
		   }
			else {

				// new potential height is estimated assuming allometric height and dbh relation is maintained
				return (Math.pow (
							newStemVolume * Math.exp(-aStemVolume) * Math.pow(aTree,(bStemVolume/bTree)),	
							(1/((bStemVolume/bTree) +cStemVolume))));
			}

		}
		else return (this.getHeight()); // height growth has ceased
	}
	/**
	* return tree dbh
	*/
	public double computeNewDbh (double newStemVolume, double newHeight) {

		double aTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		double aStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffA ();
		double bStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffB ();
		double cStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffC ();

		if (this.getPhenologicalStage() == 1) {
		   if (this.getIsShort())
				// dbh remains unchanged
				return (dbhMeters);
			else
			//dbh growth
				return (Math.pow
						(newStemVolume *  Math.exp(-aStemVolume) * Math.pow(aTree,-cStemVolume),
						(1/(bStemVolume + bTree*cStemVolume))));
		}
		else 
		{
			return (Math.pow
					(newStemVolume * Math.exp(-aStemVolume) * Math.pow(newHeight,-cStemVolume),
					(1/bStemVolume)));
		}

		

	}
	

	
		
	/**
	 * Cellular automata for fine roots grocth
	 * @author Rachmat MULIA (ICRAF) - August 2003
	  * 
	  * En tres grande partie reecrit par gregoire Talbot entre 2008 et 2011
	  * cette methode est appelle une premiere fois avec le booleen reallyOrNot=false par le module d'allocation de carbone 
	  * afin de definir l'allocation entre racines fines et racines de structure. 
	  * Dans cet appel, seule la premiere partie de la methode est active, et les densites de racines fines dans les voxels ne sont pas mises a jour
	  * le second appel, avec reallyOrNot=true, est le bon. Il gere alors la proliferation (allocation des nouvelles racines fines entre voxels, 
	  * en fonction des efficiences d'extraction d'eau, d'azote, et de la distance a l'arbre) et la colonisation de nouveaux voxels
	 */
	private void fineRootGrowth (SafeGeneralParameters safeSettings,	
								SafePlotSettings plotSettings,
								int simulationDay,
								double carbonFineRootsIncrement, 
								boolean reallyOrNot) {	// GT 17/07/2008

		double  specificRootLength 	= this.getTreeSpecies().getSpecificRootLength ();


		
		int treeIndex = this.getId()-1;			//index of this tree

		//compute nbr voxel in this plot
		SafeCell treeCell = (SafeCell) this.getCell();
		int nbVoxelTot = 0;
		SafePlot plot = (SafePlot) treeCell.getPlot();
		nbVoxelTot = treeCell.getVoxels().length * plot.getCells ().size();
		
		//temporary storing tables
		double [] additionalRootLenght    = new double [nbVoxelTot];	//m
		
		//if this tree  has roots !!!!
		if (getPlantRoots().getFirstRootNode() != null) {

			//convert carbon entry (kg C) in root lenght (m)
			double carbonToDryMatter = 1d / this.getTreeSpecies ().getWoodCarbonContent();
			double frCost = 1/(carbonToDryMatter*1000*specificRootLength); // 1/(Kg.Kg-1*g.Kg-1*m.g-1)=Kg.m-1
			double additionalRoot = carbonFineRootsIncrement/frCost;	//Kg/Kg.m-1 = m
			
			// la methode computeFineRootCost est une methode recursive, qui va calculer le cout en carbone des nouvelles racines fines 
			//(prenant en compte les besoins en racines de structure) pour l'ensemble des voxels colonises par l'arbre.
			double rootedVolume = getPlantRoots().getFirstRootNode().computeRootedVolume();

			getPlantRoots().getFirstRootNode().setFineRootsCost (0);
			getPlantRoots().getFirstRootNode().computeFineRootsCost(treeIndex,
										           additionalRoot,
										           rootedVolume,
										           this.getTreeSpecies().getWoodDensity(),
										           this.getTreeSpecies ().getWoodCarbonContent (),
					                               this.getTreeSpecies().getCRAreaToFRLengthRatio(),
					                               frCost);


			//calculation of water efficiency, nitrogen efficiency and coarse roots cost coefficient 
			double nitrogenMaxEfficency = 0;
			double waterMaxEfficency = 0;
			double fineRootsCostMax = 0;

			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getPlantRoots().getRootTopology (voxel);
				if (node != null) {
					
					// gt 17.02.2009 - no fine root growth in voxels whose parent is saturated
					boolean fineRootGrowthInThisVoxel = true;
					if (!(node.getNodeParent()==null)){
						if(node.getNodeParent().getVoxelRooted().getIsSaturated()) {	
							fineRootGrowthInThisVoxel=false;
						}
					}
					
					if(fineRootGrowthInThisVoxel) {
						waterMaxEfficency = Math.max(waterMaxEfficency, node.getWaterEfficiency());
						nitrogenMaxEfficency = Math.max(nitrogenMaxEfficency, node.getNitrogenEfficiency());
						fineRootsCostMax = Math.max(fineRootsCostMax, node.getFineRootsCost());
						voxel.setWaterEfficiency(node.getWaterEfficiency());
						voxel.setNitrogenEfficiency(node.getNitrogenEfficiency());
						voxel.setFineRootsCost(node.getFineRootsCost());

					}
				}
			 }

			
			
			double  localWaterUptakeFactor = this.getTreeSpecies().getLocalWaterUptakeFactor();
			double  localNitrogenUptakeFactor = this.getTreeSpecies().getLocalNitrogenUptakeFactor();
			double  sinkDistanceEffect = this.getTreeSpecies().getSinkDistanceEffect();
			
			double[] coefWater = new double[2];
			coefWater[0] = Math.pow(this.getWaterStress(),localWaterUptakeFactor);
			coefWater[1] = 0;
			 if(waterMaxEfficency > 0){
				 coefWater[1] = ((1+localWaterUptakeFactor)-coefWater[0])/waterMaxEfficency; 
			}
			 
			double[] coefNitrogen = new double[2];
			coefNitrogen[0] = Math.pow(this.getNitrogenSaturation(),localNitrogenUptakeFactor);
			coefNitrogen[1] = 0;
			if(nitrogenMaxEfficency > 0)
				 coefNitrogen[1] = ((1+localNitrogenUptakeFactor)-coefNitrogen[0])/nitrogenMaxEfficency; 

			
			double[] coefCost = new double[2];
			coefCost[0] = 1+sinkDistanceEffect;
			coefCost[1] = 0;
			if(fineRootsCostMax > 0) {
				coefCost[1] = -sinkDistanceEffect/fineRootsCostMax; 
			}		 
			 
			double totalMark = 0; 
			
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getPlantRoots().getRootTopology (voxel);

				
				if (node != null) {
					
					//test export
					voxel.setWaterEfficiencyMax(waterMaxEfficency);
					voxel.setNitrogenEfficiencyMax(nitrogenMaxEfficency);
					voxel.setFineRootsCostMax(fineRootsCostMax);
					voxel.setCoefWater0(coefWater[0]);
					voxel.setCoefWater1(coefWater[1]);
					voxel.setCoefNitrogen0(coefNitrogen[0]);
					voxel.setCoefNitrogen1(coefNitrogen[1]);
					voxel.setCoefCost0(coefCost[0]);
					voxel.setCoefCost1(coefCost[1]);


					
					
					// gt 17.02.2009 - no fine root growth in voxels whose parent is saturated
					boolean fineRootGrowthInThisVoxel = true;
					if (!(node.getNodeParent()==null)) {
						if(node.getNodeParent().getVoxelRooted().getIsSaturated()){	
							fineRootGrowthInThisVoxel=false;
						}
					}

					if(fineRootGrowthInThisVoxel){	

						// gt 17.02.2009 - waterUptake is replaced by waterUptakeEfficiency
						double waterMark =  coefWater[0] + (coefWater[1]*node.getWaterEfficiency());
						double nitrogenMark = coefNitrogen[0] + (coefNitrogen[1]*node.getNitrogenEfficiency());
						double costMark = coefCost[0] + (coefCost[1]*node.getFineRootsCost());
						double tot = waterMark * nitrogenMark * costMark * voxel.getVolume();
						totalMark += tot;
					
						//test export
						voxel.setWaterMark(waterMark);
						voxel.setNitrogenMark(nitrogenMark);
						voxel.setCostMark(costMark);	
					
	
					}
					
				}
			}


			// explication des notes pour l'eau et l'azote :
			// j'explique pour l'eau, c'est exactement pareil pour l'azote
			// la note waterMark pour un voxel est proportionnelle Ã¯Â¿Â½ l'efficience d'extraction d'eau dans ce voxel (waterUptake/fineRootLength)
			// cette proportionnalite est definie par une droite dont l'origine (valeur de la note pour une efficience = 0) est egale a la valeur du stress hydrique exposant LocalWaterUptakeFactor, et la valeur maximale (pour efficience=efficienceMax) est egale a 1+LocalWaterUptakeFactor
			// grace a ce formalisme, on a le comportement suivant :
			// si LocalWaterUptakeFactor = 0, la note est toujours 1, independemment du stress
			// si WaterStress = 1 (pas de stress hydrique) et LocalWaterUptakeFactor>0, la note varie entre 1 et 1+LocalWaterUptakeFactor, cela permet d'allouer plus de carbone dans les voxels ou l'extraction d'eau est plus efficace, mais cela n'handicape pas trop les autres (ce qui permet aux autres notes, nitrogenMark et costMark de s'appliquer)
			// si WaterStress  < 1 et LocalWaterUptakeFactor>0, la note varie entre WaterStress^LocalWaterUptakeFactor et 1+LocalWaterUptakeFactor. Plus le stress hydrique est fort, plus les voxels dans lesquels l'extraction d'eau est peu efficace sont penalises.
			
			// explication de la note costMark :
			// cette note est inversement proportionnelle au cout des racines fines (fineRootCost)
			// Si fineRootCost=0, la note est egale a 1+sinkDistanceEffect (parametre)
			// la note est egale a 1 pour fineRootCost=fineRootCostMax
			
			// la note globale d'un voxel est proportionnelle au produit des trois note costMark*nitrogenMark*waterMark
			// la somme des notes de tous les voxels est egale a 1. C'est pour cela qu'on calcule d'abord la somme de costMark*nitrogenMark*waterMark pour l'ensemble des voxels
			// on obtient les notes definitives en divisant les notes costMark*nitrogenMark*waterMark par cette somme.
			
			//FIRST LOOP - FOR EACH ROOTED VOXEL,
			//Calculation of carbon allocation
			//proportionally to water and nitrogen Uptake Efficiency
 
			//IL ICI ON PEUX REMPLACER PAR LECTURE DE RootTopology
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeVoxel [] voxels = cell.getVoxels();
				for (int i=0; i<voxels.length ; i++) {
				
					//if voxel is rooted by this tree
					double voxelRootDensity =  voxels[i].getTheTreeRootsDensity (treeIndex);	//m m-3
					if (voxelRootDensity > 0) {
		
						int voxelId = voxels[i].getId()-1;

						double proportion = 0;
						SafeRootNode node = getPlantRoots().getRootTopology (voxels[i]);

						boolean fineRootGrowthInThisVoxel = (getPlantRoots().getRootTopology (voxels[i]).getNodeParent()==null);
						if (!(node.getNodeParent()==null)) {
							// gt 17.02.2009 - no fine root growth in voxels whose parent is saturated
							if (!(node.getNodeParent().getVoxelRooted().getIsSaturated())) {	
								fineRootGrowthInThisVoxel=true;
							}
						}
						
						if ((fineRootGrowthInThisVoxel) && (totalMark != 0)) {	
							proportion = (voxels[i].getWaterMark()*voxels[i].getNitrogenMark()*voxels[i].getCostMark()*voxels[i].getVolume())/totalMark; 
							voxels[i].setTotalMark(totalMark);
							voxels[i].setProportion(proportion);
						}
						

						
						double voxelAdditionalRoot = additionalRoot * proportion;	    //m

						additionalRootLenght[voxelId] += voxelAdditionalRoot  ; 	//m;

					}
				}
			}

			// colonisation
			// only for real fine root growth, not for calculation of CR versus FR allometry GT 17/07/2008
			if(reallyOrNot){
				
				
				double alpha 	= this.getTreeSpecies().getColonisationThreshold ();
				double lambda	= this.getTreeSpecies().getHorizontalPreference ();
				double eta 		= this.getTreeSpecies().getGeotropismFactor ();
				double colonisationFraction = this.getTreeSpecies().getColonisationFraction ();

				//IL ICI ON PEUX REMPLACER PAR LECTURE DE RootTopology
				
				for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
					SafeCell cell = (SafeCell) c.next ();
					SafeVoxel [] voxels = cell.getVoxels();
					for (int i=0; i<voxels.length ; i++) {
						int voxelId = voxels[i].getId()-1;
						double additionalRootToVoxel = additionalRootLenght[voxelId]  ; 	//m
						
						voxels[i].setAdditionalRootsToVoxel(additionalRootToVoxel);
						
						SafeVoxel upperVoxel=voxels[i];
						if(i!=0){
							upperVoxel=voxels[i-1];
						}
						if ((additionalRootToVoxel > 0) && (!upperVoxel.getIsSaturated())) {
							int direction = getPlantRoots().getRootTopology (voxels[i]).getColonisationDirection();
							
							// conversion of directions :
							// 0 = x+ = right dir = 0
							// 1 = x- = left  dir = 0
							// 2 = y+ = front dir = 1
							// 3 = y- = back  dir = 1
							// 4 = z+ = down  dir = 2
							// 5 = z- = up    dir = 2

							// neighbour voxels
							SafeVoxel [] neighbours = new SafeVoxel [6];
							if (plot.getCell (cell.getCellIdRight()) != null) //can be null if toric symetry is OFF of Xp
								neighbours[0]=(((SafeCell) (plot.getCell (cell.getCellIdRight() ))).getVoxels())[i];
							if (plot.getCell (cell.getCellIdLeft()) != null) //can be null if toric symetry is OFF of Xn
								neighbours[1]=(((SafeCell) (plot.getCell (cell.getCellIdLeft () ))).getVoxels())[i];
							if (plot.getCell (cell.getCellIdFront()) != null) //can be null if toric symetry is OFF of Yn
								neighbours[2]=(((SafeCell) (plot.getCell (cell.getCellIdFront() ))).getVoxels())[i];
							if (plot.getCell (cell.getCellIdBack()) != null) //can be null if toric symetry is OFF of Yp
								neighbours[3]=(((SafeCell) (plot.getCell (cell.getCellIdBack () ))).getVoxels())[i];
							if(i!=(voxels.length-1))
								neighbours[4]=voxels[i+1];
							else
								neighbours[4]=null;
							if(i!=0)
								neighbours[5]=voxels[i-1];
							else
								neighbours[5]=null;
							
							
							// voxel dimensions
							
							//double Lxy=plotSettings.cellWidth;
							//double Lz=voxels[i].getThickness();
							//MODIF IL 04/02/2016 Take fine soil volume to simulate stone options
							double stone = voxels[i].getLayer().getStone(); 	//stone proportion
							double Lxy = plotSettings.cellWidth   * Math.pow((1-(stone/100.0)),(1.0/3.0));
							double Lz  = voxels[i].getThickness() * Math.pow((1-(stone/100.0)),(1.0/3.0));
							 
							// voxel dimensions correction for provenance : voxel volume is conserved
							// the voxel size in direction x (or y or z) is proportional to euclidian distance between voxel's father gravity center and the voxel that will be colonised
							// in direction x (or y or z) gravity center.
						
							double[] L = new double[3];

							if (direction >= 4) {	// voxel was colonised from z direction
								L[0]=L[1]=Math.sqrt(Math.pow(Lxy,2)+Math.pow(Lz,2));
								L[2]=2*Lz;

								
							} else {	// voxel was colonised from an horizontal direction
								L[2]=Math.sqrt(Math.pow(Lxy,2)+Math.pow(Lz,2));
								
								if (direction >= 2){ // voxel was colonised from y direction
									L[0]=Math.sqrt(2)*Lxy;
									L[1]=2*Lxy;
								} else {	// voxel was colonised from x direction
									L[0]=2*Lxy;
									L[1]=Math.sqrt(2)*Lxy;
								}
							}
							
							double coef = Math.pow((Lxy*Lxy*Lz)/(L[0]*L[1]*L[2]),1.0/3.0);	// correction to conserve voxel volume
							L[0]=coef*L[0];
							L[1]=coef*L[1];
							L[2]=coef*L[2];
				

							// voxel dimensions modification for plagiotropism
							// le voxel est deforme par lambda. 
							// Si lambda = 0.5, la forme du voxel est conservÃ¯Â¿Â½e
							// Si lambda <0.5, le voxel est aplati (son epaisseur z est diminuee, ses largeurs x et y sont augmentees)
							// Si lambda >0.5, c'est le contraire
							// le volume total du voxel est conserve, c'est le pourquoi des 1/sqrt(2*lambda)
							
							L[0]=1/Math.sqrt(2*lambda)*L[0];
							L[1]=1/Math.sqrt(2*lambda)*L[1];
							L[2]=2*lambda*L[2];
							double Lmin=Math.min(Math.min(L[0],L[1]),L[2]);
							double Lmax=Math.max(Math.max(L[0],L[1]),L[2]);

							double voxelFilling =  (getPlantRoots().getRootTopology(voxels[i]).getFineRootsTotalInvestment()
													*voxels[i].getVolumeFineSoil()
													+additionalRootToVoxel)/alpha;	//m m-3 * m3
							
							voxels[i].setL0(L[0]);
							voxels[i].setL1(L[1]);
							voxels[i].setL2(L[2]);
							voxels[i].setLmin(Lmin);
							voxels[i].setLmax(Lmax);
							
							voxels[i].setVoxelFilling(voxelFilling);
							int neighboursColonisedNumber = 0;
							boolean[] colonisation = new boolean[6];

							for(int n=0;n<6;n++){

								if(neighbours[n]!=null){

									
									if (getPlantRoots().getRootTopology().containsKey(neighbours[n])){ // if a neighbour voxel contains fine roots, no colonisation
										colonisation[n]=false;
									} else {
										double geo = 0;
										if (n==5) {	// colonisation of the upper voxel
											geo = eta; // application of the geotropism factor to all thresholds
										}
										
										int dir = (int) Math.floor(n/2);		
										// dir = 0 for x, 1 for y and 2 for z
										// there are three thresholds for colonisation :	
										// in the smaller direction, threshold is T1=Lmin^3
										//	in the medium direction, threshold is T2=Lmin*Lmoy^2
										//	in the larger direction, threshold is T3=Lmin*Lmoy*Lmax
										
										// pour bien comprendre le pourquoi de ces trois seuil, il faut imaginer un petit cube dont le volume est egal a voxelFilling, 
										// et qui grossit au centre du voxel. Lorsque qu'il touche un premier bord (seuil 1), il colonise un premier voxel
										// ensuite, il continue a se developper mais il n'est plus cubique 
										// car sa croissance dans la premiere dimension s'est arretee faute de place dans le voxel... 
										// jusqu'a toucher un second bord (seuil 2)
										// a la fin, il ne se developpe plus que dans une direction, jusqu'a atteindre le seuil 3.

										if(L[dir]>=Lmax){
											double T3threshold = L[0]*L[1]*L[2]*(1+geo);
											voxels[i].setT3threshold(n,T3threshold);
											if(voxelFilling > T3threshold){												
												colonisation[n] = true;
												neighboursColonisedNumber++;												
											} else {
												colonisation[n]=false;
											}
											
										} else {
											if((L[dir]<Lmax)&&(L[dir]>Lmin)) {	// dans ce cas, L[dir]=lmoy
												double T2threshold = (Lmin*Math.pow(L[dir],2)*(1-geo))+(L[0]*L[1]*L[2]*geo);
												voxels[i].setT2threshold(n,T2threshold);
												if (voxelFilling > T2threshold){
													colonisation[n] = true;
													neighboursColonisedNumber++;
												} else {
													colonisation[n]=false;
												}
												
											} else { // dans ce cas, L[dir]=lmin		
												double T1threshold = (Math.pow(L[dir],3)*(1-geo))+(L[0]*L[1]*L[2]*geo);
												voxels[i].setT1threshold(n,T1threshold);
												if(voxelFilling > T1threshold){
													colonisation[n] = true;
													neighboursColonisedNumber++;
												} else {
													colonisation[n]=false;
												}
											}
										}
									}
								}								
							}

							voxels[i].setNeighboursColonisedNumber(neighboursColonisedNumber);
							
							//colonisationFraction of AdditionnalRoot for colonisation
							//(1-colonisationFraction) of AdditionnalRoot for proliferation 
							// L'idee est d'allouer une faible partie du carbone a la colonisation. 
							//La colonisation est uniquement une initialisation d'un nouveau SafeRootNode. 
							// Les pas de temps suivants, les quantites de racines dans ce nouveau voxel seront gerees par la proliferation
							if (neighboursColonisedNumber > 0){
								for (int n=0; n<6; n++){
									if (colonisation[n]){
										double colonisationRootLength = (additionalRootToVoxel*colonisationFraction)/neighboursColonisedNumber;
										getPlantRoots().addTreeRootTopology (this, neighbours[n], voxels[i], simulationDay, colonisationRootLength/neighbours[n].getVolume(), n);
										neighbours[n].setTreeRootsDensity(treeIndex,colonisationRootLength/neighbours[n].getVolume()); 
										neighbours[n].setColonisationDirection(treeIndex, n);	
									}
								}
								additionalRootToVoxel=additionalRootToVoxel* (1-colonisationFraction);
							}

					
						
							double voxelRootDensity = voxels[i].getTheTreeRootsDensity (treeIndex) + (additionalRootToVoxel/voxels[i].getVolume());					
						
	
	
							if(voxelRootDensity > 0){
								voxels[i].setTreeRootsDensity (treeIndex, voxelRootDensity);
								
								getPlantRoots().getRootTopology(voxels[i]).setFineRootsDensity(voxelRootDensity);
								getPlantRoots().getRootTopology(voxels[i]).addFineRootsTotalInvestment(additionalRootToVoxel/voxels[i].getVolumeFineSoil());
								//a enlever c'est pour tester le module
								voxels[i].setFineRootsTotalInvestment(getPlantRoots().getRootTopology(voxels[i]).getFineRootsTotalInvestment());
							}
						
						} //end if additionalRootToVoxel > 0
						
						
					}//end for each voxel 
				}//end for each cell
			} // end of if(reallyOrNot)
			// GT 17/07/2008

			
			//compute carbonFineRoot in each voxel 
			double totalRootDensity = 0;
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeVoxel [] voxels = cell.getVoxels();
				for (int i=0; i<voxels.length ; i++) {
					totalRootDensity += voxels[i].getTheTreeRootsDensity (treeIndex);
				}
			}
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeVoxel [] voxels = cell.getVoxels();
				for (int i=0; i<voxels.length ; i++) {
					double carbon = getCarbonFineRoots() * (voxels[i].getTheTreeRootsDensity (treeIndex) / totalRootDensity);
					voxels[i].setTreeCarbonFineRoots (treeIndex, carbon);
					double nitrogen = getNitrogenFineRoots() * (voxels[i].getTheTreeRootsDensity (treeIndex) / totalRootDensity);
					voxels[i].setTreeNitrogenFineRoots (treeIndex, nitrogen);
				}
			}
						
			//compute coarse Root target related to additionnal fine roots lenght
			this.setCarbonCoarseRootsTarget(0);
			getPlantRoots().getFirstRootNode().computeCarbonCoarseRootsTarget(this, 
																			additionalRootLenght,
																			this.getTreeSpecies().getWoodDensity(),
																			this.getTreeSpecies().getWoodCarbonContent(),
																			this.getTreeSpecies().getCRAreaToFRLengthRatio());

		} // end of if tree has roots

		return;
	}


	/**
	 * Coarse roots growth
	 * Carbon is allocated to coarse roots in each voxel proportionnally to water flux through this voxel the day before
	 * Like Pipe model Shinozaki et al., 1964
	 * @author Rachmat MULIA (ICRAF) - August 2004
	 */
	private void coarseRootGrowth (double carbonCoarseRootsIncrement, double nitrogenCoarseRootsIncrement) {

		//if this tree  has fine roots !!!!
		if (getPlantRoots().getRootTopology() != null) {

			SafeRootNode firstNode = getPlantRoots().getFirstRootNode () ;
			
			double totalNodeImbalance = firstNode.computeCarbonCoarseRootsImbalance(this);

			if (totalNodeImbalance > 0) {
				firstNode.computeCarbonCoarseRoots (this, 
						  carbonCoarseRootsIncrement,
						  nitrogenCoarseRootsIncrement,
						  totalNodeImbalance);
			}
			else {
				System.out.println("DEJA ASSEZ DE CARBONE DANS LES CR ??? ");
			}
	
		}
	}

	/**
	 * Sometimes nitrogen is allocated to fine root BUT NO carbon
	 * This nitrogen will be affected to the first rooted voxel (it is rare and very small values) 
	 * @author Isabelle LECOMTE - September 2024
	 */
	private void nitrogenVoxelAllocation(double nitrogenFineRootsIncrement) {
	
		//if this tree  has roots !!!!
		if (getPlantRoots().getFirstRootNode() != null) {
			int treeIndex = this.getId()-1;			//index of this tree
			SafeVoxel firstVoxel = getPlantRoots().getFirstRootNode().getVoxelRooted();
			firstVoxel.addTreeNitrogenFineRoots (treeIndex, nitrogenFineRootsIncrement);
		}
	}
	/**
	* Compute TREE total root length 
	**/
	public double computeTotalRootLength () {
	
		double totalRootLength = 0;
		
		if (getPlantRoots().getFirstRootNode() == null) return 0 ;  
		Set keys = getPlantRoots().getRootTopology().keySet();
		for(Iterator it = keys.iterator(); it.hasNext ();){
			SafeRootNode node = (SafeRootNode) getPlantRoots().getRootTopology().get(it.next());
			totalRootLength += node.getFineRootsDensity()*node.getVoxelRooted().getVolume();
		}
	
		this.getPlantRoots().setTotalRootsLength(totalRootLength);
		
		return totalRootLength;
	}
		
	

	/**
	* Compute TREE plant water potential 
	**/
	public void computePlantWaterPotential (SafeGeneralParameters settings){

		if (getPlantRoots().getFirstRootNode() == null) return;  
	
		if (this.getTotalRootLength() <= 0) return;
		
		double drySoilFactor = this.getTreeSpecies().getTreeHarmonicWeightedMean();

		double plantPotential = 0;
		
		Set keys = getPlantRoots().getRootTopology().keySet();
		for(Iterator it = keys.iterator(); it.hasNext ();){
			
			SafeRootNode node = (SafeRootNode) getPlantRoots().getRootTopology().get(it.next());
			SafeVoxel v = node.getVoxelRooted();
			double neededPot = v.getWaterPotentialTheta();	// soil water potential in this voxel

			if (node.getFineRootsDensity() > 0) {			//IL 14/08/2020 add this test
				
				// additional potential for water flow from bulk soil to rhizosphere
				neededPot *= (1+this.getTreeSpecies().getTreeBufferPotential()); 
				// additional potential for water flow from root surface to xylem
				double radialTransportPotential = -this.getWaterDemand()								// L.day-1=dm3.day-1
													* 1000												// from L.day-1 to cm3.day-1
													/this.getTreeSpecies().getTreeRootConductivity()	// cm day-1
													/(this.getTotalRootLength()*100);					// m to cm
								
				
				this.getPlantRoots().setRadialTransportPotential(radialTransportPotential);
				
	
				neededPot += radialTransportPotential;
	
			
				// additional potential to account for longitudinal water transport in coarse roots from the voxel to stem base
				double longitudinalTransportPotential = -this.getWaterDemand()										//L.day-1=dm3.day-1
															* 1000															// from L.day-1 to cm3.day-1
															* this.getTreeSpecies().getTreeLongitudinalResistantFactor()	// day.cm-1.m-1
															/(this.getTotalRootLength()*100);								// m to cm
				// in the model documentation from Meine et al, this term is not divided by totalRootLength... but it leads to very different longitudinal drop potential for small and large trees because of differences in water demand... so...
				this.getPlantRoots().setLongitudinalTransportPotential(longitudinalTransportPotential);
				neededPot += longitudinalTransportPotential*node.getTreeDistance();	// topological distance (m) between the voxel and stem base
	
				
				// additional potential to account for voxel depth
				neededPot -= v.getZ()*100;		// from m to cm
	
		
				plantPotential += (-node.getFineRootsDensity() * v.getVolume()	// m.m-3 * m3 = m
										/ Math.pow(-neededPot, drySoilFactor));	// cm

				
			}

		}
		

		
		plantPotential =-Math.pow (-this.getTotalRootLength() /plantPotential , 1/drySoilFactor);

		this.getPlantRoots().setRequiredWaterPotential(plantPotential);


	}


	/**
	 * Foliage Senescence Computation - updated by gt 09.10.2009 : sigmoidal instead of exponential decrease
	 * fallenLeafProp(t)= 1/(1+exp(-b*(t-a)))
	 * a is the date for which half of leaves are fallen, it can be computed as a=LeafFallStartingDate+leafFallDuration/2
	 * b is a time constant. If we consider that leaf fall begin when 1% of leaves are fallen, it can be computed as b=2*log(99)/leafFallDuration
	 */
	public void computeCarbonFoliageSenescence (int julianDay, SafeDailyClimat dayClimat, SafeGeneralParameters	safeSettings) {

		int leavesResiduesSpreading = safeSettings.leavesResiduesSpreading;	//0=exported 1=under tree 2=all plot
		
		//FROST SENESCENCE
		//frost damage is applied to all cohorts and for all phenology type 
		double frostDamage = 1;
		if (this.treeItk.frostDamageActivation) {
			float tmin = dayClimat.getMinTemperature();
			double startDefoliationTemperature = this.getTreeSpecies ().getLeafFrostStressTemperatureMin();		//degrees
			double completeDefoliationTemperature = this.getTreeSpecies ().getLeafFrostStressTemperatureMax();	//degrees
			if (completeDefoliationTemperature <= tmin && tmin <= startDefoliationTemperature) {
				frostDamage = Math.max((tmin-completeDefoliationTemperature) / (startDefoliationTemperature-completeDefoliationTemperature),0.1);
			}
			else {
				if (tmin < completeDefoliationTemperature) frostDamage = 0.1;
			}	
		}

		setLeafFrostStress(frostDamage);
		

		//AGE SENESCENCE during LEAF FALL
		double leafAgeStress = 1;
	
		if (this.getPhenologicalStage() == 3){
			// computed for respecting the sigmoidal decrease with parameters a and b
			double a = getLeafFallStartingDate()+this.getTreeSpecies ().getLeafFallDuration()/2;
			double b = 2*Math.log(99)/this.getTreeSpecies ().getLeafFallDuration();
			double fastSenescenceRate = 1-(1+Math.exp(-b*(julianDay-a)))/(Math.exp(b)+Math.exp(-b*(julianDay-a)));		
			leafAgeStress = 1 - (this.getTreeSpecies ().getLeafSenescenceRate() + fastSenescenceRate);
		}
		setLeafAgeStress(leafAgeStress);
	

		//NB+IL 09/04/2021 
		//AGE SENESCENCE before LEAF FALL only for EVERGREEN TREES
		double leafWaterNitrogenStress = 1;
		int lastLeafIndex = this.getTreeSpecies().getNbCohortMax()-1;
			if (this.getTreeSpecies().getPhenologyType() == 2 && this.getPhenologicalStage() < 3) {
			
			//searching the last cohort with leaves to applies water and nitrogen stress effect
			while ((lastLeafIndex >= 0) && (getCarbonFoliageCohort(lastLeafIndex)==0)) {
				lastLeafIndex--;
			}			
			leafWaterNitrogenStress     = Math.pow (getWaterStress(),this.getTreeSpecies().getSenWaterStressResponsiveness ())
											* Math.pow (getNitrogenSatisfaction(), this.getTreeSpecies().getSenNitrogenStressResponsiveness ());
		}
			
		setLeafWaterNitrogenStress(leafWaterNitrogenStress);
		

		//APPLY CARBON and NITROGEN STRESS on LEAVES
		double leafNRemobFrac = this.getTreeSpecies().getLeafNRemobFraction();
		int maxIndex = this.getTreeSpecies().getNbCohortMax()-1;
		
		for (int index = 0; index < this.treeSpecies.getNbCohortMax(); index++) {

			double frostCarbonSenescence = getCarbonFoliageCohort(index) * (1-frostDamage);
			setCarbonFoliage(getCarbonFoliageCohort(index)-frostCarbonSenescence, index);
			addCarbonFoliageSen(frostCarbonSenescence); 
			if (leavesResiduesSpreading==0)addCarbonFoliageExported(frostCarbonSenescence); 
			if (leavesResiduesSpreading==1)addCarbonFoliageLitterUnderTree(frostCarbonSenescence); 
			if (leavesResiduesSpreading==2)addCarbonFoliageLitterAllPlot(frostCarbonSenescence); 
			
			//no nitrogen remobilisation of frost damage before leaf fall
			//Nitrogen is totally lost 
			double frostNitrogenSenescence = getNitrogenFoliageCohort(index) * (1-frostDamage);
			setNitrogenFoliage(getNitrogenFoliageCohort(index)-frostNitrogenSenescence, index);	
			if (this.getPhenologicalStage() < 3) {
				addNitrogenFoliageSen(frostNitrogenSenescence); 
				if (leavesResiduesSpreading==0)addNitrogenFoliageExported(frostCarbonSenescence); 
				if (leavesResiduesSpreading==1)addNitrogenFoliageLitterUnderTree(frostCarbonSenescence); 
				if (leavesResiduesSpreading==2)addNitrogenFoliageLitterAllPlot(frostCarbonSenescence); 
			} 
			else {
				addNitrogenFoliageSen(frostNitrogenSenescence*(1-leafNRemobFrac));	//GT-4.06.2010
				addNitrogenLabile(frostNitrogenSenescence*leafNRemobFrac);					//GT-4.06.2010
				if (leavesResiduesSpreading==0)addNitrogenFoliageExported(frostNitrogenSenescence*(1-leafNRemobFrac)); 
				if (leavesResiduesSpreading==1)addNitrogenFoliageLitterUnderTree(frostNitrogenSenescence*(1-leafNRemobFrac)); 
				if (leavesResiduesSpreading==2)addNitrogenFoliageLitterAllPlot(frostNitrogenSenescence*(1-leafNRemobFrac)); 
			}
			

			if (index == maxIndex) {
				double ageCarbonSenescence = getCarbonFoliageCohort(index) * (1-leafAgeStress);
				setCarbonFoliage(getCarbonFoliageCohort(index)-ageCarbonSenescence, index);
				addCarbonFoliageSen(ageCarbonSenescence); 
				if (leavesResiduesSpreading==0)addCarbonFoliageExported(ageCarbonSenescence); 
				if (leavesResiduesSpreading==1)addCarbonFoliageLitterUnderTree(ageCarbonSenescence); 
				if (leavesResiduesSpreading==2)addCarbonFoliageLitterAllPlot(ageCarbonSenescence); 
				
				double ageNitrogenSenescence = getNitrogenFoliageCohort(index) * (1-leafAgeStress);
				setNitrogenFoliage(getNitrogenFoliageCohort(index)-ageNitrogenSenescence, index);
				addNitrogenFoliageSen(ageNitrogenSenescence*(1-leafNRemobFrac));	//GT-4.06.2010
				addNitrogenLabile(ageNitrogenSenescence*leafNRemobFrac); 				//GT-4.06.2010	
				if (leavesResiduesSpreading==0)addNitrogenFoliageExported(ageNitrogenSenescence*(1-leafNRemobFrac)); 
				if (leavesResiduesSpreading==1)addNitrogenFoliageLitterUnderTree(ageNitrogenSenescence*(1-leafNRemobFrac)); 
				if (leavesResiduesSpreading==2)addNitrogenFoliageLitterAllPlot(ageNitrogenSenescence*(1-leafNRemobFrac)); 
				
			}

			if (index == lastLeafIndex) {
				double stressCarbonSenescence = getCarbonFoliageCohort(index) * (1-leafWaterNitrogenStress);
				setCarbonFoliage(getCarbonFoliageCohort(index)-stressCarbonSenescence, index);	
				addCarbonFoliageSen(stressCarbonSenescence); 
				if (leavesResiduesSpreading==0)addCarbonFoliageExported(stressCarbonSenescence); 
				if (leavesResiduesSpreading==1)addCarbonFoliageLitterUnderTree(stressCarbonSenescence); 
				if (leavesResiduesSpreading==2)addCarbonFoliageLitterAllPlot(stressCarbonSenescence); 
				
				double stressNitrogenSenescence = getNitrogenFoliageCohort(index) * (1-leafWaterNitrogenStress);
				setNitrogenFoliage(getNitrogenFoliageCohort(index)-stressNitrogenSenescence, index);	
				addNitrogenFoliageSen(stressNitrogenSenescence*(1-leafNRemobFrac)); //GT-4.06.2010
				addNitrogenLabile(stressNitrogenSenescence*leafNRemobFrac); 			//GT-4.06.2010
				if (leavesResiduesSpreading==0)addNitrogenFoliageExported(stressNitrogenSenescence*(1-leafNRemobFrac)); 
				if (leavesResiduesSpreading==1)addNitrogenFoliageLitterUnderTree(stressNitrogenSenescence*(1-leafNRemobFrac)); 
				if (leavesResiduesSpreading==2)addNitrogenFoliageLitterAllPlot(stressNitrogenSenescence*(1-leafNRemobFrac));
				

			}
		}

	}

	/**
	 * Remove each day dead branches on tree to produce litter
	 */
	public void removeBranchesDeadOnTree (int yearIndex, int nbrDays, SafeGeneralParameters	safeSettings) {
		
		int branchesResiduesSpreading = safeSettings.branchesResiduesSpreading;	//0=exported 1=under tree 2=all plot
		
		double selfPruningNbrYearsForBranchesFullDecay = this.getTreeSpecies().getSelfPruningNbrYearsForBranchesFullDecay();

		for (int y=1; y<=selfPruningNbrYearsForBranchesFullDecay; y++){
			//remove carbon from dead branches
			if (carbonBranchesDeadOnTree.containsKey(y)) {
				double valeur = getCarbonBranchesDeadOnTree(y);
					if (y==1) 	{
						carbonBranchesDeadOnTree.remove(y);
						if (branchesResiduesSpreading==0) addCarbonBranchesExported(valeur); 
						if (branchesResiduesSpreading==1) addCarbonBranchesLitterUnderTree(valeur); 
						if (branchesResiduesSpreading==2) addCarbonBranchesLitterAllPlot(valeur); 
					}
					else {
				
						double senescence = ((valeur / y) / nbrDays);			
						valeur = valeur-senescence;			
						
						carbonBranchesDeadOnTree.remove(y);
						if (valeur>0) carbonBranchesDeadOnTree.put(y-1, valeur);
						if (branchesResiduesSpreading==0)addCarbonBranchesExported(senescence); 
						if (branchesResiduesSpreading==1)addCarbonBranchesLitterUnderTree(senescence); 
						if (branchesResiduesSpreading==2)addCarbonBranchesLitterAllPlot(senescence); 
						
					}
			}
			//remove nitrogen from dead branches
			if (nitrogenBranchesDeadOnTree.containsKey(y)) {
				double valeur = getNitrogenBranchesDeadOnTree(y);
					if (y==1) 	{
						nitrogenBranchesDeadOnTree.remove(y);
						if (branchesResiduesSpreading==0)addNitrogenBranchesExported(valeur); 
						if (branchesResiduesSpreading==1)addNitrogenBranchesLitterUnderTree(valeur); 
						if (branchesResiduesSpreading==2)addNitrogenBranchesLitterAllPlot(valeur); 
						
					}
					else {
				
						double senescence = ((valeur / y) / nbrDays);			
						valeur = valeur-senescence;			
						
						nitrogenBranchesDeadOnTree.remove(y);
						if (valeur>0) nitrogenBranchesDeadOnTree.put(y-1, valeur);
						if (branchesResiduesSpreading==0)addNitrogenBranchesExported(senescence);
						if (branchesResiduesSpreading==1)addNitrogenBranchesLitterUnderTree(senescence); 
						if (branchesResiduesSpreading==2)addNitrogenBranchesLitterAllPlot(senescence);
					}
			}
		}


		
	}
	/**
	 * Nitrogen stress calculation
	 */
	public double computeNitrogenStress () {
		//  Update N stress to be used for next daily time step
		//Stress occurs if demand is not satisfied..
		//  no limit to daily nitrogenLabile use
		
		if (getTotalCarbonFoliage () > 0)
			return   (Math.max (Math.min ((getTotalN() / getTotalOptiN()), 1), 0));
		else {
			return (1);

		}
	}
	
	/*==============================
	/*INTERVENTIONS
	/*=============================
	/**
	 * Pruning intervention effect calculation
	 * @author Kevin WOLZ - 16-08-2018
	 */	
	public void pruning (double proportion, 
							double maxheight, 
							int residusIncorporation,
							int residusSpreading) {

        double newPruningHeight = Math.min(this.getHeight() * proportion, maxheight);


        if (newPruningHeight > this.getCrownBaseHeight()) {

            //modify crown radii
            double redFrac = (this.getHeight() - newPruningHeight) / (this.getHeight() - this.getCrownBaseHeight());
            setCrownRadiusTreeLine(getCrownRadiusTreeLine() * redFrac);
            setCrownRadiusInterRow(getCrownRadiusInterRow() * redFrac);
            setCrownRadiusVertical(getCrownRadiusVertical() * redFrac);

            //reduce crown volume & increase base height
            double newVolume = 4.0 / 3.0 * Math.PI * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical();
            double volumeReductionFraction = newVolume / getCrownVolume();
            setCrownVolume(newVolume);
            setCrownBaseHeight(newPruningHeight);
            
            if (residusIncorporation==0) {
                //carbon and nitrogen exported from the plot (branches and leaves are removed)
                addCarbonFoliageExported    (getTotalCarbonFoliage ()    * (1 - volumeReductionFraction));
                addCarbonBranchesExported   (getCarbonBranches ()   * (1 - volumeReductionFraction));
                addNitrogenFoliageExported  (getTotalNitrogenFoliage ()  * (1 - volumeReductionFraction));
                addNitrogenBranchesExported (getNitrogenBranches () * (1 - volumeReductionFraction));
            }
            else {
                //carbon and nitrogen lft on the plot (branches and leaves will be transformed as litter)           	
                addCarbonBranchesSen  (getCarbonBranches ()   * (1 - volumeReductionFraction));
                addCarbonFoliageSen    (getTotalCarbonFoliage ()    * (1 - volumeReductionFraction));
                addNitrogenBranchesSen (getNitrogenBranches () * (1 - volumeReductionFraction)); 
                addNitrogenFoliageSen  (getTotalNitrogenFoliage ()  * (1 - volumeReductionFraction));
                
				if (residusSpreading==1) {
					addCarbonBranchesLitterUnderTree(getCarbonBranches ()   * (1 - volumeReductionFraction)); 
					addNitrogenBranchesLitterUnderTree(getNitrogenBranches ()   * (1 - volumeReductionFraction)); 
					addCarbonFoliageLitterUnderTree(getTotalCarbonFoliage ()   * (1 - volumeReductionFraction)); 
					addNitrogenFoliageLitterUnderTree(getTotalNitrogenFoliage ()   * (1 - volumeReductionFraction)); 
				}
				if (residusSpreading==2){
					addCarbonBranchesLitterAllPlot(getCarbonBranches ()   * (1 - volumeReductionFraction));
					addNitrogenBranchesLitterAllPlot(getNitrogenBranches ()   * (1 - volumeReductionFraction)); 
					addCarbonFoliageLitterAllPlot(getTotalCarbonFoliage ()    * (1 - volumeReductionFraction));
					addNitrogenFoliageLitterAllPlot(getTotalNitrogenFoliage ()  * (1 - volumeReductionFraction)); 
				}

            }

            
            //reduce carbon and nitrogen pool
            //reduce leaf area for each cohort
            for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
            	 setLeafArea       (getLeafAreaCohort(index) * volumeReductionFraction, index);
            	 setCarbonFoliage  (getCarbonFoliageCohort(index) * volumeReductionFraction, index);
            	 setNitrogenFoliage(getNitrogenFoliageCohort(index) * volumeReductionFraction, index);
            }         
            setCarbonBranches   (getCarbonBranches ()   * volumeReductionFraction);
            setNitrogenBranches (getNitrogenBranches () * volumeReductionFraction);

        }

	}
	

	
	/**
	 * SELF Pruning intervention
	 * @author Christian DUPRAZ - 22-11-2022
	 * https://github.com/hisafe/hisafe/issues/138
	 */	
	public void selfPruning(int year) {


		if (this.getLightCompetitionIndex() < this.getTreeSpecies().getSelfPruningLCIThreshold()) {
			addNbrDaysInShade();
		}
		if (this.getNbrDaysInShade() > this.getTreeSpecies().getSelfPruningNbrDaysShade()) {

			setNbrDaysInShade(0);	
			
			double newBaseHeight = this.getHeight() * this.getTreeSpecies().getSelfPruningHeightRatio();

	        if (newBaseHeight > this.getCrownBaseHeight()) {
	    		
		        //modify crown radii
		        double redFrac = (this.getHeight() - newBaseHeight) / (this.getHeight() - this.getCrownBaseHeight());
		        setCrownRadiusTreeLine(getCrownRadiusTreeLine() * redFrac);
		        setCrownRadiusInterRow(getCrownRadiusInterRow() * redFrac);
		        setCrownRadiusVertical(getCrownRadiusVertical() * redFrac);
		
		        //reduce crown volume & increase base height
		        double newVolume = 4.0 / 3.0 * Math.PI * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical();
		        double volumeReductionFraction = newVolume / getCrownVolume();
		        setCrownVolume(newVolume);
		        setCrownBaseHeight(newBaseHeight);
		              
		        //carbon and nitrogen dead on tree
				int selfPruningNbrYearsForBranchesFullDecay = this.getTreeSpecies().getSelfPruningNbrYearsForBranchesFullDecay();
		        addCarbonBranchesDeadOnTree   (selfPruningNbrYearsForBranchesFullDecay, getCarbonBranches ()   * (1 - volumeReductionFraction));
		        addNitrogenBranchesDeadOnTree   (selfPruningNbrYearsForBranchesFullDecay, getNitrogenBranches ()   * (1 - volumeReductionFraction));
		        
		        
		        setCarbonBranches   (getCarbonBranches ()   * volumeReductionFraction);
		        setNitrogenBranches (getNitrogenBranches () * volumeReductionFraction);
	        }
		}
	}

	
	

	/**
	 *  Leaf area reduction intervention
	 * @author Christian DUPRAZ
	 */
	public void leafAreaDensityReduction (double leafAreaDensityThreshold, 
											double leafAreaDensityReductionFraction,
											int residusIncorporation,
											int residusSpreading) {
		
		if (this.getLeafAreaDensity()>0 && this.getLeafAreaDensity() > leafAreaDensityThreshold) {

			//calculate exportation or senescence 
			if (residusIncorporation==0) {
				addCarbonFoliageExported   (getTotalCarbonFoliage ()    * leafAreaDensityReductionFraction);
				addNitrogenFoliageExported   (getTotalNitrogenFoliage ()    * leafAreaDensityReductionFraction);
				addCarbonBranchesExported    (getCarbonBranches ()    * leafAreaDensityReductionFraction);
				addNitrogenBranchesExported   (getNitrogenBranches ()    * leafAreaDensityReductionFraction);   
			}
			else  {
                addCarbonFoliageSen   (getTotalCarbonFoliage ()    * leafAreaDensityReductionFraction);
                addNitrogenFoliageSen   (getTotalNitrogenFoliage ()    * leafAreaDensityReductionFraction);
	            addCarbonBranchesSen    (getCarbonBranches ()    * leafAreaDensityReductionFraction);
	            addNitrogenBranchesSen    (getNitrogenBranches ()    * leafAreaDensityReductionFraction); 
	            
				if (residusSpreading==1) {
					addCarbonBranchesLitterUnderTree(getCarbonBranches ()   * leafAreaDensityReductionFraction); 
					addNitrogenBranchesLitterUnderTree(getNitrogenBranches ()   * leafAreaDensityReductionFraction); 
					addCarbonFoliageLitterUnderTree(getTotalCarbonFoliage ()   * leafAreaDensityReductionFraction); 
					addNitrogenFoliageLitterUnderTree(getTotalNitrogenFoliage ()   * leafAreaDensityReductionFraction); 
				}
				if (residusSpreading==2){
					addCarbonBranchesLitterAllPlot(getCarbonBranches ()   * leafAreaDensityReductionFraction);
					addNitrogenBranchesLitterAllPlot(getNitrogenBranches ()   * leafAreaDensityReductionFraction); 
					addCarbonFoliageLitterAllPlot(getTotalCarbonFoliage ()    * leafAreaDensityReductionFraction);
					addNitrogenFoliageLitterAllPlot(getTotalNitrogenFoliage ()  * leafAreaDensityReductionFraction); 
				}
				
			}
   

			//reduce foliage and branches
			for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
				if (this.getLeafAreaCohort(index)>0) { 	
					setLeafArea(this.getLeafAreaCohort(index)* (1-leafAreaDensityReductionFraction), index);
					setCarbonFoliage    (getCarbonFoliageCohort (index)  * (1-leafAreaDensityReductionFraction), index);
	                setNitrogenFoliage  (getNitrogenFoliageCohort (index)* (1-leafAreaDensityReductionFraction), index);    
				}
			}
            setCarbonBranches (getCarbonBranches() * (1-leafAreaDensityReductionFraction));
            setNitrogenBranches (getNitrogenBranches() * (1-leafAreaDensityReductionFraction));
		}
	}
	
	

	/**
	 * Crown trimming intervention
	 * @author Christian DUPRAZ
	 */
	public void canopyTrimming (double treeLineTrigger, double treeLineReductionTarget, 
								double interRowTrigger, double interRowReductionTarget,
								int residusIncorporation, int residusSpreading) {
		
		//only before budburst
		boolean prune = false; 
		if (this.getPhenologicalStage()==0) {
			
			if (treeLineTrigger >0 && this.getCrownRadiusTreeLine() >= treeLineTrigger) {
				if (treeLineReductionTarget>0) this.setCrownRadiusTreeLine(treeLineReductionTarget);
				prune = true; 
			}
			
			if (interRowTrigger>0 && this.getCrownRadiusInterRow() >= interRowTrigger) {
				if (interRowReductionTarget>0) this.setCrownRadiusInterRow(interRowReductionTarget);
				prune = true; 
			}
			
			if (prune) {

				 //reduce crown volume & increase base height
	            double newVolume = 4.0 / 3.0 * Math.PI * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical();
	            double crownFactor = newVolume / getCrownVolume();
	            setCrownVolume(newVolume);

	    		//calculate exportation or senescence 
            	if (residusIncorporation==0) {
            		addCarbonFoliageExported   (getTotalCarbonFoliage ()    * (1 - crownFactor));
	                addNitrogenFoliageExported   (getTotalNitrogenFoliage ()    * (1 - crownFactor));
		            addCarbonBranchesExported   (getCarbonBranches ()   * (1 - crownFactor));
		            addNitrogenBranchesExported (getNitrogenBranches () * (1 - crownFactor));
            	}
            	else {
	                addCarbonFoliageSen   (getTotalCarbonFoliage ()    * (1 - crownFactor));
	                addNitrogenFoliageSen   (getTotalNitrogenFoliage ()    * (1 - crownFactor));
		            addCarbonBranchesSen   (getCarbonBranches ()   * (1 - crownFactor));
		            addNitrogenBranchesSen(getNitrogenBranches () * (1 - crownFactor));
 
					if (residusSpreading==1) {
						addCarbonBranchesLitterUnderTree(getCarbonBranches ()   * (1 - crownFactor)); 
						addNitrogenBranchesLitterUnderTree(getNitrogenBranches ()   * (1 - crownFactor)); 
						addCarbonFoliageLitterUnderTree(getTotalCarbonFoliage ()   * (1 - crownFactor)); 
						addNitrogenFoliageLitterUnderTree(getTotalNitrogenFoliage ()   * (1 - crownFactor)); 
					}
					if (residusSpreading==2){
						addCarbonBranchesLitterAllPlot(getCarbonBranches ()   * (1 - crownFactor));
						addNitrogenBranchesLitterAllPlot(getNitrogenBranches ()   * (1 - crownFactor)); 
						addCarbonFoliageLitterAllPlot(getTotalCarbonFoliage ()    * (1 - crownFactor));
						addNitrogenFoliageLitterAllPlot(getTotalNitrogenFoliage ()  * (1 - crownFactor)); 
					}
					
            	}

	        	//reduce foliage and branches
	            for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
	            	setLeafArea(this.getLeafAreaCohort(index)* crownFactor, index);
					setCarbonFoliage    (getTotalCarbonFoliage ()  * crownFactor, index);
	                setNitrogenFoliage  (getTotalNitrogenFoliage ()* crownFactor, index);  
		            setCarbonBranches   (getCarbonBranches ()   * crownFactor);
		            setNitrogenBranches (getNitrogenBranches () * crownFactor);
	            }
	            
			}
		}

	}
	

	/**
	 * Topping intervention
	 * @author Christian DUPRAZ (12-10-2021)
	 */		
	public void toping (SafeStand stand, double newHeight, int residusIncorporation, int residusSpreading) {
	
		double truncationRatio 	 = this.getTreeSpecies ().getEllipsoidTruncationRatio ();
		int crownShape 			 = this.getTreeSpecies ().getCrownShape ();
		double heightBefore 	= this.getHeight(); 
		double crownVolumeBefore = this.getCrownVolume(); 


		//height is updated 
		setHeight(newHeight);

		//Volume of cone before toping
		double coneVolumeBefore = 1d/3d  * Math.PI  * heightBefore * (Math.pow(getDBaseMeters(), 2)/4d);

		double dhet = getDbhMeters() * ((heightBefore - newHeight) / (heightBefore-(1.3)));

		double coneVolumeAfter = 1d/3d  * Math.PI  * newHeight * 
										((Math.pow(dhet, 2)/4d)
							              + ((dhet * getDBaseMeters())/4d)
							              + (Math.pow(getDBaseMeters(), 2)/4d));
		
		double coneFactor = coneVolumeAfter/coneVolumeBefore;


		//Update crow shape (vertical dimension) 		
		if (crownShape == 1) {	//ellipsoid
			double newCrownRadiusVertical = ((newHeight-getCrownBaseHeight())/(1-truncationRatio))/2;
			double newCrownRadiusTreeLine = getCrownRadiusTreeLine();
			double newCrownRadiusInterRow = getCrownRadiusInterRow();

			if (newHeight>getCrownBaseHeight()) {
				//on ne touche pas au rayons horizontaux 
				if (newHeight > (getCrownBaseHeight()+(heightBefore-getCrownBaseHeight())/2)
				) {
				}
				//on reduit les rayons horizontaux proportionnellement au rayon vertical  
				else {				
					newCrownRadiusTreeLine = 2*getCrownRadiusTreeLine()* newCrownRadiusVertical/getCrownRadiusVertical();
					newCrownRadiusInterRow = 2*getCrownRadiusInterRow()* newCrownRadiusVertical/getCrownRadiusVertical();			
				}
			}
			//on recree une petite couronne de diametre dhet+20cm
			//la hauteur de cette couronne est aussi recalcule
			else {
				newCrownRadiusVertical = (dhet/0.2d)+0.2d;
				newCrownRadiusTreeLine = (dhet/0.2d)+0.2d;	
				newCrownRadiusInterRow = (dhet/0.2d)+0.2d;	
				setCrownBaseHeight(2*((dhet/0.2d)+0.2d));				
			}
			

			setCrownRadiusVertical(newCrownRadiusVertical);
			setCrownRadiusTreeLine(newCrownRadiusTreeLine);
			setCrownRadiusInterRow(newCrownRadiusInterRow);
			
			setCrownVolume (Math.PI  * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical() *
					(2 * (1-truncationRatio) - (1d/3d * ( 1 - Math.pow (2*truncationRatio-1,3)))));
			
		}


		
		//Update STEM VOLUME, carbon and nitrogen stem pool 
		setStemVolume (getStemVolume() * coneFactor);	
		
		double crownFactor = getCrownVolume()/crownVolumeBefore;
		
		
		if (residusIncorporation==0) {
			addCarbonStemExported (getCarbonStem () * (1-coneFactor));
			addNitrogenStemExported (getNitrogenStem() * (1-coneFactor));
			
    		addCarbonFoliageExported   (getTotalCarbonFoliage ()    * (1 - crownFactor));
            addNitrogenFoliageExported   (getTotalNitrogenFoliage ()    * (1 - crownFactor));
            
			addCarbonBranchesExported (getCarbonBranches() * (1 - crownFactor));
			addNitrogenBranchesExported (getNitrogenBranches() * (1 - crownFactor));	
			
            
		}
		else {
			//on ne laisse pas le tronc sur place 
			addCarbonStemExported (getCarbonStem () * (1-coneFactor));
			addNitrogenStemExported (getNitrogenStem() * (1-coneFactor));
			
            addCarbonFoliageSen   (getTotalCarbonFoliage () * (1 - crownFactor));
            addNitrogenFoliageSen   (getTotalNitrogenFoliage () * (1 - crownFactor));
            
			addCarbonBranchesSen (getCarbonBranches () * (1 - crownFactor));
			addNitrogenBranchesSen (getNitrogenBranches() * (1 - crownFactor));
			
			
			if (residusSpreading==1) {
				addCarbonBranchesLitterUnderTree(getCarbonBranches ()   * (1 - crownFactor)); 
				addNitrogenBranchesLitterUnderTree(getNitrogenBranches ()   * (1 - crownFactor)); 
				addCarbonFoliageLitterUnderTree(getTotalCarbonFoliage ()   * (1 - crownFactor)); 
				addNitrogenFoliageLitterUnderTree(getTotalNitrogenFoliage ()   * (1 - crownFactor)); 
			}
			if (residusSpreading==2){
				addCarbonBranchesLitterAllPlot(getCarbonBranches ()   * (1 - crownFactor));
				addNitrogenBranchesLitterAllPlot(getNitrogenBranches ()   * (1 - crownFactor)); 
				addCarbonFoliageLitterAllPlot(getTotalCarbonFoliage ()    * (1 - crownFactor));
				addNitrogenFoliageLitterAllPlot(getTotalNitrogenFoliage ()  * (1 - crownFactor)); 
			}
			
			
		}


    	//reduce stem branches an foliage
		setCarbonStem (getCarbonStem () * coneFactor);	
		setNitrogenStem (getNitrogenStem() * coneFactor);
		
		setCarbonBranches(getCarbonBranches () * crownFactor);
		setNitrogenBranches(getNitrogenBranches () * crownFactor);
		
        for (int index = 0; index < this.getTreeSpecies().getNbCohortMax(); index++) {
        	setLeafArea(this.getLeafAreaCohort(index)* crownFactor , index);
			setCarbonFoliage    (getCarbonFoliageCohort (index)  * crownFactor, index);
            setNitrogenFoliage  (getNitrogenFoliageCohort (index)* crownFactor, index); 
        } 
		
		setTopped(true);

	}
	
	/**
	 * FRUIT Thining  intervention 
	 * @author Nicolas BARBAULT
	 */	
	public void fruitAutoThinning (int day, 
									boolean debug, 
									int lastDelay, 
									double fruitOptimalLoadLeafArea, 
									int residusIncorporation, int residusSpreading) {
		
		if (this.getFruitPhenologicalStage()>=3) {
			int fruitDelay = this.getFruitSettingDate();
			fruitDelay = fruitDelay + lastDelay;
			
			//tree is ready for thinning (test fruit setting > delay) 
			if (day == fruitDelay ){
				if (debug) StatusDispatcher.print("FRUIT THINNING AUTO Tree ID=" + this.getId());
				fruitSimpleThinning (fruitOptimalLoadLeafArea, 0, residusIncorporation, residusSpreading);
			}
			
		}
	}
	public void fruitSimpleThinning (double fruitOptimalLoadLeafArea, 
									int nbrFruitTarget, 
									int residusIncorporation, int residusSpreading) {
		
		//if fruitThinningMethod == 3 nbrFruitTarget is provided
		//else it has to be calculated : number of fruit by leaf area > optimal
		if (nbrFruitTarget == 0) {
			if (this.getFruitNbr() / this.getTotalLeafArea() > fruitOptimalLoadLeafArea) {
				nbrFruitTarget = (int) (fruitOptimalLoadLeafArea * this.getTotalLeafArea());
			}
		}
		//fruit reduction
		if (nbrFruitTarget > 0 && nbrFruitTarget < this.getFruitNbr())  {

			this.setFruitThinning(this.getFruitNbr() - nbrFruitTarget);

			//carbon reduction to do before fruit reduction
			double carbonOneFruit = getCarbonFruit()/getFruitNbr();
			double nitrogenOneFruit = getNitrogenFruit()/getFruitNbr();
			
			if (residusIncorporation==0) {
				addCarbonFruitExported(carbonOneFruit*getFruitThinning());
				addNitrogenFruitExported(nitrogenOneFruit*getFruitThinning());
			}else {
				addCarbonFruitSen(carbonOneFruit*getFruitThinning());
				addNitrogenFruitSen(nitrogenOneFruit*getFruitThinning());
				
				if (residusSpreading==1) {
					addCarbonFruitLitterUnderTree(carbonOneFruit*getFruitThinning()); 
					addNitrogenFruitLitterUnderTree(nitrogenOneFruit*getFruitThinning()); 

				}
				if (residusSpreading==2){
					addCarbonFruitLitterAllPlot(carbonOneFruit*getFruitThinning());
					addNitrogenFruitLitterAllPlot(nitrogenOneFruit*getFruitThinning()); 

				}
	
			}

			setCarbonFruit(getCarbonFruit()-(carbonOneFruit*getFruitThinning()));	
			setNitrogenFruit(getNitrogenFruit()-(nitrogenOneFruit*getFruitThinning()));

			this.setFruitNbr(nbrFruitTarget);
		}
	
	}
	
	/**
	 * ROOT Pruning intervention 
	 * @author Christian DUPRAZ
	 */	
	public void rootPruning (SafeStand stand, SafeGeneralParameters ip, double rootPruningDistance, double rootPruningDepth) {

		double cellWidth = stand.getPlot().getCellWidth();
		double xPruningLeft = this.getX()-rootPruningDistance;
		double xPruningRight= this.getX()+rootPruningDistance;

		//searching cells at the right root pruning distance
		for(Iterator it = stand.getPlot().getCells().iterator();it.hasNext();) { 
		  SafeCell c = (SafeCell) it.next(); 
		  
		  //no root pruning in the cell where the tree is planted
		  if (this.getCell().getId() != c.getId()) {
			  
			  double cellXMin = c.getX();
			  double cellXMax = c.getX()+cellWidth;
	 
			  //check if distance is respected
			  if ( ((cellXMin < xPruningLeft) && (cellXMax > xPruningLeft))  ||  ((cellXMin < xPruningRight) && (cellXMax > xPruningRight))					  
					 || (cellXMax == xPruningLeft)  || (cellXMin == xPruningRight)) {
	
				SafeVoxel voxels [] =  c.getVoxels(); 
	
				for (int i=0; i < voxels.length; i++) {
				  if (voxels[i].getSurfaceDepth() < rootPruningDepth) {
	
					  if (getPlantRoots().getRootTopology(voxels[i]) != null) {
						  if(getPlantRoots().getRootTopology(voxels[i]).getNodeParent() != null) 		{ 
							  
								float removedProp = 0;
								boolean testAnoxia = false;
								if (voxels[i].getZ() < rootPruningDepth) 
									removedProp = 1;
								else 
									removedProp = (float) ((rootPruningDepth-voxels[i].getSurfaceDepth()) / voxels[i].getThickness());
	
								getPlantRoots().getRootTopology(voxels[i]).getNodeParent().removeSonsRoots(voxels[i], this, ip, removedProp, testAnoxia, stand.getPlot().getSoil().getHumificationDepth()); 

						  }
					  }
				  }
				}
			  } 
		  }
		} 
	}	
	
	
	/**
	 * Tree  irrigation intervention
	 * @author Nicolas BARBAULT (09-11-2022)
	 **/
	public void treeIrrigation(SafePlot plot, double treeIrrigationDose, int julianDay) {
		
	
		
		 List<SafeCell>  irrigatedCell =  new ArrayList<SafeCell>();
		 
		 this.nbrDaysSinceLastIrrigation = 0;
		 

		//drip or aspersion
		for (int i = 0; i < this.treeItk.treeIrrigationDriporSprinklerX.size(); i++) {

			double maxX  = this.treeItk.treeIrrigationDriporSprinklerX.get(i)+this.treeItk.treeIrrigationRadius;
			double minX  = this.treeItk.treeIrrigationDriporSprinklerX.get(i)-this.treeItk.treeIrrigationRadius;
			double maxY  = this.treeItk.treeIrrigationDriporSprinklerY.get(i)+this.treeItk.treeIrrigationRadius;
			double minY  = this.treeItk.treeIrrigationDriporSprinklerY.get(i)-this.treeItk.treeIrrigationRadius;

			if (maxX > plot.getXSize()) maxX=plot.getXSize();
			if (minX < 0) minX = 0;
			if (maxY > plot.getYSize()) maxY = plot.getYSize();
			if (minY < 0) minY = 0;

			
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				boolean okx = false;
				boolean oky = false;
				
				//check cellX
				if (minX < maxX) {
					if ((cell.getX()+cell.getWidth() > minX)
					&&  (cell.getX() < maxX)) {
						okx = true;
					}
				}
				//toric symetry effect on X axis
				else {
					if ((cell.getX() < maxX)
						|| (cell.getX()+cell.getWidth() > minX)) {
							okx = true;
						}
				}

				//check cellY
				if (minY < maxY) {
					if ((cell.getY()+cell.getWidth() > minY)
					&&  (cell.getY() < maxY)) {
						oky = true;
					}
				}
				//toric symetry effect on Y axis
				else {
					if ((cell.getY() < maxY)
						|| (cell.getY()+cell.getWidth() > minY)) {
							oky = true;
						}
				}
				
				if (okx && oky) {
					irrigatedCell.add(cell);
				}
			}
		}
		
		//irrigation on each irrigated cell
		for (Iterator c = irrigatedCell.iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			cell.treeIrrigation(julianDay, this.treeItk.treeIrrigationMethod, treeIrrigationDose);
		}	
		
		 this.nbrDaysSinceLastIrrigation = 0;
	}
	/**
	 * Tree  fertilization intervention
	 * @author Nicolas BARBAULT (09-11-2022)
	 **/
	public void treeFertilization(SafeEvolutionParameters ep, SafePlot plot, double treeFertilizationDose, int treeFertilizerCode, int julianDay) {
		
		List<SafeCell>  fertilizedCell =  new ArrayList<SafeCell>();

		this.nbrDaysSinceLastFertilization = 0;

		double maxX   = this.getX()+this.treeItk.treeFertilizationRadius;
		if (maxX > plot.getXSize()) {					
			if (ep.toricXp > 0) maxX = maxX - plot.getXSize();
			else maxX=plot.getXSize();
		}
		double minX    = this.getX()-this.treeItk.treeFertilizationRadius;
		if (minX < 0) {
			if (ep.toricXn > 0) minX = minX + plot.getXSize();
			else minX = 0;
		}

		double maxY   = this.getY()+this.treeItk.treeFertilizationRadius;
		if (maxY > plot.getYSize()) {
			if (ep.toricYp > 0) maxY = maxY - plot.getYSize();
			else maxY = plot.getYSize();
		}
		double minY    = this.getY()-this.treeItk.treeFertilizationRadius;
		if (minY < 0) {
			if (ep.toricYn > 0) minY = minY + plot.getYSize();
			else minY = 0;
		}
		
		for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			boolean okx = false;
			boolean oky = false;
			
			//check cellX
			if (minX < maxX) {
				if ((cell.getX()+cell.getWidth() > minX)
				&&  (cell.getX() < maxX)) {
					okx = true;
				}
			}
			//toric symetry effect on X axis
			else {
				if ((cell.getX() < maxX)
					|| (cell.getX()+cell.getWidth() > minX)) {
						okx = true;
					}
			}

			//check cellY
			if (minY < maxY) {
				if ((cell.getY()+cell.getWidth() > minY)
				&&  (cell.getY() < maxY)) {
					oky = true;
				}
			}
			//toric symetry effect on Y axis
			else {
				if ((cell.getY() < maxY)
					|| (cell.getY()+cell.getWidth() > minY)) {
						oky = true;
					}
			}
			
			if (okx && oky) {
				fertilizedCell.add(cell);
			}
		}

		
		//irrigation on each irrigated cell
		for (Iterator c = fertilizedCell.iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			cell.treeFertilization(julianDay, treeFertilizationDose, treeFertilizerCode);
		}
		
	}

	
	/**
	 * Return new crown radius depending the plot size limitation
	 **/
	public double [] computeRadiiDeformation (double crownArea, SafeStand stand)  {
	// Return the 2 radii of the crown when given spacing
	// The first return value is the withinrow radius (Y axes)
		double meanRadius = Math.pow(crownArea/Math.PI,0.5);
		double[] newRadii = new double[2];		//return values
		double xLimit = 0;
		double yLimit = 0;

		//FREE planting
		//Limitation of crown expansion is plot size
		xLimit = ((SafePlot) stand.getPlot()).getXSize();
		yLimit = ((SafePlot) stand.getPlot()).getYSize();

		double distanceMin = Math.min (xLimit, yLimit);
		double distanceMax = Math.max (xLimit, yLimit);

		//Radius is still under limits
		if ((meanRadius * 2) <=  distanceMin) {
			newRadii [0] = meanRadius;
			newRadii [1] = meanRadius;
		}
		//Radius has to be limited
		else {
			newRadii [0] = distanceMin / 2;
			newRadii [1] = Math.min( Math.pow (meanRadius,2) / newRadii [0] , distanceMax/2);
		}

		//Results inversion in case of unusual planting limits
		//This case can happend after  thinning for exemple
		if (xLimit < yLimit) {
			double temp = newRadii [0];
			newRadii [0] = newRadii [1];
			newRadii [1] = temp;
		}

		return newRadii;
	}

	/**
	* Reset energy for the tree
	*/
	public void resetDirect () {
		this.setCaptureFactorForDirectPar(0);
		this.setCaptureFactorForDirectNir(0);
		this.setCFdirectLonelyTree(0);
	}

	public void resetDiffuse  () {
		this.setCaptureFactorForDiffusePar(0);
		this.setCaptureFactorForInfraRed(0);
		this.setCaptureFactorForDiffuseNir(0);
		this.setCFdiffuseLonelyTree(0);
	}


	/**
	* Add energy for the tree
	*/
	public void addDiffuse (double e) {
		captureFactorForDiffusePar +=  e;
	}
	public void addDirect (double e) {
		captureFactorForDirectPar +=  e;
	}
	public void addInfraRed (double e) {
		captureFactorForInfraRed +=  e;
	}
	public void addDirectNir (double e) {
		captureFactorForDirectNir +=  e;
	}
	public void addDiffuseNir (double e) {
		captureFactorForDiffuseNir +=  e;
	}
	
	
	public void addDirectToLonelyTree (double e) {
		cFdirectLonelyTree += e;
	}
	public void addDiffuseToLonelyTree (double e) {
		cFdiffuseLonelyTree +=  e;
	}
	/**
	 * Daily calculation of PAR intercepted by a tree depending climatic entries
	 */
	public void updateDailyLightResults (SafeDailyClimat dayClimat ,
										SafeEvolutionParameters simulationSettings,
										SafeGeneralParameters settings) {	//GT 2007

		float dailyDirect = dayClimat.getDirectPar ();				//Moles m-2 d-1
		float dailyDiffuse = dayClimat.getDiffusePar ();			//Moles m-2 d-1

		setLightCompetitionIndex(1); 
		
		//Diffuse PAR intercepted by the tree
		setDiffuseParIntercepted (getCaptureFactorForDiffusePar() * dailyDiffuse);	//m2 * Moles m-2 d-1= Moles d-1



		
		//Direct PAR intercepted by the tree
		setDirectParIntercepted (getCaptureFactorForDirectPar() * dailyDirect);	//m2 * Moles m-2 d-1= Moles d-1

		//Global radiation intercepted by the tree
		float parProp =(float) settings.parGlobalCoefficient;
		float directProp = dailyDirect/(dailyDirect+dailyDiffuse);
		float dailyGlobal = dayClimat.getGlobalRadiation();
		setGlobalRadIntercepted(dailyGlobal*(
							(captureFactorForDiffuseNir*(1-parProp)+
							captureFactorForDiffusePar*parProp) * (1-directProp)+
							(captureFactorForDirectNir*(1-parProp)+
							captureFactorForDirectPar*parProp) * (directProp)
							));


		
		//Computation of the competition index
		if((directParIntercepted+diffuseParIntercepted)>0){		// if there is Par interception
			setLightCompetitionIndex((directParIntercepted+diffuseParIntercepted)
					/(dailyDirect*cFdirectLonelyTree + dailyDiffuse*cFdiffuseLonelyTree));
		}

		//InfraRed Radiation intercepted
		float dailyInfraRed = dayClimat.getInfraRedRadiation();
		setInfraRedIntercepted(captureFactorForInfraRed*dailyInfraRed);

	}

	
	//TREE SPECIES
	public SafeTreeSpecies getTreeSpecies () {return treeSpecies ;}


	public Species getSpecies () {return treeSpecies;}
	
	//ITK
	public SafeTreeItk getTreeItk() {return treeItk;}
	public void setTreeItk(SafeTreeItk itk) {treeItk = itk;}

		//********** FOR EXPORT EXTENSION !!!!!!!!!!!!!!*****************/
		//  HAVE TO BE DONE FOR EACH NEW TABLE //
		//****************************************************************/
		public String getLeafAreaType () {
			return "Double";
		}
		public String getLeafAgeType () {
			return "Int";
		}	
		public String getLeafLueType () {
			return "Double";
		}		
		public String getCarbonFoliageType () {
			return "Double";
		}
		public String getNitrogenFoliageType () {
			return "Double";
		}


		public String getCarbonIncrementType () {
			return "Double";
		}		
		public int getLeafAreaSize () {
			return this.getTreeSpecies().getNbCohortMax();
		}

		public int getLeafAgeSize () {
			return this.getTreeSpecies().getNbCohortMax();
		}
		
		public int getLeafLueSize () {
			return this.getTreeSpecies().getNbCohortMax();
		}
		
		public int getCarbonFoliageSize () {
			return this.getTreeSpecies().getNbCohortMax();
		}
		
		public int getNitrogenFoliageSize () {
			return this.getTreeSpecies().getNbCohortMax();
		}
		


		
		public int getCarbonIncrementSize () {
			return this.getTreeSpecies().getNbCohortMax();
		}		

		
		public int getCrownType () {
			return SimpleCrownDescription.SPHERIC;
		}
		public Color getCrownColor () {
			return Color.green;
		}
		public float getTransparency () {
			return 0;
		}// 0.0 (opaque) to 1.0 (transparent)


		//VEGETATIVE PHENOLOGY
		public void setPlanted(boolean b) {planted = b;}
		public void setPhenologicalStage (int p) {phenologicalStage = p;}
		public int	getPhenologicalStage() {return phenologicalStage;} 

		public void setFirstYearStarted(boolean b) {firstYearStarted = b;}
		public void setBudburstDate (int d) {budburstDate = d;}
		public void setLeafExpansionEndingDate (int d) {leafExpansionEndingDate = d;}
		public void setLeafFallStartingDate (int d) {leafFallStartingDate = d;}
		public void setLeafFallEndingDate (int d) {leafFallEndingDate = d;}
		public void setBudburstAccumulatedTemperature (double d) {budburstAccumulatedTemperature=d;}
		public void setBudburstAccumulatedColdTemperature (double d) {budburstAccumulatedColdTemperature=d;}
		public void setHeatAccumulatedTemperature (double d) {heatAccumulatedTemperature=d;}
		public void setBudburstAccumulatedTemperatureStarted (boolean d) {budburstAccumulatedTemperatureStarted=d;}
		public void setBudburstAccumulatedColdTemperatureStarted (boolean d) {budburstAccumulatedColdTemperatureStarted=d;}
		public void setHeatAccumulatedTemperatureStarted (boolean d) {heatAccumulatedTemperatureStarted=d;}
		
		public double getBudburstAccumulatedTemperature () {return budburstAccumulatedTemperature;}
		public double getBudburstAccumulatedColdTemperature () {return budburstAccumulatedColdTemperature;}
		public boolean getHeatAccumulatedTemperatureStarted () {return heatAccumulatedTemperatureStarted;}
		public boolean getBudburstAccumulatedTemperatureStarted () {return budburstAccumulatedTemperatureStarted;}
		public boolean getBudburstAccumulatedColdTemperatureStarted () {return budburstAccumulatedColdTemperatureStarted;}
		public double getHeatAccumulatedTemperature () {return heatAccumulatedTemperature;}
		public boolean isFirstYearStarted () {return firstYearStarted;}
		public boolean isPlanted () {return planted;}
		public boolean getPlanted() {return planted;}
		
		public int getPlantingYear () {return plantingYear;}
		public int getPlantingDay  () {return plantingDay;}
		
		public int getBudburstDate () {return budburstDate;}
		public int getLeafExpansionEndingDate () {return leafExpansionEndingDate;}
		public int getLeafFallStartingDate () {return leafFallStartingDate;}
		public int getLeafFallEndingDate () {return leafFallEndingDate;}	
		
		public void setTopped(boolean b) {isTopped = b;}
		public boolean isTopped () {return isTopped;}
		
		//HARVEST
		public void setHarvested(boolean b) {harvested = b;}
		public boolean isHarvested() {return harvested;}
		public boolean getHarvested() {return harvested;}
		public void setHarvestingYear (int year) {harvestingYear = year;}
		public void setHarvestingDay  (int day) {harvestingDay = day;}
		public int getHarvestingYear () {return harvestingYear;}
		public int getHarvestingDay  () {return harvestingDay;}
		
		
		//FRUIT MODULE
		public void setFruitPhenologicalStage (int p) {fruitPhenologicalStage = p;}
		public int	getFruitPhenologicalStage() {return fruitPhenologicalStage;}
		public int getFloweringDate () {return floweringDate;}
		public int getFruitSettingDate () {return fruitSettingDate;}
		public int getFruitGrowthDate () {return fruitGrowthDate;}
		public int getVeraisonDate () {return veraisonDate;}
		public int getFloweringDuration () {return floweringDuration;}
		public int getFruitHarvestDate() {return fruitHarvestDate;}
		
		public void setFloweringDate (int d) {floweringDate = d;}
		public void setFruitSettingDate(int d) {fruitSettingDate = d;}
		public void setFruitGrowthDate (int d) {fruitGrowthDate = d;}
		public void setVeraisonDate  (int d) {veraisonDate = d;}
		public void setFloweringDuration  (int d) {floweringDuration = d;}
		public void addFloweringDuration  (int d) {floweringDuration += d;}
		public void setFruitHarvestDate (int d) {fruitHarvestDate = d;}


		

		public void	setFruitCarbonStressIndex(double d) {fruitCarbonStressIndex = d;} 
		public double getFruitCarbonStressIndex() {return fruitCarbonStressIndex;}

		public void	setLeafFrostStress(double d) {leafFrostStress = d;} 
		public double getLeafFrostStress() {return leafFrostStress;}
		
		
		public void	setLeafAgeStress(double d) {leafAgeStress = d;} 
		public double getLeafAgeStress() {return leafAgeStress;}
		
		public void	setLeafWaterNitrogenStress(double d) {leafWaterNitrogenStress = d;} 
		public double getLeafWaterNitrogenStress() {return leafWaterNitrogenStress;}
		
		
		//BNF MODULE
		public void setBnfPhenologicalStage (int p) {bnfPhenologicalStage = p;}

		public int	getBnfPhenologicalStage() {return bnfPhenologicalStage;}
		public int getBnfStartDate () {return bnfStartDate;}
		public int getBnfSteadyStateDate () {return bnfSteadyStateDate;}
		public int getBnfEndingDate () {return bnfEndingDate;}
		public double getBnfAccumulatedTemperature () {return bnfAccumulatedTemperature;}
		public boolean getBnfAccumulatedTemperatureStarted () {return bnfAccumulatedTemperatureStarted;}
		
		public void setBnfAccumulatedTemperatureStarted (boolean d) {bnfAccumulatedTemperatureStarted=d;}
		public void setBnfAccumulatedTemperature (double d) {bnfAccumulatedTemperature=d;}
		public void setBnfStartDate (int d) {bnfStartDate = d;}
		public void setBnfSteadyStateDate(int d) {bnfSteadyStateDate = d;}
		public void setBnfEndingDate(int d) {bnfEndingDate = d;}
		
		//BNF MODULE V1
		
		public void setBnfNitrogenFixationPotential(double v) {bnfNitrogenFixationPotential = (float) v;}
		public double getBnfNitrogenFixationPotential () {return (double) bnfNitrogenFixationPotential;}
		public void setBnfNitrogenFixation(double v) {bnfNitrogenFixation = (float) v;}
		public double getBnfNitrogenFixation() {return (double) bnfNitrogenFixation;}
		
		public void setBnfAnoxStress(double v) {bnfAnoxStress = (float) v;}
		public double getBnfAnoxStress() {return (double) bnfAnoxStress;}
		public void setBnfWaterStress(double v) {bnfWaterStress = (float) v;}
		public double getBnfWaterStress() {return (double) bnfWaterStress;}
		public void setBnfTemperatureStress(double v) {bnfTemperatureStress = (float) v;}
		public double getBnfTemperatureStress() {return (double) bnfTemperatureStress;}
		public void setBnfNodulationInhibition(double v) {bnfNodulationInhibition = (float) v;}
		public double getBnfNodulationInhibition() {return (double) bnfNodulationInhibition;}
		public void setBnfSoilNitrateExcess(double v) {bnfSoilNitrateExcess = (float) v;}
		public double getBnfSoilNitrateExcess() {return (double) bnfSoilNitrateExcess;}	


		public void setBnfPhenologyCoefficient(double v) {bnfPhenologyCoefficient = (float) v;}
		public double getBnfPhenologyCoefficient () {return (double) bnfPhenologyCoefficient;}
		public void setBnfMaxNitrogenFixationPotential(double v) {bnfMaxNitrogenFixationPotential = (float) v;}
		public double getBnfMaxNitrogenFixationPotential() {return (double) bnfMaxNitrogenFixationPotential;}
		

		//
		// GROWTH
		//
		public int getAge () {return age;}
		public void setAge (int a) {age = a;}
		public void addYear () {age = age + 1;}
		public void addHeight (double v) {height += v;}
		public void addLeafArea (double v,int c) {leafArea[c] +=  v;}
		
		
		public void setHeight (double v) {height = (float) v;}
		public void setDbhMeters (double v) {dbhMeters =  v;}
		public void setDBaseMeters (double v) {dBaseMeters=  v;}
		public void setDbh (double v) {dbh = (float) v;}
		public void setCrownBaseHeight (double v) {crownBaseHeight =  v;}
		public void setCrownRadiusTreeLine (double v) {crownRadiusTreeLine = v;}
		public void setCrownRadiusInterRow (double v) {crownRadiusInterRow = v;}
		public void setCrownRadiusVertical (double v) {crownRadiusVertical = v;}
		public void setCrownVolume (double v) {crownVolume = v;}
		public void setCrownParameter (double v) {crownParameter = v;}

		
		
		
		public void setNbCellsBellow(int v) {nbCellsBellow =  v;}
		public void addNbCellsBellow(int v) {nbCellsBellow +=  v;}
		public int getNbCellsBellow () {return nbCellsBellow;}
		
		public void setLaiAboveCells (double v) {laiAboveCells = v;}
		public void setLeafArea (double v, int c) {leafArea[c] = v;}
		public void setLeafAge (int v, int c) {leafAge[c] = v;}
		public void addLeafAge (int c) {leafAge[c] += 1;}
		public void setLeafLue (double v, int c) {leafLue[c] = v;}
		public void setLastLeafArea (double v) {lastLeafArea = v;}
		
		public void razLeafArea () {
			Arrays.fill(this.leafArea 	, 0);
			Arrays.fill(this.leafLue 	, 0);
			Arrays.fill(this.carbonIncrement 	, 0);
			Arrays.fill(this.carbonFoliage 	, 0);
			Arrays.fill(this.nitrogenFoliage 	, 0);
		}

		public double getLeafAreaCohort (int c) {
			if (leafArea != null) return leafArea[c];
			else return 0;
		}

		public int getLeafAgeCohort (int c) {
			if (leafAge != null) return leafAge[c];
			else return 0;
		}
		
		public double getLeafLueCohort (int c) {
			if (leafLue != null) return leafLue[c];
			else return 0;
		}
		
		public double getTotalLeafArea () {
			if (leafArea == null) return 0;
			else {
				double sum = 0;
		        for (int index = 0; index < leafArea.length; index++) 
		            sum += leafArea[index];
		        return sum;
			}
		}
		
		public double[] getLeafArea()
		{
		  if (leafArea != null) {
			  return leafArea;  
		  }
		  else {
			  return new double[this.getTreeSpecies().getNbCohortMax()];	
		  }
		}

		public int[] getLeafAge()
		{
		  if (leafAge != null) {
			  return leafAge;  
		  }
		  else {
			  return new int[this.getTreeSpecies().getNbCohortMax()];	
		  }
		}
		
		public double[] getLeafLue()
		{
		  if (leafLue != null) {
			  return leafLue;  
		  }
		  else {
			  return new double[this.getTreeSpecies().getNbCohortMax()];	
		  }
		}
		
		public double getLaiAboveCells () {return laiAboveCells;}
		public double getLai() {return getTotalLeafArea ()  / (Math.PI * crownRadiusTreeLine * crownRadiusInterRow);}

		public double getLeafAreaMax () {return leafAreaMax;}
		protected double getLastLeafArea () {return lastLeafArea;}

		public double getCrownBaseHeight () {return crownBaseHeight;}
		public double getCrownRadius () {
			return (Math.sqrt (crownRadiusTreeLine * crownRadiusInterRow));}
		public double getCrownRadiusTreeLine () {return crownRadiusTreeLine;}
		public double getCrownRadiusInterRow () {return crownRadiusInterRow;}
		public double getCrownRadiusVertical () {return crownRadiusVertical;}

		public double getCrownVolume () {return crownVolume;}
		public double getCrownParameter () {return  crownParameter;}
		public double getDbhMeters () {return dbhMeters;}
		public double getBranchVolume () {return this.getCrownVolume()*this.getTreeSpecies ().getBranchVolumeRatio ();}

		
		
		//before topping DbaseMeter = dbh * H/H-1.3
		//after topping DbaseMeter is growing like dbh 
		public double getDBaseMeters () {return dBaseMeters;}
		
		public double getStemYield () {
			return(getCarbonStem() / this.getTreeSpecies ().getWoodCarbonContent());
		}
		

		
	
	
		//ACCESSOR FOR LIGHT MODULE
		public double getCaptureFactorForDiffusePar () {
			return  captureFactorForDiffusePar;
		}
		public double getCaptureFactorForDirectPar () {
			return  captureFactorForDirectPar;	
			}
		public double getCaptureFactorForDirectNir () {
			return  captureFactorForDirectNir;	
		}
		public double getCaptureFactorForDiffuseNir () {
			return  captureFactorForDiffuseNir;
			}
		public double getCaptureFactorForInfraRed () {
			return  captureFactorForInfraRed;	
			}
		
		
		public double getDiffuseParIntercepted () {
			return  diffuseParIntercepted;		
		}
		public double getDirectParIntercepted () {
			return  directParIntercepted;		
			}
		
		public double getTotalParIntercepted () {
			return  (directParIntercepted+diffuseParIntercepted) ;		
			}	
		public double getGlobalRadIntercepted () {
			return  globalRadIntercepted;		
			}
		public double getInfraRedIntercepted () {
			return  infraRedIntercepted;		
			}
		public double getLightCompetitionIndex() {
			return  lightCompetitionIndex;		
			}
		public double getCFdirectLonelyTree() {
			return  cFdirectLonelyTree;		
			}
		public double getCFdiffuseLonelyTree() {
			return  cFdiffuseLonelyTree;			
			}
		public double getParInterceptedAnnual () {
			return  (parInterceptedAnnual);		
		}
	
	//CD 08/12/2023
	//Correction on DBH increment if there is lignt competition between trees
	//IF getLightCompetitionIndex=1 (no shade) correction = 1 (no DBH reduction)
	//IF getLightCompetitionIndex>LCImin correction = DBH reduction
	//IF getLightCompetitionIndex<=LCImin correction = 0 (no DBH growth) 
	public double getDbhIncrementLightCorrection() {
		double LCImin = this.getTreeSpecies().getLightCompetitionIndexMin();
		if (getLightCompetitionIndex()<=LCImin) return 0;
		return (getLightCompetitionIndex()-LCImin)/(1-LCImin);		
	}
	
	
	private double round(double v) {
		v = v * 1000000;
		v = Math.round(v);
		v = v / 1000000;
		return v;		

	}
	public void setCaptureFactorForDiffusePar (double e) {captureFactorForDiffusePar =   e;}
	public void setCaptureFactorForDirectPar (double e) {captureFactorForDirectPar =   e;}
	public void setCaptureFactorForDirectNir (double e) {captureFactorForDirectNir =   e;}
	public void setCaptureFactorForDiffuseNir (double e) {captureFactorForDiffuseNir =   e;}
	public void setCaptureFactorForInfraRed (double e) {captureFactorForInfraRed = e;}
	
	public void setDiffuseParIntercepted (double e) {diffuseParIntercepted = e;}
	public void setDirectParIntercepted (double e) {directParIntercepted = e;}
	public void setGlobalRadIntercepted (double e) {globalRadIntercepted = e;}
	public void setInfraRedIntercepted (double e) {infraRedIntercepted = e;}
	public void setLightCompetitionIndex (double e){lightCompetitionIndex = e;}
	public void setCFdiffuseLonelyTree (double e){cFdiffuseLonelyTree = e;}
	public void setCFdirectLonelyTree (double e){cFdirectLonelyTree = e;}


	//ACCESSOR FOR C AND N ALLOCATION MODULE

	public double getStemVolume () {return stemVolume;}
	public double getCarbonStem () {return carbonStem;}
	public double getCarbonStump () {return carbonStump;}
	public double getCarbonFoliageCohort (int i) {return carbonFoliage[i];}
	

	public double getTotalCarbonFoliage () {
		if (carbonFoliage == null) return 0;
		else {
			double sum = 0;
	        for (int index = 0; index < carbonFoliage.length; index++) 
	            sum += carbonFoliage[index];
	        return sum;
		}
	}
	public double[] getCarbonFoliage()
	{
	  if (carbonFoliage != null) {
		  return carbonFoliage;  
	  }
	  else {
		if (this.getTreeSpecies().getNbCohortMax() > 0) {
		  return new double[this.getTreeSpecies().getNbCohortMax()];
		}
		else {
		  return new double[1];
		}
	  }
	}
	
	public double getCarbonFoliageMax () {return carbonFoliageMax;}
	public double getCarbonBranches () {return carbonBranches;}
	public double getCarbonCoarseRoots () {return carbonCoarseRoots;}
	public double getCarbonFineRoots () {return carbonFineRoots;}
	public double getCarbonLabile () {return carbonLabile;}
	public double getCarbonFruit () {return carbonFruit;}
	
	//BILAN CARBONE
	public double getCarbonTotalBefore () {return carbonTotalBefore;}
	
	public double getCarbonTotal() {
		return  getTotalCarbonFoliage ()+
				carbonBranches +
				carbonCoarseRoots +
				carbonFineRoots +
				carbonFruit +
				carbonStem +
				carbonStump +
				carbonLabile ;}
	
	public double getCarbonSenTotal() {
		return  carbonFoliageSen+
				carbonFineRootsSen +
				carbonCoarseRootsSen +
				carbonBranchesSen+
				carbonFruitSen ;}
	
	public double getCarbonExportedTotal() {
		return  carbonFoliageExported +
				carbonStemExported +
				carbonStumpExported +
				carbonBranchesExported+
				carbonFruitExported;}
	
	public double getTotalCarbonIncrement () {
		if (carbonIncrement == null) return 0;
		else {
			double sum = 0;
	        for (int index = 0; index < carbonIncrement.length; index++) 
	            sum += carbonIncrement[index];
	        return sum;
		}
	}

	
	
	//bilan carbon = 0 
	// somme des pool du jour J-1
	//- somme des senescences
	//- somme des exportations (pruning)
	//+ somme du carbone alloué
	//- somme des pool du jour J
	public double getVerifCarbonBilan() {
		double bilan = getCarbonTotalBefore()
						-getCarbonSenTotal()
						-getCarbonExportedTotal()
						+getTotalCarbonIncrement()
						-getCarbonTotal();
				if (bilan > 0.000001 || bilan < -0.000001) {

					return bilan;
				}
				else return 0;

			}
	

	
	public double getVerifCarbonBranches() {
		return getBranchVolume() * this.getTreeSpecies().getWoodDensity() * this.getTreeSpecies().getWoodCarbonContent();
	}

	public double getVerifCarbonStem() {
		return getStemVolume() * this.getTreeSpecies().getWoodDensity() * this.getTreeSpecies().getWoodCarbonContent();
	}

	public double getVerifCarbonStump() {
		return getCarbonStem()*this.getTreeSpecies().getStumpToStemBiomassRatio();
	}

	
	public double getVerifCarbonFoliage() {
		
		if (!((this.getPhenologicalStage()==0)||(this.getPhenologicalStage()==4))){

			return  (this.getTotalLeafArea()	
					* this.getTreeSpecies().getLeafMassArea()		
					* this.getTreeSpecies().getLeafCarbonContent());	
		} else return 0;
	}

	public int getFlowerNbrPotential () {return flowerNbrPotential;}
	public void setFlowerNbrPotential(int v) {flowerNbrPotential =  v;}
	
	
	public double getFlowerNbrPotentialReducer () {return flowerNbrPotentialReducer;}
	public void setFlowerNbrPotentialReducer(double v) {flowerNbrPotentialReducer =  v;}
	
	public int getFlowerNbrPotentialFromLeafArea () {return flowerNbrPotentialFromLeafArea;}
	public void setFlowerNbrPotentialFromLeafArea(int v) {flowerNbrPotentialFromLeafArea =  v;}
	
	
	public int getFlowerNbrPotentialDaily () {return flowerNbrPotentialDaily;}
	public void setFlowerNbrPotentialDaily(int v) {flowerNbrPotentialDaily =  v;}
	
	public int getFlowerNbr() {return flowerNbr;}
	public void setFlowerNbr(int v) {flowerNbr =  v;}
	

	public int getFruitNbr () {return fruitNbr;}
	public void setFruitNbr(int v) {fruitNbr =  v;}
	
	
	public int getFruitThinning () {return fruitThinning;}
	public void setFruitThinning(int v) {fruitThinning =  v;}

	public double getFruitMeanDryMatterWeight () {
		double carbonToDryMatter = 0.42;			//A passer dans hisafe.par
		if (getFruitNbr() > 0) return (	(getCarbonFruit() / getFruitNbr()) / (1 - carbonToDryMatter) );
		else return 0;
	}

	public double getFruitMeanFreshMatterWeight () {return getFruitMeanDryMatterWeight() * this.getTreeSpecies().getFruitDryToFreshMatterWeight();}

	public double getFruitVolume () {return getFruitMeanDryMatterWeight () * this.getTreeSpecies().getFruitDryMaterDensity();}
	
	public double getFruitEquivalentSurfaceArea () {
		double radiusEquivalentSphere = Math.cbrt(3 * getFruitVolume () / 4 * Math.PI);
		return ((getFruitNbr () * Math.PI * radiusEquivalentSphere * radiusEquivalentSphere) / 10000);	
	}
	
	
	public double getFruitOilConcentration () {
	
		double fruitOilConversionCoeffA = this.getTreeSpecies().getFruitOilConversionCoeffA();
		double fruitOilConversionCoeffB = this.getTreeSpecies().getFruitOilConversionCoeffB();
		double fruitOilConversionCoeffC = this.getTreeSpecies().getFruitOilConversionCoeffC();
		
		if ((getFruitPhenologicalStage() == 4 ||  getFruitPhenologicalStage() == 5) & getFruitMeanFreshMatterWeight() > 0) {
			return (fruitOilConversionCoeffA * (1 - Math.exp(-fruitOilConversionCoeffB * Math.pow(getFruitMeanFreshMatterWeight()*1000,fruitOilConversionCoeffC) ))) ;
		}
		else return 0;
		
	}
	
	public double getFruitOilWeight () {
		return ((getFruitMeanFreshMatterWeight() * (getFruitOilConcentration() / 100)) * getFruitNbr());
	}
	
	
	public double getFruitOilYield () {
		return (getFruitOilWeight() * this.getTreeSpecies().getFruitOilDensity());
	}
	

	public double getFruitCarbonTarget() {
		return (this.getTreeSpecies().getFruitAllocationFraction() * getAboveGroundGrowth());
	}
	

	public double getFruitDemandTarget () {
		return (this.getTreeSpecies().getFruitMaxDryMatterAllocation() * getFruitNbr());
	}

	
	
	
	public double getFloweringHeatStress() {return floweringHeatStress;}
	public void setFloweringHeatStress(double v) {floweringHeatStress =  v;}
	
	public double getFloweringFrostStress() {return floweringFrostStress;}
	public void setFloweringFrostStress(double v) {floweringFrostStress =  v;}
	
	
	public double getCarbonStemExported () {return carbonStemExported;}
	public double getCarbonStumpExported () {return carbonStumpExported;}
	public double getCarbonFoliageExported () {return carbonFoliageExported;}
	public double getCarbonBranchesExported () {return carbonBranchesExported;}
	public double getCarbonFruitExported () {return carbonFruitExported;}
	
	
	public void setStemVolume (double v) {stemVolume =  v;}
	public void setCarbonStem (double v) {carbonStem = v;}
	public void setCarbonStump(double v) {carbonStump = v;}
	public void setCarbonFoliage (double v, int i) {carbonFoliage[i] = v;}
	public void setCarbonBranches (double v) {carbonBranches = v;}
	public void setCarbonCoarseRoots (double v) {carbonCoarseRoots = v;}
	public void setCarbonFineRoots (double v) {carbonFineRoots = v;}	
	public void setCarbonFruit(double v) {carbonFruit = v;}
	public void setCarbonTotalBefore (double v) {carbonTotalBefore = v;}
	
	
	

	
	
	public void addCarbonCoarseRoots (double v) {carbonCoarseRoots += v;}
	public void addCarbonFineRoots (double v) {carbonFineRoots += v;}
	
	public void setCarbonStemExported (double v) {carbonStemExported =  v;}
	public void setCarbonStumpExported (double v) {carbonStumpExported =  v;}
	public void setCarbonFoliageExported (double v) {carbonFoliageExported =  v;}
	public void setCarbonBranchesExported (double v) {carbonBranchesExported =  v;}	
	public void setCarbonFruitExported (double v) {carbonFruitExported =  v;}
	
	public void addCarbonStemExported (double v) {carbonStemExported += v;}
	public void addCarbonStumpExported (double v) {carbonStumpExported += v;}
	public void addCarbonFoliageExported (double v) {carbonFoliageExported += v;}
	public void addCarbonBranchesExported (double v) {carbonBranchesExported +=  v;}	
	public void addCarbonFruitExported (double v) {carbonFruitExported +=  v;}
	public void setCarbonLabile (double v) {carbonLabile = v;}
	public void	addCarbonLabile (double v) {carbonLabile +=  v;} //AQ-26.04.2011
	public void	addCarbonFruit (double v) {carbonFruit += v;} 
	
	
	public double 	getAboveGroundCFraction () {return aboveGroundCFraction;}
	public void setAboveGroundCFraction (double v) {aboveGroundCFraction =  v;}
	
	public void	setCarbonIncrement (double v, int i) {carbonIncrement[i]  =  v;}
	public void	setCarbonFoliageIncrement (double v) {carbonFoliageIncrement  =   v;}
	public void	setCarbonStemIncrement (double v) {carbonStemIncrement  =  v;}
	public void	setCarbonBranchesIncrement (double v) {carbonBranchesIncrement  =   v;}
	public void	setCarbonStumpIncrement (double v) {carbonStumpIncrement  = v;}
	public void setCarbonFineRootsIncrement (double v) {carbonFineRootsIncrement = v;}
	public void setCarbonCoarseRootsIncrement (double v) {carbonCoarseRootsIncrement =  v;}	
	public void setCarbonFruitIncrement (double v) {carbonFruitIncrement =  v;}
	public void setCarbonLabileIncrement (double v) {carbonLabileIncrement = v;}
	
	public double[] getCarbonIncrement () {return   carbonIncrement;}	
	public double getCarbonIncrementCohort (int i) {return   carbonIncrement[i];}

	public double getAboveGroundImbalance () {return  aboveGroundImbalance;}
	public double getNSCExchange () {return  NSCExchange;}
	public double getCarbonSavingFraction () {return  carbonSavingFraction;}
	
	public double getCarbonFoliageIncrement () {return  carbonFoliageIncrement;}
	public double getCarbonStemIncrement () {return carbonStemIncrement;}
	public double getCarbonBranchesIncrement () {return carbonBranchesIncrement;}
	public double getCarbonStumpIncrement () {return  carbonStumpIncrement;}
	public double getCarbonCoarseRootsIncrement (){return carbonCoarseRootsIncrement;}
	public double getCarbonFineRootsIncrement (){return  carbonFineRootsIncrement;}	
	public double getCarbonFruitIncrement (){return  carbonFruitIncrement;}	
	public double getCarbonLabileIncrement () {return carbonLabileIncrement;}
	public double getCarbonTotalIncrement (){return  carbonFoliageIncrement+carbonStemIncrement+carbonBranchesIncrement+carbonStumpIncrement
													+carbonCoarseRootsIncrement+carbonFineRootsIncrement
													+carbonFruitIncrement;}	
	

	
	
	public double getTotalCarbonBranchesDeadOnTree () {
		if (carbonBranchesDeadOnTree == null) return 0;
		else {
			double sum = 0;

			Set keys = carbonBranchesDeadOnTree.keySet();
			for(Iterator it = keys.iterator(); it.hasNext ();){
				Double valeur = carbonBranchesDeadOnTree.get(it.next());
				sum = sum +valeur;
			}

	        return sum;
		}
	}
	
	
	public void addCarbonBranchesDeadOnTree (int year, double v) {

		if (carbonBranchesDeadOnTree.containsKey(year)) {
			double valeur = getCarbonBranchesDeadOnTree (year);
			valeur = valeur + v;
			carbonBranchesDeadOnTree.remove(year);
			carbonBranchesDeadOnTree.put(year,valeur);
		}
		else  carbonBranchesDeadOnTree.put(year,v);
	}
	
	
  	public double getCarbonBranchesDeadOnTree (int year) {
  		return (double) (carbonBranchesDeadOnTree.get (year));
  	}
  	
  	
  	
	public void setTargetLfrRatio (double r) {targetLfrRatio = r;}
	
	public void setCarbonCoarseRootsTarget(double b) {carbonCoarseRootsTarget =  b;}
	public void addCarbonCoarseRootsTarget(double b) {carbonCoarseRootsTarget += b;}
	public double 	getCarbonCoarseRootsTarget() {return carbonCoarseRootsTarget; } 
	public double 	getCarbonCoarseRootsImbalance() {return carbonCoarseRootsTarget-carbonCoarseRoots; } 

	public void setCarbonAllocToGrowth (double v) {carbonAllocToGrowth = v;}


	public double getNitrogenStem () {return nitrogenStem;}
	public double getNitrogenStump () {return nitrogenStump;}
	public double getNitrogenFoliageCohort (int i) {return nitrogenFoliage[i];}
	public double getTotalNitrogenFoliage () {
		if (nitrogenFoliage == null) return 0;
		else {
			double sum = 0;
	        for (int index = 0; index < nitrogenFoliage.length; index++) 
	            sum += nitrogenFoliage[index];
	        return sum;
		}
	}
	
	public double[] getNitrogenFoliage()
	{
	  if (nitrogenFoliage != null) {
		  return nitrogenFoliage;  
	  }
	  else {
		if (this.getTreeSpecies().getNbCohortMax() > 0) {
		  return new double[this.getTreeSpecies().getNbCohortMax()];
		}
		else {
		  return new double[1];
		}
	  }
	}
	
	public double getNitrogenBranches () {return nitrogenBranches;}
	public double getNitrogenCoarseRoots () {return nitrogenCoarseRoots;};
	public double getNitrogenFineRoots () {return nitrogenFineRoots;}
	public double getNitrogenLabile () {return  nitrogenLabile;}
	public double getNitrogenFruit() {return nitrogenFruit;}
	
	public double getNitrogenStemExported () {return  nitrogenStemExported;}
	public double getNitrogenStumpExported () {return  nitrogenStumpExported;}
	public double getNitrogenFoliageExported () {return  nitrogenFoliageExported;}
	public double getNitrogenBranchesExported () {return  nitrogenBranchesExported;}
	public double getNitrogenFruitExported () {return  nitrogenFruitExported;}
	
	public double getTotalNitrogenBranchesDeadOnTree () {
		if (nitrogenBranchesDeadOnTree == null) return 0;
		else {
			double sum = 0;
			Set keys = nitrogenBranchesDeadOnTree.keySet();
			for(Iterator it = keys.iterator(); it.hasNext ();){
				Double valeur = nitrogenBranchesDeadOnTree.get(it.next());
				sum = sum +valeur;
			}
	        
	        return sum;
		}
	}
	public void addNitrogenBranchesDeadOnTree (int year, double v) {

		if (nitrogenBranchesDeadOnTree.containsKey(year)) {
			double valeur = getNitrogenBranchesDeadOnTree (year);
			valeur = valeur + v;
			if(valeur ==0)nitrogenBranchesDeadOnTree.remove(year);
			else nitrogenBranchesDeadOnTree.put(year,v);
		}
		else  nitrogenBranchesDeadOnTree.put(year,v);
	}
	
	
  	public double getNitrogenBranchesDeadOnTree (int year) {
  		return (double) (nitrogenBranchesDeadOnTree.get (year));}
  	
  	
	
	public void	setNitrogenStem (double v) {nitrogenStem = v;}
	public void	setNitrogenStump (double v) {nitrogenStump =  v;}
	public void setNitrogenFoliage (double v, int i) {nitrogenFoliage[i] =  v;}
	public void setNitrogenBranches (double v) {nitrogenBranches =  v;}
	public void setNitrogenCoarseRoots (double v) {nitrogenCoarseRoots =  v;}
	public void setNitrogenFineRoots (double v) {nitrogenFineRoots = v;}
	public void setNitrogenFruit (double v) {nitrogenFruit = v;}

	
	public void setNitrogenStemExported (double v) {nitrogenStemExported =  v;}
	public void setNitrogenStumpExported (double v) {nitrogenStumpExported =  v;}
	public void setNitrogenFoliageExported (double v) {nitrogenFoliageExported =  v;}
	public void setNitrogenBranchesExported (double v) {nitrogenBranchesExported =  v;}	
	public void setNitrogenFruitExported (double v) {nitrogenFruitExported = v;}
	public void addNitrogenStemExported (double v) {nitrogenStemExported +=  v;}
	public void addNitrogenStumpExported (double v) {nitrogenStumpExported +=  v;}
	public void addNitrogenFoliageExported (double v) {nitrogenFoliageExported +=  v;}
	public void addNitrogenBranchesExported (double v) {nitrogenBranchesExported +=  v;}	
	public void addNitrogenFruitExported (double v) {nitrogenFruitExported +=  v;}
	public void setNitrogenLabile (double v) {nitrogenLabile =  v;}
	public void	addNitrogenLabile (double v){nitrogenLabile +=  v;} //GT-4.06.2010
	private void setNitrogenSaturation (double v) {nitrogenSaturation =   v;}
	private void setNitrogenSatisfaction(double v) {nitrogenSatisfaction =   v;}
		


	public void setCarbonFoliageSen (double d) {carbonFoliageSen=  d;}
	public void addCarbonFoliageSen (double d) {carbonFoliageSen +=  d;}
	public void setNitrogenFoliageSen (double d) {nitrogenFoliageSen=  d;}
	public double getCarbonFoliageSenAnnual () {return carbonFoliageSenAnnual;}
	public void addNitrogenFoliageSen (double d) {nitrogenFoliageSen +=  d;}
	public double getCarbonFoliageSen () {return carbonFoliageSen;}
	public double getTotalNitrogenFoliageSen () {return nitrogenFoliageSen;}	

	public double getCarbonFoliageLitterUnderTree () {return carbonFoliageLitterUnderTree;}
	public double getCarbonBranchesLitterUnderTree () {return carbonBranchesLitterUnderTree;}
	public double getCarbonFruitLitterUnderTree () {return carbonFruitLitterUnderTree;}
	public double getNitrogenFoliageLitterUnderTree () {return nitrogenFoliageLitterUnderTree;}
	public double getNitrogenBranchesLitterUnderTree () {return nitrogenBranchesLitterUnderTree;}
	public double getNitrogenFruitLitterUnderTree () {return nitrogenFruitLitterUnderTree;}
	
	public double getCarbonFoliageLitterAllPlot() {return carbonFoliageLitterAllPlot;}
	public double getCarbonBranchesLitterAllPlot () {return carbonBranchesLitterAllPlot;}
	public double getCarbonFruitLitterAllPlot () {return carbonFruitLitterAllPlot;}
	public double getNitrogenFoliageLitterAllPlot () {return nitrogenFoliageLitterAllPlot;}
	public double getNitrogenBranchesLitterAllPlot () {return nitrogenBranchesLitterAllPlot;}
	public double getNitrogenFruitLitterAllPlot() {return nitrogenFruitLitterAllPlot;}
	
	
	public void addCarbonFoliageLitterUnderTree (double d) {carbonFoliageLitterUnderTree +=  d;}
	public void addCarbonBranchesLitterUnderTree (double d) {carbonBranchesLitterUnderTree +=  d;}
	public void addCarbonFruitLitterUnderTree (double d) {carbonFruitLitterUnderTree +=  d;}
	public void addNitrogenFoliageLitterUnderTree (double d) {nitrogenFoliageLitterUnderTree +=  d;}
	public void addNitrogenBranchesLitterUnderTree (double d) {nitrogenBranchesLitterUnderTree +=  d;}
	public void addNitrogenFruitLitterUnderTree (double d) {nitrogenFruitLitterUnderTree +=  d;}
	
	public void addCarbonFoliageLitterAllPlot(double d) {carbonFoliageLitterAllPlot +=  d;}
	public void addCarbonBranchesLitterAllPlot (double d) {carbonBranchesLitterAllPlot +=  d;}
	public void addCarbonFruitLitterAllPlot (double d) {carbonFruitLitterAllPlot +=  d;}
	public void addNitrogenFoliageLitterAllPlot (double d) {nitrogenFoliageLitterAllPlot +=  d;}
	public void addNitrogenBranchesLitterAllPlot (double d) {nitrogenBranchesLitterAllPlot +=  d;}
	public void addNitrogenFruitLitterAllPlot(double d) {nitrogenFruitLitterAllPlot +=  d;}
	
	
	public void setCarbonFoliageLitterUnderTree (double d) {carbonFoliageLitterUnderTree =  d;}
	public void setCarbonBranchesLitterUnderTree (double d) {carbonBranchesLitterUnderTree =  d;}
	public void setCarbonFruitLitterUnderTree (double d) {carbonFruitLitterUnderTree =  d;}
	public void setNitrogenFoliageLitterUnderTree (double d) {nitrogenFoliageLitterUnderTree =  d;}
	public void setNitrogenBranchesLitterUnderTree (double d) {nitrogenBranchesLitterUnderTree =  d;}
	public void setNitrogenFruitLitterUnderTree (double d) {nitrogenFruitLitterUnderTree =  d;}
	
	public void setCarbonFoliageLitterAllPlot(double d) {carbonFoliageLitterAllPlot =  d;}
	public void setCarbonBranchesLitterAllPlot (double d) {carbonBranchesLitterAllPlot =  d;}
	public void setCarbonFruitLitterAllPlot (double d) {carbonFruitLitterAllPlot =  d;}
	public void setNitrogenFoliageLitterAllPlot (double d) {nitrogenFoliageLitterAllPlot =  d;}
	public void setNitrogenBranchesLitterAllPlot (double d) {nitrogenBranchesLitterAllPlot =  d;}
	public void setNitrogenFruitLitterAllPlot(double d) {nitrogenFruitLitterAllPlot =  d;}
	
	
	//Senescences racines 
	public double getCarbonCoarseRootsSen() {return carbonCoarseRootsSen;}
	public double getCarbonFineRootsSen (){return carbonFineRootsSen;}
	
	public void setCarbonCoarseRootsSen(double v) {carbonCoarseRootsSen =  v;}
	public void setCarbonFineRootsSen (double v) {carbonFineRootsSen =  v;}
	public void addCarbonCoarseRootsSen(double v) {carbonCoarseRootsSen +=  v;}
	public void addCarbonFineRootsSen(double v) {carbonFineRootsSen += v;}
	public double getCarbonCoarseRootsSenAnnual() {return carbonCoarseRootsSenAnnual;}
	public double getCarbonFineRootsSenAnnual () {return carbonFineRootsSenAnnual;}
	public double getCarbonCoarseRootsSenAnoxia () {return carbonCoarseRootsSenAnoxia;}
	public double getCarbonFineRootsSenAnoxia () {return  carbonFineRootsSenAnoxia;}
	public void setCarbonCoarseRootsSenAnoxia (double v) { carbonCoarseRootsSenAnoxia =  v;}
	public void setCarbonFineRootsSenAnoxia (double v) {carbonFineRootsSenAnoxia = v;}
	public void addCarbonCoarseRootsSenAnoxia (double v) {carbonCoarseRootsSenAnoxia += v;}
	public void addCarbonFineRootsSenAnoxia (double v) {carbonFineRootsSenAnoxia +=  v;}
	public double getCarbonFineRootsSenAnoxiaAnnual () {return carbonFineRootsSenAnoxiaAnnual;}
	public double getCarbonCoarseRootsSenAnoxiaAnnual() {return carbonCoarseRootsSenAnoxiaAnnual;}
	public double getNitrogenCoarseRootsSen(){return nitrogenCoarseRootsSen;}
	public double getNitrogenFineRootsSen (){return nitrogenFineRootsSen;}
	public void setNitrogenCoarseRootsSen(double v){nitrogenCoarseRootsSen = v;}
	public void setNitrogenFineRootsSen (double v){nitrogenFineRootsSen = v;}
	public void addNitrogenCoarseRootsSen(double v){nitrogenCoarseRootsSen += v;}
	public void addNitrogenFineRootsSen(double v){nitrogenFineRootsSen += v;}
	public double getNitrogenCoarseRootsSenAnoxia(){return nitrogenCoarseRootsSenAnoxia;}
	public double getNitrogenFineRootsSenAnoxia (){return  nitrogenFineRootsSenAnoxia;}
	public void setNitrogenCoarseRootsSenAnoxia(double v){nitrogenCoarseRootsSenAnoxia =  v;}
	public void setNitrogenFineRootsSenAnoxia (double v){nitrogenFineRootsSenAnoxia = v;}
	public void addNitrogenCoarseRootsSenAnoxia(double v){nitrogenCoarseRootsSenAnoxia += v;}
	public void addNitrogenFineRootsSenAnoxia(double v){nitrogenFineRootsSenAnoxia += v;}
	
	public void addCarbonBranchesSen(double v) {carbonBranchesSen +=  v;}
	public void addCarbonFruitSen(double v) {carbonFruitSen +=  v;}
	public void setCarbonBranchesSen(double v) {carbonBranchesSen =  v;}
	public void setCarbonFruitSen(double v) {carbonFruitSen =  v;}
	public double getCarbonBranchesSen() {return  carbonBranchesSen;}
	public double getCarbonFruitSen() {return  carbonFruitSen;}
	public void setNitrogenBranchesSen(double v) {nitrogenBranchesSen =  v;}
	public void setNitrogenFruitSen(double v) {nitrogenFruitSen =  v;}
	public void addNitrogenBranchesSen(double v) {nitrogenBranchesSen +=  v;}
	public void addNitrogenFruitSen(double v) {nitrogenFruitSen +=  v;}
	public double getNitrogenBranchesSen() {return  nitrogenBranchesSen;}
	public double getNitrogenFruitSen() {return  nitrogenFruitSen;}
	
	public double getCarbonTotalSen() {return  getCarbonFoliageSen()+carbonBranchesSen+carbonFineRootsSen+carbonCoarseRootsSen+carbonFruitSen;}

	
	//---------------------------------------------------------------------------------------------


	public double	getCarbonAllocToGrowth () {return carbonAllocToGrowth;}
	public double	getCarbonAllocToGrowthAnnual () {return carbonAllocToGrowthAnnual;}
	public double	getCarbonAboveGroundEff () {
		//11/02/2021 add fruit carbon pool (IL + NB) 
		if (getTotalCarbonFoliage() > 0) return getTotalCarbonFoliage()/(getTotalCarbonFoliage()+carbonStem+carbonBranches+carbonStump+getCarbonFruit());
		else return 0;
		}
	public double	getCarbonBelowGroundEff () {
		if (carbonFineRoots+carbonCoarseRoots> 0)  return carbonFineRoots/(carbonFineRoots+carbonCoarseRoots);
		else return 0;
		}

	public double 	getTargetLfrRatio() {return targetLfrRatio;}
	public double 	getLfrRatio() {
		if(getCarbonFineRoots()>0){
			return getTotalCarbonFoliage()/(getCarbonFineRoots()+getTotalCarbonFoliage());
		} else {
			if (getTotalCarbonFoliage()>0){
				return 0;
			} else {
				return 1;
			}
		}
	}

	// for SafeExport : Nitrogen concentrations in carbon pool (Kg N / Kg C)
	public double getNCLabile () { if (getCarbonLabile() > 0) return nitrogenLabile/getCarbonLabile(); else return 0;}
	public double getNCStump () {if (getCarbonStump() > 0) return nitrogenStump/getCarbonStump(); else return 0;}
	public double getNCStem () {if (getCarbonStem() > 0)  return nitrogenStem/getCarbonStem();  else return 0;}
	public double getNCFoliage () {
		if(getTotalCarbonFoliage() != 0)
			return getTotalNitrogenFoliage()/getTotalCarbonFoliage();
		else
			return 0;
	}
	public double getNCBranches() {if (getCarbonBranches() > 0) return nitrogenBranches/getCarbonBranches(); else return 0;}
	public double getNCCoarseRoots () {if (getCarbonCoarseRoots() > 0) return nitrogenCoarseRoots/getCarbonCoarseRoots(); else return 0;}
	public double getNCFineRoots () {if (getCarbonFineRoots() > 0) return nitrogenFineRoots/getCarbonFineRoots(); else return 0;}
	public double getNCFruit() {if (getCarbonFruit() > 0) return nitrogenFruit/getCarbonFruit(); else return 0;}
	
	public double getNitrogenSaturation () {return nitrogenSaturation;}
	public double getNitrogenSatisfaction () {return nitrogenSatisfaction;}

	//ACCESSOR FOR MICROCLIMATE RESULTS
	public void setStoredRain (double v) {storedRain  =   v;}
	public void addStoredRain (double v) {storedRain  +=   v;}

	public double getStoredRain () {return storedRain;}
	public double getStoredRainMm () {
		double cellArea = this.getScene().getArea();
		return  storedRain/cellArea;
	}
	public void setStemflow (double v) {stemflow  =   v;}
	public void addStemflow (double v) {stemflow  +=   v;}
	public double getStemflow () {return stemflow;}
	public double getStemflowMm () {
		double cellArea = this.getScene().getArea();
		return  stemflow/cellArea;
	}

	public void setEvaporatedRain (double v) {evaporatedRain  = v;} 
	public void addEvaporatedRain (double v) {evaporatedRain  +=  v;}
	public double getEvaporatedRain () {return  evaporatedRain;}
	public double getEvaporatedRainMm () {
		double cellArea = this.getScene().getArea();
		return  evaporatedRain/cellArea;}


	public void setInterceptedRain (double v) {interceptedRain  = v;}
	public double getInterceptedRain () {return  interceptedRain;}
	public double getInterceptedRainAnnual () {return  interceptedRainAnnual;}
	public double getInterceptedRainMm () {
		double cellArea = this.getScene().getArea();
		return  interceptedRain/cellArea;
	}
	public void addInterceptedRain (double v) {interceptedRain  +=  v;}

	//ACCESSOR FOR WATER AND NITROGEN BUDGET
	public void setWaterDemand (double v) {waterDemand  =   v;}
	public void setWaterDemandReduced (double v) {waterDemandReduced  =  v;}
	public double getWaterDemand() {return waterDemand;}
	public double getWaterDemandMm () {
		double cellArea = this.getScene().getArea();
		return  waterDemand/cellArea;
	}


	public double getWaterDemandReduced() {return   waterDemandReduced;}
	public double getWaterDemandReducedMm () {
		double cellArea = this.getScene().getArea();
		return  waterDemandReduced/cellArea;
	}
	public double getWaterDemandReductionFactor() {return waterDemandReduced/waterDemand;}

	public double getLongitudinalPotentialDrop(){return this.getPlantRoots().getLongitudinalTransportPotential();}
	public double getRadialPotentialDrop(){return this.getPlantRoots().getRadialTransportPotential();}



	private double getStomatalConductance () {return stomatalConductance;}
	private void setStomatalConductance (double v) {stomatalConductance =  v;}
	
	
	
	public void setWaterUptake  (double v) {waterUptake  =  v;}
	public double getWaterUptake () {return  waterUptake;}
	public double getWaterUptakeAnnual () {return waterUptakeAnnual ;}
	public double getWaterUptakeMm () {
		double cellArea = this.getScene().getArea();
		return  getWaterUptake ()/cellArea;
	}

	public void addWaterUptake  (double v) {waterUptake = waterUptake+ v;}
	
	public double getWaterStress () {return waterStress;}
	public void setWaterStress (double v) {waterStress =  v;}
	//sum of water stress divided by number of days
	public double getWaterStressSpring () {return waterStressSpring/91.0;}
	public double getWaterStressSummer () {return waterStressSummer/92.0;}
	
	
	public double	getNitrogenDemandAfterFixation () {return nitrogenDemandAfterFixation;}
	public void		setNitrogenDemandAfterFixation (double v) {nitrogenDemandAfterFixation   =   v;}
	
	public double	getNitrogenDemandBeforeFixation () {return nitrogenDemandBeforeFixation;}
	public void		setNitrogenDemandBeforeFixation (double v) {nitrogenDemandBeforeFixation   =  v;}

	
	
	public void setNitrogenAvailable (double v) {nitrogenAvailable =   v;}
	public void setNitrogenUptake (double v) {nitrogenUptake =   v;}
	public void addNitrogenUptake (double v) {nitrogenUptake +=  v;}
	public double getNitrogenUptake() {return nitrogenUptake;}
	public double getNitrogenAvailable() {return nitrogenAvailable;}
	public double getNitrogenUptakeAnnual() {return nitrogenUptakeAnnual;}
	public double getNitrogenStress () {return nitrogenStress;}

	public void setNitrogenStress (double v) {nitrogenStress =  v;}
	
	
	public double getTreeLeafTranspirationRate() {
		if (this.getTotalLeafArea() > 0) return this.getWaterUptake()/this.getTotalLeafArea();
		else return 0;
	}
	

	//sum of nitrogen stress divided by number of days
	public double getNitrogenStressSpring () {return nitrogenStressSpring/91.0;}
	public double getNitrogenStressSummer () {return nitrogenStressSummer/92.0;}

	public void setLueStress (double v) {lueStress =  v;}
	public void setLueTemperatureStress (double v) {lueTemperatureStress =  v;}
	
	public void setLueWaterStress (double v) {lueWaterStress =  v;}
	public void setLueNitrogenStress (double v) {lueNitrogenStress =  v;}
	public double getLueWaterStress () {return lueWaterStress;}
	public double getLueNitrogenStress () {return lueNitrogenStress;}
	public double getLueTemperatureStress() {return lueTemperatureStress;}
	public double getLueStress () {return lueStress;}

	public double getRsBelowGroundStressEffect() {return rsBelowGroundStressEffect;}
	public double getRsNitrogenExcessEffect() {return rsNitrogenExcessEffect;}
	public double getRsLightStressEffect() {return rsLightStressEffect;}
	public double getTargetLfrDrift() {return targetLfrDrift;}
	

	public void setCo2LueEffect(double v) {co2LueEffect =  v;}
	public void setCo2WueEffect(double v) {co2WueEffect =  v;}
	public double getCo2LueEffect () {return co2LueEffect;}
	public double getCo2WueEffect () {return co2WueEffect;}
	

	public void setWaterUptakeInSaturation(double v) {waterUptakeInSaturation = v;}
	public void addWaterUptakeInSaturation (double v) {waterUptakeInSaturation +=  v;}
	public double getWaterUptakeInSaturation () {return  waterUptakeInSaturation;}

	public void setNitrogenUptakeInSaturation(double v) {nitrogenUptakeInSaturation =  v;}
	public void addNitrogenUptakeInSaturation (double v) {nitrogenUptakeInSaturation +=  v;}
	public double getNitrogenUptakeInSaturation () {return nitrogenUptakeInSaturation;}	

	public double getAboveGroundAllocFrac () {return  aboveGroundAllocFrac;}
	public double getStemAllocFrac () {return  stemAllocFrac;}
	public double getBranchAllocFrac () {return  branchAllocFrac;}
	public double getFoliageAllocFrac () {return  foliageAllocFrac;}
	
	public double getBelowGroundAllocFrac () {return  belowGroundAllocFrac;}
	public double getStumpAllocFrac () {return  stumpAllocFrac;}
	public double getFineRootsAllocFrac () {return  fineRootsAllocFrac;}
	public double getCoarseRootsAllocFrac () {return  coarseRootsAllocFrac;}

	public double getBranchImbalance() {return  branchImbalance;}
	public double getBelowGroundGrowth() {return  belowGroundGrowth;}
	public double getAboveGroundGrowth() {return  aboveGroundGrowth;}
	public double getAboveGroundSink () {return  aboveGroundSink;}
	public double getBelowGroundSink () {return  belowGroundSink;}
	
	public double getCarbonStemSink () {return  carbonStemSink;}
	public double getCarbonBranchesSink () {return  carbonBranchesSink;}
	public double getCarbonFoliageSink () {return  carbonFoliageSink;}
	public double getCarbonStumpSink () {return  carbonStumpSink;}
	public double getCarbonFineRootsSink () {return  carbonFineRootsSink;}
	public double getCarbonCoarseRootsSink () {return  carbonCoarseRootsSink;}

	public double getNitrogenStemSink () {return  nitrogenStemSink;}
	public double getNitrogenBranchesSink () {return  nitrogenBranchesSink;}
	public double getNitrogenFoliageSink () {return  nitrogenFoliageSink;}
	public double getNitrogenStumpSink () {return  nitrogenStumpSink;}
	public double getNitrogenFineRootsSink () {return  nitrogenFineRootsSink;}
	public double getNitrogenCoarseRootsSink () {return  nitrogenCoarseRootsSink;}
	public double getNitrogenFruitSink () {return  nitrogenFruitSink;}
	public double getNitrogenStemIncrement() {return  nitrogenStemIncrement;}
	public double getNitrogenBranchesIncrement () {return  nitrogenBranchesIncrement;}
	public double getNitrogenFoliageIncrement () {return  nitrogenFoliageIncrement;}
	public double getNitrogenStumpIncrement () {return  nitrogenStumpIncrement;}
	public double getNitrogenFineRootsIncrement() {return  nitrogenFineRootsIncrement;}
	public double getNitrogenCoarseRootsIncrement () {return  nitrogenCoarseRootsIncrement;}
	public double getNitrogenFruitIncrement () {return  nitrogenFruitIncrement;}

	public double getTargetCarbonStem () {return  targetCarbonStem;}
	public double getTargetStemVolume () {return  targetStemVolume;}

	public double getTargetHeight () {return  targetHeight;}
	public double getTargetDbh () {return  targetDbh;}
	public double getTargetCrownDepth () {return  targetCrownDepth;}
	public double getTargetCrownVolume () {return  targetCrownVolume;}
	public double getTargetCarbonBranches () {return  targetCarbonBranches;}
	public double getTargetCarbonFoliage () {return  targetCarbonFoliage;}
	
	public double getAllometricHeight () {
		double aTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		return (aTree * Math.pow (getDbhMeters(),bTree));
	}
	 
	public double getAllometricDbh () {
		double aTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		return (Math.pow (((1/aTree) * getHeight()) , (1/bTree)))*100;
	}
	
	public double getAllometricCrownVolume() {
		double aStemVolume 			= this.getTreeSpecies ().getStemDbhAllometricCoeffA ();
		double bStemVolume 			= this.getTreeSpecies ().getStemDbhAllometricCoeffB ();
		double cStemVolume 			= this.getTreeSpecies ().getStemDbhAllometricCoeffC ();
		return (Math.exp (aStemVolume) * Math.pow (getDbhMeters (),bStemVolume)
		               * Math.pow (getHeight(),cStemVolume));		
	}

	//for annual export 
	//return the total volume of rooted voxels
	public double getRootedVolume(){
		double volume = 0;
		if (getPlantRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();

				volume += voxel.getVolume();	
			}	
		}
		return volume; 
	}

	//for annual export 
	//return the total volume of rooted voxels per layer 
	public double[] getRootedVolumePerLayer(){
		double[] volume = new double[5];
		if (getPlantRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				int layerid = voxel.getLayer().getId();
				volume[layerid] += voxel.getVolume();	
			}	
		}
		return volume; 
	}
	public int getRootedVolumePerLayerSize () {
		return 5;
	}	
	//for annual export 
	//return the distance between trunk and most far rooted voxel on tree line
	public double getMaxRootDistanceOnTreeLine(){
		double distance = 0;
	
		if (getPlantRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				//IL 28-11-2017
				//je prends la partie entiÃ¯Â¿Â½re car si l'arbre se trouve sur le bord de la cellule le test ne marche plus
				if ((int)voxel.getX() == (int)this.getX()) {	
					double ecart = voxel.getY() - this.getY();	
					if (ecart < 0) ecart = ecart * -1.0d;				
					distance = Math.max(distance,ecart);	
				}
			}	
		}
		return distance; 
	}	

	//for annual export 
	//return the distance between trunk and most far rooted voxel on crop line
	public double getMaxRootDistanceOnCropLine(){
		double distance = 0;
	
		if (getPlantRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();		
				//IL 28-11-2017
				//je prends la partie entiÃ¯Â¿Â½re car si l'arbre se trouve sur le bord de la cellule le test ne marche plus
				if ((int)voxel.getY() == (int)this.getY()) {	
					double ecart = voxel.getX() - this.getX();
					if (ecart < 0) ecart = ecart * -1.0d;
					distance = Math.max(distance,ecart);				
				}
			}	
		}
		return distance; 
	}
	
	// for export return depth of deeper rooted voxels
	public double getRootingDepth(){
		if (getPlantRoots() != null && getPlantRoots().getFirstRootNode() != null)
		return getPlantRoots().getFirstRootNode().getDeeperSonDepth();
		else return 0;
	}


	//FINE ROOTS
	public SafePlantRoot getPlantRoots() {return plantRoots;}

	public double getTotalRootLength () {
		if (this.getPlantRoots() != null)
			return this.getPlantRoots().getTotalRootsLength();
		else
			return 0;
	}

	public double getTotalRootLength2 () {

		double carbonToDryMatter= 1d / this.getTreeSpecies ().getWoodCarbonContent ();
		return( this.getCarbonFineRoots()
				* carbonToDryMatter 									//convert C to dry matter
				* this.getTreeSpecies().getSpecificRootLength() 		//convert grammes of dry matter to m
				* 1000);													//convert kg to gr
		
	}
	
	public double getTotalOptiN () {

		//Optimum level of (structural) N for whole plant
		double optiNCStem 	 	= this.getTreeSpecies ().getOptiNCStem ();
		double optiNCStump		= this.getTreeSpecies ().getOptiNCStump();
		double optiNCFoliage 	= this.getTreeSpecies ().getOptiNCFoliage ();
		double optiNCBranch 	= this.getTreeSpecies ().getOptiNCBranch ();
		double optiNCCoarseRoot = this.getTreeSpecies ().getOptiNCCoarseRoot ();
		double optiNCFineRoot 	= this.getTreeSpecies ().getOptiNCFineRoot ();
	
		return (getCarbonBranches() 	* optiNCBranch
			    + getCarbonCoarseRoots() * optiNCCoarseRoot
				+ getCarbonFineRoots() 	* optiNCFineRoot
				+ getTotalCarbonFoliage() 	* optiNCFoliage
				+ getCarbonStem ()		* optiNCStem
				+ getCarbonStump()		* optiNCStump);
	}
	
	
	public double getTotalOptiNStem  () {

		return (getCarbonStem ()* this.getTreeSpecies ().getOptiNCStem ());
	}
	
	public double getTotalOptiNStump () {

		return (getCarbonStump ()* this.getTreeSpecies ().getOptiNCStump ());
	}
	
	public double getTotalOptiNFoliage () {

		return (getTotalCarbonFoliage()* this.getTreeSpecies ().getOptiNCFoliage ());
	}
	
	public double getTotalOptiNBranches () {

		return (getCarbonBranches()* this.getTreeSpecies ().getOptiNCBranch ());
	}
	
	public double getTotalOptiNCoarseRoots () {

		return (getCarbonCoarseRoots()* this.getTreeSpecies ().getOptiNCCoarseRoot ());
	}
	
	public double getTotalOptiNFineRoots () {

		return (getCarbonFineRoots()* this.getTreeSpecies ().getOptiNCFineRoot ());
	}
	
	public double getTotalN () {
		
		return  (getNitrogenBranches()
			  + getNitrogenCoarseRoots()
			  + getNitrogenFineRoots()
			  + getTotalNitrogenFoliage()
			  + getNitrogenStem()
			  + getNitrogenStump()
			  + getNitrogenLabile());
	}
	
	public double getRootShootRatio () {
		if (carbonStem + carbonBranches + getTotalCarbonFoliage() > 0)
		return (carbonStump + carbonFineRoots + carbonCoarseRoots) / 
				(carbonStem + carbonBranches + getTotalCarbonFoliage());
		else return 0;
	}
	/**
	* return total structural carbon
	*/
	public double getTotalStructuralCarbon() {

		return (carbonBranches + getTotalCarbonFoliage() + carbonStem + carbonStump +
				+ carbonCoarseRoots + carbonFineRoots);
	}
	
	
	/**
	* return biomasse
	*/
	public double treeWoodyDM() {

		return (carbonBranches  + carbonStem + carbonStump +
				+ carbonCoarseRoots +carbonLabile) / this.treeSpecies.getWoodCarbonContent();
	}
	
	public double treeNonWoodyDM() {

		return (getTotalCarbonFoliage() +carbonFineRoots) / this.treeSpecies.getLeafCarbonContent();
	}
	
	public double treeDM() {

		return (treeWoodyDM() +treeNonWoodyDM());
	}
	
	//for export
	public int getIdTree() {return getId();}

	public int getNbrDaysSinceLastIrrigation() {return nbrDaysSinceLastIrrigation;}
	public int getNbrDaysSinceLastFertilization() {return nbrDaysSinceLastFertilization;}
	
	public int getNbrDaysInShade() {return nbrDaysInShade;}
	public void addNbrDaysInShade() {nbrDaysInShade++;}
	public void setNbrDaysInShade(int i) {nbrDaysInShade=i;}
	
	public double getWaterUptakePotential () {
		if (plantRoots==null) return 0;
		return plantRoots.getWaterUptakePotential();
	}

  	public double getTotalVoxelCarbonCoarseRoot() {
  		double coarseRoot = 0;
  		int index = this.getId()-1;
		if (getPlantRoots().getRootTopology() == null) return coarseRoot;
//			//FOR EACH VOXEL in coarse root topology map
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getPlantRoots().getRootTopology (voxel);
				coarseRoot += voxel.getTheTreeCarbonCoarseRoots(index);
			}
			return coarseRoot;
  	}
  	
  	public double getTotalVoxelFineRootDensity() {
  		double fineRoot = 0;
  		int index = this.getId()-1;
		if (getPlantRoots().getRootTopology() == null) return fineRoot;
//			//FOR EACH VOXEL in coarse root topology map
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getPlantRoots().getRootTopology (voxel);
				fineRoot += voxel.getTheTreeRootsDensity(index);
			}
			return fineRoot;
  	}
  	
  	public void drawRootTopology() {
  		System.out.println("*******************drawRootTopology");
		if (getPlantRoots().getRootTopology() == null) return;

		
//			//FOR EACH VOXEL in coarse root topology map
			for (Iterator c = getPlantRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getPlantRoots().getRootTopology (voxel);
			System.out.println("cell="+voxel.getCell().getId()+" node="+node);


			}
  	}

  	public void drawRootMap() {
  		System.out.println("*******************drawRootMap");
		if (getPlantRoots().getRootedVoxelMap() == null) return;

		
//			//FOR EACH VOXEL in coarse root topology map
			for (Iterator c = getPlantRoots().getRootedVoxelMap().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootVoxel rootVoxel = getPlantRoots().getRootedVoxelMap (voxel);
			System.out.println("cell="+voxel.getCell().getId()+" rootVoxel="+rootVoxel);


			}
  	}

	public double getLeafAreaDensity() {
		if (this.getTotalLeafArea()==0) return 0; 
		else return this.getTotalLeafArea()/this.getCrownVolume();
	}

	
}
