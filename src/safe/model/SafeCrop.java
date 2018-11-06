package safe.model;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Iterator;

import safe.stics.*;


/**
 * SafeCrop represent the crop sowed on one cell of the plot (can be baresoil)
 * Only one SafeCrop is created by cell (state variables of the crop are homogenous)
 * Crop model is implemented by STICS (calling fortran native method) 
 *
 * @author Isabelle Lecomte - july 2002
 */

 public class SafeCrop  implements Serializable {

 	private SafeCropSpecies cropSpecies;			// crop species (can be baresoil)
 	private SafeCell cell;						    // cell object reference
 	private int cropAge;
 	
 	//JNA object
	public SafeSticsCrop sticsCrop;					//plant object  in STICS
	public SafeSticsCommun sticsCommun;				//all commun variable in STICS
	public SafeSticsSoil sticsSoil;					//soil variable in STICS
	public SafeSticsClimat sticsClimat;				//climat in STICS
	
	//REINITIALISATION
	public SafeSticsReinit sticsReinit;				//object to store reinit datas
	
	//Root topology 
	private SafeFineRoot fineRoots;			//refer to fine root object

	private boolean isMainCropSpecies;
	private boolean isPerennial;
	private String cropType;
	private int startDay;						// First day of simulation  DOY
	private String itkfileName;					// itk file name 
 	private int sowingDay;						// Date of sowing           DOY
 	private int emergenceDay;					// Date of emergence        DOY
 	private int floweringDay;					// Date of flowering        DOY
 	private int harvestDay;						// Date of harvest          DOY
 	private int   sticsDay;						// Date of STICS simulation DOY
 	private float lai;							// m2.m-2
 	private float eai;							// m2.m-2
 	private float sla;							// cm2 g-1
 	private float biomass;						// Aboveground dry matter (masec) t.ha-1
 	private float grainBiomass;					// Grain dry matter (magrain) t.ha-1
 	private float yield;						// Yield t.ha-1 
 	private float height;						// Height of canopy (hauteur) mm
 	private float grainNumber;					// nbr m-2
 	private float grainWeight;					// g
 	private float plantDensity;					// nbr m-2
 	private float sowingDepth;					// m
 	private float rootDepth;					// m
 	private float cropTemperature;				// degree C
 	private float cropMaxTemperature;			// degree C
 	private float cropMinTemperature;			// degree C
 	private float soilSurfaceTemperature;		// degree C
 	private float soilManagementDepth;			// m
 	/* Phenological stage
	1	SNU 	BareSoil
	2 	PLT 	Plantation
	3 	DOR 	Dormancy
	4 	LEV 	emergence
	5 	AMF 	maximum acceleration of leaf growth, end of juvenile phase
	6 	LAX 	maximum leaf area index, end of leaf growth
	7 	FLO 	day of anthesis
	8 	DRP 	starting date of filling of harvested organs
	9 	DES 	date of onset of water dynamics in harvested organs
	10 	MAT 	maturity
	11 	REC 	Harvest
	12 	SEN 	beginning of leaf senescence
	13	LAN		Lai Null
	*/
	private int phenologicStage;	

	private float resperenne;				//C crop reserve for perenial crops) t ha-1
	private float albedoLai;				//Albedo of the crop cobining soil with vegetation  SD	
	private float cNgrain;					//Nitrogen concentration of grains %
	private float cNplante;					//Nitrogen concentration of entire plant %
	private float qNgrain;					//Amount of nitrogen in harvested organs (grains / fruits)  kg ha-1
	private float qNplante;					//Amount of nitrogen taken up by the plant   kgN.ha-1

	// Light model output
	private float 	captureFactorForDiffusePar;				// m2 m-2 d-1
	private float	captureFactorForDirectPar;				// m2 m-2 d-1
	private float 	directParIntercepted;					// Moles PAR m-2 d-1
	private float 	diffuseParIntercepted;					// Moles PAR m-2 d-1
	private float 	totalParInterceptedMJ;					// MJ m-2	used to force Stics
	private float	competitionIndexForTotalPar;			// unitless
	private float  	parIntCum;								// Moles m-2
	private float	parIntCumByCropPhenoStage;				// Moles m-2
	private float 	interceptedPar;							// imported from Stics (verif) // GT 6/03/2008

	// extinction coefficient for Par interception by crop
	private float parExtinctionCoef;				// calculated
	private float extin;							// extinction coefficient for Stics formalism

 	//Water budget inputs
	private float capillaryRise;			// mm
	private float irrigation;				// mm

	//Water budget outputs
 	private float waterDemand;				// mm
 	private float waterDemandReduced;		// mm
	private float waterUptake;				// mm
	private float soilEvaporation;			// mm
	private float runOff;					// mm
	private float surfaceRunOff;			// mm

	//Water stress
	private float hisafeWaterStress;			// calculated by hisafe 
	private float sticsWaterStomatalStress;      // Swfac
	private float sticsWaterTurgescenceStress;   // Turfac
	private float sticsWaterSenescenceStress;    // Senfac

	//Nitrogen budget inputs kg N ha-1
	private float nitrogenRain;
	private float nitrogenIrrigation;
	private float nitrogenFertilisationMineral;
	private float nitrogenFertilisationOrganic;
	private float nitrogenFixation;
	private float nitrogenDenitrification;
	private float nitrogenHarvested;
	private float biomassHarvested;


	//Residus quality  //AQ - 05.2011
	private float cnResidus;
	private float cropCarbonLeafLitter;		//cumulative mount of C in fallen leaves (QCplantetombe+QCrogne) (kgC.ha-1) 
	private float cropNitrogenLeafLitter;	//cumulative mount of N in fallen leaves (QNplantetombe+QNrogne) (kgN.ha-1) 
	private float cropCarbonRootLitter;		//cumulative amount of C in dead roots added to soil (QCrac)  (kgC.ha-1) 
	private float cropNitrogenRootLitter;	//cumulative amount of N in dead roots added to soil (QNrac)  (kgN.ha-1 )

	//Values of previous day to get rid of STICS cumulated data
	private float nitrogenIrrigationPrev; 
	private float nitrogenFertilisationMineralPrev;	
	private float nitrogenFertilisationOrganicPrev;
	private float cropCarbonLeafLitterPrev;		
	private float cropNitrogenLeafLitterPrev;	
	private float cropCarbonRootLitterPrev;		
	private float cropNitrogenRootLitterPrev;	
	private float capillaryRisePrev;
	private float nitrogenHarvestedPrev;
	private float biomassHarvestedPrev;
	
	//Nitrogen budget outputs kg N ha-1
	private float nitrogenDemand;
	private float nitrogenUptake;			
	private float biomassRestitution;
   	private float nitrogenLeachingBottom;
   	private float nitrogenLeachingArtificial;
   	
	private float nitrogenLeachingWaterTable;	//(AQ - 05.2011)
	private float nitrogenAddedByWaterTable;
	private float nitrogenImmobilisation;
	private float nitrogenVolatilisation;
	private float nitrogenVolatilisationOrganic;	
	private float nitrogenNutritionIndex;				// inn
	
	//Nitrogen stress
	private float hisafeNitrogenStress;				// calculated by hisafe
	
	private float sticsNitrogenLaiStress;  			// innlai
	private float sticsNitrogenBiomassStress;     	// inns
	private float sticsNitrogenSenescenceStress;  	// innsenes

	
	// Stock de carbone et azote des résidus (issus de STICS)
	private float carbonResidus;					//kgC.ha-1
	private float nitrogenResidus;					//kgN.ha-1
	
	//Tree SURFACE litter (above profHum)  
	//calculated by Hi-sAFe and traited by STICS for mineralisation
	private float treeCarbonLeafLitter;
	private float treeNitrogenLeafLitter;
	private float treeCarbonFineRootLitter;
	private float treeNitrogenFineRootLitter;
	private float treeCarbonCoarseRootLitter;
	private float treeNitrogenCoarseRootLitter;

	//Tree DEEP litter (bollow profHum)   
	//calculated and traited by Hi-sAFe with special mineralisation method (Safevoxel.deepSenescentRootsMineralization)
	
	private float treeCarbonFineRootDeepLitter;
	private float treeNitrogenFineRootDeepLitter;
	private float treeCarbonCoarseRootDeepLitter;
	private float treeNitrogenCoarseRootDeepLitter;
	
	//MAX values for EXPORT
 	private float laiMax;						// m2.m-2
 	private float eaiMax;						// m2.m-2
 	private float yieldMax;						// t.ha-1 (0 % water)
 	private float biomassMax;					// t.ha-1 
 	private float rootDepthMax;					// m
 	private float heightMax;					// m
 	
	//Monthly values for EXPORT
	private float monthBiomass;
	private float monthYield;
	private float monthEai;
	private float monthLai;
	private float monthDiffuseParIntercepted;
	private float monthDirecParIntercepted;
	private int monthNbrDays;
	
	//totals for EXPORT
	private float totalCapillaryRise;			// mm
	private float totalIrrigation;				// mm
 	private float totalWaterDemand;				// mm
 	private float totalWaterDemandReduced;		// mm
	private float totalWaterUptake;				// mm
	private float totalNitrogenDemand;			//kg N ha-1
	private float totalNitrogenUptake;			//kg N ha-1
	private float totalSoilEvaporation;			// mm
	private float totalRunOff;					// mm
	private float totalSurfaceRunOff;			// mm
	private float totalDrainageBottom;			// mm
	private float totalDrainageArtificial;		// mm
	private float totalRain;					// mm
	private float totalParIntercepted;			// Moles PAR m-2 d-1
	

 	public SafeCrop () {

 		this.cropSpecies = null;
 		this.sticsSoil = null;
 		this.sticsCommun = null;	
 		this.cropAge = 0;
 		this.lai = 0;
 		this.eai = 0;
 		this.extin = 0;
 		this.parExtinctionCoef = 0;
 		this.albedoLai = 0;
 		this.biomass = 0;
 		this.rootDepth = 0;
 		this.height = 0;
 		this.cropTemperature = 0;
 		this.cropMaxTemperature = 0;
 		this.cropMinTemperature = 0;
 		this.soilSurfaceTemperature = 0;
		this.yield = 0;
		this.grainNumber = 0;
		this.grainWeight = 0;
		this.plantDensity = 0;
		this.sowingDay = 0;
		this.harvestDay = 0;
		this.sowingDepth = 0;
		this.waterDemand = 0;
		this.waterDemandReduced = 0;
		this.soilEvaporation = 0;
		this.waterUptake = 0;
		this.hisafeWaterStress = 1;
		this.hisafeNitrogenStress = 1;
 		this.nitrogenDemand = 0;
		this.phenologicStage= 0;
		this.biomassRestitution = 0;
		this.sla = 0;
		this.captureFactorForDiffusePar= 0;
		this.captureFactorForDirectPar= 0;
		this.directParIntercepted= 0;
		this.diffuseParIntercepted= 0;
		this.totalParInterceptedMJ= 0;
		this.competitionIndexForTotalPar= 1;
		this.parIntCum= 0;
		this.parIntCumByCropPhenoStage= 0;
		this.interceptedPar=0;

		//---------------------------------
		// ajout sitraka carbone des résidus----
		this.carbonResidus = 0;
		this.nitrogenResidus = 0;
		this.treeCarbonLeafLitter =0;
		this.treeNitrogenLeafLitter =0;
		this.treeCarbonFineRootLitter = 0;
		this.treeNitrogenFineRootLitter =0;
		this.treeCarbonCoarseRootLitter = 0;
		this.treeNitrogenCoarseRootLitter = 0;
		this.treeCarbonFineRootDeepLitter = 0;
		this.treeNitrogenFineRootDeepLitter =0;
		this.treeCarbonCoarseRootDeepLitter = 0;
		this.treeNitrogenCoarseRootDeepLitter = 0;

		this.nitrogenLeachingBottom =0;	
		this.nitrogenLeachingArtificial=0;
		this.nitrogenLeachingWaterTable =0;
		this.nitrogenAddedByWaterTable =0;

		this.razTotalWater();
		
		//Initialise roots informations
		this.fineRoots = new SafeFineRoot (this);
		
		//by defaut main crop species
		this.isMainCropSpecies = true;

	}
	/**
	* Constructor for cloning a crop from an old one
	*/
	public SafeCrop (SafeCrop oldCrop, boolean visible, SafeCell newCell) {

		this.cell				= newCell;		
		this.cropSpecies 		= oldCrop.cropSpecies;
		this.cropAge 			= oldCrop.cropAge; 
		
		//on recopie la référence des objets de l'ancienne cellule 
		this.sticsCrop			= oldCrop.sticsCrop;
		this.sticsSoil 			= oldCrop.sticsSoil;
		this.sticsCommun 		= oldCrop.sticsCommun;
		this.sticsClimat 		= oldCrop.sticsClimat;
		
		this.isMainCropSpecies  = oldCrop.isMainCropSpecies;
		this.itkfileName		= oldCrop.itkfileName;
		this.sowingDay   	   	= oldCrop.sowingDay;
		this.harvestDay   	   	= oldCrop.harvestDay;
		this.sticsDay   	   	= oldCrop.sticsDay;
		this.lai 				= oldCrop.lai;
		this.eai				= oldCrop.eai;
		this.extin				= oldCrop.extin;
		this.parExtinctionCoef  = oldCrop.parExtinctionCoef;
		this.albedoLai 			= oldCrop.albedoLai;
		this.biomass 			= oldCrop.biomass;
		this.yield = 0;
		this.height 			= oldCrop.height;
		this.grainNumber 		= oldCrop.grainNumber;
		this.plantDensity 		= oldCrop.plantDensity;
		this.grainWeight 		= oldCrop.grainWeight;
		this.rootDepth 			= oldCrop.rootDepth;
		this.sowingDepth 		= oldCrop.sowingDepth;
		this.cropTemperature 	= oldCrop.cropTemperature;
		this.cropMaxTemperature 	= oldCrop.cropMaxTemperature;
		this.cropMinTemperature 	= oldCrop.cropMinTemperature;
		this.soilSurfaceTemperature = oldCrop.soilSurfaceTemperature;
		this.phenologicStage	= oldCrop.phenologicStage;
		this.sla				= oldCrop.sla;
		this.resperenne     	= oldCrop.resperenne;
		this.grainBiomass        = oldCrop.grainBiomass;

		this.soilEvaporation	= 0;
		this.waterUptake		= 0;
		this.waterDemand		= 0;
		this.waterDemandReduced = 0;
		this.nitrogenLeachingBottom = 0;
		this.nitrogenLeachingArtificial = 0;
		this.nitrogenLeachingWaterTable = 0;
		this.nitrogenAddedByWaterTable = 0;

		this.captureFactorForDiffusePar	= 0;
		this.captureFactorForDirectPar	= 0;
		this.directParIntercepted		= 0;
		this.diffuseParIntercepted		= 0;
		this.totalParInterceptedMJ		= 0;
		this.competitionIndexForTotalPar= 1;
		this.parIntCum					= oldCrop.parIntCum;
		this.parIntCumByCropPhenoStage	= oldCrop.parIntCumByCropPhenoStage;
		this.interceptedPar				= oldCrop.interceptedPar;

		//--------------------------------
		// ajout sitraka carbone residus----------
		this.carbonResidus = oldCrop.carbonResidus;
		this.nitrogenResidus = oldCrop.nitrogenResidus;
		
		this.treeCarbonLeafLitter = oldCrop.treeCarbonLeafLitter;
		this.treeNitrogenLeafLitter = oldCrop.treeNitrogenLeafLitter;
		this.treeCarbonFineRootLitter =oldCrop.treeCarbonFineRootLitter;
		this.treeNitrogenFineRootLitter =oldCrop.treeNitrogenFineRootLitter;
		this.treeCarbonCoarseRootLitter = oldCrop.treeCarbonCoarseRootLitter;
		this.treeNitrogenCoarseRootLitter = oldCrop.treeNitrogenCoarseRootLitter;
		this.treeCarbonFineRootDeepLitter = oldCrop.treeCarbonFineRootDeepLitter;
		this.treeNitrogenFineRootDeepLitter = oldCrop.treeNitrogenFineRootDeepLitter;
		this.treeCarbonCoarseRootDeepLitter = oldCrop.treeCarbonCoarseRootDeepLitter;
		this.treeNitrogenCoarseRootDeepLitter = oldCrop.treeNitrogenCoarseRootDeepLitter;
		this.biomassRestitution = oldCrop.biomassRestitution;
		this.nitrogenDemand			= oldCrop.nitrogenDemand;	//to calculate  NitrogenSinkStrength
		this.hisafeWaterStress 		= oldCrop.hisafeWaterStress;
		this.hisafeNitrogenStress 	= oldCrop.hisafeNitrogenStress;

		//recopy fine roots informations
		this.fineRoots = new SafeFineRoot (oldCrop.fineRoots);

		//for annual export 
		if (oldCrop.lai > oldCrop.laiMax) 
			this.laiMax = oldCrop.lai ; 	
		else
			this.laiMax = oldCrop.laiMax ; 
	
		if (oldCrop.eai > oldCrop.eaiMax) 
			this.eaiMax = oldCrop.eai ; 	
		else
			this.eaiMax = oldCrop.eaiMax ; 
	
		if (oldCrop.rootDepth > oldCrop.rootDepthMax) 
			this.rootDepthMax = oldCrop.rootDepth ; 	
		else
			this.rootDepthMax = oldCrop.rootDepthMax ; 

		if (oldCrop.yield > oldCrop.yieldMax) 
			this.yieldMax = oldCrop.yield ; 	
		else
			this.yieldMax = oldCrop.yieldMax ; 

		
		if (oldCrop.biomass > oldCrop.biomassMax) 
			this.biomassMax = oldCrop.biomass ; 	
		else
			this.biomassMax = oldCrop.biomassMax ; 
		
		if (oldCrop.height > oldCrop.heightMax) 
			this.heightMax = oldCrop.height ; 	
		else
			this.heightMax = oldCrop.heightMax ;
		
		//Monthly values for EXPORT
		this.monthBiomass = oldCrop.monthBiomass + oldCrop.biomass;
		this.monthYield = oldCrop.monthYield + oldCrop.yield;
		this.monthEai 	= oldCrop.monthEai + oldCrop.eai;
		this.monthLai	= oldCrop.monthLai + oldCrop.lai;
		this.monthDiffuseParIntercepted = oldCrop.monthDiffuseParIntercepted + oldCrop.diffuseParIntercepted;
		this.monthDirecParIntercepted 	= oldCrop.monthDirecParIntercepted + oldCrop.directParIntercepted;	
		this.monthNbrDays = oldCrop.monthNbrDays + 1; 
		
		
		//Totals for EXPORT
		this.totalIrrigation 			= oldCrop.totalIrrigation;
		this.totalWaterDemand 			= oldCrop.totalWaterDemand;
		this.totalWaterDemandReduced	= oldCrop.totalWaterDemandReduced;
		this.totalWaterUptake 			= oldCrop.totalWaterUptake;
		this.totalNitrogenDemand 		= oldCrop.totalNitrogenDemand;
		this.totalNitrogenUptake 		= oldCrop.totalNitrogenUptake;
		this.totalSoilEvaporation 		= oldCrop.totalSoilEvaporation;
		this.totalRunOff 				= oldCrop.totalRunOff;
		this.totalSurfaceRunOff 		= oldCrop.totalSurfaceRunOff;
		this.totalDrainageBottom 		= oldCrop.totalDrainageBottom;
		this.totalDrainageArtificial    = oldCrop.totalDrainageArtificial;
		this.totalRain					= oldCrop.totalRain;
		this.totalParIntercepted        = oldCrop.totalParIntercepted;
		
		//To get daily values from STICS cumulated data
		this.nitrogenIrrigationPrev 			= oldCrop.nitrogenIrrigation; 
		this.nitrogenFertilisationMineralPrev 	= oldCrop.nitrogenFertilisationMineral;  
		this.nitrogenFertilisationOrganicPrev 	= oldCrop.nitrogenFertilisationOrganic; 
		this.capillaryRisePrev 					= oldCrop.capillaryRise;
		this.cropCarbonLeafLitterPrev = oldCrop.cropCarbonLeafLitter;		
		this.cropNitrogenLeafLitterPrev = oldCrop.cropNitrogenLeafLitter;	
		this.cropCarbonRootLitterPrev = oldCrop.cropCarbonRootLitter;		
		this.cropNitrogenRootLitterPrev = oldCrop.cropNitrogenRootLitter;	
		this.nitrogenHarvestedPrev = oldCrop.nitrogenHarvested;		
		this.biomassHarvestedPrev = oldCrop.biomassHarvested;	

	}
	
	/**
	* Destruction of STICS objects references (to clean memory) 
	*/
	public void razSticsObjects () {
		//on supprime les objets STICS 
		//et on stocke ce qui qui peux reservir si on réouvre le projet  dans sticsReinit
		this.sticsReinit        = new SafeSticsReinit(this.sticsCommun, this.sticsSoil);
		this.sticsCrop			= null;
		this.sticsCommun 		= null;
		this.sticsClimat		= null;
		this.sticsSoil			= null; 
		
	}
	
	/**
	* RAZ of month values
	*/
	public void razTotalMonth () {
		monthBiomass = 0; 
		monthYield = 0;
		monthEai = 0;
		monthLai = 0;
		monthDiffuseParIntercepted = 0;
		monthDirecParIntercepted = 0;
		monthNbrDays = 0;
	}
	
	/**
	* RAZ of max values
	*/
	public void razMaxValues () {
	 	laiMax = 0; 
	 	eaiMax = 0;
		yieldMax = 0;
		rootDepthMax = 0;
		biomassMax = 0;
		heightMax = 0;
	}
	
	/**
	 * RAZ Total values for water budget
	 **/
	public void razTotalWater () {
		this.totalCapillaryRise = 0;
		this.totalIrrigation = 0;
		this.totalWaterDemand = 0;
		this.totalWaterDemandReduced= 0;
		this.totalWaterUptake = 0;
		this.totalNitrogenDemand = 0;
		this.totalNitrogenUptake = 0;
		this.totalSoilEvaporation = 0;
		this.totalRunOff = 0;
		this.totalSurfaceRunOff = 0;
		this.totalDrainageBottom = 0;
		this.totalDrainageArtificial = 0;
		this.totalRain = 0;
		this.totalParIntercepted = 0;
	}
	
	/**
	 * STICS crop first initialisation with the sown crop species 
	 */
	public  void cropInitialisation (SafeTestJNA jna, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
									SafeSoil soil, 
									SafeCropSpecies cropSpecies,
									String cropSpeciesfileName,
									String itkfileName, 
									SafeInitialValues initialValues,									
									String exportDir,
									String laiFileName,
									boolean exportStiFile)  throws Exception {

		//soil and commum variable creation 
		if (sticsReinit == null) {
			this.sticsSoil 		= new SafeSticsSoil (soil);
			this.sticsSoil.initialise (soil, initialValues); 
		}
		//on ne recréé pas l'objet SOL
		//on le ré-initialise avec les valeurs avec celles de fin de simulation précédente
		else {
			this.sticsSoil.reinitialise (sticsReinit); 	
		}
		
		this.sticsCommun 	= new SafeSticsCommun();	
		this.sticsCrop 		= new SafeSticsCrop();
		cropSpecies.sticsItk= new SafeSticsItk ();
		this.setCropSpecies(cropSpecies);
		this.itkfileName = itkfileName;

		//JNA NATIVE method to check general parameters and soil
		//result is in output/initialisation.sti
		if (exportStiFile) jna.verifParam (sticsParam, sticsTransit, this.sticsSoil, this.sticsCommun, exportDir);

		// Loading crop species and crop intervention file 
		try {
			new SafeSticsCropFormat (cropSpeciesfileName).load (this.sticsCrop , cropSpecies);
		} catch (Exception e1) {
			System.out.println("crop species NOK e1="+e1);
		}
		
		try {
			new SafeSticsItkFormat (itkfileName).load (cropSpecies.sticsItk, initialValues, IsMainCropSpecies());
		} catch (Exception e2) {
			System.out.println("crop ITK NOK");
		}
		
		//Enchainement des simulations dans STICS = NON
		sticsCommun.P_codesuite = 0;
		
		cropSpecies.sticsItk.ipl = 1;
		this.sticsCommun.numcult = 1;
		
	    if(sticsTransit.P_option_thinning== 1)
	    	cropSpecies.sticsItk.flag_eclairmult=true;
	    else
	    	cropSpecies.sticsItk.flag_eclairmult=false;

	    if(sticsTransit.P_option_engrais_multiple == 1 )
	    	cropSpecies.sticsItk.flag_plusieurs_engrais=true;
	    else
	    	cropSpecies.sticsItk.flag_plusieurs_engrais=false;
	
	    if(sticsTransit.P_option_pature == 1 )
	    	cropSpecies.sticsItk.flag_pature=true;
	    else
	    	cropSpecies.sticsItk.flag_pature=false;

	    //if we erase this, fertilisation and irrigation will be erased in STICS
	    sticsCommun.napini[0] = cropSpecies.sticsItk.nap;
	    sticsCommun.napNini[0] = cropSpecies.sticsItk.napN;
	    sticsCommun.nbjresini[0] = cropSpecies.sticsItk.P_nbjres;
	    
	    //Initial values  
		if (IsMainCropSpecies()) {
		
			this.sticsCrop.P_stade0 =  initialValues.mainCropStage; 
			this.sticsCrop.P_lai0 = (float) initialValues.mainCropLai;
			this.sticsCrop.P_masec0 = (float) initialValues.mainCropBiomass;
			this.sticsCrop.P_zrac0 = (float) (initialValues.mainCropRootDepth * 100);	//convert in cm
			this.sticsCrop.P_magrain0 = (float) initialValues.mainCropGrainBiomass;
			this.sticsCrop.P_QNplante0 = (float) initialValues.mainCropNitrogen;
			this.sticsCrop.P_resperenne0 = (float) initialValues.mainCropReserveBiomass;
			this.sticsCrop.P_densinitial[0] = (float) initialValues.mainCropRootDensity [0];
			this.sticsCrop.P_densinitial[1] = (float) initialValues.mainCropRootDensity [1];
			this.sticsCrop.P_densinitial[2] = (float) initialValues.mainCropRootDensity [2];
			this.sticsCrop.P_densinitial[3] = (float) initialValues.mainCropRootDensity [3];
			this.sticsCrop.P_densinitial[4] = (float) initialValues.mainCropRootDensity [4];

		}
		else {
			this.sticsCrop.P_stade0 =  initialValues.interCropStage;
			this.sticsCrop.P_lai0 = (float) initialValues.interCropLai;
			this.sticsCrop.P_masec0 = (float) initialValues.interCropBiomass;
			this.sticsCrop.P_zrac0 = (float) (initialValues.interCropRootDepth * 100);	//convert in cm
			this.sticsCrop.P_magrain0 = (float) initialValues.interCropGrainBiomass;
			this.sticsCrop.P_QNplante0 = (float) initialValues.interCropNitrogen;
			this.sticsCrop.P_resperenne0 = (float) initialValues.interCropReserveBiomass;
			this.sticsCrop.P_densinitial[0] = (float) initialValues.interCropRootDensity [0];
			this.sticsCrop.P_densinitial[1] = (float) initialValues.interCropRootDensity [1];
			this.sticsCrop.P_densinitial[2] = (float) initialValues.interCropRootDensity [2];
			this.sticsCrop.P_densinitial[3] = (float) initialValues.interCropRootDensity [3];
			this.sticsCrop.P_densinitial[4] = (float) initialValues.interCropRootDensity [4];
		}

		   
		this.sticsCommun.P_codesimul=1;
		
		//on force le LAI des culture avec un fichier en entrée
		if (laiFileName != "") {
			new SafeSticsLaiFormat (laiFileName).load (this.sticsCrop);
			this.sticsCommun.P_codesimul=2;
		}
		
		//JNA NATIVE method to check plant and itk parameters
		//result is in output/initialisation.sti
		if (exportStiFile) jna.verifPlante(sticsParam, sticsTransit, this.sticsCommun, cropSpecies.sticsItk, this.sticsCrop, IsMainCropSpecies(), exportDir);
		
		//IL 25-04-2018
		//repris de STICS verifPlante (si on le zappe ben ça marche plus) 
		this.sticsCrop.ipl = 1;
		this.cropSpecies.sticsItk.ipl = 1;
		if (this.cropSpecies.sticsItk.P_ressuite == 0) this.cropSpecies.sticsItk.P_ressuite = 1;

		//Perenial crop ?
		isPerennial = false; 
		if (this.sticsCrop.P_codeperenne == 2) isPerennial= true;
	
		return;
	}

	/**
	 * STICS initialisation reload with a new crop species 
	 * SOIL IS NOT ERASED !
	 */
	public  void cropReload (SafeTestJNA jna, 
							SafeSticsParameters sticsParam, 
							SafeSticsTransit sticsTransit, 
							SafeSoil soil, 
							SafeCropSpecies cropSpecies,
							String cropSpeciesfileName,
							String itkfileName, 
							SafeInitialValues initialValues,
							String exportDir,
							boolean exportStiFile)  throws Exception {

		float rootDensity0 = 0;
		float rootDensity1 = 0;
		float rootDensity2 = 0;
		float rootDensity3 = 0;
		float rootDensity4 = 0;

		//save root density values
		if (sticsCrop != null) {
			rootDensity0 = this.sticsCrop.LRACH[0];
			rootDensity1 = this.sticsCrop.LRACH[1];
			rootDensity2 = this.sticsCrop.LRACH[2];
			rootDensity3 = this.sticsCrop.LRACH[3];
			rootDensity4 = this.sticsCrop.LRACH[4];
		}

		if (this.sticsReinit == null) {
			this.sticsReinit = new SafeSticsReinit(this.sticsCommun, this.sticsSoil);
		}
		
		//on recréé  l'objet STICSSOIL
		//on le ré-initialise certaines valeurs avec celles de fin de simulation précédente 
		if (this.sticsSoil==null) {
			SafeSticsSoil newSticsSoil 	= new SafeSticsSoil(soil);
			newSticsSoil.reinitialise (this.sticsReinit); 
			this.sticsSoil = newSticsSoil;
		}
		
		//RAZ memory
		this.sticsCommun = null;
		this.sticsCrop = null; 
		cropSpecies.sticsItk = null; 
		
		//garbage collector
		System.gc();

		
		//on recréé  l'objet STICSCOMMUN
		//on le ré-initialise certaines valeurs avec celles de fin de simulation précédente 
		SafeSticsCommun newSticsCommun 	= new SafeSticsCommun();
		newSticsCommun.reinitialise (this.sticsReinit); 
		this.sticsCommun = newSticsCommun;
	    
	    //Raz other stics objects
		this.sticsCrop 		= new SafeSticsCrop();
		cropSpecies.sticsItk= new SafeSticsItk();
		
		this.setCropSpecies(cropSpecies);

		//Initialise roots informations
		this.fineRoots = new SafeFineRoot (this);		
	
		// Loading crop species and crop intervention file 
		new SafeSticsCropFormat (cropSpeciesfileName).load (this.sticsCrop , cropSpecies);
		new SafeSticsItkFormat (itkfileName).load (cropSpecies.sticsItk, initialValues, IsMainCropSpecies());
		
		//Enchainement des simulations dans STICS = Oui
		this.sticsCommun.P_codesuite = 1;
		
		cropSpecies.sticsItk.ipl = 1;
		this.sticsCommun.numcult = 1;
		
	    if(sticsTransit.P_option_thinning== 1)
	    	cropSpecies.sticsItk.flag_eclairmult=true;
	    else
	    	cropSpecies.sticsItk.flag_eclairmult=false;

	    if(sticsTransit.P_option_engrais_multiple == 1 )
	    	cropSpecies.sticsItk.flag_plusieurs_engrais=true;
	    else
	    	cropSpecies.sticsItk.flag_plusieurs_engrais=false;
	
	    if(sticsTransit.P_option_pature == 1 )
	    	cropSpecies.sticsItk.flag_pature=true;
	    else
	    	cropSpecies.sticsItk.flag_pature=false;


	    sticsCommun.napini[0] = cropSpecies.sticsItk.nap;
	    sticsCommun.napNini[0] = cropSpecies.sticsItk.napN;
	    sticsCommun.nbjresini[0] = cropSpecies.sticsItk.P_nbjres;
	   
	    
	    //Restore CROP initial values if PERENIAL 
	    if (this.sticsCrop.P_codeperenne == 2) {

	    	//same itk file, crop is initialise with previous simulation results 
	    	if (getItkFileName().equals(itkfileName)) {
		    	//pour le fourrage (P_codeplante=2 fou) on repart du stade AMF
		    	if (this.sticsCrop.P_codeplante == 2) this.sticsCrop.P_stade0 	 =  4;//amf 
		    	//else this.sticsCrop.P_stade0 	 =  this.phenologicStage; 
		    	else this.sticsCrop.P_stade0 	 =  initialValues.mainCropStage; 
				this.sticsCrop.P_lai0 		 =  this.lai;
				this.sticsCrop.P_masec0 	 =  this.biomass;		//t ha-1
				this.sticsCrop.P_zrac0 		 =  this.rootDepth*100;	
				this.sticsCrop.P_magrain0 	 =  this.grainBiomass;	//g m-2
				this.sticsCrop.P_resperenne0 =  this.resperenne;
				this.sticsCrop.P_QNplante0   =  0;
				this.sticsCrop.P_densinitial[0] =  rootDensity0;
				this.sticsCrop.P_densinitial[1] =  rootDensity1;
				this.sticsCrop.P_densinitial[2] =  rootDensity2;
				this.sticsCrop.P_densinitial[3] =  rootDensity3;
				this.sticsCrop.P_densinitial[4] =  rootDensity4;

			    
	    	}
	    	else {	
	    	 //Initial values  
				if (IsMainCropSpecies()) {
					if (initialValues.mainCropStage != initialValues.MAIN_CROP_STAGE ) {
						this.sticsCrop.P_stade0 =  initialValues.mainCropStage; 
						this.sticsCrop.P_lai0 = (float) initialValues.mainCropLai;
						this.sticsCrop.P_masec0 = (float) initialValues.mainCropBiomass;
						this.sticsCrop.P_zrac0 = (float) (initialValues.mainCropRootDepth * 100);	//convert in cm
						this.sticsCrop.P_magrain0 = (float) initialValues.mainCropGrainBiomass;
						this.sticsCrop.P_QNplante0 = (float) initialValues.mainCropNitrogen;
						this.sticsCrop.P_resperenne0 = (float) initialValues.mainCropReserveBiomass;
						this.sticsCrop.P_densinitial[0] = (float) initialValues.mainCropRootDensity [0];
						this.sticsCrop.P_densinitial[1] = (float) initialValues.mainCropRootDensity [1];
						this.sticsCrop.P_densinitial[2] = (float) initialValues.mainCropRootDensity [2];
						this.sticsCrop.P_densinitial[3] = (float) initialValues.mainCropRootDensity [3];
						this.sticsCrop.P_densinitial[4] = (float) initialValues.mainCropRootDensity [4];
					}
					else {
				    	//pour le fourrage (P_codeplante=2 fou) on repart du stade AMF
				    	if (this.sticsCrop.P_codeplante == 2) this.sticsCrop.P_stade0 	 =  4;//amf 
				    	//else this.sticsCrop.P_stade0 	 =  this.phenologicStage; 
				    	else this.sticsCrop.P_stade0 	 =  initialValues.interCropStage; 
						this.sticsCrop.P_lai0 		 	=  this.lai;
						this.sticsCrop.P_masec0 	 	=  this.biomass;
						this.sticsCrop.P_zrac0 		 	=  this.rootDepth*100;	
						this.sticsCrop.P_magrain0 	 	=  this.grainBiomass;	
						this.sticsCrop.P_resperenne0 	=  this.resperenne;
						this.sticsCrop.P_QNplante0   	=  0;
						this.sticsCrop.P_densinitial[0] =  rootDensity0;
						this.sticsCrop.P_densinitial[1] =  rootDensity1;
						this.sticsCrop.P_densinitial[2] =  rootDensity2;
						this.sticsCrop.P_densinitial[3] =  rootDensity3;
						this.sticsCrop.P_densinitial[4] =  rootDensity4;						
					}
				}
				else {
					if (initialValues.interCropStage != initialValues.MAIN_CROP_STAGE) {
						this.sticsCrop.P_stade0 =  initialValues.interCropStage;
						this.sticsCrop.P_lai0 = (float) initialValues.interCropLai;
						this.sticsCrop.P_masec0 = (float) initialValues.interCropBiomass;
						this.sticsCrop.P_zrac0 = (float) (initialValues.interCropRootDepth * 100);	//convert in cm
						this.sticsCrop.P_magrain0 = (float) initialValues.interCropGrainBiomass;
						this.sticsCrop.P_QNplante0 = (float) initialValues.interCropNitrogen;
						this.sticsCrop.P_resperenne0 = (float) initialValues.interCropReserveBiomass;
						this.sticsCrop.P_densinitial[0] = (float) initialValues.interCropRootDensity [0];
						this.sticsCrop.P_densinitial[1] = (float) initialValues.interCropRootDensity [1];
						this.sticsCrop.P_densinitial[2] = (float) initialValues.interCropRootDensity [2];
						this.sticsCrop.P_densinitial[3] = (float) initialValues.interCropRootDensity [3];
						this.sticsCrop.P_densinitial[4] = (float) initialValues.interCropRootDensity [4];
					}
					else {
				    	//pour le fourrage (P_codeplante=2 fou) on repart du stade AMF
				    	if (this.sticsCrop.P_codeplante == 2) this.sticsCrop.P_stade0 	 =  4;//amf 
				    	else this.sticsCrop.P_stade0 	 =  this.phenologicStage; 
						this.sticsCrop.P_lai0 		 	=  this.lai;
						this.sticsCrop.P_masec0 	 	=  this.biomass;
						this.sticsCrop.P_zrac0 		 	=  this.rootDepth*100;	
						this.sticsCrop.P_magrain0 	 	=  this.grainBiomass;	
						this.sticsCrop.P_resperenne0 	=  this.resperenne;
						this.sticsCrop.P_QNplante0   	=  0;
						this.sticsCrop.P_densinitial[0] =  rootDensity0;
						this.sticsCrop.P_densinitial[1] =  rootDensity1;
						this.sticsCrop.P_densinitial[2] =  rootDensity2;
						this.sticsCrop.P_densinitial[3] =  rootDensity3;
						this.sticsCrop.P_densinitial[4] =  rootDensity4;						
					}
				}
	    	}
	    }
	    else {
			this.sticsCrop.P_stade0 	 =  1; 
			this.sticsCrop.P_lai0 		 =  0;
			this.sticsCrop.P_masec0 	 =  0;
			this.sticsCrop.P_zrac0 		 =  0;	
			this.sticsCrop.P_magrain0 	 =  0;	
			this.sticsCrop.P_resperenne0 =  0;
			this.sticsCrop.P_QNplante0   =  0;	
			this.sticsCrop.P_densinitial[0] =  0;
			this.sticsCrop.P_densinitial[1] =  0;
			this.sticsCrop.P_densinitial[2] =  0;
			this.sticsCrop.P_densinitial[3] =  0;
			this.sticsCrop.P_densinitial[4] =  0;
			this.yield = 0;
	    }
	    
		//JNA NATIVE method to check plant and itk parameters
		//result is in output/initialisation.sti
	    if (exportStiFile) jna.verifPlante(sticsParam, sticsTransit, this.sticsCommun, cropSpecies.sticsItk, this.sticsCrop, IsMainCropSpecies(), exportDir);

		//IL 25-04-2018
		//repris de STICS verifPlante (si on le zappe ben ça marche plus) 
		this.sticsCrop.ipl = 1;
		this.cropSpecies.sticsItk.ipl = 1;
		if (this.cropSpecies.sticsItk.P_ressuite == 0) this.cropSpecies.sticsItk.P_ressuite = 1;
		
		//reset cropAge
		this.cropAge = 0;
		this.sowingDay = 0;
		this.harvestDay = 0;
		
		this.itkfileName = itkfileName;
			
		return;
	}
	
	/**
	 * STICS initialisation reload with same perenial crop species 
	 * SOIL IS NOT ERASED !
	 */
	public  void cropPerenialReload (SafeTestJNA jna, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
									SafeCropSpecies cropSpecies,
									String itkfileName, 
									SafeInitialValues initialValues,
									String exportDir,
									boolean exportStiFile)  throws Exception {


		cropSpecies.sticsItk= new SafeSticsItk();
		new SafeSticsItkFormat (itkfileName).load (cropSpecies.sticsItk, initialValues, IsMainCropSpecies());
	    
		//Enchainement des simulations dans STICS = Oui
		this.sticsCommun.P_codesuite = 1;
		
		cropSpecies.sticsItk.ipl = 1;
		this.sticsCommun.numcult = 1;
		
	    if(sticsTransit.P_option_thinning== 1)
	    	cropSpecies.sticsItk.flag_eclairmult=true;
	    else
	    	cropSpecies.sticsItk.flag_eclairmult=false;

	    if(sticsTransit.P_option_engrais_multiple == 1 )
	    	cropSpecies.sticsItk.flag_plusieurs_engrais=true;
	    else
	    	cropSpecies.sticsItk.flag_plusieurs_engrais=false;
	
	    if(sticsTransit.P_option_pature == 1 )
	    	cropSpecies.sticsItk.flag_pature=true;
	    else
	    	cropSpecies.sticsItk.flag_pature=false;
			    
		//JNA NATIVE method to check plant and itk parameters
		//result is in output/initialisation.sti
	    if (exportStiFile) jna.verifPlante(sticsParam, sticsTransit, this.sticsCommun, cropSpecies.sticsItk, this.sticsCrop, IsMainCropSpecies(), exportDir);

		//IL 25-04-2018
		//repris de STICS verifPlante (si on le zappe ben ça marche plus) 
		this.sticsCrop.ipl = 1;
		this.cropSpecies.sticsItk.ipl = 1;
		if (this.cropSpecies.sticsItk.P_ressuite == 0) this.cropSpecies.sticsItk.P_ressuite = 1;
		
		//reset cropAge
		this.cropAge++;
		
		this.itkfileName = itkfileName;
		
		return;
	}
	 /**
	* STICS initialisation copy for all cells with the same crop species and itk
	*/
	public void cropInitialisationCopy (SafeCrop initialCrop) {
		
		this.sticsSoil 		= new SafeSticsSoil(initialCrop.sticsSoil);
		this.sticsCommun 	= new SafeSticsCommun(initialCrop.sticsCommun);	
		this.sticsCrop 		= new SafeSticsCrop(initialCrop.sticsCrop);
		this.itkfileName    = initialCrop.itkfileName;
		this.setCropSpecies (initialCrop.getCropSpecies());
	}

	/**
	 * STICS CROP process Apport (to add tree litter to the soil mineralisation)
	 **/
	public void processApport (SafeTestJNA safeJNA, 
								SafeSticsParameters sticsParam, 
								SafeEvolutionParameters evolutionParameters,
								SafeCell c,		
								int julianDay,
							    double profHum, 
							    double treeRootDepth,
							    double treeCarbonLeafLitter, 
							    double treeNitrogenLeafLitter) {
		
		this.setTreeCarbonLeafLitter (treeCarbonLeafLitter);	//kg C ha-1
		this.setTreeNitrogenLeafLitter (treeNitrogenLeafLitter);	//kg N ha-1
		
		//fine and coarse roots litter calculation
		double treeCarbonFineRootsLitter = 0;
		double treeNitrogenFineRootsLitter = 0;
		double treeCarbonCoarseRootsLitter = 0;
		double treeNitrogenCoarseRootsLitter = 0;
		double treeCarbonFineRootsDeepLitter = 0;
		double treeNitrogenFineRootsDeepLitter = 0;
		double treeCarbonCoarseRootsDeepLitter = 0;
		double treeNitrogenCoarseRootsDeepLitter = 0;	

		if ((evolutionParameters.treesRootLitterIncorporated) || (evolutionParameters.treesDeepRootLitterIncorporated) )	{	

			SafeVoxel [] voxels = c.getVoxels();

			//FOR EACH VOXEL
			for (int i = 0; i < voxels.length; i++) {
				
				SafeVoxel v = voxels[i];
				double voxelTop    = v.getZ()-(v.getThickness()/2);
				double voxelBottom = v.getZ()+(v.getThickness()/2);
				double prop = 1d;
				
				//SURFACE VOXEL < PROFHUM
				if (voxelBottom <= profHum) {

					if (evolutionParameters.treesRootLitterIncorporated) {
						treeCarbonCoarseRootsLitter += (v.getTotalTreeCarbonCoarseRootSen());//in kg
						treeNitrogenCoarseRootsLitter +=(v.getTotalTreeNitrogenCoarseRootSen());
						treeCarbonFineRootsLitter += (v.getTotalTreeCarbonFineRootSen());
						treeNitrogenFineRootsLitter += (v.getTotalTreeNitrogenFineRootSen());
					}

				}
				else {
					//=============================================
					//IL 31/10/2018 
					//i removed this because in deepSenescentRootsMineralization we just test
					// voxel.getZ()*100 > sticsSoil.P_profhum
					//==========================================
					//if (voxelTop < profHum){
					//	prop = (voxelBottom - profHum)/(voxelBottom-voxelTop);
					//}

					
					if (evolutionParameters.treesDeepRootLitterIncorporated) {
						treeCarbonCoarseRootsDeepLitter += (v.getTotalTreeCarbonCoarseRootSen()*prop);//in kg
						treeNitrogenCoarseRootsDeepLitter += (v.getTotalTreeNitrogenCoarseRootSen()*prop);
						treeCarbonFineRootsDeepLitter += (v.getTotalTreeCarbonFineRootSen()*prop);
						treeNitrogenFineRootsDeepLitter += (v.getTotalTreeNitrogenFineRootSen()*prop);								
					}
					
					
					//Voxel coupé en 2 par profHum
					//if ((prop < 1d) && (prop > 0))  {
					//	if (evolutionParameters.treesRootLitterIncorporated) {
					//		treeCarbonCoarseRootsLitter += (v.getTotalTreeCarbonCoarseRootSen()*(1-prop));//in kg
					//		treeNitrogenCoarseRootsLitter +=(v.getTotalTreeNitrogenCoarseRootSen()*(1-prop));
					//		treeCarbonFineRootsLitter += (v.getTotalTreeCarbonFineRootSen()*(1-prop));
					//		treeNitrogenFineRootsLitter += (v.getTotalTreeNitrogenFineRootSen()*(1-prop));
					//	}
					//}
				}
			}

			//SURFACE LITTERS
			treeCarbonFineRootsLitter = treeCarbonFineRootsLitter  / (c.getArea() / 10000); // kg ha-1	
			treeCarbonCoarseRootsLitter = treeCarbonCoarseRootsLitter  / (c.getArea() / 10000); // kg ha-1	
			treeNitrogenCoarseRootsLitter = treeNitrogenCoarseRootsLitter  / (c.getArea() / 10000); // kg ha-1	
			treeNitrogenFineRootsLitter = treeNitrogenFineRootsLitter  / (c.getArea() / 10000); // kg ha-1	
				
			this.setTreeCarbonFineRootLitter (treeCarbonFineRootsLitter);		
			this.setTreeCarbonCoarseRootLitter (treeCarbonCoarseRootsLitter);
			this.setTreeNitrogenFineRootLitter (treeNitrogenFineRootsLitter);
			this.setTreeNitrogenCoarseRootLitter (treeNitrogenCoarseRootsLitter);		
	

			//DEEP LITTERS			
			treeCarbonFineRootsDeepLitter = treeCarbonFineRootsDeepLitter  / (c.getArea() / 10000); // kg ha-1	
			treeCarbonCoarseRootsDeepLitter = treeCarbonCoarseRootsDeepLitter  / (c.getArea() / 10000); // kg ha-1	
			treeNitrogenCoarseRootsDeepLitter = treeNitrogenCoarseRootsDeepLitter  / (c.getArea() / 10000); // kg ha-1	
			treeNitrogenFineRootsDeepLitter = treeNitrogenFineRootsDeepLitter  / (c.getArea() / 10000); // kg ha-1	
			
			this.setTreeCarbonFineRootDeepLitter (treeCarbonFineRootsDeepLitter);		
			this.setTreeCarbonCoarseRootDeepLitter (treeCarbonCoarseRootsDeepLitter);
			this.setTreeNitrogenFineRootDeepLitter (treeNitrogenFineRootsDeepLitter);
			this.setTreeNitrogenCoarseRootDeepLitter (treeNitrogenCoarseRootsDeepLitter);	

		}

		//CALL STICS METHOD TO ADD LITTER INTO THE SOIL MINERALISATION
		//TREE LEAF LITTER INCORPORTION 
		if (treeCarbonLeafLitter > 0) {
						
			int typeLitter = 10;
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;	
			float freshMatterLitter = (float) (treeCarbonLeafLitter / 1000 / cfeupc);	
			float cnLitter = (float) (treeCarbonLeafLitter/ treeNitrogenLeafLitter);
			float profMax = 1.0f;
				
			if (this.getSoilManagementDepth(julianDay) != 0) {
				typeLitter = 20;
				profMax = (float) (Math.min(this.getSoilManagementDepth(julianDay),profHum)*100);
			}

			
			//call JNA native method
			safeJNA.apport(sticsParam, 
					this.sticsCommun, 
					this.sticsCrop,
					this.cropSpecies.sticsItk,
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,		//C content of residu  (% MF)
					waterLitter,
					typeLitter			//litter type
					);
		}
		
		//TREE FINE ROOTS LITTER INCORPORTION 
		//IL 13/04/2018
		//SEMBLE NE PAS MARCHER je remplace par une modif sur treesFineRootsDeepLitterIncorporated
		
	if (treeCarbonFineRootsLitter > 0) {
			int typeLitter = 21;
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;
			float freshMatterLitter = (float) (treeCarbonFineRootsLitter / 1000/ cfeupc); 
			float cnLitter = (float) (treeCarbonFineRootsLitter/treeNitrogenFineRootsLitter);
			float profMax = (float) (Math.min(treeRootDepth,profHum)*100);

			//call JNA native method
			safeJNA.apport(sticsParam, 
					this.sticsCommun,
					this.sticsCrop,
					this.cropSpecies.sticsItk,
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,				//C content of residu  (% MF)
					waterLitter,
					typeLitter			//litter type
					);

		}
		
		//TREE COARSE ROOTS LITTER INCORPORTION 
		if (treeCarbonCoarseRootsLitter > 0) {
			int typeLitter = 21;			
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;
			float freshMatterLitter = (float) (treeCarbonCoarseRootsLitter/ 1000 / cfeupc); 
			float cnLitter = (float) (treeCarbonCoarseRootsLitter/ treeNitrogenCoarseRootsLitter);
			float profMax = (float) (Math.min(treeRootDepth,profHum)*100);				
			
			//call JNA native method
			safeJNA.apport(sticsParam, 
					this.sticsCommun, 
					this.sticsCrop,
					this.cropSpecies.sticsItk,
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,				//C content of residu  (% MF)
					waterLitter,
					typeLitter			//litter type
					);
		}	

	}
	/**
	 * STICS CROP process growth part I (before water repartition)
	 **/
	public void processGrowth1 (SafeTestJNA safeJNA, 
								SafeSticsParameters sticsParam, 
								SafeSticsTransit sticsTransit, 
								SafeSticsStation sticsStation, 
								SafeCell c,
							    SafeInitialParameters safeSettings,
							    SafeEvolutionParameters evolutionParameters,
							    int simulationYear, 
								int julianDay, 
								int sticsDay,
							    double cellRad, 
							    double cellRain,
							    double cellEtp
								) {

		//call JNA native method
		safeJNA.boucleJour1(sticsParam, 
				sticsTransit, 
				sticsStation, 
				this.sticsClimat,
				this.sticsCommun, 
				this.sticsSoil,
				this.sticsCrop,
				this.cropSpecies.sticsItk,
				sticsDay, 
				julianDay,
				cellRad,
				cellRain,
				cellEtp);

		//important variable storage
		this.sticsDay 		 = sticsDay;			    //DOY
		this.soilEvaporation = this.sticsCommun.esol;	//mm
		this.emergenceDay 	 = this.sticsCrop.ilevs;    //DOY
		this.floweringDay 	 = this.sticsCrop.iflos;    //DOY
		if (this.sticsCrop.zrac == 0) this.rootDepth = 0;
		if (this.rootDepth < (this.sticsCrop.zrac / 100))
			this.rootDepth = this.sticsCrop.zrac / 100;			//convert cm in m
			
		//WATER and NITROGEN DEMAND
		///attention il faudra changer l'indice si on passe aux cultures associées
		this.waterDemand  = this.sticsCrop.eop[1];				//mm
		this.nitrogenDemand  = this.sticsCrop.demande[1];		//kg ha-1

		////real      :: lai(0:2,0:366)
		//attention il faudra changer l'indice  si on passe aux cultures associées	
		int indice          = (sticsDay*3)+1;
		this.lai 			= this.sticsCrop.lai[indice]; 
		this.biomass 		= this.sticsCrop.masec[indice];			//t ha-1		
		this.grainBiomass 	= this.sticsCrop.magrain[indice]/100;	//convert g m-2 in t ha-1
		this.qNplante 		= this.sticsCrop.QNplante[indice];		//kgN.ha-1

		
		//Sum of roots calculation (for water repartition module)
		this.computeTotalRoot (c, safeSettings);

		//Reduction factor for water demand (dimensionless)
		if (this.waterDemand > 0) {		//in mm
			this.getFineRoots().calculatePotential (safeSettings, this.waterDemand*this.getCell().getArea());		// gt - 12.11.2009 - water demand is now expressed in liters instead of mm

			//IF RACHMAT VERSION ONLY !!!!
			double waterDemandReductionFactor = this.getFineRoots().getWaterDemandReductionFactor();
			setWaterDemandReduced (this.getWaterDemand() * waterDemandReductionFactor);		//mm

		}

		//Nitrogen sink strength with nitrogen demand of the day before
		if (nitrogenDemand > 0)
			this.getFineRoots().calculateNitrogenSinkStrength (safeSettings, nitrogenDemand);

		//cumulation of Rain transmitted on the crop
		this.totalRain				+= cellRain;

		return;
	}


	/**
	 * STICS CROP process growth part II (after water repartition)
	 **/
	public boolean processGrowth2 (SafeTestJNA safeJNA, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
									SafeSticsStation sticsStation, 
									SafeStand stand,
								    int simulationYear, 
									int julianDay, 
									int sticsDay,	
									int hisafeInfluence,
								    double cellTrg,
								    double cellVisibleSky
									) {


		boolean visibleStep = false;

		//call JNA native method	
		safeJNA.boucleJour2 (sticsParam, sticsTransit, 
				sticsStation, 
				this.sticsClimat,
				this.sticsCommun, 
				this.sticsSoil,
				this.sticsCrop,
				this.cropSpecies.sticsItk,
				sticsDay, 
				julianDay,
				hisafeInfluence,
				cellTrg,
				cellVisibleSky
			);
					
		//important variable storage
		this.harvestDay = 0;
		if (this.sticsCrop.nrec > 0)
			this.harvestDay = this.sticsCrop.nrec + this.startDay;	
		
		if (this.sticsCrop.zrac == 0) this.rootDepth = 0;
		if (this.rootDepth < (this.sticsCrop.zrac / 100))
			this.rootDepth = this.sticsCrop.zrac / 100;			//convert cm in m
		
		this.sticsDay       = sticsDay;
		this.extin			= this.sticsCrop.P_extin;
		this.albedoLai 		= this.sticsCommun.albedolai;
		this.plantDensity	= this.sticsCrop.densite;
		this.cropTemperature 	= this.sticsCommun.tcult;	//degree C
		this.cropMaxTemperature 	= this.sticsCommun.TcultMax;	//degree C
		this.cropMinTemperature 	= this.sticsCommun.TcultMin;	//degree C
		this.soilSurfaceTemperature = this.getSticsSoil().TS[0];
		this.soilManagementDepth = (float) this.getSoilManagementDepth(julianDay);
		
		////real      :: eai(0:2) 
		//attention il faudra changer l'indice  si on passe aux cultures associées
		this.eai			= this.sticsCrop.eai[1];		  
		this.height 		= this.sticsCrop.hauteur[1];
		this.grainNumber 	= this.sticsCrop.nbgrains[1];
		this.grainWeight 	= this.sticsCrop.pgrain[1];
		this.sla 			= this.sticsCrop.sla[1];
		this.resperenne     = this.sticsCrop.resperenne[1];		
		this.interceptedPar = this.sticsCrop.raint[1]; 		// MJ m-2
		this.cNgrain 		= this.sticsCrop.CNgrain[1];	//%
		this.cNplante 		= this.sticsCrop.CNplante[1];	//%
		this.qNgrain		= this.sticsCrop.QNgrain[1];	//kg h-1
	
		////real      :: lai(0:2,0:366)
		//attention il faudra changer l'indice  si on passe aux cultures associées	
		int indice          = (sticsDay*3)+1;
		this.qNplante 		= this.sticsCrop.QNplante[indice];		//kgN.ha-1
		this.lai 			= this.sticsCrop.lai[indice]; 
		this.biomass 		= this.sticsCrop.masec[indice];			//t ha-1		
		this.grainBiomass 	= this.sticsCrop.magrain[indice]/100;	//convert g m-2 in t ha-1

		//Ajout sitraka recuperation carbone et azote des residus
		this.carbonResidus 	= this.sticsCommun.Cr;	//kgC.ha-1
		this.nitrogenResidus = this.sticsCommun.Nr;	//kgN.ha-1
		//---------------------------------------------------------------------------

		//WATER
		this.runOff			 = this.sticsCommun.ruissel;			// mm;
		this.surfaceRunOff 	 = this.sticsCommun.ruisselsurf;	    // mm
		//in STICS remontee is negative
		if (this.sticsSoil.remontee!=0) this.capillaryRise	 = -(this.sticsSoil.remontee);			    // mm;
		this.irrigation 	 = this.sticsCommun.airg[sticsDay-1];		// mm
	
		//NITROGEN ENTRIES (cumulated values in kg ha-1 )
		this.nitrogenRain 					= this.sticsCommun.precipjN;
		this.nitrogenIrrigation 			= this.sticsCommun.irrigN;			
		this.nitrogenFixation 				= this.sticsCrop.offrenod[1];
		this.nitrogenDenitrification 		= this.sticsSoil.Ndenit;
		this.nitrogenFertilisationMineral 	= this.sticsCommun.totapN;	
		this.nitrogenFertilisationOrganic 	= this.sticsCommun.QNresorg;
		
		//NITROGEN OUTPUTS (kg ha-1 )
		this.biomassRestitution = this.sticsCrop.qressuite;			//??? gros doute la dessus IL 20/01/2017
					
		//this.nitrogenLixiviation = this.sticsCommun.QLES + this.sticsSoil.qlesd;	
		this.nitrogenLeachingBottom = this.sticsCommun.lessiv;
		this.nitrogenLeachingArtificial = this.sticsSoil.azlesd;
		this.nitrogenImmobilisation = this.sticsSoil.Norgeng;		
		this.nitrogenVolatilisation= this.sticsSoil.Nvoleng;		
		this.nitrogenVolatilisationOrganic = this.sticsSoil.Nvolorg;		
		this.nitrogenHarvested = this.sticsCrop.Nexporte ;		
		this.biomassHarvested = this.sticsCrop.MSexporte; 
		
	
			
		//residus
		this.cnResidus = this.sticsCrop.CsurNressuite;	
		this.cropCarbonLeafLitter = this.sticsCrop.QCplantetombe[1] + this.sticsCrop.QCrogne + this.sticsCrop.QCressuite;
		this.cropNitrogenLeafLitter = this.sticsCrop.QNplantetombe[1] + this.sticsCrop.QNrogne + this.sticsCrop.QNressuite;
		this.cropCarbonRootLitter = this.sticsCrop.QCrac;
		this.cropNitrogenRootLitter = this.sticsCrop.QNrac;

		//stress indexes
	    this.sticsWaterStomatalStress= this.sticsCrop.swfac[1];
        this.sticsWaterTurgescenceStress = this.sticsCrop.turfac[1];  	
        this.sticsWaterSenescenceStress =  this.sticsCrop.senfac[1]; 
        this.nitrogenNutritionIndex= this.sticsCrop.inn[1];
        this.sticsNitrogenLaiStress = this.sticsCrop.innlai[1];  		
        this.sticsNitrogenBiomassStress= this.sticsCrop.inns[1];     
        this.sticsNitrogenSenescenceStress =  this.sticsCrop.innsenes[1]; 
        
        		
		float totalParIntercepted = this.getDirectParIntercepted()+this.getDiffuseParIntercepted();

		if  (this.cropSpecies.sticsItk.P_iplt0 == julianDay) {		//plt
			phenologicStage = 2;
			this.sowingDay = julianDay;
		}				
		if  (this.sticsCrop.nlev == sticsDay)	{		//levs
			phenologicStage = 4;
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}		
		if  (this.sticsCrop.namf == sticsDay)	{		//amf
			phenologicStage = 5;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}		
		if (this.sticsCrop.nlax == sticsDay)	{		//lax
			phenologicStage = 6;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}
		if (this.sticsCrop.nflo == sticsDay)	{		//flo
			phenologicStage = 7;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}
		if (this.sticsCrop.ndrp == sticsDay)	{		//drp
			phenologicStage = 8;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}
		if (this.sticsCrop.nmat == sticsDay)	{		//mat
			phenologicStage = 10;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}
		if (this.sticsCrop.nrec == sticsDay)	{		//rec
			phenologicStage = 11;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
			this.yield  		= this.sticsCrop.magrain[indice]/100;
		}
		if (this.sticsCrop.nsen == sticsDay)	{		//sen
			phenologicStage = 12;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}		
		if (this.sticsCrop.nlan == sticsDay)	{		//lan
			phenologicStage = 13;			
			setParIntCumByCropPhenoStage(totalParIntercepted);
		}
		
		//YIELD for PERENIAL CROPS 
		//we took the day before because masec of the day is AFTER the cut 
		if ((this.isPerennial) && (this.sticsCrop.sioncoupe)) {
			int indicePrev      = ((sticsDay-1)*3)+1;
			this.yield  		= this.sticsCrop.masec[indicePrev];
		}
		
		if ((this.sowingDay > 0) && (this.harvestDay == 0)) this.cropAge = this.cropAge+1; 
		
		//TOTALS
		this.totalCapillaryRise 	+= this.capillaryRise;
		this.totalIrrigation 		+= this.irrigation;
		this.totalWaterDemand 		+= this.waterDemand;
		this.totalWaterDemandReduced += this.waterDemandReduced;
		this.totalWaterUptake 		+= this.waterUptake;
		this.totalNitrogenDemand	+= this.nitrogenDemand;
		this.totalNitrogenUptake 	+= this.nitrogenUptake;
		this.totalSoilEvaporation 	+= this.totalSoilEvaporation;
		this.totalRunOff 			+= this.runOff;
		this.totalSurfaceRunOff 	+= this.surfaceRunOff;
		this.totalDrainageBottom 	+= this.sticsCommun.drain;
		this.totalDrainageArtificial += this.sticsSoil.qdrain;
		this.totalParIntercepted    +=  this.getDirectParIntercepted();
		this.totalParIntercepted	+= this.getDiffuseParIntercepted();
		

		//for project inspector step display
		if (this.sowingDay == julianDay)  {
			visibleStep = true;
			stand.setDisplay ("Crop sowing");
		}
		if (this.harvestDay == sticsDay) {
			visibleStep = true;
			stand.setDisplay ("Crop harvest");
		}
		return visibleStep;
	}

	/**
	* Return the soil management depth for today in meters
	**/
	protected double getSoilManagementDepth (int simulationDate) {
		int nbSoilManagement = cropSpecies.sticsItk.P_nbjtrav;
		for (int i=0; i<nbSoilManagement; i++) {
			// if YES, return the soil management depth in meters
			if (cropSpecies.sticsItk.P_jultrav[i] == simulationDate)  // soil management today
				return (cropSpecies.sticsItk.P_proftrav[i] / 100);		// convert cm to m
		}
		return 0;
	}

	/**
	* Sum of roots calculation (for water repartition module)
	**/
	public void computeTotalRoot (Object c,  SafeInitialParameters settings) {
		SafeVoxel [] voxels = null;
		voxels = ((SafeCell) c).getVoxels();
		
		double cropRootLength = 0;
		double plantPotential = 0;

		double drySoilFactor = settings.harmonicWeightedMean;
		
		for (int i=0; i<voxels.length ; i++) {		// first iterator to compute totalRootLength
			cropRootLength += voxels[i].getCropRootDensity() * voxels[i].getVolume();			// m.m-3 * m3 = m
		}
		this.getFineRoots().setTotalRootLength (cropRootLength);	


		if (cropRootLength <= 0) return;
		for (int i=0; i<voxels.length ; i++) {		// second iterator to compute required plant water potential
			double neededPot = voxels[i].getWaterPotentialTheta();	// soil water potential in this voxel

			// additional potential for water flow from bulk soil to rhizosphere
			neededPot *= (1+this.getCropSpecies().getCropBufferPotential()); 

			// additional potential for water flow from root surface to xylem
			double radialTransportPotential = -this.getWaterDemand()								// L.day-1=dm3.day-1
												* 1000														// from L.day-1 to cm3.day-1
												/this.getCropSpecies().getCropRootConductivity()			// cm day-1
												/(cropRootLength*100);									// m to cm
			
			this.getFineRoots().setRadialTransportPotential(radialTransportPotential);
			neededPot += radialTransportPotential;

			
			// additional potential to account for longitudinal water transport in coarse roots from the voxel to stem base
			double longitudinalTransportPotential = -this.getWaterDemand()										//L.day-1=dm3.day-1
														* 1000															// from L.day-1 to cm3.day-1
														* this.getCropSpecies().getCropLongitudinalResistantFactor()	// day.cm-1.m-1
														/(cropRootLength*100);										// m to cm
			// in the model documentation from Meine et al, this term is not divided by totalRootLength... but it leads to very different longitudinal drop potential for small and large trees because of differences in water demand... so...

			this.getFineRoots().setLongitudinalTransportPotential(longitudinalTransportPotential);
			neededPot += longitudinalTransportPotential*voxels[i].getZ();	// topological distance (m) between the voxel and stem base

			// additional potential to account for voxel depth
			neededPot -= voxels[i].getZ()*100;		// from m to cm

			plantPotential += -(voxels[i].getCropRootDensity() * voxels[i].getVolume()	// m.m-3 * m3 = m
									/ Math.pow(-neededPot, drySoilFactor));	// cm
			
		}
		
		
		plantPotential =-Math.pow (-cropRootLength /plantPotential , 1/drySoilFactor);


		this.getFineRoots().setRequiredWaterPotential(plantPotential);
			
	}

	/**
	* Updating the results of light interception
	**/
	public void updateDailyInterceptedPar (SafeDailyClimat dailyClimat,
											SafeInitialParameters settings,
											SafeBeamSet beamSet){


		if(this.getLai()+this.getEai() >0){
			// Climatic input
			float dailyDiffuse = dailyClimat.getDiffusePar();	// moles m-2
			float dailyDirect = dailyClimat.getDirectPar();		// moles m-2

			// Topological mask
			float diffuseMask = (float) beamSet.getSkyDiffuseMask();
			float directMask = (float) beamSet.getSkyDirectMask();


			setDiffuseParIntercepted(getCaptureFactorForDiffusePar()*dailyDiffuse);	//moles.m-2
			setDirectParIntercepted(getCaptureFactorForDirectPar()*dailyDirect);	//moles.m-2

			float parIntercepted = getDirectParIntercepted()+getDiffuseParIntercepted(); //moles.m-2

			setTotalParInterceptedMJ(parIntercepted									//moles.m-2
									 * (float) settings.molesParCoefficient);		//*MJ.moles-1 = MJ.m-2


			// Computation of a competition index for Par =
			//		(TotalParIntercepted)/(Par intercepted by the same crop in monoculture)

			float incidentPar = dailyDiffuse*diffuseMask+dailyDirect*directMask;
			if((lai+eai)>0){
				float competitionIndex = (float) (parIntercepted	// Par intercepted (moles.m-2)
								/(0.95*(1-Math.exp(-extin*(lai+eai)))	// % of Par intercepted by monocrop
								*incidentPar));						// daily incident Par (Moles m-2)
				setCompetitionIndexForTotalPar(competitionIndex);

			} else {
				setCompetitionIndexForTotalPar(1);
			}

			this.parIntCum += parIntercepted;
			this.parIntCumByCropPhenoStage += parIntercepted;
		}
	}

	/**
	* reset capture factors for light
	**/
	public void resetDirect (){captureFactorForDirectPar = 0;}
	public void resetDiffuse (){captureFactorForDiffusePar = 0;}

	/**
	* Optimization of light extinction coefficient to be coherent with Stics
	**/
	public void findCropLightCoef(SafeBeamSet beamSet, SafeInitialParameters settings, SafeDailyClimat climat){

		// % of Par intercepted by monocrop with Stic's formalism
		double toBeIntercepted =1-Math.exp(-(this.lai+this.eai)*this.extin);

		// weights of direct and diffuse Par
		double dailyDiffuse = climat.getDiffusePar()/climat.getGlobalPar();
		double dailyDirect = climat.getDirectPar()/climat.getGlobalPar();

		if(toBeIntercepted!=0){
			// initialisation of k
			double kEst=extin;

			// calculation of Newton function = intercepted(kEst)-toBeIntercepted
			//				and its derivative
			double fEst=0;
			double fEstPrime=0;
			double fOld = 1;
			double kOld = 1;
			int nbIt = 0;

			while((nbIt==0)||(((Math.abs(kEst-kOld)/kOld)>0.00001)||((Math.abs(fEst-fOld)/fOld)>0.001))){
				nbIt++;
				fEst = -toBeIntercepted;
				fEstPrime = 0;

				for (Iterator ite = beamSet.getBeams ().iterator (); ite.hasNext ();) {

					SafeBeam b = (SafeBeam) ite.next ();
					fOld=fEst;
					fEst +=(dailyDiffuse*b.getDiffuseEnergy()+dailyDirect*b.getDirectEnergy())
									* (1-Math.exp(-kEst*(lai+eai)/Math.sin(b.getHeightAngle_rad())));
					fEstPrime +=(dailyDiffuse*b.getDiffuseEnergy()+dailyDirect*b.getDirectEnergy())
								*(lai+eai)/Math.sin(b.getHeightAngle_rad())
								*Math.exp(-kEst*(lai+eai)/Math.sin(b.getHeightAngle_rad()));
				}
				kOld=kEst;
				kEst += -fEst/fEstPrime;
			}

			setParExtinctionCoef (kEst);
		}
	}

	/**
	* Crop light interception calculation 
	* used when CropLightMethod=false (set in hisafe.par) 
	**/
	public void cropLightInterception (SafeInitialParameters settings, SafeBeamSet beamSet, double directProp, double diffuseProp){
		float lai = this.getLai();
		float eai = this.getEai();
		float extin = this.getExtin();
		float raint = (float) (0.95*(1-Math.exp(-extin*(lai+eai))));
		this.setCaptureFactorForDirectPar((float) (raint*directProp));
		this.setCaptureFactorForDiffusePar((float) (raint*diffuseProp));	
	}
	
	
	
//FOR EXPORT (IL GT 19/12/07)
	
	public String getItkFileName() {return itkfileName;}
	
	public SafeSticsCrop getSticsCrop() {return sticsCrop;}

	public SafeSticsSoil getSticsSoil() {return sticsSoil;}
	
	public SafeSticsClimat getSticsClimat() {return sticsClimat;}
	
	public SafeFineRoot getFineRoots() {return fineRoots;}

	public void setCropSpecies (SafeCropSpecies cropSpecies) {this.cropSpecies=cropSpecies;}
	public SafeCropSpecies getCropSpecies () {return cropSpecies ;}

	public void setMainCropSpecies(boolean b){isMainCropSpecies=b;}
	public boolean IsMainCropSpecies(){return isMainCropSpecies;}
	public String getCropType () {
		if (IsMainCropSpecies()) return "mainCrop";
		else return "interCrop";
	}
	
	public int getCropAge () {return cropAge ;}
	
	public SafeCell getCell() {return cell;}
	public int getSowingDay () {return sowingDay;}
	public int getHarvestDay () {return harvestDay;}
	public int getSticsDay () {return sticsDay;}
	public int getFloweringDay () {return floweringDay;}
	public int getEmergenceDay () {return emergenceDay;}
	public int getPhenologicStage () {return phenologicStage;}
	public boolean isPerennial () {return isPerennial;} 

	public float getLai () {return lai;}
	public float getEai () {return eai;}
	public float getLaiMax () {return laiMax;}
	public float getEaiMax () {return eaiMax;}
	public float getExtin () {return extin;}
	public float getAlbedoLai () {return albedoLai;}
	public float getBiomass () {return biomass;}
	public float getGrainBiomass () {return grainBiomass;}
	public float getBiomassMax () {return biomassMax;}
	
	public float getHeight() {return  height;}
	public float getHeightMax () {return heightMax;}
	
	public float getYield() {return  yield;}
	public float getYieldMax() {return  yieldMax;}
	
	public float getGrainNumber () {return  grainNumber;}
	public float getGrainWeight () {return  grainWeight;}
	public float getPlantDensity () {return  plantDensity;}
	public float getYieldIndice () {
		if(this.biomass == 0){
			return 0;
		} else {
			return this.yield/this.biomass;
		}
	}

	public float getSowingDepth () {return  sowingDepth;}
	public float getRootDepth () {return  rootDepth;}
	public float getRootDepthMax () {return  rootDepthMax;}
	public float getCropTemperature () {return  cropTemperature;}
	public float getCropMaxTemperature () {return  cropMaxTemperature;}
	public float getCropMinTemperature () {return  cropMinTemperature;}
	public float getSoilSurfaceTemperature () {return soilSurfaceTemperature;}
	public float getSoilManagementDepth () {return soilManagementDepth;}
	
	public float getSoilEvaporation () {return  soilEvaporation;}
	public float getRunOff () {return  runOff;}
	public float getSurfaceRunOff() {return  surfaceRunOff;}
	public float getCapillaryRise () {return Math.max(capillaryRise-capillaryRisePrev,0);}
	
	public float getDrainageBottom () {return this.sticsCommun.drain;}
	public float getDrainageArtificial () {return this.sticsSoil.qdrain;}
	
	public float getIrrigation () {return  irrigation;}
	public float getQngrain() {return qNgrain;}
	public float getQnplante() {return qNplante;}
	public float getCngrain() {return cNgrain;}
	public float getCnplante() {return cNplante;}
	
	//SURFACE LEAVES LITTER--------------------------------------------------------
	public double getTreeCarbonLeafLitter () {return  (double) treeCarbonLeafLitter;}
	public double getTreeNitrogenLeafLitter () {return (double) treeNitrogenLeafLitter;}
	public void  setTreeCarbonLeafLitter (double v) {treeCarbonLeafLitter =  (float) v;}
	public void  setTreeNitrogenLeafLitter (double v) {treeNitrogenLeafLitter =  (float) v;}

	//SURFACE ROOT LITTER---------------------------------------------------------
	public double getTreeCarbonFineRootLitter (){return (double) treeCarbonFineRootLitter;}
	public double getTreeNitrogenFineRootLitter(){return (double) treeNitrogenFineRootLitter;}
	public double getTreeCarbonCoarseRootLitter (){return (double) treeCarbonCoarseRootLitter;}
	public double getTreeNitrogenCoarseRootLitter(){return (double) treeNitrogenCoarseRootLitter;}
	public void setTreeCarbonFineRootLitter (double v){treeCarbonFineRootLitter = (float) v;}
	public void setTreeNitrogenFineRootLitter(double v){treeNitrogenFineRootLitter = (float) v;}
	public void setTreeCarbonCoarseRootLitter (double v){treeCarbonCoarseRootLitter = (float) v;}
	public void setTreeNitrogenCoarseRootLitter(double v){treeNitrogenCoarseRootLitter = (float) v;}

	//DEEP ROOTS LITTER--------------------------------------------------------
	public double getTreeCarbonFineRootDeepLitter (){return (double) treeCarbonFineRootDeepLitter;}
	public double getTreeNitrogenFineRootDeepLitter(){return (double) treeNitrogenFineRootDeepLitter;}
	public double getTreeCarbonCoarseRootDeepLitter (){return (double) treeCarbonCoarseRootDeepLitter;}
	public double getTreeNitrogenCoarseRootDeepLitter(){return (double) treeNitrogenCoarseRootDeepLitter;}
	public void setTreeCarbonFineRootDeepLitter (double v){treeCarbonFineRootDeepLitter = (float) v;}
	public void setTreeNitrogenFineRootDeepLitter(double v){treeNitrogenFineRootDeepLitter = (float) v;}
	public void setTreeCarbonCoarseRootDeepLitter (double v){treeCarbonCoarseRootDeepLitter = (float) v;}
	public void setTreeNitrogenCoarseRootDeepLitter(double v){treeNitrogenCoarseRootDeepLitter = (float) v;}



	public double getTotalRootLength () {
		if (this.getFineRoots() != null)
			return this.getFineRoots().getTotalRootLength();
		else
			return 0;
	}
	
	//------------------------------------------------------------------------------------------
//WATER AND NITROGEN BUDGET
	public float getWaterDemand () {return  waterDemand;}
	public void setWaterDemand (double v) { waterDemand =  (float) v;}
	public float getWaterDemandReduced() {return  waterDemandReduced;}
	public void setWaterDemandReduced (double v) { waterDemandReduced = (float) v;}
	public float getHisafeWaterStress () {return hisafeWaterStress;}
	public void setHisafeWaterStress (double v) {hisafeWaterStress = (float) v;}
	public float getWaterUptake() {return  waterUptake;}
	public void  setWaterUptake(double v) {waterUptake =  (float) v;}
	public void addWaterUptake  (double v) {waterUptake  +=  (float) v;}

	
	public float getSticsWaterStomatalStress() {return sticsWaterStomatalStress;}
	public float getSticsWaterTurgescenceStress() {return sticsWaterTurgescenceStress;}
	public float getSticsWaterSenescenceStress () {return sticsWaterSenescenceStress;}

	public float getNitrogenDemand () {return nitrogenDemand;}
	public float getNitrogenUptake () {return nitrogenUptake;}
	public void setNitrogenUptake (double v) {nitrogenUptake =  (float) v;}
	public float getNitrogenRain () {return  nitrogenRain;}
	
	
	
	public float getNitrogenFixation () {return  nitrogenFixation;}
	public float getNitrogenDenitrification () {return  nitrogenDenitrification;}
	public float getBiomassRestitution () {return biomassRestitution;}
	public float getNitrogenLeachingBottom () {return  nitrogenLeachingBottom;}
	public float getNitrogenLeachingArtificial () {return  nitrogenLeachingArtificial;}
	
	
	
	public float getNitrogenLeachingWaterTable() {return  nitrogenLeachingWaterTable;}
	public float getNitrogenAddedByWaterTable () {return  nitrogenAddedByWaterTable;}// AQ 11/04/2011
	public void addNitrogenAddedByWaterTable (double np) {nitrogenAddedByWaterTable +=np;}// AQ 11/04/2011
	public void addNitrogenLeachingWaterTable (double lix) {nitrogenLeachingWaterTable +=lix;}		
	public float getNitrogenImmobilisation () {return  nitrogenImmobilisation;}
	public float getNitrogenVolatilisation () {return  nitrogenVolatilisation;}
	public float getNitrogenVolatilisationOrganic () {return  nitrogenVolatilisationOrganic;}
	public float getHisafeNitrogenStress () {return hisafeNitrogenStress;}
	public void setHisafeNitrogenStress (double v) {hisafeNitrogenStress = (float) v;}

	public float getNitrogenIrrigation () {return  Math.max(nitrogenIrrigation- nitrogenIrrigationPrev,0);}
	public float getNitrogenFertilisationMineral () {return  Math.max(nitrogenFertilisationMineral - nitrogenFertilisationMineralPrev,0);}
	public float getNitrogenFertilisationOrganic () {return  Math.max(nitrogenFertilisationOrganic - nitrogenFertilisationOrganicPrev,0);}
	public float getNitrogenHumusMineralisation () {return  this.sticsSoil.cumvminh;}
	public float getNitrogenResiduMineralisation () {return  this.sticsSoil.cumvminr;}
	public float getCropCarbonLeafLitter () {return  Math.max(cropCarbonLeafLitter - cropCarbonLeafLitterPrev,0);}
	public float getCropNitrogenLeafLitter () {return  Math.max(cropNitrogenLeafLitter - cropNitrogenLeafLitterPrev,0);}
	public float getCropCarbonRootLitter () {return  Math.max(cropCarbonRootLitter - cropCarbonRootLitterPrev,0);}
	public float getCropNitrogenRootLitter () {return  Math.max(cropNitrogenRootLitter - cropNitrogenRootLitterPrev,0);}
	public float getNitrogenHarvested() {return  Math.max(nitrogenHarvested- nitrogenHarvestedPrev,0);}
	public float getBiomassHarvested () {return  Math.max(biomassHarvested- biomassHarvestedPrev,0);}
	
	
	//------------------------------------------------------------------------------------
	//  carbone et azote des résidus du sol
	public float getCarbonResidus() {return  carbonResidus;}
	public float getNitrogenResidus() {return nitrogenResidus;}
	public float getNitrogenResidus2() {return this.sticsCommun.Ntousresidusprofil;}		// total of Nitrogen from residues (all residues on P_profhum) // kgN.ha-1

	
	public float getTotalNitrogenHumusStock(){return this.sticsCommun.Nhumt;}	// Total quantity of N humus (active + inert fractions) in the soil // kg.ha-1
	public float getTotalCarbonHumusStock(){return this.sticsCommun.Chumt;}		// Total amount of C humus (active + inert fractions) in the soil // kg.ha-1
	public float getActiveCarbonHumusStock(){return  this.sticsCommun.Chuma;}
	public float getInactiveCarbonHumusStock() {return this.sticsCommun.Chumi;}	
	public float getActiveNitrogenHumusStock(){return  this.sticsCommun.Nhuma;}	// Amount of active nitrogen in the soil humus pool // kg.ha-1
	public float getInactiveNitrogenHumusStock() {return this.sticsCommun.Nhumi;}
	
	public float getNitrogenLossNitrification() {return this.sticsCommun.em_N2O;}

	
	public float getCnResidus(){return cnResidus;}
	//------------------------------------------------------------------------------------------------------
	public float getNitrogenNutritionIndex() {return nitrogenNutritionIndex;}
	public float getSticsNitrogenLaiStress() {return sticsNitrogenLaiStress;}
	public float getSticsNitrogenBiomassStress () {return sticsNitrogenBiomassStress;}
	public float getSticsNitrogenSenescenceStress () {return sticsNitrogenSenescenceStress;}

	public float getSla(){return sla;}


	//Monthly values for export
	public float getMonthBiomass () {
		if (monthNbrDays > 0)   return  monthBiomass/monthNbrDays;
		else return 0;
		}
	public float getMonthYield() {
		if (monthNbrDays > 0)   return  monthYield/monthNbrDays;
		else return 0;
		}
	public float getMonthEai () {
		if (monthNbrDays > 0)   return  monthEai/monthNbrDays;
		else return 0;
		}
	public float getMonthLai () {
		if (monthNbrDays > 0)   return  monthLai/monthNbrDays;
		else return 0;
		}
	public float getMonthDiffuseParIntercepted () {
		if (monthNbrDays > 0)   return  monthDiffuseParIntercepted/monthNbrDays;
		else return 0;
		}
	public float getMonthDirectParIntercepted () {
		if (monthNbrDays > 0)   return  monthDirecParIntercepted/monthNbrDays;
		else return 0;
		}


	//TOTALS
	public float getTotalCapillaryRise () {return  totalCapillaryRise;}
	public float getTotalIrrigation () {return  totalIrrigation;}
	public float getTotalWaterDemand () {return  totalWaterDemand;}
	public float getTotalWaterDemandReduced() {return  totalWaterDemandReduced;}
	public float getTotalWaterUptake () {return  totalWaterUptake;}
	public float getTotalNitrogenDemand () {return  totalNitrogenDemand;}
	public float getTotalNitrogenUptake () {return  totalNitrogenUptake;}
	public float getTotalSoilEvaporation () {return  totalSoilEvaporation;}
	public float getTotalRunOff () {return  totalRunOff;}
	public float getTotalSurfaceRunOff () {return  totalSurfaceRunOff;}
	public float getTotalDrainageBottom () {return  totalDrainageBottom;}
	public float getTotalDrainageArtificial () {return  totalDrainageArtificial;}
	
	public float getTotalRain () {return  totalRain;}
	public float getTotalParIntercepted () {return  totalParIntercepted;}




	/****************************************************
	MANAGEMENT OF THE RESULTS OF LIGHT COMPETITION MODULE
	*****************************************************/

	// accesors for light budget
	public float getCaptureFactorForDiffusePar(){return captureFactorForDiffusePar;}
	public float getCaptureFactorForDirectPar(){return captureFactorForDirectPar;}
	public float getDirectParIntercepted(){return directParIntercepted;}
	public float getDiffuseParIntercepted(){return diffuseParIntercepted;}
	public float getTotalParInterceptedMJ(){return totalParInterceptedMJ;}
	public float getParIntCum(){return parIntCum;}
	public float getParIntCumByCropPhenoStage(){return parIntCumByCropPhenoStage;}
	public float getCompetitionIndexForTotalPar(){return competitionIndexForTotalPar;}
	public float getInterceptedPar(){return interceptedPar;}

	public void setCaptureFactorForDiffusePar(float e){captureFactorForDiffusePar=e;}
	public void setCaptureFactorForDirectPar(float e){captureFactorForDirectPar=e;}
	public void setDirectParIntercepted(float e){directParIntercepted=e;}
	public void setDiffuseParIntercepted(float e){diffuseParIntercepted=e;}
	public void setTotalParInterceptedMJ(float e){totalParInterceptedMJ=e;}
	public void setParIntCum(float e){parIntCum=e;}
	public void setParIntCumByCropPhenoStage(float e){parIntCumByCropPhenoStage=e;}
	public void setCompetitionIndexForTotalPar(float e){competitionIndexForTotalPar=e;}

	public void addDirect(float e) {this.captureFactorForDirectPar += e;}
	public void addDiffuse(float e){this.captureFactorForDiffusePar += e;}

	public double getParExtinctionCoef () {return (double) parExtinctionCoef;}
	public void setParExtinctionCoef (double v) {parExtinctionCoef = (float) v;}

	
	public double getTetstomate() {return  this.getSticsCrop().tetstomate;}
	public double getTeturg() {return this.getSticsCrop().teturg;}
	
	//MULCH
	public double getCarbonMulch() {return this.sticsCommun.Cmulch;}				// Total C in mulch at soil surface // kg.ha-1
	public double getNitrogenMulch() {return this.sticsCommun.Nmulch;}				// Total N in mulch at soil surface // kg.ha-1
	public double getCarbonMulchTreeFoliage() {return this.sticsCommun.Cnondec[9];}	// undecomposable C in residue i present in the mulch // kg.ha-1
	public double getMulchBiomass() {return this.sticsCommun.qmulch;}				//Quantity of plant mulch // t.ha-1
	public double getMulchEvaporation() {return this.sticsCommun.Emulch;}		    // Direct evaporation of water intercepted by the mulch // mm
	public double getMulchWaterStock() {return this.sticsCommun.mouillmulch;}		// Water stock in the mulch // mm
	public double getMulchCoverRatio() {return this.sticsCommun.couvermulch;}		// Cover ratio of mulch  // 0-1
	

	//POUR FRANCESCO
	public double getTempStressLue() {return  this.getSticsCrop().ftemp;}
	public double getTempStressGrainFilling() {return this.getSticsCrop().ftempremp;}
	public double getFrostStressPlantDensity () {return this.getSticsCrop().fgellev;}
	public double getFrostStressFoliage() {return this.getSticsCrop().fstressgel;}
	public double getFrostStressReprod() {return this.getSticsCrop().fgelflo;}
	
	public double getNitrogenGrain() {return this.qNgrain;}
	public double getCarbonMicrobes() {return this.sticsCommun.Cb;}	
	public double getCarbonMicrobesMulch() {return this.sticsCommun.Cbmulch;}	
	public double getNitrogenMicrobes() {return this.sticsCommun.Nb;}	
	public double getNitrogenMicrobesMulch() {return this.sticsCommun.Nbmulch;}	
			
	public double getWaterUptakePotential () {
		if (fineRoots==null) return 0;
		return fineRoots.getWaterUptakePotential();
	}


	
}


