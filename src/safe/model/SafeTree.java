package safe.model;

import java.awt.Color;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.defaulttype.SimpleCrownDescription;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Speciable;
import capsis.defaulttype.Species;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.kernel.GScene;


/**
 * Safe tree object
 * @author Isabelle Lecomte (INRA SYSTEM) - July 2002 
 */

public class SafeTree extends SpatializedTree implements Speciable, SimpleCrownDescription {

	// WARNING: It's a GMaddTree, see the super class for other states variables
	// dbh is in meter !!!!!!!!!!!!!!!!!!!!!!!

	private SafeTreeSpecies treeSpecies;	//refer to the species parameters
	private boolean planted;			

	//SIZE
	private float dbhMeters;  				//  m (generic dbh is in cm)
	private float crownBaseHeight;  		//  m
	private float crownRadiusTreeLine; 		//  m (Y axix)
	private float crownRadiusInterRow; 		//  m (X axix)
	private float crownRadiusVertical; 		//  m (Z axis)
	private float crownParameter;			//  no unit (used only for paraboloide shape)
	private float crownVolume;				//  m3
	private float stemVolume;				//  m3
	private float leafArea;					//  m2
	private int   nbCellsBellow;			// number of cells bellow the tree crown
	private float laiAboveCells;			//  m2/m2 lai on cell basis
	
	//PHENOLOGY
	private int 		age;								//in days
	private int 		phenologicalStage;					// 0 : before budburst ; 1 : during leaf expansion ; 2 : before leaf fall ; 3 : during leaf fall ; 4 : after leaf fall
	private boolean		firstYearStarted;					//set to true when budburst start the first year (never reset)
	private int			budburstDate;						//julian day
	private float		budburstAccumulatedTemperature;		//day degrees
	private int			leafExpansionEndingDate;			//julian day
	private int			leafFallStartingDate;				//julian day
	private int			leafFallEndingDate;					//julian day

	//COURBAUD LIGHT MODEL CALCULATIONS
	private double	l_captureFactorForDiffusePar;		// m2 d-1
	private double	l_captureFactorForDirectPar;		// m2 d-1
	private double	l_captureFactorForInfraRed;			// m2 d-1
	private double	l_captureFactorForDirectNir;		// m2 d-1
	private double	l_captureFactorForDiffuseNir;		// m2 d-1
	private float	l_directParIntercepted;				// Moles PAR d-1
	private float	l_diffuseParIntercepted;			// Moles PAR d-1
	private float	l_globalRadIntercepted;				// MJ d-1
	private float	l_infraRedIntercepted;				// Watts
	private float	l_cFdirectLonelyTree;				// capture factor for direct Par if the tree were alone
	private float	l_cFdiffuseLonelyTree;				// idem for diffuse Par														//(used for computing the light competition index)
	private float	l_lightCompetitionIndex;			// ratio between Par intercepted and Par intercepted by the same tree alone on the field
	private float 	lastLeafArea;						// in m2 (leaf area used for lighting model triggering)

	//MICROCLIMATE CALCULATION
	private float interceptedRain; 			// l  d-1
	private float storedRain; 				// l  d-1
	private float evaporatedRain; 			// l  d-1
	private float stemflow; 				// l  d-1

	//TRANSPIRATION
	private float stomatalConductance;		//
	private float waterDemand;				// l  
	private float waterDemandReduced;		// l  
	private float waterUptake;				// l  
	private float waterStress;				// 0-1
	private float waterStressSpring;		// 0-1
	private float waterStressSummer;		// 0-1

	//NITROGEN
	private float nitrogenDemand;			// kg N
	private float nitrogenUptake;			// kg N
	private float nitrogenStress;			// 0-1
	private float nitrogenStressSpring;		// 0-1
	private float nitrogenStressSummer;		// 0-1	
	private float nitrogenSaturation;		// 0-1
	private float nitrogenSatisfaction;		// 0-1
	
	//IN SATURATION
	private float waterUptakeInSaturation;		// l  
	private float nitrogenUptakeInSaturation;	// kg N

	//Stres effet
	private float lueStressEffect; 				//stress effect of LUE
	private float rsStressEffect;				//stress effect on shoot root allocation
	
	//C POOLS
	//Structural Carbon in tree pools  (kg C)	
	private float carbonStem;
	private float carbonFoliage;
	private float carbonBranches;
	private float carbonCoarseRoots;
	private float carbonFineRoots;
	private float carbonLabile;
	private float carbonStump;
	
	//senescence
	private float carbonFoliageSen;
	private float carbonFineRootSen;
	private float carbonCoarseRootSen;
	private float carbonFineRootSenAnoxia;
	private float carbonCoarseRootSenAnoxia;
	
	//exportation (remove from the plot after pruning or pollarding) (kg C) 
	private float carbonFoliageExported;
	private float carbonStemExported;
	private float carbonBranchesExported;
	
	//Daily carbon allocated  for  root growth (kg C) 
	private float carbonIncrement;
	private float carbonCoarseRootsIncrement;  
	private float carbonFineRootsIncrement; 
	private float carbonFoliageIncrement;  
	private float carbonStemIncrement; 
	private float carbonBranchesIncrement; 
	private float carbonStumpIncrement; 
	
	
	//N POOLS
	//Structural Nitrogen in tree pools  (kg N)
	private float nitrogenStem;
	private float nitrogenStump;
	private float nitrogenFoliage;
	private float nitrogenBranches;
	private float nitrogenCoarseRoots;
	private float nitrogenFineRoots;
	private float nitrogenLabile;
	
	//exportation (remove from the plot after pruning or pollarding)  (kg N)
	private float nitrogenFoliageExported;
	private float nitrogenStemExported;
	private float nitrogenBranchesExported;
	
	//senescence (kg N)
	private float nitrogenFoliageSen;
	private float nitrogenFineRootSen;
	private float nitrogenCoarseRootSen;
	private float nitrogenFineRootSenAnoxia;
	private float nitrogenCoarseRootSenAnoxia;	

	//Fraction of total Cavailable which is allocated to different organs
	private float carbonAllocToGrowth; 		//Amount of available C allocated to growth (kg C)
	private float aboveGroundCFraction;		//fraction of Carbon total allocated to above ground
	private float coarseRootTotalTargetBiomass; 	//target coarse root biomass for C allocation module
	private float targetLfrRatio;								//target value for carbonFoliage/(carbonFineRoots+carbonFoliage)

	//new data for allocation
	private double aboveGroundAllocFrac;
	private double stemAllocFrac;
	private double branchAllocFrac;
	private double foliageAllocFrac;
	
	private double belowGroundAllocFrac;
	private double stumpAllocFrac;
	private double fineRootAllocFrac;
	private double coarseRootAllocFrac;
			
	private double aboveGroundSink;
	private double stemSink;
	private double branchSink;
	private double foliageSink;
			
	private double belowGroundSink;
	private double stumpSink;
	private double fineRootSink;
	private double coarseRootSink;
			
	//Roots
	private SafeFineRoot fineRoots;			//refer to fine root object
	
	//to set pollarding
	private boolean pollarding;			//set to true year of pollarding (to manage budburst delay)
	private int budburstDelay;			//nbr od days for delaying budburst
	private boolean pollarding2;	
	
	//Annual summaries
	private float leafAreaMax;						//  max for the year m2
	private float carbonFoliageMax;					//  max for the year kg
	private float waterUptakeAnnual;				// l  d-1
	private float nitrogenUptakeAnnual;
	private float parInterceptedAnnual;				// Moles PAR d-1
	private float interceptedRainAnnual; 			// l  d-1
	private float carbonAllocToGrowthAnnual; 		//Amount of available C allocated to growth (kg C)
	private float carbonFoliageSenAnnual;
	private float carbonFineRootSenAnnual;
	private float carbonCoarseRootSenAnnual;	
	private float carbonFineRootSenAnoxiaAnnual;
	private float carbonCoarseRootSenAnoxiaAnnual;
	

	/**
	 * Constructor for new SafeTree.
	 * @see SpatializedTree
	 */
	public SafeTree (	int 	id, 					
						GScene 	stand,
						double	height,
						double	crownBaseHeight,
						double	crownRadius,	
						double	x, 						
						double	y, 						
						double  z,			//GT 2007 slope
						String	treeSpeciesName,	
						SafeInitialParameters	safeSettings)  throws Exception {

		super (id, stand,  0, height, 0, false, x, y, z);		//z = height 

		//Initialise tree species
		//on cherche en priorite les fichiers parametres dans le repertoire de simulation
		//s'il n'existe pas on cherche ensuite dans le repertoire safe/data/simSettings	
		//en dernier lieu on prend le reperoire pas defaut safe/data
		
		try {
			
			String dataPath = safeSettings.dataPath + "/treeSpecies/"+treeSpeciesName+".tree";
			SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
			new SafeTreeFormat (dataPath).load (treeSpecies);
			this.treeSpecies = treeSpecies;

		} catch (CancellationException e) {
			throw e;	
		} catch (Exception e) {

			try {
				
				String dataPath = safeSettings.dataOriginalPath + "/simSettings/"+safeSettings.projectName+"/treeSpecies/"+treeSpeciesName+".tree";					
				SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
				new SafeTreeFormat (dataPath).load (treeSpecies);
				this.treeSpecies = treeSpecies;
				System.out.println("WARNING treeSpecies read in folder "+safeSettings.dataOriginalPath + "/simSettings/"+safeSettings.projectName);
			} catch (CancellationException e1) {
				throw e1;
			} catch (Exception e1) {

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
		}

		this.treeSpecies.setFileName(treeSpeciesName);
		
		//Initialise roots informations
		this.fineRoots = new SafeFineRoot (this);


		//Initialise phenology
		setPlanted (false);
		setFirstYearStarted (false);
		setBudburstAccumulatedTemperature (0);
		setPollarding (false);
		setPollarding2 (false);
		setBudburstDelay (0); 
		setPhenologicalStage(0);
	
		//Initialisation of tree dimension
		setHeight (height);
		setCrownBaseHeight (crownBaseHeight);



		//Tree dimensions are linked through the following allometric relationships : 
		// 1) height=aTree * dbh^bTree
		double aTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		double truncationRatio = this.getTreeSpecies ().getEllipsoidTruncationRatio ();
		
		setDbhMeters (Math.pow (((1/aTree) * height) , (1/bTree)));		//m
		setDbh (getDbhMeters() * 100);									//cm

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
				setCrownRadiusVertical (((height-crownBaseHeight)/(1-truncationRatio))/2);
			}
			double crownArea = Math.PI*Math.pow(crownRadius,2);		// gt - 5.10.2009 - initial crown radius is now given in the pld input file
			//crown radius calcultation limited by tree line and inter-row distance
			double newRadius [] = new double [2];
			newRadius = computeRadiiDeformation (crownArea , (SafeStand) stand);
			setCrownRadiusTreeLine   (Math.max(this.getCrownRadiusTreeLine(),newRadius[0]));
			setCrownRadiusInterRow   (Math.max(this.getCrownRadiusInterRow(),newRadius[1]));

			//Elipsoide crown volume with trucature ratio
			setCrownVolume (Math.PI  * this.getCrownRadiusTreeLine() * this.getCrownRadiusInterRow() * this.getCrownRadiusVertical() *
			 				(2 * (1-truncationRatio) - (1d/3d * (1 - Math.pow (2 * truncationRatio-1,3)))));
		}

		//Paraboloid of revolution crown shape (after B.Courbeaud 2000)
		if (crownShape == 2) {
			setCrownParameter ((aCrown * Math.pow ((this.getDbhMeters () / height), bCrown)));
			setCrownRadiusTreeLine (Math.sqrt (this.getCrownParameter() * (height - crownBaseHeight)));
			setCrownRadiusInterRow (Math.sqrt (this.getCrownParameter() * (height - crownBaseHeight)));
			setCrownRadiusVertical ((height-crownBaseHeight)/2);
			setCrownVolume (1d/2d * Math.PI * this.getCrownParameter ()
							* Math.pow (height-crownBaseHeight,2));
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
		               * Math.pow (getHeight(),cStemVolume));		// gt - 26/10/2010

		
		//initialize leaf area
		setLeafArea (0);
		setLaiAboveCells (0);
		setNbCellsBellow (0);

		//Inititalise daily values
		setWaterStress (1);
		setNitrogenStress (1);
		setNitrogenSaturation (0);
		setNitrogenSatisfaction(1);
		setWaterDemand (0);
		setWaterDemandReduced(0);
		setNitrogenDemand (0);
		setWaterUptake (0);
		setWaterUptakeInSaturation (0);
		setCarbonIncrement (0);
		setNitrogenUptake (0);
		setNitrogenUptakeInSaturation (0);
		setEvaporatedRain (0);
		setStoredRain (0);
		setStemflow (0);

		setCaptureFactorForDiffusePar(0);
		setCaptureFactorForDirectPar(0);
		setCaptureFactorForInfraRed(0);
		setCaptureFactorForDirectNir(0);
		setCaptureFactorForDiffuseNir(0);
		setCFdirectLonelyTree(0);
		setCFdiffuseLonelyTree(0);
		setLightCompetitionIndex(1);
		setDirectParIntercepted(0);
		setDiffuseParIntercepted(0);
		setGlobalRadIntercepted(0);
		setInfraRedIntercepted(0);

		// initialisation of carbon senescence value	
		this.carbonFoliageSen = 0; 
		this.nitrogenFoliageSen = 0; 
		this.carbonFineRootSen = 0;
		this.carbonCoarseRootSen = 0;
		this.carbonFineRootSenAnoxia = 0;
		this.carbonCoarseRootSenAnoxia = 0;		
		this.nitrogenFineRootSen = 0;
		this.nitrogenCoarseRootSen = 0;
		this.nitrogenFineRootSenAnoxia = 0;
		this.nitrogenCoarseRootSenAnoxia = 0;
		this.carbonFineRootsIncrement = 0;
		
		setCoarseRootTotalTargetBiomass(0); // GT 17/07/2008
		

	}

	/**
	 * Clone a SafeTree: first calls super.clone (), then clone the SafeTree instance variables.
	 */
	public Object clone () {

		SafeTree newTree = (SafeTree) super.clone ();

		//Copy roots informations
		newTree.fineRoots = new SafeFineRoot (this.fineRoots);

		

		newTree.setAge(this.getAge());
		newTree.setCaptureFactorForDiffusePar (this.getCaptureFactorForDiffusePar());
		newTree.setCaptureFactorForDirectPar (this.getCaptureFactorForDirectPar());
		newTree.setCaptureFactorForDirectNir (this.getCaptureFactorForDirectNir());
		newTree.setCaptureFactorForDiffuseNir (this.getCaptureFactorForDiffuseNir());
		newTree.setCaptureFactorForInfraRed (this.getCaptureFactorForInfraRed());
		newTree.setCFdiffuseLonelyTree(this.getCFdiffuseLonelyTree());
		newTree.setCFdirectLonelyTree(this.getCFdirectLonelyTree());
		newTree.setLightCompetitionIndex (1);
		newTree.setCarbonIncrement (0);
		newTree.setDirectParIntercepted (0);
		newTree.setDiffuseParIntercepted (0);
		newTree.setGlobalRadIntercepted (0);
		newTree.setInfraRedIntercepted (0);
		newTree.setCoarseRootTotalTargetBiomass(0);
		newTree.setTargetLfrRatio(this.getTargetLfrRatio());

		newTree.setWaterDemand (0);
		newTree.setWaterUptake (0);		
		newTree.setWaterUptakeInSaturation (0);
		newTree.setWaterStress (1);
		newTree.waterStressSpring = this.waterStressSpring;
		newTree.waterStressSummer = this.waterStressSummer;


		newTree.setNitrogenDemand (0);
		newTree.setNitrogenUptake (0);
		newTree.setNitrogenUptakeInSaturation (0);
		newTree.setNitrogenStress (1); 
		newTree.nitrogenStressSpring = this.nitrogenStressSpring;
		newTree.nitrogenStressSummer = this.nitrogenStressSummer;
		
		newTree.setStomatalConductance(this.getStomatalConductance ());
		
		newTree.setPlanted (this.isPlanted ());
		newTree.setFirstYearStarted (this.isFirstYearStarted ());
		newTree.setPollarding (this.isPollarding ());
		newTree.setPollarding2 (this.isPollarding2 ());
		newTree.setBudburstDelay (this.getBudburstDelay ());
		
		newTree.setInterceptedRain (0);	//IL 07/02/2018
		newTree.setStemflow (0);		//IL 07/02/2018
		newTree.setEvaporatedRain (0);	//IL 03/07/2018
		newTree.setCarbonFineRootsIncrement(0);
		
		//IL 28/02/2018 on traite la min�ralisation des liti�res de feuilles � J+1
		//pour les racines pas la peine, on part des valeurs dans les voxels
		newTree.setCarbonFoliageSen (this.getCarbonFoliageSen()); 	
		newTree.setNitrogenFoliageSen (this.getNitrogenFoliageSen());
		
		newTree.setCarbonFineRootSen (0);
		newTree.setCarbonCoarseRootSen (0);
		newTree.setNitrogenFineRootSen (0);
		newTree.setNitrogenCoarseRootSen (0);
		
		newTree.setCarbonFineRootSenAnoxia (0);
		newTree.setCarbonCoarseRootSenAnoxia (0);
		newTree.setNitrogenFineRootSenAnoxia (0);
		newTree.setNitrogenCoarseRootSenAnoxia (0);
		
		//exportation is NOT cumulated
		newTree.setCarbonStemExported (0);
		newTree.setCarbonFoliageExported (0);
		newTree.setCarbonBranchesExported (0);
		newTree.setNitrogenStemExported (0);
		newTree.setNitrogenFoliageExported (0);
		newTree.setNitrogenBranchesExported (0);		

		newTree.setNbCellsBellow (0);
		
		return newTree;
	}
	/**
	 * Reload the tree species parameter file
	 * used when we reopen a project and changing some tree parameters 
	 */
	public void reloadSpecies (SafeEvolutionParameters ep, SafeInitialParameters safeSettings, String treeSpeciesName) throws Exception {

		//Initialise tree species
		//on cherche en priorite les fichiers parametres dans le repertoire de simulation
		//s'il n'existe pas on cherche ensuite dans le repertoire safe/data/simSettings	
		//en dernier lieu on prend le repertoire pas defaut safe/data
		
		try {
			
			String dataPath = ep.simulationDir + "/treeSpecies/"+treeSpeciesName+".tree";
			SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
			new SafeTreeFormat (dataPath).load (treeSpecies);
			this.treeSpecies = treeSpecies;
			System.out.println("RELOAD treeSpecies dataPath="+dataPath);
		} catch (CancellationException e) {
			throw e;	
		} catch (Exception e) {

			try {
				
				String dataPath = safeSettings.dataOriginalPath + "/simSettings/"+safeSettings.projectName+"/treeSpecies/"+treeSpeciesName+".tree";					
				SafeTreeSpecies treeSpecies	= new SafeTreeSpecies ();
				new SafeTreeFormat (dataPath).load (treeSpecies);
				this.treeSpecies = treeSpecies;
				System.out.println("WARNING treeSpecies read in folder "+safeSettings.dataOriginalPath + "/simSettings/"+safeSettings.projectName);
			} catch (CancellationException e1) {
				throw e1;
			} catch (Exception e1) {

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
		}
		
		this.treeSpecies.setFileName(treeSpeciesName);

	}
	/**
	 * Planting a SafeTree.
	 */
	public void plant () {

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
		
		setTargetLfrRatio (initialTargetLfrRatio);
		
		setCarbonStem (getStemVolume()			// m3
						* woodDensity 			// kg m-3
						* woodCarbonContent);	// %
						

		if (!((this.phenologicalStage==0)||(this.phenologicalStage==4))){
			setCarbonFoliage  (aLeafArea*Math.pow(getCrownVolume(),bLeafArea)		// m2 
								* leafMassArea		// kg m-2
								* leafCarbonContent);	// %
		} else setCarbonFoliage (0);

		//BranchVolume is set to a fixed fraction of total crown volume (to be refined later based on AMAP architectural models)
		double branchVolumeRatio 	= this.getTreeSpecies ().getBranchVolumeRatio ();
		double branchVolume = (this.getCrownVolume() * branchVolumeRatio);
		
		setCarbonBranches(branchVolume * woodDensity * woodCarbonContent);
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
		setNitrogenFoliage (getCarbonFoliage() * optiNCFoliage);
		setNitrogenBranches	(getCarbonBranches() * optiNCBranch) ;
		setNitrogenFineRoots  (carbonFineRoot * optiNCFineRoot);
		
		// initialisation of carbon senescence value	
		this.carbonFoliageSen = 0; 
		this.nitrogenFoliageSen = 0; 
		this.carbonFineRootSen = 0;
		this.carbonCoarseRootSen = 0;
		this.carbonFineRootSenAnoxia = 0;
		this.carbonCoarseRootSenAnoxia = 0;		
		this.nitrogenFineRootSen = 0;
		this.nitrogenCoarseRootSen = 0;
		this.nitrogenFineRootSenAnoxia = 0;
		this.nitrogenCoarseRootSenAnoxia = 0;
		this.carbonFineRootsIncrement = 0;


	}
	/**
	 * Calculate totals for annual Export 
	 */	
	public void processTotal () {

		if (this.leafArea > this.leafAreaMax) this.leafAreaMax = this.leafArea; 
		if (this.carbonFoliage > this.carbonFoliageMax) this.carbonFoliageMax = this.carbonFoliage; 
		this.waterUptakeAnnual += this.getWaterUptake();
		this.nitrogenUptakeAnnual += this.getNitrogenUptake();
		this.parInterceptedAnnual +=  this.l_directParIntercepted + this.l_diffuseParIntercepted;
		this.interceptedRainAnnual +=  this.interceptedRain;	
		this.carbonAllocToGrowthAnnual +=  this.carbonAllocToGrowth;
		this.carbonFoliageSenAnnual +=  this.carbonFoliageSen;
		this.carbonFineRootSenAnnual +=  this.carbonFineRootSen;
		this.carbonCoarseRootSenAnnual +=  this.carbonCoarseRootSen;
		this.carbonFineRootSenAnoxiaAnnual +=  this.carbonFineRootSenAnoxia;
		this.carbonCoarseRootSenAnoxiaAnnual +=  this.carbonCoarseRootSenAnoxia;
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
		carbonFineRootSenAnnual = 0;
		carbonCoarseRootSenAnnual = 0;
		carbonFineRootSenAnoxiaAnnual = 0;
		carbonCoarseRootSenAnoxiaAnnual = 0;
	}

	
	
	/**
	* Tree fine roots initialisation
	* Done before coarse Root
	*/
	public void fineRootsInitialisation (SafePlot plot, SafeInitialValues initialValues, SafeInitialParameters settings, 
										 SafePlotSettings plotSettings, SafeEvolutionParameters evolutionParameters) {

	
		
		int treeIndex = this.getId() - 1;		//index of this tree
		Vertex3d treeCoordinate = new Vertex3d(this.getX(),this.getY(),this.getZ());	

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
		
		int rootShape = initialValues.rootShape[treeInitIndex];
		double shapeParam1=0;
		double shapeParam2=0;
		double shapeParam3=0;

		// Available equations for carbon repartition are :
		//  1- Uniform  2- Reverse to the tree distance 3- Negative exponential
		int rootRepartition = initialValues.rootRepartition[treeInitIndex];

		if (rootShape == 1) {	//sphere
			shapeParam1 = initialValues.shapeParam1[treeInitIndex];
		}
		if (rootShape == 2) {	//elipsoide
			shapeParam1 = initialValues.shapeParam1[treeInitIndex];
			shapeParam2 = initialValues.shapeParam2[treeInitIndex];
			shapeParam3 = initialValues.shapeParam3[treeInitIndex];
		}
		if (rootShape == 3) {	//cone
			shapeParam1 = initialValues.shapeParam1[treeInitIndex];
			shapeParam2 = initialValues.shapeParam2[treeInitIndex];
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
			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
				SafeCell cell = (SafeCell) c.next ();
				SafeVoxel [] voxel = cell.getVoxels();
				for (int i=0; i<voxel.length; i++) {
				
					if (voxel[i].isVoxelInShape (rootShape, treeCoordinate, shapeParam1, shapeParam2,shapeParam3,
												plotWidth, plotHeight, evolutionParameters)) {

						//compute the fine root density value
						voxel[i].computeTreeFineRootRepartition (treeIndex, rootRepartition,
																totalTreeRootLength, totalCoefficient);
						
						//ADD empty topology to be filled by coarse root initialisation	

						if (!this.getFineRoots().getRootTopology().containsKey(voxel[i]))
							this.getFineRoots().addEmptyRootTopology (voxel[i]);
					}						
				}
			}
		}
	}
	
	/**
	* Tree coarse roots initialisation
	*/
	public void coarseRootsInitialisation (	SafePlot plot, 
											SafeInitialValues initialValues, 
											SafeInitialParameters settings, 
											SafePlotSettings plotSettings) throws Exception  {		
		
		PlotOfCells plotc = (PlotOfCells) plot; // fc-30.10.2017
				
		//Coarse root topology type defines priority to parent determination
		double  coarseRootTopologyType 	= this.getTreeSpecies().getCoarseRootTopologyType();

		///Search the voxel reference where the tree is planted
		int treeId = this.getId() - 1 ;
		SafeCell plantedCell = (SafeCell) (this.getCell ());
		SafeVoxel [] voxel = plantedCell.getVoxels();
		SafeVoxel plantedVoxel = voxel[0];
		plantedVoxel.setColonisationDirection(treeId, 4);
		
		//Init TREE coarse root node
		SafeRootNode plantedNode = this.getFineRoots().getRootTopology (plantedVoxel);
		this.getFineRoots().setFirstRootNode (plantedNode);

		//for each voxel of the PLOT
		for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			SafeVoxel [] voxels = cell.getVoxels();
			for (int i=0; i<voxels.length; i++) {

				//if this voxel is rooted by this TREE
				if (voxels[i].getTheTreeRootDensity (treeId) > 0) {
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
								densityMax = topVoxel.getTheTreeRootDensity (treeId);
							}
							//explore other voxel in case of highest density
							for (int k=0; k<4; k++) {
								if (neighbourgs[k] != null) {
									if (neighbourgs[k].getTheTreeRootDensity (treeId) > densityMax) {
										densityMax  = neighbourgs[k].getTheTreeRootDensity (treeId);
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
										if (neighbourgs[k].getTheTreeRootDensity (treeId) == densityMax) {
											if (neighbourgs[k].getTheTreeDistance (treeId) < distanceMin) {
												distanceMin = neighbourgs[k].getTheTreeDistance (treeId);
												densityMax  = neighbourgs[k].getTheTreeRootDensity (treeId);
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
									if (neighbourgs[k].getTheTreeRootDensity (treeId) > densityMax) {
										distanceMin = neighbourgs[k].getTheTreeDistance (treeId);
										densityMax  = neighbourgs[k].getTheTreeRootDensity (treeId);
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
								if (topVoxel.getTheTreeRootDensity (treeId) > densityMax) {
									parentVoxel = topVoxel;		
									direction = 4;
								}
							}
						}
					}

					//UPDATE empty topology already created in fine root initialisation
					if (this.getFineRoots().getRootTopology ().containsKey(voxels[i])) {
						SafeRootNode node = this.getFineRoots().getRootTopology (voxels[i]);
  						if (parentVoxel != null) {
								SafeRootNode fatherNode = this.getFineRoots().getRootTopology (parentVoxel);
								node.setNodeParent(fatherNode);
								fatherNode.addNodeColonised (node);									
								node.computeDistanceFromFather(direction);	//calculate distance from father
								node.computeEffDist(this);
								voxels[i].setColonisationDirection(treeId, direction);
						}
  						node.setFineRootDensity (voxels[i].getTheTreeRootDensity (treeId));
					}
				}
			}
		}
		
		//recursive algorithme for updating distances and biomass
		if (plantedNode != null) {
			plantedNode.setDistances(0);
			//Initialise coarse root biomass
			this.coarseRootsBiomassInitialisation (plot, initialValues, settings, plotSettings);	// gt - 5.10.2009 - added initialValues
		}
		else {
			System.out.println("Tree planting with no roots ");
			throw new CancellationException();	// abort
		}
	
	}
	
	/**
	 * Coarse roots biomass initialisation
	 * @author Rachmat MULIA (ICRAF) - August 2004
	 * updated by G.Talbot and I.Lecomte - September 2008
	 */
	public void coarseRootsBiomassInitialisation (SafePlot plot, SafeInitialValues initialValues, SafeInitialParameters settings, SafePlotSettings plotSettings) {	// gt - 5.10.2009 - added initialValues
		
		//if this tree  has fine roots !!!!
		if (this.getFineRoots().getFirstRootNode() != null) {

			
			int treeIndex = this.getId() - 1;		//index of this tree
			int treeInitIndex = treeIndex;

			//compute coarse roots target biomass proportionnally to total tree fine roots
			this.setCoarseRootTotalTargetBiomass(0);
			SafeRootNode firstNode = this.getFineRoots().getFirstRootNode () ;
			firstNode.computeCoarseRootTargetBiomass (this, null,
										this.getTreeSpecies().getWoodDensity(),
										this.getTreeSpecies ().getWoodCarbonContent (),
										this.getTreeSpecies().getCRAreaToFRLengthRatio());

			this.setCarbonCoarseRoots (this.getCoarseRootTotalTargetBiomass());

			//calculation of coarse root biomass for each root node
			firstNode.computeCoarseRootBiomass (treeIndex, this.getCoarseRootTotalTargetBiomass(), this.getCoarseRootTotalTargetBiomass(), this.getCoarseRootTotalTargetBiomass());


			//recalculation of fine root density for each root node and each voxel
//			for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
//				SafeCell cell = (SafeCell) c.next ();
//				SafeVoxel [] voxel = cell.getVoxels();
//				for (int i=0; i<voxel.length; i++) {
//
//					double treeRootDensity = voxel[i].getTheTreeRootDensity (treeIndex);
//					if (treeRootDensity > 0) {
//						double newTreeRootDensity  = initialValues.amount[treeInitIndex]*treeRootDensity;
//						voxel[i].setTreeRootDensity (treeIndex, newTreeRootDensity);	
//						this.getFineRoots().getRootTopology(voxel[i]).setFineRootDensity(newTreeRootDensity);
//						this.getFineRoots().getRootTopology(voxel[i]).setFineRootTotalInvestment(newTreeRootDensity);
//					}
//				}
//			}
//			//Carbon fine root are recalculated with initial value amount (%) 
//			this.setCarbonFineRoots(initialValues.amount[treeIndex]*this.getCarbonFineRoots());
//			
			//IL 12/096/2018 ne sert � rien ici
			//this.computeTotalRoot(settings);

			//recalculation of Nitrogen pools
			setAboveGroundCFraction ((getCarbonBranches() + getCarbonStem())
						/ (getCarbonBranches() + getCarbonStem() + getCarbonStump()+ getCarbonCoarseRoots ()));


			setCarbonLabile((carbonStem+carbonBranches+carbonCoarseRoots+carbonStump)*this.getTreeSpecies().getTargetNSCFraction());

			double optiNCCoarseRoot = this.getTreeSpecies ().getOptiNCCoarseRoot ();
			double targetNCoefficient = this.getTreeSpecies ().getTargetNCoefficient ();
			setNitrogenCoarseRoots  (carbonCoarseRoots * optiNCCoarseRoot);
			setNitrogenLabile(targetNCoefficient*getTotalOptiN() - getTotalN());

		}
	}	
	/**
	 * Process growth part I (before water repartition module)
	 */
	public boolean processGrowth1 (SafeStand stand,
									SafeInitialParameters safeSettings,
									SafePlotSettings plotSettings,
									SafeDailyClimat dayClimat,
									SafeMacroClimat macroClimat) {	
		boolean visibleStep = false;

		// J=365 , the tree is one year older
		int julianDay = dayClimat.getJulianDay ();
		int year = dayClimat.getYear ();

		// J=365 , the tree is one year older		
		if (julianDay ==1)
			this.age = this.age + 1;


		// Tree phenology : calculation of the growth status
		if (this.getTreeSpecies ().getPhenologyType () == 1)		// cold deciduous trees
				this.computePhenology (dayClimat);

		if (this.getBudburstDate () == julianDay) 		{
			visibleStep = true;
			stand.setDisplay ("Budburst date");
		}

		if (this.getLeafFallEndingDate () == julianDay) {
			visibleStep = true;
			stand.setDisplay ("End of leaf fall date");
		}

		//Sum of fine roots calculation (for water repartition module)
		this.computeTotalRoot(safeSettings);


		// Potential transpiration

		if (this.getLeafArea() != 0) {		// gt - 01.10.2009 - added phenological stage != : if leafFall began, no more water demand

			double demand = 0;
			double evaporation = 0;
			
			//if there is leaves AND leaves fall is NOT begin
			if ((this.phenologicalStage!=3)&&(this.phenologicalStage!=4))
			 demand = this.computeWaterDemand (dayClimat);

			evaporation = this.computeEvaporation (demand);

			this.setWaterDemand (demand - (evaporation*0.99));
			
			if (this.getWaterDemand () > 0) {
				//double waterDemand = this.getWaterDemand() / stand.getArea();	//convert liters to mm		// gt - 12.11.2009 - water demand expressed in liters
				this.getFineRoots().calculatePotential (safeSettings, this.getWaterDemand ());

				// Reduction factor for water demand
				//IF RACHMAT VERSION ONLY !!!!
				double waterDemandReductionFactor = this.getFineRoots().getWaterDemandReductionFactor();
				setWaterDemandReduced (this.getWaterDemand() * waterDemandReductionFactor);		//liters
			}

			//Nitrogen demand calculation
			double nitrogenDemand = this.computeNitrogenDemand ();
			setNitrogenDemand (nitrogenDemand);

			//Nitrogen stress calculation
			double nitrogenStress = this.computeNitrogenStress ();
			setNitrogenStress (nitrogenStress);
			

			//Nitrogen sink strength with nitrogen demand of the day before
			if (this.getNitrogenDemand () > 0) 
					this.getFineRoots().calculateNitrogenSinkStrength (safeSettings, this.getNitrogenDemand ());
			
		} 
		else
		{
			setWaterDemand (0);
			setWaterDemandReduced (0);
			setWaterUptake (0);
			setWaterStress (1);
			setNitrogenDemand (0);
			setNitrogenUptake (0);
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
								SafeInitialParameters safeSettings,
								SafePlotSettings plotSettings,
								SafeDailyClimat dayClimat,
								int simulationDay) {


		double carbonIncrement = 0;

		// J=365 , the tree is one year older
		int julianDay = dayClimat.getJulianDay ();
		
		
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
		
		
		//setNitrogenStress(1);	// (To inactivate nitrogen stress)	-- AQ

		//Compute spring and summer stress
		if (julianDay >= 91 && julianDay < 181) {
			nitrogenStressSpring = nitrogenStressSpring + (1-nitrogenStress); 
		}
		if (julianDay > 181 && julianDay < 273) {
			nitrogenStressSummer = nitrogenStressSummer + (1-nitrogenStress); 
		}	
		

		double lueStressEffect = 1;
		//Conversion of light intercepted (Moles PAR) to carbon (kg)
		if ((this.getLeafArea() != 0)&&(this.phenologicalStage!=3)&&(this.phenologicalStage!=4)) {		// gt - 01.10.2009 - added phenological stage != : if leafFall began, no more photosynthesis
			
			double lueMax = this.getTreeSpecies ().getLueMax();		//gr C MJ-1 (PAR)
			int tmax=getBudburstDate()+this.getTreeSpecies ().getLeafAgeForLueMax();
			double lue = Math.max(lueMax*(1-	this.getTreeSpecies().getLeafSenescenceTimeConstant()*Math.pow(simulationDay-tmax,2)),0);
			
			//ADD il+kw 06.07.2018
			//add lue stress Responsiveness parameters
			
			double lueStressMethod = treeSpecies.getLueStressMethod ();
			double lueWaterStressResponsiveness    = treeSpecies.getLueWaterStressResponsiveness ();
			double lueNitrogenStressResponsiveness = treeSpecies.getLueNitrogenStressResponsiveness ();
			
			double lueWaterStress     = Math.pow (getWaterStress(),lueWaterStressResponsiveness);
			double lueNitrogenStress  = Math.pow (getNitrogenSatisfaction(), lueNitrogenStressResponsiveness);
			if (lueStressMethod == 1)        
				lueStressEffect = lueWaterStress * lueNitrogenStress;
			else 
				lueStressEffect = Math.min(lueWaterStress, lueNitrogenStress);


			carbonIncrement =   (getDiffuseParIntercepted () + getDirectParIntercepted ())
								* safeSettings.molesParCoefficient		 //Conversion from Moles PAR to MJoules
								* lueStressEffect
								* lue									// gr C MJ-1 (PAR)
								/ 1000;										// gr to kg
		}

		setCarbonIncrement (carbonIncrement);
		setLueStressEffect (lueStressEffect);
		
		//Fine Root senescence 
		this.setCarbonFineRootSen(0); 
		this.setCarbonCoarseRootSen(0);
		this.setNitrogenFineRootSen (0);
		this.setNitrogenCoarseRootSen (0);	
		this.setCarbonFineRootSenAnoxia (0);
		this.setCarbonCoarseRootSenAnoxia (0);
		this.setNitrogenFineRootSenAnoxia (0);
		this.setNitrogenCoarseRootSenAnoxia (0);
		
		if (isFirstYearStarted ()) {
			if (getFineRoots().getFirstRootNode() != null) {
				getFineRoots().getFirstRootNode().computeCoarseRootTargetBiomass(this, null, 
																this.getTreeSpecies().getWoodDensity() , 
																this.getTreeSpecies ().getWoodCarbonContent () , 
																this.getTreeSpecies().getCRAreaToFRLengthRatio());
				
				getFineRoots().getFirstRootNode().computeRootSenescence(this, safeSettings);
			}			
		}
		
		//Carbon foliage senescence calculation
		this.setCarbonFoliageSen(0); 
		this.setNitrogenFoliageSen(0);
		if ((this.getCarbonFoliage() != 0) && ((this.phenologicalStage==3)||(this.phenologicalStage==4))) 		
			this.computeCarbonFoliageSenescence(julianDay);
		
		//C allocation in different pools (stem, branches, foliage, roots)
		this.computeCarbonAllocation (stand, safeSettings, plotSettings, simulationDay,
								getCarbonIncrement(),
								this.getNitrogenUptake ());	
		
		//Allometric growth (dbh, height, crownDiameter)
		this.computeAllometricGrowth (stand, safeSettings);
		
			//Fine Root growth
		if (this.getCarbonFineRootsIncrement() > 0)
			this.fineRootGrowth(safeSettings, plotSettings, simulationDay, this.getCarbonFineRootsIncrement(), true);
		
		//Coarse Root growth
		if (this.getCarbonCoarseRootsIncrement () > 0)
			this.coarseRootGrowth(this.getCarbonCoarseRootsIncrement());
		
		//IL 12/06/2018
		//i think this is not usefull
		//this.updateRoots(safeSettings);
	
	}


	/**
	 * Phenology module for cold deciduous SafeTree
	 * @author Christian DUPRAZ (INRA SYSTEM) - July 2003
	 * updated by Gregoire Talbot on 29/04/2009
	 */
	private void computePhenology (SafeDailyClimat dayClimat) {
	 /*	Calculation of  :
		- budburst depending of accumulation of day degrees
		- end of leaf area expansion depending of a threshold date
		- leaf fall depending of accumulation of degree-days	*/
		int julianDay = dayClimat.getJulianDay ();
		double leafFallFrostThreshold = this.getTreeSpecies().getLeafFallFrostThreshold(); //threshold of daily temperature that trigger leaf fall
		float minTemperature = dayClimat.getMinTemperature();

		switch(this.phenologicalStage){
			case 0 : // before budburst
				int dayStart = this.getTreeSpecies ().getBudBurstTempAccumulationDateStart ();	// date to start temperature cumul (julian day)
	
				if(julianDay >= dayStart){
					double budburstTempThreshold = this.getTreeSpecies ().getBudBurstTempThreshold ();	
					double budburstAccumulatedTemp = this.getTreeSpecies ().getBudBurstAccumulatedTemp ();	
					float meanTemperature = dayClimat.getMeanTemperature ();
					this.budburstAccumulatedTemperature += Math.max(meanTemperature-budburstTempThreshold,0);
					
					if (this.budburstAccumulatedTemperature >= budburstAccumulatedTemp) {
						//after pollarding, budburst is delayed 
						if (this.isPollarding()) {
							if (getBudburstDelay() > 0 ) {
								setBudburstDelay(getBudburstDelay()-1);
							}
							else {
								setPollarding (false);
							}
						}
						else {
							this.phenologicalStage ++; 		//budburst is STARTING !
							this.budburstAccumulatedTemperature=0;
							setBudburstDate (julianDay);
							setFirstYearStarted (true);
						}
					}
				}
			break;
			case 1 : // budburst started
				int leafExpansionDuration = this.getTreeSpecies ().getLeafExpansionDuration ();		// date for the end of leaf expansion (julian day)
				//after pollarding, leaf expension duration is expended to leaf fall  
				if (this.isPollarding2()) 
					leafExpansionDuration = this.getTreeSpecies().getBudBurstToLeafFallDuration();	
	
				if(julianDay-this.getBudburstDate()>leafExpansionDuration){
					this.phenologicalStage ++;	// end of leaf expansion
					setLeafExpansionEndingDate (julianDay);
					setPollarding2 (false);
				}
			break;
			case 2 : // leaf expansion finished
				int budBurstToLeafFallDuration = this.getTreeSpecies().getBudBurstToLeafFallDuration();		// date for the end of leaf expansion (julian day)
				//if((/*meanTemperatureLast15Days*/ meanT<leafFallThresholdTemp)||(minTemperature<leafFallFrostThreshold)||(julianDay>=340)){		// gt - 09.10.2009
					//this.phenologicalStage ++;	// beginning of leaf fall
					//setLeafFallStartingDate (julianDay);
				//}
				if(julianDay-this.getBudburstDate()>budBurstToLeafFallDuration){
					this.phenologicalStage ++;	// beginning of leaf fall
					setLeafFallStartingDate (julianDay);
				} else {
					if(minTemperature<leafFallFrostThreshold){
						this.phenologicalStage ++;	// beginning of leaf fall
						setLeafFallStartingDate (julianDay);
					}
						
				}
					
			break;
			case 3 :	// leaf fall began
				int leafFallDuration = this.getTreeSpecies ().getLeafFallDuration ();
				if((julianDay>=365)||(julianDay>=(this.getLeafFallStartingDate()+leafFallDuration))){
					this.phenologicalStage ++;	// end of leaf fall
					setLeafFallEndingDate (julianDay);
				}
			break;
			case 4 :	// leaf fall finished
				if(julianDay<=10){
					this.phenologicalStage=0;
				}
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
			if (this.getPhenologicalStage() ==3) {
				evaporation = this.getStoredRain ();
			}
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
		double totalOptiN = (double) getTotalOptiN();
		double totalN = (double) getTotalN();
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
	/**
	 * C allocation module
	 * @author Gregoire VINCENT (IRD) - August 2003
	 */
	private void computeCarbonAllocation (SafeStand stand,
									SafeInitialParameters safeSettings,
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
		double maxCrownRadiusInc    = treeSpecies.getMaxCrownRadiusInc();
		
		
		//Tree specific parameters for Non-structural carbon
		double targetNSCFraction		= treeSpecies.getTargetNSCFraction ();
		double maxNSCUseFraction		= treeSpecies.getMaxNSCUseFraction ();
		double maxNSCUseFoliageFraction = treeSpecies.getMaxNSCUseFoliageFraction();

		//arbitrary level of imbalance above which remobilisation of reserves is triggered
		double imbalanceThreshold 	= treeSpecies.getImbalanceThreshold ();

		/*
		* Calculation of above ground imbalance
		* Deduction of carbon exchange with NSC and total carbon allocation to growth
		*/
		double NSCExchange = 0;
		double aboveGroundImbalance = 0;
		
		double starCarbonFoliage = aLeafArea*Math.pow(getCrownVolume(),bLeafArea)*leafCarbonContent*leafMassArea;	// gt - 26/10/2010 : targetCarbonFoliage
		aboveGroundImbalance = Math.max (1 - getCarbonFoliage ()/ starCarbonFoliage, 0);	// gt - 27.01.2009
		double respi = 0;//0.0002*(getCarbonBranches()+getCarbonCoarseRoots()+getCarbonStem()+getCarbonFineRoots()+getCarbonFoliage());
		//double respi=0.00357*getCarbonFoliage()+0.000854*getCarbonFineRoots()+0.00014*(getCarbonBranches()+getCarbonCoarseRoots()+getCarbonFineRoots());
										
		if(this.phenologicalStage==1){
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
		
		if ((aboveGroundImbalance > imbalanceThreshold) && (this.phenologicalStage==1)){
			// gt - 26/10/2010 : getTotalStructuralCarbon replaced by starCarbonFoliage to avoid a too fast budburst
			//NSCExchange = Math.min (maxNSCFraction * getCarbonLabile(), getTotalStructuralCarbon () * maxDailyNSC);
			NSCExchange = Math.min (getCarbonLabile() * maxNSCUseFraction, starCarbonFoliage * maxNSCUseFoliageFraction);	
		}

		
		double NSCDesat = (getCarbonLabile()-NSCExchange)/ ((getCarbonBranches() + getCarbonStem()+ getCarbonStump()+ getCarbonCoarseRoots())*targetNSCFraction);
		double nsSink ;
		if(NSCDesat>=2){
			nsSink=0;
		} else {			
			nsSink = Math.exp((1/NSCDesat-1/(2-NSCDesat)));
		}

		
		double savingFraction = nsSink/(nsSink+1);
		
		carbonAllocated += NSCExchange;

		setCarbonLabile (getCarbonLabile () - NSCExchange+carbonAllocated*savingFraction);

	

		
		carbonAllocated*=(1-savingFraction);

		setCarbonAllocToGrowth (carbonAllocated);


		/*
		* Effect of water and nitrogen stress and nitrogen saturation on targetLfrRatio
		*/

		//Stress effect is the minimum value for the nbr days for stress memory (tree species parameter)
		double rsStressEffect = 1;
		double minTargetLfrRatio =treeSpecies.getMinTargetLfrRatio ();
		double maxTargetLfrRatio =treeSpecies.getMaxTargetLfrRatio ();

		if((this.phenologicalStage==1)||(this.phenologicalStage==2)){ 

			double maxTargetVariation	= treeSpecies.getMaxTargetLfrRatioDailyVariation ();
			double targetLfrRatioUpperDrift = treeSpecies.getTargetLfrRatioUpperDrift ();
			double rsStressMethod = treeSpecies.getRsStressMethod ();
			double rsNitrogenStressResponsiveness 	= treeSpecies.getRsNitrogenStressResponsiveness ();
			double rsWaterStressResponsiveness    	= treeSpecies.getRsWaterStressResponsiveness ();
			double rsWaterStress      = Math.pow (getWaterStress(),rsWaterStressResponsiveness);
			double rsNitrogenStress = Math.pow (getNitrogenSatisfaction(),rsNitrogenStressResponsiveness);
			if (rsStressMethod == 1)        
				rsStressEffect = rsWaterStress * rsNitrogenStress;
			else 
				rsStressEffect = Math.min(rsWaterStress, rsNitrogenStress); 


			if(rsStressEffect > 0.98){
				// derive vers le haut
				double RSNoStressResponsiveness = treeSpecies.getRsNoStressResponsiveness ();
				rsStressEffect = 1  +Math.pow(getNitrogenSaturation(),1/RSNoStressResponsiveness);
				targetLfrRatio += targetLfrRatioUpperDrift*maxTargetVariation*rsStressEffect;				
			} else {
				targetLfrRatio -= maxTargetVariation*(1-rsStressEffect);
			}
		}
			
		setTargetLfrRatio (Math.max(minTargetLfrRatio, Math.min(targetLfrRatio, maxTargetLfrRatio)));
		setRsStressEffect (rsStressEffect);
		
		/*
		* Calculation of aboveground allocation fraction :
		* 	- The allocation ratio is calculated to reach a target value for the
		*		ratio leaf/fineRoots  (targetLfrRatio) during leaf expansion
		*	-  When leaf expansion if finished, aboveGroundAllocFraction is aboveGroundCfraction
		*/
		this.aboveGroundAllocFrac = 0;
		this.belowGroundAllocFrac = 0;

		if(carbonAllocated>0){
				double rStar = this.getTargetLfrRatio();
				double epsiA = this.getCarbonAboveGroundEff();
				double epsiB = getCarbonBelowGroundEff();
				double cFr = getCarbonFineRoots();
				double cL = getCarbonFoliage();
				
				if(this.phenologicalStage != 1){
					cL = getCarbonFoliage();
					epsiA = cL/(cL+getCarbonStem()+getCarbonBranches());
				}
				aboveGroundAllocFrac = (rStar*(cFr+cL+epsiB*carbonAllocated)- cL) / (carbonAllocated*(epsiA+rStar*(epsiB-epsiA)));
				aboveGroundAllocFrac = Math.max(0,Math.min(1,aboveGroundAllocFrac));
				belowGroundAllocFrac = 1 - aboveGroundAllocFrac;
		}
		/*
		* Computation of sink forces of fine roots and coarse roots using the explicit
		* topological rooting system
		*	- a priori allocation of all carbon to fine roots
		*	- calculation of total coarse root biomass needed to support fine root system
		*	- sink forces computation
		*/

		double belowGroundGrowth = carbonAllocated * belowGroundAllocFrac;
		
		this.fineRootAllocFrac=0;
		this.coarseRootAllocFrac=0;
		this.fineRootSink = 0;
		this.coarseRootSink=0;
		this.belowGroundSink=0;
		double fineRootsGrowth=0;
		double coarseRootsGrowth=0;

		if(belowGroundGrowth>0){
			
			fineRootSink = belowGroundGrowth;
			
			// cellular automata is called just for FR/CR allocation computation
			// the method computing the needed coarse root biomass is called within
			// the cellular automata
			fineRootGrowth (safeSettings, plotSettings, simulationDay,fineRootSink,false);

			// 2) sink force computation
			coarseRootSink = Math.max(getCoarseRootTotalTargetBiomass()-getCarbonCoarseRoots(),0);
			
			if((coarseRootSink+fineRootSink)>belowGroundGrowth){
				belowGroundSink = coarseRootSink + fineRootSink;
				fineRootAllocFrac = fineRootSink/belowGroundSink;
				fineRootsGrowth = belowGroundGrowth * fineRootAllocFrac;
				setCarbonFineRootsIncrement (fineRootsGrowth);	
				coarseRootAllocFrac = 1-fineRootAllocFrac;
				coarseRootsGrowth =  belowGroundGrowth * coarseRootAllocFrac;
				setCarbonCoarseRootsIncrement (coarseRootsGrowth);
			} else {
				fineRootsGrowth=fineRootSink;
				fineRootAllocFrac=fineRootsGrowth/belowGroundGrowth;				
				setCarbonFineRootsIncrement (fineRootsGrowth);
				coarseRootsGrowth = belowGroundGrowth-fineRootsGrowth;
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
		this.stemSink = 0;
		this.branchSink = 0;
		this.foliageSink = 0;
		this.stumpSink = 0;
		this.aboveGroundSink = 0;
		double stemGrowth    = 0;
		double stumpGrowth  = 0;
		double branchGrowth  = 0;
		double foliageGrowth = 0;

		if(carbonAllocated*aboveGroundAllocFrac>0){

			//Tree growth
			// Compute approximate tree dimensions following incremental growth
			// those would-be compartment sizes are used to determine the sink forces
			// of the various compartments
			double targetCarbonStem = getCarbonStem()+carbonAllocated*aboveGroundAllocFrac;
			double targetStemVolume =  targetCarbonStem/(woodCarbonContent*woodDensity);


			//1/ We assume the following relationship is always correct
			//   stemVolume=exp(aStemVolume) * dbh^bStemVolume * height^cStemVolume
			//2/ As long as the tree is shorter than expected only growth in height is allowed
			//3/ If tree is balanced then both DBH and Height need to be updated
			//4/ When foliage growth has ceased only dbh may increase
			//5/ Sink forces are further constrained through limiting radial crown expansion to a species specific (?)
			//   currently hardcoded value, set to 0.01 (m d-1) corresponding to the maximum daily increment;
			double targetHeight;
			double targetDbh;
			// gt 17.02.2009
			if(treeIsShort() && (this.phenologicalStage==1)){		// if tree is short, additional stem sink to grow in height
				targetHeight = Math.max(computeNewHeight (safeSettings, targetStemVolume, getDbhMeters()), aTree * Math.pow (getDbhMeters(),bTree));// gt - 26/10/2010
				targetDbh = Math.max(getDbhMeters(),computeNewDbh(targetStemVolume,targetHeight));// gt - 26/10/2010
				targetStemVolume = Math.exp(aStemVolume)*Math.pow(targetDbh,bStemVolume)*Math.pow(targetHeight,cStemVolume);
				targetCarbonStem = targetStemVolume*(woodCarbonContent*woodDensity); // gt - 26/10/2010
			} else {
				targetHeight = computeNewHeight (safeSettings, targetStemVolume, getDbhMeters());// gt - 26/10/2010
				targetDbh = computeNewDbh (targetStemVolume, getHeight());// gt - 26/10/2010
			}

			//We then need to compute associated crown volume to estimate branch and leaves sinks
			double targetCrownDepth  = targetHeight - getCrownBaseHeight ();
			double targetCrownVolume = 0;
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

			//crown radial expansion (as well as growth in height is stopped once leaf expansion is over
			if(this.phenologicalStage==1){
				targetCrownVolume=this.getCrownVolume();
			}
	
			//Absolute sink of branch assuming allocation to stem of available C is proportional
			// to relative stem size
			double targetBranchVolume =  targetCrownVolume * branchVolumeRatio;
			double targetCarbonBranches = (targetBranchVolume * woodDensity * woodCarbonContent);
			branchSink = Math.max (targetCarbonBranches - getCarbonBranches(),0);

			// Expected Leaf C given stem volume and height of crown base (kg)
			double targetCarbonFoliage = aLeafArea*Math.pow(targetCrownVolume,bLeafArea) * leafMassArea * leafCarbonContent /**(getCarbonBranches()/targetCarbonBranches)*/;	// gt - 23.09.2009 added constraint from branch biomass
			if (this.phenologicalStage==1){
				foliageSink = Math.max ((targetCarbonFoliage - getCarbonFoliage()),0);
				if(aboveGroundImbalance!=1)
					foliageSink/=(1-aboveGroundImbalance);
			}			
			stemSink = Math.max (targetCarbonStem - getCarbonStem(),0);
			stumpSink = Math.max (targetCarbonStem*stumpToStemBiomassRatio - getCarbonStump(),0);
			
			aboveGroundSink = branchSink + foliageSink + stemSink + stumpSink;
			//Actual allocation of C  based on allometric relations
			if (aboveGroundSink > 0)  {
				stemAllocFrac = stemSink / aboveGroundSink;
				stumpAllocFrac = stumpSink / aboveGroundSink;
				branchAllocFrac = branchSink / aboveGroundSink;
				foliageAllocFrac = foliageSink / aboveGroundSink;
			}
			stemGrowth = carbonAllocated* aboveGroundAllocFrac * stemAllocFrac;
			stumpGrowth = carbonAllocated* aboveGroundAllocFrac * stumpAllocFrac;
			branchGrowth = carbonAllocated* aboveGroundAllocFrac * branchAllocFrac;
			foliageGrowth =  carbonAllocated* aboveGroundAllocFrac *foliageAllocFrac;
		}
		
		
		
		setCarbonFoliageIncrement(foliageGrowth);  // kg C
		setCarbonStemIncrement(stemGrowth); 
		setCarbonBranchesIncrement(branchGrowth); 
		setCarbonStumpIncrement(stumpGrowth); 
		
		
		
		// UPDATE Structural C in aboveground compartments (kg C)
		setCarbonBranches (getCarbonBranches () + branchGrowth);
		setCarbonStem (getCarbonStem () + stemGrowth);
		setCarbonStump(getCarbonStump()+stumpGrowth);
		setCarbonFoliage (getCarbonFoliage () + (foliageGrowth));
		setAboveGroundCFraction ((getCarbonBranches() + getCarbonStem())
								/ (getCarbonBranches() + getCarbonStem() + getCarbonStump() + getCarbonCoarseRoots ()));


		/*
		* Nitrogen allocation
		* computation of N remobilisation from leaf and fine root senescence
		*/
		double optiNCStem 	 		= treeSpecies.getOptiNCStem ();
		double optiNCStump 	 		= treeSpecies.getOptiNCStump ();
		double optiNCFoliage 		= treeSpecies.getOptiNCFoliage ();
		double optiNCBranch 		= treeSpecies.getOptiNCBranch ();
		double optiNCCoarseRoot 	= treeSpecies.getOptiNCCoarseRoot ();
		double optiNCFineRoot 		= treeSpecies.getOptiNCFineRoot ();

		//Nitrogen allocation sub-routine; nitrogen is apportioned to the various
		//compartments based on compartment size and target N content*/

		//Absolute deficit of N in compartment occurring after growth of the various C //compartments
		double NbranchSink = 0;
		double NCoarseRsink = 0;
		double NfineRootSink = 0;
		double NFoliageSink = 0;
		double NstemSink = 0;
		double NstumpSink = 0;

		if(this.phenologicalStage==1 || this.phenologicalStage==2){
			NbranchSink = Math.max(optiNCBranch-getNitrogenBranches()/getCarbonBranches(),0)
						* getCarbonBranches();
			NCoarseRsink = Math.max(optiNCCoarseRoot-getNitrogenCoarseRoots()/getCarbonCoarseRoots(),0)
						* getCarbonCoarseRoots();
			NstemSink = Math.max(optiNCStem-getNitrogenStem()/getCarbonStem(),0)
						* getCarbonStem();
			NstumpSink = Math.max(optiNCStump-getNitrogenStump()/getCarbonStump(),0)
					* getCarbonStump();

			if (getCarbonFineRoots() > 0)
				NfineRootSink = Math.max(optiNCFineRoot-getNitrogenFineRoots()/getCarbonFineRoots(),0)
							 * getCarbonFineRoots();
			if (getCarbonFoliage() > 0)
				NFoliageSink = Math.max(optiNCFoliage-getNitrogenFoliage()/getCarbonFoliage(),0)
							 * getCarbonFoliage();

		}
		//Total N sink following growth of C compartments
		double totalNSink = NbranchSink+NCoarseRsink+NfineRootSink+NFoliageSink+NstemSink+NstumpSink;

		//relative sink of foliage and subsequent N flow
		double RelNFoliageSink = 0;
		double RelNbranchSink = 0;
		double RelNstemSink = 0;
		double RelNstumpSink = 0;
		double RelNFineRSink = 0;
		double RealNCoarseRSink = 0;

		if (totalNSink>0) {
			RelNFoliageSink = NFoliageSink	/ totalNSink;
			RelNbranchSink  = NbranchSink	/ totalNSink;
			RelNstemSink    = NstemSink		/ totalNSink;
			RelNstumpSink   = NstumpSink	/ totalNSink;
			RelNFineRSink   = NfineRootSink	/ totalNSink;
			RealNCoarseRSink= NCoarseRsink	/ totalNSink;
		}

		double totalNitrogenAllocated = Math.min(totalNSink,getNitrogenLabile()+nitrogenUptake);
		double NtoFoliage		= totalNitrogenAllocated * RelNFoliageSink;
		double NToBranch		= totalNitrogenAllocated * RelNbranchSink;
		double NToStem			= totalNitrogenAllocated * RelNstemSink;
		double NToStump			= totalNitrogenAllocated * RelNstumpSink;
		double NToFineRoots		= totalNitrogenAllocated * RelNFineRSink;
		double NToCoarseRoots	= totalNitrogenAllocated * RealNCoarseRSink;


		// UPDATE Structural N content of compartment (kg)

		setNitrogenLabile 		(getNitrogenLabile()
									+nitrogenUptake-totalNitrogenAllocated);
		setNitrogenBranches  	(getNitrogenBranches ()		+ NToBranch);
		setNitrogenStem 		(getNitrogenStem ()			+ NToStem);
		setNitrogenStump 		(getNitrogenStump ()		+ NToStump);
		setNitrogenFoliage 		(getNitrogenFoliage ()		+ NtoFoliage);
		setNitrogenFineRoots 	(getNitrogenFineRoots ()	+ NToFineRoots);
		setNitrogenCoarseRoots 	(getNitrogenCoarseRoots ()	+ NToCoarseRoots);
	}
	
	/**
	 * Allometric growth (dbh, height, crowndiameter)
	 */
	public void computeAllometricGrowth (SafeStand stand, SafeInitialParameters safeSettings) {

		//Tree specific parameters
		int    crownShape 	 	 = this.getTreeSpecies ().getCrownShape ();
		double aCrown 	 		 = this.getTreeSpecies ().getCrownDbhAllometricCoeffA ();
		double bCrown	 		 = this.getTreeSpecies ().getCrownDbhAllometricCoeffB ();
		double bTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		double woodDensity 		 = this.getTreeSpecies ().getWoodDensity ();
		double woodCarbonContent = this.getTreeSpecies ().getWoodCarbonContent ();
		double leafMassArea  	 = this.getTreeSpecies ().getLeafMassArea ();
		double leafCarbonContent = this.getTreeSpecies ().getLeafCarbonContent();
		double dcbFromDbh		 = this.getTreeSpecies ().getDcbFromDbhAllometricCoeff ();
		
		//UPDATE tree dimensions
		if (getCarbonIncrement() > 0 ) {
			
			setStemVolume (getCarbonStem () / (woodCarbonContent * woodDensity));
		
			double newHeight = computeNewHeight (safeSettings, getStemVolume(), getDbhMeters());// gt - 26/10/2010
			double newDbh    = computeNewDbh (getStemVolume(), getHeight());// gt - 26/10/2010
	
			setHeight (Math.max(getHeight(), newHeight));
			setDbhMeters (Math.max(getDbhMeters(), newDbh));
			setDbh (getDbhMeters() * 100); //cm
		}

		//UPDATE leaf area if growing season
		if (!(this.phenologicalStage==0 || this.phenologicalStage==4)) {
			double truncationRatio = this.getTreeSpecies().getEllipsoidTruncationRatio();
			double branchVolumeRatio = this.getTreeSpecies().getBranchVolumeRatio();
			double branchImbalance = getCarbonBranches()/(getCrownVolume()* branchVolumeRatio * woodDensity * woodCarbonContent);
			double maxCrownRadiusInc =  this.getTreeSpecies().getMaxCrownRadiusInc();
			if ((this.phenologicalStage==1) /*& (branchImbalance>=0.95)*/) {		// gt - 11.12.2009 - if branch biomass does'nt fill crown volume : crown radius are not allowed to grow

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

			setLeafArea ((getCarbonFoliage()/leafCarbonContent)*(1/leafMassArea));
			
		}
		else setLeafArea(0);

	}

	/**
	 * return Boolean, tracking stem "compression"
	 */
	public boolean treeIsShort () {
		double aTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		if ((aTree * Math.pow (getDbhMeters(),bTree)) > (getHeight()*1.001))
			return (true);
		else return (false);
	}
	/**
	* return tree height
	*/
	public double computeNewHeight (SafeInitialParameters safeSettings, double newStemVolume, double newDbhMeters) {

		double aTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffA ();
		double bTree 			 = this.getTreeSpecies ().getHeightDbhAllometricCoeffB ();
		double aStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffA ();
		double bStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffB ();
		double cStemVolume 		 = this.getTreeSpecies ().getStemDbhAllometricCoeffC ();
		double maxHeightInc 	=  this.getTreeSpecies ().getMaxHeightInc();
		if (this.phenologicalStage==1) {
			//tree height growth and crown expansion is possible
		   if (this.treeIsShort())
				// new potential height is estimated assuming no additional dbh increment
				return (Math.min((this.getHeight()+maxHeightInc), Math.pow (
						   newStemVolume * Math.exp(-aStemVolume) * Math.pow(newDbhMeters,-bStemVolume),
						   (1/cStemVolume))));
			else
				// new potential height is estimated assuming allometric height and dbh relation is maintained
				return (Math.min((this.getHeight()+maxHeightInc), Math.pow (
							newStemVolume * Math.exp(-aStemVolume) * Math.pow(aTree,(bStemVolume/bTree)) * Math.pow (1,-bStemVolume),	
							(1/((bStemVolume/bTree) +cStemVolume)))));
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

		if (this.phenologicalStage==1) {
		   if (this.treeIsShort())
				// dbh remains unchanged
				return (dbhMeters);
			else
			//dbh growth
				return (Math.pow
						(newStemVolume *  Math.exp(-aStemVolume) * Math.pow(aTree,-cStemVolume) * Math.pow(1,-bStemVolume),
						(1/(bStemVolume + bTree*cStemVolume))));
		}
		else
		//height growth has ceased only dbh is updated
			return (Math.pow
						(newStemVolume * Math.exp(-aStemVolume) * Math.pow(1,-bStemVolume) * Math.pow(newHeight,-cStemVolume),
						(1/bStemVolume)));
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
	private void fineRootGrowth (SafeInitialParameters safeSettings,	SafePlotSettings plotSettings,
								int simulationDay,
								double carbonFineRootsIncrement, boolean reallyOrNot) {	// GT 17/07/2008

		double  specificRootLength 	= this.getTreeSpecies().getSpecificRootLength ();

		//if this tree  has roots !!!!
		if (getFineRoots().getFirstRootNode() != null) {

			int treeIndex = this.getId()-1;			//index of this tree

			//compute nbr voxel in this plot
			SafeCell treeCell = (SafeCell) this.getCell();
			int nbVoxelTot = 0;
			SafePlot plot = (SafePlot) treeCell.getPlot();
			nbVoxelTot = treeCell.getVoxels().length * plot.getCells ().size();

			//temporary storing tables
			double [] additionalRootLenght    = new double [nbVoxelTot];	//m

			//convert carbon entry (kg C) in root lenght (m)
			double carbonToDryMatter = 1d / this.getTreeSpecies ().getWoodCarbonContent();
			double frCost = 1/(carbonToDryMatter*1000*specificRootLength); // 1/(Kg.Kg-1*g.Kg-1*m.g-1)=Kg.m-1
			double additionalRoot = carbonFineRootsIncrement/frCost;	//Kg/Kg.m-1 = m
			
			// la methode computeFineRootCost est une methode recursive, qui va calculer le cout en carbone des nouvelles racines fines 
			//(prenant en compte les besoins en racines de structure) pour l'ensemble des voxels colonises par l'arbre.
			double rootedVolume = getFineRoots().getFirstRootNode().computeRootedVolume();

			getFineRoots().getFirstRootNode().setFineRootCost (0);
			getFineRoots().getFirstRootNode().computeFineRootCost(treeIndex,
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

			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getFineRoots().getRootTopology (voxel);
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
						fineRootsCostMax = Math.max(fineRootsCostMax, node.getFineRootCost());
						voxel.setWaterEfficiency(node.getWaterEfficiency());
						voxel.setNitrogenEfficiency(node.getNitrogenEfficiency());
						voxel.setFineRootCost(node.getFineRootCost());


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
			if(nitrogenMaxEfficency > 0){
				 coefNitrogen[1] = ((1+localNitrogenUptakeFactor)-coefNitrogen[0])/nitrogenMaxEfficency; 
			}
			
			double[] coefCost = new double[2];
			coefCost[0] = 1+sinkDistanceEffect;
			coefCost[1] = 0;
			if(fineRootsCostMax > 0) {
				coefCost[1] = -sinkDistanceEffect/fineRootsCostMax; 
			}		 
			 
			double totalMark = 0; 
			
			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getFineRoots().getRootTopology (voxel);

				
				if (node != null) {
					
					//test export
					voxel.setWaterEfficiencyMax(waterMaxEfficency);
					voxel.setNitrogenEfficiencyMax(nitrogenMaxEfficency);
					voxel.setFineRootCostMax(fineRootsCostMax);
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
						double costMark = coefCost[0] + (coefCost[1]*node.getFineRootCost());
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
			// la note waterMark pour un voxel est proportionnelle � l'efficience d'extraction d'eau dans ce voxel (waterUptake/fineRootLength)
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
					double voxelRootDensity =  voxels[i].getTheTreeRootDensity (treeIndex);	//m m-3
					if (voxelRootDensity > 0) {
						
						int voxelId = voxels[i].getId()-1;
						double proportion = 0;
						SafeRootNode node = getFineRoots().getRootTopology (voxels[i]);

						boolean fineRootGrowthInThisVoxel = (getFineRoots().getRootTopology (voxels[i]).getNodeParent()==null);
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
						
						additionalRootLenght[voxelId] += additionalRoot * proportion ; 	//m;

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

						voxels[i].setAdditionalRootToVoxel(additionalRootToVoxel);
						
						SafeVoxel upperVoxel=voxels[i];
						if(i!=0){
							upperVoxel=voxels[i-1];
						}
						if ((additionalRootToVoxel > 0) && (!upperVoxel.getIsSaturated())) {
							int direction = getFineRoots().getRootTopology (voxels[i]).getColonisationDirection();
							
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
							// Si lambda = 0.5, la forme du voxel est conserv�e
							// Si lambda <0.5, le voxel est aplati (son epaisseur z est diminuee, ses largeurs x et y sont augmentees)
							// Si lambda >0.5, c'est le contraire
							// le volume total du voxel est conserve, c'est le pourquoi des 1/sqrt(2*lambda)
							
							L[0]=1/Math.sqrt(2*lambda)*L[0];
							L[1]=1/Math.sqrt(2*lambda)*L[1];
							L[2]=2*lambda*L[2];
							double Lmin=Math.min(Math.min(L[0],L[1]),L[2]);
							double Lmax=Math.max(Math.max(L[0],L[1]),L[2]);

							double voxelFilling =  (getFineRoots().getRootTopology(voxels[i]).getFineRootTotalInvestment()
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

									
									if (getFineRoots().getRootTopology().containsKey(neighbours[n])){ // if a neighbour voxel contains fine roots, no colonisation
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
										getFineRoots().addTreeRootTopology (this, neighbours[n], voxels[i], simulationDay, colonisationRootLength/neighbours[n].getVolume(), n);
										neighbours[n].setTreeRootDensity(treeIndex,colonisationRootLength/neighbours[n].getVolume()); 
										neighbours[n].setColonisationDirection(treeIndex, n);	
									}
								}
								additionalRootToVoxel=additionalRootToVoxel* (1-colonisationFraction);
							}

					
						
							double voxelRootDensity = voxels[i].getTheTreeRootDensity (treeIndex) + (additionalRootToVoxel/voxels[i].getVolume());					
						
	
	
							if(voxelRootDensity > 0){
								voxels[i].setTreeRootDensity (treeIndex, voxelRootDensity);
								
								getFineRoots().getRootTopology(voxels[i]).setFineRootDensity(voxelRootDensity);
								getFineRoots().getRootTopology(voxels[i]).addFineRootTotalInvestment(additionalRootToVoxel/voxels[i].getVolumeFineSoil());
								//a enlever c'est pour tester le module
								voxels[i].setFineRootTotalInvestment(getFineRoots().getRootTopology(voxels[i]).getFineRootTotalInvestment());
							}
						
						} //end if additionalRootToVoxel > 0
						
						
					}//end for each voxel 
				}//end for each cell
			} // end of if(reallyOrNot)
			// GT 17/07/2008

			this.setCoarseRootTotalTargetBiomass(0);
			getFineRoots().getFirstRootNode().computeCoarseRootTargetBiomass(this, additionalRootLenght,
										this.getTreeSpecies().getWoodDensity(),
										this.getTreeSpecies().getWoodCarbonContent(),
										this.getTreeSpecies().getCRAreaToFRLengthRatio());

		} // end of if tree has roots

		return;
	}

	


	/**
	 * Coarse roots growth
	 * @author Rachmat MULIA (ICRAF) - August 2004
	 */
	private void coarseRootGrowth (double carbonCoarseRootsIncrement) {
	// Carbon is allocated to coarse roots in each voxel proportionnally
	// to water flux through this voxel the day before
	// Like Pipe model Shinozaki et al., 1964
	//

		//if this tree  has fine roots !!!!
		if (getFineRoots().getRootTopology() != null) {

			//index of this tree
			int treeIndex = this.getId()-1;

			//FOR EACH VOXEL
			SafeRootNode firstNode = getFineRoots().getFirstRootNode () ;
			double oldCoarseRootBiomass = getCarbonCoarseRoots();
			double coarseRootTotalBiomassImbalance = getCoarseRootTotalTargetBiomass()-oldCoarseRootBiomass;
			firstNode.computeCoarseRootBiomass (treeIndex, carbonCoarseRootsIncrement,
													coarseRootTotalBiomassImbalance,
													oldCoarseRootBiomass);
		}
	}

/**
 * relative influence of dry soil layers on the perceived potential
 * with the default value of 1 indicating a harmonic weighted mean
 *  */

	public void computeTotalRoot (SafeInitialParameters settings){

		if (getFineRoots().getFirstRootNode() == null) return;  
	
		double fineRootLength = 0; 
		
		Set keys = getFineRoots().getRootTopology().keySet();
		for(Iterator it = keys.iterator(); it.hasNext ();){
			SafeRootNode node = (SafeRootNode) getFineRoots().getRootTopology().get(it.next());
			fineRootLength += node.getFineRootDensity()*node.getVoxelRooted().getVolume();
		}
	
		this.getFineRoots().setTotalRootLength(fineRootLength);
		
		if (fineRootLength <= 0) return;
		
		double drySoilFactor = settings.harmonicWeightedMean;

		double plantPotential = 0;
		
		for(Iterator it = keys.iterator(); it.hasNext ();){
			
			SafeRootNode node = (SafeRootNode) getFineRoots().getRootTopology().get(it.next());
			SafeVoxel v = node.getVoxelRooted();
			double neededPot = v.getWaterPotentialTheta();	// soil water potential in this voxel

			// additional potential for water flow from bulk soil to rhizosphere
			neededPot *= (1+this.getTreeSpecies().getTreeBufferPotential()); 
			// additional potential for water flow from root surface to xylem
			double radialTransportPotential = -this.getWaterDemand()								// L.day-1=dm3.day-1
												* 1000														// from L.day-1 to cm3.day-1
												/this.getTreeSpecies().getTreeRootConductivity()			// cm day-1
												/(this.getTotalRootLength()*100);							// m to cm
							
			
			this.getFineRoots().setRadialTransportPotential(radialTransportPotential);
			

			neededPot += radialTransportPotential;

		
			// additional potential to account for longitudinal water transport in coarse roots from the voxel to stem base
			double longitudinalTransportPotential = -this.getWaterDemand()										//L.day-1=dm3.day-1
														* 1000															// from L.day-1 to cm3.day-1
														* this.getTreeSpecies().getTreeLongitudinalResistantFactor()	// day.cm-1.m-1
														/(this.getTotalRootLength()*100);								// m to cm
			// in the model documentation from Meine et al, this term is not divided by totalRootLength... but it leads to very different longitudinal drop potential for small and large trees because of differences in water demand... so...
			this.getFineRoots().setLongitudinalTransportPotential(longitudinalTransportPotential);
			neededPot += longitudinalTransportPotential*node.getTreeDistance();	// topological distance (m) between the voxel and stem base

			
			// additional potential to account for voxel depth
			neededPot -= v.getZ()*100;		// from m to cm

	
			plantPotential += (-node.getFineRootDensity() * v.getVolume()	// m.m-3 * m3 = m
									/ Math.pow(-neededPot, drySoilFactor));	// cm

		}
		
		plantPotential =-Math.pow (-this.getTotalRootLength() /plantPotential , 1/drySoilFactor);

		this.getFineRoots().setRequiredWaterPotential(plantPotential);

	
	}


	/**
	 * Foliage Senescence Computation - updated by gt 09.10.2009 : sigmoidal instead of exponential decrease
	 * fallenLeafProp(t)= 1/(1+exp(-b*(t-a)))
	 * a is the date for which half of leaves are fallen, it can be computed as a=LeafFallStartingDate+leafFallDuration/2
	 * b is a time constant. If we consider that leaf fall begin when 1% of leaves are fallen, it can be computed as b=2*log(99)/leafFallDuration
	 */
	public void computeCarbonFoliageSenescence (int julianDay) {
		

		double leafSenescenceRate = this.getTreeSpecies ().getLeafSenescenceRate();
		double leafNRemobFrac = this.getTreeSpecies().getLeafNRemobFraction();//GT-4.06.2010
		int leafFallDuration = this.getTreeSpecies ().getLeafFallDuration();
		
		double carbonLeafSen = 0;
		double nitrogenLeafSen = 0;
		double fastSenescenceRate = 0;

		// gt - 14.01.2009 - the last day of leaf fall, all remaining leaves die
		if (this.phenologicalStage==4) {
			setCarbonFoliageSen(this.carbonFoliage);
			setNitrogenFoliageSen(this.nitrogenFoliage);
			setCarbonFoliage(0);
			setNitrogenFoliage(0);
			
		} else {

			double a = getLeafFallStartingDate()+leafFallDuration/2;
			double b = 2*Math.log(99)/leafFallDuration;
			
			// computed for respecting the sigmoidal decrease with parameters a and b
			fastSenescenceRate = 1-(1+Math.exp(-b*(julianDay-a)))/(Math.exp(b)+Math.exp(-b*(julianDay-a)));						
			carbonLeafSen = this.carbonFoliage * (leafSenescenceRate + fastSenescenceRate);
			setCarbonFoliageSen(carbonLeafSen); //ajout Sitraka, 07/10/2000
			setCarbonFoliage(this.carbonFoliage-this.carbonFoliageSen);	//GT-4.06.2010
			
			// calcul remob azote
			setNitrogenFoliageSen(carbonLeafSen*(1-leafNRemobFrac)*this.nitrogenFoliage/this.carbonFoliage);	//GT-4.06.2010
			addNitrogenLabile(carbonLeafSen*leafNRemobFrac*this.nitrogenFoliage/this.carbonFoliage);//GT-4.06.2010
			setNitrogenFoliage(this.nitrogenFoliage-this.nitrogenFoliageSen/(1-leafNRemobFrac));	//GT-4.06.2010
		}
	}
	
	

	/**
	 * Nitrogen demand calculation
	 */
	public double computeNitrogenStress () {
		//  Update N stress to be used for next daily time step
		//Stress occurs if demand is not satisfied..
		//  no limit to daily nitrogenLabile use
		
		if (getCarbonFoliage () > 0)
			return   (Math.max (Math.min ((getTotalN() / getTotalOptiN()), 1), 0));
		else {
			return (1);

		}
	}
	
	/*==============================
	/*INTERVENTIONS
	/*==============================
	/**
	 * Pruning intervention effect calculation
	 * @author Gr�goire VINCENT (IRD) - May 2004
	 */
	/*public void pruning (double newCrownBaseheight) throws Exception {
	// This is code for simulation of pruning (all branches are removed under the newCrownBaseheight)
	// Ellipsoide shape is recalculated with this newCrownBaseHeight
	//
		double reductionFraction = 1 - ((getHeight() - newCrownBaseheight)  / (getHeight() - getCrownBaseHeight()));

		if (reductionFraction < 1 && reductionFraction >=0 ) {
		//si >1 : la nouvelle (ou excl. l'ancienne) CrownBaseHeight est superieur � la hauteur
		//si <=0: la nouvelle CrownBaseHeight est inferieure � l'ancienne (�lagage n�gatif)
			setCrownBaseHeight (newCrownBaseheight);
			setCrownRadiusTreeLine ( getCrownRadiusTreeLine() * Math.sin (Math.acos (reductionFraction)));
			setCrownRadiusInterRow ( getCrownRadiusInterRow() * Math.sin (Math.acos (reductionFraction)));

			//if truncated ellipsoid
			setCrownRadiusVertical (((height-newCrownBaseheight)/(1-this.getTruncatureRatio()))/2);

			double newVolume = Math.PI  * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical() *
			 				  (2 * (1-this.getTruncatureRatio()) - (1d/3d * ( 1 - Math.pow (2*this.getTruncatureRatio()-1,3))));


			double volumeReductionFraction = newVolume / getCrownVolume();

			setCarbonFoliage  (getCarbonFoliage () * volumeReductionFraction);
			setCarbonBranches  (getCarbonBranches () * volumeReductionFraction);
			setNitrogenFoliage  (getNitrogenFoliage () * volumeReductionFraction);
			setNitrogenBranches  (getNitrogenBranches () * volumeReductionFraction);
			setCrownVolume (newVolume);
		}

		else {
			Log.println("PRUNING WITH NO EFFECT FOR TREE Nr"+this.getId()+" !!!!");
			throw(new Exception("PRUNING WITH NO EFFECT FOR TREE Nr"+getId()+"\nThe new crown base height is invalid ("+newCrownBaseheight+")\n MUST BE "+getCrownBaseHeight()+"<= CrownBaseHeight <"+getHeight()));
		}

	}*/
	/**
	 * Pruning intervention effect calculation
	 * @author Gr�goire TALBOT
	 */	
	/*public void pruning (double proportion, double maxheight) {

		double newPruningHeight = Math.min(this.getHeight()*proportion, maxheight);
		
		if (newPruningHeight > this.getCrownBaseHeight()){
			
			double redFrac = (this.getHeight()-newPruningHeight)/(this.getHeight()-this.getCrownBaseHeight());
			setCrownRadiusTreeLine(getCrownRadiusTreeLine()*redFrac);
			setCrownRadiusInterRow(getCrownRadiusInterRow()*redFrac);
			setCrownRadiusVertical(getCrownRadiusVertical()*redFrac);
			double newVolume = 4/3*Math.PI*getCrownRadiusTreeLine()*getCrownRadiusInterRow()*getCrownRadiusVertical();
			double volumeReductionFraction = newVolume / getCrownVolume();
			
			if (volumeReductionFraction != 0) {
				//carbon and nitrogen exported from the plot (branches and leaves are removed) 
				addCarbonFoliageExported (getCarbonFoliage () * (1/volumeReductionFraction));
				addCarbonBranchesExported (getCarbonBranches () * (1/volumeReductionFraction));
				addNitrogenFoliageExported (getNitrogenFoliage () * (1/volumeReductionFraction));
				addNitrogenBranchesExported (getNitrogenBranches () * (1/volumeReductionFraction));
				
				//reduce carbon and nitrogen pool
				setCarbonFoliage  (getCarbonFoliage () * volumeReductionFraction);
				setCarbonBranches  (getCarbonBranches () * volumeReductionFraction);
				setNitrogenFoliage  (getNitrogenFoliage () * volumeReductionFraction);
				setNitrogenBranches  (getNitrogenBranches () * volumeReductionFraction);

				setLeafArea(getLeafArea() * volumeReductionFraction);
				setCrownVolume(newVolume);
				setCrownBaseHeight(newPruningHeight);
			}
		}
	}*/
	/**
	 * Pruning intervention effect calculation
	 * @author Kevin WOLZ - 16-08-2018
	 */	
	public void pruning (double proportion, double maxheight, double densityReductionFraction) {

        double newPruningHeight = Math.min(this.getHeight() * proportion, maxheight);

        if (newPruningHeight > this.getCrownBaseHeight()) {

            //modify crown radii
            double redFrac = (this.getHeight() - newPruningHeight) / (this.getHeight() - this.getCrownBaseHeight());
            setCrownRadiusTreeLine(getCrownRadiusTreeLine() * redFrac);
            setCrownRadiusInterRow(getCrownRadiusInterRow() * redFrac);
            setCrownRadiusVertical(getCrownRadiusVertical() * redFrac);

            //reduce crown volume & increase base height
            double newVolume = 4 / 3 * Math.PI * getCrownRadiusTreeLine() * getCrownRadiusInterRow() * getCrownRadiusVertical();
            double volumeReductionFraction = newVolume / getCrownVolume();
            setCrownVolume(newVolume);
            setCrownBaseHeight(newPruningHeight);

            //carbon and nitrogen exported from the plot (branches and leaves are removed)
            addCarbonFoliageExported    (getCarbonFoliage ()    * (1 - volumeReductionFraction));
            addCarbonBranchesExported   (getCarbonBranches ()   * (1 - volumeReductionFraction));
            addNitrogenFoliageExported  (getNitrogenFoliage ()  * (1 - volumeReductionFraction));
            addNitrogenBranchesExported (getNitrogenBranches () * (1 - volumeReductionFraction));
            
            //reduce carbon and nitrogen pool
            setCarbonFoliage    (getCarbonFoliage ()    * volumeReductionFraction);
            setCarbonBranches   (getCarbonBranches ()   * volumeReductionFraction);
            setNitrogenFoliage  (getNitrogenFoliage ()  * volumeReductionFraction);
            setNitrogenBranches (getNitrogenBranches () * volumeReductionFraction);

            //reduce leaf area
            setLeafArea(getLeafArea() * volumeReductionFraction);

        }

        if (densityReductionFraction > 0) {

            //carbon and nitrogen exported from the plot (branches and leaves are removed)
            addCarbonFoliageExported    (getCarbonFoliage ()    * densityReductionFraction);
            addCarbonBranchesExported   (getCarbonBranches ()   * densityReductionFraction);
            addNitrogenFoliageExported  (getNitrogenFoliage ()  * densityReductionFraction);
            addNitrogenBranchesExported (getNitrogenBranches () * densityReductionFraction);
            
            //reduce carbon and nitrogen pool
            setCarbonFoliage    (getCarbonFoliage ()    * (1 - densityReductionFraction));
            setCarbonBranches   (getCarbonBranches ()   * (1 - densityReductionFraction));
            setNitrogenFoliage  (getNitrogenFoliage ()  * (1 - densityReductionFraction));
            setNitrogenBranches (getNitrogenBranches () * (1 - densityReductionFraction));

            //reduce leaf area
            setLeafArea(getLeafArea() * (1 - densityReductionFraction));

        }
}
	/**
	 * Pollarding intervention
	 * @author Isabelle LECOMTE (02-10-2017)
	 **/
	public void pollarding (SafeStand stand, double newHeight, double canopyLeft, int julianDay) {
		
		if (newHeight < this.getHeight()) {
			
			double newCrownBaseHeight = newHeight - canopyLeft;
			
			if (newCrownBaseHeight > 0) {
			
				//diameter at soil level
				double db = this.getDbhMeters() * this.getHeight() / (this.getHeight()-(1.3));
				//diameter at pollarding height
				double dph = this.getDbhMeters() * (this.getHeight() - newHeight) / (this.getHeight()-(1.3));

				double r1 = db/2d;
				double r2 = dph/2d;

				//stem volume ratio
				double stemVolumeBefore = (Math.PI / 3d) * this.getHeight() * (r1*r1) ;
				double stemVolumeAfter  = (Math.PI / 3d) * newHeight * ((r1*r1)+(r1*r2)+(r2*r2)) ;
				double stemVolumeRatio = stemVolumeAfter / stemVolumeBefore;

				
				//C pool reduction
				double totalCarbonBefore = this.getCarbonBranches()+this.getCarbonStem()+this.getCarbonStump()+this.getCarbonCoarseRoots()+this.getCarbonFineRoots();
				
				//carbon exported from the plot (branches, leaves and stem are removed) 
				addCarbonFoliageExported (getCarbonFoliage ());
				addCarbonBranchesExported (getCarbonBranches ());
				addCarbonStemExported (this.getCarbonStem() * (1/stemVolumeRatio));
				
				//reduce carbon pools
				setCarbonStem (this.getCarbonStem() * stemVolumeRatio);
				setCarbonFoliage(0);
				setCarbonBranches(0);
				setLeafArea(0); 
				
				
				double totalCarbonAfter = this.getCarbonStem()+this.getCarbonStump()+this.getCarbonCoarseRoots()+this.getCarbonFineRoots();
				double totalCarbonRatio = totalCarbonAfter / totalCarbonBefore; 
				setCarbonLabile(this.carbonLabile * totalCarbonRatio);

				
				//Nitrogen
				double optiNCStem 	 	= this.getTreeSpecies ().getOptiNCStem ();
				
				//carbon exported from the plot (branches, leaves and stem are removed) 
				addNitrogenFoliageExported (getNitrogenFoliage ());
				addNitrogenBranchesExported (getNitrogenBranches ());
				addNitrogenStemExported (this.getNitrogenStem() * (1/optiNCStem));
				
				//reduce nitrogen pools
				setNitrogenStem (getCarbonStem() * optiNCStem);
				setNitrogenFoliage (0);
				setNitrogenBranches	(0) ;

				//size update
				//UPDATE tree dimensions
				//General parameters
				setStemVolume (this.getStemVolume() * stemVolumeRatio);

				double newCrowRadius = 0.25; 
	
				
				//Ellipsoid crown shape (after G.Vincent 2003)
				int crownShape 	= this.getTreeSpecies ().getCrownShape ();
				double aCrown	= this.getTreeSpecies ().getCrownDbhAllometricCoeffA ();
				double bCrown 	= this.getTreeSpecies ().getCrownDbhAllometricCoeffB ();
				
				
				double truncationRatio = this.getTreeSpecies ().getEllipsoidTruncationRatio ();
				
				if (crownShape == 1) {
					//if truncated ellipsoid
					if (truncationRatio == 1) {		//100%
						setCrownRadiusVertical (0);				//METTRE ICI UN MESSAGE ERREUR !!!!!
					}
					else
					{
						setCrownRadiusVertical (((newHeight-newCrownBaseHeight)/(1-truncationRatio))/2);
					}
					
					double crownArea = Math.PI*Math.pow(newCrowRadius,2);		
					//crown radius calcultation limited by tree line and inter-row distance
					double newRadius [] = new double [2];
					newRadius = computeRadiiDeformation (crownArea , (SafeStand) stand);
					setCrownRadiusTreeLine   (Math.max(this.getCrownRadiusTreeLine(),newRadius[0]));
					setCrownRadiusInterRow   (Math.max(this.getCrownRadiusInterRow(),newRadius[1]));
	
					//Elipsoide crown volume with trucature ratio
					setCrownVolume (Math.PI  * this.getCrownRadiusTreeLine() * this.getCrownRadiusInterRow() * this.getCrownRadiusVertical() *
					 				(2 * (1-truncationRatio) - (1d/3d * (1 - Math.pow (2 * truncationRatio-1,3)))));
				}
	
				//Paraboloid of revolution crown shape (after B.Courbeaud 2000)
				if (crownShape == 2) {
					setCrownParameter ((aCrown * Math.pow ((this.getDbhMeters () / newHeight), bCrown)));
					setCrownRadiusTreeLine (Math.sqrt (this.getCrownParameter() * (newHeight - newCrownBaseHeight)));
					setCrownRadiusInterRow (Math.sqrt (this.getCrownParameter() * (newHeight - newCrownBaseHeight)));
					setCrownRadiusVertical ((newHeight-newCrownBaseHeight)/2);
					setCrownVolume (1d/2d * Math.PI * this.getCrownParameter ()
									* Math.pow (newHeight - newCrownBaseHeight,2));
				}
				

				this.setHeight (newHeight);
				this.setCrownBaseHeight (newCrownBaseHeight);
				this.setPollarding (true);
				this.setPollarding2 (true);

				//phenology delay after pollarding 
				if (this.treeSpecies.getPhenologyType() == 1) {
					if (julianDay > getLeafFallEndingDate()) {
						double nbrDay = 365d - getLeafFallEndingDate() + getBudburstDate();
						double constante = (this.treeSpecies.getBudBurstDelayMaxAfterPollaring()-this.treeSpecies.getBudBurstDelayMinAfterPollaring()) / nbrDay ;
						//ATTENTION le jour de pollaring doit etre compris entre la date de fin de chutte des feuilles et 365
						//sinon il doit etre inf�rieur � la date de debourrement de l'ann�e suivante (+365) 
						julianDay = julianDay - getLeafFallEndingDate();
						double val = constante * julianDay;
						int calcul = this.treeSpecies.getBudBurstDelayMinAfterPollaring() + (int) val;
						this.setBudburstDelay(calcul);	
					}
					else {
						this.setBudburstDelay(this.treeSpecies.getBudBurstDelayMaxAfterPollaring());
					}
				}
				else {
					this.setBudburstDelay(0);
				}
			}
		}
	}

	/**
	 * Root pruning intervention
	 **/
	public void rootPruning (SafeStand stand, SafeInitialParameters ip, double rootPruningDistance, double rootPruningDepth) {


		double cellHWidth = stand.getPlot().getCellWidth() / 2.0;
		double xtest1min = this.getX()-rootPruningDistance-cellHWidth;
		double xtest1max = this.getX()-rootPruningDistance+cellHWidth;
		double xtest2min = this.getX()+rootPruningDistance-cellHWidth;
		double xtest2max = this.getX()+rootPruningDistance+cellHWidth;
		  

		//searching cells at the right root pruning distance
		for(Iterator it = stand.getPlot().getCells().iterator();it.hasNext();) { 
			  SafeCell c = (SafeCell) it.next(); 
			  
			  //no root pruning in the cell where the tree is planted
			  if (this.getCell().getId() != c.getId()) {
				  
				  double cx = c.getX()+cellHWidth;
				  
				  //check if distance is respected
				  if ( ( (cx >= xtest1min) && (cx < xtest1max) ) 
					|| ( (cx > xtest2min) && (cx <= xtest2max)) ) {

					  SafeVoxel voxels [] =  c.getVoxels(); 

						for (int i=0; i < voxels.length; i++) {
							  if (voxels[i].getSurfaceDepth() < rootPruningDepth) {

								  if (getFineRoots().getRootTopology(voxels[i]) != null) {
									  if(getFineRoots().getRootTopology(voxels[i]).getNodeParent() != null) 		{ 
										  double removedProp = Math.min(Math.max((rootPruningDepth-(voxels[i].getZ()-(voxels[i].getThickness()/2)))/voxels[i].getThickness() , 0),1);
										  boolean testAnoxia = false;
										  getFineRoots().getRootTopology(voxels[i]).getNodeParent().removeSonsRoots(removedProp, voxels[i], this, ip, testAnoxia); 
									
									  }
								  }
							  }
						}
				  } 
			  }
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
		l_captureFactorForDiffusePar +=  e;
	}
	public void addDirect (double e) {
		l_captureFactorForDirectPar +=  e;
	}
	public void addInfraRed (double e) {
		l_captureFactorForInfraRed +=  e;
	}
	public void addDirectNir (double e) {
		l_captureFactorForDirectNir +=  e;
	}
	public void addDiffuseNir (double e) {
		l_captureFactorForDiffuseNir +=  e;
	}
	
	
	public void addDirectToLonelyTree (double e) {
		l_cFdirectLonelyTree += (float) e;
	}
	public void addDiffuseToLonelyTree (double e) {
		l_cFdiffuseLonelyTree += (float) e;
	}
	/**
	 * Daily calculation of PAR intercepted by a tree depending climatic entries
	 */
	public void updateDailyLightResults (SafeDailyClimat dayClimat ,
										SafeEvolutionParameters simulationSettings,
										SafeInitialParameters settings) {	//GT 2007

		float dailyDirect = dayClimat.getDirectPar ();				//Moles m-2 d-1
		float dailyDiffuse = dayClimat.getDiffusePar ();			//Moles m-2 d-1

		//Diffuse PAR intercepted by the tree
		setDiffuseParIntercepted (getCaptureFactorForDiffusePar() * dailyDiffuse);	//m2 * Moles m-2 d-1= Moles d-1

		//Direct PAR intercepted by the tree
		setDirectParIntercepted (getCaptureFactorForDirectPar() * dailyDirect);	//m2 * Moles m-2 d-1= Moles d-1

	
		//Global radiation intercepted by the tree
		float parProp =(float) settings.parGlobalCoefficient;
		float directProp = dailyDirect/(dailyDirect+dailyDiffuse);
		float dailyGlobal = dayClimat.getGlobalRadiation();
		setGlobalRadIntercepted(dailyGlobal*(
							(l_captureFactorForDiffuseNir*(1-parProp)+
							l_captureFactorForDiffusePar*parProp) * (1-directProp)+
							(l_captureFactorForDirectNir*(1-parProp)+
							l_captureFactorForDirectPar*parProp) * (directProp)
							));

		//Computation of the competition index
		if((l_directParIntercepted+l_diffuseParIntercepted)>0){		// if there is Par interception
			setLightCompetitionIndex((l_directParIntercepted+l_diffuseParIntercepted)
					/(dailyDirect*l_cFdirectLonelyTree + dailyDiffuse*l_cFdiffuseLonelyTree));
		}

		//InfraRed Radiation intercepted
		float dailyInfraRed = dayClimat.getInfraRedRadiation();
		setInfraRedIntercepted(l_captureFactorForInfraRed*dailyInfraRed);

	}

	//ACCESSOR FOR LIGHT MODULE
	public double getCaptureFactorForDiffusePar () {
		return  round(l_captureFactorForDiffusePar);
	}
	public double getCaptureFactorForDirectPar () {
		return  round(l_captureFactorForDirectPar);	
		}
	public double getCaptureFactorForDirectNir () {
		return  round(l_captureFactorForDirectNir);	
	}
	public double getCaptureFactorForDiffuseNir () {
		return  round(l_captureFactorForDiffuseNir);
		}
	public double getCaptureFactorForInfraRed () {
		return  round(l_captureFactorForInfraRed);	
		}
	
	
	public double getDiffuseParIntercepted () {
		return  round((double) l_diffuseParIntercepted);		
	}
	public double getDirectParIntercepted () {
		return  round((double) l_directParIntercepted);		
		}
	public double getGlobalRadIntercepted () {
		return  round((double) l_globalRadIntercepted);		
		}
	public double getInfraRedIntercepted () {
		return  round((double) l_infraRedIntercepted);		
		}
	public double getLightCompetitionIndex() {
		return  round((double) l_lightCompetitionIndex);		
		}
	public double getCFdirectLonelyTree() {
		return  round((double) l_cFdirectLonelyTree);		
		}
	public double getCFdiffuseLonelyTree() {
		return  round((double) l_cFdiffuseLonelyTree);			
		}
	public double getParInterceptedAnnual () {
		return  (parInterceptedAnnual);		
	}
	
	private double round(double v) {
		v = v * 1000000;
		v = Math.round(v);
		v = v / 1000000;
		return v;		

	}
	public void setCaptureFactorForDiffusePar (double e) {l_captureFactorForDiffusePar =   e;}
	public void setCaptureFactorForDirectPar (double e) {l_captureFactorForDirectPar =   e;}
	public void setCaptureFactorForDirectNir (double e) {l_captureFactorForDirectNir =   e;}
	public void setCaptureFactorForDiffuseNir (double e) {l_captureFactorForDiffuseNir =   e;}
	public void setCaptureFactorForInfraRed (double e) {l_captureFactorForInfraRed = e;}
	
	public void setDiffuseParIntercepted (double e) {l_diffuseParIntercepted = (float) e;}
	public void setDirectParIntercepted (double e) {l_directParIntercepted = (float) e;}
	public void setGlobalRadIntercepted (double e) {l_globalRadIntercepted = (float) e;}
	public void setInfraRedIntercepted (double e) {l_infraRedIntercepted = (float) e;}
	public void setLightCompetitionIndex (double e){l_lightCompetitionIndex = (float) e;}
	public void setCFdiffuseLonelyTree (double e){l_cFdiffuseLonelyTree = (float) e;}
	public void setCFdirectLonelyTree (double e){l_cFdirectLonelyTree = (float) e;}


	//ACCESSOR FOR C AND N ALLOCATION MODULE

	public double getStemVolume () {return (double) stemVolume;}
	public double getCarbonStem () {return (double) carbonStem;}
	public double getCarbonStump () {return (double) carbonStump;}
	public double getCarbonFoliage () {return (double) carbonFoliage;}
	public double getCarbonFoliageMax () {return (double) carbonFoliageMax;}
	public double getCarbonBranches () {return (double) carbonBranches;}
	public double getCarbonCoarseRoots () {return (double) carbonCoarseRoots;}
	public double getCarbonFineRoots () {return (double) carbonFineRoots;}
	public double getCarbonLabile () {return (double) carbonLabile;}
	public double getCarbonStemExported () {return (double) carbonStemExported;}
	public double getCarbonFoliageExported () {return (double) carbonFoliageExported;}
	public double getCarbonBranchesExported () {return (double) carbonBranchesExported;}

	public void setStemVolume (double v) {stemVolume = (float) v;}
	public void setCarbonStem (double v) {carbonStem = (float) v;}
	public void setCarbonStump(double v) {carbonStump = (float) v;}
	public void setCarbonFoliage (double v) {carbonFoliage = (float) v;}
	public void setCarbonBranches (double v) {carbonBranches = (float) v;}
	public void setCarbonCoarseRoots (double v) {carbonCoarseRoots = (float) v;}
	public void setCarbonFineRoots (double v) {carbonFineRoots = (float) v;}	
	public void addCarbonCoarseRoots (double v) {carbonCoarseRoots += (float) v;}
	public void addCarbonFineRoots (double v) {carbonFineRoots += (float) v;}
	
	public void setCarbonStemExported (double v) {carbonStemExported = (float) v;}
	public void setCarbonFoliageExported (double v) {carbonFoliageExported = (float) v;}
	public void setCarbonBranchesExported (double v) {carbonBranchesExported = (float) v;}	
	public void addCarbonStemExported (double v) {carbonStemExported += (float) v;}
	public void addCarbonFoliageExported (double v) {carbonFoliageExported += (float) v;}
	public void addCarbonBranchesExported (double v) {carbonBranchesExported += (float) v;}		
	public void setCarbonLabile (double v) {carbonLabile = (float) v;}
	public void	addCarbonLabile (double v) {carbonLabile += (float) v;} //AQ-26.04.2011

	public double 	getAboveGroundCFraction () {return (double) aboveGroundCFraction;}
	public void setAboveGroundCFraction (double v) {aboveGroundCFraction = (float) v;}
	
	public void	setCarbonIncrement (double v) {carbonIncrement  =  (float) v;}
	public void	setCarbonFoliageIncrement (double v) {carbonFoliageIncrement  =  (float) v;}
	public void	setCarbonStemIncrement (double v) {carbonStemIncrement  =  (float) v;}
	public void	setCarbonBranchesIncrement (double v) {carbonBranchesIncrement  =  (float) v;}
	public void	setCarbonStumpIncrement (double v) {carbonStumpIncrement  =  (float) v;}
	public void setCarbonFineRootsIncrement (double v) {carbonFineRootsIncrement = (float) v;}
	public void setCarbonCoarseRootsIncrement (double v) {carbonCoarseRootsIncrement = (float) v;}	
	
	public double getCarbonIncrement () {return (double)  carbonIncrement;}	
	public double getCarbonFoliageIncrement () {return (double) carbonFoliageIncrement;}
	public double getCarbonStemIncrement () {return (double) carbonStemIncrement;}
	public double getCarbonBranchesIncrement () {return (double) carbonBranchesIncrement;}
	public double getCarbonStumpIncrement () {return (double) carbonStumpIncrement;}
	public double getCarbonCoarseRootsIncrement (){return (double) carbonCoarseRootsIncrement;}
	public double getCarbonFineRootsIncrement (){return (double) carbonFineRootsIncrement;}	
	
	
	
	public void setTargetLfrRatio (double r) {targetLfrRatio = (float) r;}
	public void setCoarseRootTotalTargetBiomass(double b) {coarseRootTotalTargetBiomass = (float) b;}
	public void addCoarseRootTotalTargetBiomass(double b) {coarseRootTotalTargetBiomass += (float) b;}
	
	public double 	getCoarseRootTotalTargetBiomass() {return (double) coarseRootTotalTargetBiomass; } 
	public void setCarbonAllocToGrowth (double v) {carbonAllocToGrowth = (float) v;}


	public double getNitrogenStem () {return (double) nitrogenStem;}
	public double getNitrogenStump () {return (double) nitrogenStump;}
	public double getNitrogenFoliage () {return (double) nitrogenFoliage;}
	public double getNitrogenBranches () {return (double) nitrogenBranches;}
	public double getNitrogenCoarseRoots () {return (double) nitrogenCoarseRoots;};
	public double getNitrogenFineRoots () {return (double) nitrogenFineRoots;}
	public double getNitrogenLabile () {return (double) nitrogenLabile;}
	public double getNitrogenStemExported () {return (double) nitrogenStemExported;}
	public double getNitrogenFoliageExported () {return (double) nitrogenFoliageExported;}
	public double getNitrogenBranchesExported () {return (double) nitrogenBranchesExported;}
	public void	setNitrogenStem (double v) {nitrogenStem = (float) v;}
	public void	setNitrogenStump (double v) {nitrogenStump = (float) v;}
	public void setNitrogenFoliage (double v) {nitrogenFoliage = (float) v;}
	public void setNitrogenBranches (double v) {nitrogenBranches = (float) v;}
	public void setNitrogenCoarseRoots (double v) {nitrogenCoarseRoots = (float) v;}
	public void setNitrogenFineRoots (double v) {nitrogenFineRoots = (float) v;}
	public void setNitrogenStemExported (double v) {nitrogenStemExported = (float) v;}
	public void setNitrogenFoliageExported (double v) {nitrogenFoliageExported = (float) v;}
	public void setNitrogenBranchesExported (double v) {nitrogenBranchesExported = (float) v;}	
	public void addNitrogenStemExported (double v) {nitrogenStemExported += (float) v;}
	public void addNitrogenFoliageExported (double v) {nitrogenFoliageExported += (float) v;}
	public void addNitrogenBranchesExported (double v) {nitrogenBranchesExported += (float) v;}	
	public void setNitrogenLabile (double v) {nitrogenLabile = (float) v;}
	public void	addNitrogenLabile (double v){nitrogenLabile += (float) v;} //GT-4.06.2010
	private void setNitrogenSaturation (double v) {nitrogenSaturation =  (float) v;}
	private void setNitrogenSatisfaction(double v) {nitrogenSatisfaction =  (float) v;}
		
	
	//Senescences racines 
	public double getCarbonCoarseRootSen() {return (double) carbonCoarseRootSen;}
	public double getCarbonFineRootSen (){return (double)  carbonFineRootSen;}
	
	public void setCarbonCoarseRootSen(double v) {carbonCoarseRootSen = (float) v;}
	public void setCarbonFineRootSen (double v) {carbonFineRootSen = (float) v;}
	public void addCarbonCoarseRootSen(double v) {carbonCoarseRootSen += (float) v;}
	public void addCarbonFineRootSen(double v) {carbonFineRootSen += (float) v;}
	public double getCarbonCoarseRootSenAnnual() {return (double) carbonCoarseRootSenAnnual;}
	public double getCarbonFineRootSenAnnual () {return (double) carbonFineRootSenAnnual;}
	public double getCarbonCoarseRootSenAnoxia () {return (double) carbonCoarseRootSenAnoxia;}
	public double getCarbonFineRootSenAnoxia () {return (double) carbonFineRootSenAnoxia;}
	public void setCarbonCoarseRootSenAnoxia (double v) { carbonCoarseRootSenAnoxia = (float) v;}
	public void setCarbonFineRootSenAnoxia (double v) {carbonFineRootSenAnoxia = (float) v;}
	public void addCarbonCoarseRootSenAnoxia (double v) {carbonCoarseRootSenAnoxia += (float) v;}
	public void addCarbonFineRootSenAnoxia (double v) {carbonFineRootSenAnoxia += (float) v;}
	public double getCarbonFineRootSenAnoxiaAnnual () {return (double) carbonFineRootSenAnoxiaAnnual;}
	public double getNitrogenCoarseRootSen(){return (double) nitrogenCoarseRootSen;}
	public float getNitrogenFineRootSen (){return nitrogenFineRootSen;}
	public void setNitrogenCoarseRootSen(double v){nitrogenCoarseRootSen = (float)v;}
	public void setNitrogenFineRootSen (double v){nitrogenFineRootSen = (float)v;}
	public void addNitrogenCoarseRootSen(double v){nitrogenCoarseRootSen += (float)v;}
	public void addNitrogenFineRootSen(double v){nitrogenFineRootSen += (float)v;}
	public double getNitrogenCoarseRootSenAnoxia(){return  (double)nitrogenCoarseRootSenAnoxia;}
	public double getNitrogenFineRootSenAnoxia (){return  (double)nitrogenFineRootSenAnoxia;}
	public void setNitrogenCoarseRootSenAnoxia(double v){nitrogenCoarseRootSenAnoxia = (float) v;}
	public void setNitrogenFineRootSenAnoxia (double v){nitrogenFineRootSenAnoxia = (float)v;}
	public void addNitrogenCoarseRootSenAnoxia(double v){nitrogenCoarseRootSenAnoxia += (float)v;}
	public void addNitrogenFineRootSenAnoxia(double v){nitrogenFineRootSenAnoxia += (float)v;}
	
	//---------------------------------------------------------------------------------------------


	public double	getCarbonAllocToGrowth () {return (double) carbonAllocToGrowth;}
	public double	getCarbonAllocToGrowthAnnual () {return (double) carbonAllocToGrowthAnnual;}
	public double	getCarbonAboveGroundEff () {return carbonFoliage/(carbonFoliage+carbonStem+carbonBranches+carbonStump);}
	public double	getCarbonBelowGroundEff () {return carbonFineRoots/(carbonFineRoots+carbonCoarseRoots);}

	public double 	getTargetLfrRatio() {return (double) targetLfrRatio;}
	public double 	getLfrRatio() {
		if(getCarbonFineRoots()>0){
			return getCarbonFoliage()/(getCarbonFineRoots()+getCarbonFoliage());
		} else {
			if (getCarbonFoliage()>0){
				return 0;
			} else {
				return 1;
			}
		}
	}

	// for SafeExport : Nitrogen concentrations in carbon pool (Kg N / Kg C)
	public double getNCLabile () {return nitrogenLabile/getCarbonLabile();}
	public double getNCStump () {return nitrogenStump/getCarbonStump();}
	public double getNCStem () {return nitrogenStem/getCarbonStem();}
	public double getNCFoliage () {
		if(getCarbonFoliage() != 0)
			return nitrogenFoliage/getCarbonFoliage();
		else
			return 0;
	}
	public double getNCBranches() {return nitrogenBranches/getCarbonBranches();}
	public double getNCCoarseRoots () {return nitrogenCoarseRoots/getCarbonCoarseRoots();};
	public double getNCFineRoots () {return nitrogenFineRoots/getCarbonFineRoots();}
	
	public double getNitrogenSaturation () {return (double) nitrogenSaturation;}
	public double getNitrogenSatisfaction () {return (double) nitrogenSatisfaction;}

	//ACCESSOR FOR MICROCLIMATE RESULTS
	public void setStoredRain (double v) {storedRain  =  (float) v;}
	public void addStoredRain (double v) {storedRain  +=  (float) v;}

	public float getStoredRain () {return storedRain;}
	public double getStoredRainMm () {
		double cellArea = this.getScene().getArea();
		return  storedRain/cellArea;
	}
	public void setStemflow (double v) {stemflow  =  (float) v;}
	public void addStemflow (double v) {stemflow  +=  (float) v;}
	public float getStemflow () {return stemflow;}
	public double getStemflowMm () {
		double cellArea = this.getScene().getArea();
		return  stemflow/cellArea;
	}

	public void setEvaporatedRain (double v) {evaporatedRain  =  (float) v;} 
	public void addEvaporatedRain (double v) {evaporatedRain  +=  (float) v;}
	public float getEvaporatedRain () {return  evaporatedRain;}
	public double getEvaporatedRainMm () {
		double cellArea = this.getScene().getArea();
		return  evaporatedRain/cellArea;}


	public void setInterceptedRain (double v) {interceptedRain  =  (float) v;}
	public float getInterceptedRain () {return  interceptedRain;}
	public float getInterceptedRainAnnual () {return  interceptedRainAnnual;}
	public double getInterceptedRainMm () {
		double cellArea = this.getScene().getArea();
		return  interceptedRain/cellArea;
	}
	public void addInterceptedRain (double v) {interceptedRain  +=  (float) v;}

	//ACCESSOR FOR WATER AND NITROGEN BUDGET
	public void setWaterDemand (double v) {waterDemand  =  (float) v;}
	public void setWaterDemandReduced (double v) {waterDemandReduced  = (float) v;}
	public double getWaterDemand() {return (double)  waterDemand;}
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

	public double getLongitudinalPotentialDrop(){return this.getFineRoots().getLongitudinalTransportPotential();}
	public double getRadialPotentialDrop(){return this.getFineRoots().getRadialTransportPotential();}



	private double getStomatalConductance () {return (double) stomatalConductance;}
	private void setStomatalConductance (double v) {stomatalConductance = (float) v;}
	
	
	
	public void setWaterUptake  (double v) {waterUptake  = (float) v;}
	public double getWaterUptake () {return  (double) waterUptake;}
	public double getWaterUptakeAnnual () {return (double) waterUptakeAnnual ;}
	public double getWaterUptakeMm () {
		double cellArea = this.getScene().getArea();
		return  getWaterUptake ()/cellArea;
	}

	public void addWaterUptake  (double v) {waterUptake = waterUptake+ (float) v;}
	
	public double getWaterStress () {return (double) waterStress;}
	public void setWaterStress (double v) {waterStress =  (float) v;}
	//sum of water stress divided by number of days
	public double getWaterStressSpring () {return waterStressSpring/91.0;}
	public double getWaterStressSummer () {return waterStressSummer/92.0;}
	
	
	public double	getNitrogenDemand () {return (double)  nitrogenDemand;}
	public void		setNitrogenDemand (double v) {nitrogenDemand   =  (float) v;}
	public void setNitrogenUptake (double v) {nitrogenUptake =  (float) v;}
	public void addNitrogenUptake (double v) {nitrogenUptake = nitrogenUptake+ (float) v;}
	public double getNitrogenUptake() {return (double) nitrogenUptake;}
	public double getNitrogenUptakeAnnual() {return (double) nitrogenUptakeAnnual;}
	public double getNitrogenStress () {return (double) nitrogenStress;}

	public void setNitrogenStress (double v) {nitrogenStress =  (float) v;}
	
	//sum of nitrogen stress divided by number of days
	public double getNitrogenStressSpring () {return nitrogenStressSpring/91.0;}
	public double getNitrogenStressSummer () {return nitrogenStressSummer/92.0;}
	public void setRsStressEffect (double v) {rsStressEffect =  (float) v;}
	public void setLueStressEffect (double v) {lueStressEffect =  (float) v;}
	public double getRsStressEffect () {return (double) rsStressEffect;}
	public double getLueStressEffect () {return (double) lueStressEffect;}
	
	//TREE SPECIES
	public SafeTreeSpecies getTreeSpecies () {return treeSpecies ;}


	//Speciable interface FC 20.12.2004
	public Species getSpecies () {return treeSpecies;}



	public int getCrownType () {
		return SimpleCrownDescription.SPHERIC;
	}
	public Color getCrownColor () {
		return Color.green;
	}
	public float getTransparency () {
		return 0;
	}// 0.0 (opaque) to 1.0 (transparent)


	//PHENOLOGY
	public void setPlanted(boolean b) {planted = b;}
	public void setPhenologicalStage (int p) {phenologicalStage = p;}
	public int	getPhenologicalStage() {return phenologicalStage;} 
	public void setFirstYearStarted(boolean b) {firstYearStarted = b;}
	public void setBudburstDate (int d) {budburstDate = d;}
	public void setLeafExpansionEndingDate (int d) {leafExpansionEndingDate = d;}
	public void setLeafFallStartingDate (int d) {leafFallStartingDate = d;}
	public void setLeafFallEndingDate (int d) {leafFallEndingDate = d;}
	public void setBudburstAccumulatedTemperature (float d) {budburstAccumulatedTemperature=d;}

	protected float getBudburstAccumulatedTemperature () {return budburstAccumulatedTemperature;}
	public boolean isFirstYearStarted () {return firstYearStarted;}
	public boolean isPlanted () {return planted;}
	public int getBudburstDate () {return budburstDate;}
	public int getLeafExpansionEndingDate () {return leafExpansionEndingDate;}
	public int getLeafFallStartingDate () {return leafFallStartingDate;}
	public int getLeafFallEndingDate () {return leafFallEndingDate;}
	
	public void setPollarding(boolean b) {pollarding = b;}
	public boolean isPollarding() {return pollarding;}
	public void setPollarding2(boolean b) {pollarding2 = b;}
	public boolean isPollarding2() {return pollarding2;}
	
	public void setBudburstDelay (int i) {budburstDelay = i;}
	public int getBudburstDelay() {return budburstDelay;}	
	
	
	//
	// GROWTH
	//
	public void addYear (int i) {age = age + i;}
	public void addHeight (double v) {height += v;}
	public void addLeafArea (double v) {leafArea +=  (float) v;}
	
	
	public void setHeight (double v) {height = (float) v;}
	public void setDbhMeters (double v) {dbhMeters = (float) v;}
	public void setDbh (double v) {dbh = (float) v;}
	public void setCrownBaseHeight (double v) {crownBaseHeight = (float) v;}
	public void setCrownRadiusTreeLine (double v) {crownRadiusTreeLine = (float) v;}
	public void setCrownRadiusInterRow (double v) {crownRadiusInterRow = (float) v;}
	public void setCrownRadiusVertical (double v) {crownRadiusVertical = (float) v;}
	public void setCrownVolume (double v) {crownVolume = (float) v;}
	public void setCrownParameter (double v) {crownParameter = (float) v;}

	
	
	
	public void setNbCellsBellow(int v) {nbCellsBellow =  v;}
	public void addNbCellsBellow(int v) {nbCellsBellow +=  v;}
	public double getNbCellsBellow () {return nbCellsBellow;}
	
	public void setLaiAboveCells (double v) {laiAboveCells =  (float) v;}
	public void setLeafArea (double v) {leafArea =  (float) v;}
	public void setLastLeafArea (double v) {lastLeafArea = (float) v;}

	public float getLaiAboveCells () {return laiAboveCells;}
	public double getLai() {return leafArea / (Math.PI * crownRadiusTreeLine * crownRadiusInterRow);}
	public double getLeafArea () {return (double) leafArea;}
	public double getLeafAreaMax () {return (double) leafAreaMax;}
	protected float getLastLeafArea () {return lastLeafArea;}

	public double getCrownBaseHeight () {return (double) crownBaseHeight;}
	public double getCrownRadius () {
		return (Math.sqrt (crownRadiusTreeLine * crownRadiusInterRow));}
	public double getCrownRadiusTreeLine () {return (double) crownRadiusTreeLine;}
	public double getCrownRadiusInterRow () {return (double) crownRadiusInterRow;}
	public double getCrownRadiusVertical () {return (double) crownRadiusVertical;}

	public double getCrownVolume () {return (double) crownVolume;}
	public float getCrownParameter () {return  crownParameter;}
	public double getDbhMeters () {return (double) dbhMeters;}


	public double getFrLength(){
		double frLength = getFineRoots().getFirstRootNode().computeCoarseRootTargetBiomass(this, null, 0d , 0d , 0d);
		return(frLength);
	}
	
	public double getStemYield () {
		return(getCarbonStem() / this.getTreeSpecies ().getWoodCarbonContent());
	}
	
	
	//Sitraka 07/10/2009
	public double getCarbonFoliageSen () {return (double) carbonFoliageSen;}
	public double getCarbonFoliageSenAnnual () {return carbonFoliageSenAnnual;}
	public double getNitrogenFoliageSen() {return nitrogenFoliageSen;}
	public void setCarbonFoliageSen (double d) {carbonFoliageSen= (float) d;}
	public void setNitrogenFoliageSen (double d) {nitrogenFoliageSen= (float) d;}

	

	public void setWaterUptakeInSaturation(double v) {waterUptakeInSaturation = (float) v;}
	public void addWaterUptakeInSaturation (double v) {waterUptakeInSaturation += (float) v;}
	public double getWaterUptakeInSaturation () {return (double) waterUptakeInSaturation;}

	public void setNitrogenUptakeInSaturation(double v) {nitrogenUptakeInSaturation = (float) v;}
	public void addNitrogenUptakeInSaturation (double v) {nitrogenUptakeInSaturation += (float) v;}
	public double getNitrogenUptakeInSaturation () {return (double) nitrogenUptakeInSaturation;}	
	
	//Increment biomass of an existing coarse root
	public void incrementCoarseRoots (SafeVoxel voxel, double biomass) {
		SafeRootNode node = (SafeRootNode) (getFineRoots().getRootTopology().get (voxel));
		node.addCoarseRootBiomass (biomass);
	}


	public double getAboveGroundAllocFrac () {return  aboveGroundAllocFrac;}
	public double getStemAllocFrac () {return  stemAllocFrac;}
	public double getBranchAllocFrac () {return  branchAllocFrac;}
	public double getFoliageAllocFrac () {return  foliageAllocFrac;}
	
	public double getBelowGroundAllocFrac () {return  belowGroundAllocFrac;}
	public double getStumpAllocFrac () {return  stumpAllocFrac;}
	public double getFineRootAllocFrac () {return  fineRootAllocFrac;}
	public double getCoarseRootAllocFrac () {return  coarseRootAllocFrac;}
	
	public double getAboveGroundSink () {return  aboveGroundSink;}
	public double getStemSink () {return  stemSink;}
	public double getBranchSink () {return  branchSink;}
	public double getFoliageSink () {return  foliageSink;}
	
	public double getBelowGroundSink () {return  belowGroundSink;}
	public double getStumpSink () {return  stumpSink;}
	public double getFineRootSink () {return  fineRootSink;}
	public double getCoarseRootSink () {return  coarseRootSink;}
	
	
	//for annual export 
	//return the total volume of rooted voxels
	public double getRootedVolume(){
		double volume = 0;
		if (getFineRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();

				volume += voxel.getVolume();	
			}	
		}
		return volume; 
	}

	//for annual export 
	//return the total volume of rooted voxels per layer 
	public float[] getRootedVolumePerLayer(){
		float[] volume = new float[5];
		if (getFineRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
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
	
		if (getFineRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				//IL 28-11-2017
				//je prends la partie enti�re car si l'arbre se trouve sur le bord de la cellule le test ne marche plus
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
	
		if (getFineRoots().getFirstRootNode() != null) 
		{
			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();		
				//IL 28-11-2017
				//je prends la partie enti�re car si l'arbre se trouve sur le bord de la cellule le test ne marche plus
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
		if (getFineRoots().getFirstRootNode() != null)
		return getFineRoots().getFirstRootNode().getDeeperSonDepth();
		else return 0;
	}


	//FINE ROOTS
	public SafeFineRoot getFineRoots() {return fineRoots;}

	public double getTotalRootLength () {
		if (this.getFineRoots() != null)
			return this.getFineRoots().getTotalRootLength();
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
	
	public float getTotalOptiN () {

		//Optimum level of (structural) N for whole plant
		double optiNCStem 	 	= this.getTreeSpecies ().getOptiNCStem ();
		double optiNCStump		= this.getTreeSpecies ().getOptiNCStump();
		double optiNCFoliage 	= this.getTreeSpecies ().getOptiNCFoliage ();
		double optiNCBranch 	= this.getTreeSpecies ().getOptiNCBranch ();
		double optiNCCoarseRoot = this.getTreeSpecies ().getOptiNCCoarseRoot ();
		double optiNCFineRoot 	= this.getTreeSpecies ().getOptiNCFineRoot ();
	
		return (float)  (getCarbonBranches() 	* optiNCBranch
			    + getCarbonCoarseRoots() * optiNCCoarseRoot
				+ getCarbonFineRoots() 	* optiNCFineRoot
				+ getCarbonFoliage() 	* optiNCFoliage
				+ getCarbonStem ()		* optiNCStem
				+ getCarbonStump()		* optiNCStump);
	}
	
	public float getTotalN () {
		
		return (float) (getNitrogenBranches()
			  + getNitrogenCoarseRoots()
			  + getNitrogenFineRoots()
			  + getNitrogenFoliage()
			  + getNitrogenStem()
			  + getNitrogenStump()
			  + getNitrogenLabile());
	}
	
	public double getRootShootRatio () {
		return (carbonStump + carbonFineRoots + carbonCoarseRoots) / 
				(carbonStem + carbonBranches + carbonFoliage);
	}
	/**
	* return total structural carbon
	*/
	public double getTotalStructuralCarbon() {

		return (carbonBranches + carbonFoliage + carbonStem + carbonStump +
				+ carbonCoarseRoots + carbonFineRoots);
	}
	
	//for export
	public int getIdTree() {return getId();}

	public double getWaterUptakePotential () {
		if (fineRoots==null) return 0;
		return fineRoots.getWaterUptakePotential();
	}
	
  	public void drawRootTopology() {
  		System.out.println("*******************drawRootTopology");
		if (getFineRoots().getRootTopology() == null) return;

		
//			//FOR EACH VOXEL in coarse root topology map
			for (Iterator c = getFineRoots().getRootTopology().keySet().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootNode node = getFineRoots().getRootTopology (voxel);
			System.out.println("cell="+voxel.getCell().getId()+" node="+node);


			}
  	}

  	public void drawRootMap() {
  		System.out.println("*******************drawRootMap");
		if (getFineRoots().getRootedVoxelMap() == null) return;

		
//			//FOR EACH VOXEL in coarse root topology map
			for (Iterator c = getFineRoots().getRootedVoxelMap().iterator (); c.hasNext ();) {
				SafeVoxel voxel = (SafeVoxel) c.next ();
				SafeRootVoxel rootVoxel = getFineRoots().getRootedVoxelMap (voxel);
			System.out.println("cell="+voxel.getCell().getId()+" rootVoxel="+rootVoxel);


			}
  	}
  	
  	public void drawNodes() {
  		System.out.println("*******************drawNodes");
//		if (firstRootNode == null) return;
//		firstRootNode.drawNodes();
  	}
  	
}
