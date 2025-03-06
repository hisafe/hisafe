/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
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
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */
package safe.model;

import java.io.Serializable;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import safe.stics.*;

/**
 * SafeCrop represent the crop sowed on this cell (can be baresoil)
 * Only one SafeCrop is created by cell (state variables of the crop are homogenous)
 * Crop model is implemented by STICS (calling fortran native method) 
 *
 * @author Isabelle Lecomte - INRAE Montpellier - july 2002 
 */

 public class SafeCrop  implements Serializable {

	private static final long serialVersionUID = 1L;
	private String cropSpeciesName;				//crop species name (can be baresoil)
 	private SafeCell cell;						//cell object reference
 	private int cropAge;						//age of the ccop since sowing (DAYS) 
 	
 	//JNA object
	public SafeSticsCrop sticsCrop;				//plant variables in STICS
	public SafeSticsCommun sticsCommun;			//commun variables in STICS
	public SafeSticsSoil sticsSoil;				//soil variables in STICS
	public SafeSticsClimat sticsClimat;			//climat varibles in STICS
	
	//LAI observed from file to force LAI calculated
	public Map<String, SafeSticsLai> laiMap;

	//Roots topology 
	private SafePlantRoot plantRoots;			//refer to plant root object

	private int startDay;						// First day of simulation  DOY
 	private int sowingDay;						// Date of sowing           DOY
 	private int harvestDay;						// Date of harvest          DOY
 	private int sticsDay;						// Date of STICS simulation DOY
 	private float lai;							// m2.m-2
 	private float eai;							// m2.m-2
 	private float biomass;						// Aboveground dry matter (masec) t.ha-1
 	private float grainBiomass;					// Grain dry matter (magrain) t.ha-1							
 	private float biomassIncrement;				// Growth rate of the plant (dltams) t ha-1.j-1
 	private float biomassHarvested;				// Aboveground dry matter havested (MSexporte) t.ha-1
 	private float yield;						// Yield t.ha-1 
 	private float height;						// Height of canopy (hauteur) mm
 	private float rootsDepth;					// m
 	private float soilManagementDepth;			// m
 	
 	/* Phenological stage vegetavive
		1=snu SOL NU		
		2=plt PLANTATION ou SEMIS		
		3=dor DORMANCE (ou DEBDORM et FINDORM pour les ligneux)	
		4=ger GERMINATION	
		5=lev LEVEE		
		6=amf accélération maximale de croissance foliaire		
		7=lax indice foliaire maxi, fin de croissance foliaire nette ou brute selon l’option.		
		8=sen début sénescence nette (option LAInet)		
		9=lan indice foliaire nul (option LAInet)		
		10=rec RECOLTE
	*/
	private int phenologicStageVegetative;		//Phenological stage vegetavive
	private int cptDaysStageVegetative;			//compting days between 2 stages vegetative
	
 	/* Phenological stage reproductive
		1=snu SOL NU
		2=flo Floraison		
		3=drp début remplissage des organes récoltés		
		4=nou Nouaison (Fin de la nouaison, pour les plantes indéterminées)		
		5=des début dessication des organes récoltés	
		6=mat maturité physiologique	
		7=rec Récolte
	*/
	private int phenologicStageReproductive;	//Phenological stage reproductive
	private int cptDaysStageReproductive;		//compting days between 2 stages reproductive
				
	// Light model output
	private float 	captureFactorForDiffusePar;		// m2 m-2 d-1
	private float	captureFactorForDirectPar;		// m2 m-2 d-1
	private float 	directParIntercepted;			// Moles PAR m-2 d-1
	private float 	diffuseParIntercepted;			// Moles PAR m-2 d-1
	private float	competitionIndexForTotalPar;	// unitless
	private float 	interceptedPar;					// imported from Stics (verif) // GT 6/03/2008
	private float   parExtinctionCoef;				// extinction coefficient for Par interception by crop

 	//Water budget 
	private float capillaryRise;					// mm
	private float irrigation;						// mm
 	private float waterDemandReduced;				// mm
	private float waterUptake;						// mm
	private float hisafeWaterStress;				// calculated by hisafe 
	private float hisafeTurfac;						// turfac calculated by hisafe 
	private float hisafeSenfac;						// senfac calculated by hisafe 
	private float sumHisafeWaterStressVegetative;	//sum of water stress between 2 vegetative stage
	private float sumHisafeWaterStressReproductive; //sum of water stress between 2 reproductive stage

	//Nitrogen budget 
	private float nitrogenIrrigation;				//kg ha-1
	private float nitrogenFertilisationMineral;		//kg ha-1
	private float nitrogenFertilisationOrganic;		//kg ha-1
	private float nitrogenHarvested;				//kg ha-1
	private float nitrogenUptake;					//kg ha-1
	private float nitrogenLeachingWaterTable;		//kg ha-1 
	private float nitrogenAddedByWaterTable;		//kg ha-1 
	private float hisafeNitrogenStress;				// calculated by hisafe
	private float sumHisafeNitrogenStressVegetative;	//sum of Nitrogen stress between 2 vegetative stage
	private float sumHisafeNitrogenStressReproductive; //sum of Nitrogen stress between 2 reproductive stage


	//Crop carbon and nitrogen litter //AQ - 05.2011
	private float cropCarbonLeafLitter;		//cumulative mount of C in fallen leaves (QCplantetombe+QCrogne+QCressuite) (kgC.ha-1) 
	private float cropNitrogenLeafLitter;	//cumulative mount of N in fallen leaves (QNplantetombe+QNrogne+QNressuite) (kgN.ha-1) 
	private float cropCarbonRootsLitter;		//cumulative amount of C in dead roots added to soil (QCrac)  (kgC.ha-1) 
	private float cropNitrogenRootsLitter;	//cumulative amount of N in dead roots added to soil (QNrac)  (kgN.ha-1 )

	//Values of previous day to get rid of STICS cumulated data
	private float nitrogenIrrigationPrev; 
	private float nitrogenFertilisationMineralPrev;	
	private float nitrogenFertilisationOrganicPrev;
	private float cropCarbonLeafLitterPrev;		
	private float cropNitrogenLeafLitterPrev;	
	private float cropCarbonRootsLitterPrev;		
	private float cropNitrogenRootsLitterPrev;	
	private float capillaryRisePrev;
	private float nitrogenHarvestedPrev;
	private float biomassHarvestedPrev;
	

	//MAX values for EXPORT
 	private float laiMax;						// m2.m-2
 	private float eaiMax;						// m2.m-2
 	private float yieldMax;						// t.ha-1 (0 % water)
 	private float biomassMax;					// t.ha-1 
 	private float rootsDepthMax;				// m
 	private float heightMax;					// m
 	
	//Monthly values for EXPORT
	private float monthBiomass;
	private float monthYield;
	private float monthEai;
	private float monthLai;
	private float monthDiffuseParIntercepted;
	private float monthDirecParIntercepted;
	private int monthNbrDays;
	
	//totals for annual EXPORT
	private float annualCapillaryRise;				// mm
	private float annualIrrigation;					// mm
 	private float annualWaterDemand;				// mm
 	private float annualWaterDemandReduced;			// mm
	private float annualWaterUptake;				// mm
	private float annualNitrogenDemand;				//kg N ha-1
	private float annualNitrogenUptake;				//kg N ha-1
	private float annualSoilEvaporation;			// mm
	private float annualRunOff;						// mm
	private float annualSurfaceRunOff;				// mm
	private float annualDrainageBottom;				// mm
	private float annualDrainageArtificial;			// mm
	private float annualRain;						// mm
	private float annualParIntercepted;				// Moles PAR m-2 
	private float annualNitrogenLeachingBottom;		//kg N ha-1
	private float annualNitrogenLeachingWaterTable; //kg N ha-1
	
//Ajout pour debug
	private float qNplante;							//Amount of nitrogen taken up by the plant   kgN.ha-1
	private float ircarb;							//Carbon harvest index // g  grain g plant-1
	private float deltai;	
	private float durvie;
	private float laisen; 
	private float tmin; 
	private float tmax; 
	private float codecaltemp;
	
 	public SafeCrop (SafeCell cell) {

 		this.cell = cell;
 		this.sticsSoil 		= null;	
		this.sticsCommun 	= new SafeSticsCommun();	
		this.sticsCrop 		= new SafeSticsCrop();

 		this.cropAge = 0;
 		this.lai = 0;
 		this.deltai = 0; 
 		this.durvie = 0;
 		this.laisen = 0;
 		this.eai = 0;
 		this.biomass = 0;
 		this.rootsDepth = 0;
 		this.height = 0;
		this.yield = 0;
 		this.parExtinctionCoef = 0;
		this.sowingDay = 0;
		this.harvestDay = 0;
		this.waterDemandReduced = 0;
		this.waterUptake = 0;
		this.hisafeWaterStress = 1;
		this.hisafeTurfac = 1;
		this.hisafeSenfac = 1;
		this.hisafeNitrogenStress = 1;
		this.phenologicStageVegetative= 1;
		this.phenologicStageReproductive= 1;
		this.cptDaysStageVegetative = 0;
		this.cptDaysStageReproductive = 0;
		this.sumHisafeWaterStressVegetative = 0;
		this.sumHisafeNitrogenStressVegetative = 0;
		this.sumHisafeWaterStressReproductive = 0;
		this.sumHisafeNitrogenStressReproductive = 0;
		this.captureFactorForDiffusePar= 0;
		this.captureFactorForDirectPar= 0;
		this.directParIntercepted= 0;
		this.diffuseParIntercepted= 0;
		this.competitionIndexForTotalPar= 1;
		this.interceptedPar=0;


		this.nitrogenLeachingWaterTable =0;
		this.nitrogenAddedByWaterTable =0;
		this.tmin = 0;
		this.tmax = 0;

		//Initialise roots informations
		this.plantRoots = new SafePlantRoot (this);
		
		//by defaut main crop species
		this.startDay = 0;

	}
 	public void dailyRaz () {
 		
		//To get daily values from STICS cumulated data
		this.nitrogenIrrigationPrev 			= nitrogenIrrigation; 
		this.nitrogenFertilisationMineralPrev 	= nitrogenFertilisationMineral;  
		this.nitrogenFertilisationOrganicPrev 	= nitrogenFertilisationOrganic; 
		this.capillaryRisePrev 					= capillaryRise;
		this.cropCarbonLeafLitterPrev 			= cropCarbonLeafLitter;		
		this.cropNitrogenLeafLitterPrev 		= cropNitrogenLeafLitter;	
		this.cropCarbonRootsLitterPrev 			= cropCarbonRootsLitter;		
		this.cropNitrogenRootsLitterPrev 		= cropNitrogenRootsLitter;	
		this.nitrogenHarvestedPrev 				= nitrogenHarvested;		
		this.biomassHarvestedPrev 				= biomassHarvested;	
		
		//Monthly values for EXPORT
		this.monthBiomass = this.monthBiomass + this.biomass;
		this.monthYield = this.monthYield + this.yield;
		this.monthEai 	= this.monthEai + this.eai;
		this.monthLai	= this.monthLai + this.lai;
		this.monthDiffuseParIntercepted = this.monthDiffuseParIntercepted + this.diffuseParIntercepted;
		this.monthDirecParIntercepted 	= this.monthDirecParIntercepted + this.directParIntercepted;	
		this.monthNbrDays = this.monthNbrDays + 1; 
							
		//Annual values for EXPORT
		if (this.lai > this.laiMax) 
			this.laiMax = this.lai ; 	
		if (this.eai > this.eaiMax) 
			this.eaiMax = this.eai ; 	
		if (this.rootsDepth > this.rootsDepthMax) 
			this.rootsDepthMax = this.rootsDepth ; 	
		if (this.yield > this.yieldMax) 
			this.yieldMax = this.yield ; 	
		if (this.biomass > this.biomassMax) 
			this.biomassMax = this.biomass ; 	
		if (this.height > this.heightMax) 
			this.heightMax = this.height ; 	
				
		//raz daily		
		this.waterUptake		= 0;
		this.waterDemandReduced = 0;
		this.nitrogenLeachingWaterTable = 0;
		this.nitrogenAddedByWaterTable = 0;
		this.captureFactorForDiffusePar	= 0;
		this.captureFactorForDirectPar	= 0;
		this.directParIntercepted		= 0;
		this.diffuseParIntercepted		= 0;
		this.competitionIndexForTotalPar= 1;
		this.yield = 0;

		this.getPlantRoots().dailyRaz();
		
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
	 * RAZ of annual values razTotalWater
	 **/
	public void razTotalAnnual () {
		this.annualCapillaryRise = 0;
		this.annualIrrigation = 0;
		this.annualWaterDemand = 0;
		this.annualWaterDemandReduced= 0;
		this.annualWaterUptake = 0;
		this.annualNitrogenDemand = 0;
		this.annualNitrogenUptake = 0;
		this.annualSoilEvaporation = 0;
		this.annualRunOff = 0;
		this.annualSurfaceRunOff = 0;
		this.annualDrainageBottom = 0;
		this.annualDrainageArtificial = 0;
		this.annualRain = 0;
		this.annualNitrogenLeachingBottom = 0;
		this.annualNitrogenLeachingWaterTable = 0; 
		this.annualParIntercepted = 0;		
		this.laiMax = 0; 
		this.eaiMax = 0;
		this.yieldMax = 0;
		this.rootsDepthMax = 0;
		this.biomassMax = 0;
		this.heightMax = 0;

	}
	
	/**
	 * STICS crop FIRST initialization with the first crop species 
	 * SOIL IS NOT INITIALISED !
	 */
	public  void cropInitialisation (SafeTestJNA jna, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
									SafeSoil soil, 
									SafeCropZone zone,
									SafePlotSettings settings,									
									String exportDir,
									String laiFileName)  throws Exception {

		
		//soil initialisation
		this.sticsSoil 		= new SafeSticsSoil (soil);
		this.sticsSoil.initialise (soil, settings); 

		//JNA NATIVE method to check general parameters and soil
		//result is in output/initialisation.sti
		jna.verifParam (sticsParam, sticsTransit, this.sticsSoil, this.sticsCommun, exportDir);

		//Parameter for chaining simulations in STICS = NO
		sticsCommun.P_codesuite = 0;

		//Parameter for number of crop in STICS = 1
		sticsCommun.numcult = 1;
		
		//LAI forcing with a file entry 
		sticsCommun.P_codesimul=1;
		if (laiFileName != "") {
			new SafeSticsLaiFormat (laiFileName).load (this);
			sticsCommun.P_codesimul=2;
		}

	    //if we remove this, fertilization and irrigation will be erased in STICS
	    sticsCommun.napini[0] = zone.getSticsItk().nap;
	    sticsCommun.napNini[0] = zone.getSticsItk().napN;
	    sticsCommun.nbjresini[0] = zone.getSticsItk().P_nbjres;
	    
	    //Initial values (for perenial crops) 
		sticsCrop.P_stade0 =  zone.getInitialCropStage(); 
		sticsCrop.P_lai0 = (float) zone.getInitialCropLai();
		sticsCrop.P_masec0 = (float) zone.getInitialCropBiomass();
		sticsCrop.P_zrac0 = (float) (zone.getInitialCropRootsDepth() * 100);	//convert in cm
		sticsCrop.P_magrain0 = (float) zone.getInitialCropGrainBiomass();
		sticsCrop.P_QNplante0 = (float) zone.getInitialCropNitrogen();
		sticsCrop.P_resperenne0 = (float) zone.getInitialCropReserveBiomass();
		sticsCrop.P_densinitial[0] = (float) zone.getInitialCropRootsDensity(0);
		sticsCrop.P_densinitial[1] = (float) zone.getInitialCropRootsDensity(1);
		sticsCrop.P_densinitial[2] = (float) zone.getInitialCropRootsDensity(2);
		sticsCrop.P_densinitial[3] = (float) zone.getInitialCropRootsDensity(3);
		sticsCrop.P_densinitial[4] = (float) zone.getInitialCropRootsDensity(4);

		//SAVE some STICS values that can be erased in perenial chaining years
		System.arraycopy(this.sticsCrop.P_stamflax	, 0, zone.getCropSpecies().P_stamflax	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stlevamf	, 0, zone.getCropSpecies().P_stlevamf	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stlevdrp	, 0, zone.getCropSpecies().P_stlevdrp	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stflodrp	, 0, zone.getCropSpecies().P_stflodrp	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stlaxsen	, 0, zone.getCropSpecies().P_stlaxsen	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stsenlan	, 0, zone.getCropSpecies().P_stsenlan	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stdrpmat	, 0, zone.getCropSpecies().P_stdrpmat	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stdrpdes	, 0, zone.getCropSpecies().P_stdrpdes	, 0, 	30);
		
		//JNA NATIVE method to check plant and itk parameters
		//result is in output/initialisation.sti
		jna.verifPlante(sticsParam, sticsTransit, this.sticsCommun, zone.getSticsItk(), this.sticsCrop, zone.getId(), exportDir);
	    
		return;
	}

	/**
	 * STICS initialization reload with a new crop species 
	 * SOIL IS NOT ERASED !
	 */
	public  void cropReload (SafeTestJNA jna, 
			SafeSticsParameters sticsParam, 
			SafeSticsTransit sticsTransit, 
			SafeSoil soil, 
			SafeCropZone zone,
			String exportDir)  throws Exception {


		//put soil initial values = current values (for STICS REPORT) 
		this.sticsSoil.reinitialise (); 

		//RAZ CROP data
		this.sticsCommun.reinitialise (); 
		this.sticsCrop.reinitialise();

		//RAZ CROP roots informations
		this.plantRoots = new SafePlantRoot (this);	
		
		//Parameter for chaining simulations in STICS = YES
		sticsCommun.P_codesuite = 1;		

		//Parameter for number of crop in STICS = 1
		sticsCommun.numcult = 1;
		
	    //if we remove this, fertilization and irrigation will be erased in STICS
	    sticsCommun.napini[0] = zone.getSticsItk().nap;
	    sticsCommun.napNini[0] = zone.getSticsItk().napN;
	    sticsCommun.nbjresini[0] = zone.getSticsItk().P_nbjres;
	   
		//SAVE some STICS values that can be erased in perenial chaining years
		System.arraycopy(this.sticsCrop.P_stamflax	, 0, zone.getCropSpecies().P_stamflax	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stlevamf	, 0, zone.getCropSpecies().P_stlevamf	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stlevdrp	, 0, zone.getCropSpecies().P_stlevdrp	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stflodrp	, 0, zone.getCropSpecies().P_stflodrp	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stlaxsen	, 0, zone.getCropSpecies().P_stlaxsen	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stsenlan	, 0, zone.getCropSpecies().P_stsenlan	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stdrpmat	, 0, zone.getCropSpecies().P_stdrpmat	, 0, 	30); 
		System.arraycopy(this.sticsCrop.P_stdrpdes	, 0, zone.getCropSpecies().P_stdrpdes	, 0, 	30);
		
	    //Restore CROP initial values if PERENIAL 
	    if (this.sticsCrop.P_codeperenne == 2) {

		    //Initial values  
			this.sticsCrop.P_stade0 =  zone.getInitialCropStage(); 
			this.sticsCrop.P_lai0 = (float) zone.getInitialCropLai();
			this.sticsCrop.P_masec0 = (float) zone.getInitialCropBiomass();
			this.sticsCrop.P_zrac0 = (float) (zone.getInitialCropRootsDepth() * 100);	//convert in cm
			this.sticsCrop.P_magrain0 = (float) zone.getInitialCropGrainBiomass();
			this.sticsCrop.P_QNplante0 = (float) zone.getInitialCropNitrogen();
			this.sticsCrop.P_resperenne0 = (float) zone.getInitialCropReserveBiomass();
			this.sticsCrop.P_densinitial[0] = (float) zone.getInitialCropRootsDensity(0);
			this.sticsCrop.P_densinitial[1] = (float) zone.getInitialCropRootsDensity(1);
			this.sticsCrop.P_densinitial[2] = (float) zone.getInitialCropRootsDensity(2);
			this.sticsCrop.P_densinitial[3] = (float) zone.getInitialCropRootsDensity(3);
			this.sticsCrop.P_densinitial[4] = (float) zone.getInitialCropRootsDensity(4);

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
			this.phenologicStageReproductive = 1;
			this.phenologicStageVegetative = 1;
			this.cptDaysStageVegetative = 0;
			this.cptDaysStageReproductive = 0;
			this.sumHisafeWaterStressVegetative = 0;
			this.sumHisafeNitrogenStressVegetative = 0;
			this.sumHisafeWaterStressReproductive = 0;
			this.sumHisafeNitrogenStressReproductive = 0;
			this.yield = 0;

	    }

		//JNA NATIVE method to check plant and itk parameters
		//result is in output/initialisation.sti
	    jna.verifPlante(sticsParam, sticsTransit, this.sticsCommun, zone.getSticsItk(), this.sticsCrop, zone.getId(), exportDir);
		
		//reset cropAge
		this.cropAge = 0;
		this.sowingDay = 0;
		this.harvestDay = 0;

		return;
	}
	

	/**
	 * STICS initialization reload with same perenial crop species
	 * CROP data are NOT ERASED 
	 * SOIL IS NOT ERASED 
	 */	
	public  void cropPerenialReload (SafeTestJNA jna, 
			SafeSticsParameters sticsParam, 
			SafeSticsTransit sticsTransit, 
			SafeCropZone zone, 
			String exportDir)  throws Exception {
			

		
		//put soil initial values = current values (for STICS REPORT) 
		this.sticsSoil.reinitialise (); 
		
		//Parameter for chaining simulations in STICS = YES
		sticsCommun.P_codesuite = 1;		

		//Parameter for number of crop in STICS = 1
		sticsCommun.numcult = 1;
		    
		//RESTORE some STICS values that can be erased in chaining years
		System.arraycopy(zone.getCropSpecies().P_stamflax	, 0, this.sticsCrop.P_stamflax	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stlevamf	, 0, this.sticsCrop.P_stlevamf	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stlevdrp	, 0, this.sticsCrop.P_stlevdrp	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stflodrp	, 0, this.sticsCrop.P_stflodrp	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stlaxsen	, 0, this.sticsCrop.P_stlaxsen	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stsenlan	, 0, this.sticsCrop.P_stsenlan	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stdrpmat	, 0, this.sticsCrop.P_stdrpmat	, 0, 	30); 
		System.arraycopy(zone.getCropSpecies().P_stdrpdes	, 0, this.sticsCrop.P_stdrpdes	, 0, 	30); 


		//JNA NATIVE method to check plant and itk parameters
		//result is in output/initialisation.sti
		jna.verifPlante(sticsParam, sticsTransit, this.sticsCommun, zone.getSticsItk(), this.sticsCrop, zone.getId(), exportDir);
		
		//update cropAge
		this.cropAge++;
		
		return;
	}
	
	/**
	* STICS initialization copy for all cells with the same crop species 
	*/
	public void cropInitialisationCopy (SafeCrop initialCrop) {
				
		this.sticsSoil 		= new SafeSticsSoil(initialCrop.sticsSoil);
		this.sticsCommun 	= new SafeSticsCommun(initialCrop.sticsCommun);	
		this.sticsCrop 		= new SafeSticsCrop(initialCrop.sticsCrop);
		this.setCropSpeciesName(initialCrop.getCropSpeciesName());
	}
	/**
	 * FORCING LAI if laiFileName != "" (hisafe.par) 
	 **/
	public void forceLai (int year, int dayStart, int dayEnd, boolean isLeap) {
		
		int julianDay = dayStart;
		int index = 2;
		float maxLai = 0;
		int j = 2;

		for (int i=dayStart; i<dayEnd; i++) {
			
			//leap year 
			if (isLeap) {
				if (julianDay>366) {
					julianDay = 1; 
					year++;
				}			
			}
			else {
				if (julianDay>365) {
					julianDay = 1; 
					year++;
				}			
			}

			SafeSticsLai r = getLaiObs(year, julianDay);

			int indice          = ((index-1)*3)+1;
			if (indice < 1099) {
				this.sticsCrop.lai[indice]	= (float) r.getLai();
				this.sticsCrop.lai[indice+1]= this.sticsCrop.lai[indice];
				this.sticsCrop.lai[indice+2]= this.sticsCrop.lai[indice];
				if (this.sticsCrop.lai[indice] > maxLai) {
					maxLai= this.sticsCrop.lai[indice];
					this.sticsCrop.nlaxobs=j;
				}
				index++; 			
			}	
			julianDay++;
			j++;
		}

	}
	/**
	 * TREE LITTER SOIL INCORCOPATION  (Stump, Leaves, Branches, Fruits, Fine roots, Coarse roots)  
	 * CALL STICS process "apport" (to add litters to the soil mineralization)
	 **/
	public void soilIncorporation (SafeTestJNA safeJNA, 
								SafeSticsParameters sticsParam, 
								SafeEvolutionParameters evolutionParameters,
								SafeCell c,		
								int julianDay,
							    double humificationDepth, 
							    double treeRootsDepth,
							    double treeCarbonFoliageLitter, 
							    double treeNitrogenFoliageLitter,
							    double treeCarbonBranchesLitter, 
							    double treeNitrogenBranchesLitter,
							    double treeCarbonFruitLitter, 
							    double treeNitrogenFruitLitter) {

		//TREE FOLIAGE LITTER INCORPORTION 
		if (treeCarbonFoliageLitter > 0) {

			int typeLitter = 10;
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;	
			float freshMatterLitter = (float) (treeCarbonFoliageLitter / 1000 / cfeupc);	//convert kg C ha-1 in T MS ha-1
			float cnLitter = (float) (treeCarbonFoliageLitter/ treeNitrogenFoliageLitter);
			float profMax = 1.0f;
			
			//if soil management litter goes deeper 
			if (this.getSoilManagementDepth(julianDay) != 0) {
				typeLitter = 20;
				profMax = (float) (Math.min(this.getSoilManagementDepth(julianDay),humificationDepth)*100);
			}

			//CALL STICS METHOD TO ADD LITTER INTO THE SOIL MINERALISATION
			safeJNA.apport(sticsParam, 
					this.sticsSoil,
					this.sticsCommun, 
					this.sticsCrop,
					this.getCell().getCropZone().getSticsItk(),
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,		//C content of residu  (% MF)
					waterLitter,		//Water content of residue   %FM
					typeLitter			//litter type
					);
			
			//STICS CUMULS FOR BILAN
			this.sticsCommun.treeCarbonFoliageLitter = this.sticsCommun.treeCarbonFoliageLitter + (float) treeCarbonFoliageLitter;      
			this.sticsCommun.treeNitrogenFoliageLitter = this.sticsCommun.treeNitrogenFoliageLitter + (float) treeNitrogenFoliageLitter; 
		      
		      
		}
		
		
		//TREE BRANCHES LITTER INCORPORTION 		
		if (treeCarbonBranchesLitter > 0) {
	
			int typeLitter = 8;		//grapevine shoots on surface
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;	
			float freshMatterLitter = (float) (treeCarbonBranchesLitter / 1000 / cfeupc);	//convert kg C ha-1 in T MS ha-1
			float cnLitter = (float) (treeCarbonBranchesLitter/ treeNitrogenBranchesLitter);
			float profMax = 1.0f;
				
			//if soil management litter goes deeper 
			if (this.getSoilManagementDepth(julianDay) != 0) {
				typeLitter = 18;
				profMax = (float) (Math.min(this.getSoilManagementDepth(julianDay),humificationDepth)*100);
			}
			
			//CALL STICS METHOD TO ADD LITTER INTO THE SOIL MINERALISATION
			safeJNA.apport(sticsParam, 
					this.sticsSoil,
					this.sticsCommun, 
					this.sticsCrop,
					this.getCell().getCropZone().getSticsItk(),
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  T MS ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,		//C content of residu  (% MF)
					waterLitter,		//Water content of residue   %FM
					typeLitter			//litter type
					);
			
			//STICS CUMULS FOR BILAN
			this.sticsCommun.treeCarbonBranchesLitter = this.sticsCommun.treeCarbonBranchesLitter + (float) treeCarbonBranchesLitter;      
			this.sticsCommun.treeNitrogenBranchesLitter = this.sticsCommun.treeNitrogenBranchesLitter + (float) treeNitrogenBranchesLitter; 			
		}
				
		
		
		//TREE FRUIT LITTER INCORPORTION 
		if (treeCarbonFruitLitter > 0) {

			int typeLitter = 6;		//vinasse on surface
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;	
			float freshMatterLitter = (float) (treeCarbonFruitLitter / 1000 / cfeupc);	//convert kg C ha-1 in T MS ha-1
			float cnLitter = (float) (treeCarbonFruitLitter/ treeNitrogenFruitLitter);
			float profMax = 1.0f;
				
			//if soil management litter goes deeper 
			if (this.getSoilManagementDepth(julianDay) != 0) {
				typeLitter = 16;
				profMax = (float) (Math.min(this.getSoilManagementDepth(julianDay),humificationDepth)*100);
			}
			
			//CALL STICS METHOD TO ADD LITTER INTO THE SOIL MINERALISATION
			safeJNA.apport(sticsParam, 
					this.sticsSoil,
					this.sticsCommun, 
					this.sticsCrop,
					this.getCell().getCropZone().getSticsItk(),
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,		//C content of residu  (% MF)
					waterLitter,		//Water content of residue   %FM
					typeLitter			//litter type
					);
			
			//STICS CUMULS FOR BILAN
			this.sticsCommun.treeCarbonFruitLitter = this.sticsCommun.treeCarbonFruitLitter + (float) treeCarbonFruitLitter;      
			this.sticsCommun.treeNitrogenFruitLitter = this.sticsCommun.treeNitrogenFruitLitter + (float) treeNitrogenFruitLitter; 			
		}
		
		
				
		//FINE ROOTS AND COARSE LITTER INCORPORTION 
		double treeCarbonFineRootsLitter = 0;
		double treeNitrogenFineRootsLitter = 0;
		double treeCarbonCoarseRootsLitter = 0;
		double treeNitrogenCoarseRootsLitter = 0;


		//FOR EACH VOXEL
		SafeVoxel [] voxels = c.getVoxels();
		for (int i = 0; i < voxels.length; i++) {
			
			SafeVoxel v = voxels[i];
			double voxelBottom = v.getZ()+(v.getThickness()/2);

			
			//VOXEL ABOVE PROFHUM 
			if (voxelBottom <= humificationDepth) {
				treeCarbonCoarseRootsLitter += (v.getTotalTreeCarbonCoarseRootsSen());//in kg
				treeNitrogenCoarseRootsLitter +=(v.getTotalTreeNitrogenCoarseRootsSen());
				treeCarbonFineRootsLitter += (v.getTotalTreeCarbonFineRootsSen());
				treeNitrogenFineRootsLitter += (v.getTotalTreeNitrogenFineRootsSen());
			}

	
		}	

		//ROOT LITTERS
		treeCarbonFineRootsLitter = treeCarbonFineRootsLitter  / (c.getArea() / 10000); // convert kg C in kg C ha-1	
		treeCarbonCoarseRootsLitter = treeCarbonCoarseRootsLitter  / (c.getArea() / 10000); // convert kg C in kg C ha-1		
		treeNitrogenCoarseRootsLitter = treeNitrogenCoarseRootsLitter  / (c.getArea() / 10000); // convert kg C in kg C ha-1		
		treeNitrogenFineRootsLitter = treeNitrogenFineRootsLitter  / (c.getArea() / 10000); // convert kg C in kg C ha-1		
		this.getCell().setTreeCarbonFineRootsLitter (treeCarbonFineRootsLitter);		
		this.getCell().setTreeCarbonCoarseRootsLitter (treeCarbonCoarseRootsLitter);
		this.getCell().setTreeNitrogenFineRootsLitter (treeNitrogenFineRootsLitter);
		this.getCell().setTreeNitrogenCoarseRootsLitter (treeNitrogenCoarseRootsLitter);		
	
		
		//TREE FINE ROOTS LITTER INCORPORTION 
		if (treeCarbonFineRootsLitter > 0) {

			int typeLitter = 21;
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;
			float freshMatterLitter = (float) (treeCarbonFineRootsLitter / 1000/ cfeupc); //convert kg C ha-1 in T MS ha-1
			float cnLitter = (float) (treeCarbonFineRootsLitter/treeNitrogenFineRootsLitter);	
			float profMax = (float) (humificationDepth * 100);


			//CALL STICS METHOD TO ADD LITTER INTO THE SOIL MINERALISATION
			safeJNA.apport(sticsParam, 
					this.sticsSoil,
					this.sticsCommun,
					this.sticsCrop,
					this.getCell().getCropZone().getSticsItk(),
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,		//C content of residu  (% MF)
					waterLitter,		//Water content of residue   %FM
					typeLitter			//litter type
					);

			
			//STICS CUMULS FOR BILAN
			this.sticsCommun.treeCarbonFineRootsLitter = this.sticsCommun.treeCarbonFineRootsLitter + (float) treeCarbonFineRootsLitter;      
			this.sticsCommun.treeNitrogenFineRootsLitter = this.sticsCommun.treeNitrogenFineRootsLitter + (float) treeNitrogenFineRootsLitter; 
			
		}
		
		//TREE COARSE ROOTS LITTER INCORPORTION 
		if (treeCarbonCoarseRootsLitter > 0) {

			int typeLitter = 21;			
			float waterLitter = (float) -1.e-10;
			//0.5 is forced carbon content just to provide STICS with a value of fresh matter
			float cfeupc = 0.5f;
			float freshMatterLitter = (float) (treeCarbonCoarseRootsLitter/ 1000 / cfeupc); //convert kg C ha-1 in T MS ha-1
			float cnLitter = (float) (treeCarbonCoarseRootsLitter/ treeNitrogenCoarseRootsLitter);
			float profMax = (float) humificationDepth;	
			
			//call JNA native method
			safeJNA.apport(sticsParam, 
					this.sticsSoil,
					this.sticsCommun, 
					this.sticsCrop,
					this.getCell().getCropZone().getSticsItk(),
					profMax,
					freshMatterLitter, 	//Fresh matter (FM) added from residue ires  t.ha-1
					cnLitter,			//C/N ratio of residu
					cfeupc * 100,		//C content of residu  (% MF)
					waterLitter,		//Water content of residue   %FM
					typeLitter			//litter type
					);
					
								//STICS CUMULS FOR BILAN
			this.sticsCommun.treeCarbonFineRootsLitter = this.sticsCommun.treeCarbonFineRootsLitter + (float) treeCarbonCoarseRootsLitter;      
			this.sticsCommun.treeNitrogenFineRootsLitter = this.sticsCommun.treeNitrogenFineRootsLitter + (float) treeCarbonCoarseRootsLitter; 
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
							    SafeGeneralParameters safeSettings,
							    SafeEvolutionParameters evolutionParameters,
							    int simulationYear, 
								int julianDay, 
								int sticsDay,
							    double cellRad, 
							    double cellRain,
							    double cellEtp,
							    double cellVisibleSky,
							    int flagFirst
								) {
		
		
		//call JNA native method
		safeJNA.boucleJour1(sticsParam, 
				sticsTransit, 
				sticsStation, 
				this.sticsClimat,
				this.sticsCommun, 
				this.sticsSoil,
				this.sticsCrop,
				this.getCell().getCropZone().getSticsItk(),
				sticsDay, 
				julianDay,
				cellRad,
				cellRain,
				cellEtp,
				cellVisibleSky,
				flagFirst);


		//variable storage
		if (this.startDay == 0) this.startDay = julianDay;
		this.sticsDay 		 = sticsDay;			    //DOY

		if (this.sticsCrop.zrac == 0) this.rootsDepth = 0;
		if (this.rootsDepth < (this.sticsCrop.zrac / 100))
			this.rootsDepth = this.sticsCrop.zrac / 100;			//convert cm in m
			
		////real      :: lai(0:2,0:366)
		//attention il faudra changer l'indice  si on passe aux cultures associées	
		int indice          = (sticsDay*3)+1;
		this.lai 			= this.sticsCrop.lai[indice]; 
		this.deltai 		= this.sticsCrop.deltai[indice];
		this.durvie 		= this.sticsCrop.durvie[indice];
		this.laisen 		= this.sticsCrop.laisen[indice];
		this.biomass 		= this.sticsCrop.masec[indice];			//t ha-1		
		this.grainBiomass 	= this.sticsCrop.magrain[indice]/100;	//convert g m-2 in t ha-1

		this.tmin 			= this.sticsClimat.tmin[sticsDay-1];
		this.tmax 			= this.sticsClimat.tmax[sticsDay-1];
		
		int indice2          = ((sticsDay-1)*3)+1;
		this.qNplante 		= this.sticsCrop.QNplante[indice2];		//kgN.ha-1
		
		//Nitrogen sink strength with nitrogen demand of the day before
		if (this.getNitrogenDemand() > 0)
			this.getPlantRoots().calculateNitrogenSinkStrength (safeSettings, this.getNitrogenDemand());

		//cumulation of Rain transmitted on the crop
		this.annualRain		+= cellRain;

		this.codecaltemp = sticsStation.P_codecaltemp;

		return;
	}


	/**
	 * STICS CROP process growth part II (after water repartition)
	 **/
	public void processGrowth2 (SafeTestJNA safeJNA, 
									SafeSticsParameters sticsParam, 
									SafeSticsTransit sticsTransit, 
									SafeSticsStation sticsStation, 
									SafeStand stand,
								    int simulationYear, 
									int julianDay, 
									int sticsDay,	
									int hisafeInfluence,
								    double cellVisibleSky
									) {

		//call JNA native method	
		safeJNA.boucleJour2 (sticsParam, sticsTransit, 
				sticsStation, 
				this.sticsClimat,
				this.sticsCommun, 
				this.sticsSoil,
				this.sticsCrop,
				this.getCell().getCropZone().getSticsItk(),
				sticsDay, 
				julianDay,
				hisafeInfluence,
				cellVisibleSky
			);
					
		//important variable storage
		this.harvestDay = 0;

		if (this.sticsCrop.nrec > 0)
			this.harvestDay = this.sticsCrop.nrec + this.startDay;	

		if (this.sticsCrop.zrac == 0) this.rootsDepth = 0;
		if (this.rootsDepth < (this.sticsCrop.zrac / 100))
			this.rootsDepth = this.sticsCrop.zrac / 100;			//convert cm in m
		
		this.sticsDay       = sticsDay;

		this.soilManagementDepth = (float) this.getSoilManagementDepth(julianDay);
		
		////real      :: eai(0:2) 
		//attention il faudra changer l'indice  si on passe aux cultures associées
		this.eai			= this.sticsCrop.eai[1];		  
		this.height 		= this.sticsCrop.hauteur[1];
	
	
		this.interceptedPar = this.sticsCrop.raint[1]; 		// MJ m-2


		//attention il faudra changer l'indice  si on passe aux cultures associées	
		////real      :: lai(0:2,0:366)
		int indice          = (sticsDay*3)+1;
		this.lai 			= this.sticsCrop.lai[indice]; 
		this.biomass 		= this.sticsCrop.masec[indice];			//t ha-1		
		this.grainBiomass 	= this.sticsCrop.magrain[indice]/100;	//convert g m-2 in t ha-1
		this.biomassIncrement 	= this.sticsCrop.dltams[indice];	//t ha-1
		this.ircarb 		= this.sticsCrop.ircarb[indice];		//Carbon harvest index // g  grain g plant-1
		int indice2          = ((sticsDay-1)*3)+1;
		this.qNplante 		= this.sticsCrop.QNplante[indice2];		//kgN.ha-1
		

		//in STICS remontee is negative
		if (this.sticsSoil.remontee!=0) this.capillaryRise	 = -(this.sticsSoil.remontee);			    // mm;
		this.irrigation 	 = this.sticsCommun.airg[sticsDay-1];		// mm
	
		//NITROGEN ENTRIES (cumulated values in kg ha-1 )
		this.nitrogenIrrigation 			= this.sticsCommun.irrigN;			
		this.nitrogenFertilisationMineral 	= this.sticsCommun.totapN;	
		this.nitrogenFertilisationOrganic 	= this.sticsCommun.QNresorg;

		this.nitrogenHarvested = this.sticsCrop.Nexporte ;		
		this.biomassHarvested = this.sticsCrop.MSexporte; 
		

		//residus
		this.cropCarbonLeafLitter = this.sticsCrop.QCplantetombe[1] + this.sticsCrop.QCrogne + this.sticsCrop.QCressuite;
		this.cropNitrogenLeafLitter = this.sticsCrop.QNplantetombe[1] + this.sticsCrop.QNrogne + this.sticsCrop.QNressuite;
		this.cropCarbonRootsLitter = this.sticsCrop.QCrac;
		this.cropNitrogenRootsLitter = this.sticsCrop.QNrac;


        //SET phenologicStageVegetative
		if  (this.getCell().getCropZone().getSticsItk().P_iplt0 == julianDay) {		//plt
			phenologicStageVegetative = 2;
			this.sowingDay = julianDay;
			this.sticsCrop.densite = this.getCell().getCropZone().getSticsItk().P_densitesem;			//on remet la densite de semis
		}	
		if  ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.ndebdorm > 0) && (phenologicStageVegetative < 3))	{		//debdorm
			phenologicStageVegetative = 3;
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;
		}	
		if  ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nger > 0)&& (phenologicStageVegetative < 4))	{		//ger
			phenologicStageVegetative = 4;
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}	
		if  ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nlev > 0)&& (phenologicStageVegetative < 5))	{		//lev
			phenologicStageVegetative = 5;
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;	
		}		
		if  ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.namf > 0)&& (phenologicStageVegetative < 6))	{		//amf
			phenologicStageVegetative = 6;		
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}		
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nlax > 0)&& (phenologicStageVegetative < 7))	{		//lax
			phenologicStageVegetative = 7;	
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}	
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nsen > 0)&& (phenologicStageVegetative < 8))	{		//sen
			phenologicStageVegetative = 8;		
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}
		//sometimes SEN is not SET by STICS, we force it by testing dltaisen >0 after LAX
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nlax > 0) && (this.sticsCrop.dltaisen[1] > 0) && (cptDaysStageVegetative>0) && (phenologicStageVegetative < 8))	{		//sen
			phenologicStageVegetative = 8;		
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}
		
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nlan > 0)&& (phenologicStageVegetative < 9))	{		//lan
			phenologicStageVegetative = 9;		
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}	
		//sometimes LAN is not SET by STICS, we force it by testing lai = 0 after SEN
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nsen > 0) &&  (this.getLai() == 0) && (phenologicStageVegetative < 9))	{		//lan
			phenologicStageVegetative = 9;		
			cptDaysStageVegetative=0;
			sumHisafeWaterStressVegetative=0;
			sumHisafeNitrogenStressVegetative=0;			
		}
		
	    //SET phenologicStageReproductive
		if ((this.sticsCrop.nrec == 0) && (this.sticsCrop.nflo > 0))	{		//flo
			phenologicStageReproductive = 2;			
		}		
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.ndrp > 0) && (phenologicStageReproductive < 3))	{		//drp
			phenologicStageReproductive = 3;			
			cptDaysStageReproductive=0;
			sumHisafeWaterStressReproductive=0;
			sumHisafeNitrogenStressReproductive=0;			
		}		
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nnou > 0) && (phenologicStageReproductive < 4))	{		//nou
			phenologicStageReproductive = 4;	
			cptDaysStageReproductive=0;
			sumHisafeWaterStressReproductive=0;
			sumHisafeNitrogenStressReproductive=0;				
		}
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.ndebdes > 0) && (phenologicStageReproductive < 5))	{		//debdes
			phenologicStageReproductive = 5;
			cptDaysStageReproductive=0;
			sumHisafeWaterStressReproductive=0;
			sumHisafeNitrogenStressReproductive=0;				
		}	
		if ((this.sticsCrop.nrec == 0) &&(this.sticsCrop.nmat > 0) && (phenologicStageReproductive < 6))	{		//mat
			phenologicStageReproductive = 6;
			cptDaysStageReproductive=0;
			sumHisafeWaterStressReproductive=0;
			sumHisafeNitrogenStressReproductive=0;				
		}
	
	    //HARVEST 
		if (this.sticsCrop.nrec > 0) {		//rec
			if (phenologicStageVegetative == 10)	{	//back to 1 the day after rec 	
				phenologicStageVegetative = 1;	
				phenologicStageReproductive = 1;
			}
			else if (phenologicStageVegetative > 1){
				phenologicStageVegetative = 10;	
				phenologicStageReproductive = 7;
				this.yield  		= this.sticsCrop.magrain[indice]/100;
				cptDaysStageVegetative=0;
				sumHisafeWaterStressVegetative=0;
				sumHisafeNitrogenStressVegetative=0;
			}
		}

		
		//YIELD for PERENIAL CROPS 
		//we took the day before because masec of the day is AFTER the cut 
		if ((this.isPerennial()) && (this.sticsCrop.sioncoupe)) {
			int indicePrev      = ((sticsDay-1)*3)+1;
			this.yield  		= this.sticsCrop.masec[indicePrev];
		}

		//After planting we start compting stress days 
		if ((phenologicStageVegetative > 1) && (phenologicStageVegetative < 10)){
			cptDaysStageVegetative++;
			sumHisafeWaterStressVegetative+=hisafeWaterStress;
			sumHisafeNitrogenStressVegetative+=hisafeNitrogenStress;
			cptDaysStageReproductive++;
			sumHisafeWaterStressReproductive+=hisafeWaterStress;
			sumHisafeNitrogenStressReproductive+=hisafeNitrogenStress;
		}

		if ((this.sowingDay > 0) && (this.harvestDay == 0)) this.cropAge = this.cropAge+1; 
		
		//TOTALS
		this.annualIrrigation 		+= this.irrigation;
		this.annualWaterDemand 		+= this.getWaterDemand();
		this.annualWaterDemandReduced += this.waterDemandReduced;
		this.annualWaterUptake 		+= this.waterUptake;
		this.annualNitrogenDemand	+= this.getNitrogenDemand();
		this.annualNitrogenUptake 	+= this.nitrogenUptake;
		this.annualSoilEvaporation 	+= this.getSoilEvaporation();
		this.annualRunOff 			+= this.getRunOff();
		this.annualSurfaceRunOff 	+= this.getSurfaceRunOff();
		this.annualDrainageBottom 	+= this.sticsCommun.drain;
		this.annualDrainageArtificial += this.sticsSoil.qdrain;
		this.annualCapillaryRise 	+= this.capillaryRise;
		this.annualNitrogenLeachingBottom += this.sticsCommun.lessiv;
		this.annualNitrogenLeachingWaterTable += this.nitrogenLeachingWaterTable; 

	}
	/**
	 * Store values at the end of a rotation
	 * These values will be used to initialized the next rotation
	 **/
	public void storeValues (int sticsDay) {
		

		
		double QNplante_fin;
		int indice  = (sticsDay*3)+1;
		QNplante_fin = this.sticsCrop.QNplante[indice];
        if (this.sticsCrop.P_codebfroid == 3 &&  this.sticsCrop.P_codedormance == 3) {
            QNplante_fin = this.sticsCrop.QNplante[(this.sticsCrop.ntaille-1*3) + this.sticsCommun.AOAS]    
                    - (this.sticsCrop.mabois[this.sticsCommun.AS] * 0.5 * 10.0)              
                    - (this.sticsCrop.mabois[this.sticsCommun.AO] * 0.5 * 10.0)              
                    - this.sticsCrop.Qngrain_ntailleveille;
        }

		this.sticsCrop.P_lai0 = this.sticsCrop.lai[indice];
		this.sticsCrop.P_masec0 = this.sticsCrop.masec[indice];
		this.sticsCrop.P_zrac0 = this.sticsCrop.zrac;
		this.sticsCrop.P_QNplante0 = (float) QNplante_fin;
		for (int iz=0; iz<5; iz++) {
			this.sticsCrop.P_densinitial[iz] = this.sticsCrop.LRACH[iz];
		}
	

		
		this.sticsCrop.P_resperenne0 = this.sticsCrop.resperenne[this.sticsCommun.AOAS];
		this.sticsCommun.cu0[0] = this.sticsCrop.cu[sticsDay];
		this.sticsCommun.somelong0[0] = this.sticsCrop.somelong;
        if (this.sticsCommun.cu0[0] == 0) 
        	this.sticsCommun.nfindorm0[0] = this.sticsCrop.nfindorm;
        else
        	this.sticsCommun.nfindorm0[0] = 0;
        
        this.sticsCommun.Nb0 = this.sticsCommun.Nb;
        this.sticsCommun.Cb0 = this.sticsCommun.Cb;
        this.sticsCommun.Nr0 = this.sticsCommun.Nr;
        this.sticsCommun.Cr0 = this.sticsCommun.Cr;
        this.sticsCommun.Cmulch0 = this.sticsCommun.Cmulchdec + this.sticsCommun.Cmulchnd;
        this.sticsCommun.Nmulch0 = this.sticsCommun.Nmulchdec + this.sticsCommun.Nmulchnd;
        this.sticsCommun.Cbmulch0 = this.sticsCommun.Cbmulch;
        this.sticsCommun.Nbmulch0 = this.sticsCommun.Nbmulch;
        
        this.sticsCommun.tcult = this.sticsCommun.tcultveille;
	}
	
	/**
	* Return the soil management depth for today in meters
	**/
	protected float getSoilManagementDepth (int simulationDate) {
		int nbSoilManagement = this.getCell().getCropZone().getSticsItk().P_nbjtrav;
		for (int i=0; i<nbSoilManagement; i++) {
			if (this.getCell().getCropZone().getSticsItk().P_jultrav[i] == simulationDate)  // soil management = today
				return (this.getCell().getCropZone().getSticsItk().P_proftrav[i] / 100);	// convert cm to m
		}
		return 0;
	}

	/**
	* Compute CROP total root length 
	**/
	public double computeTotalRootsLength () {
	
		double cropRootLength = 0;
		SafeVoxel [] voxels = getCell().getVoxels();
		
		//Crop total root length
		for (int i=0; i<voxels.length ; i++) {		// first iterator to compute totalRootLength
			cropRootLength += voxels[i].getCropRootsDensity() * voxels[i].getVolume();			// m.m-3 * m3 = m
		}
		this.getPlantRoots().setTotalRootsLength (cropRootLength);	
		return cropRootLength;
	}
		

	/**
	* Compute CROP plant water potential 
	**/
	public void computePlantWaterPotential (SafeGeneralParameters settings) {
		
		if (this.getTotalRootsLength() <= 0) return;
		
		SafeVoxel [] voxels = getCell().getVoxels();
		
		double plantPotential = 0;

		double drySoilFactor = this.getCell().getCropZone().getCropSpecies().getCropHarmonicWeightedMean();
			
		
		//plant water potential 
		for (int i=0; i<voxels.length ; i++) {		
			
			if (voxels[i].getCropRootsDensity() > 0) { //IL 14/08/2020 add this test
				double neededPot = voxels[i].getWaterPotentialTheta();	// soil water potential in this voxel  m3 m-3
	
				// additional potential for water flow from bulk soil to rhizosphere
				neededPot *= (1+this.getCell().getCropZone().getCropSpecies().getCropBufferPotential()); 

				
				// additional potential for water flow from root surface to xylem
				double radialTransportPotential = -this.getWaterDemand()								// L.day-1=dm3.day-1
													* 1000												// from L.day-1 to cm3.day-1
													/this.getCell().getCropZone().getCropSpecies().getCropRootConductivity()			// cm day-1
													/(this.getTotalRootsLength()	/this.getCell().getArea()*100);							// m to cm
													//we divide by cellArea to normalize total root lenght CD+IL 20/12/2023

	
				this.getPlantRoots().setRadialTransportPotential(radialTransportPotential);
				neededPot += radialTransportPotential;
				
				// additional potential to account for longitudinal water transport in coarse roots from the voxel to stem base
				double longitudinalTransportPotential = -this.getWaterDemand()										//L.day-1=dm3.day-1
															* 1000													// from L.day-1 to cm3.day-1
															* this.getCell().getCropZone().getCropSpecies().getCropLongitudinalResistantFactor()	// day.cm-1.m-1
															/(this.getTotalRootsLength()/this.getCell().getArea()*100);									// m to cm
															//we divide by cellArea to normalize total root lenght CD+IL 20/12/2023

				// in the model documentation from Meine et al, this term is not divided by cropRootLength... 
				//but it leads to very different longitudinal drop potential for small and large trees because of differences in water demand... so...
	
				this.getPlantRoots().setLongitudinalTransportPotential(longitudinalTransportPotential);
				neededPot += longitudinalTransportPotential*voxels[i].getZ();	// topological distance (m) between the voxel and stem base
	
				// additional potential to account for voxel depth
				neededPot -= voxels[i].getZ()*100;		// from m to cm
	
				plantPotential += -(voxels[i].getCropRootsDensity() * voxels[i].getVolume()	// m.m-3 * m3 = m
										/ Math.pow(-neededPot, drySoilFactor));	// cm		
				
			}
		}
		
		plantPotential =-Math.pow (-this.getTotalRootsLength() /plantPotential , 1/drySoilFactor);
		this.getPlantRoots().setRequiredWaterPotential(plantPotential);

	}

	

	/**
	* Crop light interception values from STICS 
	* USED if cropLightMethod = 0 (crop interception calculated by STICS) 
	**/
	public void cropSticsLightInterception (SafeGeneralParameters settings, SafeBeamSet<SafeBeam> beamSet, double directProp, double diffuseProp){
		float lai 	= this.getLai();
		float eai 	= this.getEai();
		float extin = this.getExtin();
		float raint = (float) (0.95*(1-Math.exp(-extin*(lai+eai))));
		this.setCaptureFactorForDirectPar((float) (raint*directProp));
		this.setCaptureFactorForDiffusePar((float) (raint*diffuseProp));	
	}
	
	/**
	* Optimization of light extinction coefficient to be coherent with Stics
	* USED if cropLightMethod = 1 (crop interception calculated by Hi-sAFe) 
	**/
	public void findCropLightCoef(SafeBeamSet<SafeBeam> beamSet, SafeGeneralParameters settings, SafeDailyClimat climat){

		// % of Par intercepted by monocrop with Stic's formalism
		double toBeIntercepted =1-Math.exp(-(this.getLai()+this.getEai())*this.getExtin());

		// weights of direct and diffuse Par
		double dailyDiffuse = climat.getDiffusePar()/climat.getGlobalPar();
		double dailyDirect = climat.getDirectPar()/climat.getGlobalPar();

		if(toBeIntercepted!=0){
			// initialisation of k
			double kEst=this.getExtin();

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
									* (1-Math.exp(-kEst*(this.getLai()+this.getEai())/Math.sin(b.getHeightAngle_rad())));
					fEstPrime +=(dailyDiffuse*b.getDiffuseEnergy()+dailyDirect*b.getDirectEnergy())
								*(this.getLai()+this.getEai())/Math.sin(b.getHeightAngle_rad())
								*Math.exp(-kEst*(this.getLai()+this.getEai())/Math.sin(b.getHeightAngle_rad()));
				}
				kOld=kEst;
				kEst += -fEst/fEstPrime;
			}

			setParExtinctionCoef (kEst);
		}
	}

	/**
	* Updating the results of light interception with daily climat
	**/
	public void updateDailyInterceptedPar (SafeDailyClimat dailyClimat,
											SafeGeneralParameters settings,
											SafeBeamSet<SafeBeam> beamSet){

		if (this.getLai()+this.getEai() > 0){
			
			// Climatic input
			float dailyDiffuse = dailyClimat.getDiffusePar();	// moles m-2
			float dailyDirect = dailyClimat.getDirectPar();		// moles m-2

			// Topological mask
			float diffuseMask = (float) beamSet.getSkyDiffuseMask();
			float directMask = (float) beamSet.getSkyDirectMask();


			setDiffuseParIntercepted(getCaptureFactorForDiffusePar()*dailyDiffuse);	//moles.m-2
			setDirectParIntercepted(getCaptureFactorForDirectPar()*dailyDirect);	//moles.m-2

			float parIntercepted = getDirectParIntercepted()+getDiffuseParIntercepted(); //moles.m-2

			// Computation of a competition index for Par =
			//		(TotalParIntercepted)/(Par intercepted by the same crop in monoculture)
			float incidentPar = dailyDiffuse*diffuseMask+dailyDirect*directMask;
			if((this.getLai()+this.getEai()) > 0){
				float competitionIndex = (float) (parIntercepted	// Par intercepted (moles.m-2)
								/(0.95*(1-Math.exp(-this.getExtin()*(this.getLai()+this.getEai())))	// % of Par intercepted by monocrop
								*incidentPar));						// daily incident Par (Moles m-2)
				setCompetitionIndexForTotalPar(competitionIndex);

			} else {
				setCompetitionIndexForTotalPar(1);
			}
		}
	}
	
	/**
	* reset capture factors for light
	**/
	public void resetDirect (){captureFactorForDirectPar = 0;}
	public void resetDiffuse (){captureFactorForDiffusePar = 0;}
	
	/**
	 * ADD the LAI for a  day in the map
	 */
	public void addLaiMap (SafeSticsLai l) {
		if (laiMap==null) laiMap = new Hashtable ();
		String key    = l.getYear()+"|"+l.getJulianDay();
		laiMap.put (key, l);
	}

	/**
	 * Return the LAI observed for a julian day
	 */
	public SafeSticsLai getLaiObs  (int y, int d)  {

		String julian = new Integer(d).toString();
		String year = new Integer(y).toString();
		String key = year+"|"+julian;
		if (d == 0) return null; 
		SafeSticsLai s = (SafeSticsLai) laiMap.get (key);
		if (s != null) 	return s;
		else {
			
			System.out.println("Unrecognized lai in LAI file : "+key);
			System.exit(1);

		}
		return null;
	}
	
	
	public SafeSticsCrop getSticsCrop() {return sticsCrop;}

	public SafeSticsSoil getSticsSoil() {return sticsSoil;}
	
	public SafeSticsCommun getSticsCommun () {return sticsCommun;}
	
	public SafeSticsClimat getSticsClimat() {return sticsClimat;}
	
	public SafePlantRoot getPlantRoots() {return plantRoots;}
	
	public int getCropAge () {return cropAge ;}
	public String getCropSpeciesName () {return cropSpeciesName;}
	public void setCropSpeciesName (String s) {cropSpeciesName = s;}
	
	public SafeCell getCell() {return cell;}
	public int getSowingDay () {return sowingDay;}
	public int getHarvestDay () {return harvestDay;}
	public int getSticsDay () {return sticsDay;}
	public int getPhenologicStageVegetative () {return phenologicStageVegetative;}
	public int getPhenologicStageReproductive () {return phenologicStageReproductive;}
	
	public boolean isPerennial () {
		if (this.sticsCrop.P_codeperenne == 2) return true;
		else return false;
	} 

	public float getLai () {return lai;}
	public float getEai () {return eai;}
	public float getLaiMax () {return laiMax;}
	public float getEaiMax () {return eaiMax;}
	public float getExtin () {return this.sticsCrop.P_extin;}
	public float getAlbedoLai () {return this.sticsCommun.albedolai;}		//Albedo of the crop cobining soil with vegetation  SD	
	public float getBiomass () {return biomass;}
	public float getGrainBiomass () {return grainBiomass;}
	public float getFruitBiomass () {return this.sticsCrop.mafruit;}	//// Dry matter of harvested organs  // t ha-1
	public float getTuberBiomass () {return this.sticsCrop.matuber;} 	// Dry matter of harvested organs  t.ha-1
	public float getBiomassMax () {return biomassMax;}
	public float getBiomassIncrement() {return biomassIncrement;}
	

	public float getHeight() {return  height;}
	public float getHeightMax () {return heightMax;}
	
	public float getYield() {return  yield;}
	public float getYieldMax() {return  yieldMax;}
	
	public float getGrainNumber () {
		if (phenologicStageVegetative==1) return 0;
		return  this.sticsCrop.nbgrains[1];		// nbr m-2
	}
	public float getGrainWeight () {return  this.sticsCrop.pgrain[1];}		//g
	
	public float getGrainGrowthRate () {
		if (phenologicStageVegetative==1) return 0;
		return this.sticsCrop.dltags[1];	//Growth rate of the grains  // t ha-1.j-1
	}
 
	public float getPlantDensity () {
		return  this.sticsCrop.densite;		// nbr m-2
	}	
	
	public float getYieldIndice () {
		if(this.biomass == 0){
			return 0;
		} else {
			return this.yield/this.biomass;
		}
	}


	public float getRootsDepth () {return  rootsDepth;}
	public float getRootsDepthMax () {return  rootsDepthMax;}
	public float getCropTemperature () {return  this.sticsCommun.tcult;}			//degree C;
	public float getCropMaxTemperature () {return  this.sticsCommun.TcultMax;}
	public float getCropMinTemperature () {return  this.sticsCommun.TcultMin;}
	public float getSoilSurfaceTemperature () {return this.getSticsSoil().TS[0];}
	public float getSoilManagementDepth () {return soilManagementDepth;}	
	public float getSoilEvaporation () {return  this.sticsCommun.esol;}			//mm
	public float getRunOff () {return  this.sticsCommun.ruissel;}				//mm
	public float getSurfaceRunOff() {return  this.sticsCommun.ruisselsurf;}		//mm
	public float getCapillaryRise () {return Math.max(capillaryRise-capillaryRisePrev,0);}	
	public float getDrainageBottom () {return this.sticsCommun.drain;}			//Water flux drained at the base of the soil profile // mm j-1
	public float getDrainageArtificial () {return this.sticsSoil.qdrain;}	
	public float getIrrigation () {return  irrigation;}
	public float getQNgrain() {return this.sticsCrop.QNgrain[1];}		//Amount of nitrogen in harvested organs (grains / fruits)  kg ha-1
	public float getQNplante() {return qNplante;}
	public float getCNgrain() {return this.sticsCrop.CNgrain[1];}		//Nitrogen concentration of grains %
	public float getCNplante() {return this.sticsCrop.CNplante[1];}		//Nitrogen concentration of entire plant %
	public float getIrcarb() {return ircarb;}
	public float getGrainWaterContent() {return this.sticsCrop.teaugrain[1];}
	

	public double getTotalRootsLength () {
		if (this.getPlantRoots() != null)
			return this.getPlantRoots().getTotalRootsLength();
		else
			return 0;
	}
	

	//WATER AND NITROGEN BUDGET
	public float getWaterDemand () {return  this.sticsCrop.eop[1];}						//mm;
	public float getWaterDemandReduced() {return  waterDemandReduced;}
	public void setWaterDemandReduced (double v) { waterDemandReduced = (float) v;}
	public float getHisafeWaterStress () {return hisafeWaterStress;}
	public void setHisafeWaterStress (double v) {hisafeWaterStress = (float) v;}
	public float getHisafeTurfac () {return hisafeTurfac;}
	public void setHisafeTurfac (double v) {hisafeTurfac = (float) v;}
	public float getHisafeSenfac () {return hisafeSenfac;}
	public void setHisafeSenfac (double v) {hisafeSenfac = (float) v;}
	public float getWaterUptake() {return  waterUptake;}
	public void  setWaterUptake(double v) {waterUptake =  (float) v;}
	public void addWaterUptake  (double v) {waterUptake  +=  (float) v;}
	
	public float getMeanHisafeWaterStressVegetative () {
		if (cptDaysStageVegetative > 0)
		return sumHisafeWaterStressVegetative/cptDaysStageVegetative;
		else return 0;
	}
	public float getMeanHisafeWaterStressReproductive () {
		if (cptDaysStageReproductive > 0)
		return sumHisafeWaterStressReproductive/cptDaysStageReproductive;
		else return 0;
	}

	public float getSticsWaterStomatalStress() {return this.sticsCrop.swfac[1];}
	public float getSticsWaterTurgescenceStress() {return this.sticsCrop.turfac[1];}
	public float getSticsWaterSenescenceStress () {return this.sticsCrop.senfac[1];}
	public float getNitrogenDemand () {return this.sticsCrop.demande[1];}		//kg ha-1;
	public float getNitrogenUptake () {return nitrogenUptake;}
	public void setNitrogenUptake (double v) {nitrogenUptake =  (float) v;}
	public float getNitrogenRain () {return  this.sticsCommun.precipjN;}
	public float getHisafeNitrogenStress () {return hisafeNitrogenStress;}
	public void setHisafeNitrogenStress (double v) {hisafeNitrogenStress = (float) v;}

	public float getMeanHisafeNitrogenStressVegetative () {
		
		if (cptDaysStageVegetative > 0)
		return sumHisafeNitrogenStressVegetative/cptDaysStageVegetative;
		else return 0;
	}
	public float getMeanHisafeNitrogenStressReproductive () {
		if (cptDaysStageReproductive > 0)
		return sumHisafeNitrogenStressReproductive/cptDaysStageReproductive;
		else return 0;
	}
	
	public float getNitrogenFixation () {return  this.sticsCrop.offrenod[1];}
	public float getNitrogenDenitrification () {return  this.sticsSoil.Ndenit;}
	public float getBiomassRestitution () {return this.sticsCrop.qressuite;}			//??? gros doute la dessus IL 20/01/2017;
	public float getNitrogenLeachingBottom () {return  this.sticsCommun.lessiv;}
	public float getNitrogenLeachingArtificial () {return  this.sticsSoil.azlesd;}
	public float getNitrogenLeachingWaterTable() {return  nitrogenLeachingWaterTable;}
	public float getNitrogenAddedByWaterTable () {return  nitrogenAddedByWaterTable;}// AQ 11/04/2011
	public void addNitrogenAddedByWaterTable (double np) {nitrogenAddedByWaterTable +=np;}// AQ 11/04/2011
	public void addNitrogenLeachingWaterTable (double lix) {nitrogenLeachingWaterTable +=lix;}		
	public float getNitrogenImmobilisation () {return   this.sticsSoil.Norgeng;}
	public float getNitrogenVolatilisation () {return  this.sticsSoil.Nvoleng;}
	public float getNitrogenVolatilisationOrganic () {return  this.sticsSoil.Nvolorg;}
	public float getNitrogenIrrigation () {return  Math.max(nitrogenIrrigation- nitrogenIrrigationPrev,0);}
	public float getNitrogenFertilisationMineral () {return  Math.max(nitrogenFertilisationMineral - nitrogenFertilisationMineralPrev,0);}
	public float getNitrogenFertilisationOrganic () {return  Math.max(nitrogenFertilisationOrganic - nitrogenFertilisationOrganicPrev,0);}
	public float getNitrogenHumusMineralisation () {return  this.sticsSoil.cumvminh;}
	public float getNitrogenResiduMineralisation () {return  this.sticsSoil.cumvminr;}
	public float getCropCarbonLeafLitter () {return  Math.max(cropCarbonLeafLitter - cropCarbonLeafLitterPrev,0);}
	public float getCropNitrogenLeafLitter () {return  Math.max(cropNitrogenLeafLitter - cropNitrogenLeafLitterPrev,0);}
	public float getCropCarbonRootsLitter () {return  Math.max(cropCarbonRootsLitter - cropCarbonRootsLitterPrev,0);}
	public float getCropNitrogenRootsLitter () {return  Math.max(cropNitrogenRootsLitter - cropNitrogenRootsLitterPrev,0);}
	public float getNitrogenHarvested() {return  Math.max(nitrogenHarvested- nitrogenHarvestedPrev,0);}
	public float getBiomassHarvested () {return  Math.max(biomassHarvested- biomassHarvestedPrev,0);}
	public float getSaturation() {return  this.sticsCommun.saturation;}						//mm;
	


	public float getCarbonResidus() {return  this.sticsCommun.Cr;}				// Amount of C in the soil organic residues // kg.ha-1
	public float getNitrogenResidus() {return this.sticsCommun.Nr;}				//Amount of N remaining in the decaying organic residues in the soil  // kg.ha-1
	public float getCarbonMicrobialBiomass() {return this.sticsCommun.Cb;} 		// amount of C in the microbial biomass decomposing organic residues mixed with soil // kg.ha-1
	public float getNitrogenMicrobialBiomass() {return this.sticsCommun.Nb;} 	// Amount of N remaining in the biomass decaying organic residues // kg.ha-1
	public double getCarbonMicrobialBiomassMulch() {return this.sticsCommun.Cbmulch;}	// amount of C in the microbial biomass decomposing organic residues at soil surface (mulch) // kg.ha-1	
	public double getNitrogenMicrobialBiomassMulch() {return this.sticsCommun.Nbmulch;}	// amount of N in microbial biomass decomposing mulch // kg.ha-1
	
	public float getNitrogenResidus2() {return this.sticsCommun.Ntousresidusprofil;}		// total of Nitrogen from residues (all residues on P_profhum) // kgN.ha-1
	public float getTotalNitrogenHumusStock(){return this.sticsCommun.Nhumt;}	// Total quantity of N humus (active + inert fractions) in the soil // kg.ha-1
	public float getTotalCarbonHumusStock(){return this.sticsCommun.Chumt;}		// Total amount of C humus (active + inert fractions) in the soil // kg.ha-1
	public float getActiveCarbonHumusStock(){return  this.sticsCommun.Chuma;}
	public float getInactiveCarbonHumusStock() {return this.sticsCommun.Chumi;}	
	public float getActiveNitrogenHumusStock(){return  this.sticsCommun.Nhuma;}	// Amount of active nitrogen in the soil humus pool // kg.ha-1
	public float getInactiveNitrogenHumusStock() {return this.sticsCommun.Nhumi;}	
	public float getNitrogenLossNitrification() {return this.sticsCommun.em_N2O;}
	public float getCsurNressuite(){return this.sticsCrop.CsurNressuite;}
	public float getNitrogenNutritionIndex() {return this.sticsCrop.inn[1];}
	public float getSticsNitrogenLaiStress() {return this.sticsCrop.innlai[1];}
	public float getSticsNitrogenBiomassStress () {return this.sticsCrop.inns[1];}
	public float getSticsNitrogenSenescenceStress () {return this.sticsCrop.innsenes[1];}

	public float getSla(){return this.sticsCrop.sla[1];}					// cm2 g-1;
	public float getResperenne() {return this.sticsCrop.resperenne[1];}		//C crop reserve for perenial crops) t ha-1

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
	public float getAnnualCapillaryRise () {return  annualCapillaryRise;}
	public float getAnnualIrrigation () {return  annualIrrigation;}
	public float getAnnualWaterDemand () {return  annualWaterDemand;}
	public float getAnnualWaterDemandReduced() {return  annualWaterDemandReduced;}
	public float getAnnualWaterUptake () {return  annualWaterUptake;}
	public float getAnnualNitrogenDemand () {return  annualNitrogenDemand;}
	public float getAnnualNitrogenUptake () {return  annualNitrogenUptake;}
	public float getAnnualSoilEvaporation () {return  annualSoilEvaporation;}
	public float getAnnualRunOff () {return  annualRunOff;}
	public float getAnnualSurfaceRunOff () {return  annualSurfaceRunOff;}
	public float getAnnualDrainageBottom () {return  annualDrainageBottom;}
	public float getAnnualDrainageArtificial () {return  annualDrainageArtificial;}
	public float getAnnualNitrogenLeachingBottom () {return  annualNitrogenLeachingBottom;}
	public float getAnnualNitrogenLeachingWaterTable () {return  annualNitrogenLeachingWaterTable;}
	
	public float getAnnualRain () {return  annualRain;}
	public float getAnnualParIntercepted () {return  annualParIntercepted;}


	/****************************************************
	MANAGEMENT OF THE RESULTS OF LIGHT COMPETITION MODULE
	*****************************************************/
	public float getCaptureFactorForDiffusePar(){return captureFactorForDiffusePar;}
	public float getCaptureFactorForDirectPar(){return captureFactorForDirectPar;}
	public float getDirectParIntercepted(){return directParIntercepted;}
	public float getDiffuseParIntercepted(){return diffuseParIntercepted;}
	public float getTotalParIntercepted(){return directParIntercepted+diffuseParIntercepted;}
	public float getCompetitionIndexForTotalPar(){return competitionIndexForTotalPar;}
	public float getInterceptedPar(){return interceptedPar;}
	public void setCaptureFactorForDiffusePar(float e){captureFactorForDiffusePar=e;}
	public void setCaptureFactorForDirectPar(float e){captureFactorForDirectPar=e;}
	public void setDirectParIntercepted(float e){
		directParIntercepted=e;
		annualParIntercepted +=e;
	}
	public void setDiffuseParIntercepted(float e){
		diffuseParIntercepted=e;
		annualParIntercepted +=e;
	}


	public void setCompetitionIndexForTotalPar(float e){competitionIndexForTotalPar=e;}
	public void addDirect(float e) {this.captureFactorForDirectPar += e;}
	public void addDiffuse(float e){this.captureFactorForDiffusePar += e;}
	public double getParExtinctionCoef () {return (double) parExtinctionCoef;}
	public void setParExtinctionCoef (double v) {parExtinctionCoef = (float) v;}


	//MULCH
	public double getCarbonMulch() {return this.sticsCommun.Cmulch;}				// Total C in mulch at soil surface // kg.ha-1
	public double getNitrogenMulch() {return this.sticsCommun.Nmulch;}				// Total N in mulch at soil surface // kg.ha-1
	public double getCarbonMulchTreeFoliage() {return this.sticsCommun.Cnondec[9];}	// undecomposable C in residue i present in the mulch // kg.ha-1
	public double getMulchBiomass() {return this.sticsCommun.qmulch;}				//Quantity of plant mulch // t.ha-1
	public double getMulchEvaporation() {return this.sticsCommun.Emulch;}		    // Direct evaporation of water intercepted by the mulch // mm
	public double getMulchWaterStock() {return this.sticsCommun.mouillmulch;}		// Water stock in the mulch // mm
	public double getMulchCoverRatio() {return this.sticsCommun.couvermulch;}		// Cover ratio of mulch  // 0-1
	
	//POUR FRANCESCO
	public double getTempStressLue() {
		if (phenologicStageVegetative==1) return 1;
		else return  this.getSticsCrop().ftemp;
	}
	public double getTempStressGrainFilling() {
		if (phenologicStageVegetative==1) return 1;
		else return this.getSticsCrop().ftempremp;
		}
	public double getFrostStressPlantDensity () {
		if (phenologicStageVegetative==1) return 1;
		else return this.getSticsCrop().fgellev;
	}
	public double getFrostStressFoliage() {
		if (phenologicStageVegetative==1) return 1;
		else return this.getSticsCrop().fstressgel;
	}
	public double getFrostStressReprod() {
		if (phenologicStageVegetative==1) return 1;
		else return this.getSticsCrop().fgelflo;
	}
	
	public double getNitrogenGrain() {return this.sticsCrop.QNgrain[1];}	//kg h-1;

			
	public double getWaterUptakePotential () {
		if (plantRoots==null) return 0;
		return plantRoots.getWaterUptakePotential();
	}
	
	
	//PHENOLOGY (stades végétatifs) 
	//PLT : semis ou plantation (annuelles)
	public int getNplt() {
		if (this.sticsCrop.nplt==0) return 0;
		return this.sticsCrop.nplt+startDay;
	}	
	//DEBDORM et FINDORM : entrée et levée de dormance (ligneux)
	public int getNdebdorm() {
		if (this.sticsCrop.ndebdorm==0) return 0;
		return this.sticsCrop.ndebdorm+startDay;
	}	
	public int getNfindorm() {
		if (this.sticsCrop.nfindorm==0) return 0;
		return this.sticsCrop.nfindorm+startDay;
	}
	//LEV : levée ou débourrement végétatif
	public int getNlev() {
		if (this.sticsCrop.nlev==0) return 0;
		return this.sticsCrop.nlev+startDay;
	}
	//GER : germination
	public int getNger() {
		if (this.sticsCrop.nger==0) return 0;
		return this.sticsCrop.nger+startDay;
	}
    //AMF : accélération maximale de croissance foliaire, fin de phase juvénile
	public int getNamf() {
		if (this.sticsCrop.namf==0) return 0;
		return this.sticsCrop.namf+startDay;
	}	
	//LAX : indice foliaire maxi, fin de croissance foliaire nette ou brute selon l’option.
	public int getNlax() {
		if (this.sticsCrop.nlax==0) return 0;
		return this.sticsCrop.nlax+startDay;
	}		
	//SEN : début sénescence nette (option LAInet)
	public int getNsen() {
		if (this.sticsCrop.nsen==0) return 0;
		return this.sticsCrop.nsen+startDay;
	}
	//LAN : indice foliaire nul (option LAInet)
	public int getNlan() {
		if (this.sticsCrop.nlan==0) return 0;
		return this.sticsCrop.nlan+startDay;
	}
	//REC : récolte
	public int getNrec() {
		if (this.sticsCrop.nrec==0) return 0;
		return this.sticsCrop.nrec+startDay;
	}
	
	//PHENOLOGY (stades organes récoltés)
	//FLO : floraison (début sensibilité au gel des fruits)
	public int getNflo() {
		if (this.sticsCrop.nflo==0) return 0;
		return this.sticsCrop.nflo+startDay;
	}
    //DRP : début remplissage des organes récoltés
	public int getNdrp() {
		if (this.sticsCrop.ndrp==0) return 0;
		return this.sticsCrop.ndrp+startDay;
	}
	
	//NOU : fin de la nouaison (option indéterminée)
	public int getNnou() {
		if (this.sticsCrop.nnou==0) return 0;
		return this.sticsCrop.nnou+startDay;
	}
		
	//DEBDES ; début dynamique hydrique des fruits
	public int getNdebdes() {
		if (this.sticsCrop.ndebdes==0) return 0;
		return this.sticsCrop.ndebdes+startDay;
	}
	
	//MAT : maturité physiologique
	public int getNmat() {
		if (this.sticsCrop.nmat==0) return 0;
		return this.sticsCrop.nmat+startDay;
	}

	public double getTetstomate() {return  this.getSticsCrop().tetstomate;}
	public double getTeturg() {return this.getSticsCrop().teturg;}	
	public float getTeta() {		return this.sticsCrop.teta[1];}
	public float getTetsen() {return this.sticsCrop.tetsen[1];}	
	public float getSlrac() {return this.sticsCrop.slrac[1];}
	public float getCumlr() {return this.sticsCrop.cumlr[1];}	
	public float getCumlracz() {return this.sticsCrop.cumlracz;}
	public float getSupres() {return this.sticsCommun.supres;} 
	public float getEp() {return this.sticsCrop.ep[1];}
	public float getResrac() {return this.sticsCrop.resrac;}
	public float getDensite() {return this.sticsCrop.densite;}
	public float getCoeflev() {return this.sticsCrop.coeflev;}
	public float getVitmoy() {return this.sticsCrop.vitmoy[1];}
	public float getRemobilj() {return this.sticsCrop.remobilj[1];}
	public float getTcultMin() {return this.sticsCommun.TcultMin;}
	public float getTcultMax() {return this.sticsCommun.TcultMax;}
	public float getPgraingel() {return this.sticsCrop.pgraingel[1];}
	public float getDeltai() {return this.deltai;}
	public float getDurvie() {return this.durvie;}
	public float getLaisen() {return this.laisen;}
	public float getDltaisen() {return this.sticsCrop.dltaisen[1];}
	public float getExolai() {return this.sticsCrop.exolai;}
	public float getEfdensite() {return this.sticsCrop.efdensite;}
	public float getTempeff() {return this.sticsCrop.tempeff;}
	public float getTustress() {return this.sticsCommun.tustress;}
	public float getDeltaimaxi() {return this.sticsCrop.deltaimaxi[1];}
	public float getTmin() {return this.tmin;}
	public float getTmax() {return this.tmax;}
	public float getCodebeso() {return this.sticsCrop.P_codebeso;}
	public float getCodecaltemp() {return this.codecaltemp;}
	public float getCodetemp() {return this.sticsCrop.P_codetemp;}
	public float getQressuite() {return this.sticsCrop.qressuite;}
	public float getQNressuite() {return this.sticsCrop.QNressuite;}
	public float getQCressuite() {return this.sticsCrop.QCressuite;}	
	public float getQCplante() {return this.sticsCrop.QCplante;}
	public float getQNplantefauche() {return this.sticsCrop.QNplantefauche;}
	public float getMsfauche() {return this.sticsCrop.msfauche;}
	public float getCrac() {return this.sticsCrop.Crac;}
	public float getNrac() {return this.sticsCrop.Nrac;}
	public float getQCrac() {return this.sticsCrop.QCrac;}
	public float getQNrac() {return this.sticsCrop.QNrac;}

	
	public float getLueDay() {
		if (this.getBiomassIncrement() == 0) return 0;
		if (this.getTotalParIntercepted() == 0) return 0;
		return this.getBiomassIncrement()/this.getTotalParIntercepted(); //kg MS/mole PAR
	} 

	public float getLueInt() {
		if (this.getBiomass() == 0) return 0;
		if (this.getAnnualParIntercepted() == 0) return 0;
		return this.getBiomass()/this.getAnnualParIntercepted(); //kg MS/mole PAR
	} 
	
	public float getWueDay() {
		if (this.getBiomassIncrement() == 0) return 0;
		if (this.getWaterUptake() == 0) return 0;
		return this.getBiomassIncrement()*100/this.getWaterUptake();//g MS/liter
	} 
	
	public float getWueInt() {
		if (this.getBiomass() == 0) return 0;
		if (this.getAnnualWaterUptake() == 0) return 0;
		return this.getBiomass()*100/this.getAnnualWaterUptake(); //g MS/liter
	} 
	
	public String toString(){
		String str = "crop cropSpeciesName ="+cropSpeciesName;
		return str;
	}
}


