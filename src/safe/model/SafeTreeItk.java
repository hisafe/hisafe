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
import java.util.ArrayList;
import java.util.List;


/**
 * TREE management parameters  
 * 
 * @author Isabelle Lecomte - INRA Montpellier France  - July 2023
 */
public class SafeTreeItk   implements Serializable {

	private static final long serialVersionUID = 1L;
	
	public String treeSpecies;
	
	// TREE PLANTING 
	public int plantingYear; 				// year index
	public int plantingMonth;  				// month
	public int plantingDay;  				// days
	public int plantingAge;					// Initial age - years
	public double plantingHeight; 			// Initial height - m
	public double plantingCrownBaseHeight; 	// Initial crown base height - m
	public double plantingCrownRadius; 		// Initial crown radius (in both the x and y directions) - m
	public List<Integer> plantingCohortAge; // Leaf age by cohort - years
	
	// ROOT INIT
	public int plantingRootShape; 			// 1=Sphere; 2=Ellipsoid; 3=Cone
	public int plantingRootRepartition;  	// 1=Uniform; 2=Inverse proportional to distance; 3=Negative exponential
	public double plantingRootShapeParam1; 	// Sphere: radius of sphere, Ellipsoid: radius of ellipsoid in X direction, Cone: radius of cone base
	public double plantingRootShapeParam2; 	// Sphere: NOT USE, Ellipsoid: radius of ellipsoid in Y direction, Cone: depth of cone
	public double plantingRootShapeParam3; 	// Sphere: NOT USED, Ellipsoid: radius of ellipsoid in Z direction, Cone: NOT USED
	
	// TREE HARVEST 
	public int treeHarvestYear; 			// tree harvest year
	public int treeHarvestDay; 				// tree harvest day
	
	// TREE PRUNING 
	public List<Integer> treePruningYears; 					// tree pruning years
	public List<Integer> treePruningDays; 					// tree pruning days
	public List<Double> treePruningProp; 					// tree pruning proportion
	public List<Double> treePruningMaxHeight; 				// tree pruning max height in meters
	public List<Integer> treePruningResiduesIncorporation; 	// tree pruning residues incorporation (0=exported 1=on floor)
	public List<Integer> treePruningResiduesSpreading; 		// tree pruning residues spreading (0=exported 1=under tree crown 2=all over the plot)

	// TREE ROOT PRUNING 
	public List<Integer> treeRootPruningYears; 	// tree root pruning years
	public List<Integer> treeRootPruningDays; 	// tree root pruning days
	public List<Double> treeRootPruningDistance; // tree root pruning distance
	public List<Double> treeRootPruningDepth; 	// tree root pruning depth

	//TREE TOPING C.DUPRAZ 12.10.2021
	public List<Integer> treeTopingYears; 					// tree toping years
	public List<Integer> treeTopingDays; 					// tree toping days
	public List<Double> treeTopingHeight; 	    			// tree height after toping
	public List<Integer> treeTopingResiduesIncorporation; 	// tree toping residues incorporation (0=exported 1=on floor)
	public List<Integer> treeTopingResiduesSpreading; 		// tree toping residues spreading (0=exported 1=under tree crown 2=all over the plot)
	
	// FRUIT THINNING N.BARBAULT 03/08/2021
	public List<Integer> fruitThinningYears; 				 // fruit thinning years
	public List<Integer> fruitThinningMethod;  				 // fruit thinning method 1=none 2=auto 3=manual
	public List<Integer> fruitThinningDays; 				 // fruit thinning days if manual
	public List<Integer> fruitThinningFruitNbrTarget; 		 // nbr fruit target after thinning
	public List<Double> fruitOptimalLoadLeafArea;			 // nbr fruit optimal for a 1m2 of leaf area
	public List<Integer> fruitThinningDelayAfterSetting;	 // nbr of day after fruit setting for thinning	
	public List<Integer> fruitThinningResiduesIncorporation; // fruit thinning  residues incorporation (0=exported 1=on floor)
	public List<Integer> fruitThinningResiduesSpreading; 	 // fruit thinning  residues spreading (0=exported 1=under tree crown 2=all over the plot)
	
	// LEAF AREA REDUCTION N.BARBAULT 20/08/2021
	public List<Integer> leafAreaDensityReductionYears; 		//leaf area reduction years
	public List<Integer> leafAreaDensityReductionDays;			//leaf area reduction days
	public List<Double>  leafAreaDensityReductionThreshold;		//threshold to trigger leaf area reduction
	public List<Double>  leafAreaDensityReductionFraction;		//fraction of leaf area reduction 
	public List<Integer> leafAreaDensityReductionResiduesIncorporation; 	// leaf area reduction residues incorporation (0=exported 1=on floor)
	public List<Integer> leafAreaDensityReductionResiduesSpreading; 	// leaf area reduction residues  spreading (0=exported 1=under tree crown 2=all over the plot)
	
	// TREE CANOPY TRIMMING N.BARBAULT 20/08/2021
	public List<Integer> canopyTrimmingYears; 						//canopy trimming years
	public List<Integer> canopyTrimmingDays; 						//canopy trimming days
	public List<Double>  canopyTrimmingTreeLineTrigger;				//crown radius trigger for canopy trimming on treeLine (m)
	public List<Double>  canopyTrimmingTreeLineReductionTarget;		//crown radius target after canopy trimming on treeLine (m) 
	public List<Double>  canopyTrimmingInterRowTrigger;				//crown radius trigger for canopy trimming on treeLine (m)
	public List<Double>  canopyTrimmingInterRowReductionTarget;		//crown radius target after canopy trimming on treeLine (m) 
	public List<Integer> canopyTrimmingResiduesIncorporation; 		// canopyTrimming residus incorporation (0=exported 1=on floor)
	public List<Integer> canopyTrimmingResiduesSpreading; 	// leaf area reduction residues  spreading (0=exported 1=under tree crown 2=all over the plot)
	
	//FRUIT HARVEST
	public List<Integer>  fruitHarvestDays; 							// tree harvest days
	
	
	//TREE IRRIGATION N.BARBAULT 09/11/2022
	public int treeIrrigationType;  							//0= none 1=auto 2=manual
	public int treeIrrigationMethod;  							//1=drip 2=aspersion 3=flooding
	public List<Double> treeIrrigationDriporSprinklerX;			//dripor sprinklers position X
	public List<Double> treeIrrigationDriporSprinklerY;			//dripor sprinklers position Y
	public Double treeIrrigationRadius;  						//radius distance of irrigation from dripor sprinkler
	
	public Double treeIrrigationWaterStressTrigger; 			//if auto Tree water stress to triggered irrigation 
	public Double treeIrrigationAutomaticDose; 					//tree irrigation automatic water dose 
	public List<Integer> treeIrrigationYears;  					//tree irrigation years 
	public List<Integer> treeIrrigationDays;  					//tree irrigation manual days 
	public List<Double> treeIrrigationDose;  	    			//tree irrigation manual water dose for each day 

	//TREE FETILIZATION N.BARBAULT 09/11/2022
	public int treeFertilizationType;  							//0= none 1=auto 2=manual
	public List<Integer> treeFertilizationYears;  				//tree fertilization years
	public Double treeFertilizationRadius;  					//distance of fertilization to the tree truck 
	public Double treeFertilizationNitrogenStressTrigger; 		//Tree N stress to triggered fertilization 
	public Double treeFertilizationAutomaticDose; 				//tree fertilization automatic water dose 
	public Integer treeFertilizerAutomaticCode;  				//tree fertilizer code for manual option
	public List<Integer> treeFertilizationDays;  				//tree fertilization days for manual option
	public List<Integer> treeFertilizerCode;  					//tree fertilizer code for manual option
	public List<Double> treeFertilizationDose;  	    		//tree fertilization N dose for manual option 
	

	//frost damage activation
	public boolean frostDamageActivation = true;	


	/**
	 * Constructor.
	 */
	public SafeTreeItk() throws Exception {

		plantingCohortAge = new ArrayList<Integer>();
		
		treePruningYears = new ArrayList<Integer>();
		treePruningProp = new ArrayList<Double>();
		treePruningMaxHeight = new ArrayList<Double>();
		treePruningDays = new ArrayList<Integer>();
		treePruningResiduesIncorporation = new ArrayList<Integer>();
		treePruningResiduesSpreading = new ArrayList<Integer>();
		
		treeRootPruningYears = new ArrayList<Integer>();
		treeRootPruningDays = new ArrayList<Integer>();
		treeRootPruningDistance = new ArrayList<Double>();
		treeRootPruningDepth = new ArrayList<Double>();
		treeTopingYears = new ArrayList<Integer>();
		treeTopingDays = new ArrayList<Integer>();
		treeTopingHeight = new ArrayList<Double>();
		treeTopingResiduesIncorporation = new ArrayList<Integer>();
		treeTopingResiduesSpreading = new ArrayList<Integer>();

		fruitHarvestDays = new ArrayList<Integer>();
		fruitThinningYears = new ArrayList<Integer>();
		fruitThinningMethod = new ArrayList<Integer>();
		fruitThinningDays = new ArrayList<Integer>();
		fruitThinningFruitNbrTarget = new ArrayList<Integer>();
		fruitOptimalLoadLeafArea = new ArrayList<Double>();
		fruitThinningDelayAfterSetting = new ArrayList<Integer>();
		fruitThinningResiduesIncorporation = new ArrayList<Integer>();
		fruitThinningResiduesSpreading = new ArrayList<Integer>();
		
		leafAreaDensityReductionYears = new ArrayList<Integer>();
		leafAreaDensityReductionDays = new ArrayList<Integer>();
		leafAreaDensityReductionThreshold = new ArrayList<Double>();
		leafAreaDensityReductionFraction = new ArrayList<Double>();
		leafAreaDensityReductionResiduesIncorporation = new ArrayList<Integer>();
		leafAreaDensityReductionResiduesSpreading = new ArrayList<Integer>();

		canopyTrimmingYears = new ArrayList<Integer>();
		canopyTrimmingDays = new ArrayList<Integer>();
		canopyTrimmingTreeLineTrigger = new ArrayList<Double>();
		canopyTrimmingTreeLineReductionTarget = new ArrayList<Double>();
		canopyTrimmingInterRowTrigger = new ArrayList<Double>();
		canopyTrimmingInterRowReductionTarget = new ArrayList<Double>();		
		canopyTrimmingResiduesIncorporation = new ArrayList<Integer>();
		canopyTrimmingResiduesSpreading = new ArrayList<Integer>();

		treeIrrigationYears = new ArrayList<Integer>();
		treeIrrigationDays = new ArrayList<Integer>();
		treeIrrigationDriporSprinklerX = new ArrayList<Double>();
		treeIrrigationDriporSprinklerY = new ArrayList<Double>();
		treeIrrigationDose = new ArrayList<Double>();
		
		treeFertilizationYears = new ArrayList<Integer>();
		treeFertilizationDays = new ArrayList<Integer>();
		treeFertilizerCode = new ArrayList<Integer>();
		treeFertilizationDose = new ArrayList<Double>();
	}
	
}
