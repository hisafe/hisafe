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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * TREE management parameters  
 * 
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeTreeItk   implements Serializable {


	/** Tree species name */
	public String treeSpecies;
	
	// PLANTING 
	/** Tree planting year index */
	public int plantingYear; 					
	/** Tree planting day (julian day)  */
	public int plantingDay;  				
	/** Tree age at planting (years)  */
	public int plantingAge;					
	/** Tree height at planting (m)  */
	public double plantingHeight; 		
	/**  Tree crown base height at planting (m)   */
	public double plantingCrownBaseHeight; 	
	/**  Tree crown radius at planting (m)   */
	public double plantingCrownRadius; 		
	/**  Leaf ages by cohorts at tree planting (m)   */
	public List<Integer> plantingCohortAge; 
	/**  Root shape at tree planting (1=Sphere; 2=Ellipsoid; 3=Cone)   */
	public int plantingRootShape; 		
	/**  Root repartition at tree planting (1=Uniform; 2=Inverse proportional to distance; 3=Negative exponential)   */
	public int plantingRootRepartition;  	
	/** Root shape parameter at tree planting   */
	public double plantingRootShapeParam1; 	
	/**  Root shape parameter at tree planting     */
	public double plantingRootShapeParam2; 	
	/**  Root shape parameter at tree planting     */
	public double plantingRootShapeParam3; 	
	
	// HARVEST 
	/** Tree harvest year index */
	public int treeHarvestYear; 			
	/** Tree harvest day  (julian day) */
	public int treeHarvestDay; 				
	/** Fruit harvest days  (julian day) */
	public List<Integer>  fruitHarvestDays; 
	
	//TROPICAL SPECIES MODULE (FAIDHERBIA)
	//ADDED  BY LEA TRESCH 01/2026
	/** Date of budburst for the first year (initialisation) */
	public int budburstInitialisation;
	
	// PRUNING 
	/** Tree pruning years index */
	public List<Integer> treePruningYears; 				
	/** Tree pruning days   (julian day)  */
	public List<Integer> treePruningDays; 					
	/** Tree pruning proportion */
	public List<Double> treePruningProp; 				
	/** Tree pruning max height (m)  */
	public List<Double> treePruningMaxHeight; 			
	/** Tree pruning residues incorporation (0=exported 1=on floor) */
	public List<Integer> treePruningResiduesIncorporation;
	/** Tree pruning residues spreading (0=exported 1=under tree crown 2=all over the plot) */
	public List<Integer> treePruningResiduesSpreading; 		

	// ROOT PRUNING 
	/** Tree root pruning years index */
	public List<Integer> treeRootPruningYears; 	
	/** Tree root pruning days   (julian day)  */
	public List<Integer> treeRootPruningDays; 	
	/** Tree root pruning distance from tree trunk   (m)  */
	public List<Double> treeRootPruningDistance; 
	/** Tree root pruning depth   (m)  */
	public List<Double> treeRootPruningDepth; 	

	//TOPING C.DUPRAZ 12.10.2021
	/** Tree toping years index */
	public List<Integer> treeTopingYears; 				
	/** Tree toping days   (julian day)  */
	public List<Integer> treeTopingDays; 				
	/** Tree toping height   (m)  */
	public List<Double> treeTopingHeight; 	    			
	/** Tree toping residues incorporation (0=exported 1=on floor)  */
	public List<Integer> treeTopingResiduesIncorporation; 
	/** Tree toping residues spreading 0=exported 1=under tree crown 2=all over the plot)  */
	public List<Integer> treeTopingResiduesSpreading; 		
	
	// FRUIT THINNING N.BARBAULT 03.08.2021
	/** Fruit thinning years index */
	public List<Integer> fruitThinningYears; 				 
	/** Fruit thinning method (1=none 2=auto 3=manual) */
	public List<Integer> fruitThinningMethod;  				 
	/** Fruit thinning days if manual (julian days) */
	public List<Integer> fruitThinningDays; 				
	/** Fruit target after thinning (nbr fruits) */
	public List<Integer> fruitThinningFruitNbrTarget; 		
	/**  Fruit optimal number for a 1m2 of leaf area for automatic thinning */
	public List<Double>  fruitOptimalLoadLeafArea;			 
	/** Nbr of day after fruit setting for automatic thinning	 */
	public List<Integer> fruitThinningDelayAfterSetting;	 // 
	/** Fruit thinning  residues incorporation (0=exported 1=on floor)  */
	public List<Integer> fruitThinningResiduesIncorporation; 
	/** Fruit thinning  residues spreading 0=exported 1=under tree crown 2=all over the plot)  */
	public List<Integer> fruitThinningResiduesSpreading; 	
	
	// LEAF AREA REDUCTION N.BARBAULT 20.08.2021
	/** Leaf area reduction years index */
	public List<Integer> leafAreaDensityReductionYears; 		
	/** Leaf area reduction days (julian days) */
	public List<Integer> leafAreaDensityReductionDays;			
	/** Leaf area density threshold to trigger leaf area reduction (m2 m-3)  */
	public List<Double>  leafAreaDensityReductionThreshold;		
	/** Fraction of leaf area reduction (%)  */
	public List<Double>  leafAreaDensityReductionFraction;		
	/** Leaf area reduction residues incorporation (0=exported 1=on floor)  */
	public List<Integer> leafAreaDensityReductionResiduesIncorporation; 	
	/** Leaf area reduction residues spreading 0=exported 1=under tree crown 2=all over the plot) */
	public List<Integer> leafAreaDensityReductionResiduesSpreading; 	
	
	// CANOPY TRIMMING N.BARBAULT 20.08.2021
	/** Canopy trimming years index */
	public List<Integer> canopyTrimmingYears; 						
	/** Canopy trimming years days (julian days) */
	public List<Integer> canopyTrimmingDays; 						
	/** Crown radius trigger for canopy trimming on tree Line (m)*/
	public List<Double>  canopyTrimmingTreeLineTrigger;				
	/** Crown radius target after canopy trimming on tree Line (m)  */
	public List<Double>  canopyTrimmingTreeLineReductionTarget;		
	/** Crown radius trigger for canopy trimming on inter row (m) */
	public List<Double>  canopyTrimmingInterRowTrigger;				
	/** Crown radius target after canopy trimming on inter row (m)*/
	public List<Double>  canopyTrimmingInterRowReductionTarget;		 
	/** Canopy trimming residues incorporation (0=exported 1=on floor)  */
	public List<Integer> canopyTrimmingResiduesIncorporation; 	
	/** Canopy trimming residues spreading 0=exported 1=under tree crown 2=all over the plot) */
	public List<Integer> canopyTrimmingResiduesSpreading; 	
	
	// IRRIGATION N.BARBAULT 09.11.2022
	/** Tree irrigation type (0=none 1=auto 2=manual) */
	public int treeIrrigationType;  							
	/** Tree irrigation method (1=drip 2=aspersion 3=flooding) */
	public int treeIrrigationMethod;  							
	/** dripor sprinklers position X */
	public List<Double> treeIrrigationDriporSprinklerX;			
	/** dripor sprinklers position Y */
	public List<Double> treeIrrigationDriporSprinklerY;			
	/** Radius distance of irrigation from dripor sprinkler (m) */
	public Double treeIrrigationRadius;  						
	/** Tree water stress to triggered automatic irrigation  */
	public Double treeIrrigationWaterStressTrigger; 		
	/** Tree automatic irrigation water dose (mm) */
	public Double treeIrrigationAutomaticDose; 				
	/** Tree manual irrigation years index */
	public List<Integer> treeIrrigationYears;  					
	/** Tree manual irrigation days (julian days) */
	public List<Integer> treeIrrigationDays;  					
	/** Tree manual irrigation doses (mm) */
	public List<Double> treeIrrigationDose;  	    			

	// FETILIZATION N.BARBAULT 09.11.2022
	/** Tree fertilization type (0=none 1=auto 2=manual) */
	public int treeFertilizationType;  						
	/** Distance of fertilization to the tree truck   */
	public Double treeFertilizationRadius;  				
	/** Tree N stress to triggered automatic fertilization  */
	public Double treeFertilizationNitrogenStressTrigger; 		
	/** Tree automatic fertilization code (1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency) */
	public Integer treeFertilizerAutomaticCode;  			
	/** Tree automatic fertilization doses (kg N) */
	public Double treeFertilizationAutomaticDose; 				
	/** Tree manual fertilization years index */
	public List<Integer> treeFertilizationYears;  				
	/** Tree manual fertilization days (julian days) */
	public List<Integer> treeFertilizationDays;  				
	/** Tree manual fertilization code (1 =Nitrate.of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrateof calcium,8= fixed efficiency) */
	public List<Integer> treeFertilizerCode;  					
	/** Tree manual fertilization doses (kg N) */
	public List<Double> treeFertilizationDose;  	    		

	/** Frost damage option activation */
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
