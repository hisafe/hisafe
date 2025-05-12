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

import java.util.Iterator;

public class SafeWaterCompetitionModel  {
	
	/**
	 * WATER AND NITROGEN REPARTITION PROCESS
	 * 
	 * @author : M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
	 * @author : D.HARJA          - ICRAF Bogor Indonisia
	 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
	 * 
	 * @param stand Reference to SafeStand object
	 * @param generalParameters Reference to SafeGeneralParameters object
	 * 
	 **/
	public static void waterNitrogenRepartition (SafeStand stand,
												 SafeGeneralParameters generalParameters) {

		//FOR EACH TREE CALCULATE WATER POTENTIEL and WATER DEMAND REDUICE 
		for (Iterator i = stand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() && !tree.isHarvested()) {
				
				double treeTotalRootLenght = tree.computeTotalRootLength();
				if (treeTotalRootLenght>0) tree.computePlantWaterPotential();
				
				if (tree.getWaterDemand () > 0) {
					
					//calculate potentials
					double  campbellFactor = tree.getTreeSpecies().getCampbellFactorIcraf();
					double halfCurrWaterPotential =  tree.getTreeSpecies().getHalfCurrWaterPotentialIcraf();
					tree.getPlantRoots().calculatePotential (tree.getWaterDemand (), campbellFactor, halfCurrWaterPotential);

					// Reduction factor for water demand
					double waterDemandReductionFactor = tree.getPlantRoots().getWaterDemandReductionFactor();
					tree.setWaterDemandReduced (tree.getWaterDemand() * waterDemandReductionFactor);		//liters

				}
			}
		}
		
		//FOR EACH CROP CALCULATE WATER POTENTIEL and WATER DEMAND REDUICE 
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {

			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop = cell.getCrop();
			
			double cropTotalRootLenght = crop.computeTotalRootsLength();
			if (cropTotalRootLenght>0) crop.computePlantWaterPotential();
		
			if (crop.getWaterDemand () > 0) {		//in mm
				//calculate potentials
				double  campbellFactor = cell.getCropZone().getCropSpecies().getCampbellFactorIcraf();
				double halfCurrWaterPotential =  cell.getCropZone().getCropSpecies().getHalfCurrWaterPotentialIcraf();
				crop.getPlantRoots().calculatePotential (crop.getWaterDemand()* (float)cell.getArea(), campbellFactor, halfCurrWaterPotential);		
				
				//Reduction factor for water demand (dimensionless)
				double waterDemandReductionFactor = crop.getPlantRoots().getWaterDemandReductionFactor();
				crop.setWaterDemandReduced (crop.getWaterDemand() * waterDemandReductionFactor);		//mm
			}
		}
		
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeVoxel voxels[] = cell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {

				//root map creation
				voxels[iz].initRootMap();
				
				//for the crop
				if (voxels[iz].getCropRootsDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, generalParameters);

					
				}

				//Same for trees roots
				for (int t=0; t < voxels[iz].getTreeRootsDensitySize();t++) {

					if (voxels[iz].getTheTreeRootsDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (stand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, generalParameters);
						}
					}
				}
			}
		}

		//FOR EACH VOXEL, RAZ OF WATER UPTAKE POTENTIAL
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].razWaterUptakePotential ();
			}
		}	
		
		//FOR EACH VOXEL, CALCULATION OF WATER AND NITROGEN UPTAKE POTENTIAL
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].countWaterUptakePotential (generalParameters); 
					voxels[iz].countNitrogenUptakePotential (stand, generalParameters);	
			}
		}


		//FOR EACH CROP, CALCULATION OF WATER AND NITROGEN UPTAKE
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop	= cell.getCrop();
			SafePlantRoot cropRoot = crop.getPlantRoots();
			
			//WATER
			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cell.getArea();	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cell.getArea();				//liters
			
			crop.setWaterUptake (0);
			double waterStress = 1;
			
			//water extraction is computed if demand is > 0 and if  crop is NOT harvested 
			if ((cropWaterDemandReduced > 0) && (crop.getPhenologicStageVegetative() < 11)) {

				double cropWaterUptake = cropRoot.calculateWaterUptake (stand, crop,  cropWaterDemandReduced, true);
				crop.setWaterUptake (cropWaterUptake / cell.getArea());					 	//mm

				if (cropWaterDemand > 0) {
					if(cropWaterUptake <= 0) 
						waterStress = generalParameters.waterStressMin; 						
					else 
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);		
					
					waterStress = Math.max (waterStress,  generalParameters.waterStressMin);
				}

				//Store crop water uptake in STICS
				crop.sticsCrop.ep[0] = (float) crop.getWaterUptake() ;
				crop.sticsCrop.ep[1] = (float) crop.getWaterUptake() ;	
			}
			else {

				if ((cropWaterDemand > 0) && (cropWaterDemandReduced == 0) ) 
					waterStress = generalParameters.waterStressMin; 
			}
			
			//Store the water stress 
			crop.setHisafeWaterStomatalStress (waterStress);
			crop.sticsCrop.swfac[0] = (float) waterStress;
			crop.sticsCrop.swfac[1] = (float) waterStress;

			//NITROGEN		
			double cropNitrogenDemand  = (crop.getNitrogenDemand() / 10) * cell.getArea();	//convert kg ha-1 in g
			crop.setNitrogenUptake (0);
			double nitrogenStress = 1;
		
			if (cropNitrogenDemand > 0) {

				double cropNitrogenUptake = cropRoot.calculateNitrogenUptake (stand, crop,  cropNitrogenDemand);
	
				crop.setNitrogenUptake ((cropNitrogenUptake * 10) / cell.getArea());		//convert g in kg ha-1
				if(cropNitrogenUptake <= 0) 
					nitrogenStress = generalParameters.nitrogenStressMin; 
				else {
					nitrogenStress = Math.min (cropNitrogenUptake  / cropNitrogenDemand, 1);
					nitrogenStress = Math.max (nitrogenStress, generalParameters.nitrogenStressMin);
				}
			}
			//store nitrogen stress
			crop.setHisafeNitrogenStress (nitrogenStress);
			//crop.sticsCrop.innlai[0] = (float) nitrogenStress;
			//crop.sticsCrop.innlai[1] = (float) nitrogenStress;
		
		}

		//FOR EACH TREE, CALCULATION OF WATER AND NITROGEN UPTAKE
		for (Iterator iter=stand.getTrees().iterator(); iter.hasNext(); ) {
			SafeTree tree = (SafeTree)iter.next();
			if (tree.isPlanted() && !tree.isHarvested()) {
				SafePlantRoot treeRoot = (SafePlantRoot) tree.getPlantRoots();
	
				//WATER
				tree.setWaterUptake (0);
				double treeWaterDemand = tree.getWaterDemandReduced ();		//liters
				if (treeWaterDemand > 0 && treeRoot.getActualWaterPotential()<0) {
					double treeWaterUptake = treeRoot.calculateWaterUptake (stand, tree,  treeWaterDemand, true);
					tree.setWaterUptake(treeWaterUptake);
				}
				
				//NITROGEN UPTAKE
				tree.setNitrogenAvailable (0);
				tree.setNitrogenUptake (0);
				
				double treeNitrogenDemand = tree.getNitrogenDemandAfterFixation() * 1000;	//convert kg in g;
		
				if (treeNitrogenDemand > 0 && treeRoot.getActualWaterPotential()<0) {

					//calcul de setNitrogenUptakeWithoutFixation dans ce code
					double nitrogenUptake = treeRoot.calculateNitrogenUptake (stand, tree,  treeNitrogenDemand);
					tree.setNitrogenUptake(nitrogenUptake);


					//add fixation 
					if (tree.getTreeSpecies().getNitrogenFixation()) {
						tree.setNitrogenAvailable (tree.getNitrogenUptake() + tree.getBnfNitrogenFixation());
					}	
					else 
						tree.setNitrogenAvailable (tree.getNitrogenUptake());
				}
				else {
					
					//if demand = 0, uptake = fixation
					if (tree.getTreeSpecies().getNitrogenFixation())
						tree.setNitrogenAvailable(tree.getBnfNitrogenFixation());
				}		
			}
		}
	}

	/**
	 * TURFAC CALCULATION (crop turgescence stress) 
	 * ADDED in JUNE 2020 
	 * @param stand Reference to SafeStand object
	 * @param generalParameters Reference to SafeGeneralParameters object
	 **/	
	public static void computeWaterStressTurfac  (SafeStand stand,
														SafeGeneralParameters generalParameters) {


		//FOR EACH VOXEL, CALCULATION OF WATER STOCK REDUICED OF 20%
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeVoxel voxels[] = cell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
				double psisto = cell.getCrop().sticsCrop.P_psisto;
				double psiturg = cell.getCrop().sticsCrop.P_psiturg;
				voxels[iz]. saveWaterStock ();	//save real values
				double waterStockTurfac = voxels[iz].computeWaterStockTurfac (psisto, psiturg);	
				voxels[iz].setWaterStock(waterStockTurfac);	//replace real value by -20%		
			}
		}

		
		//FOR EACH TREE CALCULATED WATER POTENTIEL and WATER DEMAND REDUICE   
		for (Iterator i = stand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() && !tree.isHarvested()) {
				
				double treeTotalRootLenght = tree.computeTotalRootLength();
				if (treeTotalRootLenght>0) tree.computePlantWaterPotential();
				
				if (tree.getWaterDemand () > 0) {
					//calculate potentials
					double  campbellFactor = tree.getTreeSpecies().getCampbellFactorIcraf();
					double halfCurrWaterPotential =  tree.getTreeSpecies().getHalfCurrWaterPotentialIcraf();
					tree.getPlantRoots().calculatePotential (tree.getWaterDemand (), campbellFactor, halfCurrWaterPotential);
	
					// Reduction factor for water demand
					double waterDemandReductionFactor = tree.getPlantRoots().getWaterDemandReductionFactor();
					tree.setWaterDemandReduced (tree.getWaterDemand() * waterDemandReductionFactor);		//liters
				}
			}
		}
		
		//FOR EACH CROP CALCULATED WATER POTENTIEL and WATER DEMAND REDUICE 
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {

			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop = cell.getCrop();
			double cropTotalRootLenght = crop.computeTotalRootsLength();
			if (cropTotalRootLenght>0) crop.computePlantWaterPotential();
			

			if (crop.getWaterDemand () > 0) {		//in mm
				//calculate potentials
				double  campbellFactor = cell.getCropZone().getCropSpecies().getCampbellFactorIcraf();
				double halfCurrWaterPotential =  cell.getCropZone().getCropSpecies().getHalfCurrWaterPotentialIcraf();
				crop.getPlantRoots().calculatePotential (crop.getWaterDemand()*cell.getArea(), campbellFactor, halfCurrWaterPotential);		
				
				// Reduction factor for water demand
				double waterDemandReductionFactor = crop.getPlantRoots().getWaterDemandReductionFactor();
				crop.setWaterDemandReduced (crop.getWaterDemand() * waterDemandReductionFactor);		//mm
			}
		}
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			
			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				//root map creation
				voxels[iz].initRootMap();
				
				//for the crop
				if (voxels[iz].getCropRootsDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, generalParameters);
				}

				//Same for trees roots
				for (int t=0; t < voxels[iz].getTreeRootsDensitySize();t++) {

					if (voxels[iz].getTheTreeRootsDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (stand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, generalParameters);
						}
					}
				}
			}
		}
			
		//FOR EACH VOXEL, RAZ OF WATER UPTAKE POTENTIAL
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
				voxels[iz].razWaterUptakePotential ();
			}
		}	

		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
				voxels[iz].countWaterUptakePotential (generalParameters);
			}
		}
			


		//FOR EACH CROP, CALCULATION OF WATER UPTAKE AND WATER STRESS
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop	= cell.getCrop();
			SafePlantRoot cropRoot = crop.getPlantRoots();

			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cell.getArea();	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cell.getArea();				//liters
			double waterStress = 1;
			
			//Water stress is calculated if demand if positive and crop is not harvested
			if ((cropWaterDemandReduced > 0) && (crop.getPhenologicStageVegetative() < 11)) {
				double cropWaterUptake = cropRoot.calculateWaterUptake (stand, crop, cropWaterDemandReduced, false);

				if (cropWaterDemand > 0) {
					if (cropWaterUptake <= 0) 
						waterStress =  generalParameters.waterStressMin; //P_swfacmin;
					else 
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);						
				}
				waterStress = Math.max (waterStress,  generalParameters.waterStressMin);
			}

			//Store water stress in STICS turfac				
			crop.setHisafeWaterTurgescenceStress(waterStress);
			crop.sticsCrop.turfac[0] = (float) waterStress;
			crop.sticsCrop.turfac[1] = (float) waterStress;

		}	
	
		
		//Restore the real water stock in each voxels
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeVoxel voxels[] = cell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
				voxels[iz].restoreWaterStock ();
			}
		}
	}
	
	/**
	 * SENFAC CALCULATION (crop senescence stress) 
	 * ADDED in JUNE 2020 
	 * @param stand Reference to SafeStand object
	 * @param generalParameters Reference to SafeGeneralParameters object
	 **/
	public static void computeWaterStressSenfac  (SafeStand stand,
														SafeGeneralParameters generalParameters) {

		double plotArea = stand.getArea();
		double cellArea = 0;

		//FOR EACH VOXEL, CALCULATION OF WATER STOCK INCREASE OF 20%
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();

			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {
				double psisto = cell.getCrop().sticsCrop.P_psisto;
				double psiturg = cell.getCrop().sticsCrop.P_psiturg;
				voxels[iz]. saveWaterStock ();	//save real water stock value
				double waterStockSenfac = voxels[iz].computeWaterStockSenfac (psisto, psiturg);
				voxels[iz].setWaterStock(waterStockSenfac);	//replace water stock with +20%
			}
		}

		
		//FOR EACH TREE CALCULATE WATER POTENTIEL and WATER DEMAND REDUICE 
		for (Iterator i = stand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() && !tree.isHarvested()) {
				
				double treeTotalRootLenght = tree.computeTotalRootLength();
				if (treeTotalRootLenght>0) tree.computePlantWaterPotential();

				if (tree.getWaterDemand () > 0) {
					//calculate potentials
					double  campbellFactor = tree.getTreeSpecies().getCampbellFactorIcraf();
					double halfCurrWaterPotential =  tree.getTreeSpecies().getHalfCurrWaterPotentialIcraf();
					tree.getPlantRoots().calculatePotential (tree.getWaterDemand (), campbellFactor, halfCurrWaterPotential);

					// Reduction factor for water demand
					double waterDemandReductionFactor = tree.getPlantRoots().getWaterDemandReductionFactor();
					tree.setWaterDemandReduced (tree.getWaterDemand() * waterDemandReductionFactor);		//liters
				}
			}
		}
		
		//FOR EACH CROP CALCULATE WATER POTENTIEL and WATER DEMAND REDUICE 
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {

			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop = cell.getCrop();
			double cropTotalRootLenght = crop.computeTotalRootsLength();
			if (cropTotalRootLenght>0) crop.computePlantWaterPotential();

			if (crop.getWaterDemand () > 0) {		//in mm
				//calculate potentials
				double  campbellFactor = cell.getCropZone().getCropSpecies().getCampbellFactorIcraf();
				double halfCurrWaterPotential =  cell.getCropZone().getCropSpecies().getHalfCurrWaterPotentialIcraf();
				crop.getPlantRoots().calculatePotential (crop.getWaterDemand()*cell.getArea(), campbellFactor, halfCurrWaterPotential);		

				//Reduction factor for water demand (dimensionless)
				double waterDemandReductionFactor = crop.getPlantRoots().getWaterDemandReductionFactor();
				crop.setWaterDemandReduced (crop.getWaterDemand() * waterDemandReductionFactor);		//mm
			}
		}
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeVoxel voxels[] = cell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {

				//root map creation
				voxels[iz].initRootMap();
				
				//for the crop
				if (voxels[iz].getCropRootsDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, generalParameters);
				}

				//Same for trees roots
				for (int t=0; t < voxels[iz].getTreeRootsDensitySize();t++) {
					if (voxels[iz].getTheTreeRootsDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (stand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, generalParameters);
						}
					}
				}
			}
		}
			
		//FOR EACH VOXEL, RAZ OF WATER UPTAKE POTENTIAL
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].razWaterUptakePotential ();
			}
		}	

		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].countWaterUptakePotential (generalParameters);
			}
		}
			
		//FOR EACH CROP, CALCULATION OF WATER UPTAKE AND STRESS
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			cellArea = cell.getArea();
			SafeCrop crop	= cell.getCrop();
			SafePlantRoot cropRoot = crop.getPlantRoots();

			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cellArea;	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cellArea;				//liters
			double waterStress = 1;
			
			//Water stress is calculated if demand if positive and crop is not harvested
			if ((cropWaterDemandReduced > 0) && (crop.getPhenologicStageVegetative() < 11) && (crop.getHisafeWaterStomatalStress() < 1)) {
				
				double cropWaterUptake = cropRoot.calculateWaterUptake (stand, crop, cropWaterDemandReduced, false);

				if (cropWaterDemand > 0) {
					if(cropWaterUptake <= 0) 
						waterStress =  generalParameters.waterStressMin; //P_swfacmin;
					else 
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);						
				}
				
				waterStress = Math.max (waterStress,  generalParameters.waterStressMin);

			}
			//Store water stress in STICS senfac				
			crop.setHisafeWaterSenescenceStress(waterStress);
			crop.sticsCrop.senfac[0] = (float) waterStress;
			crop.sticsCrop.senfac[1] = (float) waterStress;

		}	
	
		//Restore real water stock in each voxels
		for (Iterator iter=stand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			SafeVoxel voxels[] = cell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
				voxels[iz].restoreWaterStock ();
			}
		}
	}
}
