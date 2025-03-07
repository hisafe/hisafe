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

import java.util.Iterator;

public class SafeWaterCompetitionModel  {

	/**
	 * WATER AND NITROGEN REPARTITION PROCESS
	 * 
	 * @author M.Van NOORDWIJK  - ICRAF Bogor Indonisia FROM JULY 2004 TO MAY 2005
	 * @author D.HARJA          - ICRAF Bogor Indonisia
	 * @author I.LECOMTE        - INRAE Montpellier France
	 * 
	 **/
	public static void waterNitrogenRepartition (SafeStand newStand,
												SafeGeneralParameters safeSettings,
												int day) {
		
		int nbTrees = safeSettings.nbTrees;
		double plotArea = newStand.getArea();
		double cellArea = 0;

		//FOR EACH PLANT (tree ou crop)  calculate WATER DEMAND REDUICE and POTENTIEL 
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() && !tree.isHarvested()) {
				
				double treeTotalRootLenght = tree.computeTotalRootLength();
				if (treeTotalRootLenght>0) tree.computePlantWaterPotential(safeSettings);
				
				if (tree.getWaterDemand () > 0) {
					tree.getPlantRoots().calculatePotential (safeSettings, tree.getWaterDemand ());

					// Reduction factor for water demand
					double waterDemandReductionFactor = tree.getPlantRoots().getWaterDemandReductionFactor();
					tree.setWaterDemandReduced (tree.getWaterDemand() * waterDemandReductionFactor);		//liters
				}
			}
		}

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {

			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop = cell.getCrop();
			
			double cropTotalRootLenght = crop.computeTotalRootsLength();
			if (cropTotalRootLenght>0) crop.computePlantWaterPotential(safeSettings);
			
			//Reduction factor for water demand (dimensionless)
			if (crop.getWaterDemand () > 0) {		//in mm
				crop.getPlantRoots().calculatePotential (safeSettings, crop.getWaterDemand()* (float)cell.getArea());		// gt - 12.11.2009 - water demand is now expressed in liters instead of mm
				double waterDemandReductionFactor = crop.getPlantRoots().getWaterDemandReductionFactor();
				crop.setWaterDemandReduced (crop.getWaterDemand() * waterDemandReductionFactor);		//mm
			}
		}
		
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			
			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				//root map creation
				voxels[iz].initRootMap();
				
				//for the crop
				if (voxels[iz].getCropRootsDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, safeSettings, voxels[iz]);
				}

				//Same for trees roots
				for (int t=0; t < nbTrees;t++) {

					if (voxels[iz].getTheTreeRootsDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (newStand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, safeSettings,voxels[iz]);
						}
					}
				}
			}
		}

		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].razWaterUptakePotential ();
			}
		}	
		
		//FOR EACH VOXEL, CALCULATION OF WATER AND NITROGEN UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].countWaterUptakePotential (newStand, safeSettings, cellArea, plotArea, day, true);
					voxels[iz].countNitrogenUptakePotential (newStand, safeSettings);	
			}
		}


		//FOR EACH PLANT, CALCULATION OF PLANTS WATER AND NITROGEN UPTAKE
		//for each crop
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			cellArea = cell.getArea();
			SafeCrop crop	= cell.getCrop();
			SafePlantRoot cropRoot = crop.getPlantRoots();
			int phenologicalStage = crop.getPhenologicStageVegetative();
			//WATER

			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cellArea;	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cellArea;				//liters
			
			crop.setWaterUptake (0);
			double waterStress = 1;
			
			//water extraction is computed if demand is > 0 and if  crop is NOT harvested 
			if ((cropWaterDemandReduced > 0) && (phenologicalStage < 11)) {

				double cropWaterUptake = cropRoot.calculateWaterUptake (newStand, crop, safeSettings, cropWaterDemandReduced, day);
				crop.setWaterUptake (cropWaterUptake / cellArea);					 	//mm

				if (cropWaterDemand > 0) {
					if(cropWaterUptake <= 0) {
						waterStress = safeSettings.stressMin; //P_swfacmin;						
					}
					else {
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);		
					}
					waterStress = Math.max (waterStress,  safeSettings.stressMin);
				}

				//Ajout influence sur STICS
				crop.sticsCrop.ep[0] = (float) crop.getWaterUptake() ;
				crop.sticsCrop.ep[1] = (float) crop.getWaterUptake() ;	
			}
			else {
				//if cropWaterDemand = 0 stress is very high 
				if ((cropWaterDemand > 0) && (cropWaterDemandReduced == 0) ) {
					waterStress = 0.0001d; 
				}
			}
			//Set the stress 
			crop.setHisafeWaterStress (waterStress);
			
			//Also in STICS	
			crop.sticsCrop.swfac[0] = (float) waterStress;
			crop.sticsCrop.swfac[1] = (float) waterStress;


			//NITROGEN		
			double cropNitrogenDemand  = (crop.getNitrogenDemand() / 10) * cellArea;	//convert kg ha-1 in g
			crop.setNitrogenUptake (0);
			double nitrogenStress = 1;
			
			if (cropNitrogenDemand > 0) {
				double cropNitrogenUptake = cropRoot.calculateNitrogenUptake (newStand, crop, safeSettings, cropNitrogenDemand);
				

				crop.setNitrogenUptake ((cropNitrogenUptake * 10) / cellArea);		//convert g in kg ha-1
				if(cropNitrogenUptake <= 0) {
					nitrogenStress = 0.0001d;
				}
				else {
					nitrogenStress = Math.min (cropNitrogenUptake  / cropNitrogenDemand, 1);
					nitrogenStress = Math.max (nitrogenStress,  0.0001d);
				}
			}
			crop.setHisafeNitrogenStress (nitrogenStress);
			//Ajout influence sur STICS
			//crop.sticsCrop.innlai[0] = (float) nitrogenStress;
			//crop.sticsCrop.innlai[1] = (float) nitrogenStress;
		
		}

		//for each tree
		for (Iterator iter=newStand.getTrees().iterator(); iter.hasNext(); ) {
			SafeTree tree = (SafeTree)iter.next();
			if (tree.isPlanted() && !tree.isHarvested()) {
				SafePlantRoot treeRoot = (SafePlantRoot) tree.getPlantRoots();
	
				//WATER
				tree.setWaterUptake (0);
				double treeWaterDemand = tree.getWaterDemandReduced ();		//liters
				if (treeWaterDemand > 0) {
					double treeWaterUptake = treeRoot.calculateWaterUptake (newStand, tree, safeSettings, treeWaterDemand, day);
				}
				
	
				//NITROGEN UPTAKE
				tree.setNitrogenAvailable (0);
				tree.setNitrogenUptake (0);
				
				double treeNitrogenDemand = tree.getNitrogenDemandAfterFixation() * 1000;	//convert kg in g;
				if (treeNitrogenDemand > 0) {
					//calcul de setNitrogenUptakeWithoutFixation dans ce code
					double value = treeRoot.calculateNitrogenUptake (newStand, tree, safeSettings, treeNitrogenDemand);
					
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
	 **/	
	public static void waterNitrogenRepartitionTurfac (SafeStand newStand,
			SafeGeneralParameters safeSettings,
			int day) {

		int nbTrees = safeSettings.nbTrees;
		double plotArea = newStand.getArea();
		double cellArea = 0;

		//FOR EACH VOXEL, CALCULATION OF WATER STOCK REDUICED OF 20%
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();

			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {
				double psisto = cell.getCrop().sticsCrop.P_psisto;
				double psiturg = cell.getCrop().sticsCrop.P_psiturg;
				voxels[iz].computeWaterStockTurfac (psisto, psiturg);	
				voxels[iz].setWaterStock(voxels[iz].getWaterStockTurfac());			
			}
		}

		
		//FOR EACH PLANT (tree ou crop)  calculate WATER DEMAND REDUICE and POTENTIEL 
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() && !tree.isHarvested()) {
				
				double treeTotalRootLenght = tree.computeTotalRootLength();
				if (treeTotalRootLenght>0) tree.computePlantWaterPotential(safeSettings);
				
				if (tree.getWaterDemand () > 0) {
					tree.getPlantRoots().calculatePotential (safeSettings, tree.getWaterDemand ());

					// Reduction factor for water demand
					double waterDemandReductionFactor = tree.getPlantRoots().getWaterDemandReductionFactor();
					tree.setWaterDemandReduced (tree.getWaterDemand() * waterDemandReductionFactor);		//liters
				}
			}
		}
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {

			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop = cell.getCrop();
			double cropTotalRootLenght = crop.computeTotalRootsLength();
			if (cropTotalRootLenght>0) crop.computePlantWaterPotential(safeSettings);
			
			//Reduction factor for water demand (dimensionless)
			if (crop.getWaterDemand () > 0) {		//in mm
				crop.getPlantRoots().calculatePotential (safeSettings, crop.getWaterDemand()*cell.getArea());		// gt - 12.11.2009 - water demand is now expressed in liters instead of mm

				double waterDemandReductionFactor = crop.getPlantRoots().getWaterDemandReductionFactor();
				crop.setWaterDemandReduced (crop.getWaterDemand() * waterDemandReductionFactor);		//mm

			}
		}
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			
			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				//root map creation
				voxels[iz].initRootMap();
				
				//for the crop
				if (voxels[iz].getCropRootsDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, safeSettings, voxels[iz]);
				}

				//Same for trees roots
				for (int t=0; t < nbTrees;t++) {

					if (voxels[iz].getTheTreeRootsDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (newStand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, safeSettings,voxels[iz]);
						}
					}
				}
			}
		}
			
		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].razWaterUptakePotential ();
			}
		}	

		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].countWaterUptakePotential (newStand, safeSettings, cellArea, plotArea, day, false);
			}
		}
			

	

		//FOR EACH PLANT, CALCULATION OF PLANTS WATER AND NITROGEN UPTAKE
		//for each crop
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			cellArea = cell.getArea();
			SafeCrop crop	= cell.getCrop();
			SafePlantRoot cropRoot = crop.getPlantRoots();
			int phenologicalStage = crop.getPhenologicStageVegetative();
			//WATER

			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cellArea;	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cellArea;				//liters

			double waterStress = 1;
			//on calcule l'extraction si la demande n'est pas null et qu'on a pas atteint le stade récolte 
			if ((cropWaterDemandReduced > 0) && (phenologicalStage < 11)) {
				double cropWaterUptake = cropRoot.calculateWaterUptake2 (newStand, crop, safeSettings, cropWaterDemandReduced, day);

				if (cropWaterDemand > 0) {
					if(cropWaterUptake <= 0) {
						waterStress =  safeSettings.stressMin; //P_swfacmin;

					}
					else {
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);						
					}
				}

			}

			//Ajout influence sur STICS					
			waterStress = Math.max (waterStress,  safeSettings.stressMin);
			
			crop.setHisafeTurfac(waterStress);
			crop.sticsCrop.turfac[0] = (float) waterStress;
			crop.sticsCrop.turfac[1] = (float) waterStress;

		}	
	
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();

			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				voxels[iz].resetWaterStockNormal ();

			}
		}
	}
	
	/**
	 * SENFAC CALCULATION (crop senescence stress) 
	 * ADDED in JUNE 2020 
	 **/
	public static void waterNitrogenRepartitionSenfac (SafeStand newStand,
			SafeGeneralParameters safeSettings,
			int day) {

		int nbTrees = safeSettings.nbTrees;
		double plotArea = newStand.getArea();
		double cellArea = 0;

		//FOR EACH VOXEL, CALCULATION OF WATER STOCK REDUICED OF 20%
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();

			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {
				double psisto = cell.getCrop().sticsCrop.P_psisto;
				double psiturg = cell.getCrop().sticsCrop.P_psiturg;
				voxels[iz].computeWaterStockSenfac (psisto, psiturg);
				voxels[iz].setWaterStock(voxels[iz].getWaterStockSenfac());	
			}
		}

		
		//FOR EACH PLANT (tree ou crop)  calculate WATER DEMAND REDUICE and POTENTIEL 
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() && !tree.isHarvested()) {
				
				double treeTotalRootLenght = tree.computeTotalRootLength();
				if (treeTotalRootLenght>0) tree.computePlantWaterPotential(safeSettings);
				
				
				if (tree.getWaterDemand () > 0) {
					tree.getPlantRoots().calculatePotential (safeSettings, tree.getWaterDemand ());

					// Reduction factor for water demand
					double waterDemandReductionFactor = tree.getPlantRoots().getWaterDemandReductionFactor();
					tree.setWaterDemandReduced (tree.getWaterDemand() * waterDemandReductionFactor);		//liters
				}
			}
		}
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {

			SafeCell cell	= (SafeCell) iter.next();
			SafeCrop crop = cell.getCrop();
			double cropTotalRootLenght = crop.computeTotalRootsLength();
			if (cropTotalRootLenght>0) crop.computePlantWaterPotential(safeSettings);
			
			//Reduction factor for water demand (dimensionless)
			if (crop.getWaterDemand () > 0) {		//in mm
				crop.getPlantRoots().calculatePotential (safeSettings, crop.getWaterDemand()*cell.getArea());		// gt - 12.11.2009 - water demand is now expressed in liters instead of mm

				double waterDemandReductionFactor = crop.getPlantRoots().getWaterDemandReductionFactor();
				crop.setWaterDemandReduced (crop.getWaterDemand() * waterDemandReductionFactor);		//mm

			}
		}
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			
			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				//root map creation
				voxels[iz].initRootMap();
				
				
				//for the crop
				if (voxels[iz].getCropRootsDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, safeSettings, voxels[iz]);
				}

				//Same for trees roots
				for (int t=0; t < nbTrees;t++) {

					if (voxels[iz].getTheTreeRootsDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (newStand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, safeSettings,voxels[iz]);
						}
					}
				}
			}
		}
			
		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].razWaterUptakePotential ();
			}
		}	

		//FOR EACH VOXEL, CALCULATION OF WATER UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].countWaterUptakePotential (newStand, safeSettings, cellArea, plotArea, day, false);
			}
		}
			
		//FOR EACH PLANT, CALCULATION OF PLANTS WATER AND NITROGEN UPTAKE
		//for each crop
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			cellArea = cell.getArea();
			SafeCrop crop	= cell.getCrop();

			SafePlantRoot cropRoot = crop.getPlantRoots();
			int phenologicalStage = crop.getPhenologicStageVegetative();
			
			//WATER

			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cellArea;	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cellArea;				//liters
			double waterStress = 1;
			
			//on calcule l'extraction si la demande n'est pas null et qu'on a pas atteint le stade récolte 
			//et si on a dejà du stress stomatique
			if ((cropWaterDemandReduced > 0) && (phenologicalStage < 11) &&  (crop.getHisafeWaterStress() < 1)) {
				
				double cropWaterUptake = cropRoot.calculateWaterUptake2 (newStand, crop, safeSettings, cropWaterDemandReduced, day);

				if (cropWaterDemand > 0) {
					if(cropWaterUptake <= 0) {
						waterStress =  safeSettings.stressMin; //P_swfacmin;

					}
					else {
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);						
					}
				}

			}
			//Ajout influence sur STICS					
			waterStress = Math.max (waterStress,  safeSettings.stressMin);
			
			crop.setHisafeSenfac(waterStress);
			crop.sticsCrop.senfac[0] = (float) waterStress;
			crop.sticsCrop.senfac[1] = (float) waterStress;
			

		}	
	
		
		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();

			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				voxels[iz].resetWaterStockNormal ();

			}
		}
	}
}
