package safe.model;

import java.util.Iterator;


public class SafeWaterCompetitionModel  {

	/**
	 * WATER AND NITROGEN REPARTITION PROCESS
	 * @author
	 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia
	 * B.LUSIANA        - ICRAF Bogor Indonisia
	 * D.HARJA          - ICRAF Bogor Indonisia
	 * I.LECOMTE        - INRA Montpellier France
	 * FROM JULY 2004 TO MAY 2005
	 **/
	public static void waterNitrogenRepartition (SafeStand newStand,
												SafeInitialParameters safeSettings,
												int day) {


		int nbTrees = safeSettings.nbTrees;
		double plotArea = newStand.getArea();
		double cellArea = 0;

		//FOR EACH VOXEL, CALCULATION OF PRESSURE HEAD IN SOIL AT PLANT ROOT SURFACE (cm)

		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			
			SafeVoxel voxels[] = cell.getVoxels();

			for (int iz=0; iz<voxels.length; iz++) {

				//for the crop
				if (voxels[iz].getCropRootDensity () > 0) {
					SafeCrop crop = cell.getCrop();
					voxels[iz].computePlantRhizospherePotential (crop, safeSettings, voxels[iz]);
				}

				//Same for trees roots
				for (int t=0; t < nbTrees;t++) {

					if (voxels[iz].getTheTreeRootDensity(t) > 0) {
						int treeId = t+1;
						SafeTree tree = (SafeTree) (newStand.getTree (treeId));
						if (tree != null) {					//tree can be missing after a thinning intervention
							voxels[iz].computePlantRhizospherePotential (tree, safeSettings,voxels[iz]);
						}
					}
				}
			}
		}


		//FOR EACH VOXEL, CALCULATION OF WATER AND NITROGEN UPTAKE POTENTIAL
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell theCell	= (SafeCell)iter.next();
			cellArea = theCell.getArea();
			SafeVoxel voxels[] = theCell.getVoxels();
			for (int iz=0; iz<voxels.length; iz++) {
					voxels[iz].countWaterUptakePotential (newStand, safeSettings, cellArea, plotArea, day);
					voxels[iz].countNitrogenUptakePotential (newStand, safeSettings);
			}
		}


		//FOR EACH PLANT, CALCULATION OF PLANTS WATER AND NITROGEN UPTAKE
		//for each crop
		for (Iterator iter=newStand.getPlot().getCells().iterator(); iter.hasNext();) {
			SafeCell cell	= (SafeCell) iter.next();
			cellArea = cell.getArea();
			SafeCrop crop	= cell.getCrop();
			SafeFineRoot cropRoot = crop.getFineRoots();
			int phenologicalStage = crop.getPhenologicStage();
			//WATER

			double cropWaterDemandReduced  = crop.getWaterDemandReduced ()  * cellArea;	//liters
			double cropWaterDemand  = crop.getWaterDemand ()  * cellArea;				//liters
			
			
			crop.setWaterUptake (0);
			double waterStress = 1;
			//on calcule l'extraction si la demande n'est pas null et qu'on a pas atteint le stade récolte 
			if ((cropWaterDemandReduced > 0) && (phenologicalStage < 11)) {
				double cropWaterUptake = cropRoot.calculateWaterUptake (newStand, crop, safeSettings, cropWaterDemandReduced, day);
				crop.setWaterUptake (cropWaterUptake / cellArea);					 	//mm
				if (cropWaterDemand > 0) {
					if(cropWaterUptake <= 0) {
						waterStress = 0.0001d;
					}
					else {
						waterStress = Math.min (cropWaterUptake  / cropWaterDemand, 1);
						waterStress = Math.max (waterStress, 0.0001d);
					}
				}
			}
			crop.setHisafeWaterStress (waterStress);
			//Ajout influence sur STICS
			//crop.sticsCrop.swfac[0] = (float) waterStress;
			//crop.sticsCrop.swfac[1] = (float) waterStress;

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
					nitrogenStress = Math.max (nitrogenStress, 0.0001d);
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
			SafeFineRoot treeRoot = (SafeFineRoot) tree.getFineRoots();

			//WATER
			tree.setWaterUptake (0);
			double treeWaterDemand = tree.getWaterDemandReduced ();		//liters
			if (treeWaterDemand > 0) {
				double treeWaterUptake = treeRoot.calculateWaterUptake (newStand, tree, safeSettings, treeWaterDemand, day);
			}
			

			//NITROGEN
			tree.setNitrogenUptake (0);
			double treeNitrogenDemand = tree.getNitrogenDemand() * 1000;	//convert kg in g;
			if (treeNitrogenDemand > 0) {
				double treeNitrogenUptake = treeRoot.calculateNitrogenUptake (newStand, tree, safeSettings, treeNitrogenDemand);
			}
			
		}
	}


}
