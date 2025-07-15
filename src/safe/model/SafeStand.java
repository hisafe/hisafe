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
import java.util.Date;
import java.util.Iterator;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;

/**
 * STAND description 
 *
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeStand extends TreeList implements Serializable {

	/** simulation date start  */
	private Date startDate;				
	/** simulation julian day (0-730) */
	private int julianDay;				
	/** weather day  */
	private int weatherDay;					
	/** weather month  */
	private int weatherMonth;			
	/** weather year  */
	private int weatherYear;				

	public SafeStand () {
		super ();
		this.startDate = new Date();
	}

	/**
	 * Creates the plot 
	 * @param model Reference to GModel object
	 * @param cellWidth Cells width (m)
	 * @param nRows Number of cells rows
	 * @param nCols Number of cells columns
	 */
	public void createPlot (GModel model, double cellWidth, int nRows, int nCols) {
		SafePlot initPlot = new SafePlot (this, cellWidth, nRows, nCols);
		this.setPlot(initPlot);
	}

	/**
	 * Redefinition of getEvolutionBase to return a stand with cloned trees
	 */
	public GScene getEvolutionBase () {
		SafeStand newStand = (SafeStand) super.getHeavyClone ();
		return newStand;
	}
	

	/**
	 * Redefinition of getInterventionBase to return a stand with cloned trees
	 */
	public GScene getInterventionBase () {
		SafeStand newStand = (SafeStand) super.getHeavyClone ();
		return newStand;
	}
	
	/**
	 * Creation of all objets (cells, trees, soil, voxels) attached to the stand
	 * @param generalParameters Reference to SafeGeneralParameters object
	 */
	public void createAll (SafeGeneralParameters generalParameters) throws Exception {
	
		SafePlotSettings plotSettings = this.getPlot().getPlotSettings();
		SafePlot plot = this.getPlot();
		SafeSoil soil = this.getPlot().getSoil();
		
		// 1. Get plot dimensions
		double cellWidth 	= this.getPlot().getCellWidth();
		double plotWidth 	= plotSettings.plotWidth;
		double plotHeight 	= plotSettings.plotHeight;
		int nLin = (int) (plotHeight / cellWidth);
		int nCol = (int) (plotWidth / cellWidth);
		double voxelThicknessMax = soil.getVoxelThicknessMax();
		int nbVoxels = soil.getNbVoxels();
		int nbLayerCreated = soil.getNbLayers();


		//2. Cells creation depending of
		//   ne sert que pour amorcer l'EXPORT CropZone
		plot.initialiseCropZone ();
		
		//3. Cells creation depending of
		//    1) nbr of line and nbr of column on the plot
		//    2) cell width
		int id = 0;

		for (int i=0; i < nLin; i++) {
			for (int j=0; j < nCol; j++) {
				id = id + 1;
				double x = j * cellWidth;
				double y = (nLin - (i + 1)) * cellWidth;
				double z = zCoordinate(x, y, this.getPlot().getPlotSettings());
				Vertex3d coord = new Vertex3d(x, y, z);

				SafeCell cell = new SafeCell (plot, coord, i, j, id, nbVoxels);
				plot.addCell (cell);
			}
		}

		//4. Trees creation
		this.clearTrees();
		int nbTrees = this.getPlot().getPlotSettings().nbTrees;
		int idTree = 1;
		for (int i=0; i<nbTrees ; i++) {

			double xTree =  this.getPlot().getPlotSettings().treeX[i];
			double yTree =  this.getPlot().getPlotSettings().treeY[i];
			double zTree =  zCoordinate(xTree, yTree, this.getPlot().getPlotSettings());	//GT 2007 slope
			String treeSpeciesName = this.getPlot().getPlotSettings().treeSpecies[i];

			
			try {
					SafeTree tree = new SafeTree (this, 
											idTree,  
											treeSpeciesName,
											xTree, yTree, zTree, //GT 2007 slope
											generalParameters);
	
					this.addTree (tree);
					idTree++;
			}
			catch (Throwable e1) {
				throw e1;
			}
		}


		//5. Voxels creation for each cell of the plot
		int voxelID = 1;
		for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();

			int voxelIndex = 0;
			double voxelDepth = 0;

			//for each layer, calculation of voxel to create (number and thickness)
			for (int i=0; i< nbLayerCreated; i++) {
				SafeLayer layer = soil.getLayer(i);			//layer reference
				double layerThickness = layer.getThickness();				
				double voxelThickness = 0;

				//single voxel
				if (layerThickness <= voxelThicknessMax) {
			
					SafeVoxel voxel = new SafeVoxel (voxelID, voxelIndex, layer, cell,
							layerThickness, voxelDepth,
							nbTrees);
				
					cell.addVoxel(voxelIndex, voxel);
					
					voxelID++;
					voxelIndex++;
					voxelDepth += layerThickness;
					
					//to avoid rounding problems !!!!   IL 04/01/06
					voxelDepth =  (Math.round (voxelDepth * Math.pow (10,2)) ) / (Math.pow (10,2));

				}
				//several voxels
				else {

					//to avoid some problem with double numbers IL 22/02/2021
					int nbVoxelLayer = (int)(layerThickness*10/voxelThicknessMax*10)/100;

					int lt = (int) (layerThickness*1000);
					int vt = (int) (voxelThicknessMax*1000);
					if (lt%vt>0) nbVoxelLayer++;
					
					voxelThickness = voxelThicknessMax;

					//Firsts voxels
					for (int v=0; v < nbVoxelLayer-1; v++) {
					
						SafeVoxel voxel = new SafeVoxel (voxelID, voxelIndex, layer, cell,
														voxelThickness, voxelDepth,
														nbTrees);

						cell.addVoxel(voxelIndex, voxel);

						voxelID++;
						voxelIndex++;
						voxelDepth += voxelThickness;

						//to avoid rounding problems !!!!   IL 04/01/06
						voxelDepth =  (Math.round (voxelDepth * Math.pow (10,2)) ) / (Math.pow (10,2));
					}
					

					//last voxel for rounding total thickness to a cm multiple
					double reste = layerThickness - (voxelThickness*(nbVoxelLayer-1));
					reste =  (Math.round (reste * Math.pow (10,2)) ) / (Math.pow (10,2));
					
					SafeVoxel voxel = new SafeVoxel (voxelID, voxelIndex, layer, cell,
													reste, voxelDepth,   
													nbTrees
													);
					
					cell.addVoxel(voxelIndex, voxel);

					voxelID++;
					voxelIndex++;
					voxelDepth += reste;
					voxelDepth =  (Math.round (voxelDepth * Math.pow (10,2)) ) / (Math.pow (10,2));

				}
			}
		}

		soil.setNbVoxels(nbVoxels);

		
	}

	/**
	 * Initialization of trees management files (itk)
	 * @param evolutionParameters Reference to SafeEvolutionParameters object
	 */
	public void initialiseTreeItk (SafeEvolutionParameters evolutionParameters)  {
		
		for (Iterator iter1=this.getTrees().iterator(); iter1.hasNext(); ) {
			SafeTree tree = (SafeTree) iter1.next();
			try {
				tree.loadItk(evolutionParameters);
			} catch (Exception e2) {
		
				System.out.println("TREE ITK initialisation problem... simulation is canceled !");
				System.exit(1);
			}
		}
		
		
	}
	/**
	 * Initialization of the stand 
	 */
	public void initialisation () {

		SafePlot initPlot = (SafePlot) this.getPlot();

		// Initialisation of all other cells soil initial values
		for (Iterator c = initPlot.getCells ().iterator (); c.hasNext ();) {
			SafeCell cell = (SafeCell) c.next ();
			SafeVoxel[] voxels = cell.getVoxels();
			int nbVoxels = voxels.length;
			for (int i=0; i<nbVoxels ; i++) {

				//Water and nitrogen initialisation
				int nbLayer = voxels[i].getLayer().getId();
				double nProp = voxels[i].getThickness()/voxels[i].getLayer().getThickness();
				voxels[i].initializeWaterNitrogen (initPlot.getPlotSettings().layerWaterContent[nbLayer],
													initPlot.getPlotSettings().layerNo3Content[nbLayer]*nProp,
													initPlot.getPlotSettings().layerNh4Content[nbLayer]*nProp);		
			}
		}

	}

	/**
	 * Reload the tree species file in case of reopening a project 
	 * @param ep Reference to SafeEvolutionParameters object
	 * @param generalParameters Reference to SafeGeneralParameters object
	 */
	public void reloadTreeSpecies (SafeEvolutionParameters ep, SafeGeneralParameters generalParameters) throws Exception {
		
		for (Iterator iter1=this.getTrees().iterator(); iter1.hasNext(); ) {
			SafeTree tree = (SafeTree) iter1.next();
			tree.reloadSpecies(ep, generalParameters, tree.getTreeSpecies().getFileName());
		}
	}
	/**
	 * Search all cells with trees above and calculate lai of tree above each cell
	 **/
	public void computeLaiAboveCells () {

		for (Iterator iter=this.getPlot().getCells().iterator(); iter.hasNext(); ) {
			
			SafeCell cell = (SafeCell) iter.next();

			double cellX = cell.getXCenter();		//Gravity center of the cell
			double cellY = cell.getYCenter();
			cell.setIsTreeAbove (false);
			cell.setLaiTree (0);
			cell.razTreeAbove();
			
			for (Iterator iter1=this.getTrees().iterator(); iter1.hasNext(); ) {
				
				SafeTree tree = (SafeTree) iter1.next();
				if (tree.isPlanted() && !tree.isHarvested()) {
					double treeX = tree.getX ();		//tree coordinates
					double treeY = tree.getY ();
					double crownRadiusTreeLine = tree.getCrownRadiusTreeLine ();
					double crownRadiusInterRow = tree.getCrownRadiusInterRow ();

					//correction for toric symetry (IL 16/05/2025)
					double distX = Math.abs (treeX - cellX) ;
					double distY = Math.abs (treeY - cellY) ;
					distX = Math.min(distX, Math.abs(distX - this.getPlot().getXSize()));
					distY = Math.min(distY, Math.abs(distY - this.getPlot().getYSize()));
					
					
					// The cell gravity center is in the crown shape projection
					if ((Math.pow(distX ,2) / Math.pow(crownRadiusInterRow,2))
						+ (Math.pow (distY ,2) / Math.pow(crownRadiusTreeLine,2))
					< 1 ) {
						
						cell.setIsTreeAbove (true);
						cell.addTreeAbove (tree);
						cell.addLaiTree (tree.getLai());
						tree.addNbCellsBellow (1);
					}
				}
				
			}
		}

	}	
   
	
 	
    /**
	 * Check if a tree roots have colonized the all scene (at least one voxel for each cell) 
	* @param treeID Id of the tree 
	 */
    public boolean isAllColonised (int treeID)  {

    	boolean retour = true; 
		SafePlot plot = (SafePlot) this.getPlot();

		//STICS initialisation for each cell
		for (Iterator c = plot.getCells().iterator(); (c.hasNext() && retour);) {
			SafeCell cell = (SafeCell) c.next ();
			retour = cell.isColonised(treeID);
		}
		return retour;
    }
    
    
   /**
	* Tree foliage Carbon Litter spread on all plot (kg) 
	*/
	public double getTreesCarbonFoliageLitterAllPlot () {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getCarbonFoliageLitterAllPlot();
		}
		return total;
	}
   /**
	* Tree foliage Nitrogen Litter spread on all plot(kg) 
	*/
	public double getTreesNitrogenFoliageLitterAllPlot () {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getNitrogenFoliageLitterAllPlot();
		}
		return total;
	}

	
   /**
	* Tree branches Carbon Litter spread on all plot (kg) 
	*/
	public double getTreesCarbonBranchesLitterAllPlot () {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getCarbonBranchesLitterAllPlot();
		}
		return total;
	}
   /**
	* Tree branches Nitrogen Litter spread on all plot (kg) 
	*/
	public double getTreesNitrogenBranchesLitterAllPlot () {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getNitrogenBranchesLitterAllPlot();
		}
		return total;
	}
		
	/**
	* Tree fine roots Carbon Litter (kg) 
	*/
	public double getTreesCarbonFineRootsLitter () {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getCarbonFineRootsSen();
		}
		return total;
	}
   /**
	* Tree fine roots  Nitrogen Litter (kg) 
	*/
	public double getTreesNitrogenFineRootsLitter() {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getNitrogenFineRootsSen();
		}
		return total;
	}
		
	/**
	* Tree Coarse roots Carbon Litter (kg) 
	*/
	public double getTreesCarbonCoarseRootsLitter() {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getCarbonCoarseRootsSen();
		}
		return total;
	}
   /**
	* Tree Coarse roots  Nitrogen Litter (kg) 
	*/
	public double getTreesNitrogenCoarseRootsLitter() {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getNitrogenCoarseRootsSen();
		}
		return total;
	}
   /**
	* Tree fruit Carbon Litter (kg) 
	*/
	public double getTreesCarbonFruitLitterAllPlot() {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getCarbonFruitLitterAllPlot();
		}
		return total;
	}
   /**
	* Tree fruit Nitrogen litter (kg) 
	*/
	public double getTreesNitrogenFruitLitterAllPlot() {
		double total = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			total += t.getNitrogenFruitLitterAllPlot();
		}
		return total;
	}
		
		
   /**
	* Tree max root depth
	*/
	public double getTreesMaxRootDepth () {
		double max = 0; 
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			if (t.isPlanted() && !t.isHarvested()) max = Math.max(max,t.getRootingDepth());
		}
		return max;
	}	

	public Date getStartDate () {return startDate;}
	
	public int getWeatherDay () {return weatherDay;}
	public int getWeatherMonth () {return weatherMonth;}
	public int getWeatherYear () {return weatherYear;}
	public int getJulianDay () {return julianDay;}

	public String getCaption () {
		String caption = "";
		if (isInterventionResult ()) {caption += "*";}
		caption += weatherDay + "/" +weatherMonth+ "/" + weatherYear;
		return caption;
	}

	public void setStartDate (Date d) {startDate = d;}

	public void setJulianDay (int d) {julianDay = d;}
	public void setWeatherDay (int d) {weatherDay = d;}
	public void setWeatherMonth (int d) {weatherMonth = d;}
	public void setWeatherYear (int d) {weatherYear = d;}


	/**
	* Compute z coordinate of a point (x,y).
	*/
	public static double zCoordinate (double x, double y, SafePlotSettings plotSettings) {
		double slope = Math.toRadians(plotSettings.slopeIntensity);
		double treeLineOrientation = plotSettings.treeLineOrientation;					
		double slopeAspect	= plotSettings.slopeAspect;									
		double bottomAzimut = Math.toRadians(-90+treeLineOrientation-slopeAspect);
		double z = -Math.tan(slope)*(x*Math.cos(bottomAzimut)+y*Math.sin(bottomAzimut));
		return z;
	}

	@Override
	public SafePlot getPlot () {
		return (SafePlot) plot; // fc-30.10.2017
	}

}
