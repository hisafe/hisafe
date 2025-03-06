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
import java.util.Date;
import java.util.Iterator;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;


/**
 * STAND description 
 *
 * @author Isabelle Lecomte - INRA Montpellier France - July 2002
 */
public class SafeStand extends TreeList {

	private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a Stand
	 * A class Inmmutable already exist in super class !!!!
	 */
	public static class Immutable2 implements Cloneable, Serializable {
		private static final long serialVersionUID = 1L;
		public float latitude;		//degree
		public float elevation;		//m
	}

	protected Immutable2 immutable2;

	private Date startDate;					//date et heure du jour debut de simulation (peut être modifiée si on reouvre le projet) 
	private int julianDay;					//julianDay of simulation (0-730)
	private int weatherDay;					//julianDay of weather (1-365)
	private int weatherMonth;				//month of weather
	private int weatherYear;				//year of weather

	
	/** TO DISPLAY IN SCENARIO INSPECTOR **/
	private String display;
	

	// WARNING: if references to objects (not primitive types) are added here,
	// implement a "public Object clone ()" method (see RectangularPlot.clone () for template)

	public SafeStand (double latitude, double elevation) {
		super ();

		createImmutable2 ();

		immutable2.latitude  = (float) latitude;
		immutable2.elevation = (float) elevation;
		this.startDate = new Date();
	}

	/**
	 * Create an Immutable object whose class is declared at one level of the hierarchy.
	 * This is called only in constructor for new logical object in superclass.
	 * If an Immutable is declared in subclass, subclass must redefine this method
	 * (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable2 () {immutable2 = new Immutable2 ();}


	/**
	 * Creates the plot and cells.
	 */
	public void createPlot (GModel model, double cellWidth) { }

	/**
	 * Redefinition of getEvolutionBase to return a stand WITH CLONED TREES
	 */
	public GScene getEvolutionBase () {
		SafeStand newStand = (SafeStand) super.getHeavyClone ();
		newStand.display = null;
		return newStand;
	}
	

	/**
	 * Redefinition of getInterventionBase to return a stand WITH CLONED TREES
	 */
	public GScene getInterventionBase () {
		SafeStand newStand = (SafeStand) super.getHeavyClone ();
		newStand.display = null;
		return newStand;
	}
	
	/**
	 * Creation of all objets (plot, trees, soil, voxels) attached to the stand
	 */
	public void createAll (SafeGeneralParameters safeSettings, 
							SafePlotSettings plotSettings) throws Exception {
	
		// 1. PlotOfCells creation
		double aspect 			   	= plotSettings.slopeAspect;
		double slope  			   	= plotSettings.slopeIntensity;
		double treeLineOrientation 	= plotSettings.treeLineOrientation;	
		double northOrientation 	= plotSettings.northOrientation;
		double cellWidth 			= plotSettings.cellWidth;

		double plotWidth 	= plotSettings.plotWidth;
		double plotHeight 	= plotSettings.plotHeight;
		int nLin = (int) (plotHeight / cellWidth);
		int nCol = (int) (plotWidth / cellWidth);
		
		SafePlot initPlot = new SafePlot (this, safeSettings, cellWidth, nLin, nCol,
				  aspect, slope, 
				  northOrientation, treeLineOrientation);
		
		this.setPlot(initPlot);
		double cellSurface = initPlot.getCellSurface ();
		plotSettings.cellSurface = cellSurface;

		//2. Soil creation
		//general parameters
		SafeSoil soil = new SafeSoil (safeSettings, plotSettings);		

		initPlot.setSoil(soil);

		//3. Layers creation
		int nbLayerMax = safeSettings.NB_LAYER_MAX;
		double voxelThicknessMax = plotSettings.voxelThicknessMax;
		int nbVoxels = 0;
		int nbLayerCreated = 0;
		double surfaceDepth = 0;
		
		for (int nbLayer=0; nbLayer<nbLayerMax; nbLayer++) {
			double layerThickness 		= plotSettings.layerThickness[nbLayer];
			double sand 			= plotSettings.layerSand[nbLayer];
			double clay 			= plotSettings.layerClay[nbLayer];
			double limestone 		= plotSettings.layerLimeStone[nbLayer];
			double organicMatter 	= plotSettings.layerOrganicMatter[nbLayer];
			double partSizeSand 	= plotSettings.layerPartSizeSand[nbLayer];
			double layerStone 		= plotSettings.layerStone[nbLayer];
			double infiltrability 	= plotSettings.layerInfiltrability[nbLayer];	
			int stoneType 			= plotSettings.layerStoneType[nbLayer];
			
			if (layerThickness > 0) {
				SafeLayer layer = new SafeLayer(nbLayer, surfaceDepth, layerThickness,
								sand, clay, limestone, organicMatter,
								partSizeSand, layerStone, stoneType, infiltrability,
								safeSettings);
				
				soil.putLayer (nbLayer, layer);
				nbLayerCreated++;

				soil.addDepth (layerThickness);	//cumulation of total soil depth
				double volume = soil.getDepth() * plotWidth * plotHeight;
				soil.setVolume (volume);
				surfaceDepth += layerThickness;

				//to avoid some problem with double numbers IL 22/02/2021
				surfaceDepth =  (Math.round (surfaceDepth * Math.pow (10,2)) ) / (Math.pow (10,2));

				//nb  voxel calculation depending on layer thickness and voxelThicknessMax
				if (layerThickness <= voxelThicknessMax) {
					nbVoxels++; 
				}
				else {
					//to avoid some problem with double numbers IL 22/02/2021
					int nbVoxelLayer = (int)(layerThickness*10/voxelThicknessMax*10)/100;
					int lt = (int) (layerThickness*1000);
					int vt = (int) (voxelThicknessMax*1000);
					if (lt%vt>0) nbVoxelLayer++;

					nbVoxels += nbVoxelLayer;
				}
			}
		}
		//4. Cells creation depending of
		//   ne sert que pour amorcer l'EXPORT CropZone
		initPlot.initialiseCropZone ();
		
		//5. Cells creation depending of
		//    1) nbr of line and nbr of column on the plot
		//    2) cell width
		int id = 0;

		for (int i=0; i < nLin; i++) {
			for (int j=0; j < nCol; j++) {
				id = id + 1;
				double x = j * cellWidth;
				double y = (nLin - (i + 1)) * cellWidth;
				double z = zCoordinate(x, y, plotSettings);
				Vertex3d coord = new Vertex3d(x, y, z);

				SafeCell cell = new SafeCell (initPlot, coord, i, j, id, nbVoxels);
				initPlot.addCell (cell);
			}
		}

		//6. Trees creation
		this.clearTrees();
		int nbTrees = plotSettings.nbTrees;
		int idTree = 1;
		for (int i=0; i<nbTrees ; i++) {

			double xTree =  plotSettings.treeX[i];
			double yTree =  plotSettings.treeY[i];
			double zTree =  zCoordinate(xTree, yTree, plotSettings);	//GT 2007 slope
			String treeSpeciesName = plotSettings.treeSpecies[i];

			
			try {
					SafeTree tree = new SafeTree (this, 
											idTree,  
											treeSpeciesName,
											xTree, yTree, zTree, //GT 2007 slope
											safeSettings);
	
					this.addTree (tree);
					idTree++;
			}
			catch (Throwable e1) {
				throw e1;
			}
		}


		//7. Voxels creation for each cell of the plot
		int voxelID = 1;
		for (Iterator c = initPlot.getCells ().iterator (); c.hasNext ();) {
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
			
					SafeVoxel voxel = new SafeVoxel (voxelID, layer, cell,
							layerThickness, voxelDepth,
							nbTrees);
				
					cell.addVoxel(voxelIndex, voxel);
					layer.addVoxel(voxel);
					
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
					
						SafeVoxel voxel = new SafeVoxel (voxelID, layer, cell,
														voxelThickness, voxelDepth,
														nbTrees);

						cell.addVoxel(voxelIndex, voxel);
						layer.addVoxel(voxel);

						voxelID++;
						voxelIndex++;
						voxelDepth += voxelThickness;

						//to avoid rounding problems !!!!   IL 04/01/06
						voxelDepth =  (Math.round (voxelDepth * Math.pow (10,2)) ) / (Math.pow (10,2));
					}
					

					//last voxel for rounding total thickness to a cm multiple
					double reste = layerThickness - (voxelThickness*(nbVoxelLayer-1));
					reste =  (Math.round (reste * Math.pow (10,2)) ) / (Math.pow (10,2));
					
					SafeVoxel voxel = new SafeVoxel (voxelID, layer, cell,
													reste, voxelDepth,   
													nbTrees
													);
					
					cell.addVoxel(voxelIndex, voxel);
					layer.addVoxel(voxel);

					voxelID++;
					voxelIndex++;
					voxelDepth += reste;
					voxelDepth =  (Math.round (voxelDepth * Math.pow (10,2)) ) / (Math.pow (10,2));

				}
			}
		}

		soil.setNbVoxels(nbVoxels);
		safeSettings.nbTrees=nbTrees;
		
	}

	/**
	 * Initialisation of treeITK
	 */	
	public void initialiseTreeItk (SafeEvolutionParameters evolutionParameters, SafeGeneralParameters safeSettings)  {
		
		for (Iterator iter1=this.getTrees().iterator(); iter1.hasNext(); ) {
			SafeTree tree = (SafeTree) iter1.next();
			try {
			tree.loadItk(evolutionParameters, safeSettings);
			} catch (Exception e2) {
		
				System.out.println("TREE ITK initialisation problem... simulation is canceled !");
				System.exit(1);
			}
		}
		
		
	}
	/**
	 * Stand initialisation at the beginning of simulation
	 */
	public void initialisation (SafeGeneralParameters safeSettings,  SafePlotSettings plotSettings) {

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
				voxels[i].initializeWaterNitrogen (plotSettings.layerWaterContent[nbLayer],
													plotSettings.layerNo3Content[nbLayer]*nProp,
													plotSettings.layerNh4Content[nbLayer]*nProp);		
			}
		}

	}

	/**
	 * Creation of all objets (plot, trees, soil, voxels) attached to the stand
	 */
	public void reloadTreeSpecies (SafeEvolutionParameters ep, SafeGeneralParameters safeSettings) throws Exception {
		
		for (Iterator iter1=this.getTrees().iterator(); iter1.hasNext(); ) {
			SafeTree tree = (SafeTree) iter1.next();
			tree.reloadSpecies(ep, safeSettings, tree.getTreeSpecies().getFileName());
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
		

					// The cell gravity center is in the crown shape projection
					if ((Math.pow(cellX - treeX ,2) / Math.pow(crownRadiusInterRow,2))
						+ (Math.pow (cellY - treeY ,2) / Math.pow(crownRadiusTreeLine,2))
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
	* Tree  roots pruning after soil management - gt-09.07.2009
	*  In case of soil management, some fine roots are removed from trees.
	*  If soil management depth is below the gravity center of a voxel
	*  the coarse root in this voxel and all depending topology are removed
	*/

	public void treeRootSoilManagement (SafeCell cell, float soilManagementDepth, SafeGeneralParameters settings) {
		
		SafePlot initPlot = (SafePlot) this.getPlot();
		
		for (Iterator it = this.getTrees().iterator(); it.hasNext();) {
			SafeTree t = (SafeTree) it.next();
			//if tree has roots
			if (t.getPlantRoots().getFirstRootNode() != null) {
				int i = 0;
				SafeVoxel voxel = cell.getVoxels()[i];
				float voxelSurface = (float)voxel.getSurfaceDepth();

				while (voxelSurface < soilManagementDepth) {
					if ((t.getCell().getId() != cell.getId()) && (t.getPlantRoots().getRootTopology(voxel) != null)) {

						if (t.getPlantRoots().getRootTopology(voxel).getNodeParent() != null) {
							float removedProp = 0;
							boolean testAnoxia = false;
							
							if ((float)voxel.getZ() < soilManagementDepth) 
								removedProp = 1;
							else 
								removedProp = (soilManagementDepth-voxelSurface) / (float)voxel.getThickness();

							
							t.getPlantRoots().getRootTopology(voxel).getNodeParent().removeSonsRoots(voxel, t, settings, removedProp, testAnoxia, initPlot.getSoil().getHumificationDepth());
						}
					}
					i++;
					voxel = cell.getVoxels()[i];
					voxelSurface = (float) voxel.getSurfaceDepth();
				}
			}
		}
	}
	
 	
    /**
	 * Check if tree root have colonized the all scene (at least one voxel for each cell) 
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
	

	public float getLatitude () {return immutable2.latitude;}
	public float getElevation () {return immutable2.elevation;}
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


	/** TO DISPLAY IN SCENARIO INSPECTOR **/
	public void setDisplay (String s) {display = s;}
	public String getToolTip () {
		return getCaption ()+((display == null) ? "" : "-"+display);
	}

	/**
	* Compute z coordinate of a point (x,y).
	*/
	public static double zCoordinate (double x, double y, SafePlotSettings plotSettings) {
		double slope = Math.toRadians(plotSettings.slopeIntensity);
		double treeLineOrientation = plotSettings.treeLineOrientation;						//degree
		double slopeAspect	= plotSettings.slopeAspect;										//degree
		double bottomAzimut = Math.toRadians(-90+treeLineOrientation-slopeAspect);
		double z = -Math.tan(slope)*(x*Math.cos(bottomAzimut)+y*Math.sin(bottomAzimut));
		return z;
	}

	@Override
	public SafePlot getPlot () {
		return (SafePlot) plot; // fc-30.10.2017
	}

}
