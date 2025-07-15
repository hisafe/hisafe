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
import java.util.Iterator;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.kernel.GScene;


/**
 * SafePlot represent the spatial desagregation of the SafeStand on a grid 
 * 
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafePlot extends RectangularPlot implements Serializable {

	/** Reference to the SafePlotSettings object : plot parameters */
	private SafePlotSettings plotSettings; 
	/** Reference of SafeSoil object  */
	private SafeSoil soil;						
	/** List of the plot crop zones  */
	private ArrayList<SafeCropZone> cropZones;
	
	//ANNUAL PAR TOTALS FOR EXPORT
	/** Total annual PAR intercepted by trees (Moles PAR m-2) */
	private double annualParInterceptedByTrees;			
	/**  Total annual PAR intercepted by crops (Moles PAR m-2)  */
	private double annualParInterceptedByCrops;				
	/** Total annual PAR incident(Moles PAR m-2) */
	private double annualParIncident;					

	//ANNUAL WATER BUDGET FOR EXPORT  
	/** Total annual water entries by irrigation (liter) */
	private double annualIrrigation;					
	/** Total annual water entries by water table flow (liter) */
	private double annualWaterAddedByWaterTable;
	/** Total annual water entries by rain transmitted by trees (liter)  */
	private double annualRainTransmittedByTrees;	
	/** Total annual water entries by rain transmitted by crops or bare soil (liter)  */
	private double annualRainTransmittedByCrops;
	//OUT
	/** Total annual water uptake by trees (liter)   */
	private double annualWaterUptakeByTrees;		
	/** Total annual water uptake by crops (liter)  */
	private double annualWaterUptakeByCrops;		
	/** Total annual water uptake by trees in saturated voxels (liter)  */
	private double annualWaterUptakeInSaturationByTrees;		
	/** Total annual water uptake by crops in saturated voxels (liter)  */
	private double annualWaterUptakeInSaturationByCrops;		
	/** Total annual soil evaporation (liter) */
	private double annualEvaporation;				
	/**  Total annual soil surface run off (liter)   */
	private double annualSurfaceRunOff;								
	/** Total annual soil bottom drainage (liter)  */
	private double annualDrainageBottom;				
	/**  Total annual soil artificial drainage (liter)   */
	private double annualDrainageArtificial;			
	/**  Total annual rain intercepted by trees (liter)   */
	private double annualRainInterceptedByTrees;					
	/** Total annual rain intercepted by crops (liter)   */
	private double annualRainInterceptedByCrops;	
	/** Total annual water out by desaturation (liter) */
	private double annualWaterToDesaturation;						
				


	//ANNUAL NITROGEN BUDGET FOR EXPORT 
	/** Total annual N entries by mineral fertilization (kg N ha-1) */
	private double annualFertilisationMineral;
	/** Total annual N entries by organic fertilization (kg N ha-1) */
	private double annualFertilisationOrganic;
	/** Total annual N entries by rain (kg N ha-1) */
	private double annualNitrogenRain;
	/** Total annual N entries by irrigation (kg N ha-1) */
	private double annualNitrogenIrrigation;
	//OUT 
	/** Total annual N uptake by trees (kg N ha-1) */
	private double annualNitrogenUptakeByTrees;		//kg N  
	/** Total annual N uptake by crops (kg N ha-1) */
	private double annualNitrogenUptakeByCrops;		//kg N  
	/** Total annual N out by soil run off (kg N ha-1) */
	private double annualNitrogenRunOff;			
	/** Total annual N out by soil bottom leaching (kg N ha-1) */
	private double annualNitrogenLeachingBottom;	
	/** Total annual N out by water table leaching  (kg N ha-1) */
	private double annualNitrogenLeachingWaterTable;

	public SafePlot(GScene stand, double cellWidth) {

		super(stand, cellWidth);
	}
	
	/**
	 * Constructor
     * @param stand Reference to the GScene (stand) object
 	 * @param cellWidth Cells width (m) 
     * @param nRows Number of rows on the plot
 	 * @param nCols Number of columns on the plot	
	 */
	public SafePlot(GScene stand, double cellWidth, int nRows, int nCols) {

		super(stand, cellWidth);

		double userWidth = cellWidth * nCols;
		double userHeight = cellWidth * nRows;
		getImmutable().nLin = nRows;
		getImmutable().nCol = nCols;

		// 2. Prepare a cell matrix
		defineMatrix(getImmutable().nLin, getImmutable().nCol);
		// fc - replaced by preceding line - 28.11.2001 - cells = new
		// SquareCell[getImmutable().nLin][getImmutable().nCol];

		// 3. Set plot bottomLeft
		setOrigin(new Vertex3d(0d, 0d, 0d));
		setXSize(userWidth);
		setYSize(userHeight);
		setArea (userWidth * userHeight);

	}

	
	/**
	 * Computation of cell neighbors in 4 directions (right left back front) 
     * @param evolutionParameters Reference to the SafeEvolutionParameters object
	 */
	public void computeCellsNeighbourg (SafeEvolutionParameters evolutionParameters) {

		for (Iterator c = getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			int i = cell.getIGrid();
			int j = cell.getJGrid();

			SafeCell rightNeighbourg = (SafeCell) this.getCell(i, j + 1);

			//if toric symetry id off on Xp
			if (evolutionParameters.toricXp == 0) {
				if (rightNeighbourg.getX() < cell.getX()) rightNeighbourg = null;
			}
			if (rightNeighbourg != null)	cell.setCellIdRight(rightNeighbourg.getId());
			

			SafeCell leftNeighbourg = (SafeCell) this.getCell(i, j - 1);
			//if toric symetry id off on Xn
			if (evolutionParameters.toricXn == 0) {
				if (leftNeighbourg.getX() > cell.getX()) leftNeighbourg = null;
			}
			if (leftNeighbourg != null) cell.setCellIdLeft(leftNeighbourg.getId());
	

			SafeCell backNeighbourg = (SafeCell) this.getCell(i - 1, j);
			//if toric symetry id off on Yn
			if (evolutionParameters.toricYn == 0) {
				if (backNeighbourg.getY() < cell.getY()) backNeighbourg = null;
			}			
			if (backNeighbourg != null) cell.setCellIdBack(backNeighbourg.getId());
		

			SafeCell frontNeighbourg = (SafeCell) this.getCell(i + 1, j);
			//if toric symetry id off on Yp
			if (evolutionParameters.toricYp == 0) {
				if (frontNeighbourg.getY() > cell.getY()) frontNeighbourg = null;
			}			
			if (frontNeighbourg != null) cell.setCellIdFront(frontNeighbourg.getId());
			
		}
	}
	
	/**
	 * Creation of the crop ZONES
	 */	
	public void initialiseCropZone ()  {
		cropZones = new ArrayList<SafeCropZone>();
		SafeCropZone zone = new SafeCropZone(1,"ZONE1");
		cropZones.add(zone);
	}
	/**
	 * Initialization of the crop ZONES
     * @param evolutionParameters Reference to the SafeEvolutionParameters object
	 */	
	public void initialiseCropZone (SafeEvolutionParameters evolutionParameters)  {
		
		cropZones = new ArrayList<SafeCropZone>();
		int nbCells = 0;
		 for(int i = 0 ; i < evolutionParameters.zonesIds.size(); i++) {
			 int zoneId = evolutionParameters.zonesIds.get(i);
			 String zoneName = evolutionParameters.zonesNames.get(i);
			 String zoneCellList = evolutionParameters.zonesCellsList.get(i);
			 String zoneTecList = evolutionParameters.zonesTecsList.get(i);
			 
			 List<SafeCell> zoneCells = new ArrayList<SafeCell>();
			 List<String> zoneItk = new ArrayList<String>();
		
			String[] st = zoneCellList.split(",");
			for (int k = 0; k < st.length; k++) {

				String[] suite = st[k].split("-");

				if (suite.length > 1) {
					int cellIdDeb = Integer.parseInt(suite[0]);
					int cellIdFin = Integer.parseInt(suite[1]);
					for (int cellId = cellIdDeb; cellId <= cellIdFin; cellId++) {
						SafeCell cell = (SafeCell) this.getCell(cellId);
						if (cell != null) {
							zoneCells.add(cell);
							nbCells++;
						}
						else {
							System.out.println("WRONG CELL ID "+cellId+" IN CROP ZONES DEFINITION") ;
							System.exit(1);	
						}
					}
				}
				else {
					int cellId = Integer.parseInt(st[k]);
					SafeCell cell = (SafeCell) this.getCell(cellId);
					if (cell != null) {
						zoneCells.add(cell);
						nbCells++;
					}
					else {
						System.out.println("WRONG CELL ID "+cellId+" IN CROP ZONES DEFINITION") ;
						System.exit(1);	
					}
				}
			}
		
		
			String[] st2 = zoneTecList.split(",");
			for (int k = 0; k < st2.length; k++) {
				String itkName = st2[k];
				int occurence = 1;
				if (itkName.contains("(") && itkName.contains(")")) {
					int index1 = itkName.indexOf("(");
					int index2 = itkName.indexOf(")");
					occurence = Integer.parseInt(itkName.substring(index1 + 1, index2));
					itkName = itkName.substring(0, index1);
				}
				
				for (int o = 0; o < occurence; o++) zoneItk.add(itkName);
			}

			SafeCropZone zone = new SafeCropZone(zoneId, zoneName, evolutionParameters, zoneCells, zoneItk, plotSettings.cellSurface);
			zone.initCells();
			cropZones.add(zone);
		 }
		 
		 //check all cell have a zone 
		for (Iterator i = this.getCells().iterator(); i.hasNext();) {
			SafeCell c = (SafeCell) i.next();
			if (c.getCropZone()==null) {
				System.out.println("CELL ID "+c.getId()+" MISSING IN CROP ZONES DEFINITION") ;
				System.exit(1);
			}
		}
	}

	
	/**
	 * Compute all saturated voxels regards to water table depth
     * @param waterTableDepth Water table depth (m) 
	 */
	public void computeWaterTable (double waterTableDepth) {

		// for each cell of the plot
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxels = cell.getVoxels();
			
			boolean changes = false;
			double waterAddedByWaterTable = 0;
			double waterTakenByDesaturation = 0;
			double drainageWaterTable = 0;
			double nitrogenLeaching= 0; // AQ
			double nitrogenAddedByWaterTable = 0;

			// for each voxel of the cell
			for (int iz = 0; iz < voxels.length; iz++) {
				// Voxel gravity center under water table depth are saturated
				if (voxels[iz].getZ() >= waterTableDepth) {
					
					// calculate the water stock increase in this voxel
					//5 positions in result table (0=water, 1-2=NO3, 3-4=NH4) 
					double[] waterNStockIncrease = new double[3];
					waterNStockIncrease = voxels[iz].setIsSaturated(true, this.getSoil());
					
					if (waterNStockIncrease[0] >= 0) waterAddedByWaterTable += waterNStockIncrease[0];
					
					// Small negative value can occur when water table saturates voxels that are already saturated FROM STICS (due to heavy rain)
					// This is because rounding errors can cause the field capacity calculated by STICS to be higher (by a very small amount) than the
					// field capacity calculated by HISAFE. This value is always extremely small. It is just sent to increase the draiangeBottom from STICS.
					else drainageWaterTable -= waterNStockIncrease[0];
					
					nitrogenLeaching += (waterNStockIncrease[1] + waterNStockIncrease[3]); 																						
					nitrogenAddedByWaterTable += (waterNStockIncrease[2] + waterNStockIncrease[4]); 																							
					changes = true;

				}
				// voxel saturated which are no more under water table are back
				// to field capacity
				else if (voxels[iz].getIsSaturated() == true) {
					// calculate the water stock decrease in this voxel
					double[] waterNStockIncrease = voxels[iz].setIsSaturated(false, this.getSoil());
					waterTakenByDesaturation -= waterNStockIncrease[0];
					changes = true; //A REACTIVER QUAND ON PASSERA DE SAT A
					// FC -- AQ 08.08.2011
				}
			}
			// if something have changed, new values have to be desagregated in
			// STICS mini layers
			if (changes) {		
				cell.voxelsToMinicouchesWaterNitrogen();
				cell.addWaterAddedByWaterTable(waterAddedByWaterTable / cell.getArea()); // mm
				cell.addDrainageWaterTable(drainageWaterTable / cell.getArea()); // mm
				cell.addWaterTakenByDesaturation(waterTakenByDesaturation / cell.getArea()); // mm
				cell.addNitrogenLeachingWaterTable(nitrogenLeaching * 10 / cell.getArea()); // AQ from g to kg/cell to kg/ha
				cell.addNitrogenAddedByWaterTable(nitrogenAddedByWaterTable * 10 / cell.getArea()); 
			}
		}

	} // fin

	/**
	 * Compute Deep Root Mineralization: AQ
	 */
	public void deepSenescentRootsMineralization(SafeGeneralParameters generalParameters, double humificationDepth) {
		
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxels = cell.getVoxels();

			// for each voxel of the cell
			for (int iz = 0; iz < voxels.length; iz++) {				
					voxels[iz].deepSenescentRootsMineralization(generalParameters,  cell.getCrop().sticsSoil, humificationDepth);
			}	
		}
	}

	

	
	/**
	 * Calculate totals for annual Export 
	 */	
	public void processTotalAnnual () {
	
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			t.processTotal();
		}

		this.annualParIncident += this.getTotalParIncident();
		this.annualParInterceptedByTrees += this.getTotalParInterceptedByTrees();
		this.annualParInterceptedByCrops += this.getTotalParInterceptedByCrops();
		this.annualWaterUptakeByTrees += this.getTotalWaterUptakeByTrees();
		this.annualWaterUptakeByCrops += this.getTotalWaterUptakeByCrops();
		this.annualRainInterceptedByTrees += this.getTotalRainInterceptedByTrees();
		this.annualRainTransmittedByTrees += this.getTotalRainTransmittedByTrees();	
		this.annualRainInterceptedByCrops += this.getTotalRainInterceptedByCrops();
		this.annualRainTransmittedByCrops += this.getTotalRainTransmittedByCrops();
		this.annualWaterAddedByWaterTable += this.getTotalWaterAddedByWaterTable();
		this.annualWaterToDesaturation += this.getTotalWaterToDesaturation();	
		this.annualEvaporation += this.getTotalSoilEvaporation();
		this.annualIrrigation+= this.getTotalIrrigation();
		this.annualFertilisationMineral = this.getTotalNitrogenFertilisationMineral();
		this.annualFertilisationOrganic = this.getTotalNitrogenFertilisationOrganic();
		this.annualNitrogenRain = this.getTotalNitrogenRain();
		this.annualNitrogenIrrigation = this.getTotalNitrogenIrrigation();	
		this.annualNitrogenRunOff += this.getTotalNitrogenRunOff();	
		this.annualSurfaceRunOff += this.getTotalSurfaceRunOff();
		this.annualDrainageBottom += getTotalDrainageBottom();
		this.annualDrainageArtificial += getTotalDrainageArtificial();
		this.annualNitrogenLeachingBottom += getTotalNitrogenLeachingBottom();
		this.annualNitrogenLeachingWaterTable += getTotalNitrogenLeachingWaterTable();
		this.annualWaterUptakeInSaturationByTrees += this.getTotalWaterUptakeInSaturationByTrees();			
		this.annualWaterUptakeInSaturationByCrops += this.getTotalWaterUptakeInSaturationByCrops();	
		this.annualNitrogenUptakeByTrees += this.getTotalNitrogenUptakeByTrees();
		this.annualNitrogenUptakeByCrops += this.getTotalNitrogenUptakeByCrops();
	}
	/**
	 * RAZ annual totals 
	 */
	public void razTotalAnnual() {
		
		for (Iterator i = ((SafeStand) this.getScene()).getTrees().iterator(); i.hasNext();) {
			SafeTree t = (SafeTree) i.next();
			t.razTotalAnnual();
		}

		for (Iterator j = this.getCells().iterator(); j.hasNext();) {
			SafeCell cell = (SafeCell) j.next();
			cell.razTotalAnnual();
		}
		
		annualParIncident = 0; 
		annualParInterceptedByTrees = 0;	
		annualParInterceptedByCrops = 0;
		annualWaterUptakeByTrees = 0;
		annualWaterUptakeByCrops = 0;
		annualWaterUptakeInSaturationByTrees = 0;
		annualWaterUptakeInSaturationByCrops = 0;
		annualRainInterceptedByTrees= 0;
		annualRainTransmittedByTrees= 0;
		annualRainInterceptedByCrops= 0;
		annualRainTransmittedByCrops= 0;
		annualWaterAddedByWaterTable = 0;
		annualWaterToDesaturation = 0;
		annualNitrogenRunOff = 0;
		annualEvaporation = 0;
		annualIrrigation = 0;
		annualFertilisationMineral = 0;
		annualFertilisationOrganic = 0;		
		annualNitrogenRain = 0;
		annualNitrogenIrrigation = 0;
		annualSurfaceRunOff = 0;
		annualDrainageBottom = 0;
		annualDrainageArtificial = 0;	
		annualNitrogenLeachingBottom = 0;	
		annualNitrogenLeachingWaterTable = 0; 
		annualNitrogenUptakeByTrees = 0;
		annualNitrogenUptakeByCrops = 0;

	}	
	protected void initPlot() {}


	public int getNbCells () {return this.getCells().size();}


	//****************************************
	//Total and mean values for EXPORT
	//****************************************
	public double getTotalCropBiomass() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getBiomass(); //// Aboveground dry matter (masec) t.ha-1
		}
		return total; //t.ha-1
	}
	
	public double getMeanCropBiomass() {
		return getTotalCropBiomass()  / getNbCells (); //t.ha-1
	}
	

	public double getTotalCropYield() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getYield();
		}
		return total;	//t.ha-1
	}

	public double getMeanCropYield() {
		return getTotalCropYield() / getNbCells ();	//t.ha-1
	}

	public double getTotalCropLai() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getLai();		// m2.m-2
		}
		return total;
	}
	public double getMeanCropLai() {
		return getTotalCropLai()/ getNbCells ();	// m2.m-2
	}

	public double getTotalCropEai() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getEai();	// m2.m-2
		}
		return total;	// m2.m-2
	}	
	public double getMeanCropEai() {
		return getTotalCropEai() / getNbCells ();	// m2.m-2
	}
	

	
	public double getTotalCropHeight() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getHeight();	// Height of canopy (hauteur) mm
		}
		return total;	//m
	}

	public double getMeanCropHeight() {
		return getTotalCropHeight() / getNbCells ();		//m
	}

	public double getMeanCropGrainNumber() {
		double total = 0;
		int count = 0;
		double cropYield = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropYield = cell.getCrop().getGrainBiomass();
			if (cropYield != 0) {
				total += cell.getCrop().getGrainNumber();		// nbr m-2
				count++;
			}
		}
		if (count > 0)
			total /= count;
		return total;		// nbr m-2
	}

	public double getMeanCropGrainWeight() {
		double total = 0;
		int count = 0;
		double cropYield = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropYield = cell.getCrop().getGrainBiomass();
			if (cropYield != 0) {
				total += cell.getCrop().getGrainWeight();		//g
				count++;
			}

		}
		if (count > 0)
			total /= count;
		return total;			//g
	}

	public double getMeanCropPlantDensity() {
		double total = 0;
		int count = 0;
		double cropBiomass = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropBiomass = cell.getCrop().getBiomass();
			if (cropBiomass != 0) {
				total += cell.getCrop().getPlantDensity();		// nbr m-2
				count++;
			}
		}
		if (count > 0)
			total /= count;
		return total;		// nbr m-2
	}

	public double getMeanCropSla() {
		double total = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
				cropLai = cell.getCrop().getLai();
				if (cropLai != 0) {
					total += cell.getCrop().getSla();		// cm2 g-1;
					count++;
				}
		}
		if (count > 0)
			total /= count;	// cm2 g-1;
		return total;
	}
	
	public double getMeanCropTemperature() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getCropTemperature();		//degree C;
		}

		return total / getNbCells ();			//degree C;
	}

	public double getMeanSoilSurfaceTemperature() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getSoilSurfaceTemperature();		//degree C;
		}

		return total / getNbCells ();			//degree C;
	}
	
	public double getMeanCropRootsLenght() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getTotalRootsLength();		//m
		}
		return total / getNbCells ();		//m
	}
	public double getMeanCropRootsDepth() {
		double total = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				total += cell.getCrop().getRootsDepth();		//m
				count++;
			}
		}
		if (count > 0)
			total /= count;
		return total;		//m
	}
	
	public double getMeanCropQngrain() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getQNgrain();	//Amount of nitrogen in harvested organs (grains / fruits)  kgN ha-1
		}
		return total / this.getNbCells();	//kgN ha-1
	}

	public double getMeanCropQnplante() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getQNplante();		//Amount of nitrogen taken up by the plant   kgN.ha-1
		}
		return total / this.getNbCells();	//kgN ha-1
	}

	public double getMeanCropCngrain() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getCNgrain();		//Nitrogen concentration of grains %
		}
		return total / this.getNbCells();				//%
	}

	public double getMeanCropCnplante() {
		double total = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			total += cell.getCrop().getCNplante();		//Nitrogen concentration of entire plant %
		}
		return total / this.getNbCells();		//%
	}
	

	//LIGHT

	public double getTotalParIncident() {
		SafeMacroClimat climat = ((SafeModel) this.getScene().getStep().getProject().getModel()).getMacroClimat();
		int julianDay = ((SafeStand) this.getScene()).getJulianDay();
		int year = ((SafeStand) this.getScene()).getWeatherYear();
		double par = 0;
		try {
			SafeDailyClimat dailyClimat = climat.getDailyWeather(year, julianDay);
			if (dailyClimat != null) {
				par = (double) dailyClimat.getGlobalPar();
			}
		} catch (Throwable e) {
			Log.println("weather data not found for day " + julianDay);
		}

		return par;// Moles PAR m-2
	}
	
	public double getMeanParIncident() {
		return getTotalParIncident() / this.getNbCells();		//moles PAR m-2
	}
	
	
	public double getTotalParInterceptedByTrees() {
		double par = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			par += t.getDiffuseParIntercepted() + t.getDirectParIntercepted();
		}
		return par;//Moles PAR m-2
	}

	public double getTotalParInterceptedByCrops() {
		double par = 0;

		for (Iterator i = this.getCells().iterator(); i.hasNext();) {
			SafeCell c = (SafeCell) i.next();
			par += c.getCrop().getDiffuseParIntercepted() + c.getCrop().getDirectParIntercepted();
		}
		return par;//Moles PAR m-2

	}

		
	//STRESSES
	public double getMeanCropHisafeWaterStress() {
		double waterStress = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				waterStress += cell.getCrop().getHisafeWaterStomatalStress();
				count++;
			}
		}
		if (count > 0)
			waterStress /= count;
		return waterStress;
	}

	public double getMeanCropHisafeNitrogenStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getHisafeNitrogenStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}
	
	public double getMeanCropNitrogenLaiStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getSticsNitrogenLaiStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMeanCropNitrogenBiomassStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getSticsNitrogenBiomassStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMeanCropNitrogenSenescenceStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			cropLai = cell.getCrop().getLai();
			if (cropLai != 0) {
				temp += cell.getCrop().getSticsNitrogenSenescenceStress();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}
	
		
		
	//WATER
	
	public double getWaterTableDepth() {
		SafeMacroClimat climat = ((SafeModel) this.getScene().getStep().getProject().getModel()).getMacroClimat();
		int julianDay = ((SafeStand) this.getScene()).getJulianDay();
		int year = ((SafeStand) this.getScene()).getWeatherYear();
		double waterTableDepth = 0;
		try {
			SafeDailyClimat dailyClimat = climat.getDailyWeather(year, julianDay);
			if (dailyClimat != null) 
			waterTableDepth = dailyClimat.getWaterTableDepth();
		} catch (Throwable e) {
			Log.println("weather data not found for day " + julianDay);
		}
		return waterTableDepth;
	}
	
	public double getTotalWaterStock() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				waterStock += voxel[i].getWaterStock();
							
			}
		}
		return waterStock;		//liters
	}


	public double getTotalWaterUptakeByCrops() {
		double waterUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterUptake +=cell.getCrop().getWaterUptake() * cell.getArea();	//convert mm to liters
		}
		return waterUptake; //liters
	}
	
	public double getTotalWaterUptakeInSaturationByCrops() {
		double waterUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterUptake += cell.getWaterUptakeInSaturationByCrop()* cell.getArea();	//convert mm to liters
		}
		return waterUptake; //liters
	}
	
	public double getTotalWaterUptakeByTrees() {
		double waterUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			SafeVoxel[] voxels = cell.getVoxels();
			for (int i = 0; i < voxels.length; i++) {	
			waterUptake +=voxels[i].getTotalTreeWaterUptake();	//liters
			}
		
		}
		return waterUptake;	//liters
	}
	
	public double getTotalWaterUptakeInSaturationByTrees() {
		double waterUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterUptake += cell.getWaterUptakeInSaturationByTrees();//liters
		}
		return waterUptake;	//liters
	}
	
	public double getTotalSoilEvaporation() {
		double waterEvap = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterEvap += cell.getCrop().getSoilEvaporation() *cell.getArea();//convert mm to liters
		}
		return waterEvap;//liters
	}

	public double getTotalIrrigation() {
		double irrig = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			irrig += cell.getCrop().getIrrigation() * cell.getArea();//convert mm to liters
		}
		return irrig;//liters
	}

	public double getTotalRainTransmittedByTrees() {
		double rain = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			rain += ((SafeCell) c.next()).getRainTransmittedByTrees() * getArea();//convert mm to liters
		}
		return rain;//liters	
	}
	
	public double getTotalRainTransmittedByCrops() {
		double rain = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();	
			rain += cell.getRainTransmittedByCrop() * cell.getArea();//convert mm to liters
		}
		return rain;//liters	
	}
	
	public double getTotalRainInterceptedByTrees() {
		double rain = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			rain += t.getInterceptedRain();
		}
		return rain;//liters
	}
	
	public double getTotalRainInterceptedByCrops() {
		double rain = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			rain += cell.getRainInterceptedByCrop() * cell.getArea();//convert mm to liters
		}
		return rain;//liters	
	}

	
	public double getTotalCropsWaterDemand() {
		double wdem = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			wdem += cell.getCrop().getWaterDemand() * cell.getArea();//convert mm to liters
		}
		return wdem; //liters
	}

	public double getTotalCropsWaterDemandReduced() {
		double wdem = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			wdem += cell.getCrop().getWaterDemandReduced()* cell.getArea();//convert mm to liters
		}
		return wdem; //liters
	}

	public double getTotalSurfaceRunOff() {
		double runOff = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			runOff += cell.getCrop().getSurfaceRunOff()* cell.getArea();//convert mm to liters
		}
		return runOff;//liters
	}
	
	public double getTotalDrainageBottom() {
		
		double drainage = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			drainage += cell.getCrop().getDrainageBottom()* cell.getArea();//convert mm to liters
		}
		return drainage; //liters
	}

	public double getTotalDrainageArtificial() {
		
		double drainage = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			drainage += cell.getCrop().getDrainageArtificial()* cell.getArea();//convert mm to liters
		}
		return drainage; //liters
	}
	
	public double getTotalWaterAddedByWaterTable() {
		double water = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			water += cell.getWaterAddedByWaterTable()* cell.getArea();//convert mm to liters
		}
		return water; //liters
	}
	
	public double getTotalWaterToDesaturation() {
		double waterDesat = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterDesat += cell.getWaterTakenByDesaturation()* cell.getArea();//convert mm to liters
		}
		return waterDesat;//liters
	}
	
	//NITROGEN
	public double getTotalCropsNitrogenDemand() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			temp += cell.getCrop().getNitrogenDemand() / 10000 * cell.getArea(); //kgN ha-1 to Kg N
		}

		return temp;//kgN ;
	}	

	public double getTotalNitrogenUptakeByCrops() {
		double nitrogenUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			SafeVoxel[] voxels = cell.getVoxels();
			for (int i = 0; i < voxels.length; i++) {	
				nitrogenUptake +=voxels[i].getCropNitrogenUptake();	//gr
			}
		
		}
		return nitrogenUptake/1000;	//kg N
	}
	
	public double getTotalNitrogenUptakeByTrees() {
		double nitrogenUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			SafeVoxel[] voxels = cell.getVoxels();
			for (int i = 0; i < voxels.length; i++) {	
				nitrogenUptake +=voxels[i].getTotalTreeNitrogenUptake();	//gr
			}
		
		}
		return nitrogenUptake/1000;	//kg N
	}

	public double getTotalNitrogenUptakeInSaturationByCrops() {
		double nitrogenUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogenUptake += cell.getNitrogenUptakeInSaturationByCrop();
		}
		return nitrogenUptake; 	//kg N
	}
	

	
	public double getTotalNitrogenUptakeInSaturationByTrees() {
		double nitrogenUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogenUptake += cell.getNitrogenUptakeInSaturationByTrees(); 
		}
		return nitrogenUptake;	//kg N
	}
	
	public double getTotalNitrogenFertilisationMineral() {
		double nitrogen = 0; 
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenFertilisationMineral(); 
		}
		return nitrogen;//kg N ha-1
	}

	public double getTotalNitrogenFertilisationOrganic() {
		double nitrogen = 0; 
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenFertilisationOrganic(); 
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenIrrigation() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenIrrigation();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenRain() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenRain();
		}
		return nitrogen;//kg N ha-1
	}
	
	

	
	
	public double getTotalNitrogenRunOff() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getNitrogenRunOff();
		}
		return nitrogen;//kg N ha-1
	}
	
	
	public double getTotalNitrogenFixation() {
		
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenFixation();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenHumusMineralisation() {
		
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenHumusMineralisation();
		}
		return nitrogen;//kg N ha-1
	}

	public double getTotalNitrogenResiduMineralisation() {
		
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenResiduMineralisation();
		}
		return nitrogen;//kg N ha-1
	}

	public double getTotalNitrogenDenitrification() {
		
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenDenitrification();
		}
		return nitrogen;//kg N ha-1
	}

	

	public double getTotalNitrogenLeachingBottom() {
		
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nitrogen += cell.getCrop().getNitrogenLeachingBottom();
		}
		return nitrogen;//kg N ha-1
	}
	
	public double getTotalNitrogenLeachingWaterTable() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getNitrogenLeachingWaterTable();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenAddedByWaterTable() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getNitrogenAddedByWaterTable();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenImmobilisation() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenImmobilisation();
		}
		return v;//kg N ha-1
	}
	
	public double getTotalNitrogenVolatilisation() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenVolatilisation();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenVolatilisationOrganic() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenVolatilisationOrganic();
		}
		return v;//kg N ha-1
	}


		
	//RESIDUS
	public double getTotalBiomassRestitution() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getBiomassRestitution(); // quantity of aerial residues from the previous crop // t.ha-1
		}
		return v;	// t.ha-1
	}

	public double getTotalCarbonResidus() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getCarbonResidus();
		}
		return v;	//kgC.ha-1
	}

	public double getTotalNitrogenResidus() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenResidus();
		}
		return v;	//kgN.ha-1
	}
	
	//LITTERS
	public double getTotalTreeCarbonFoliageLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonFoliageLitter();	
		}
		return temp;	//KG
	}

	public double getTotalTreeNitrogenFoliageLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenFoliageLitter();
		}
		return temp;	//KG
	}

	
	public double getTotalTreeCarbonBranchesLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonBranchesLitter();	
		}
		return temp;	//kg C ha-1
	}

	public double getTotalTreeNitrogenBranchesLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenBranchesLitter();
		}
		return temp;	//kg N ha-1
	}
	
	public double getTotalTreeCarbonFruitLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonFruitLitter();	
		}
		return temp;	//kg C ha-1
	}

	public double getTotalTreeNitrogenFruitLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenFruitLitter();
		}
		return temp;	//kg N ha-1
	}
	public double getTotalTreeCarbonFineRootsLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonFineRootsLitter();
		}
		return temp;	//KG
	}

	public double getTotalTreeNitrogenFineRootsLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenFineRootsLitter();
		}
		return temp;	//KG
	}
	
	public double getTotalTreeCarbonCoarseRootsLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeCarbonCoarseRootsLitter();
		}
		return temp; //KG
	}

	public double getTotalTreeNitrogenCoarseRootsLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getTreeNitrogenCoarseRootsLitter();
		}
		return temp;	//KG
	}	

	public double getTotalCarbonHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTotalCarbonHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalNitrogenHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTotalNitrogenHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalInactiveCarbonHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getInactiveCarbonHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalActiveCarbonHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getActiveCarbonHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalInactiveNitrogenHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getInactiveNitrogenHumusStock();
		}
		return temp;	//KG ha-1
	}

	public double getTotalActiveNitrogenHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getActiveNitrogenHumusStock();
		}
		return temp;	//KG ha-1
	}	

	
	// Export methods about trees


	
	
	
	
	//TOTAL ANNUAL FOR EXPORT
	//LIGHT
	public double getAnnualParIncident() {
		return annualParIncident;
	}

	
	//WATER
	
	public double getAnnualParInterceptedByTrees() {
		return annualParInterceptedByTrees;
	}
	
	
	public double getAnnualParInterceptedByCrops() {
		return annualParInterceptedByCrops;
	}
	
	
	public double getAnnualWaterUptakeByTrees() {
		return annualWaterUptakeByTrees;
	}
	
	public double getAnnualWaterUptakeByCrops() {
		return annualWaterUptakeByCrops;
	}
	
	
	public double getAnnualSurfaceRunOff() {
		return annualSurfaceRunOff;
	}
	
	public double getAnnualWaterAddedByWaterTable() {
		return annualWaterAddedByWaterTable;
	}
	public double getAnnualWaterToDesaturation() {
		return annualWaterToDesaturation;
	}


	
	public double getAnnualEvaporation() {
		return annualEvaporation;
	}	
	
	public double getAnnualIrrigation() {
		return annualIrrigation;
	}	
	public double getAnnualFertilisationMineral() {
		return annualFertilisationMineral;
	}
	public double getAnnualFertilisationOrganic() {
		return annualFertilisationOrganic;
	}
	
	public double getAnnualNitrogenRain() {
		return annualNitrogenRain;
	}
	public double getAnnualNitrogenIrrigation() {
		return annualNitrogenIrrigation;
	}
	
	public double getAnnualNitrogenRunOff() {
		return annualNitrogenRunOff;
	}	

	public double getAnnualRainInterceptedByTrees() {
		return annualRainInterceptedByTrees;
	}

	public double getAnnualRainTransmittedByTrees() {
		return annualRainTransmittedByTrees;
	}
	
	public double getAnnualRainInterceptedByCrops() {
		return annualRainInterceptedByCrops;
	}

	public double getAnnualRainTransmittedByCrops() {
		return annualRainTransmittedByCrops;
	}
	
	public double getAnnualDrainageBottom() {
		return annualDrainageBottom;
	}
	public double getAnnualDrainageArtificial() {
		return annualDrainageArtificial;
	}	
	
	public double getAnnualNitrogenLeachingBottom() {
		return annualNitrogenLeachingBottom;
	}
	public double getAnnualNitrogenLeachingWaterTable() {
		return annualNitrogenLeachingWaterTable;
	}
	

	
	
	public double getAnnualWaterUptakeInSaturationByTrees() {
		return annualWaterUptakeInSaturationByTrees;
	}
	public double getAnnualWaterUptakeInSaturationByCrops() {
		return annualWaterUptakeInSaturationByCrops;
	}
	
	
	//NITROGEN
	public double getAnnualNitrogenUptakeByTrees() {
		return annualNitrogenUptakeByTrees;
	}
	public double getAnnualNitrogenUptakeByCrops() {
		return annualNitrogenUptakeByCrops;
	}
	
	


	public SafePlotSettings getPlotSettings() {return plotSettings;}
	public void setPlotSettings(SafePlotSettings p) {plotSettings = p;}
	
	public SafeSoil getSoil () {return soil;}
	public void setSoil (SafeSoil s) {soil = s;}
	public ArrayList<SafeCropZone> getCropZones() {
		return cropZones;
	}
	public SafeCropZone getCropZone(int id) {
		return cropZones.get(id);
	}
	
}
