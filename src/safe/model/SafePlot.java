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
import java.util.Iterator;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.kernel.GScene;


/**
 * SafePlot represent the spatial desagregation of the SafeStand on a grid 
 * 
 * @author Isabelle Lecomte - INRA Montpellier France - July 2002
 */
public class SafePlot extends RectangularPlot implements Serializable {

	// WARNING: if references to objects (not primitive types) are added here,
	// implement a "public Object clone ()" method (see RectangularPlot.clone ()
	// for template)
	// float variables must be accessed with access methods (casts to double)
	

	private static final long serialVersionUID = 1L;

	private SafeSoil soil;						//the description of the soil
	private ArrayList<SafeCropZone> cropZones;	//crop zones
	
	private float slopeIntensity; 			// degree
	private float slopeAspect; 				// degree
	private float treeLineOrientation; 		// degree
	private float northOrientation; 		// degree
	private float cellSurface; 				// cells surface m2

	//ANNUAL PAR TOTALS FOR EXPORT
	private double annualParInterceptedByTrees;				// Moles PAR m-2
	private double annualParInterceptedByCrops;				// Moles PAR m-2
	private double annualParIncident;						// Moles PAR m-2

	
	//ANNUAL WATER BUDGET FOR EXPORT  
	//IN (liters) 
	private double waterStockBefore;
	private double annualIrrigation;					
	private double annualWaterAddedByWaterTable;
	private double annualRainTransmittedByTrees;	
	private double annualRainTransmittedByCrops;
	
	//OUT (liters) 
	private double annualWaterUptakeByTrees;		
	private double annualWaterUptakeByCrops;		
	private double annualWaterUptakeInSaturationByTrees;		
	private double annualWaterUptakeInSaturationByCrops;		
	private double annualEvaporation;				
	private double annualSurfaceRunOff;								
	private double annualDrainageBottom;				
	private double annualDrainageArtificial;			
	private double annualRainInterceptedByTrees;					
	private double annualRainInterceptedByCrops;	
	private double annualWaterToDesaturation;						
				


	//ANNUAL NITROGEN BUDGET FOR EXPORT 
	//IN (kg N ha-1)

	private double annualFertilisationMineral;
	private double annualFertilisationOrganic;
	private double annualNitrogenRain;
	private double annualNitrogenIrrigation;
	
	//OUT (kg N ha-1)
	private double annualNitrogenUptakeByTrees;		//kg N  
	private double annualNitrogenUptakeByCrops;		//kg N  
	private double annualNitrogenRunOff;			
	private double annualNitrogenLeachingBottom;	
	private double annualNitrogenLeachingWaterTable;

	
						

	
	/**
	 * Create the plot
	 */
	public SafePlot(GScene stn, SafeGeneralParameters settings, double cw, int nRows, int nCols, double aspect,
				double slope,  double northOrientation, double treeLineOrientation) {
			
		
		super(stn, cw);
		double userWidth = cw * nCols;
		double userHeight = cw * nRows;
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

		// 4. More initializations...
		this.slopeAspect = (float) aspect;
		this.slopeIntensity = (float) slope;
		this.northOrientation = (float) northOrientation;
		this.treeLineOrientation = (float) treeLineOrientation;
		setArea (userWidth * userHeight);
		setCellSurface ((float) (getCellWidth() * getCellWidth()));

	}
	/**
	 * Initialisation of the crop ZONES
	 */	
	public void initialiseCropZone ()  {
		cropZones = new ArrayList<SafeCropZone>();
		SafeCropZone zone = new SafeCropZone(1,"ZONE1");
		cropZones.add(zone);
	}
	/**
	 * Initialisation of the crop ZONES
	 */	
	public void initialiseCropZone (SafeEvolutionParameters evolutionParameters)  {
		
		cropZones = new ArrayList<SafeCropZone>();
		int nbCells = 0;
		 for(int i = 0 ; i < evolutionParameters.zoneId.size(); i++) {
			 int zoneId = evolutionParameters.zoneId.get(i);
			 String zoneName = evolutionParameters.zoneName.get(i);
			 String zoneCellList = evolutionParameters.zoneCellList.get(i);
			 String zoneTecList = evolutionParameters.zoneTecList.get(i);
			 
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

			SafeCropZone zone = new SafeCropZone(evolutionParameters, zoneId, zoneName, zoneCells, zoneItk, cellSurface);
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
	 * RAZ annual totals for export
	 */
	public void razTotalAnnual() {
		

		this.setWaterStockBefore(this.getTotalWaterStock());
		
		for (Iterator i = ((SafeStand) this.getScene()).getTrees().iterator(); i.hasNext();) {
			SafeTree t = (SafeTree) i.next();
			t.razTotalAnnual();
		}

		for (Iterator i = this.getCells().iterator(); i.hasNext();) {
			SafeCell c = (SafeCell) i.next();
			c.getCrop().razTotalAnnual();
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
	/**
	 * Compute all saturated voxels regards to water table depth
	 */
	public void computeWaterTable(SafeGeneralParameters safeSettings, 
								SafePlotSettings plotSettings,
								double waterTableDepth, boolean first) {

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
					waterNStockIncrease = voxels[iz].setIsSaturated(true, plotSettings, first);
					
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
					double[] waterNStockIncrease = voxels[iz].setIsSaturated(false, plotSettings, first);
					waterTakenByDesaturation -= waterNStockIncrease[0];
					changes = true; //A REACTIVER QUAND ON PASSERA DE SAT A
					// FC -- AQ 08.08.2011
				}
			}
			// if something have changed, new values have to be desagregated in
			// STICS mini layers
			if (changes) {		
				cell.voxelsToMinicouchesWaterNitrogen(safeSettings);
				cell.addWaterAddedByWaterTable(waterAddedByWaterTable / cell.getArea()); // mm
				cell.addDrainageWaterTable(drainageWaterTable / cell.getArea()); // mm
				cell.addWaterTakenByDesaturation(waterTakenByDesaturation / cell.getArea()); // mm
				cell.getCrop().addNitrogenLeachingWaterTable(nitrogenLeaching * 10 / cell.getArea()); // AQ from g to kg/cell to kg/ha
				cell.getCrop().addNitrogenAddedByWaterTable(nitrogenAddedByWaterTable * 10 / cell.getArea()); 
			}
		}

	} // fin

	/**
	 * Compute Deep Root Mineralization: AQ
	 */
	public void deepSenescentRootsMineralization(double humificationDepth, SafeGeneralParameters safeSettings) {
		
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxels = cell.getVoxels();

			// for each voxel of the cell
			for (int iz = 0; iz < voxels.length; iz++) {				
					voxels[iz].deepSenescentRootsMineralization(safeSettings,  cell.getCrop().sticsSoil, humificationDepth, cell.getArea());
			}	
		}
	}

	/**
	 * Computation of cell neighbourgs for tree roots colonisation in voxels
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

	protected void initPlot() {}


	public float getSlopeIntensity() {return slopeIntensity;}
	public float getSlopeAspect() {return slopeAspect;}
	public float getTreeLineOrientation() {return treeLineOrientation;}
	public float northOrientation() {return northOrientation;}
	public float getCellSurface() {return cellSurface;}
	public void setCellSurface(double e) {cellSurface = (float) e;}

	public int getNbCells () {
		return this.getCells().size();
	}
	//***************************************************
	// Methods for exportation about WATER BUDGET
	//***************************************************
	public void setWaterStockBefore(double v) {
		waterStockBefore = v;
	}
	public double getWaterStockBefore() {
		return waterStockBefore;
	}

	
	
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
				waterStress += cell.getCrop().getHisafeWaterStress();
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
			v += cell.getCrop().getNitrogenLeachingWaterTable();
		}
		return v;//kg N ha-1
	}

	public double getTotalNitrogenAddedByWaterTable() {
		
		double v = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			v += cell.getCrop().getNitrogenAddedByWaterTable();
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
	
	


	
	public SafeSoil getSoil () {return soil;}
	public void setSoil (SafeSoil s) {soil = s;}
	public ArrayList<SafeCropZone> getCropZones() {
		return cropZones;
	}
	public SafeCropZone getCropZone(int id) {
		return cropZones.get(id);
	}
	
}
