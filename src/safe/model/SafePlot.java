package safe.model;

import java.io.Serializable;
import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.kernel.GScene;


/**
 * SafePlot represent the spatial desagregation of the SafeStand on a grid 
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafePlot extends RectangularPlot implements Serializable {

	// WARNING: if references to objects (not primitive types) are added here,
	// implement a "public Object clone ()" method (see RectangularPlot.clone ()
	// for template)
	// float variables must be accessed with access methods (casts to double)
	
	private SafeSoil soil;					//the description of the soil
	
	private float slopeIntensity; 			// degree
	private float slopeAspect; 				// degree
	private float treeLineOrientation; 		// degree
	private float northOrientation; 		// degree
	private float treeCropDistance; 		// m
	private float treeCropRadius; 			// m
	private float cellSurface; 				// cells surface m2
	private float mainCropArea; 			// surface covered by crop m2
	private float interCropArea; 		    // surface covered by intercrop surface m2

	//ANNUAL PAR TOTALS FOR EXPORT
	private double annualParInterceptedByTrees;				// Moles PAR
	private double annualParInterceptedByMainCrop;			// Moles PAR
	private double annualParInterceptedByInterCrop;			// Moles PAR
	private double annualParIncident;						// Moles PAR
	private double annualParIncidentMainCrop;				// %
	private double annualParIncidentInterCrop;				// %
	
	//ANNUAL WATER TOTALS FOR EXPORT (mm) 
	private double annualWaterUptakeByTrees;					
	private double annualWaterUptakeByMainCrop;				
	private double annualWaterUptakeByInterCrop;				
	private double annualWaterEvaporatedInMainCrop;				
	private double annualWaterEvaporatedInInterCrop;						
	private double annualWaterAddedByWaterTable;					
	private double annualWaterToDesaturation;						
	private double annualInterceptedRainByTrees;				
	private double annualInterceptedRainByMainCrop;				
	private double annualInterceptedRainByInterCrop;			
					
	private double annualRunOff;								
	private double annualSurfaceRunOff;							
	private double annualDrainageBottom;
	private double annualDrainageArtificial;
	private double annualWaterUptakeInSaturationByTrees;		
	private double annualWaterUptakeInSaturationByMainCrop;	
	private double annualWaterUptakeInSaturationByInterCrop;	
	private double annualIrrigationInMainCrop;					
	private double annualIrrigationInInterCrop;					
	
	//ANNUAL NITROGEN TOTALS FOR EXPORT (kg ha-1) 
	private double annualNitrogenUptakeByTrees;					
	private double annualNitrogenUptakeByMainCrop;				
	private double annualNitrogenUptakeByInterCrop;				
	private double annualNitrogenFertilisationMineralInMainCrop;			
	private double annualNitrogenFertilisationMineralInInterCrop;		
	private double annualNitrogenIrrigationInMainCrop;			
	private double annualNitrogenIrrigationInInterCrop;			
	private double annualNitrogenRainInMainCrop;			
	private double annualNitrogenRainInInterCrop;			
	
	
	//ANNUAL CARBON STOCK TOTALS FOR EXPORT (Kg) 
	private double annualTreesCarbonBranches;					
	private double annualTreesCarbonCoarseRoots;			
	private double annualTreesCarbonFineRoots;					
	private double annualTreesCarbonLabile;						
	private double annualTreesCarbonStem;						
	private double annualTreesCarbonStump;						

	//FOR EXPORT
	private float precipitation;
	
	/**
	 * Create the plot
	 */
	public SafePlot(GScene stn, SafeInitialParameters settings, double cw, int nRows, int nCols, double aspect,
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
	 * Calculate totals for annual Export 
	 */	
	public void processTotalAnnual () {
	
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			t.processTotal();
		}
		
		this.annualParIncident += this.getParIncident();
		this.annualParIncidentMainCrop += this.getParIncidentMainCrop();
		this.annualParIncidentInterCrop += this.getParIncidentInterCrop();
		this.annualParInterceptedByTrees += this.getParInterceptedByTrees();
		this.annualParInterceptedByMainCrop += this.getParInterceptedByMainCrop();
		this.annualParInterceptedByInterCrop += this.getParInterceptedByInterCrop();	
		
		this.annualWaterUptakeByTrees += this.getWaterUptakeByTrees();
		this.annualWaterUptakeByMainCrop += this.getWaterUptakeByMainCrop();
		this.annualWaterUptakeByInterCrop += this.getWaterUptakeByInterCrop();				
		this.annualWaterEvaporatedInMainCrop += this.getWaterEvaporatedInMainCrop();
		this.annualWaterEvaporatedInInterCrop += this.getWaterEvaporatedInInterCrop();
		this.annualInterceptedRainByTrees += this.getInterceptedRainByTrees();
		this.annualInterceptedRainByMainCrop += this.getInterceptedRainByMainCrop();
		this.annualInterceptedRainByInterCrop += this.getInterceptedRainByInterCrop();
		this.annualWaterAddedByWaterTable += this.getWaterAddedByWaterTable();
		this.annualWaterToDesaturation += this.getWaterToDesaturation();	

		this.annualRunOff += this.getRunOff();
		this.annualSurfaceRunOff += this.getSurfaceRunOff();
		this.annualDrainageBottom += getDrainageBottom();
		this.annualDrainageArtificial += getDrainageArtificial();
		this.annualWaterUptakeInSaturationByTrees += this.getWaterUptakeInSaturationByTrees();
		this.annualWaterUptakeInSaturationByMainCrop += this.getWaterUptakeInSaturationByMainCrop();
		this.annualWaterUptakeInSaturationByInterCrop += this.getWaterUptakeInSaturationByInterCrop();		
		this.annualIrrigationInMainCrop += this.getIrrigationInMainCrop();
		this.annualIrrigationInInterCrop += this.getIrrigationInInterCrop();
		
		this.annualNitrogenUptakeByTrees += this.getNitrogenUptakeByTrees();
		this.annualNitrogenUptakeByMainCrop += this.getNitrogenUptakeByMainCrop();
		this.annualNitrogenUptakeByInterCrop += this.getNitrogenUptakeByInterCrop();
		this.annualNitrogenFertilisationMineralInMainCrop += this.getNitrogenFertilisationMineralInMainCrop();
		this.annualNitrogenFertilisationMineralInInterCrop += this.getNitrogenFertilisationMineralInInterCrop();
		this.annualNitrogenIrrigationInMainCrop += this.getNitrogenIrrigationInMainCrop();
		this.annualNitrogenIrrigationInInterCrop += this.getNitrogenIrrigationInInterCrop();
		this.annualNitrogenRainInMainCrop += this.getNitrogenRainInMainCrop();
		this.annualNitrogenRainInInterCrop += this.getNitrogenRainInInterCrop();

		this.annualTreesCarbonBranches += this.getCarbonBranches();	
		this.annualTreesCarbonCoarseRoots += this.getCarbonCoarseRoots();		
		this.annualTreesCarbonFineRoots += this.getCarbonFineRoots();	
		this.annualTreesCarbonLabile += this.getCarbonLabile();	
		this.annualTreesCarbonStem += this.getCarbonStem();	
		this.annualTreesCarbonStump += this.getCarbonStump();	

	}
	/**
	 * RAZ annual totals for export
	 */
	public void razTotalAnnual() {
		
		for (Iterator i = ((SafeStand) this.getScene()).getTrees().iterator(); i.hasNext();) {
			SafeTree t = (SafeTree) i.next();
			t.razTotalAnnual();
		}

		for (Iterator i = this.getCells().iterator(); i.hasNext();) {
			SafeCell c = (SafeCell) i.next();
			c.getCrop().razMaxValues();
		}

		annualParIncident = 0; 
		annualParIncidentMainCrop = 0; 
		annualParIncidentInterCrop = 0; 
		annualParInterceptedByTrees = 0;
		annualParInterceptedByMainCrop = 0;
		annualParInterceptedByInterCrop = 0;
		
		annualWaterUptakeByTrees = 0;
		annualWaterUptakeByMainCrop= 0; 
		annualWaterUptakeByInterCrop = 0; 
		annualWaterUptakeInSaturationByTrees = 0;
		annualWaterUptakeInSaturationByMainCrop = 0;
		annualWaterUptakeInSaturationByInterCrop = 0;
		annualWaterEvaporatedInMainCrop = 0; 
		annualWaterEvaporatedInInterCrop = 0; 
		annualInterceptedRainByTrees= 0;
		annualInterceptedRainByMainCrop= 0;
		annualInterceptedRainByInterCrop= 0;
		annualWaterAddedByWaterTable = 0;
		annualWaterToDesaturation = 0;

		annualRunOff = 0;
		annualSurfaceRunOff = 0;
		annualDrainageBottom = 0;
		annualDrainageArtificial = 0;
		annualIrrigationInMainCrop = 0;
		annualIrrigationInInterCrop = 0;
			
		annualNitrogenUptakeByTrees = 0;
		annualNitrogenUptakeByMainCrop= 0; 
		annualNitrogenUptakeByInterCrop = 0; 
		annualNitrogenFertilisationMineralInMainCrop = 0;
		annualNitrogenFertilisationMineralInInterCrop = 0; 
		annualNitrogenIrrigationInMainCrop = 0;
		annualNitrogenIrrigationInInterCrop = 0;
		annualNitrogenRainInMainCrop = 0;
		annualNitrogenRainInInterCrop = 0;
		
		annualTreesCarbonBranches= 0;	
		annualTreesCarbonCoarseRoots= 0;	
		annualTreesCarbonFineRoots= 0;
		annualTreesCarbonLabile= 0;
		annualTreesCarbonStem= 0;
		annualTreesCarbonStump= 0;

	}	
	/**
	 * Compute all saturated voxels regards to water table depth
	 */
	public void computeWaterTable(SafeInitialParameters safeSettings, 
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
	public void deepSenescentRootsMineralization(SafeInitialParameters safeSettings) {
		
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxels = cell.getVoxels();

			// for each voxel of the cell
			for (int iz = 0; iz < voxels.length; iz++) {				

				if(!voxels[iz].getIsSaturated()) 
					voxels[iz].deepSenescentRootsMineralization(safeSettings, cell.getCrop().sticsSoil, cell.getArea());
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
	public float getMainCropArea() {return mainCropArea;}
	public float getInterCropArea() {return interCropArea;}
	public float getTreeCropDistance() {return treeCropDistance;}
	public float getTreeCropRadius() {return treeCropRadius;}
	public void setTreeCropDistance(double v) {treeCropDistance = (float) v;}
	public void setTreeCropRadius(double v) {treeCropRadius = (float) v;}
	public void setCellSurface(double e) {cellSurface = (float) e;}
	public void setMainCropArea(double e) {mainCropArea = (float) e;}
	public void setInterCropArea (double e) {interCropArea= (float) e;}
	
	/**
	 * Get main crop name
	 */
	public String getMainCropName () {
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCrop crop = ((SafeCell) c.next()).getCrop();
			if (crop.IsMainCropSpecies()) {
				return crop.getCropSpecies().getName();			
			}
		}
		return "";		
	}
	/**
	 * Get inter crop name
	 */
	public String getInterCropName () {
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCrop crop = ((SafeCell) c.next()).getCrop();
			if (!crop.IsMainCropSpecies()) {
				return crop.getCropSpecies().getName();			
			}
		}
		return "";		
	}
	
	//Methods for exportation about  LIGHT BUDGET
	public double getParInterceptedByTrees() {
		double par = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			par += t.getDiffuseParIntercepted() + t.getDirectParIntercepted();
		}
		return par / this.getArea();
	}

	public double getParInterceptedByTreesCompetFree() {
		double par = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			par += (t.getDiffuseParIntercepted() + t.getDirectParIntercepted()) / t.getLightCompetitionIndex();
		}
		return par / this.getArea();
	}

	public double getParInterceptedByMainCrop() {
		double par = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCrop crop = ((SafeCell) c.next()).getCrop();
			if (crop.IsMainCropSpecies()) {
				par += (crop.getDiffuseParIntercepted() + crop.getDirectParIntercepted()) * this.getCellSurface();
			}
		}
		return par / this.getMainCropArea();
	}

	public double getParInterceptedByInterCrop() {

		if (this.getInterCropArea() > 0) {
			double par = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCrop crop = ((SafeCell) c.next()).getCrop();
				if (!crop.IsMainCropSpecies()) {
					par += (crop.getDiffuseParIntercepted() + crop.getDirectParIntercepted()) * this.getCellSurface();
				}
			}
			return par / this.getInterCropArea();
		}
		else return 0;
	}
	
	public double getParInterceptedByMainCropCompetFree() {
		double par = 0; 
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCrop crop = ((SafeCell) c.next()).getCrop();
			if (crop.IsMainCropSpecies()) {
				par += (crop.getDiffuseParIntercepted() + crop.getDirectParIntercepted())
						/ crop.getCompetitionIndexForTotalPar() * getCellSurface();
			}
		}
		return par / this.getMainCropArea();
	}
	
	public double getParInterceptedByInterCropCompetFree() {
		double par = 0; 
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCrop crop = ((SafeCell) c.next()).getCrop();
			if (!crop.IsMainCropSpecies()) {
				par += (crop.getDiffuseParIntercepted() + crop.getDirectParIntercepted())
						/ crop.getCompetitionIndexForTotalPar() * getCellSurface();
			}
		}
		return par / this.getMainCropArea();
	}

	public double getParIncident() {
		SafeMacroClimat climat = ((SafeModel) this.getScene().getStep().getProject().getModel()).getMacroClimat();
		int julianDay = ((SafeStand) this.getScene()).getJulianDay();
		int year = ((SafeStand) this.getScene()).getWeatherYear();
		double par = 0;
		try {
			SafeDailyClimat dailyClimat = climat.getDailyWeather(year, julianDay);
			if (dailyClimat != null) {
				par = (double) dailyClimat.getGlobalPar();
				par *= this.getXSize() * this.getYSize();
			}
		} catch (Throwable e) {
			Log.println("weather data not found for day " + julianDay);
		}
		return par / this.getArea();
	}

	
	
	public double getParIncidentMainCrop() {
		double par = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeCrop crop = cell.getCrop();
			if (crop.IsMainCropSpecies()) {
				par += (cell.getDirectParIncident() +   cell.getDiffuseParIncident() )* getCellSurface();
			}
		}
		return par / this.getMainCropArea();
	}

	public double getParIncidentInterCrop() {
		
		if (this.getInterCropArea() > 0) {
			double par = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				SafeCrop crop = cell.getCrop();
				if (!crop.IsMainCropSpecies()) {
					par += (cell.getDirectParIncident() +   cell.getDiffuseParIncident() )* getCellSurface();
				}
			}
			return par / this.getInterCropArea();
		}
		else return 0;
	}
	
	
	//***************************************************
	// Methods for exportation about WATER BUDGET
	//***************************************************
	public double getWaterStock() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				waterStock += Math
						.max(voxel[i].getWaterStock()/*-voxel[i].getLayer().getInaccessibleWater()*voxel[i].getVolume()*1000*/,
								0);
			}
		}
		return waterStock / this.getArea();
	}

	public double getMaximalWaterStock() {
		double maxWaterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				maxWaterStock += (voxel[i].getLayer().getFieldCapacity() - voxel[i].getLayer()
						.getWiltingPoint()) * voxel[i].getVolume() * 1000;
			}
		}
		return maxWaterStock / this.getArea();
	}

	public double getWaterStockAvailableForTrees() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				boolean isTreeRoot = false;
				int nbTrees = ((SafeModel) this.getScene().getStep().getProject().getModel()).getPlotSettings().nbTrees;
				for (int j = 0; j < nbTrees; j++) {
					if (voxel[i].getTheTreeRootDensity(j) != 0)
						isTreeRoot = true;
				}
				if (isTreeRoot)
					waterStock += Math.max(voxel[i].getWaterStock() - voxel[i].getLayer().getWiltingPoint()
							* voxel[i].getVolume() * 1000, 0);
			}
		}
		return waterStock / this.getArea();
	}

	public double getWaterStockAvailableForMainCrop() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			if (cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();

				for (int i = 0; i < voxel.length; i++) {
					if (voxel[i].getCropRootDensity() != 0)
						waterStock += Math.max(voxel[i].getWaterStock() - voxel[i].getLayer().getWiltingPoint()
								* voxel[i].getVolume() * 1000, 0);
				}
			}
		}
		return waterStock / this.getArea();
	}

	public double getWaterStockAvailableForInterCrop() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();

				for (int i = 0; i < voxel.length; i++) {
					if (voxel[i].getCropRootDensity() != 0)
						waterStock += Math.max(voxel[i].getWaterStock() - voxel[i].getLayer().getWiltingPoint()
								* voxel[i].getVolume() * 1000, 0);
				}
			}
		}
		return waterStock / this.getArea();
	}

	
	
	
	
	public double getWaterStockCommonTreeMainCrop() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();

				for (int i = 0; i < voxel.length; i++) {
					boolean isTreeRoot = false;

					int nbTrees = ((SafeModel) this.getScene().getStep().getProject().getModel()).getPlotSettings().nbTrees;
					for (int j = 0; j < nbTrees; j++) {
						if (voxel[i].getTheTreeRootDensity(j) != 0)
							isTreeRoot = true;
					}
					if (isTreeRoot && (voxel[i].getCropRootDensity() != 0))
						waterStock += Math.max(voxel[i].getWaterStock() - voxel[i].getLayer().getWiltingPoint()
								* voxel[i].getVolume() * 1000, 0);
				}
			}
		}
		return waterStock / this.getArea();
	}

	public double getWaterStockInSaturation() {
		double waterStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				if (voxel[i].getIsSaturated())
					waterStock += Math.max(voxel[i].getWaterStock() - voxel[i].getLayer().getWiltingPoint()
							* voxel[i].getVolume() * 1000, 0);
			}
		}
		return waterStock / this.getArea();
	}

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

	public double getWaterUptakeByTrees() {
		double waterUptake = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree tree = (SafeTree) c.next();
			waterUptake += tree.getWaterUptake();
		}

		return waterUptake / this.getArea();
	}

	public double getWaterUptakeByMainCrop() {
		double waterUptake = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {			
				waterUptake +=cell.getCrop().getWaterUptake();
			}
		}
		return waterUptake / this.getArea();
	}

	public double getWaterUptakeByInterCrop() {
		double waterUptake = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				waterUptake +=cell.getCrop().getWaterUptake();
			}
		}
		return waterUptake/ this.getArea();
	}


	public double getWaterEvaporatedInMainCrop() {
		double waterEvap = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();
	
				for (int i = 0; i < voxel.length; i++) {
					waterEvap += voxel[i].getEvaporation();
				}
			}
		}
		return waterEvap / this.getArea();
	}

	public double getWaterEvaporatedInInterCrop() {
		double waterEvap = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();
	
				for (int i = 0; i < voxel.length; i++) {
					waterEvap += voxel[i].getEvaporation();
				}
			}
		}
		return waterEvap / this.getArea();
	}
	

	public double getIrrigationInMainCrop() {
		double irrig = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				irrig += cell.getCrop().getIrrigation() * getCellSurface();
			}
		}
		return irrig / this.getMainCropArea();
	}

	public double getIrrigationInInterCrop() {
		
		if (this.getInterCropArea() > 0) {
			double irrig = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					irrig += cell.getCrop().getIrrigation() * getCellSurface();
				}
			}			
			return irrig / this.getInterCropArea();
		}
		else return 0;
	}
	
	
	
	
	

	public double getWaterAddedByWaterTable() {
		double water = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			water += cell.getWaterAddedByWaterTable() * getCellSurface();
		}
		return water / this.getArea();
	}

	public double getWaterToDesaturation() {
		double waterDesat = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			waterDesat += cell.getWaterTakenByDesaturation() * getCellSurface();
		}
		return waterDesat / this.getArea();
	}

	public double getRainTransmittedByTrees() {
		double rain = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			rain += ((SafeCell) c.next()).getRainTransmittedByTrees() * getCellSurface();
		}
		return rain / this.getArea();
	}
	
	public double getRunOff() {
		double runOff = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			runOff += ((SafeCell) c.next()).getCrop().getRunOff() * getCellSurface();
		}
		return runOff / this.getArea();
	}

	public double getSurfaceRunOff() {
		double runOff = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			runOff += ((SafeCell) c.next()).getCrop().getSurfaceRunOff() * getCellSurface();
		}
		return runOff / this.getArea();
	}

	public double getInterceptedRainByTrees() {
		double rain = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			rain += t.getInterceptedRain();
		}
		return rain / this.getArea();
	}
	
	
	public double getInterceptedRainByMainCrop() {
		double intWater = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				intWater += cell.getRainInterceptedByCrop() * getCellSurface();
			}
		}
		return intWater /  this.getMainCropArea();
	}

	
	public double getInterceptedRainByInterCrop() {
		if (this.getInterCropArea() > 0) {
			double intWater = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					intWater += cell.getRainInterceptedByCrop() * getCellSurface();
				}
			}			
			return intWater / this.getInterCropArea();
		}
		else return 0;
		

	}
	
	public double getDrainageBottom() {
		double drainage = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			
			drainage += ((SafeCell) c.next()).getCrop().getDrainageBottom() * getCellSurface();
		}
		return drainage / this.getArea();
	}

	public double getDrainageArtificial() {
		double drainage = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			
			drainage += ((SafeCell) c.next()).getCrop().getDrainageArtificial() * getCellSurface();
		}
		return drainage / this.getArea();
	}
	
	public double getWaterUptakeInSaturationByTrees() {
		double water = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			water += ((SafeCell) c.next()).getWaterUptakeInSaturationByTrees();
		}
		return water / this.getArea();
	}

	public double getWaterUptakeInSaturationByMainCrop() {
		double water = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				water += cell.getWaterUptakeInSaturationByCrop();
			}
		}
		return water / this.getMainCropArea();
	}

	

	public double getWaterUptakeInSaturationByInterCrop() {
		if (this.getInterCropArea() > 0) {
			double water = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					water += cell.getWaterUptakeInSaturationByCrop();
				}
			}
		
			return water / this.getInterCropArea();
		}
		else return 0;
	}

	//***************************************************
	//NITROGEN BUDGET
	//***************************************************
	public double getNitrogenStockAvailableForTrees() {
		double nitrogenStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				boolean isTreeRoot = false;
				int nbTrees = ((SafeModel) this.getScene().getStep().getProject().getModel()).getPlotSettings().nbTrees;
				for (int j = 0; j < nbTrees; j++) {
					if (voxel[i].getTheTreeRootDensity(j) != 0)
						isTreeRoot = true;
				}
				if (isTreeRoot)
					nitrogenStock += voxel[i].getNitrogenNo3Stock() + voxel[i].getNitrogenNh4Stock();
			}
		}
		return nitrogenStock / this.getArea();
	}

	public double getNitrogenStockAvailableForMainCrop() {
		double nitrogenStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			if (cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();

				for (int i = 0; i < voxel.length; i++) {
					if (voxel[i].getCropRootDensity() != 0)
						nitrogenStock += voxel[i].getNitrogenNo3Stock() + voxel[i].getNitrogenNh4Stock();
				}
			}
		}
		return nitrogenStock / this.getArea();
	}

	public double getNitrogenStockAvailableForInterCrop() {
		double nitrogenStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();

			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();

				for (int i = 0; i < voxel.length; i++) {
					if (voxel[i].getCropRootDensity() != 0)
						nitrogenStock += voxel[i].getNitrogenNo3Stock() + voxel[i].getNitrogenNh4Stock();
				}
			}
		}
		return nitrogenStock / this.getArea();
	}
	
	
	public double getNitrogenStockCommonTreeMainCrop() {
		double nitrogenStock = 0;

		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeVoxel[] voxel = cell.getVoxels();

				for (int i = 0; i < voxel.length; i++) {
					boolean isTreeRoot = false;

					int nbTrees = ((SafeModel) this.getScene().getStep().getProject().getModel()).getPlotSettings().nbTrees;
					for (int j = 0; j < nbTrees; j++) {
						if (voxel[i].getTheTreeRootDensity(j) != 0)
							isTreeRoot = true;
					}
					if (isTreeRoot && (voxel[i].getCropRootDensity() != 0))
						nitrogenStock += voxel[i].getNitrogenNo3Stock() + voxel[i].getNitrogenNh4Stock();
				}
			}
		}
		return nitrogenStock / this.getArea();
	}
	
	public double getNitrogenUptakeByTrees() {
		double nitrogenUptake= 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree tree = (SafeTree) c.next();
			nitrogenUptake += tree.getNitrogenUptake();
		}		
		return nitrogenUptake *10000 / this.getArea(); //convert in kg ha-1
	}

	public double getNitrogenUptakeByMainCrop() {
		double nitrogenUptake = 0;
		int cpt = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				nitrogenUptake +=cell.getCrop().getNitrogenUptake();
				cpt++;
			}
		}

		if (nitrogenUptake > 0) return nitrogenUptake/cpt; //already in kg ha-1
		else return 0;
	}

	public double getNitrogenUptakeByInterCrop() {
		double nitrogenUptake = 0;
		int cpt = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				nitrogenUptake +=cell.getCrop().getNitrogenUptake();
				cpt++;
			}
		}
		if (nitrogenUptake > 0) return nitrogenUptake/cpt; //already in kg ha-1
		else return 0;
	}	

	public double getNitrogenFertilisationMineralInMainCrop() {
		double nitrogen = 0; 
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				nitrogen += cell.getCrop().getNitrogenFertilisationMineral() * getCellSurface();
			}
		}
		return nitrogen / this.getMainCropArea();
	}
	
	public double getNitrogenFertilisationMineralInInterCrop() {
		if (this.getInterCropArea() > 0) {
			double nitrogen = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					nitrogen += cell.getCrop().getNitrogenFertilisationMineral() * getCellSurface();
				}
			}
		
			return nitrogen / this.getInterCropArea();
		}
		else return 0;
	}
	

	
	public double getNitrogenIrrigationInMainCrop() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				nitrogen += cell.getCrop().getNitrogenIrrigation() * getCellSurface();
			}
		}
		return nitrogen / this.getMainCropArea();
	}
	
	public double getNitrogenIrrigationInInterCrop() {
		
		if (this.getInterCropArea() > 0) {
			double nitrogen = 0;
	
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					nitrogen += cell.getCrop().getNitrogenIrrigation() * getCellSurface();
				}
			}

			return nitrogen / this.getInterCropArea();
		}
		else return 0;
	}
	
	public double getNitrogenRainInMainCrop() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				nitrogen += cell.getCrop().getNitrogenRain() * getCellSurface();
			}
		}
		return nitrogen / this.getMainCropArea();
	}
	
	public double getNitrogenRainInInterCrop() {
		if (this.getInterCropArea() > 0) {
			double nitrogen = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					nitrogen += cell.getCrop().getNitrogenRain() * getCellSurface();
				}
			}
		
			return nitrogen / this.getInterCropArea();
		}
		else return 0;
	}	
	
	
	
	
	public double getNitrogenRunOffInMainCrop() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				nitrogen += cell.getNitrogenRunOff() * getCellSurface();
			}
		}
		return nitrogen / this.getMainCropArea();
	}
	
	public double getNitrogenRunOffInInterCrop() {
		if (this.getInterCropArea() > 0) {
			double nitrogen = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					nitrogen += cell.getNitrogenRunOff() * getCellSurface();
				}
			}
		
			return nitrogen / this.getInterCropArea();
		}
		else return 0;
	}	
	
	public double getNitrogenUptakeInSaturationByTrees() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			nitrogen += ((SafeCell) c.next()).getNitrogenUptakeInSaturationByTrees();
		}
		return nitrogen / this.getArea();
	}

	public double getNitrogenUptakeInSaturationByMainCrop() {
		double nitrogen = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				nitrogen += cell.getNitrogenUptakeInSaturationByCrop();
			}
		}
		return nitrogen / this.getMainCropArea();
	}
	
	public double getNitrogenUptakeInSaturationByInterCrop() {
		double nitrogen = 0;
		if (this.getInterCropArea()>0) {
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					nitrogen += cell.getNitrogenUptakeInSaturationByCrop();
				}
			}
			return nitrogen / this.getInterCropArea();
		}
		else return 0;
	}
	public double getNitrogenFixation() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenFixation() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenHumusMineralisation() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenHumusMineralisation() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenResiduMineralisation() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenResiduMineralisation() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenDenitrification() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenDenitrification() * getCellSurface();
		}
		return temp / this.getArea();
	}




	public double getBiomassRestitution() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getBiomassRestitution() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getTotalCarbonResidus() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getCarbonResidus() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getTotalNitrogenResidus() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenResidus() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenLeachingBottom() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenLeachingBottom() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenLeachingWaterTable() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenLeachingWaterTable() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenAddedByWaterTable() { // aq 04.11.2011
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenAddedByWaterTable() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenImmobilisation() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenImmobilisation() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenVolatilisation() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenVolatilisation() * getCellSurface();
		}
		return temp / this.getArea();
	}

	public double getNitrogenVolatilisationOrganic() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getNitrogenVolatilisationOrganic() * getCellSurface();
		}
		return temp / this.getArea();
	}


//	public double getMineralNitrogenStock() {
//
//		double temp = 0;
//		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
//			SafeCell cell = (SafeCell) c.next();
//			SafeVoxel[] voxel = cell.getVoxels();
//			for (int i = 0; i < voxel.length; i++) {
//				temp += (voxel[i].getNitrogenNo3Stock() + voxel[i].getNitrogenNh4Stock());
//			}
//		}
//
//		return temp / this.getArea() * 10; // from g.m-2 to Kg.ha-1
//	}

	public double getMineralNitrogenStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			temp += cell.getMineralNitrogenStock();	//already in Kg.ha-1
		}
		return temp;
	}
	
	public double getNitrateStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			SafeVoxel[] voxel = cell.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				temp += voxel[i].getNitrogenNo3Stock();
			}
		}
		return temp / this.getArea() * 10; // from g.m-2 to Kg.ha-1
	}

	public double getTotalCarbonHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTotalCarbonHumusStock();
		}
		return temp/ this.getArea();
	}

	public double getTotalNitrogenHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTotalNitrogenHumusStock();
		}
		return temp/ this.getArea();
	}

	public double getInactiveCarbonHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getInactiveCarbonHumusStock();
		}
		return temp/ this.getArea();
	}

	public double getActiveCarbonHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getActiveCarbonHumusStock();
		}
		return temp/ this.getArea();
	}

	public double getInactiveNitrogenHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getInactiveNitrogenHumusStock();
		}
		return temp/ this.getArea();
	}

	public double getActiveNitrogenHumusStock() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getActiveNitrogenHumusStock();
		}
		return temp/ this.getArea();
	}

	
	//LITTERS
	public double getTotalTreeCarbonLeafLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeCarbonLeafLitter();
		}
		return temp;
	}

	public double getTotalTreeNitrogenLeafLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeNitrogenLeafLitter();
		}
		return temp;
	}

	public double getTotalTreeCarbonFineRootLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeCarbonFineRootLitter();
		}
		return temp;
	}

	public double getTotalTreeNitrogenFineRootLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeNitrogenFineRootLitter();
		}
		return temp;
	}
	
	public double getTotalTreeCarbonCoarseRootLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeCarbonCoarseRootLitter();
		}
		return temp;
	}

	public double getTotalTreeNitrogenCoarseRootLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeNitrogenCoarseRootLitter();
		}
		return temp;
	}	

	public double getTotalTreeCarbonFineRootDeepLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeCarbonFineRootDeepLitter();
		}
		return temp;
	}

	public double getTotalTreeNitrogenFineRootDeepLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeNitrogenFineRootDeepLitter();
		}
		return temp;
	}
	
	public double getTotalTreeCarbonCoarseRootDeepLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeCarbonCoarseRootDeepLitter();
		}
		return temp;
	}

	public double getTotalTreeNitrogenCoarseRootDeepLitter() {
		double temp = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			temp += ((SafeCell) c.next()).getCrop().getTreeNitrogenCoarseRootDeepLitter(); 
		}
		return temp;
	}	
	//***************************************************
	// Export methods about main crop 
	//***************************************************
	//daily export lai
	public double getMainCropMeanLai() {
		double lai = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				lai += cell.getCrop().getLai();
				nbcell++;
			}
		}
		return lai/nbcell;
	}
	
	
	
	//daily export eai
	public double getMainCropMeanEai() {
		double eai = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				eai += cell.getCrop().getEai();
				nbcell++;
			}
		}
		return (eai / nbcell);
	}
	
	
	
	//daily export biomass
	public double getMainCropMeanBiomass() {
		double biomass = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				biomass += cell.getCrop().getBiomass();
				nbcell++;
			}
		}
		return (biomass / nbcell);
	}

	
	//daily export height
	public double getMainCropMeanHeight() {
		double height = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				height += cell.getCrop().getHeight();
				nbcell++;
			}
		}
		return (height / nbcell);
	}

	
		
	//daily export yield
	public double getMainCropMeanYield() {
		double yield = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				yield += cell.getCrop().getGrainBiomass();
				nbcell++;
			}
		}
		return (yield / nbcell);
	}

	public double getMainCropSceneYield() {
		double yield = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			nbcell++;
			if (cell.getCrop().IsMainCropSpecies()) 
				yield += cell.getCrop().getGrainBiomass();
		}
		return (yield / nbcell);
	}	
	
	
	
	
	
	//annual crop lai, eai, biomass, yield export
	public double getMainCropMeanPeakLai() {
		double lai = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				lai += cell.getCrop().getLaiMax();
				nbcell++;
			}
		}
		return (lai / nbcell);
	}
	
	public double getMainCropMeanPeakEai() {
		double eai = 0;
		int nbcell = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				eai += cell.getCrop().getEaiMax();
				nbcell++;
			}
		}
		return (eai / nbcell);
	}
	
	public double getMainCropMeanPeakBiomass() {
		double biomass = 0;
		double cropsurface = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				biomass += cell.getCrop().getBiomassMax() * this.getCellSurface();
				cropsurface += this.getCellSurface();
			}
		}
		return (biomass / cropsurface);
	}
	
	
	
	
	// Export methods about inter crop 
	
		//daily export lai
		public double getInterCropMeanLai() {
			double lai = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					lai += cell.getCrop().getLai();
					nbcell++;
				}
			}
			return lai/nbcell;
		}
		
		
		
		//daily export eai
		public double getInterCropMeanEai() {
			double eai = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					eai += cell.getCrop().getEai();
					nbcell++;
				}
			}
			return (eai / nbcell);
		}
		
		
		
		//daily export biomass
		public double getInterCropMeanBiomass() {
			double biomass = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					biomass += cell.getCrop().getBiomass();
					nbcell++;
				}
			}
			return (biomass / nbcell);
		}

		
		//daily export height
		public double getInterCropMeanHeight() {
			double height = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					height += cell.getCrop().getHeight();
					nbcell++;
				}
			}
			return (height / nbcell);
		}

		
					
		//daily export yield
		public double getInterCropMeanYield() {
			double yield = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					yield += cell.getCrop().getGrainBiomass();
					nbcell++;
				}
			}
			return (yield / nbcell);
		}

		public double getInterCropSceneYield() {
			double yield = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				nbcell++;
				if (!cell.getCrop().IsMainCropSpecies()) 
					yield += cell.getCrop().getGrainBiomass();
			}
			return (yield / nbcell);
		}	
		
		
		
		
		
		//annual crop lai, eai, biomass, yield export
		public double getInterCropMeanPeakLai() {
			double lai = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					lai += cell.getCrop().getLaiMax();
					nbcell++;
				}
			}
			return (lai / nbcell);
		}
		
		public double getInterCropMeanPeakEai() {
			double eai = 0;
			int nbcell = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					eai += cell.getCrop().getEaiMax();
					nbcell++;
				}
			}
			return (eai / nbcell);
		}
		
		public double getInterCropMeanPeakBiomass() {
			double biomass = 0;
			double cropsurface = 0;
			for (Iterator c = this.getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				if (!cell.getCrop().IsMainCropSpecies()) {
					biomass += cell.getCrop().getBiomassMax() * this.getCellSurface();
					cropsurface += this.getCellSurface();
				}
			}
			return (biomass / cropsurface);
		}
		
	
	
	// other export data for main crop
	
	public double getMainCropQngrain() {
		double qNgrain = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies())
				qNgrain += cell.getCrop().getQngrain() * getCellSurface();
		}
		return qNgrain / this.getArea();
	}

	public double getMainCropQnplante() {
		double qnplant = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies())
				qnplant += cell.getCrop().getQnplante() * getCellSurface();
		}
		return qnplant / this.getArea();
	}

	public double getMainCropCngrain() {
		double cNgrain = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies())
				cNgrain += cell.getCrop().getCngrain() * getCellSurface();
		}
		return cNgrain / this.getArea();
	}

	public double getMainCropCnplante() {
		double cnplant = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies())
				cnplant += cell.getCrop().getCnplante() * getCellSurface();
		}
		return cnplant / this.getArea();
	}

	public double getMainCropSla() {
		double sla = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					sla += crop.getSla();
					count++;
				}
			}
		}
		if (count > 0)
			sla /= count;
		return sla;
	}

	public double getMainCropGrainNumber() {
		double grainNumber = 0;
		int count = 0;
		double cropYield = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropYield = crop.getGrainBiomass();
				if (cropYield != 0) {
					grainNumber += crop.getGrainNumber();
					count++;
				}
			}
		}
		if (count > 0)
			grainNumber /= count;
		return grainNumber;
	}

	public double getMainCropGrainWeight() {
		double grainWeight = 0;
		int count = 0;
		double cropYield = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropYield = crop.getGrainBiomass();
				if (cropYield != 0) {
					grainWeight += crop.getGrainWeight();
					count++;
				}
			}
		}
		if (count > 0)
			grainWeight /= count;
		return grainWeight;
	}

	public double getMainCropPhenologicStage() {
		double phenologicStage = 0;
		int count = 0;
		double cropBiomass = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropBiomass = crop.getBiomass();
				if (cropBiomass != 0) {
					phenologicStage += crop.getPhenologicStage();
					count++;
				}
			}
		}
		if (count > 0)
			phenologicStage /= count;
		return phenologicStage;
	}

	public double getMainCropPlantDensity() {
		double plantDensity = 0;
		int count = 0;
		double cropBiomass = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropBiomass = crop.getBiomass();
				if (cropBiomass != 0) {
					plantDensity += crop.getPlantDensity();
					count++;
				}
			}
		}
		if (count > 0)
			plantDensity /= count;
		return plantDensity;
	}

	public double getMainCropWaterStress() {
		double waterStress = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					waterStress += crop.getHisafeWaterStress();
					count++;
				}
			}
		}
		if (count > 0)
			waterStress /= count;
		return waterStress;
	}

	
	public double getMainCropWaterDemand() {
		double wdem = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				wdem += crop.getWaterDemand();
			}
		}
		return wdem / this.getArea();
	}

	public double getMainCropWaterDemandReduced() {
		double wdem = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				wdem += crop.getWaterDemandReduced();
			}
		}
		wdem /= this.getArea();
		return wdem;
	}

	public double getMainCropWaterPotential() {
		double wpot = 0;
		int count = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				wpot += crop.getFineRoots().getActualWaterPotential();
				count++;
			}
		}
		return wpot / count;
	}

	public double getMainCropTotalRootLength() {
		double frl = 0;
		int count = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				frl += crop.getTotalRootLength();
				count++;
			}
		}
		return frl / count;
	}


	public double getMainCropStomatalWaterStress() {
		double waterStress = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					waterStress += crop.getSticsWaterStomatalStress();
					count++;
				}
			}
		}
		if (count > 0)
			waterStress /= count;
		return waterStress;
	}	
	public double getMainCropSenescenceWaterStress() {
		double waterStress = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					waterStress += crop.getSticsWaterSenescenceStress();
					count++;
				}
			}
		}
		if (count > 0)
			waterStress /= count;
		return waterStress;
	}

	public double getMainCropTurgescenceWaterStress() {
		double waterStress = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					waterStress += crop.getSticsWaterTurgescenceStress();
					count++;
				}
			}
		}
		if (count > 0)
			waterStress /= count;
		return waterStress;
	}



	public double getMainCropTemperature() {
		double temp = 0;
		int count = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				temp += crop.getCropTemperature();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMainCropSoilSurfaceTemperature() {
		double temp = 0;
		int count = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				temp += crop.getSoilSurfaceTemperature();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}
	
	public double getMainCropRootDepth() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					temp += crop.getRootDepth();
					count++;
				}
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMainCropNitrogenStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					temp += crop.getHisafeNitrogenStress();
					count++;
				}
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	
	public double getMainCropNitrogenLaiStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					temp += crop.getSticsNitrogenLaiStress();
					count++;
				}
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMainCropNitrogenBiomassStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					temp += crop.getSticsNitrogenBiomassStress();
					count++;
				}
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	public double getMainCropNitrogenSenescenceStress() {
		double temp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					temp += crop.getSticsNitrogenSenescenceStress();
					count++;
				}
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	

	



	public double getMainCropNitrogenDemand() {
		double temp = 0;
		int count = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				temp += crop.getNitrogenDemand();
				count++;
			}
		}
		if (count > 0)
			temp /= count;
		return temp;
	}

	// Export methods about trees

	

	
	public double getCarbonBranches() {
		double carbonBranches = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			carbonBranches += t.getCarbonBranches();
		}
		return carbonBranches * 10000 / this.getArea(); //convert in kg ha-1
	}

	
	public double getCarbonCoarseRoots() {
		double carbonCoarseRoots = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			carbonCoarseRoots += t.getCarbonCoarseRoots();
		}
		return carbonCoarseRoots * 10000 / this.getArea();  //convert in kg ha-1
	}
	
	public double getCarbonFineRoots() {
		double carbonFineRoots = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			carbonFineRoots += t.getCarbonFineRoots();
		}
		return carbonFineRoots * 10000 / this.getArea();  //convert in kg ha-1
	}
	
	public double getCarbonStem() {
		double carbonStem = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			carbonStem+= t.getCarbonStem();
		}
		return carbonStem * 10000 / this.getArea();  //convert in kg ha-1
	}

	public double getCarbonStump() {
		double carbonStump = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			carbonStump+= t.getCarbonStump();
		}
		return carbonStump * 10000 / this.getArea();  //convert in kg ha-1
	}
	
	public double getCarbonLabile() {
		double carbonLabile = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			carbonLabile += t.getCarbonLabile();
		}
		return carbonLabile * 10000 / this.getArea();  //convert in kg ha-1
	}
	
	
	public double getCarbonFoliageMax() {
		double carbonFoliageMax = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			if (carbonFoliageMax < t.getCarbonFoliageMax()) carbonFoliageMax= t.getCarbonFoliageMax(); 
		}
		return carbonFoliageMax;
	}
	
	public double getLeafAreaMax() {
		double laefAreaMax = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			if (laefAreaMax < t.getLeafAreaMax()) laefAreaMax= t.getLeafAreaMax(); 
		}
		return laefAreaMax;
	}	
	
	
	public double getNitrogenBranches() {
		double nitrogenBranches = 0;
		for (Iterator c = ((SafeStand) this.getScene()).getTrees().iterator(); c.hasNext();) {
			SafeTree t = (SafeTree) c.next();
			nitrogenBranches += t.getNitrogenBranches();
		}
		return nitrogenBranches * 10000 / this.getArea();  //convert in kg ha-1
	}

	
	
	//POUR FRANCESCO
	public double getMainCropTempStressLue() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					ftemp += crop.getSticsCrop().ftemp;
					count++;
				}
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}
	
	public double getMainCropTempStressGrainFilling() {
		double ftempremp = 0;
		int count = 0;
		double stade = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				stade = crop.getPhenologicStage();
				//entre stade DRP et MAT
				if ((stade >= 8) && (stade <= 10)) {
					ftempremp += crop.getSticsCrop().ftempremp;
					count++;
				}
			}
		}
		if (count > 0)
			ftempremp /= count;
		return ftempremp;
	}

	public double getMainCropFrostStressPlantDensity() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();

				ftemp += crop.getSticsCrop().fgellev;
				count++;
				
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}

	
	public double getMainCropFrostStressFoliage() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();

				ftemp += crop.getSticsCrop().fstressgel;
				count++;
				
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}

	public double getMainCropFrostStressReprod() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();

				ftemp += crop.getSticsCrop().fgelflo;
				count++;
				
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}
	
	
	
	
	
	
	public double getInterCropTempStressLue() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				cropLai = crop.getLai();
				if (cropLai != 0) {
					ftemp += crop.getSticsCrop().ftemp;
					count++;
				}
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}
	
	public double getInterCropTempStressGrainFilling() {
		double ftempremp = 0;
		int count = 0;
		double stade = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();
				stade = crop.getPhenologicStage();
				//entre stade DRP et MAT
				if ((stade >= 8) && (stade <= 10)) {
					ftempremp += crop.getSticsCrop().ftempremp;
					count++;
				}
			}
		}
		if (count > 0)
			ftempremp /= count;
		return ftempremp;
	}

	public double getInterCropFrostStressPlantDensity() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();

				ftemp += crop.getSticsCrop().fgellev;
				count++;
				
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}

	
	public double getInterCropFrostStressFoliage() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();

				ftemp += crop.getSticsCrop().fstressgel;
				count++;
				
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}

	public double getInterCropFrostStressReprod() {
		double ftemp = 0;
		int count = 0;
		double cropLai = 0;
		for (Iterator c = this.getCells().iterator(); c.hasNext();) {
			SafeCell cell = (SafeCell) c.next();
			if (!cell.getCrop().IsMainCropSpecies()) {
				SafeCrop crop = cell.getCrop();

				ftemp += crop.getSticsCrop().fgelflo;
				count++;
				
			}
		}
		if (count > 0)
			ftemp /= count;
		return ftemp;
	}
	
	//TOTAL ANNUAL FOR EXPORT
	//LIGHT
	public double getAnnualParIncident() {
		return annualParIncident;
	}

	public double getAnnualParIncidentMainCrop() {
		return annualParIncidentMainCrop;
	}
	public double getAnnualParIncidentInterCrop() {
		return annualParIncidentInterCrop;
	}	
	public double getAnnualParInterceptedByTrees() {
		return annualParInterceptedByTrees;
	}
	public double getAnnualParInterceptedByMainCrop() {
		return annualParInterceptedByMainCrop;
	}
	public double getAnnualParInterceptedByInterCrop() {
		return annualParInterceptedByInterCrop;
	}
	
	//WATER
	public double getAnnualWaterUptakeByTrees() {
		return annualWaterUptakeByTrees;
	}
	public double getAnnualWaterUptakeByMainCrop() {
		return annualWaterUptakeByMainCrop;
	}
	public double getAnnualWaterUptakeByInterCrop() {
		return annualWaterUptakeByInterCrop;
	}

	public double getAnnualWaterEvaporatedInMainCrop() {
		return annualWaterEvaporatedInMainCrop;
	}
	public double getAnnualWaterEvaporatedInInterCrop() {
		return annualWaterEvaporatedInInterCrop;
	}
	public double getAnnualWaterAddedByWaterTable() {
		return annualWaterAddedByWaterTable;
	}
	public double getAnnualWaterToDesaturation() {
		return annualWaterToDesaturation;
	}

	public double getAnnualRunOff() {
		return annualRunOff;
	}	
	public double getAnnualSurfaceRunOff() {
		return annualSurfaceRunOff;
	}
	public double getAnnualInterceptedRainByTrees() {
		return annualInterceptedRainByTrees;
	}
	public double getAnnualInterceptedRainByMainCrop() {
		return annualInterceptedRainByMainCrop;
	}
	public double getAnnualInterceptedRainByInterCrop() {
		return annualInterceptedRainByInterCrop;
	}
	public double getAnnualDrainageBottom() {
		return annualDrainageBottom;
	}
	public double getAnnualDrainageArtificial() {
		return annualDrainageArtificial;
	}	
	public double getAnnualWaterUptakeInSaturationByTrees() {
		return annualWaterUptakeInSaturationByTrees;
	}
	public double getAnnualWaterUptakeInSaturationByMainCrop() {
		return annualWaterUptakeInSaturationByMainCrop;
	}
	public double getAnnualWaterUptakeInSaturationByInterCrop() {
		return annualWaterUptakeInSaturationByInterCrop;
	}	
	
	public double getAnnualIrrigationInMainCrop() {
		return annualIrrigationInMainCrop;
	}
	
	public double getAnnualIrrigationInInterCrop() {
		return annualIrrigationInInterCrop;
	}	
	
	//NITROGEN
	public double getAnnualNitrogenUptakeByTrees() {
		return annualNitrogenUptakeByTrees;
	}
	public double getAnnualNitrogenUptakeByMainCrop() {
		return annualNitrogenUptakeByMainCrop;
	}
	public double getAnnualNitrogenUptakeByInterCrop() {
		return annualNitrogenUptakeByInterCrop;
	}
	
	public double getAnnualNitrogenFertilisationMineralInMainCrop() {
		return annualNitrogenFertilisationMineralInMainCrop;
	}
	
	public double getAnnualNitrogenFertilisationMineralInInterCrop() {
		return annualNitrogenFertilisationMineralInInterCrop;
	}	
	public double getAnnualNitrogenIrrigationInMainCrop() {
		return annualNitrogenIrrigationInMainCrop;
	}
	
	public double getAnnualNitrogenIrrigationInInterCrop() {
		return annualNitrogenIrrigationInInterCrop;
	}	
	
	public double getAnnualNitrogenRainInMainCrop() {
		return annualNitrogenRainInMainCrop;
	}
	
	public double getAnnualNitrogenRainInInterCrop() {
		return annualNitrogenRainInInterCrop;
	}		
	
	
	//CARBON	
	public double getAnnualTreesCarbonBranches() {
		return annualTreesCarbonBranches;
	}
	public double getAnnualTreesCarbonCoarseRoots() {
		return annualTreesCarbonCoarseRoots;
	}
	public double getAnnualTreesCarbonFineRoots() {
		return annualTreesCarbonFineRoots;
	}
	public double getAnnualTreesCarbonLabile() {
		return annualTreesCarbonLabile;
	}
	public double getAnnualTreesCarbonStem() {
		return annualTreesCarbonStem;
	}	
	public double getAnnualTreesCarbonStump() {
		return annualTreesCarbonStump;
	}	

	
	
	public SafeSoil getSoil () {return soil;}
	public void setSoil (SafeSoil s) {soil = s;}

	
	public float getPrecipitation () {return precipitation;}
	public void setPrecipitation (float v) {precipitation = v;}
	
}
