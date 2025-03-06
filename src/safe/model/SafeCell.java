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
import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;

import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.ShiftItem;
import capsis.defaulttype.plotofcells.SquareCell;
import safe.stics.*;

/**
 * SafeCell is a square spatial division of a SafePlot 
 *
 * @author Isabelle Lecomte - INRAE Montpellier - july 2002 
 */
public class SafeCell extends SquareCell {


	private static final long serialVersionUID = 1L;
	
	//4 neighbourg cells used for toric symetry
	private static class Immutable2 implements Cloneable, Serializable {
		private static final long serialVersionUID = 1L;
		public int cellIdRight;					//ID of the right neighbourg cell x+
		public int cellIdLeft;					//ID of the left  neighbourg cell x-
		public int cellIdBack;					//ID of the back  neighbourg cell y+
		public int cellIdFront;					//ID of the front neighbourg cell y-
	}
	protected Immutable2 immutable2;

	private SafeCropZone cropZone;				//reference of the crop zone object 
	private SafeCrop crop;						//reference of the crop object sown on this cell
	private SafeVoxel [] voxels;				//references of voxels objects attached to this cell
	private int idTreePlanted;					//ID of tree if planted in this cell 
	private boolean isTreeAbove;				//true if one tree at least is above the cell 
	Collection<SafeTree> treeAbove;				//list of tree above this cell
	private float  laiTree;						//sum of lai of all trees above

	//LIGHT MODULE RESULTS (in % for each execution of the process lighting)
	private float relativeToFlatCellDirectParIncident;	// %
	private float relativeToFlatCellDiffuseParIncident;	// %
	private float relativeToFlatCellVisibleSky;			// %
	private float relativeToFlatCellDirectNirIncident;	// %
	private float relativeToFlatCellDiffuseNirIncident;	// %

	private float directParIncident;				//moles.m-2
	private float diffuseParIncident;				//moles.m-2
	private float relativeDirectParIncident;		//%
	private float relativeDiffuseParIncident;		//%
	private float relativeTotalParIncident;			//%
	private float relativeGlobalRadIncident;		//%
	private float visibleSky;						//%
	private float etpCalculated;					//etp calculated with global incident radiation
	
	//WATER AND NITROGEN BUDGET
	private float waterAddedByWaterTable;			//amout of water provided by the water table to saturated voxels (mm)
	private float waterTakenByDesaturation;			//amout of water taken    by the water table to desaturated voxels (mm)
	private float waterUptakeInSaturationByTrees;	//amout of water uptaken  by the trees in saturated voxels (liters) 
	private float waterUptakeInSaturationByCrop;	//amout of water uptaken  by the crop  in saturated voxels (liters) 
	private float nitrogenUptakeInSaturationByTrees;//amout of nitrogen uptaken  by the trees in saturated voxels (kg) 
	private float nitrogenUptakeInSaturationByCrop;	//amout of nitrogen uptaken  by the trees in saturated voxels (kg) 

	//MICROCLIMATE MODEL DAILY VALUE
	private float rainInterceptedByTrees; 			//Rain intercepted by trees on this cell (mm d-1)
	private float rainTransmittedByTrees; 			//Rain transmitted by trees on this cell (mm d-1)
	private float stemFlowByTrees; 					//Stemflow by trees on this cell (mm d-1)
	//rainInterceptedByCrop is taken directly from SticsCrop.interpluie[1] (mm d-1)
	//rainTransmittedByCrop is calculated (mm d-1) 
	//stemFlowByCrop is taken directly from SticsCrop.stemflow (mm d-1) 
	
	//CARBON SURFACE LITTER (Kg C )
	private double 	treeCarbonFoliageLitter;		//litter from tree foliage
	private double 	treeCarbonBranchesLitter;		//litter from tree branches
	private double 	treeCarbonFruitLitter;			//litter from tree fruit
	private double  treeCarbonFineRootsLitter;		//litter from tree fine roots
	private double  treeCarbonCoarseRootsLitter;	//litter from tree coarse roots
	
	//NITROGEN SURFACE LITTER (Kg N ha-1)
	private double 	treeNitrogenFoliageLitter;		//litter from tree foliage
	private double 	treeNitrogenBranchesLitter;		//litter from tree branches
	private double 	treeNitrogenFruitLitter;		//litter from tree fruit
	private double  treeNitrogenFineRootsLitter;	//litter from tree fine roots
	private double  treeNitrogenCoarseRootsLitter;	//litter from tree coarse roots
	
	//MONTLY MEAN VALUES FOR EXPORT
	private float monthDirectParIncident;			//moles PAR m-2
	private float monthDiffuseParIncident;			//moles PAR m-2
	private float monthVisibleSky;					//%	
	private float monthDirectPar;					//moles PAR m-2
	private float monthDiffusePar;					//moles PAR m-2
	private int   monthNbrDays;						//number of days in the month (used for monthly calculation) 					

	//ANNUAL VALUES FOR EXPORT
	private float annualWaterUptakeByTrees;			//liters
	private float annualNitrogenUptakeByTrees;		//liters
	private float annualParIncident;				//moles PAR m-2
	
	public SafeCell (SafePlot plot, Vertex3d coord, int i, int j, int id, int nbVoxels) {
		
		super (plot, id, 0, coord, i, j);			//SquareCell

		createImmutable2 ();
		immutable2.cellIdRight = 0;
		immutable2.cellIdLeft  = 0;
		immutable2.cellIdBack  = 0;
		immutable2.cellIdFront = 0;

		// Creation of the soil voxels
		crop = new SafeCrop (this);
		voxels = new SafeVoxel [nbVoxels];
		treeAbove = new TreeSet<SafeTree> (new SafeTreeHeightComparator());	//trees are sorted on tree height max to min
		
		//reset light results 
		resetDirect();
		resetDiffuse();
		
		this.relativeToFlatCellDirectParIncident=1f;
		this.relativeToFlatCellDiffuseParIncident=1f;
		this.relativeToFlatCellVisibleSky=1f;
		this.relativeToFlatCellDirectNirIncident=1f;
		this.relativeToFlatCellDiffuseNirIncident=1f;
		this.directParIncident=0f;
		this.diffuseParIncident=0f;
		this.relativeDirectParIncident=1f;
		this.relativeDiffuseParIncident=1f;
		this.relativeTotalParIncident=1f;
		this.relativeGlobalRadIncident=1f;
		this.visibleSky=1f;

		//reset water transfert results
		setRainTransmittedByTrees (0);
		setStemFlowByTrees (0); 
		setRainInterceptedByTrees (0); 
		setWaterAddedByWaterTable (0);
		setWaterTakenByDesaturation (0);
		setWaterUptakeInSaturationByTrees(0);	
		setWaterUptakeInSaturationByCrop(0);	
		setNitrogenUptakeInSaturationByTrees(0);	
		setNitrogenUptakeInSaturationByCrop(0);	

	}

	/**
	 * Create an Immutable object whose class is declared at one level of the hierarchy.
	 * This is called only in constructor for new logical object in superclass.
	 * If an Immutable is declared in subclass, subclass must redefine this method
	 * (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable2 () {immutable2 = new Immutable2 ();}

	/**
	 * Clone a SafeCell
	 */
	public Object clone () {

		SafeCell c = (SafeCell) super.clone ();	// calls protected Object Object.clone () {}
		return c;
	}
	/**
	 * Reset or add daily results on this cell
	 */
	public void dailyRaz () {
		
		//total Monthly values
		this.monthVisibleSky=this.monthVisibleSky+this.visibleSky;	
		this.monthNbrDays = this.monthNbrDays + 1;

		//Totals for EXPORT
		this.annualWaterUptakeByTrees = this.annualWaterUptakeByTrees + (float) this.getWaterUptakeByTrees();				
		this.annualNitrogenUptakeByTrees = this.annualNitrogenUptakeByTrees+ (float) this.getNitrogenUptakeByTrees();	
		this.annualParIncident = this.annualParIncident + (float) this.getTotalParIncident();

		//RAZ
		this.setWaterAddedByWaterTable (0);
		this.setWaterTakenByDesaturation (0);
		this.setWaterUptakeInSaturationByTrees (0);
		this.setWaterUptakeInSaturationByCrop (0);
		this.setNitrogenUptakeInSaturationByTrees (0);
		this.setNitrogenUptakeInSaturationByCrop (0);	
		this.setRainTransmittedByTrees (0);
		this.setRainInterceptedByTrees (0); 
		this.setStemFlowByTrees (0); 

	}
	
	/**
	 * Reset annual results on this cell
	 */
	public void razTotalAnnual () {
		annualWaterUptakeByTrees = 0;				
		annualNitrogenUptakeByTrees = 0;
		annualParIncident= 0 ;									
	}
	
	/**
	 * Reset diffuse incident energy on this cell
	 */
	public void resetDiffuse() {
		setRelativeToFlatCellDiffuseParIncident (0);
		setRelativeDiffuseParIncident (0);
		setRelativeToFlatCellDiffuseNirIncident(0);
		setRelativeToFlatCellVisibleSky (0);
		setVisibleSky(0);
		setRelativeGlobalRadIncident (0);
		setDiffuseParIncident(0);

	}
	/**
	 * Reset direct incident energy on this cell
	 */
	public void resetDirect() {
		setRelativeToFlatCellDirectParIncident(0);
		setRelativeDirectParIncident (0);
		setRelativeTotalParIncident (0);
		setRelativeToFlatCellDirectNirIncident(0);
		setRelativeGlobalRadIncident (0);
		setDirectParIncident(0);
	}

	/**
	 * Reset month results on this cell
	 */
	public void razTotalMonth () {
		monthDirectPar = 0; 
		monthDiffusePar = 0;
		monthDirectParIncident = 0; 
		monthDiffuseParIncident = 0;
		monthVisibleSky = 0;
		monthNbrDays = 0;
		this.getCrop().razTotalMonth();
	}
	/**
	* Agregation of STICS minicouches values in HISAFE voxels 
	* after STICS PART 1
	*/
	public void miniCouchesToVoxelsAfterStics1 (SafeSticsParameters sticsParam, 
												double waterTableDepth,
												int simulationDay,
												int sticsDay) {

		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;
		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;

		double cellArea = this.getArea(); 			//m2
		double zrac = sticsCrop.zrac;

		//FOR EACH VOXEL
		for (int i = 0; i < this.voxels.length; i++) {
			float cropRootDensity = 0;
			float cropRootEffectiveDensity = 0;
			float voxelNo3 = 0;
			float voxelNh4 = 0;
			float voxelMoisture = 0;
			float soilTemperature = 0;
			float soilEvapo    = 0;


			//number of miniCouches in this voxel
			int miniCoucheMin = voxels[i].getMiniCoucheMin();			//starting  miniCouches  for current voxel
			int miniCoucheMax = voxels[i].getMiniCoucheMax();	   		//ending    miniCouches  for current voxel
			int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	   	//number    miniCouches  for current voxel

			for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
				
				voxelMoisture 	+= sticsCommun.HUR[z];				    // voxel soil humidity 	%		
				soilEvapo  		+= sticsCommun.esz[z];				    // voxel soil evaporation 	mm		
				soilTemperature += sticsCommun.tsol[z];					// Soil temperature	degrees
				voxelNo3 		+= sticsSoil.nit[z+1];					// voxel soil no3 kg N ha-1		
				voxelNh4 		+= sticsSoil.amm[z];					// voxel soil nh4 kg N ha-1	
			
				//For these data No need to go further than root depth limit
				if (z <= zrac) 	{				
					cropRootDensity += sticsCrop.rljour [z] ;
					cropRootEffectiveDensity += sticsCrop.lracz [z] ;
				}		
			}


			//convert cm/cm3 in m/m3 (miniCouches is 1cm)
			cropRootDensity = (cropRootDensity * 10000) / miniCoucheNumber;
			cropRootEffectiveDensity = (cropRootEffectiveDensity * 10000) / miniCoucheNumber;
			voxels[i].setCropRootsDensity (cropRootDensity);
			voxels[i].setCropRootsEffectiveDensity (cropRootEffectiveDensity);

			//add a new root branch in the TOPOLOGY MAP
			if (cropRootDensity > 0) {
				if (! this.getCrop().getPlantRoots().getRootTopology ().containsKey(voxels[i])) {
					if (i > 0)
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], voxels[i-1], simulationDay, cropRootDensity);
					else
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], null, simulationDay, cropRootDensity);
				}
			}
			
			//UPDATE fine root density in the TOPOLOGY MAP
			if (this.getCrop().getPlantRoots().getRootTopology ().containsKey(voxels[i]))
				this.getCrop().getPlantRoots().setFineRootTopology (voxels[i], cropRootDensity);			

			//Store agregation result in voxel
			double newTheta 			= (voxelMoisture/10)/miniCoucheNumber;
			double newSoilTemperature	= soilTemperature/miniCoucheNumber;
			double newNo3				= voxelNo3;
			double newNh4 				= voxelNh4;

			//If voxel is saturated, we do not reset water and nitrogen stock
			//because of rounding error this involve small differences with Field Capacity 
			//Is it better to test FC <> THETA than to test IsStaturated because the first day it does not work well
			//SEE GITHUB issue #88
			//if (voxels[i].getLayer().getFieldCapacity()!= voxels[i].getTheta()) {
			if (voxels[i].getZ() < waterTableDepth) {
				voxels[i].setNitrogenNo3Stock ((newNo3 / 10) * cellArea);				    //convert kg ha-1 in g
				voxels[i].setNitrogenNh4Stock ((newNh4 / 10) * cellArea);				    //convert kg ha-1 in g
				voxels[i].setWaterStock (newTheta * voxels[i].getVolume () * 1000);		    //convert m3 m-3 in liters
			}

			voxels[i].setSoilTemperature(newSoilTemperature); 							//mean of temperature
			voxels[i].setEvaporation  (soilEvapo * cellArea);							//convert mm in liters;	

		}
	}


	/**
	* Agregation of STICS minicouches values in HISAFE voxels 
	* after STICS PART 2
	* Nitrogen have been updated by majNsol
	*/
	public void miniCouchesToVoxelsAfterStics2 (double waterTableDepth) {

		double cellArea = this.getArea(); 			//m2
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		
		//FOR EACH VOXEL
		for (int i = 0; i < this.voxels.length; i++) {
			
			//If voxel is saturated, we do not reset water and nitrogen stock
			//because of rounding error this involve small differences with Field Capacity 
			//Is it better to test FC <> THETA than to test IsStaturated because the first day it does not work well
			//SEE GITHUB issue #88
			//if (voxels[i].getLayer().getFieldCapacity()!= voxels[i].getTheta()) {
			if (voxels[i].getZ() < waterTableDepth) {	
				float voxelNo3 = 0;
				float voxelNh4 = 0;
				float voxelMoisture = 0;
	
				//number of miniCouches in this voxel
				int miniCoucheMin = voxels[i].getMiniCoucheMin();			//starting  miniCouches  for current voxel
				int miniCoucheMax = voxels[i].getMiniCoucheMax();	   		//ending    miniCouches  for current voxel
				int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	   	//number    miniCouches  for current voxel
				
				for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
					voxelMoisture 	+= sticsCommun.HUR[z];				    // voxel soil humidity 	%	
					voxelNo3 		+= sticsSoil.nit[z+1];					// voxel soil no3 kg N ha-1		
					voxelNh4 		+= sticsSoil.amm[z];					// voxel soil nh4 kg N ha-1	
				}
				

				//Store agregation result in voxel
				double newTheta 		= (voxelMoisture/10)/miniCoucheNumber;
				double newNo3			= voxelNo3;
				double newNh4 			= voxelNh4;
				voxels[i].setWaterStock (newTheta * voxels[i].getVolume () * 1000);		    //convert m3 m-3 in liters
				voxels[i].setNitrogenNo3Stock ((newNo3 / 10) * cellArea);				    //convert kg ha-1 in g
				voxels[i].setNitrogenNh4Stock ((newNh4 / 10) * cellArea);				    //convert kg ha-1 in g	
			}
		}
	}	
	
	/**
	* Agregation of STICS minicouches values in HISAFE voxels 
	* after STICS PART 2 
	* only if water extraction have been calculated by STICS (testing STICS stand alone) 
	*/
	public void miniCouchesToVoxelsAfterSticsWaterExtraction () {

		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;

		double totalWaterUptake = 0; 				//sum of water uptake in this cell
		double totalNitrogenUptake = 0; 			//sum of nitrogen uptake in this cell
		double cellArea = this.getArea(); 			//m2
		int zrac = (int) sticsCrop.zrac +1;	

		//FOR EACH VOXEL if THERE IS ROOTS
		if (sticsCrop.zrac > 0) {
			
			for (int i = 0; i < this.voxels.length; i++) {
	
				float cropWaterUptake  = 0;			
				float cropNitrogenUptake = 0;
	
				//number of miniCouches in this voxel
				int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
				int miniCoucheMax = voxels[i].getMiniCoucheMax();	   	//ending    miniCouches  for current voxel
				
				miniCoucheMax = Math.min(miniCoucheMax, zrac);
	
				for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
			
					//For these data No need to go further than root depth limit
					if (z <= zrac) {
						//if pure crop, crop water extraction is computed by STICS
						//real      :: epz(0:2,1000) 
						int indice     = (z*3)+1;
						cropWaterUptake    += sticsCrop.epz[indice];			// voxel crop water uptake 	mm		
						cropNitrogenUptake += sticsCommun.absz[z];			    // voxel crop nitrogen uptake kg N ha-1		
					}
				}

					
				//if pure crop, water and nitrogen extraction is done in STICS		
				voxels[i].setCropWaterUptake  (cropWaterUptake * cellArea);				// convert mm in liters
				voxels[i].setCropNitrogenUptake ((cropNitrogenUptake / 10) * cellArea );// convert kg ha-1 in g
				

				totalWaterUptake += cropWaterUptake; 		  		// total crop water uptake (all voxels)
				totalNitrogenUptake  += cropNitrogenUptake ;		// total crop nitrogen uptake (all voxels)
				
				//Sum water extracted in saturated zone in voxel is saturated by water table
				if (voxels[i].getIsSaturated ()) 
					this.addWaterUptakeInSaturationByCrop (cropWaterUptake * cellArea);	// gt - 5.02.2009			
			}		
		}
		
		this.getCrop().setWaterUptake (totalWaterUptake);			// mm
		this.getCrop().setNitrogenUptake (totalNitrogenUptake);		// kg ha-1

		double waterStress = 1;
		double nitrogenStress = 1;
		if (this.getCrop().getWaterDemand() > 0) {
			if(totalWaterUptake <= 0) {
				waterStress = 0.0001d; //P_swfacmin;						
			}
			else {
				waterStress = Math.min (totalWaterUptake  / this.getCrop().getWaterDemand(), 1);		
			}
			waterStress = Math.max (waterStress,  0.0001d);
		}
		//Set the stress 
		crop.setHisafeWaterStress (waterStress);
		
		if (this.getCrop().getNitrogenDemand() > 0) {
			if(totalNitrogenUptake <= 0) {
				nitrogenStress = 0.0001d; //P_swfacmin;						
			}
			else {
				nitrogenStress = Math.min (totalNitrogenUptake  / this.getCrop().getNitrogenDemand(), 1);		
			}
			nitrogenStress = Math.max (nitrogenStress,  0.0001d);
		}
		//Set the stress 
		crop.setHisafeNitrogenStress (nitrogenStress);
	}
	
	/**
	* Desagregation of HISAFE voxels in STICS miniCouches (crop and tree water and nitrogen uptake) 
	* Called after water and nitrogen competition calculation  
	*/	
	public void voxelsToMiniCouches (SafeGeneralParameters safeSettings, boolean isDebugMode) {

		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;

		double cellArea = this.getArea(); 	//m2
		int cropRootDepth = (int) (this.getCrop().getRootsDepth() * 100);	//il - 05.07.2017 verif stics int(zrac)
		float ha =  sticsCommun.ha*10; // gt - 05.02.2009 - residual humidity
	
		//RAZ tree and crop water and nitrogen extraction table before desagregation
		for (int i=0; i<1000; i++) {	
			int indice     = (i*3)+1;		////real      :: epz(0:2,1000) 
			sticsCrop.epz[indice] = 0;
			sticsCommun.absz[i]=0;
		}
		//il correction 16/02/2017
		for (int i=0; i<safeSettings.STICS_MINI_LAYERS; i++) {	
			sticsCommun.treeWaterUptake [i] =0;
			sticsCommun.treeNitrogenUptake [i] =0;

		}

		
		// if (tree+crop water uptake + soil evaporation) > (waterStock-ha) in a miniCouche
		// ha is residual water. 
		// In that case, tree and crop water  uptake are affected to the next miniCouche
		double reportWaterTree = 0;	
		double reportWaterCrop = 0;	

		
		//FOR EACH VOXEL
		for (int i=0; i < this.voxels.length; i++) {

			//IF voxel is SATURATED we don't extract water and nitrogen from STICS SOIL
			if (!voxels[i].getIsSaturated()) {
				//number of miniCouches in this voxel
				int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
				int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel
				int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	   //number    miniCouches  for current voxel
	
				//Grab voxel values for WATER and NITROGEN UPTAKE
				double cropWaterUptake = 0;
				double treeWaterUptake = 0;
				double cropNitrogenUptake = 0;
				double treeNitrogenUptake = 0;
	
				cropWaterUptake = voxels[i].getCropWaterUptake ();					//liters
				cropNitrogenUptake = voxels[i].getCropNitrogenUptake ();			//g N
				
				for (int t=0; t < safeSettings.nbTrees; t++) {
					treeWaterUptake += voxels[i].getTheTreeWaterUptake (t);			//liters
					treeNitrogenUptake += voxels[i].getTheTreeNitrogenUptake (t);	//g N

				}
				
				//if no extraction, no need to continue
				if ((treeWaterUptake > 0) || (treeNitrogenUptake > 0) || (cropWaterUptake > 0) || (cropNitrogenUptake > 0)) {
	
					//Compute nitrogen total (no3+nh4) in this voxel
					float nitrogenTotal = 0;
					float cropNitrogenTotal = 0;
					for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
						if (sticsSoil.nit[z+1] > 0) {
							nitrogenTotal += sticsSoil.nit[z+1];
							if  (cropRootDepth >= miniCoucheMin) {
								cropNitrogenTotal +=sticsSoil.nit[z+1];	
							}
						}
						if (sticsSoil.amm[z] > 0) {
							//il correction 16/02/2017
							//nitrogenTotal += sticsSoil.amm[z+1];
							nitrogenTotal += sticsSoil.amm[z];
							if (cropRootDepth >= miniCoucheMin)  {
								cropNitrogenTotal +=sticsSoil.amm[z];									
							}
						}
					}
					
					//Passage dans STICS de l'extraction en eau des arbres dans les minicouches 
					for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
						
	//IL 16/02/2017 Je ne comprends pas ce code, je l'enlève					
	/*					if (voxels[i].getIsSaturated() && (reportWaterTree > 0)) {	// gt - 5.02.2009 - if we have to report waterExtraction in a saturated voxel
							//sticsCommun.treeWaterUptake [z] = 0;
							sticsCommun.treeWaterUptake [z] = (float) reportWaterTree;	//il 05.07.2017 je pense c'est plus cohérent
							this.addWaterExtractedInSaturationByTrees(reportWaterTree);
							reportWaterTree = 0;
						} else {*/
							if ((sticsCommun.HUR[z]								// water content
								-sticsCommun.esz[z]								// soil evaporation
								-(treeWaterUptake/miniCoucheNumber/cellArea) 	// tree water uptake
								-(cropWaterUptake/miniCoucheNumber/cellArea)	// crop water uptake
								-reportWaterTree								// tree report from above
								-reportWaterCrop) 								// crop report from above
									< ha) {										// gt - 05.02.2009 - residual humidity
	
								sticsCommun.treeWaterUptake[z] = (float) Math.max(sticsCommun.HUR[z]
																	-sticsCommun.esz[z]
																	-cropWaterUptake/cellArea/miniCoucheNumber
																	-ha
																	-reportWaterCrop
															,0);
								reportWaterTree += treeWaterUptake/cellArea/miniCoucheNumber 
												- sticsCommun.treeWaterUptake[z];
	
							} else {
								sticsCommun.treeWaterUptake [z] = (float) ((treeWaterUptake/cellArea/miniCoucheNumber)	//convert liters in mm
																			+reportWaterTree);
								reportWaterTree = 0;

							}
					//	}
		
					
						int indice     = (z*3)+1;				//Variable sticsCrop.epz(0:2,1000) 
						//IL 16/02/2017 Je ne comprends pas ce code, je l'enlève
	/*					if (voxels[i].getIsSaturated() && (reportWaterCrop > 0)) {
							//sticsCrop.epz[indice]=0;
							sticsCrop.epz[indice] = (float) reportWaterCrop;		//il 05.07.2017 je pense c'est plus cohérent				
							this.addWaterExtractedInSaturationByCrops(reportWaterCrop);
							reportWaterCrop = 0;
	
						} else {*/
							if((sticsCommun.HUR[z]
								-sticsCommun.esz[z]
								-cropWaterUptake/cellArea/miniCoucheNumber
								-reportWaterCrop) < ha) {
					
								sticsCrop.epz[indice] = (float) Math.max((sticsCommun.HUR[z]-sticsCommun.esz[z]-ha),0);
								
								reportWaterCrop += (cropWaterUptake/cellArea/miniCoucheNumber) 
											   - sticsCrop.epz[indice];

	
							} else {
								sticsCrop.epz[indice] 	= (float) ((cropWaterUptake/cellArea/miniCoucheNumber) 		//convert liters in mm
																+reportWaterCrop);
								reportWaterCrop = 0;
							}
					//	}
	
					    
						
						//Passage dans STICS de l'extraction en AZOTE des arbres et de la culture dans les minicouches 
					  				
						if ( (sticsSoil.nit[z+1] > 0 ) && (sticsSoil.amm[z] >= 0)) {	
							if  (treeNitrogenUptake > 0) {	
								sticsCommun.treeNitrogenUptake [z] = (float) ((treeNitrogenUptake  / cellArea) * 10)
															 *(sticsSoil.nit[z+1]+sticsSoil.amm[z])/nitrogenTotal;	

							}
	
							if  (cropNitrogenUptake > 0) {	
								//IL 16/02/2017 On enleve ce test pour autoriser l'extraction dans TOUTES les mini-couches du voxel
								//if (z<=cropRootDepth) {	
									sticsCommun.absz[z] = (float) ((cropNitrogenUptake  / cellArea) * 10)
											*(sticsSoil.nit[z+1]+sticsSoil.amm[z])/cropNitrogenTotal;	
								//}
							}		
					   }
					}			
				}	
			}	//if (!voxels[i].getIsSaturated()) 
		}	//FOR EACH VOXEL
		
		//Si après exploration de tous les voxels il reste des choses à extraire 
		//Qu'est ce qu'on fait ???? 
	
			if (reportWaterTree > 0.000001)  System.out.println("cell="+this.getId()+"  reportWaterTree="+reportWaterTree);
			if (reportWaterCrop > 0.000001) System.out.println("cell="+this.getId()+"  reportWaterCrop="+reportWaterCrop);
		
		
	}
	
	/**
	* Desagregation of HISAFE voxels in STICS miniCouches (water and nitrogen content) 
	* Called after water table calculation  
	*/
	public void voxelsToMinicouchesWaterNitrogen (SafeGeneralParameters safeSettings) {

		double cellArea = this.getArea();
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;

		//FOR EACH SATURATED VOXEL
		for (int i=0; i < this.voxels.length; i++) {

			//ATTENTION, ce test sera a revoir si l'on decide de passer la teneur en eau de SAT a FC
			//en cas de descente de la nappe !!!!!!!!!!
			 if (voxels[i].getIsSaturated () == true) { 

				//number of miniCouches in this voxel
				int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
				int miniCoucheMax = voxels[i].getMiniCoucheMax();	   	//ending    miniCouches  for current voxel
				int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	//number    miniCouches  for current voxel
		
				//Grab voxel values
				double theta = voxels[i].getTheta ();				//%
				double qnO3 = voxels[i].getNitrogenNo3Stock ();		//g/voxel
				double qnH4 = voxels[i].getNitrogenNh4Stock ();		//g/voxel
	
				for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
					sticsCommun.HUR[z] 	= (float) (theta * 10);							// soil humidity 
					sticsSoil.nit[z+1] = (float) (qnO3*10/cellArea/miniCoucheNumber);	// from g to kg.ha-1.cm-1 (/cellArea/1000*10000/(voxelThickness*100))
					sticsSoil.amm[z] = (float) (qnH4*10/cellArea/miniCoucheNumber);		// from g to kg.ha-1	

				}
			}
		}
	}

	
	/**
	* Reduction of waterStock with soil evaporation before water repartition
	* gt - 5.02.2009
	*/
	public void computeEvaporation () {
		
		SafeVoxel[] voxels = this.getVoxels();
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		
		// for each voxel of the cell
		for (int i = 0; i < voxels.length; i++) {				
			
			//number of miniCouches in this voxel
			int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
			int miniCoucheMax = voxels[i].getMiniCoucheMax();	   	//ending    miniCouches  for current voxel
			Double soilEvapo = 0.0;
			for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
				soilEvapo  		+= sticsCommun.esz[z];				// voxel soil evaporation mm	
			}	
			
			if (soilEvapo != 0) {
				voxels[i].setEvaporation (soilEvapo);
				voxels[i].reduceWaterStock(soilEvapo*this.getArea());
			}
		}
	}	
	
	/**
	* Recalculation of crop root topology
	* After STICS PART 1 (crop root growth) 
	*/
	public void computeCropRootsTopology (int simulationDay) {
	
		//FOR EACH VOXEL
		for (int i = 0; i < this.voxels.length; i++) {
	
			double cropRootDensity = voxels[i].getCropRootsDensity();
			if (cropRootDensity > 0) {
				
				//ADD NEW TOPOLOGY NODE
				if (! this.getCrop().getPlantRoots().getRootTopology ().containsKey(voxels[i])) {
					if (i > 0)
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], voxels[i-1], simulationDay, cropRootDensity);
					else
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], null, simulationDay, cropRootDensity);
				}
		
				//UPDATE fine root density in the TOPOLOGY NODE
				if (this.getCrop().getPlantRoots().getRootTopology ().containsKey(voxels[i]))
					this.getCrop().getPlantRoots().setFineRootTopology (voxels[i], cropRootDensity);
			}

		}

	}
	

    /**
	 * Check if the cell is colonized by tree roots (at least one voxel) 
	 */
    public boolean isColonised (int treeID)  {
    	boolean isColonised = false; 		
		SafeVoxel[] voxels = this.getVoxels();

		int nbVoxels = voxels.length;
		for (int i=0; (i<nbVoxels && (!isColonised))  ; i++) {
			if (voxels[i].getTheTreeRootsDensity(treeID-1) > 0) {			
				isColonised = true; 
			}
		}
		return isColonised;
    }
	

	/**
	 * Daily calculation of PAR incident on the cell depending climatic entries
	 */
	public void updateDailyLightResults(SafeBeamSet<SafeBeam> beamSet, SafeDailyClimat dayClimat, SafeGeneralParameters settings) {

		//crop light interception method 0=STICS 
		if(!settings.cropLightMethod){
			getCrop().cropSticsLightInterception(settings, beamSet, getRelativeToFlatCellDirectParIncident(), getRelativeToFlatCellDiffuseParIncident());
		}

		//Calculation of diffuse PAR transmitted today on this cell in Moles m-2 d-1
		float dailyDiffuse = dayClimat.getDiffusePar ();				  					// Climatic entry of the day in Moles m-2 d-1
		setDiffuseParIncident(dailyDiffuse * getRelativeToFlatCellDiffuseParIncident())  ;  // Moles m-2 d-1

		
		//Same in relative (%)
		float skyDiffuseMask =(float)(beamSet.getSkyDiffuseMask());

		if ((getDiffuseParIncident() > 0) && (skyDiffuseMask >0) ) {
			setRelativeDiffuseParIncident (getRelativeToFlatCellDiffuseParIncident()/skyDiffuseMask);		// %
			if (getRelativeDiffuseParIncident() > 1)
				setRelativeDiffuseParIncident(1); //to avoid rounding errors
		}
		else setRelativeDiffuseParIncident(0);

		//Calculation of direct PAR transmitted today on this cell in Moles m-2 d-1
		double dailyDirect = dayClimat.getDirectPar ();												//Climatic entry of the day in Moles m-2 d-1 //GT 2007
		double dayDirectTransmitted = dailyDirect * this.getRelativeToFlatCellDirectParIncident();	//Moles m-2 d-1
		setDirectParIncident (dayDirectTransmitted);												//Moles m-2 d-1

			
		//Same in relative (%)
		double skyDirectMask = beamSet.getSkyDirectMask();

		if (( dayDirectTransmitted> 0) && (skyDirectMask > 0)) {
			setRelativeDirectParIncident(this.getRelativeToFlatCellDirectParIncident()/skyDirectMask); // % of daily direct radiation reaching the scene
			if (getRelativeDirectParIncident() > 1) {setRelativeDirectParIncident (1);}	//to avoid rounding errors
		}
		else setRelativeDirectParIncident(0);


		//total is direct + diffuse
		double totalParIncident = getDirectParIncident() + getDiffuseParIncident();

		//Same in relative (%)
		if (dayClimat.getGlobalPar () > 0) {
			setRelativeTotalParIncident (totalParIncident /((dailyDirect*skyDirectMask) + (dailyDiffuse*skyDiffuseMask)));
			if (getRelativeTotalParIncident() > 1) setRelativeTotalParIncident (1);	//to avoid rounding errors
		}
		else setRelativeTotalParIncident(0);


		//Compute relative visibleSky
		setVisibleSky(getRelativeToFlatCellVisibleSky()/((float) (beamSet.getSkyInfraRedMask())));

		//Compute relative Global Radiation Incident
		double parProp = settings.parGlobalCoefficient;
		double directProp = dailyDirect/(dailyDirect+dailyDiffuse);
		setRelativeGlobalRadIncident(
			(getRelativeDiffuseParIncident()*parProp + getRelativeToFlatCellDiffuseNirIncident()/skyDiffuseMask*(1-parProp))
			*(1-directProp)
			+(getRelativeDirectParIncident()*parProp + getRelativeToFlatCellDirectNirIncident()/skyDirectMask*(1-parProp))
			*(directProp)
			);

		
		//month cumul 
		monthDirectPar += dailyDirect;					
		monthDiffusePar += dailyDiffuse;					
		monthDirectParIncident += directParIncident;	
		monthDiffuseParIncident += diffuseParIncident;
	}


	/**
	 * Find shading cells from shading mask 
	 *  used to compute light interception by crops if cropLightMethod=1
	 */
	public void findShadingCells (SafeBeamSet<SafeBeam> beamSet, SafePlot plot){

		for(Iterator t1 = beamSet.getBeams().iterator(); t1.hasNext(); ){
			SafeBeam beam = (SafeBeam) t1.next();

			for(Iterator t2 = beam.getShadingMasks().iterator(); t2.hasNext(); ){
				SafeShadingMask mask = (SafeShadingMask) t2.next();

				for(Iterator t3 = mask.getShadingNeighbours().iterator(); t3.hasNext(); ){
					SafeShadingNeighbour neighbour = (SafeShadingNeighbour) t3.next();

					int iGrid = this.getIGrid();
					int jGrid = this.getJGrid();

					int nLin = plot.getNLin();
					int nCol = plot.getNCol();

					int iDec = neighbour.getRelCoordinates().x;
					int jDec = neighbour.getRelCoordinates().y;

					neighbour.setCell((SafeCell) plot.getCell (iGrid+iDec, jGrid+jDec));		// uses modulo


					double xShift = 0;
					double yShift = 0;
					double zShift = 0;

					if (iGrid+iDec < 0) {
						int dep = Math.abs (iGrid+iDec) - 1;	// overflow
						int n = dep/nLin;							// integer division
						yShift = (n+1) * plot.getYSize ();
					}

					if (iGrid+iDec > nLin-1) {
						int dep = Math.abs ((nLin-1)-iGrid-iDec) - 1;	// overflow
						int n = dep/nLin;							// integer division
						yShift = - (n+1) * plot.getYSize ();
					}

					if (jGrid+jDec < 0) {
						int dep = Math.abs (jGrid+jDec) - 1;	// overflow
						int n = dep/nCol;							// integer division
						xShift = - (n+1) * plot.getXSize ();
					}

					if (jGrid+jDec > nCol-1) {
						int dep = Math.abs ((nCol-1)-jGrid-jDec) - 1;	// overflow
						int n = dep/nCol;							// integer division
						 xShift = (n+1) * plot.getXSize ();
					}

					neighbour.setShift(new ShiftItem (xShift, yShift, zShift));
				} //end of neighbours
			} //end of masks
		} //end of beams
	}
	
	/**
	 * TREE irrigation : SET dose in the crop to be treated by STICS
	 * method = 1 (drip) water is injected in second voxel 
	 * method = 2,3 (aspersion flood) water is on the surface
	 **/
	public void treeIrrigation (int n, int method, double irrigationDose) {
		
		SafeSticsItk cropItk = this.getCropZone().sticsItk;
		SafeSticsCommun sc = this.getCrop().sticsCommun;
		
		if (method==1) {
			cropItk.P_codlocirrigTree = 3;
			cropItk.P_locirrigTree = 1;
		}
		if (method==2) {
			cropItk.P_codlocirrigTree = 1;
			cropItk.P_locirrigTree = 0;
		}
		if (method==3) {
			cropItk.P_codlocirrigTree = 1;
			cropItk.P_locirrigTree = 0;
		}		
		
		//add a new irrigation date
		if (sc.airg[n]==0) {
			sc.airg[n] = (float) irrigationDose;
			//irrigation number reset
			cropItk.nap = 0;			
			for (int i=0; i<366; i++) {
				if (sc.airg[i]>0) cropItk.nap +=  1;
			}
		} 
		//irrigation date already exists (update the dose) 
		else 
			sc.airg[n] += (float) irrigationDose;
	
	}
	
	/**
	 * TREE fertilization : SET dose in the crop to be treated by STICS
	 **/
	public void treeFertilization(int n, double fertilizationDose, int fertilizerCode) {

		SafeSticsItk cropItk = this.getCropZone().sticsItk;
		SafeSticsCommun sc = this.getCrop().sticsCommun;
		
		//add a new fertilization date
		if (sc.anit[n]==0) {
			sc.anit[n] = (float) fertilizationDose;
			sc.type_ferti[n] = fertilizerCode;
			//fertilization number reset
			cropItk.napN = 0;
			for (int i=0; i<366; i++) {
				if (sc.anit[i]>0) cropItk.napN +=  1;
			}
		}
		//fertilization date already exists (update the dose) 
		else 
			sc.anit[n] += (float) fertilizationDose;
			
	}
	
	/**
	 * Return the first tree planted on this cell
	 **/
	public SafeTree getTree () {
		if (trees != null) return (SafeTree) trees.get(0);
		else return (null);
	}
	
	/**
	 * Set the crop reference on this cell
	 **/
	public void setCrop (SafeCrop crop) {
		this.crop  = crop;
	}

	/**
	 * Return the crop object sown on this cell
	 **/
	public SafeCrop getCrop () {return crop ;}
	/**
	 * Return the crop name sown on this cell
	 **/
	public String getCropName () {return this.getCropZone().getCropSpecies().getName();}

	/**
	 * Set  the crop zone object 
	 **/
	public void setCropZone (SafeCropZone z) {this.cropZone = z;}
	/**
	 * Return the crop zone object 
	 **/
	public SafeCropZone getCropZone () {return cropZone;}
	public int getIdZone() {return cropZone.getId();}
	public String getZoneName () {return cropZone.getName();}
	/**
	 * Add a voxel on the voxel collection for this cell
	 **/
	public void addVoxel (int i, SafeVoxel voxel) {voxels[i]=voxel;}
	/**
	 * Return the voxel collection for this cell
	 **/
	public SafeVoxel[] getVoxels  () {return voxels;}

	public SafeVoxel getFirstVoxel  () {return voxels[0];}
	
	protected void setCellIdRight (int id) {
		immutable2.cellIdRight = id;
	}
	protected int getCellIdRight () {
		return immutable2.cellIdRight;
	}
	protected void setCellIdLeft (int id) {
		immutable2.cellIdLeft = id;
	}
	protected int getCellIdLeft () {
		return immutable2.cellIdLeft;
	}
	protected void setCellIdBack (int id) {
		immutable2.cellIdBack = id;
	}
	protected int getCellIdBack () {
		return immutable2.cellIdBack;
	}
	protected void setCellIdFront (int id) {
		immutable2.cellIdFront = id;
	}
	protected int getCellIdFront () {
		return immutable2.cellIdFront;
	}

	public boolean isTreeAbove() {return isTreeAbove;}
	public void setIsTreeAbove (boolean b) {isTreeAbove = b;}
	
	public int getIdTreePlanted() {return idTreePlanted;}
	public void setIdTreePlanted (int i) {idTreePlanted = i;}
	
	
	public void razTreeAbove() {treeAbove = new TreeSet<SafeTree> (new SafeTreeHeightComparator());}
	public void addTreeAbove (SafeTree t) {treeAbove.add(t);}
	public Collection<SafeTree> getTreeAbove () {return treeAbove;}
	public int getNbrTreeAbove() {return treeAbove.size();}
	
	public void setLaiTree (double v) {laiTree = (float) v;}
	public void addLaiTree (double v) {laiTree += (float) v;}
	public double getLaiTree () {return (double) laiTree;}

	public void setRainInterceptedByTrees (double v) {rainInterceptedByTrees  =   (float) v;}
	public void addRainInterceptedByTrees (double v) {rainInterceptedByTrees  +=   (float) v;}
	public double getRainInterceptedByTrees () {return (double) rainInterceptedByTrees;}
	public void setRainTransmittedByTrees (double v) {rainTransmittedByTrees  =   (float) v;}
	public double getRainTransmittedByTrees () {return (double) rainTransmittedByTrees;}
	public void setStemFlowByTrees (double v) {stemFlowByTrees  =   (float) v;}
	public double getStemFlowByTrees () {return  (double) stemFlowByTrees;}
	public double getRainInterceptedByCrop () {return  (double) this.getCrop().sticsCrop.interpluie[1];}
	public double getStemFlowByCrop () {return  (double) this.getCrop().sticsCrop.stemflow;}
	public double getRainTransmittedByCrop () {return getRainTransmittedByTrees() - getRainInterceptedByCrop () + getStemFlowByCrop ();}
	
	public void setEtpCalculated (double e) {etpCalculated =  (float) e;}
	public double getEtpCalculated () {return  (double) etpCalculated;}

	public float getNitrogenRunOff () {
		if (rainTransmittedByTrees > 0) 
			return (getCrop().getNitrogenRain() * (getCrop().getRunOff()/rainTransmittedByTrees));
		else return 0;
	}
	
	public void setWaterAddedByWaterTable(double v) {waterAddedByWaterTable = (float) v;}
	public void addWaterAddedByWaterTable (double v) {waterAddedByWaterTable += (float) v;}
	public double getWaterAddedByWaterTable () {return (double) waterAddedByWaterTable;}
	public void addDrainageWaterTable (double v) {this.getCrop().sticsCommun.drain += (float) v;}

	
	public void setWaterTakenByDesaturation (double v) {waterTakenByDesaturation = (float) v;}
	public void addWaterTakenByDesaturation(double v) {waterTakenByDesaturation += (float) v;}
	public double getWaterTakenByDesaturation () {return (double) waterTakenByDesaturation;}


	public void setWaterUptakeInSaturationByTrees (double v) {waterUptakeInSaturationByTrees = (float) v;}
	public void addWaterUptakeInSaturationByTrees (double v) {waterUptakeInSaturationByTrees += (float) v;}
	public double getWaterUptakeInSaturationByTrees () {return (double) waterUptakeInSaturationByTrees;}
	public void setWaterUptakeInSaturationByCrop (double v) {waterUptakeInSaturationByCrop = (float) v;}
	public void addWaterUptakeInSaturationByCrop (double v) {waterUptakeInSaturationByCrop += (float) v;}
	public double getWaterUptakeInSaturationByCrop () {return (double) waterUptakeInSaturationByCrop;}


	
	//aq - 10.06.2011 - Same for Nitrogen
	public void setNitrogenUptakeInSaturationByTrees (double v) {nitrogenUptakeInSaturationByTrees = (float) v;}
	public void addNitrogenUptakeInSaturationByTrees (double v) {nitrogenUptakeInSaturationByTrees += (float) v;}
	public double getNitrogenUptakeInSaturationByTrees () {return (double) nitrogenUptakeInSaturationByTrees;}
	public void setNitrogenUptakeInSaturationByCrop (double v) {nitrogenUptakeInSaturationByCrop = (float) v;}
	public void addNitrogenUptakeInSaturationByCrop (double v) {nitrogenUptakeInSaturationByCrop += (float) v;}
	public double getNitrogenUptakeInSaturationByCrop () {return (double) nitrogenUptakeInSaturationByCrop;}

	
	public void setTreeCarbonFoliageLitter(double v) {treeCarbonFoliageLitter =  v;}
	public void addTreeCarbonFoliageLitter (double v) {treeCarbonFoliageLitter +=  v;}
	public double getTreeCarbonFoliageLitter () {return  treeCarbonFoliageLitter;}
	
	public void setTreeCarbonBranchesLitter(double v) {treeCarbonBranchesLitter =  v;}
	public void addTreeCarbonBranchesLitter (double v) {treeCarbonBranchesLitter +=  v;}
	public double getTreeCarbonBranchesLitter () {return  treeCarbonBranchesLitter;}
	
	public void setTreeCarbonFruitLitter(double v) {treeCarbonFruitLitter =  v;}
	public void addTreeCarbonFruitLitter (double v) {treeCarbonFruitLitter +=  v;}
	public double getTreeCarbonFruitLitter () {return  treeCarbonFruitLitter;}

	public void setTreeNitrogenFoliageLitter(double v) {treeNitrogenFoliageLitter =  v;}
	public void addTreeNitrogenFoliageLitter(double v) {treeNitrogenFoliageLitter +=  v;}
	public double getTreeNitrogenFoliageLitter () {return treeNitrogenFoliageLitter;}
	
	
	public void setTreeNitrogenBranchesLitter(double v) {treeNitrogenBranchesLitter = v;}
	public void addTreeNitrogenBranchesLitter(double v) {treeNitrogenBranchesLitter +=  v;}
	public double getTreeNitrogenBranchesLitter () {return  treeNitrogenBranchesLitter;}
	
	public void setTreeNitrogenFruitLitter(double v) {treeNitrogenFruitLitter =  v;}
	public void addTreeNitrogenFruitLitter(double v) {treeNitrogenFruitLitter +=  v;}
	public double getTreeNitrogenFruitLitter () {return  treeNitrogenFruitLitter;}


	//ROOT LITTER---------------------------------------------------------
	public double getTreeCarbonFineRootsLitter (){return treeCarbonFineRootsLitter;}
	public double getTreeNitrogenFineRootsLitter(){return treeNitrogenFineRootsLitter;}
	public double getTreeCarbonCoarseRootsLitter (){return treeCarbonCoarseRootsLitter;}
	public double getTreeNitrogenCoarseRootsLitter(){return treeNitrogenCoarseRootsLitter;}
	public void setTreeCarbonFineRootsLitter (double v){treeCarbonFineRootsLitter = v;}
	public void setTreeNitrogenFineRootsLitter(double v){treeNitrogenFineRootsLitter = v;}
	public void setTreeCarbonCoarseRootsLitter (double v){treeCarbonCoarseRootsLitter = v;}
	public void setTreeNitrogenCoarseRootsLitter(double v){treeNitrogenCoarseRootsLitter = v;}
	public void addTreeCarbonFineRootsLitter (double v){treeCarbonFineRootsLitter += v;}
	public void addTreeNitrogenFineRootsLitter(double v){treeNitrogenFineRootsLitter += v;}
	public void addTreeCarbonCoarseRootsLitter (double v){treeCarbonCoarseRootsLitter += v;}
	public void addTreeNitrogenCoarseRootsLitter(double v){treeNitrogenCoarseRootsLitter += v;}

	
	
	public double getNitrogenUptakeByTrees () {
		double nitrogenUptakeByTrees = 0;
		SafeVoxel[] voxels = this.getVoxels();
		for (int i=0; (i<voxels.length)  ; i++) {
			nitrogenUptakeByTrees += voxels[i].getTotalTreeNitrogenUptake(); 
		}
		return nitrogenUptakeByTrees / this.getArea() * 10; // from g to Kg.ha-1
	}
	
	public double getNitrogenUptakeByCrop () {

		return this.getCrop().getNitrogenUptake();
	}
	
	public double getMineralNitrogenStock() {
		double temp = 0;
		SafeVoxel[] voxel = this.getVoxels();
		for (int i = 0; i < voxel.length; i++) {
			temp += (voxel[i].getNitrogenNo3Stock() + voxel[i].getNitrogenNh4Stock());
		}
		return temp / this.getArea() * 10; // from g to Kg.ha-1
	}
	
	/****************************************************
	MANAGEMENT OF THE RESULTS OF LIGHT COMPETITION MODULE
	*****************************************************/

	public double getRelativeToFlatCellDirectParIncident () {return (double) relativeToFlatCellDirectParIncident;}
	public double getRelativeToFlatCellDiffuseParIncident () {return (double) relativeToFlatCellDiffuseParIncident;}
	public double getRelativeToFlatCellVisibleSky () {return (double) relativeToFlatCellVisibleSky;}
	public double getRelativeToFlatCellDirectNirIncident () {return (double) relativeToFlatCellDirectNirIncident;}
	public double getRelativeToFlatCellDiffuseNirIncident () {return (double) relativeToFlatCellDiffuseNirIncident;}

	public double getDirectParIncident () {return (double) directParIncident;}
	public double getDiffuseParIncident () {return (double) diffuseParIncident;}
	public double getTotalParIncident () {return (double) directParIncident+diffuseParIncident;}
	public double getRelativeDirectParIncident () {return (double) relativeDirectParIncident;}
	public double getRelativeDiffuseParIncident () {return (double) relativeDiffuseParIncident;}
	public double getRelativeTotalParIncident () {return (double) relativeTotalParIncident;}
	public double getRelativeGlobalRadIncident () {return (double) relativeGlobalRadIncident;}
	public double getVisibleSky () {return visibleSky;}

	public void setRelativeToFlatCellDirectParIncident (double e) {relativeToFlatCellDirectParIncident= (float) e;}
	public void setRelativeToFlatCellDiffuseParIncident (double e) {relativeToFlatCellDiffuseParIncident= (float) e;}
	public void setRelativeToFlatCellVisibleSky (double e) {relativeToFlatCellVisibleSky= (float) e;}
	public void setRelativeToFlatCellDirectNirIncident (double e) {relativeToFlatCellDirectNirIncident= (float) e;}
	public void setRelativeToFlatCellDiffuseNirIncident (double e) {relativeToFlatCellDiffuseNirIncident= (float) e;}

	public void setDirectParIncident (double e) {directParIncident= (float) e;}
	public void setDiffuseParIncident (double e) {diffuseParIncident=(float) e;}
	public void setRelativeDirectParIncident (double e) {relativeDirectParIncident=(float) e;}
	public void setRelativeDiffuseParIncident (double e) {relativeDiffuseParIncident=(float) e;}
	public void setRelativeTotalParIncident (double e) {relativeTotalParIncident=(float) e;}
	public void setRelativeGlobalRadIncident (double e) {relativeGlobalRadIncident=(float) e;}
	public void setVisibleSky (double e) {visibleSky= (float)e;}

	// add functions
	public void addDirectPar (double energy){this.relativeToFlatCellDirectParIncident += (float) energy;}
	public void addDiffusePar (double energy){this.relativeToFlatCellDiffuseParIncident += (float) energy;}
	public void addVisibleSky (double energy){this.relativeToFlatCellVisibleSky += (float) energy;}
	public void addDirectNir (double energy){this.relativeToFlatCellDirectNirIncident += (float) energy;}
	public void addDiffuseNir (double energy){this.relativeToFlatCellDiffuseNirIncident += (float) energy;}

	//Monthly values for export

	public float getMonthDirectPar() { return monthDirectPar;}
	public float getMonthDiffusePar () {return monthDiffusePar;}
	
	public float getMonthDirectParIncident () { 
		if (monthNbrDays > 0) return monthDirectParIncident/monthNbrDays;
		else return 0;
	}
	public float getMonthDiffuseParIncident () {
		if (monthNbrDays > 0)   return monthDiffuseParIncident/monthNbrDays;
		else return 0;
	}
	public float getMonthRelativeDirectParIncident () {
		if (monthDirectPar > 0) 
			return monthDirectParIncident/monthDirectPar;
		else return 0;
	}
	public float getMonthRelativeDiffuseParIncident () {
		if (monthDiffusePar > 0)   
			return monthDiffuseParIncident/monthDiffusePar;
		else return 0;
	}
	public float getMonthRelativeTotalParIncident () {
		if (monthDirectPar+monthDiffusePar > 0)   return (monthDirectParIncident + monthDiffuseParIncident)/(monthDirectPar+monthDiffusePar);
		else return 0;
	}
	public float getMonthVisibleSky () {
		if (monthNbrDays > 0)   return monthVisibleSky/monthNbrDays;
		else return 0;
	}	
	


	
	public String getCropSpeciesName () {
		if (this.getCropZone() == null) return "";
		if (this.getCropZone().getCropSpecies() == null) return "";
		return this.getCropZone().getCropSpecies().getName();
	}
	

	
	public double getWaterUptakeByTrees () {
		double waterUptakeByTrees = 0;
		SafeVoxel[] voxels = this.getVoxels();
		for (int i=0; (i<voxels.length)  ; i++) {
			waterUptakeByTrees += voxels[i].getTotalTreeWaterUptake(); //liters
		}
		return waterUptakeByTrees / this.getArea();		//convert liter to mm
	}

	public double getWaterUptakeByCrop () {

		return this.getCrop().getWaterUptake();		//mm
	}
	
	public double getTreeFineRootsLength() {
		double length = 0;
		for (int i = 0; i < voxels.length; i++) {
			length += voxels[i].getTotalTreeRootsDensity() * voxels[i].getVolume(); //m m-3 * m3
		}
		return length;	//m
	}
	
	public double getTreeCarbonCoarseRoots() {
		double carbon = 0;
		for (int i = 0; i < voxels.length; i++) {
			carbon += voxels[i].getTotalTreeCarbonCoarseRoots(); // kg C 
		}
		return carbon / (this.getArea() / 10000); //kg ha-1
	}


	//for export
	public int getIdCell() {return getId();}
	
	// Methods for exportation about WATER BUDGET
	public double getWaterStock() {
		double waterStock = 0;

		SafeVoxel[] voxel = this.getVoxels();

		for (int i = 0; i < voxel.length; i++) {
			waterStock += Math
					.max(voxel[i].getWaterStock(),
							0);
		}
		
		return waterStock / this.getArea();
	}
	
	
	public double getNitrogenNo3Stock() {
		double nitrogenStock = 0;


		SafeVoxel[] voxel = this.getVoxels();

		for (int i = 0; i < voxel.length; i++) {
			nitrogenStock += Math
					.max(voxel[i].getNitrogenNo3Stock(),
							0);
		}
		
		return nitrogenStock / this.getArea();
	}
	
	public double getNitrogenNh4Stock() {
		double nitrogenStock = 0;


		SafeVoxel[] voxel = this.getVoxels();

		for (int i = 0; i < voxel.length; i++) {
			nitrogenStock += Math
					.max(voxel[i].getNitrogenNh4Stock(),
							0);
		}
		
		return nitrogenStock / this.getArea();
	}
	
	
	//to export tree deep root residus
	public double getCumulatedTreeNitrogenShallowRootsSen () {
		double nitrogenRootResidu=0;
		
		SafeVoxel[] voxel = this.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				if (voxel[i].getZ()*100 <= this.getCrop().sticsSoil.P_profhum) 
					nitrogenRootResidu += voxel[i].getCumulatedTreeNitrogenRootsSen();
			}
			   
			return (nitrogenRootResidu / (this.getArea() / 10000));		 //convert kg in kg ha-1 
	}	
	//to export tree deep root residus 
	public double getCumulatedTreeNitrogenDeepRootsSen () {
		double nitrogenRootResidu=0;
		
		SafeVoxel[] voxel = this.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				if (voxel[i].getZ()*100 > this.getCrop().sticsSoil.P_profhum) 
					nitrogenRootResidu += voxel[i].getCumulatedTreeNitrogenRootsSen();
			}
			   
		return (nitrogenRootResidu / (this.getArea() / 10000));		 //convert kg in kg ha-1 
	}
	
	//Total for export
		public float getAnnualWaterUptakeByTrees() { return annualWaterUptakeByTrees;}
		public float getAnnualNitrogenUptakeByTrees() { return annualNitrogenUptakeByTrees;}
		public float getAnnualParIncident() { return annualParIncident;}
		
		public String toString(){
			String str = "id = "+this.getId();
			if (this.getCropZone()!=null)
				str = str+" cropZone ="+this.getCropZone().getId();
			if (this.getCrop()!=null)
				str = str+" crop ="+this.getCrop().getCropSpeciesName();
			return str;
		}
}
