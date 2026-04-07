/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Gr�goire Talbot 1, Nicolas Barbault 1, 
 * Gr�goire Vincent 2 , Rachmat Mulia 3, Fran�ois Bussi�re 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Herv� Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Universit� Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d��tude de la for�t, Universit� du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d�H�res, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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
import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;

import capsis.defaulttype.ShiftItem;
import capsis.defaulttype.plotofcells.SquareCell;
import jeeb.lib.util.Vertex3d;
import safe.stics.SafeSticsCommun;
import safe.stics.SafeSticsCrop;
import safe.stics.SafeSticsItk;
import safe.stics.SafeSticsParameters;
import safe.stics.SafeSticsSoil;

/**
 * CELL is a square spatial division of a SafePlot  
 *
 * @author : Isabelle Lecomte - INRA (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeCell extends SquareCell {

	/** 4 neighbor cells used for toric symmetry */
	private static class Immutable2 implements Cloneable, Serializable {
		/**  ID of the right neighbor cell x+ */
		public int cellIdRight;		
		/**  ID of the left neighbor cell x- */
		public int cellIdLeft;				
		/**  ID of the back neighbor cell y+ */
		public int cellIdBack;					
		/**  ID of the front neighbor cell y- */
		public int cellIdFront;				
	}
	protected Immutable2 immutable2;
	
	/** Reference of the crop zone object  */
	private SafeCropZone cropZone;			
	/** Reference of the crop object  */
	private SafeCrop crop;						
	/** References of soil voxels objects attached to this cell  */
	private SafeVoxel [] voxels;				
	/** ID of tree if planted in this cell   */
	private int idTreePlanted;					
	/** True if at least one tree crown is above the cell    */
	private boolean isTreeAbove;				
	/** List of tree crown above this cell  */
	Collection<SafeTree> treeAbove;				
	/** Sum of lai of all trees above */
	private float  laiTree;						
	/** ETP calculated on the cell   */
	private float etpCalculated;
	
	//LIGHT 
	private float relativeToFlatCellDirectParIncident;	// %
	private float relativeToFlatCellDiffuseParIncident;	// %
	private float relativeToFlatCellVisibleSky;			// %
	private float relativeToFlatCellDirectNirIncident;	// %
	private float relativeToFlatCellDiffuseNirIncident;	// %

	/** Direct PAR incident on the cell (moles PAR m-2)  */
	private float directParIncident;				
	/** Diffuse PAR incident on the cell (moles PAR m-2)  */
	private float diffuseParIncident;	
	/** Relative direct PAR incident on the cell (%)  */
	private float relativeDirectParIncident;	
	/** Relative diffuse PAR incident on the cell (%)  */
	private float relativeDiffuseParIncident;		
	/** Relative total PAR incident on the cell (%)  */
	private float relativeTotalParIncident;	
	/** Relative global PAR incident on the cell (%) */
	private float relativeGlobalRadIncident;
	/** Visible sky from the cell (%) */
	private float visibleSky;
	/** Total month direct PAR  on this cell (moles PAR m-2)   */
	private float monthDirectPar;				
	/** Total month diffuse PAR  on this cell (moles PAR m-2)   */
	private float monthDiffusePar;				
	/** Total month direct PAR incident on this cell (moles PAR m-2)   */
	private float monthDirectParIncident;		
	/** Total month diffuse PAR incident on this cell (moles PAR m-2)   */
	private float monthDiffuseParIncident;			
	/** Total month visible sky on this cell (%)   */
	private float monthVisibleSky;								
	/** Total annual direct PAR incident on this cell (moles PAR m-2)   */
	private float annualDirectParIncident;		
	/** Total annual diffuse PAR incident on this cell (moles PAR m-2)   */
	private float annualDiffuseParIncident;				
	
	//WATER ENTRIES 
	/** Rain intercepted by trees on this cell (mm d-1)   */
	private float rainInterceptedByTrees; 			
	/** Rain transmitted by trees on this cell (mm d-1) (mm d-1)   */
	private float rainTransmittedByTrees; 			
	/** Stemflow by trees on this cell  (mm d-1)   */
	private float stemFlowByTrees; 				

	//WATER AND NITROGEN UPTAKE 
	/** Total annual water uptake by crop (mm)   */
	private float annualWaterUptakeByCrop;		
	/** Total annual nitrogen uptake by crop (kg N ha-1)   */
	private float annualNitrogenUptakeByCrop;	
	/** Total annual water uptake by trees (mm)   */
	private float annualWaterUptakeByTrees;		
	/** Total annual nitrogen uptake by trees (kg N ha-1)   */
	private float annualNitrogenUptakeByTrees;		
	
	//WATER TABLE IN AND OUT
	/** Amount of water provided by the water table flooding  (mm)   */
	private float waterAddedByWaterTable;			
	/** Amount of water taken by the water table flow back (mm)   */
	private float waterTakenByDesaturation;				
	/** Amount of water uptake  by the trees in saturated voxels (mm)   */
	private float waterUptakeInSaturationByTrees;	
	/** Amount of water uptake  by the crop in saturated voxels (mm)   */
	private float waterUptakeInSaturationByCrop;	
	/** Amount of nitrogen uptake  by the trees in saturated voxels (kg N ha-1)   */
	private float nitrogenUptakeInSaturationByTrees;
	/** Amount of nitrogen uptake  by the crop in saturated voxels (kg N ha-1)   */
	private float nitrogenUptakeInSaturationByCrop;
	/** Amount of nitrogen provided by the water table flooding  (kg N ha-1 )   */
	private float nitrogenAddedByWaterTable;		
	/** Amount of nitrogen taken by the water table flow back (kg N ha-1 )   */
	private float nitrogenLeachingWaterTable;	
	/** Amount of annual nitrogen taken by the water table flow back (kg N ha-1 )   */
	private float annualNitrogenLeachingWaterTable; 
	
	//CARBON AND NITROGEN TREE LITTER AND RESIDUES 
	/** Carbon litter from tree foliage residues (kg C)   */
	private double 	treeCarbonFoliageLitter;		
	/** Carbon litter from tree branches residues (kg C)   */
	private double 	treeCarbonBranchesLitter;		
	/** Carbon litter from tree fruits residues (kg C)   */
	private double 	treeCarbonFruitLitter;			
	/** Carbon litter from tree fine roots residues (kg C)   */
	private double  treeCarbonFineRootsLitter;		
	/** Carbon litter from tree coarse roots residues (kg C)   */
	private double  treeCarbonCoarseRootsLitter;	
	/** Nitrogen litter from tree foliage residues (kg N)   */
	private double 	treeNitrogenFoliageLitter;		
	/** Nitrogen litter from tree branches residues (kg N)   */
	private double 	treeNitrogenBranchesLitter;		
	/** Nitrogen litter from tree fruits residues (kg N)   */
	private double 	treeNitrogenFruitLitter;		
	/** Nitrogen litter from tree fine roots residues (kg N)   */
	private double  treeNitrogenFineRootsLitter;	
	/** Nitrogen litter from tree coarse roots residues (kg N)   */
	private double  treeNitrogenCoarseRootsLitter;

	
	/**	
	 *	Constructor 
	 * @param plot the SafePlot where the cell is attached
	 * @param coord origin coordinate of the cell in 3D
	 * @param i line number in the cell grid
	 * @param j column number in the cell grid
	 * @param id id of the cell
	 * @param nbVoxels number of voxels
	 */
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

		//reset water results
		setRainTransmittedByTrees (0);
		setStemFlowByTrees (0); 
		setRainInterceptedByTrees (0); 
		setWaterAddedByWaterTable (0);
		setWaterTakenByDesaturation (0);
		setWaterUptakeInSaturationByTrees(0);	
		setWaterUptakeInSaturationByCrop(0);	
		setNitrogenUptakeInSaturationByTrees(0);	
		setNitrogenUptakeInSaturationByCrop(0);	
		setNitrogenLeachingWaterTable(0);	
		setNitrogenAddedByWaterTable(0);	

	}

	/**
	 * Create an Immutable object.
	 */
	protected void createImmutable2 () {immutable2 = new Immutable2 ();}

	/**
	 * Clone the SafeCell
	 */
	public Object clone () {

		SafeCell c = (SafeCell) super.clone ();	
		return c;
	}

	/**
	 * Reset daily results on this cell
	 */
	public void razDaily () {

		//total Monthly values
		this.monthVisibleSky=this.monthVisibleSky+this.visibleSky;	

		//Totals for EXPORT
		this.annualWaterUptakeByTrees = this.annualWaterUptakeByTrees + (float) this.getWaterUptakeByTrees();				
		this.annualNitrogenUptakeByTrees = this.annualNitrogenUptakeByTrees+ (float) this.getNitrogenUptakeByTrees();	

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
		this.setNitrogenLeachingWaterTable(0);	
		this.setNitrogenAddedByWaterTable(0);
		this.getCrop().razDaily();
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
		this.getCrop().razTotalMonth();
	}
	/**
	 * Reset annual results on this cell
	 */
	public void razTotalAnnual () {
		annualWaterUptakeByTrees = 0;				
		annualNitrogenUptakeByTrees = 0;
		annualDirectParIncident = 0; 
		annualDiffuseParIncident = 0;
		this.getCrop().razTotalAnnual();
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
	 * Aggregation of STICS mini-layers values in Hi-sAFe voxels after STICS PART 1
	 * @param sticsParam SafeSticsParameters
	 * @param waterTableDepth water table depth (m)
	 * @param simulationDay Hi-sAFe simulation day
	 * @param sticsDay STICS simulation day
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
				if (this.getCrop().getPlantRoots().getRootTopology (voxels[i]) == null) {
					if (i > 0)
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], voxels[i-1], simulationDay, cropRootDensity);
					else
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], null, simulationDay, cropRootDensity);
				}
			}
			
			//UPDATE fine root density in the TOPOLOGY MAP
			if (this.getCrop().getPlantRoots().getRootTopology (voxels[i]) != null) 
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
	 * Aggregation of STICS mini-layers values in Hi-sAFe voxels after STICS PART 2
	 * @param waterTableDepth water table depth (m)
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
	 * Aggregation of STICS mini-layers values in Hi-sAFe voxels after STICS PART 2
	*  only if water extraction have been calculated by STICS (testing STICS stand alone) 
	 * @param generalParameters Reference to SafeGeneralParameters object
	*/
	public void miniCouchesToVoxelsAfterSticsWaterExtraction (SafeGeneralParameters generalParameters) {

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
						//real      :: epz(0:2,1000) 	0=shade+sun 1=sun 2=shade (in case of associated crops) 1000=mini layess number
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

		//Set the water stress 
		double waterStress = 1;
		double nitrogenStress = 1;
		if (this.getCrop().getWaterDemand() > 0) {
			if(totalWaterUptake <= 0) {
				waterStress = generalParameters.waterStressMin;					
			}
			else {
				waterStress = Math.min (totalWaterUptake  / this.getCrop().getWaterDemand(), 1);		
			}
			waterStress = Math.max (waterStress,  generalParameters.waterStressMin);
		}
	
		crop.setHisafeWaterStomatalStress (waterStress);
		
		//Set the nitrogen stress 
		if (this.getCrop().getNitrogenDemand() > 0) {
			if(totalNitrogenUptake <= 0) {
				nitrogenStress = generalParameters.nitrogenStressMin;						
			}
			else {
				nitrogenStress = Math.min (totalNitrogenUptake  / this.getCrop().getNitrogenDemand(), 1);		
			}
			nitrogenStress = Math.max (nitrogenStress,  generalParameters.nitrogenStressMin);
		}

		crop.setHisafeNitrogenStress (nitrogenStress);
	}
	
	/**
	 * Desaggregation of Hi-sAFe voxels values in STICS mini-layers (crop and tree water and nitrogen uptake) 
	*  Called after water and nitrogen competition calculation
	*  @param generalParameters Reference to SafeGeneralParameters object
	*  @param plotSettigs Reference to SafePlotSettings object

	*/
	public void voxelsToMiniCouches (SafeGeneralParameters generalParameters, SafePlotSettings plotSettigs) {

		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;

		double cellArea = this.getArea(); 	//m2
		int cropRootDepth = (int) (this.getCrop().getRootsDepth() * 100);	//il - 05.07.2017 verif stics int(zrac)
		float ha =  sticsCommun.ha*10; // gt - 05.02.2009 - residual humidity
	
		//RAZ tree and crop water and nitrogen extraction table before desagregation
		for (int i=0; i<1000; i++) {	
			//real      :: epz(0:2,1000) 0=shade+sun 1=sun 2=shade (in case of associated crops) 1000=mini layess number
			int indice     = (i*3)+1;		
			sticsCrop.epz[indice] = 0;
			sticsCommun.absz[i]=0;
		}
		for (int i=0; i<generalParameters.STICS_MINI_LAYERS; i++) {	
			sticsCommun.treeWaterUptake [i] =0;
			sticsCommun.treeNitrogenUptake [i] =0;
		}


		//FOR EACH VOXEL
		for (int i=0; i < this.voxels.length; i++) {

			//IF voxel is SATURATED we don't extract water and nitrogen from STICS SOIL
			if (!voxels[i].getIsSaturated()) {
				//number of miniCouches in this voxel
				int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
				int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel

				//Grab voxel values for WATER and NITROGEN UPTAKE
				double voxelCropWaterUptake  = voxels[i].getCropWaterUptake ();					//liters
				double voxelCropNitrogenUptake = voxels[i].getCropNitrogenUptake ();			//g N
				double voxelTreeWaterUptake = 0;
				double voxelTreeNitrogenUptake = 0;
				for (int t=0; t < plotSettigs.nbTrees; t++) {
					voxelTreeWaterUptake += voxels[i].getTheTreeWaterUptake (t);			//liters
					voxelTreeNitrogenUptake += voxels[i].getTheTreeNitrogenUptake (t);		//g N
				}
				
				//if no extraction, no need to continue
				if ((voxelTreeWaterUptake > 0) || (voxelTreeNitrogenUptake > 0) || (voxelCropWaterUptake > 0) || (voxelCropNitrogenUptake > 0)) {
	
					//Compute water (HUR) and nitrogen total (no3+nh4) in this voxel
					float waterTotal = 0;
					float cropWaterTotal = 0;
					float nitrogenTotal = 0;
					float cropNitrogenTotal = 0;
					for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
						if (sticsCommun.HUR[z] > 0) {
							waterTotal += sticsCommun.HUR[z];
							if  (cropRootDepth >= miniCoucheMin) {
								cropWaterTotal += sticsCommun.HUR[z];		//only for crop
							}
						}
						if (sticsSoil.nit[z+1] > 0) {  //indice of nit is different (+1)
							nitrogenTotal += sticsSoil.nit[z+1];
							if  (cropRootDepth >= miniCoucheMin) {
								cropNitrogenTotal +=sticsSoil.nit[z+1];	 	//only for crop
							}
						}
						if (sticsSoil.amm[z] > 0) {
							nitrogenTotal += sticsSoil.amm[z];
							if (cropRootDepth >= miniCoucheMin)  {
								cropNitrogenTotal +=sticsSoil.amm[z];		//only for crop							
							}
						}
					}

					//Water and nitrogen are share in mini-layer in proportion of humidity or nitrogen content 
					for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
						int indice     = (z*3)+1;	
						//Water extraction in mini-layers  in proportion of humidity 			  				
						if (sticsCommun.HUR[z] > 0) {	
							if  (voxelTreeWaterUptake > 0) {	
								sticsCommun.treeWaterUptake[z] = (float) ((voxelTreeWaterUptake  / cellArea))
										 								*(sticsCommun.HUR[z]/waterTotal);
							}

							if  (voxelCropWaterUptake > 0) {	
										//Variable sticsCrop.epz(0:2,1000)
								sticsCrop.epz[indice] = (float) ((voxelCropWaterUptake  / cellArea))
										                       *(sticsCommun.HUR[z]/cropWaterTotal);	
							}	
					   }
						
						//Nitrogen extraction in mini-layers  in proportion of nitrogen content  		  				
						if ( (sticsSoil.nit[z+1] > 0 ) && (sticsSoil.amm[z] >= 0)) {	
							if  (voxelTreeNitrogenUptake > 0) {	
								sticsCommun.treeNitrogenUptake [z] = (float) ((voxelTreeNitrogenUptake  / cellArea) * 10)
															                 *(sticsSoil.nit[z+1]+sticsSoil.amm[z])/nitrogenTotal;	
							}
							if  (voxelCropNitrogenUptake > 0) {	
								sticsCommun.absz[z] = (float) ((voxelCropNitrogenUptake  / cellArea) * 10)
										                       *(sticsSoil.nit[z+1]+sticsSoil.amm[z])/cropNitrogenTotal;	
							}		
					   }
					}			
				} //if no extraction
			}	//IF voxel is SATURATED 
		}	//FOR EACH VOXEL
	}
	/**
	 * Desaggregation of hisafe voxels values in STICS mini-layers (water and nitrogen content) 
	*  Called after water table calculation 
	*/
	public void voxelsToMinicouchesWaterNitrogen () {

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
	* Recalculation of crop root topology after STICS PART 1 (crop root growth)
	* @param simulationDay Hi-sAfe simulation day  
	*/
	public void computeCropRootsTopology (int simulationDay) {
		
		//FOR EACH VOXEL
		for (int i = 0; i < this.voxels.length; i++) {
	
			double cropRootDensity = voxels[i].getCropRootsDensity();
			SafeRootNode cropNode = this.getCrop().getPlantRoots().getRootTopology (voxels[i]);
			
			if (cropRootDensity > 0) {
				
				//ADD NEW TOPOLOGY NODE
				if (this.getCrop().getPlantRoots().getRootTopology (voxels[i]) == null) {
					if (i > 0)
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], voxels[i-1], simulationDay, cropRootDensity);
					else
						this.getCrop().getPlantRoots().addCropRootTopology (voxels[i], null, simulationDay, cropRootDensity);
				}
		
				//UPDATE fine root density in the TOPOLOGY NODE
				if (this.getCrop().getPlantRoots().getRootTopology (voxels[i]) != null) 
					this.getCrop().getPlantRoots().setFineRootTopology (voxels[i], cropRootDensity);
			}
			//REMOVE TOPOLOGY NODE (in case of cuttong grass) 
			else {
				if (cropNode != null) {
					this.getCrop().getPlantRoots().getRootTopology ().remove (voxels[i]);
					SafeRootNode nodeParent  = cropNode.getNodeParent ();
					if (nodeParent!= null) nodeParent.getNodeColonised().remove (cropNode);
					
				}
			}
		}
	}
	

    /**
	 * Check if the cell is colonized by a tree roots (at least one voxel) 
	 * @param treeID ID of the tree to check   
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
	 * @param generalParameters Reference to SafeGeneralParameters general parameters 
	 * @param beamSet Reference to SafeBeamSet collection of light beam 
	 * @param dayClimat Reference to SafeDailyClimat climate of the day 

	 */
	public void updateDailyLightResults(SafeGeneralParameters generalParameters, SafeBeamSet<SafeBeam> beamSet, SafeDailyClimat dayClimat) {

		//crop light interception method 0=STICS 
		if(!generalParameters.hisafeLightMethodForCrop){
			getCrop().cropSticsLightInterception( beamSet, getRelativeToFlatCellDirectParIncident(), getRelativeToFlatCellDiffuseParIncident());
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
		double parProp = generalParameters.parGlobalCoefficient;
		double directProp = dailyDirect/(dailyDirect+dailyDiffuse);
		setRelativeGlobalRadIncident(
			(getRelativeDiffuseParIncident()*parProp + getRelativeToFlatCellDiffuseNirIncident()/skyDiffuseMask*(1-parProp))
			*(1-directProp)
			+(getRelativeDirectParIncident()*parProp + getRelativeToFlatCellDirectNirIncident()/skyDirectMask*(1-parProp))
			*(directProp)
			);

		//month total 
		monthDirectPar += dailyDirect;					
		monthDiffusePar += dailyDiffuse;	
		monthDirectParIncident += getRelativeDirectParIncident();					
		monthDiffuseParIncident += getRelativeDiffuseParIncident();
		
		//annual total
		annualDirectParIncident += getRelativeDirectParIncident();					
		annualDiffuseParIncident += getRelativeDiffuseParIncident();
	}


	/**
	 * Find shading cells from shading mask 
	 *  used to compute light interception by crops if hisafeLightMethodForCrop=1
	 * @param beamSet SafeBeamSet collection of light beam 
	 * @param plot the SafePlot
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
	 * @param sticsSimulationDay stics simulation day  
	 * @param method 1=drip (water is injected in second voxel) / 2=aspersion (on surface) / 3=flood (on surface)
	 * @param irrigationDose irrigation dose (mm) 
	 **/
	public void treeIrrigation (int method, double irrigationDose) {
		
		SafeSticsItk cropItk = this.getCropZone().sticsItk;
		SafeSticsCommun sc = this.getCrop().sticsCommun;
		
		if (method==1) {
			cropItk.P_codlocirrigTree = 1;
			cropItk.P_locirrigTree = 0;
		}
		if (method==2) {
			cropItk.P_codlocirrigTree = 1;
			cropItk.P_locirrigTree = 0;
		}
		if (method==3) {
			cropItk.P_codlocirrigTree = 1;
			cropItk.P_locirrigTree = 0;
		}		

		int n = this.getCropZone().getSticsSimulationDay() - 1;
		
		//add a new irrigation date
		if (sc.airgTree[n]==0) {
			sc.airgTree[n] = (float) irrigationDose;
	
			for (int i=0; i<366; i++) {
				if (sc.airgTree[i]>0) {
					sc.napTree +=  1;
				}
			}
		} 
		//irrigation date already exists (update the dose) 
		else 
			sc.airgTree[n] += (float) irrigationDose;
	
	}
	
	/**
	 * TREE fertilization : SET dose in the crop to be treated by STICS
	 * @param sticsSimulationDay stics simulation day  
	 * @param fertilizationDose fertilization dose (kg) 
	 * @param fertilizerCode 1=Nitrate of ammonium ,2=Solution,3=urea,4=Anhydrous ammoniac,5= Sulfate of ammonium,6=phosphate of ammonium,7=Nitrate of calcium,8= fixed efficiency
	 **/
	public void treeFertilization(int sticsSimulationDay, double fertilizationDose, int fertilizerCode) {

		SafeSticsItk cropItk = this.getCropZone().sticsItk;
		SafeSticsCommun sc = this.getCrop().sticsCommun;
		
		//add a new fertilization date
		if (sc.anit[sticsSimulationDay]==0) {
			sc.anit[sticsSimulationDay] = (float) fertilizationDose;
			sc.type_ferti[sticsSimulationDay] = fertilizerCode;
			//fertilization number reset
			cropItk.napN = 0;
			for (int i=0; i<366; i++) {
				if (sc.anit[i]>0) cropItk.napN +=  1;
			}
		}
		//fertilization date already exists (update the dose) 
		else 
			sc.anit[sticsSimulationDay] += (float) fertilizationDose;			
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
	 * 	@param crop the SafeCrop  
	 **/
	public void setCrop (SafeCrop crop) { this.crop  = crop;}
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
	 * @param zone the SafeCropZone   
	 **/
	public void setCropZone (SafeCropZone zone) {this.cropZone = zone;}
	/**
	 * Return the crop zone object 
	 **/
	public SafeCropZone getCropZone () {return cropZone;}
	/**
	 * Return the crop zone ID 
	 **/
	public int getIdZone() {return cropZone.getId();}
	/**
	 * Return the crop zone name 
	 **/	
	public String getZoneName () {return cropZone.getName();}
	/**
	 * Add a voxel on the voxel collection for this cell
	 * @param i index of the voxel
	 * @param voxel SafeVoxel to add
	 **/
	public void addVoxel (int i, SafeVoxel voxel) {voxels[i]=voxel;}
	/**
	 * Return the voxel collection for this cell
	 **/
	public SafeVoxel[] getVoxels  () {return voxels;}
	/**
	 * Return the first voxel for this cell
	 **/
	public SafeVoxel getFirstVoxel  () {return voxels[0];}
	/**
	 * Return the cell ID (this is for export) 
	 **/	
	public int getIdCell() {return getId();}
	/**
	 * Set the cell ID on the right
	 **/
	protected void setCellIdRight (int id) {immutable2.cellIdRight = id;}
	/**
	 * Return the cell ID on the right
	 **/	
	protected int getCellIdRight () {return immutable2.cellIdRight;}
	/**
	 * Set the cell ID on the left
	 **/	
	protected void setCellIdLeft (int id) {immutable2.cellIdLeft = id;}
	/**
	 * Return the cell ID on the right
	 **/
	protected int getCellIdLeft () {return immutable2.cellIdLeft;}
	/**
	 * Set the cell ID on the back
	 **/
	protected void setCellIdBack (int id) {immutable2.cellIdBack = id;}
	/**
	 * Return the cell ID on the right
	 **/
	protected int getCellIdBack () {return immutable2.cellIdBack;}
	/**
	 * Set the cell ID on the front
	 **/
	protected void setCellIdFront (int id) {immutable2.cellIdFront = id;}
	/**
	 * Get the cell ID on the right
	 *  @return cell id
	 **/
	protected int getCellIdFront () {return immutable2.cellIdFront;}
	/**
	 * Return the tree ID if a tree is planted
	 **/
	public int getIdTreePlanted() {return idTreePlanted;}
	/**
	 * Set the tree ID if a tree is planted
	 **/
	public void setIdTreePlanted (int i) {idTreePlanted = i;}
	/**
	 * Return true if a tree crown is above the cell
	 **/
	public boolean isTreeAbove() {return isTreeAbove;}
	/**
	 * Set true if a tree crown is above the cell
	 **/	
	public void setIsTreeAbove (boolean b) {isTreeAbove = b;}
	/**
	 * RAZ the tree collection above 
	 **/
	public void razTreeAbove() {treeAbove = new TreeSet<SafeTree> (new SafeTreeHeightComparator());}
	/**
	 * Return the collection of tree above 
	 **/	
	public Collection<SafeTree> getTreeAbove () {return treeAbove;}
	/**
	 * Return the number of trees in the collection of tree above 
	 **/	
	public int getNbrTreeAbove() {return treeAbove.size();}	
	/**
	 * ADD the tree in the collection of tree above 
	 **/
	public void addTreeAbove (SafeTree t) {treeAbove.add(t);}
	/**
	 * Set the lai of the trees above 
	 **/	
	public void setLaiTree (double v) {laiTree = (float) v;}
	/**
	 * Add a value to the lai of the trees above 
	 **/
	public void addLaiTree (double v) {laiTree += (float) v;}
	/**
	 * Return the value to the lai of the trees above 
	 **/
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

	public float getNitrogenLeachingWaterTable() {return  nitrogenLeachingWaterTable;}
	public float getNitrogenAddedByWaterTable () {return  nitrogenAddedByWaterTable;}// AQ 11/04/2011
	public void addNitrogenAddedByWaterTable (double np) {nitrogenAddedByWaterTable +=np;}// AQ 11/04/2011
	public void addNitrogenLeachingWaterTable (double lix) {nitrogenLeachingWaterTable +=lix;}	
	public void setNitrogenAddedByWaterTable (double np) {nitrogenAddedByWaterTable =(float)np;}// AQ 11/04/2011
	public void setNitrogenLeachingWaterTable (double lix) {nitrogenLeachingWaterTable =(float)lix;}
	public double getAnnualNitrogenLeachingWaterTable () {return  (double) annualNitrogenLeachingWaterTable;}
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
	public float getMonthDirectParIncident () { return monthDirectParIncident;}
	public float getMonthDiffuseParIncident () {return monthDiffuseParIncident;}
	public float getMonthVisibleSky () {return monthVisibleSky;}	
	public float getMonthDirectPar() { return monthDirectPar;}
	public float getMonthDiffusePar () {return monthDiffusePar;}

	
	public float getMonthRelativeDirectParIncident () {
		if (getMonthDirectPar() > 0) 
			return getMonthDirectParIncident ()/getMonthDirectPar();
		else return 0;
	}
	public float getMonthRelativeDiffuseParIncident () {
		if (getMonthDiffusePar () > 0)   
			return getMonthDiffuseParIncident ()/getMonthDiffusePar ();
		else return 0;
	}
	public float getMonthRelativeTotalParIncident () {
		if (getMonthDirectPar()+getMonthDiffusePar () > 0)   return (getMonthDirectParIncident () + getMonthDiffuseParIncident ())/(getMonthDirectPar()+getMonthDiffusePar ());
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
	public float getAnnualWaterUptakeByCrop() { return annualWaterUptakeByCrop;}
	public float getAnnualNitrogenUptakeByCrop() { return annualNitrogenUptakeByCrop;}
	public float getAnnualDirectParIncident() { return annualDirectParIncident;}
	public float getAnnualDiffuseParIncident() { return annualDiffuseParIncident;}
	public String toString(){
		String str = "id = "+this.getId();
		if (this.getCropZone()!=null)
			str = str+" cropZone ="+this.getCropZone().getId();
		if (this.getCrop()!=null)
			str = str+" crop ="+this.getCrop().getCropSpeciesName();
		return str;
	}
}
