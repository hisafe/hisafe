package safe.model;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.TreeSet;

import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.ShiftItem;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.kernel.Step;
import safe.stics.*;

/**
 * SafeCell is a square spatial division of a SafePlot 
 *
 * @author Isabelle Lecomte - july 2002 
 */
public class SafeCell extends SquareCell {

	//4 neighbourg cells used for toric symetry
	private static class Immutable2 implements Cloneable, Serializable {
		public int cellIdRight;	//ID of the right neighbourg cell x+
		public int cellIdLeft;	//ID of the left  neighbourg cell x-
		public int cellIdBack;	//ID of the back  neighbourg cell y+
		public int cellIdFront;	//ID of the front neighbourg cell y-
	}
	protected Immutable2 immutable2;

	private SafeCrop crop;						//reference of the crop object sown on this cell
	private SafeVoxel [] voxels;				//references of voxels objects attached to this cell
	private boolean treeAbove;					//true if one tree at least is above the cell 
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

	//WATER BUDGET
	private float waterAddedByWaterTable;			//amout of water provided by the water table to saturated voxels (mm)
	private float waterTakenByDesaturation;			//amout of water taken    by the water table to desaturated voxels (mm)
	private float waterUptakeInSaturationByTrees;
	private float waterUptakeInSaturationByCrop;
	private float nitrogenUptakeInSaturationByTrees;
	private float nitrogenUptakeInSaturationByCrop;

	//MICROCLIMATE MODEL DAILY VALUE
	private float rainInterceptedByTrees; 			//Rain intercepted by trees on this cell (mm d-1)
	private float rainTransmittedByTrees; 			//Rain transmitted by trees on this cell (mm d-1)
	private float stemFlowByTrees; 					//Stemflow by trees on this cell (mm d-1)
	
	//rainInterceptedByCrop is taken directly from SticsCrop.interpluie[1] (mm d-1)
	//stemFlowByCrop is taken directly from SticsCrop.stemflow (mm d-1) 
	//rainTransmittedByCrop is calculated (mm d-1) 
	
	private float etpCalculated;					// etp calculated with global incident radiation

	
	//MONTLY MEAN VALUES FOR EXPORT
	private float monthDirectParIncident;				//moles.m-2
	private float monthDiffuseParIncident;				//moles.m-2
	private float monthVisibleSky;						//%	
	private float monthDirectPar;						//moles.m-2
	private float monthDiffusePar;						//moles.m-2
	private int monthNbrDays;							//number of days in the month					
	
	
	public SafeCell (SafePlot plot, Vertex3d coord, int i, int j, int id, int nbVoxels) {
		
		super (plot, id, 0, coord, i, j);		//SquareCell

		createImmutable2 ();
		immutable2.cellIdRight = 0;
		immutable2.cellIdLeft  = 0;
		immutable2.cellIdBack  = 0;
		immutable2.cellIdFront = 0;

		// Creation of the soil voxels
		voxels = new SafeVoxel [nbVoxels];
		
		//reset light results 
		resetDirect();
		resetDiffuse();
		this.relativeToFlatCellDirectParIncident=1;
		this.relativeToFlatCellDiffuseParIncident=1;
		this.relativeToFlatCellVisibleSky=1;
		this.relativeToFlatCellDirectNirIncident=1;
		this.relativeToFlatCellDiffuseNirIncident=1;
		this.directParIncident=0;
		this.diffuseParIncident=0;
		this.relativeDirectParIncident=1;
		this.relativeDiffuseParIncident=1;
		this.relativeTotalParIncident=1;
		this.relativeGlobalRadIncident=1;
		this.visibleSky=1;

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

		// Copy the light model results of the original cell
		c.setRelativeToFlatCellDirectParIncident(this.getRelativeToFlatCellDirectParIncident());
		c.setRelativeToFlatCellDiffuseParIncident(this.getRelativeToFlatCellDiffuseParIncident());
		c.setRelativeToFlatCellVisibleSky(this.getRelativeToFlatCellVisibleSky());
		c.setRelativeToFlatCellDirectNirIncident(this.getRelativeToFlatCellDirectNirIncident());
		c.setRelativeToFlatCellDiffuseNirIncident(this.getRelativeToFlatCellDiffuseNirIncident());

		
		//Monthly values
		c.monthDirectPar=this.monthDirectPar;	
		c.monthDiffusePar=this.monthDiffusePar;
		c.monthDirectParIncident=this.monthDirectParIncident;	
		c.monthDiffuseParIncident=this.monthDiffuseParIncident;
		c.monthVisibleSky=this.monthVisibleSky+this.visibleSky;	
		c.monthNbrDays = this.monthNbrDays + 1;

		// Cloning soil voxels
		SafeVoxel oldvoxels [] = c.voxels;
		SafeVoxel newvoxels [] = new SafeVoxel [this.voxels.length];
		
		for (int j=0; j < this.voxels.length; j++) {
			SafeLayer l = oldvoxels[j].getLayer();
			SafeVoxel ci = new SafeVoxel(this.voxels[j], c);
			l.addVoxel(ci);
			newvoxels[j] = ci;
		}
		c.voxels= newvoxels;

		// Cloning the crop object
		// STICS memory table are saved only if step is visible, if not they are deleted to save memory space
		Step step = this.getPlot().getScene().getStep();
		if (this.getCrop () != null) c.crop = new SafeCrop (this.getCrop (), step.isVisible(), c);

		//RAZ
		c.setWaterAddedByWaterTable (0);
		c.setWaterTakenByDesaturation (0);
		c.setWaterUptakeInSaturationByTrees (0);
		c.setWaterUptakeInSaturationByCrop (0);
		c.setNitrogenUptakeInSaturationByTrees (0);
		c.setNitrogenUptakeInSaturationByCrop (0);	
		c.setRainTransmittedByTrees (0);
		c.setRainInterceptedByTrees (0); 
		c.setStemFlowByTrees (0); 

		return c;
	}


	/**
	* Agregation of STICS minicouches values in HISAFE voxels 
	* after STICS PART 1
	*/
	public void miniCouchesToVoxelsAfterStics1 (SafeSticsParameters sticsParam, 
												int simulationDay) {

		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;
		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;

		double cellArea = this.getArea(); 			//m2
		double lvopt = sticsParam.P_lvopt;			 // Optimum root density // cm root.cm-3 soil 
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

			//nombre of miniCouches in this voxel
			int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
			int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel
			int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	   //number    miniCouches  for current voxel

			for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
				
				voxelMoisture 	+= sticsCommun.HUR[z];				    // voxel soil humidity 	%						
				soilEvapo  		+= sticsCommun.esz[z];				    // voxel soil evaporation 	mm		
				soilTemperature += sticsCommun.tsol[z];					// Soil temperature	degrees
				voxelNo3 		+= sticsSoil.nit[z+1];					// voxel soil no3 kg N ha-1		
				voxelNh4 		+= sticsSoil.amm[z];					// voxel soil nh4 kg N ha-1	
				
				//For these data No need to go further than root depth limit
				if (z <= zrac) {					
					cropRootDensity += sticsCrop.rljour [z] ;
					cropRootEffectiveDensity += sticsCrop.flrac [z]* lvopt;	//FLRAC * lvopt
				}
			}


			//convert cm/cm3 in m/m3 (miniCouches is 1cm)
			cropRootDensity = (cropRootDensity * 10000) / miniCoucheNumber;
			voxels[i].setCropRootDensity (cropRootDensity);


			//add a new root branch in the TOPOLOGY MAP
			if (cropRootDensity > 0) {
				if (! this.getCrop().getFineRoots().getRootTopology ().containsKey(voxels[i])) {
					if (i > 0)
						this.getCrop().getFineRoots().addCropRootTopology (voxels[i], voxels[i-1], simulationDay, cropRootDensity);
					else
						this.getCrop().getFineRoots().addCropRootTopology (voxels[i], null, simulationDay, cropRootDensity);
				}
			}
			
			//UPDATE fine root density in the TOPOLOGY MAP
			if (this.getCrop().getFineRoots().getRootTopology ().containsKey(voxels[i]))
				this.getCrop().getFineRoots().setFineRootTopology (voxels[i], cropRootDensity);			

			//Store agregation result in voxel
			double newTheta 		= (voxelMoisture/10)/miniCoucheNumber;
			double newSoilTemperature	= soilTemperature/miniCoucheNumber;
			double newNo3			= voxelNo3;
			double newNh4 			= voxelNh4;

			voxels[i].setNitrogenNo3Stock ((newNo3 / 10) * cellArea);				    //convert kg ha-1 in g
			voxels[i].setNitrogenNh4Stock ((newNh4 / 10) * cellArea);				    //convert kg ha-1 in g
			voxels[i].setWaterStock (newTheta * voxels[i].getVolume () * 1000);		    //convert m3 m-3 in liters
			voxels[i].setSoilTemperature(newSoilTemperature); 							//mean of temperature
			voxels[i].setEvaporation  (soilEvapo * cellArea);							//convert mm in liters;				
		}
	}

	/**
	* Agregation of STICS minicouches values in HISAFE voxels 
	* after STICS PART 2
	* Nitrogen have been updated by majNsol
	*/
	public void miniCouchesToVoxelsAfterStics2 () {

		double cellArea = this.getArea(); 			//m2
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;

		//FOR EACH VOXEL
		for (int i = 0; i < this.voxels.length; i++) {

			float voxelNo3 = 0;
			float voxelNh4 = 0;
			float voxelMoisture = 0;

			//nombre of miniCouches in this voxel
			int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
			int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel
			int miniCoucheNumber = voxels[i].getMiniCoucheNumber();	   //number    miniCouches  for current voxel
			
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
	
	/**
	* Agregation of STICS minicouches values in HISAFE voxels 
	* after STICS PART 2 
	* only if water extraction have been calculated by STICS (testing STICS stand alone) 
	*/
	public void miniCouchesToVoxelsAfterSticsWaterExtraction () {

		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;

		double totalWaterUptake = 0; 		//sum of water uptake in this cell
		double totalNitrogenUptake = 0; 	//sum of nitrogen uptake in this cell
		double cellArea = this.getArea(); 			//m2
		int zrac = (int) sticsCrop.zrac +1;	

		
		//FOR EACH VOXEL if THERE IS ROOTS
		if (sticsCrop.zrac > 0) {
			
			for (int i = 0; i < this.voxels.length; i++) {
	
				float cropWaterUptake  = 0;			
				float cropNitrogenUptake = 0;
	
				//nombre of miniCouches in this voxel
				int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
				int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel
				
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
				voxels[i].setCropNitrogenUptake ((cropNitrogenUptake / 10) * cellArea );	//  convert kg ha-1 in g

				totalWaterUptake += cropWaterUptake; 		  				// total crop water uptake (all voxels)
				totalNitrogenUptake  += cropNitrogenUptake ;		// total crop nitrogen uptake (all voxels)
				
				//Sum water extracted in saturated zone in voxel is saturated by water table
				if (voxels[i].getIsSaturated ()) 
					this.addWaterUptakeInSaturationByCrop (cropWaterUptake * cellArea);	// gt - 5.02.2009
				
			}
		}
		

		this.getCrop().setWaterUptake (totalWaterUptake);			// mm
		this.getCrop().setNitrogenUptake (totalNitrogenUptake);		// kg ha-1
	
/*		//calcul des stress equivalents hisafe 
		this.getCrop().setHisafeWaterStress(1.0);
		if ((this.getCrop().getWaterDemand() > 0) &&  (this.getCrop().getPhenologicStage() < 13)) {
			double waterStress = Math.min (totalWaterUptake  / this.getCrop().getWaterDemand(), 1);
			waterStress = Math.max (waterStress, 0);
			this.getCrop().setHisafeWaterStress(waterStress);
		}
		
		this.getCrop().setHisafeNitrogenStress(1.0);
		if ((this.getCrop().getNitrogenDemand() > 0) &&  (this.getCrop().getPhenologicStage() < 13)) {
			double nitrogenStress = Math.min (totalNitrogenUptake  / this.getCrop().getNitrogenDemand(), 1);
			nitrogenStress = Math.max (nitrogenStress, 0);
			this.getCrop().setHisafeNitrogenStress(nitrogenStress);
		}	*/		
	}

	/**
	* Desgregation of  HISAFE voxels values in STICS miniCouches
	* Tree and crop water and nitrogen uptake have to be reported in STICS mini-couches
	*/
	public void voxelsToMiniCouches (SafeInitialParameters safeSettings) {

		SafeSticsCrop sticsCrop = this.getCrop().sticsCrop;
		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;

		double cellArea = this.getArea(); 	//m2
		//int rootDepth = (int) (this.getCrop().getRootDepth() * 100)+1;	//gt - 05.02.2009 "+1" added	
		int cropRootDepth = (int) (this.getCrop().getRootDepth() * 100);	//il - 05.07.2017 verif stics int(zrac)
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
						
	//IL 16/20/2017 Je ne comprends pas ce code, je l'enlève					
	/*					if (voxels[i].getIsSaturated() && (reportWaterTree > 0)) {	// gt - 5.02.2009 - if we have to report waterExtraction in a saturated voxel
							//sticsCommun.treeWaterUptake [z] = 0;
							sticsCommun.treeWaterUptake [z] = (float) reportWaterTree;	//il 05.07.2017 je pense c'est plus cohérent
							this.addWaterExtractedInSaturationByTrees(reportWaterTree);
							if (reportWaterTree > 0) System.out.println("cell="+this.getId()+" z="+z+" addWaterExtractedInSaturationByTrees="+reportWaterTree);
							
							reportWaterTree = 0;
						} else {*/
							if ((sticsCommun.HUR[z]			// miniCouche water content
								-sticsCommun.esz[z]			// soil evaporation
								-((treeWaterUptake/miniCoucheNumber) + (cropWaterUptake/miniCoucheNumber))/cellArea	// tree and crop water uptake
								-reportWaterTree-reportWaterCrop) < ha) {
	
								sticsCommun.treeWaterUptake[z] = (float) Math.max(sticsCommun.HUR[z]
																	-sticsCommun.esz[z]
																	-cropWaterUptake/cellArea/miniCoucheNumber
																	-ha
																	-reportWaterCrop
															,0);
								reportWaterTree += treeWaterUptake/cellArea/miniCoucheNumber - sticsCommun.treeWaterUptake[z];
	
							} else {
								sticsCommun.treeWaterUptake [z] = (float) ((treeWaterUptake/cellArea/miniCoucheNumber)	//convert liters in mm
															+reportWaterTree);
								reportWaterTree = 0;

							}
					//	}
		
					
						int indice     = (z*3)+1;				//Variable sticsCrop.epz(0:2,1000) 
						//IL 16/20/2017 Je ne comprends pas ce code, je l'enlève
	/*					if (voxels[i].getIsSaturated() && (reportWaterCrop > 0)) {
							//sticsCrop.epz[indice]=0;
							
							if (reportWaterCrop > 0) System.out.println("cell="+this.getId()+" z="+z+" addWaterExtractedInSaturationByCrops="+reportWaterCrop);
							
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
		if (reportWaterTree > 0)  System.out.println("cell="+this.getId()+"  reportWaterTree="+reportWaterTree);
		if (reportWaterCrop > 0) System.out.println("cell="+this.getId()+"  reportWaterCrop="+reportWaterCrop);
		
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
			
			//nombre of miniCouches in this voxel
			int miniCoucheMin = voxels[i].getMiniCoucheMin();		//starting  miniCouches  for current voxel
			int miniCoucheMax = voxels[i].getMiniCoucheMax();	   //ending    miniCouches  for current voxel
			Double soilEvapo = 0.0;
			for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
				soilEvapo  		+= sticsCommun.esz[z];				    // voxel soil evaporation mm	
			}	
			
			if (soilEvapo != 0) {
				voxels[i].setEvaporation (soilEvapo);
				voxels[i].reduceWaterStock(soilEvapo*this.getArea());
			}
		}
	}	
	

	/**
	* Desgregation of HISAFE voxels water and nitrogen content in STICS miniCouches
	* This code is called after water Table calculation 
	*/
	public void voxelsToMinicouchesWaterNitrogen (SafeInitialParameters safeSettings) {

		double cellArea = this.getArea();


		SafeSticsCommun sticsCommun = this.getCrop().sticsCommun;
		SafeSticsSoil sticsSoil = this.getCrop().sticsSoil;
		
		//FOR EACH SATURATED VOXEL
		for (int i=0; i < this.voxels.length; i++) {

			//ATTENTION, ce test sera ï¿½ revoir si l'on decide de passer la teneur en eau de SAT ï¿½ FC
			//en cas de descente de la nappe !!!!!!!!!!
			 if (voxels[i].getIsSaturated () == true) { 

				//Grab voxel values
				double theta = voxels[i].getTheta ();				//%
				double qnO3 = voxels[i].getNitrogenNo3Stock ();		//g/voxel
				double qnH4 = voxels[i].getNitrogenNh4Stock ();		//g/voxel
				
				//desAgregate voxel values in 1cm miniCouches
				
				//nombre of miniCouches in this voxel
				int miniCoucheMin = (int) (voxels[i].getSurfaceDepth() * 100);		//starting  miniCouches  for current voxel
				int voxelMiniLayerNumber = (int) (voxels[i].getThickness() * 100);
				int miniCoucheMax = miniCoucheMin + voxelMiniLayerNumber - 1;	//ending    miniCouches  for current voxel

				for (int z=miniCoucheMin; z <= miniCoucheMax; z++) {
					sticsCommun.HUR[z] 	= (float) (theta * 10);			// soil humidity 
					sticsSoil.nit[z+1] = (float) (qnO3*10/cellArea/voxelMiniLayerNumber);			// from g to kg.ha-1.cm-1 (/cellArea/1000*10000/(voxelThickness*100))
					sticsSoil.amm[z] = (float) (qnH4*10/cellArea/voxelMiniLayerNumber);			// from g to kg.ha-1
				}
			}
		}
	}

	/**
	* Recalculation of crop root topology
	* After STICS crop root growth (PART I) 
	*/
	public void computeCropRootTopology (int simulationDay) {
	
		//FOR EACH VOXEL
		for (int i = 0; i < this.voxels.length; i++) {
	
			double cropRootDensity = voxels[i].getCropRootDensity();
			if (cropRootDensity > 0) {

				if (! this.getCrop().getFineRoots().getRootTopology ().containsKey(voxels[i])) {
					if (i > 0)
						this.getCrop().getFineRoots().addCropRootTopology (voxels[i], voxels[i-1], simulationDay, cropRootDensity);
					else
						this.getCrop().getFineRoots().addCropRootTopology (voxels[i], null, simulationDay, cropRootDensity);
				}
		
				//UPDATE fine root density in the TOPOLOGY MAP
				if (this.getCrop().getFineRoots().getRootTopology ().containsKey(voxels[i]))
					this.getCrop().getFineRoots().setFineRootTopology (voxels[i], cropRootDensity);
			}

		}

	}
	/**
	 * Return the collection of trees above a cell
	 **/
	public Collection treeAboveComputation() {

	// At the moment it is impossible to have several trees avove the same cell
	// because of limititation of crown volume
	// but this part of the code will be ready if this constraint is one day removed
	    double cellX = this.getXCenter();
		double cellY = this.getYCenter();
		Collection treeAboves = new TreeSet (new SafeTreeHeightComparator());	//trees are sorted on tree height max to min
		SafeStand stand = (SafeStand) this.getPlot().getScene();

		for (Iterator iter=stand.getTrees().iterator(); iter.hasNext(); ) {
				SafeTree tree = (SafeTree) iter.next();
				double treeX = tree.getX ();
				double treeY = tree.getY ();
				double crownRadiusTreeLine = tree.getCrownRadiusTreeLine ();
				double crownRadiusInterRow = tree.getCrownRadiusInterRow ();
				
				// The cell gravity center is in the crown shape projection
				if (
						  (Math.pow(cellX - treeX ,2) / Math.pow(crownRadiusInterRow,2))
						+ (Math.pow (cellY - treeY ,2) / Math.pow(crownRadiusTreeLine,2))
						< 1 ) 
						treeAboves.add (tree);
		}

		return (treeAboves);
	}

    /**
	 * Check if tree root have colonized the cell (at least one voxel) 
	 */
    public boolean isColonised (int treeID)  {
    	boolean retour = false; 		
		SafeVoxel[] voxels = this.getVoxels();

		int nbVoxels = voxels.length;
		for (int i=0; (i<nbVoxels && (!retour))  ; i++) {
			if (voxels[i].getTheTreeRootDensity(treeID-1) > 0) {			
				retour = true; 
			}
		}
		return retour;
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
	 * Daily calculation of PAR incident on the cell depending climatic entries
	 */
	public void updateDailyLightResults(SafeBeamSet beamSet, SafeDailyClimat dayClimat, SafeInitialParameters settings) {

		if(!settings.cropLightMethod){
			getCrop().cropLightInterception(settings, beamSet, getRelativeToFlatCellDirectParIncident(), getRelativeToFlatCellDiffuseParIncident());
		}

		//Calculation of diffuse PAR transmitted today on this cell in Moles m-2 d-1
		float dailyDiffuse = dayClimat.getDiffusePar ();				  // Climatic entry of the day in Moles m-2 d-1

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
		double dailyDirect = dayClimat.getDirectPar ();					//Climatic entry of the day in Moles m-2 d-1 //GT 2007
		double dayDirectTransmitted = dailyDirect * this.getRelativeToFlatCellDirectParIncident();		//Moles m-2 d-1
		setDirectParIncident (dayDirectTransmitted);	//Moles m-2 d-1

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
	 *  used to compute light interception by crops
	 */
	public void findShadingCells (SafeBeamSet beamSet, SafePlot plot){

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
	 * Set the crop species on this cell
	 **/
	public void setCropSpecies (SafeCropSpecies cropSpecies) {
		if (this.crop == null) this.crop  = new SafeCrop ();
		this.crop.setCropSpecies(cropSpecies);
	}
	/**
	 * Return the crop object sown on this cell
	 **/
	public SafeCrop getCrop () {return crop ;}
	/**
	 * Return the crop name sown on this cell
	 **/
	public String getCropName () {return (crop.getCropSpecies()).getName() ;}
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

	public boolean isTreeAbove() {return treeAbove;}
	public void setTreeAbove (boolean b) {treeAbove = b;}
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
	public double getRainInterceptedByCrop () {return  (double) this.getCrop().getSticsCrop().interpluie[1];}
	public double getStemFlowByCrop () {return  (double) this.getCrop().getSticsCrop().stemflow;}
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
	public void addDrainageWaterTable (double v) {getCrop().sticsCommun.drain += (float) v;}

	
	public void setWaterTakenByDesaturation (double v) {waterTakenByDesaturation = (float) v;}
	public void addWaterTakenByDesaturation(double v) {waterTakenByDesaturation += (float) v;}
	public double getWaterTakenByDesaturation () {return (double) waterTakenByDesaturation;}


	public void setWaterUptakeInSaturationByTrees (double v) {waterUptakeInSaturationByTrees = (float) v;}
	public void addWaterUptakeInSaturationByTrees (double v) {waterUptakeInSaturationByTrees += (float) v;}
	public double getWaterUptakeInSaturationByTrees () {return (double) waterUptakeInSaturationByTrees;}
	public void setWaterUptakeInSaturationByCrop (double v) {waterUptakeInSaturationByCrop = (float) v;}
	public void addWaterUptakeInSaturationByCrop (double v) {waterUptakeInSaturationByCrop += (float) v;}
	public double getWaterUptakeInSaturationByCrop () {return (double) waterUptakeInSaturationByCrop;}

	public double getWaterUptakeByTrees () {
		double waterUptakeByTrees = 0;
		SafeVoxel[] voxels = this.getVoxels();
		for (int i=0; (i<voxels.length)  ; i++) {
			waterUptakeByTrees += voxels[i].getTotalTreeWaterUptake(); 
		}
		return waterUptakeByTrees;
	}
	
	//aq - 10.06.2011 - Same for Nitrogen
	public void setNitrogenUptakeInSaturationByTrees (double v) {nitrogenUptakeInSaturationByTrees = (float) v;}
	public void addNitrogenUptakeInSaturationByTrees (double v) {nitrogenUptakeInSaturationByTrees += (float) v;}
	public double getNitrogenUptakeInSaturationByTrees () {return (double) nitrogenUptakeInSaturationByTrees;}
	public void setNitrogenUptakeInSaturationByCrop (double v) {nitrogenUptakeInSaturationByCrop = (float) v;}
	public void addNitrogenUptakeInSaturationByCrop (double v) {nitrogenUptakeInSaturationByCrop += (float) v;}
	public double getNitrogenUptakeInSaturationByCrop () {return (double) nitrogenUptakeInSaturationByCrop;}

	public double getNitrogenUptakeByTrees () {
		double nitrogenUptakeByTrees = 0;
		SafeVoxel[] voxels = this.getVoxels();
		for (int i=0; (i<voxels.length)  ; i++) {
			nitrogenUptakeByTrees += voxels[i].getTotalTreeNitrogenUptake(); 
		}
		return nitrogenUptakeByTrees / this.getArea() * 10; // from g to Kg.ha-1
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

	// get set functions
	public double getRelativeToFlatCellDirectParIncident () {return (double) relativeToFlatCellDirectParIncident;}
	public double getRelativeToFlatCellDiffuseParIncident () {return (double) relativeToFlatCellDiffuseParIncident;}
	public double getRelativeToFlatCellVisibleSky () {return (double) relativeToFlatCellVisibleSky;}
	public double getRelativeToFlatCellDirectNirIncident () {return (double) relativeToFlatCellDirectNirIncident;}
	public double getRelativeToFlatCellDiffuseNirIncident () {return (double) relativeToFlatCellDiffuseNirIncident;}

	public double getDirectParIncident () {return (double) directParIncident;}
	public double getDiffuseParIncident () {return (double) diffuseParIncident;}
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
		if (this.getCrop() == null) return "";
		if (this.getCrop().getCropSpecies() == null) return "";
		return this.getCrop().getCropSpecies().getName();
	}
	

	public double getTotalTreeWaterUptake() {
		double waterUptake = 0;
		for (int i = 0; i < voxels.length; i++) {
			double treeUptake[]= voxels[i].getTreeWaterUptake();
			for (int j = 0; j < treeUptake.length; j++) {
				waterUptake +=  treeUptake[j];
			}
		}
		return waterUptake;
	}
	
	

	public double getTreeFineRootLength() {
		double length = 0;
		for (int i = 0; i < voxels.length; i++) {
			length += voxels[i].getTotalTreeRootDensity() * voxels[i].getVolume(); //m m-3 * m3
		}
		return length;	//m
	}
	
	public double getTreeCoarseRootBiomass() {
		double biomass = 0;
		for (int i = 0; i < voxels.length; i++) {
			biomass += voxels[i].getTotalTreeCoarseRootBiomass(); // kg C 
		}
		return biomass / (this.getArea() / 10000); //kg ha-1
	}
	public String toString(){
		String str = "";
		if (this.getCrop() != null)
			str = "id = "+this.getId()+" crop ="+this.getCrop().getCropSpecies().getName();
		else
			str = "id = "+this.getId()+" crop = NULL";
		return str;
	}

	/**
	 * return the list of voxel ID for a list of soil depth
	 **/
	public Collection getVoxelList (float [] tabDepth) {
		ArrayList voxelList = new ArrayList ();
		for (int i = 0; i < voxels.length; i++) {
			for (int j=0; j < tabDepth.length; j++) {
				if ((voxels[i].getSurfaceDepth () <= tabDepth[j])
				&& (voxels[i].getSurfaceDepth() + voxels[i].getThickness()  > tabDepth[j]))
					voxelList.add(voxels[i]);
			}
		}
		return voxelList;
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
	//to export tree deep root residus 
	public double getNitrogenDeepRootResidu () {
		double nitrogenDeepRootResidu=0;
		
		SafeVoxel[] voxel = this.getVoxels();

			for (int i = 0; i < voxel.length; i++) {
				if (voxel[i].getZ()*100 > this.getCrop().sticsSoil.P_profhum) 
					nitrogenDeepRootResidu += voxel[i].getNitrogenRootResidu();
			}
			   
		return nitrogenDeepRootResidu * 10 / this.getArea();		 //convert g in kg ha-1 
		}
	
}
