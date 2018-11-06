package safe.model;

import capsis.kernel.AbstractSettings;

/**
 * Initial values before beginning a simulation
 *
 * @author Isabelle Lecomte - June 2003
 */
public class SafeInitialValues extends AbstractSettings {

	//************************************************************************************************/
	//   LAYER SECTION 
	//************************************************************************************************/
	public double [] layerWaterContent = new double[5];		//m3 m-3
	public double [] layerNo3Content   = new double[5];		//Kg ha-1
	public double [] layerNh4Content   = new double[5];		//Kg ha-1

	//************************************************************************************************/
	//   TREE SECTION 
	//************************************************************************************************/
	public String [] treeSpecies = new String[10];
	public int [] treeAge = new int[10];
	public double [] treeHeight = new double[10];
	public double [] treeCrownBaseHeight = new double[10];
	
	public double [] crownRadius = new double[10];
	public double [] treeX = new double[10];
	public double [] treeY= new double[10];

	//tree root section
	public int [] rootShape = new int[10];					//1=sphere 2=elipsoid 3=conic
	public int [] rootRepartition = new int[10];			//1=Uniform  2=Reverse to the tree distance 3=Negative exponential
	public double [] shapeParam1 = new double[10];			//shape dimension x
	public double [] shapeParam2 = new double[10];			//shape dimension y
	public double [] shapeParam3 = new double[10];			//shape dimension z

	
	//************************************************************************************************/
	//   CROP SECTION 
	//************************************************************************************************/
	public final int[] CROP_STAGE =  {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13};
	public final String[] S_CROP_STAGE =  {"snu", "plt", "dor","lev", "amf", "lax", "flo", "drp", "des", "mat", "rec", "sen", "lan"};
	public final int MAIN_CROP_STAGE = 1; // default phenological stage is baresoil (snu)

	
	// Main crop
	public int mainCropStage = MAIN_CROP_STAGE; 			// Crop stage used at the beginning of simulation (P_stade0) 
	public double mainCropLai = 0;						 	// Initial leaf area index (P_lai0) m2 m-2 
	public double mainCropBiomass = 0; 						//  initial biomass (P_masec0) t ha-1
	public double mainCropRootDepth = 0; 					// initial depth of root front  (P_zrac0) m
	public double mainCropGrainBiomass = 0; 				// initial grain dry weight (P_magrain0)  g m-2
	public double mainCropNitrogen = 0; 					// initial nitrogen amount in the plant (P_QNplante0) kg ha-1 
	public double mainCropReserveBiomass = 0; 				// initial reserve biomass (P_resperenne0)  t ha-1
	public double[] mainCropRootDensity = {0, 0, 0, 0, 0}; 	// Table of initial root density of the 5 horizons of soil for fine earth (P_densinitial)  cm cm-3
	
	// Inter crop
	public int interCropStage = MAIN_CROP_STAGE; 
	public double interCropLai = 0 ; 
	public double interCropBiomass = 0; 
	public double interCropRootDepth = 0; 
	public double interCropGrainBiomass = 0; 
	public double interCropNitrogen = 0; 
	public double interCropReserveBiomass = 0; 
	public double[] interCropRootDensity = {0, 0, 0, 0, 0}; 

}

