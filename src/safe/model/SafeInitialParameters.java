package safe.model;

import capsis.kernel.AbstractSettings;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.InitialParameters;
import jeeb.lib.util.PathManager;
import java.util.Collection;

/**
 * GENERAL parameters
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafeInitialParameters extends AbstractSettings implements InitialParameters {

	public String standInventory;

	// **** PLOT **************************************************************************
	public static final int NB_TREE_MAX  = 10; // max number of trees on the plot 
	public static final int NB_LAYER_MAX = 5; // max number of pedologic layers
	public int nbLayers = 0; 				  // number of layers
	public int nbTrees = 0; 				  // number of trees

	// ** LIGHT module ********************************************************************
	// turtle beam description in degrees (Azimut, Height angles, UOC , SOC)
	public static final int NB_TURTLE_BEAM = 46; 			// number of diffuse beams if turtle repartition
	public final double[] LIGHT_TURTLE_AZ = {12.23, 59.77, 84.23, 131.77, 156.23, 203.77, 228.23, 275.77, 300.23,
			347.77, 36, 108, 180, 252, 324, 0, 72, 144, 216, 288, 23.27, 48.73, 95.27, 120.73, 167.27, 192.73, 239.27,
			264.73, 311.27, 336.73, 0, 72, 144, 216, 288, 36, 108, 180, 252, 324, 0, 72, 144, 216, 288, 180};
	public final double[] LIGHT_TURTLE_EL = {9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 10.81, 10.81,
			10.81, 10.81, 10.81, 26.57, 26.57, 26.57, 26.57, 26.57, 31.08, 31.08, 31.08, 31.08, 31.08, 31.08, 31.08,
			31.08, 31.08, 31.08, 47.41, 47.41, 47.41, 47.41, 47.41, 52.62, 52.62, 52.62, 52.62, 52.62, 69.16, 69.16,
			69.16, 69.16, 69.16, 90};
	public final double[] LIGHT_TURTLE_SOC = {0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043,
			0.0043, 0.0055, 0.0055, 0.0055, 0.0055, 0.0055, 0.014, 0.014, 0.014, 0.014, 0.014, 0.0197, 0.0197, 0.0197,
			0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 0.0336, 0.0336, 0.0336, 0.0336, 0.0336, 0.0399,
			0.0399, 0.0399, 0.0399, 0.0399, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0481};
	public final double[] LIGHT_TURTLE_UOC = {0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007,
			0.0086, 0.0086, 0.0086, 0.0086, 0.0086, 0.017, 0.017, 0.017, 0.017, 0.017, 0.0224, 0.0224, 0.0224, 0.0224,
			0.0224, 0.0224, 0.0224, 0.0224, 0.0224, 0.0224, 0.0317, 0.0317, 0.0317, 0.0317, 0.0317, 0.036, 0.036,
			0.036, 0.036, 0.036, 0.0405, 0.0405, 0.0405, 0.0405, 0.0405, 0.0377};
	public final double[] LIGHT_TURTLE_IR = {0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069,
			0.0069, 0.0081, 0.0081, 0.0081, 0.0081, 0.0081, 0.0192, 0.0192, 0.0192, 0.0192, 0.0192, 0.0222, 0.0222,
			0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0316, 0.0316, 0.0316, 0.0316, 0.0316,
			0.0342, 0.0342, 0.0342, 0.0342, 0.0342, 0.0402, 0.0402, 0.0402, 0.0402, 0.0402, 0.0430};

	// if false, interception by crop is computed with stics's formalism
	// if true, computed together with interception by trees.
	public static final boolean CROP_LIGHT_METHOD = false;

	// ** WATER REPARTITION module *************l**********************************************
	public final double PF_WILTING_POINT = 4.2;
	public final double PF_FIELD_CAPACITY = 2.5;

	// **STICS**********************************************************************************
	public final int STICS_MINI_LAYERS = 1000; 	/* number of STICS mini layers */

	// *** STONE TYPES *********************************************
	public String[] STONE_NAME = {"Beauce limestone 1", "Beauce limestone 2", "Lutetian limestone",
			"Lutetian Brackish marl and limestone", "Morainic gravels", "Unweathered flint, sandstone or granite", 
			"Weathered granite","Jurassic limestone", "Pebbles from Magneraud", "Other pebbles"};
	public double[] STONE_VOLUMIC_DENSITY = {2.2, 1.8, 2.1, 2.3, 2.5, 2.65, 2.3, 2.2, 1.5, 0}; // g cm-3
	public double[] STONE_WATER_CONTENT = {0.07, 0.16, 0.11, 0.05, 0.03, 0.01, 0.05, 0.05, 0.26, 0.0}; // % ponderal


	// ******** UNIT CONVERSION FACTOR ****************************************/
	public final double M3_TO_CM3 = 1000000; // volume m3 to cm3
	public final double MM3_TO_CMCM3 = 10000; // density mm-3 to cm cm-3

	//********** READ in GENERAL PARAMETER FILE
	// Coefficients a and b of the relationship : Diffuse/Global = a - b Global/G0
	// this is related to the location of the plot
	public  double diffuseCoeffA;
	public  double diffuseCoeffB;

	// Coefficient to convert GLOBAL radiation to PAR (PAR = 0.48 RG)
	public  double parGlobalCoefficient;
	// Coefficient to convert Moles to MJ (1 Mole = 0.217 MJ)
	// Approximation by photosyn assistant (Dundee Scientific Ltd)
	public  double molesParCoefficient; // for PAR radiation

	// Angstrom coefficients for calculating insolation
	public  double aangst;
	public  double bangst;

	// ** MICROCLIMATE module ********************************************************************
	public  double priestleyTaylorCoeff;
	public  double sigma; // Stefan-Boltzman constant (W m-2 T-4)
	public  double gamma; // Psychrometric constant (mbar/degreeC)
	
	// ** LIGHT MODULE ********************************************************************
	public double timeStep;	// time (in hours) between two calculations of Sun position
	public int nbTimeStepMax;	// maximal number of calculations
	public boolean SOC;		//standard overcast sky (0-1) 
	public boolean UOC;		//uniform overcast sky (0-1) 
	public boolean turtleOption;	//turtle beam repartition option  (0-1) 
	public double diffuseAngleStep;
	public double declinationThreshold;		// light module is launch if sun declination has increase more than the thresold
	public double leafAreaThreshold;		// light module is launch if leaf area has increase more than the thresold
	public int nbImpactMultiplication;		//nbr of beam impact on one cell (1-4-9) 
	
	public boolean cropLightMethod;		// interception by crop

	public Collection cellImpacts;	// Vertex3d for light impact of each cell (process lighting)
	

	// This parameter indicates the relative influence of dry voxels on the calculation
	// of the averaged soil water potential perceived by the plant
	// When = 1, we use a harmonic average
	public  double harmonicWeightedMean ;

	// Calculation of Phi_pF
	public  double integrationStep;
	public  double maxPhiPF;
	
	// ** Snow module - IL-06-10-2017 **********************************************************
	public  double maxTempSnow;			//° If Tmax <  x  rain is transformed in snow
	public  double minTempSnow ;			//° If Tmoy <= x  rain is transformed in snow
	public  double maxDailySnowMelt;	//mm Max amount of snow that can melt each day
	public  double maxTempSnowMelt;	//° If Tmoy = x, snow can melt = maxDailySnowMelt
	public  double minTempSnowMelt;	    //° If Tmoy < x, snow can melt = 0
		
	// Nitrogen extraction module
	//nitrogen parameters
	public double nitrogenDiffusionConstant;		//cm2 day-1
	public double nitrogenEffectiveDiffusionA0;	//ND
	public double nitrogenEffectiveDiffusionA1;	//ND
	public double no3AbsorptionConstant;			//ND
	public double nh4AbsorptionConstant;			//ND
	public double no3Fraction;						//ND
	
	//Nitrogen deep mineralisation module
	public double fmin1;
	public double fmin2;
	public double fmin3;
	
	//STICS water extraction method for pure crop (set in hisafe.par) 
	public boolean sticsWaterExtraction = false;
	
	//LAI FILE NAME (stics lai forcing)
	public String laiFileName=""; 			// crop lai data file
	
	// Scene construction
	// the scene is built with pldFileName OR (plotSettings AND initialValues)
	public String pldFileName;

	public SafePlotSettings plotSettings; // plot parameters
	public SafeInitialValues initialValues; // initial values (soil, tree...)

	 // default value
	public String projectName = "Safe";
	public String dataOriginalPath = PathManager.getInstallDir () + "/data/safe";
	public String dataPath = PathManager.getInstallDir () + "/data/safe";

	private GScene initScene;

	/**
	 * Constructor.
	 */
	public SafeInitialParameters () throws Exception {
		this.dataPath = PathManager.getInstallDir () + "/data/safe";
		
	}

	/**
	 * Constructor for scripts.
	 */
	public SafeInitialParameters (String workingDir, String projectName,  String pldFileName) throws Exception {
		this ();
		this.pldFileName = pldFileName;
		this.projectName = projectName; 
		this.dataPath = workingDir;

	}

	public void resetDataPath () {
		this.dataPath = PathManager.getInstallDir () + "/data/safe";
	}

	@Override
	public void buildInitScene (GModel model) throws Exception {

		SafeModel m = (SafeModel) model;

		if (pldFileName != null) {
			initScene = (SafeStand) m.loadInitStand (pldFileName, m, this);

		} 

	}

	@Override
	public GScene getInitScene () {
		return initScene;
	}

}
