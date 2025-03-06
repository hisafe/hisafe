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

import capsis.kernel.AbstractSettings;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.InitialParameters;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Vertex3d;

import java.util.Collection;

/**
 * GENERAL parameters
 * 
 * @author Isabelle Lecomte - INRAE Montpellier - July 2002
 */
public class SafeGeneralParameters extends AbstractSettings implements InitialParameters {


	private static final long serialVersionUID = 1L;

	public String standInventory;

	// **** PLOT **************************************************************************
	
	public static final int NB_TREE_MAX  = 100; // max number of trees on the plot 
	public static final int NB_LAYER_MAX = 5; 	// max number of pedologic layers
	public static final int NB_COHORT_MAX  = 10; // max number of leaf cohort on trees
	public int nbLayers = 0; 				  	// number of layers
	public int nbTrees = 0; 				  	// number of trees

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
	public int nbImpactMultiplication;		// number of beam impact on one cell (1-4-9) 
	
	public boolean cropLightMethod;			//crop light interception method 0=STICS 1=Hi-sAFe 

	public Collection<Vertex3d> cellImpacts;	// Vertex3d for light impact of each cell (process lighting)
	



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
	
	//default spreading zone for residues (1=Under the tree crown 2=all over the plot)
	public int leavesResiduesSpreading;			
	public int branchesResiduesSpreading;			
	
	//STICS water extraction method for pure crop (set in hisafe.par) 
	public boolean sticsWaterExtraction = false;
	public double stressMin; 
	
	//LAI FILE NAME (stics lai forcing)
	public String laiFileName=""; 			// crop lai data file
	
	// Scene construction
	// the scene is built with pldFileName OR (plotSettings AND initialValues)
	public String pldFileName;

	public SafePlotSettings plotSettings; // plot parameters


	 // default value
	public String dataOriginalPath = PathManager.getInstallDir () + "/data/safe";
	public String dataPath = PathManager.getInstallDir () + "/data/safe";

	private GScene initScene;

	/**
	 * Constructor.
	 */
	public SafeGeneralParameters () throws Exception {
		this.dataPath = PathManager.getInstallDir () + "/data/safe";
		
	}

	/**
	 * Constructor for scripts.
	 */
	public SafeGeneralParameters (String workingDir,   String pldFileName) throws Exception {
		this ();
		this.pldFileName = pldFileName;
		this.dataPath = workingDir;

	}

	public void resetDataPath (String workingDir) {
		this.dataPath = workingDir;
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
