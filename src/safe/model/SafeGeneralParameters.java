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
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeGeneralParameters extends AbstractSettings implements InitialParameters {

	// ****  STATIC VALUES *****************************************************************
	/** Max number of trees on the plot */
	public static final int NB_TREE_MAX  = 100;
	/** Max number of pedologic layers */
	public static final int NB_LAYER_MAX = 5; 
	/** Max number of leaf cohorts on evergreen trees */
	public static final int NB_COHORT_MAX  = 10; // 
	/** Soil wilting point default value */
	public static final double PF_WILTING_POINT = 4.2;
	/** Soil wield capacity default value  */
	public static final double PF_FIELD_CAPACITY = 2.5;
	/** STICS soil mini layers numbers  */
	public static final int STICS_MINI_LAYERS = 1000; 	
	
	/** number of diffuse beams if turtle repartition i light module */
	public static final int NB_TURTLE_BEAM = 46; 			// 
	/** turtle beams Azimut in degrees  */
	public static final double[] LIGHT_TURTLE_AZ = {12.23, 59.77, 84.23, 131.77, 156.23, 203.77, 228.23, 275.77, 300.23,
			347.77, 36, 108, 180, 252, 324, 0, 72, 144, 216, 288, 23.27, 48.73, 95.27, 120.73, 167.27, 192.73, 239.27,
			264.73, 311.27, 336.73, 0, 72, 144, 216, 288, 36, 108, 180, 252, 324, 0, 72, 144, 216, 288, 180};
	/** turtle beams Height in degrees  */
	public static final double[] LIGHT_TURTLE_EL = {9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 9.23, 10.81, 10.81,
			10.81, 10.81, 10.81, 26.57, 26.57, 26.57, 26.57, 26.57, 31.08, 31.08, 31.08, 31.08, 31.08, 31.08, 31.08,
			31.08, 31.08, 31.08, 47.41, 47.41, 47.41, 47.41, 47.41, 52.62, 52.62, 52.62, 52.62, 52.62, 69.16, 69.16,
			69.16, 69.16, 69.16, 90};
	/** turtle beams standard overcast sky (unitless)  */
	public static final double[] LIGHT_TURTLE_SOC = {0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043, 0.0043,
			0.0043, 0.0055, 0.0055, 0.0055, 0.0055, 0.0055, 0.014, 0.014, 0.014, 0.014, 0.014, 0.0197, 0.0197, 0.0197,
			0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 0.0197, 0.0336, 0.0336, 0.0336, 0.0336, 0.0336, 0.0399,
			0.0399, 0.0399, 0.0399, 0.0399, 0.0495, 0.0495, 0.0495, 0.0495, 0.0495, 0.0481};
	/** turtle beams uniform overcast sky (unitless)  */
	public static final double[] LIGHT_TURTLE_UOC = {0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007,
			0.0086, 0.0086, 0.0086, 0.0086, 0.0086, 0.017, 0.017, 0.017, 0.017, 0.017, 0.0224, 0.0224, 0.0224, 0.0224,
			0.0224, 0.0224, 0.0224, 0.0224, 0.0224, 0.0224, 0.0317, 0.0317, 0.0317, 0.0317, 0.0317, 0.036, 0.036,
			0.036, 0.036, 0.036, 0.0405, 0.0405, 0.0405, 0.0405, 0.0405, 0.0377};
	/** turtle beams infra-red energy  (m-2 of beam section) */
	public static final double[] LIGHT_TURTLE_IR = {0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069, 0.0069,
			0.0069, 0.0081, 0.0081, 0.0081, 0.0081, 0.0081, 0.0192, 0.0192, 0.0192, 0.0192, 0.0192, 0.0222, 0.0222,
			0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0222, 0.0316, 0.0316, 0.0316, 0.0316, 0.0316,
			0.0342, 0.0342, 0.0342, 0.0342, 0.0342, 0.0402, 0.0402, 0.0402, 0.0402, 0.0402, 0.0430};

	/** Stone type names */
	public static final String[] STONE_NAME = {"Beauce limestone 1", "Beauce limestone 2", "Lutetian limestone",
			"Lutetian Brackish marl and limestone", "Morainic gravels", "Unweathered flint, sandstone or granite", 
			"Weathered granite","Jurassic limestone", "Pebbles from Magneraud", "Other pebbles"};
	/** Stone type volumetric density  (g cm-3) */
	public static final double[] STONE_VOLUMIC_DENSITY = {2.2, 1.8, 2.1, 2.3, 2.5, 2.65, 2.3, 2.2, 1.5, 0}; 
	/** Stone type water content  (% ponderal) */
	public static final double[] STONE_WATER_CONTENT = {0.07, 0.16, 0.11, 0.05, 0.03, 0.01, 0.05, 0.05, 0.26, 0.0}; 

	/** Conversion factor volume m3 to cm3 */
	public static final double M3_TO_CM3 = 1000000; 
	/**  Conversion factor density mm-3 to cm cm-3 */
	public static final double MM3_TO_CMCM3 = 10000; 

	
	//********** READ in GENERAL PARAMETER FILE************************************************************
	// this is related to the location of the plot
	/** Coefficients A of the relationship : Diffuse/Global = A - B Global/G0 */
	public  double diffuseCoeffA;
	/** Coefficients B of the relationship : Diffuse/Global = A - B Global/G0 */
	public  double diffuseCoeffB;
	/** Coefficient to convert GLOBAL radiation to PAR  */
	public  double parGlobalCoefficient;
	/** Coefficient to convert Moles to MJ for PAR radiation
	 *  Approximation by photosyn assistant (Dundee Scientific Ltd) */
	public  double molesParCoefficient; 
	/** Angstrom coefficients for calculating insolation */
	public  double aangst;
	/** Angstrom coefficients for calculating insolation */
	public  double bangst;

	
	// LIGHT MODULE 
	/**  Time between two calculations of the sun position (hours) */
	public double timeStep;
	/**  Maximal number of calculations */
	public int nbTimeStepMax;	
	/**  Standard overcast sky option (Yes/No) */
	public boolean SOC;		
	/**  uniform overcast sky option (Yes/No) */
	public boolean UOC;		
	/**  Turtle beam repartition option   (Yes/No) */
	public boolean turtleOption;	
	/**  Angle step between two beams  (degree) */
	public double diffuseAngleStep;
	/**Threshold change in sun declination that triggers a recalculation of the light module */
	public double declinationThreshold;		
	/**Threshold change in tree leaf area volume that triggers a recalculation of the light module */
	public double leafAreaThreshold;	
	/** Number of beam impact traced to each cell */
	public int nbImpactMultiplication;		 
	/** light interception method used for crop 0=stics 1=Hisafe */
	public boolean hisafeLightMethodForCrop;		
	/** Collection of beam impact of each cell  */
	public Collection<Vertex3d> cellImpacts;	// 
	
	//Microclimate
	/** Priestley Taylor coefficient */
	public  double priestleyTaylorCoeff;
	/** Stefan-Boltzman constant (W m-2 T-4) */
	public  double stefanBoltzmanConstant; 
	/** Psychrometric constant (mbar/degreeC) */
	public  double psychrometricConstant ; 
	/** Parameter for calculation of pedotransfert integral of conductivity (Phi_pF)   */
	public  double integrationStep;
	/** Parameter for calculation of pedotransfert integral of conductivity (Phi_pF)  */
	public  double maxPhiPF;
	
	//Water module
	/** water extraction method for pure crop 0=hisafe 1=stics */
	public boolean sticsWaterExtractionForCrop = false;
	/** Minimum value for water stress */
	public double waterStressMin;
	/** Minimum value for nitrogen stress */
	public double nitrogenStressMin;
	
	//Nitrogen module
	/** Parameter for Nitrogen diffusion  (cm2 day-1) */
	public double nitrogenDiffusionConstant;	
	/** Parameter for Nitrogen diffusion  (unitless) */
	public double nitrogenEffectiveDiffusionA0;	
	/** Parameter for Nitrogen diffusion  (unitless) */
	public double nitrogenEffectiveDiffusionA1;	
	/** Parameter for Nitrogen absorption (unitless) */
	public double no3AbsorptionConstant;			
	/** Parameter for Nitrogen absorption (unitless) */
	public double nh4AbsorptionConstant;			
	/** Parameter for Nitrogen absorption (unitless) */
	public double no3Fraction;	

	
	// Snow module - CD-06-10-2017 
	/** Rain become snow when max temperature of the day is below this value (degree) */
	public  double maxTempSnow;		
	/** Rain become snow when mean temperature of the day is below this value (degree) */
	public  double minTempSnow ;		
	/** Max amount of snow that can melt each day (mm)  */
	public  double maxDailySnowMelt;	
	/** Snow melt is max when mean temperature of the day is above this value (degree)*/
	public  double maxTempSnowMelt;	//° 
	/** Snow melt does not occur when mean temperature of the day is below this value (degree) */
	public  double minTempSnowMelt;	   
		

	//mineralization
	/** default spreading zone for leaves residues (1=Under the tree crown 2=all over the plot) */
	public int leavesResiduesSpreading;			
	/** default spreading zone for branches residues (1=Under the tree crown 2=all over the plot) */
	public int branchesResiduesSpreading;			
	/** Relative potential mineralization rate in deep mineralization module */
	public double fmin1;
	/** Clay effect on the potential mineralization rate in deep mineralization module */
	public double fmin2;
	/** CaCO3 effect on the potential mineralization rate in deep mineralization */
	public double fmin3;

	/** Reference to the GScene object : initial scene */
	private GScene initScene;

	/** Default path to the default data folder */
	public String dataDefaultPath = PathManager.getInstallDir () + "/data/safe";
	
	/** Default path to the specific data folder */
	public String dataPath = PathManager.getInstallDir () + "/data/safe";

	/** Name of the pld file (plot definition) */
	public String pldFileName;

				  

	/**
	 * Constructor.
	 */
	public SafeGeneralParameters () throws Exception {
		this.dataPath = PathManager.getInstallDir () + "/data/safe";
		
	}

	/**
	 * Constructor for scripts.
	 * @param dataPath the path for data entry files 
	 * @param pldFileName the Name of the pld file name (plot definition) 
	 */
	public SafeGeneralParameters (String dataPath,   String pldFileName) throws Exception {
		this ();
		this.pldFileName = pldFileName;
		this.dataPath = dataPath;

	}
	/**
	 * Set the data path
	 * @param dataPath the path for data entry files 
	 */
	public void setDataPath (String dataPath) {
		this.dataPath = dataPath;
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
