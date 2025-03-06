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

/**
 * PLOT parameters
 *
 * @author Isabelle Lecomte - INRA Montpellier France - July 2002
 */

public class SafePlotSettings extends AbstractSettings {


	private static final long serialVersionUID = 1L;
	
	// PLOT DEFINITION
	public String standInventory;	// name id the pld file
	public double plotLatitude;		// degrees
	public double plotLongitude;	// degrees
	public double plotElevation;	// m
	
	// angle of slope bottom on the compass from the North, clockwise rotation
	// northern  : 0, eastern : 90, southern  : 180, western  : 270
	// public final double BOTTOM_AZIMUT = 270; //azimut of slope bottom from x
	// axis, trigonometric rotation (this value cannot be modified)
	// azimut of vector orthogonal to slope //GT 2007 slope
	public double slopeIntensity;
	public double slopeAspect;

	// angle of tree line on the compass from the North,  clcokwise rotation
	// northern : 0, eastern : 90, southern  : 180, western : 270
	//  vector of tree line is contrary to vector of y axis	
	public double northOrientation;
	public double treeLineOrientation;
	
	public double cellWidth;	// m
	public double plotHeight;	// m 
	public double plotWidth;	// m
	public double cellSurface;  // m2
	public int nbTrees = 0;
	
	// SOIL DEFINITION
	public double humificationDepth;	// equivalent depth of humification (m) between depth of tillage and 0.6 m (P_profhum)
	public double minNh4Concentration;	// minimum soil concentration in NH4 (kgN ha-1 mm-1) (P_concseuil) 
	public double ph;					// P_pH of mixing soil + organic amendments (P_pH) 
	public double organicNitrogen;		// organic nitrogen content (%) in moisture soil horizon (P_Norg) 
	public double albedo;				// albedo of bare soil in dry state (0-1) (P_albedo) 
	public double evaporationValue;		// evaporation value (mm) at the end of maximum evaporation stage (P_q0) 
	public double rainRunOffFraction;	// rainwater run-off (compared with total rainfall) under bare soil conditions (between 0 and 1 ) (P_ruisolnu) 
	public double cropRootObstruction;	// Obstruction to crop roots (m) (P_obstarac) 
	
	public boolean swellingClaySoil;	// option: creation of supplementary compartment in water balance for swelling clay soils (P_codefente) 
	public boolean macroporosity;		// option: water flux in the macroporosity of soils to estimate water excess and drip by  overflowing (P_codemacropor)
	public boolean nitrification;		// option: activate nitrification calculation (P_codenitrif) 
	public boolean denitrification;		// option: allow the calculation of denitrification (P_codedenit)
	
	public boolean capillary;					//option: to activate capillary rise (P_coderemontcap) 
	public double capillaryUptake;				// capillary rise upward water flux (mm d-1) (P_capiljour) 
	public double capillaryUptakeMinWater;		// min water to activate capillary uptake (g water/g soil) (P_humcapil) 
	
	public boolean artificialDrainage;			//option : artificial drainage (P_codrainage) 
	public double impermeableLayerDepth;		// Upper depth of the impermeable layer (from the soil surface) (m) (P_profimper) 
	public double drainagePipesSpacing;			// in between drains distance (m) (P_ecartdrain) 
	public double drainagePipesDepth;			// drain depth (m) (P_profdrain)  
	public double waterConductivity ;			// hydraulic conductivity in the soil above and below the drains (SD) (P_Ksol) 

	public double soilCrustRainMin;  			// minimal rain quantity for the crust occurrence (pluiebat) // mm day-1 
	public double soilCrustDepth;   			// mulch depth from which a crust occurs (mulchbat) // 0 
	public double evaporationMaxDepth;  		// maximal depth of soil affected by soil evaporation (zesx) // cm 
	public double evaporationDepthContribution; // soil contribution to evaporation as a function of depth (cfes)  // SD
	public double roughnessLength;  			// roughness length of bare soil (z0solnu) // cm 
	public double denitrificationRate;   		// potential rate of denitrification per 1 cm soil layer (vpotdenit)// kg ha-1 j-1 cm-1  
	public double denitrificationDepth;  		// soil depth on which denitrification is active with the appropriate option (profdenit) // cm 
	public double soilHumusCN;   				// Initial C to N ratio of soil humus (CsurNsol) // SD 
	public double runOffCoefPlantMulch;    		// runoff coefficient taking account for plant mulch (penterui) // SD  

			
	// LAYERS DEFINITION
	public double[] layerThickness = new double[5];			//m
	public double[] layerClay = new double[5];				//%
	public double[] layerSand = new double[5];				//%
	public double[] layerLimeStone = new double[5];			//%
	public double[] layerOrganicMatter = new double[5];
	public double[] layerPartSizeSand = new double[5];
	public double[] layerStone = new double[5];				//%
	public int[] layerStoneType = new int[5];				//0-9
	public double[] layerInfiltrability = new double[5];

	//   LAYER INIT 
	public double [] layerWaterContent = new double[5];		//m3 m-3
	public double [] layerNo3Content   = new double[5];		//Kg ha-1
	public double [] layerNh4Content   = new double[5];		//Kg ha-1
	
	// VOXELS DEFINITION
	public double voxelThicknessMax;	// m

	// WATER TABLE
	public boolean waterTable = false;

	// NITROGEN REPARTITION PARAMETERS
	public double no3ConcentrationInWaterTable;				
	public double nh4ConcentrationInWaterTable;


	//   TREE SECTION 
	public String [] treeSpecies = new String[100];
	public double [] treeX = new double[100];
	public double [] treeY = new double[100];
}
