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

package safe.extension.ioformat;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

// This extension is for safe model.
// It may become usable by other models: implement other constructors
// ex: public SafeInventory (MobyDickStand stand) throws Exception {
// with: import mobydick.model.*;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;
import safe.model.SafeGeneralParameters;
import safe.model.SafeModel;
import safe.model.SafePlotSettings;
import safe.model.SafeStand;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.StandRecordSet;

/**
 * SafeInventory contains records description for safe input/output inventory file.
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafeInventory extends StandRecordSet implements OFormat {

	private static final long serialVersionUID = 1L;

	static {
		Translator.addBundle ("safe.extension.ioformat.SafeInventory");
	}


	// Generic keyword record is described in superclass: key = value

	// Safe layer record is described here
	@Import
	static public class LayerRecord extends Record {
		private static final long serialVersionUID = 1L;
		
		public LayerRecord () {
			super ();
		}

		public LayerRecord (String line) throws Exception {
			super (line);
		}

		// public String getSeparator () {return ";";} // to change default "\t" separator
		public String name;
		public double thickness; // m
		public double sandPercent; // %
		public double clayPercent; // %
		public double limeStonePercent; // %
		public double organicMatterPercent; // %
		public double partSizeSand; // ï¿½m
		public double stone; // %
		public int stoneType; // 1-10
		public double infiltrability; // mm j-1

	}

	// Safe layer initial values record is described here
	@Import
	static public class LayerTreeRecord extends Record {
		private static final long serialVersionUID = 1L;
		public LayerTreeRecord () {
			super ();
		}

		public LayerTreeRecord (String line) throws Exception {
			super (line);
		}

		// public String getSeparator () {return ";";} // to change default "\t" separator
		public String name;
		public String z1; 
		public double z2; 
		public double z3; 
	}


	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if the extension can
	 * deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) { return false; }
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof SafeStand)) { return false; }

		} catch (Exception e) {
			Log.println (Log.ERROR, "SafeInventory.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("SafeInventory.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("SafeInventory.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-22.08.2018
	//public static final String VERSION = "1.0";

	//
	// RecordSet -> File
	// is described in superclass (save (fileName)).
	//

	/**
	 * Phantom constructor. Only to ask for extension properties (authorName, version...).
	 */
	public SafeInventory () {}

	/**
	 * File -> RecordSet is delegated to superclass.
	 */
	// public SafeInventory (String fileName) throws Exception {super (fileName);}

	public SafeInventory (String fileName) throws Exception {
		prepareImport (fileName);
	} // for direct use for Import

	public SafeInventory (SafeGeneralParameters settings, SafePlotSettings plotSettings)
			throws Exception {
		createRecordSet (settings, plotSettings);
	} // for direct use for Export

	public void createRecordSet (SafeGeneralParameters settings, SafePlotSettings plotSettings)
			throws Exception {

		
	}

	/**
	 * RecordSet -> SafeCreatePlotSettings Implementation here. Was initialy described in
	 * SafeModel.loadInitStand () To load a stand for another model, recognize real type of model :
	 * if (model instanceof SafeModel) -> this code if (model instanceof MobyDickModel) -> other
	 * code...
	 */
	public GScene load (GModel model) throws Exception {
		return load (model, (SafeGeneralParameters) model.getSettings ());
	}
	
	public GScene load (GModel model, SafeGeneralParameters safeSettings) throws Exception {


		SafeModel m = (SafeModel) model;
	
		safeSettings.plotSettings = new SafePlotSettings ();

		SafePlotSettings plotSettings = safeSettings.plotSettings;


		int nbLayerMax = safeSettings.NB_LAYER_MAX;
		int nbTreeMax = safeSettings.NB_TREE_MAX;
		int nbLayer = 0;
		int treeId = 0;
		int layerId = 0;
		int layerInitId = 0;

		Set<String> requiredParameters = new HashSet<>();
		requiredParameters.add("LAYER");
		requiredParameters.add("LAYERINIT");
		requiredParameters.add("latitude");
		requiredParameters.add("elevation");
		requiredParameters.add("cellWidth");
		requiredParameters.add("northOrientation");
		requiredParameters.add("plotHeight");		
		requiredParameters.add("plotWidth");
		requiredParameters.add("slopeIntensity");		
		requiredParameters.add("slopeAspect");
		requiredParameters.add("waterTable");
		requiredParameters.add("no3ConcentrationInWaterTable");
		requiredParameters.add("nh4ConcentrationInWaterTable");		
		requiredParameters.add("voxelThicknessMax");
		requiredParameters.add("humificationDepth");
		requiredParameters.add("organicNitrogen");
		requiredParameters.add("albedo");
		requiredParameters.add("evaporationValue");
		requiredParameters.add("rainRunOffFraction");
		requiredParameters.add("cropRootObstruction");
		requiredParameters.add("ph");
		requiredParameters.add("capillary");
		requiredParameters.add("capillaryUptake");
		requiredParameters.add("capillaryUptakeMinWater");
		requiredParameters.add("artificialDrainage");
		requiredParameters.add("impermeableLayerDepth");
		requiredParameters.add("drainagePipesSpacing");
		requiredParameters.add("drainagePipesDepth");
		requiredParameters.add("waterConductivity");
		requiredParameters.add("swellingClaySoil");
		requiredParameters.add("nitrification");
		requiredParameters.add("denitrification");
		requiredParameters.add("macroporosity");
		requiredParameters.add("soilCrustRainMin");
		requiredParameters.add("soilCrustDepth");    
		requiredParameters.add("evaporationMaxDepth");     
		requiredParameters.add("evaporationDepthContribution");
		requiredParameters.add("roughnessLength"); 
		requiredParameters.add("soilHumusCN");
		requiredParameters.add("runOffCoefPlantMulch"); 
		requiredParameters.add("denitrificationDepth");
		requiredParameters.add("denitrificationRate");
		
		
		
		// RAZ tables with default values

		for (int l = 0; l < nbLayerMax; l++) {
			plotSettings.layerThickness[l] = 0;
			plotSettings.layerClay[l] = 0;
			plotSettings.layerSand[l] = 0;
			plotSettings.layerLimeStone[l] = 0;
			plotSettings.layerOrganicMatter[l] = 0;
			plotSettings.layerPartSizeSand[l] = 0;
			plotSettings.layerStone[l] = 0;
			plotSettings.layerStoneType[l] = 0;
			plotSettings.layerInfiltrability[l] = 0;
		}



		
		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();

			// Layer Record
			if (record instanceof SafeInventory.LayerRecord) {

				if (nbLayer==0) requiredParameters.remove("LAYER");
				SafeInventory.LayerRecord r = (SafeInventory.LayerRecord) record; 
			
				nbLayer++;
				if ((layerId > (nbLayerMax - 1)) || (nbLayer > nbLayerMax)) {
					System.out.println("LAYER More than " + nbLayerMax + " layers !"); 
					throw new CancellationException();	// abort
				}
			

				safeSettings.nbLayers = nbLayer;
				plotSettings.layerThickness[layerId] = r.thickness;
				plotSettings.layerClay[layerId] = r.clayPercent;
				plotSettings.layerSand[layerId] = r.sandPercent;
				plotSettings.layerLimeStone[layerId] = r.limeStonePercent;
				plotSettings.layerOrganicMatter[layerId] = r.organicMatterPercent;
				plotSettings.layerPartSizeSand[layerId] = r.partSizeSand;
				plotSettings.layerStone[layerId] = r.stone;
				plotSettings.layerStoneType[layerId] = r.stoneType;
				plotSettings.layerInfiltrability[layerId] = r.infiltrability;

				layerId++;

			} 

			else if (record instanceof SafeInventory.LayerTreeRecord) {

				SafeInventory.LayerTreeRecord r = (SafeInventory.LayerTreeRecord) record; 
				if (r.name.equals ("LAYERINIT")){	// Layer initial Record
					
					// initialisation first time only
					if (layerInitId == 0) {
						requiredParameters.remove("LAYERINIT");
						for (int l = 0; l < nbLayerMax; l++) {
							plotSettings.layerWaterContent[l] = 0;
							plotSettings.layerNo3Content[l] = 0;
							plotSettings.layerNh4Content[l] = 0;
						}
					}
					double waterContent = Double.parseDouble(r.z1);
					double  no3Content = r.z2;
					double  nh4Content = r.z3;

					if (layerInitId > (nbLayerMax - 1)) {
						System.out.println ("LAYERINIT More than " + nbLayerMax + " layers !"); 
						throw new CancellationException();	// abort
					}
					plotSettings.layerWaterContent[layerInitId] = waterContent;
					plotSettings.layerNo3Content[layerInitId] = no3Content;
					plotSettings.layerNh4Content[layerInitId] = nh4Content;

					layerInitId++;
				}
				else if (r.name.equals ("TREE")){ // Tree Record
					
					if (treeId > (nbTreeMax - 1)) throw new Exception ("More than " + nbTreeMax + " trees !"); 
					
					// First tree and all trees on his right
					String treeSpecies = r.z1;
					plotSettings.treeSpecies[treeId] = treeSpecies;

					double x = (double) r.z2;
					double y = (double) r.z3;


					if ((x==0) && (y==0)) {	
						plotSettings.treeX[treeId] = plotSettings.plotWidth/2;
						plotSettings.treeY[treeId] = plotSettings.plotHeight/2;				
					}
					else {
						plotSettings.treeX[treeId] = x;
						plotSettings.treeY[treeId] = y;
					}
					
					if (plotSettings.treeX[treeId] > plotSettings.plotWidth) {
						System.out.println("Tree X : "+plotSettings.treeX[treeId]+" is out of plot width : " + plotSettings.plotWidth);
						throw new CancellationException();	// abort
					}
					if (plotSettings.treeY[treeId] > plotSettings.plotHeight) {
						System.out.println("Tree Y : "+plotSettings.treeY[treeId]+" is out of plot height : " + plotSettings.plotHeight);
						throw new CancellationException();	// abort
					}	
					
					
					if (((plotSettings.treeX[treeId]-(plotSettings.cellWidth/2))%(plotSettings.cellWidth) != 0)|| ((plotSettings.treeY[treeId]-(plotSettings.cellWidth/2))%(plotSettings.cellWidth) != 0)) {
						System.out.println("Tree coordinate are not compatible with plot dimension or cell width.");
						throw new CancellationException();	// abort
					}
					
					
					treeId++;
					plotSettings.nbTrees = treeId;
				}

			} // KEY Records
			else if (record instanceof SafeInventory.KeyRecord) {

				SafeInventory.KeyRecord r = (SafeInventory.KeyRecord) record; // cast to precise
																				// type
				
				if (r.key.equals ("latitude")) {
					plotSettings.plotLatitude = r.getDoubleValue ();
					requiredParameters.remove("latitude");
					
				} else if (r.key.equals ("elevation")) {
					plotSettings.plotElevation = r.getDoubleValue ();
					requiredParameters.remove("elevation");
					
				} else if (r.key.equals ("slopeIntensity")) {
					plotSettings.slopeIntensity = r.getDoubleValue ();
					requiredParameters.remove("slopeIntensity");
					
				} else if (r.key.equals ("slopeAspect")) {
					plotSettings.slopeAspect = r.getDoubleValue ();
					requiredParameters.remove("slopeAspect");
					
				} else if (r.key.equals ("northOrientation")) {
					plotSettings.northOrientation = r.getDoubleValue ();
					plotSettings.treeLineOrientation 	= 180 - plotSettings.northOrientation;
					if (plotSettings.treeLineOrientation < 0) plotSettings.treeLineOrientation+= 360;
					requiredParameters.remove("northOrientation");
					
				} else if (r.key.equals ("cellWidth")) {
					plotSettings.cellWidth = r.getDoubleValue ();
					requiredParameters.remove("cellWidth");
					
				} else if (r.key.equals ("plotWidth")) {
					plotSettings.plotWidth = r.getDoubleValue ();
					requiredParameters.remove("plotWidth");
					
				} else if (r.key.equals ("plotHeight")) {
					plotSettings.plotHeight = r.getDoubleValue ();
					requiredParameters.remove("plotHeight");
					
				} else if (r.key.equals ("voxelThicknessMax")) {
					plotSettings.voxelThicknessMax = r.getDoubleValue ();
					requiredParameters.remove("voxelThicknessMax");
					
				} else if (r.key.equals ("waterTable")) {
					
					int b = r.getIntValue();
					plotSettings.waterTable = false;
					if (b==0) plotSettings.waterTable = false;
					if (b==1) plotSettings.waterTable = true;
					requiredParameters.remove("waterTable");
					

				} else if (r.key.equals ("humificationDepth")) {
					plotSettings.humificationDepth = r.getDoubleValue ();
					requiredParameters.remove("humificationDepth");
					
				} else if (r.key.equals ("organicNitrogen")) {
					plotSettings.organicNitrogen = r.getDoubleValue ();
					requiredParameters.remove("organicNitrogen");
					
				} else if (r.key.equals ("albedo")) {
					plotSettings.albedo = r.getDoubleValue ();
					requiredParameters.remove("albedo");
					
				} else if (r.key.equals ("evaporationValue")) {
					plotSettings.evaporationValue = r.getDoubleValue ();
					requiredParameters.remove("evaporationValue");
					
				} else if (r.key.equals ("rainRunOffFraction")) {
					plotSettings.rainRunOffFraction = r.getDoubleValue ();
					requiredParameters.remove("rainRunOffFraction");
					
				} else if (r.key.equals ("cropRootObstruction")) {
					plotSettings.cropRootObstruction = r.getDoubleValue ();
					requiredParameters.remove("cropRootObstruction");
					
				} else if (r.key.equals ("minNh4Concentration")) {
					plotSettings.minNh4Concentration = r.getDoubleValue ();
					requiredParameters.remove("minNh4Concentration");
					
				} else if (r.key.equals ("ph")) {
					plotSettings.ph = r.getDoubleValue ();
					requiredParameters.remove("ph");
					
				} else if (r.key.equals ("capillary")) {
					
					int b = r.getIntValue();
					plotSettings.capillary = false;
					if (b==0) plotSettings.capillary = false;
					if (b==1) plotSettings.capillary = true;
					requiredParameters.remove("capillary");
					

				} else if (r.key.equals ("capillaryUptake")) {
					plotSettings.capillaryUptake = r.getDoubleValue ();
					requiredParameters.remove("capillaryUptake");
					
				} else if (r.key.equals ("capillaryUptakeMinWater")) {
					plotSettings.capillaryUptakeMinWater = r.getDoubleValue ();
					requiredParameters.remove("capillaryUptakeMinWater");
					
				} else if (r.key.equals ("artificialDrainage")) {
					
					int b = r.getIntValue();
					plotSettings.artificialDrainage = false;
					if (b==0) plotSettings.artificialDrainage = false;
					if (b==1) plotSettings.artificialDrainage = true;
					requiredParameters.remove("artificialDrainage");
					

				} else if (r.key.equals ("impermeableLayerDepth")) {
					plotSettings.impermeableLayerDepth = r.getDoubleValue ();
					requiredParameters.remove("impermeableLayerDepth");
					
				} else if (r.key.equals ("drainagePipesSpacing")) {
					plotSettings.drainagePipesSpacing = r.getDoubleValue ();
					requiredParameters.remove("drainagePipesSpacing");
					
				} else if (r.key.equals ("drainagePipesDepth")) {
					plotSettings.drainagePipesDepth = r.getDoubleValue ();
					requiredParameters.remove("drainagePipesDepth");
					
				} else if (r.key.equals ("waterConductivity")) {
					plotSettings.waterConductivity = r.getDoubleValue ();
					requiredParameters.remove("waterConductivity");
					
				} else if (r.key.equals ("swellingClaySoil")) {
					
					int b = r.getIntValue();
					plotSettings.swellingClaySoil = false;
					if (b==0) plotSettings.swellingClaySoil = false;
					if (b==1) plotSettings.swellingClaySoil = true;
					requiredParameters.remove("swellingClaySoil");
					

				} else if (r.key.equals ("macroporosity")) {
					
					int b = r.getIntValue();
					plotSettings.macroporosity = false;
					if (b==0) plotSettings.macroporosity = false;
					if (b==1) plotSettings.macroporosity = true;
					requiredParameters.remove("macroporosity");
					

				} else if (r.key.equals ("nitrification")) {
					
					int b = r.getIntValue();
					plotSettings.nitrification = false;
					if (b==0) plotSettings.nitrification = false;
					if (b==1) plotSettings.nitrification = true;
					requiredParameters.remove("nitrification");
					
					
				} else if (r.key.equals ("denitrification")) {
					
					int b = r.getIntValue();
					plotSettings.denitrification = false;
					if (b==0) plotSettings.denitrification = false;
					if (b==1) plotSettings.denitrification = true;
					requiredParameters.remove("denitrification");
					
				
				} else if (r.key.equals ("soilCrustRainMin")) {
					plotSettings.soilCrustRainMin = r.getDoubleValue ();
					requiredParameters.remove("soilCrustRainMin");
					
				} else if (r.key.equals ("soilCrustDepth")) {
					plotSettings.soilCrustDepth = r.getDoubleValue ();
					requiredParameters.remove("soilCrustDepth");
					
				} else if (r.key.equals ("evaporationMaxDepth")) {
					plotSettings.evaporationMaxDepth = r.getDoubleValue ();
					requiredParameters.remove("evaporationMaxDepth");
					
				} else if (r.key.equals ("evaporationDepthContribution")) {
					plotSettings.evaporationDepthContribution = r.getDoubleValue ();
					requiredParameters.remove("evaporationDepthContribution");
					
				} else if (r.key.equals ("roughnessLength")) {
					plotSettings.roughnessLength = r.getDoubleValue ();
					requiredParameters.remove("roughnessLength");
					
				} else if (r.key.equals ("denitrificationRate")) {
					plotSettings.denitrificationRate = r.getDoubleValue ();	
					requiredParameters.remove("denitrificationRate");
					
				} else if (r.key.equals ("denitrificationDepth")) {
					plotSettings.denitrificationDepth = r.getDoubleValue ();
					requiredParameters.remove("denitrificationDepth");
					
				} else if (r.key.equals ("soilHumusCN")) {
					plotSettings.soilHumusCN = r.getDoubleValue ();
					requiredParameters.remove("soilHumusCN");
					
				} else if (r.key.equals ("runOffCoefPlantMulch")) {
					plotSettings.runOffCoefPlantMulch = r.getDoubleValue ();
					requiredParameters.remove("runOffCoefPlantMulch");
					
				
				} else if (r.key.equals ("no3ConcentrationInWaterTable")) {
					plotSettings.no3ConcentrationInWaterTable = r.getDoubleValue ();
					requiredParameters.remove("no3ConcentrationInWaterTable");
					
				} else if (r.key.equals ("nh4ConcentrationInWaterTable")) {
					plotSettings.nh4ConcentrationInWaterTable = r.getDoubleValue ();
					requiredParameters.remove("nh4ConcentrationInWaterTable");
					
									
				}

			} else {
				System.out.println ("Unrecognized record : " + record); // automatic toString ()
				throw new CancellationException();	// abort
																			// (or null)
			}
		}

		
		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing plot parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort

		}
		
		//plot dimension verification
		if ((plotSettings.plotWidth%plotSettings.cellWidth != 0) || (plotSettings.plotHeight%plotSettings.cellWidth != 0)) {
			System.out.println("Plot dimensions are not compatible with cell width.");
			throw new CancellationException();	// abort
		};
		

		//layer number 
		if (layerId!=layerInitId){
			System.out.println("Number of layers and number of layersInit are not the same !");
			throw new CancellationException();	// abort
		}
		
		double latitude = plotSettings.plotLatitude;
		double elevation = plotSettings.plotElevation;

		SafeStand initStand = new SafeStand (latitude, elevation);
		return initStand;
	}

	// fc-7.12.2020 Deprecated
//	// //////////////////////////////////////////////// IOFormat stuff
//	public boolean isImport () {
//		return true;
//	}
//
//	public boolean isExport () {
//		return false;
//	}

}
