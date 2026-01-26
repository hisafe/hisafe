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

package safe.extension.ioformat;

import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Import;
import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Translator;
import safe.model.SafeGeneralParameters;
import safe.model.SafeLayer;
import safe.model.SafeModel;
import safe.model.SafePlot;
import safe.model.SafePlotSettings;
import safe.model.SafeStand;
import safe.model.SafeSoil;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.StandRecordSet;

/**
 * SafeInventory contains records description for safe input plot file.
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafeInventory extends StandRecordSet implements OFormat {

	static {
		Translator.addBundle ("safe.extension.ioformat.SafeInventory");
	}


	/** Soil layer description */
	@Import
	static public class LayerRecord extends Record {

		public LayerRecord () {super ();}

		public LayerRecord (String line) throws Exception {super (line);}

		/** record name = LAYER */
		public String name;
		/** Soil layer thickness (m) */
		public double thickness;
		/** Percentage of sand (%)   */
		public double sandPercent; 
		/** Percentage of clay (%)  */
		public double clayPercent; 
		/**  Percentage of limestone (%)  */
		public double limeStonePercent; 
		/**  Percentage of organic matter (%)  */
		public double organicMatterPercent; 
		/**  Particle size of sand  (micrometers) */
		public double partSizeSand; 
		/**  Percentage of stones  (%) */
		public double stone;
		/**  Stones type 1=limestone B1, 2=limestone B2, 3=limestone L, 4=scree L, 5=gravel m, 6=flint, 7=granite a, 8=limestone J, 9=other1, 10=other2 */
		public int stoneType;
		/**  Infiltrability rate at the base of the layer (mm day-1) */
		public double infiltrability; 
	}

	/** Soil layer initialization of tree position */
	@Import
	static public class LayerTreeRecord extends Record {
		private static final long serialVersionUID = 1L;
		public LayerTreeRecord () {super ();}

		public LayerTreeRecord (String line) throws Exception {super (line);}

		/** record name = LAYERINIT or TREE */
		public String name;
		/** First record values */
		public String z1; 
		/** Second record values  */
		public double z2; 
		/** third record values  */
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
	 * Return the interface name.
	 */
	@Override
	public String getName () {return Translator.swap ("SafeInventory.name");}

	/**
	 * Return the interface author.
	 */
	@Override
	public String getAuthor () {return "Isabelle LECOMTE";}

	/**
	 * Return the interface description.
	 */
	@Override
	public String getDescription () {return Translator.swap ("SafeInventory.description");}

	/**
	 *  Return the interface version.
	 */
	@Override
	public String getVersion () {return "4.3";}


	/**
	 * Phantom constructor. Only to ask for extension properties (authorName, version...).
	 */
	public SafeInventory () {}

	/**
	 * Constructor
	 * @param plotFileName Name of the plot entry file
	 */

	public SafeInventory (String plotFileName) throws Exception {prepareImport (plotFileName);} 

	/**
	 * Load safe inventory (plot layers trees)
	 * @param model Reference to GModel object
	 * @return the stand with plot and soil 
	 */
	public GScene load (GModel model) throws Exception {
		return load (model, (SafeGeneralParameters) model.getSettings ());
	}
	
	/**
	 * Load safe inventory (plot layers trees)
	 * @param model Reference to GModel object
	 * @param generalParameters Reference to SafeGeneralParameters object
	 * @return the stand with plot and soil 
	 */
	
	public GScene load (GModel model, SafeGeneralParameters generalParameters) throws Exception {

		SafeModel m = (SafeModel) model;
	
		SafePlotSettings plotSettings = new SafePlotSettings ();

		SafeSoil initSoil = new SafeSoil ();
		
		int nbLayerMax = SafeGeneralParameters.NB_LAYER_MAX;
		int nbTreeMax = SafeGeneralParameters.NB_TREE_MAX;
		int nbLayers = 0;
		int nbVoxels = 0;
		int treeId = 0;
		int layerId = 0;
		int layerInitId = 0;
		double surfaceDepth = 0;

		SafeLayer[] soilLayers = new SafeLayer [nbLayerMax];
		
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
		requiredParameters.add("minNh4Concentration");
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

		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();

			
			// Layer Record
			if (record instanceof SafeInventory.LayerRecord) {

				if (nbLayers==0) requiredParameters.remove("LAYER");
				SafeInventory.LayerRecord r = (SafeInventory.LayerRecord) record; 
			
				nbLayers++;
				if ((layerId > (nbLayerMax - 1)) || (nbLayers > nbLayerMax)) {
					System.out.println("LAYER More than " + nbLayerMax + " layers !"); 
					throw new CancellationException();	// abort
				}
			
				if (r.thickness > 0) {

					SafeLayer layer = new SafeLayer(layerId, surfaceDepth, r.thickness,
							r.sandPercent, r.clayPercent, r.limeStonePercent, r.organicMatterPercent,
							r.partSizeSand, r.stone, r.stoneType, r.infiltrability,
							initSoil,
							generalParameters);

					soilLayers[layerId] = layer;
					layerId++;

					initSoil.addDepth (r.thickness);	//cumulation of total soil depth
					double volume = initSoil.getDepth() * plotSettings.plotWidth * plotSettings.plotHeight;
					initSoil.setVolume (volume);
					surfaceDepth += r.thickness;

					//to avoid some problem with double numbers IL 22/02/2021
					surfaceDepth =  (Math.round (surfaceDepth * Math.pow (10,2)) ) / (Math.pow (10,2));

					//nb  voxel calculation depending on layer thickness and voxelThicknessMax
					if (r.thickness <= initSoil.getVoxelThicknessMax()) {
						nbVoxels++; 
					}
					else {
						//to avoid some problem with double numbers IL 22/02/2021
						int nbVoxelLayer = (int)(r.thickness*10/initSoil.getVoxelThicknessMax()*10)/100;
						int lt = (int) (r.thickness*1000);
						int vt = (int) (initSoil.getVoxelThicknessMax()*1000);
						if (lt%vt>0) nbVoxelLayer++;
						nbVoxels += nbVoxelLayer;
					}
				}
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
					plotSettings.latitude = r.getDoubleValue ();
					requiredParameters.remove("latitude");
					
				} else if (r.key.equals ("elevation")) {
					plotSettings.elevation = r.getDoubleValue ();
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
					plotSettings.cellSurface = plotSettings.cellWidth * plotSettings.cellWidth;
					requiredParameters.remove("cellWidth");
					
				} else if (r.key.equals ("plotWidth")) {
					plotSettings.plotWidth = r.getDoubleValue ();
					requiredParameters.remove("plotWidth");
					
				} else if (r.key.equals ("plotHeight")) {
					plotSettings.plotHeight = r.getDoubleValue ();
					requiredParameters.remove("plotHeight");
					
				} else if (r.key.equals ("voxelThicknessMax")) {
					initSoil.setVoxelThicknessMax (r.getDoubleValue ());
					requiredParameters.remove("voxelThicknessMax");
					
				} else if (r.key.equals ("waterTable")) {
					
					int b = r.getIntValue();
					initSoil.setWaterTable (false);
					if (b==0) initSoil.setWaterTable (false);
					if (b==1) initSoil.setWaterTable (true);
					requiredParameters.remove("waterTable");
					

				} else if (r.key.equals ("humificationDepth")) {
					initSoil.setHumificationDepth(r.getDoubleValue ());
					requiredParameters.remove("humificationDepth");
					
				} else if (r.key.equals ("organicNitrogen")) {
					initSoil.setOrganicNitrogen(r.getDoubleValue ());
					requiredParameters.remove("organicNitrogen");
					
				} else if (r.key.equals ("albedo")) {
					initSoil.setAlbedo(r.getDoubleValue ());
					requiredParameters.remove("albedo");
					
				} else if (r.key.equals ("evaporationValue")) {
					initSoil.setEvaporationValue(r.getDoubleValue ());
					requiredParameters.remove("evaporationValue");
					
				} else if (r.key.equals ("rainRunOffFraction")) {
					initSoil.setRainRunOffFraction(r.getDoubleValue ());
					requiredParameters.remove("rainRunOffFraction");
					
				} else if (r.key.equals ("cropRootObstruction")) {
					initSoil.setCropRootObstruction (r.getDoubleValue ());
					requiredParameters.remove("cropRootObstruction");
					
				} else if (r.key.equals ("minNh4Concentration")) {
					initSoil.setMinNh4Concentration (r.getDoubleValue ());
					requiredParameters.remove("minNh4Concentration");
					
				} else if (r.key.equals ("ph")) {
					initSoil.setPh (r.getDoubleValue ());
					requiredParameters.remove("ph");
					
				} else if (r.key.equals ("capillary")) {
					
					int b = r.getIntValue();
					initSoil.setCapillary (false);
					if (b==0) initSoil.setCapillary (false);
					if (b==1) initSoil.setCapillary (true);
					requiredParameters.remove("capillary");
					

				} else if (r.key.equals ("capillaryUptake")) {
					initSoil.setCapillaryUptake (r.getDoubleValue ());
					requiredParameters.remove("capillaryUptake");
					
				} else if (r.key.equals ("capillaryUptakeMinWater")) {
					initSoil.setCapillaryUptakeMinWater (r.getDoubleValue ());
					requiredParameters.remove("capillaryUptakeMinWater");
					
				} else if (r.key.equals ("artificialDrainage")) {
					
					int b = r.getIntValue();
					initSoil.setArtificialDrainage (false);
					if (b==0) initSoil.setArtificialDrainage (false);
					if (b==1) initSoil.setArtificialDrainage (true);
					requiredParameters.remove("artificialDrainage");
					

				} else if (r.key.equals ("impermeableLayerDepth")) {
					initSoil.setImpermeableLayerDepth (r.getDoubleValue ());
					requiredParameters.remove("impermeableLayerDepth");
					
				} else if (r.key.equals ("drainagePipesSpacing")) {
					initSoil.setDrainagePipesSpacing (r.getDoubleValue ());
					requiredParameters.remove("drainagePipesSpacing");
					
				} else if (r.key.equals ("drainagePipesDepth")) {
					initSoil.setDrainagePipesDepth (r.getDoubleValue ());
					requiredParameters.remove("drainagePipesDepth");
					
				} else if (r.key.equals ("waterConductivity")) {
					initSoil.setWaterConductivity (r.getDoubleValue ());
					requiredParameters.remove("waterConductivity");
					
				} else if (r.key.equals ("swellingClaySoil")) {
					
					int b = r.getIntValue();
					initSoil.setSwellingClaySoil (false);
					if (b==0) initSoil.setSwellingClaySoil (false);
					if (b==1) initSoil.setSwellingClaySoil (true);
					requiredParameters.remove("swellingClaySoil");
					

				} else if (r.key.equals ("macroporosity")) {
					
					int b = r.getIntValue();
					initSoil.setMacroporosity (false);
					if (b==0) initSoil.setMacroporosity (false);
					if (b==1) initSoil.setMacroporosity (true);
					requiredParameters.remove("macroporosity");
					

				} else if (r.key.equals ("nitrification")) {
					
					int b = r.getIntValue();
					initSoil.setNitrification (false);
					if (b==0) initSoil.setNitrification (false);
					if (b==1) initSoil.setNitrification (true);
					requiredParameters.remove("nitrification");
					
					
				} else if (r.key.equals ("denitrification")) {
					
					int b = r.getIntValue();
					initSoil.setDenitrification (false);
					if (b==0) initSoil.setDenitrification (false);
					if (b==1) initSoil.setDenitrification (true);
					requiredParameters.remove("denitrification");
					
				
				} else if (r.key.equals ("soilCrustRainMin")) {
					initSoil.setSoilCrustRainMin(r.getDoubleValue ());
					requiredParameters.remove("soilCrustRainMin");
					
				} else if (r.key.equals ("soilCrustDepth")) {
					initSoil.setSoilCrustDepth (r.getDoubleValue ());
					requiredParameters.remove("soilCrustDepth");
					
				} else if (r.key.equals ("evaporationMaxDepth")) {
					initSoil.setEvaporationMaxDepth (r.getDoubleValue ());
					requiredParameters.remove("evaporationMaxDepth");
					
				} else if (r.key.equals ("evaporationDepthContribution")) {
					initSoil.setEvaporationDepthContribution (r.getDoubleValue ());
					requiredParameters.remove("evaporationDepthContribution");
					
				} else if (r.key.equals ("roughnessLength")) {
					initSoil.setRoughnessLength (r.getDoubleValue ());
					requiredParameters.remove("roughnessLength");
					
				} else if (r.key.equals ("denitrificationRate")) {
					initSoil.setDenitrificationRate (r.getDoubleValue ());	
					requiredParameters.remove("denitrificationRate");
					
				} else if (r.key.equals ("denitrificationDepth")) {
					initSoil.setDenitrificationDepth (r.getDoubleValue ());
					requiredParameters.remove("denitrificationDepth");
					
				} else if (r.key.equals ("soilHumusCN")) {
					initSoil.setSoilHumusCN (r.getDoubleValue ());
					requiredParameters.remove("soilHumusCN");
					
				} else if (r.key.equals ("runOffCoefPlantMulch")) {
					initSoil.setRunOffCoefPlantMulch (r.getDoubleValue ());
					requiredParameters.remove("runOffCoefPlantMulch");
					
				
				} else if (r.key.equals ("no3ConcentrationInWaterTable")) {
					initSoil.setNo3ConcentrationInWaterTable (r.getDoubleValue ());
					requiredParameters.remove("no3ConcentrationInWaterTable");
					
				} else if (r.key.equals ("nh4ConcentrationInWaterTable")) {
					initSoil.setNh4ConcentrationInWaterTable (r.getDoubleValue ());
					requiredParameters.remove("nh4ConcentrationInWaterTable");
			
				} else if (r.key.equals ("pedotransfertThetaSat")) {
					initSoil.setPedotransfertThetaSat (r.getDoubleValue());
				} else if (r.key.equals ("pedotransfertKSat")) {
					initSoil.setPedotransfertKsat (r.getDoubleValue());
				} else if (r.key.equals ("pedotransfertAlpha")) {
					initSoil.setPedotransfertAlpha (r.getDoubleValue());
				} else if (r.key.equals ("pedotransfertLambda")) {
					initSoil.setPedotransfertLambda (r.getDoubleValue());
				} else if (r.key.equals ("pedotransfertBulkDensity")) {
					initSoil.setPedotransfertBulkDensity (r.getDoubleValue());
				} else if (r.key.equals ("pedotransfertN")) {
					initSoil.setPedotransfertN (r.getDoubleValue());
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
			throw new CancellationException();	// abort
		}
		
		//return the stand with plot and soil 
		SafeStand initStand = new SafeStand ();
		
		// 1. PlotOfCells creation
		double cellWidth 	= plotSettings.cellWidth;
		double plotWidth 	= plotSettings.plotWidth;
		double plotHeight 	= plotSettings.plotHeight;
		int nLin = (int) (plotHeight / cellWidth);
		int nCol = (int) (plotWidth / cellWidth);

		initStand.createPlot (m, plotSettings.cellWidth,  nLin, nCol);
		initStand.getPlot().setPlotSettings(plotSettings);
		SafeLayer[] realSoilLayers = new SafeLayer [layerId];
        for (int index = 0; index < layerId; index++) 
         	realSoilLayers[index]= soilLayers[index];

		initSoil.setLayers(realSoilLayers);
		initSoil.setNbVoxels(nbVoxels);
		initStand.getPlot().setSoil(initSoil);

		return initStand;
	}

}
