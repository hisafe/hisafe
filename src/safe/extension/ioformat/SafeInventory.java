/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2001 Francois de Coligny
 * 
 * This program is free software; you can redistribute it and/or modify it under the terms of the
 * GNU General Public License as published by the Free Software Foundation; either version 2 of the
 * License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License along with this program; if
 * not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
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
import safe.model.SafeInitialParameters;
import safe.model.SafeInitialValues;
import safe.model.SafeModel;
import safe.model.SafePlotSettings;
import safe.model.SafeStand;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.StandRecordSet;

/**
 * SafeInventory contains records description for safe input/output inventory file.
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafeInventory extends StandRecordSet {

	static {
		Translator.addBundle ("safe.extension.ioformat.SafeInventory");
	}


	// Generic keyword record is described in superclass: key = value

	// Safe layer record is described here
	@Import
	static public class LayerRecord extends Record {

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
		public double partSizeSand; // �m
		public double stone; // %
		public int stoneType; // 1-10
		public double infiltrability; // mm j-1

	}


	// Safe layer initial values record is described here
	@Import
	static public class LayerInitialRecord extends Record {

		public LayerInitialRecord () {
			super ();
		}

		public LayerInitialRecord (String line) throws Exception {
			super (line);
		}

		// public String getSeparator () {return ";";} // to change default "\t" separator
		public String name;
		public double waterContent; // m3 m-3
		public double no3Content; // kg h-1
		public double nh4Content; // kg h-1
	}


	// Safe tree record is described here
	@Import
	static public class TreeRecord extends Record {

		public TreeRecord () {
			super ();
		}

		public TreeRecord (String line) throws Exception {
			super (line);
		}

		// public String getSeparator () {return ";";} // to change default "\t" separator
		public String name;
		public String treeSpecies;
		public double height; // m
		public double crownBaseHeight; // m
		public double crownRadius; // gt - 5.10.2009
		public double treeX;
		public double treeY;
	}


	// Tree fine root initial values record is described here
	@Import
	static public class TreeFineRootInitialRecord extends Record {

		public TreeFineRootInitialRecord () {
			super ();
		}

		public TreeFineRootInitialRecord (String line) throws Exception {
			super (line);
		}

		// public String getSeparator () {return ";";} // to change default "\t" separator
		public String name;
		public int rootShape; // 1-3
		public int rootRepartition; // 1-3
		public double shapeParam1; // m
		public double shapeParam2; // m
		public double shapeParam3; // m
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
		createRecordSet (fileName);
	} // for direct use for Import

	public SafeInventory (SafeInitialParameters settings, SafePlotSettings plotSettings, SafeInitialValues initialValues)
			throws Exception {
		createRecordSet (settings, plotSettings, initialValues);
	} // for direct use for Export

	public void createRecordSet (SafeInitialParameters settings, SafePlotSettings plotSettings, SafeInitialValues initialValues)
			throws Exception {

		add (new CommentRecord ("STAND"));

		add (new KeyRecord ("latitude", "" + plotSettings.plotLatitude));
		add (new KeyRecord ("elevation", "" + plotSettings.plotElevation));

		add (new EmptyRecord ());
		add (new CommentRecord ("PLOT"));
		add (new KeyRecord ("cellWidth", "" + plotSettings.cellWidth));
		add (new KeyRecord ("plotWidth", "" + plotSettings.plotWidth));
		add (new KeyRecord ("plotHeight", "" + plotSettings.plotHeight));
		add (new KeyRecord ("northOrientation", "" + plotSettings.northOrientation));
		add (new KeyRecord ("slopeIntensity", "" + plotSettings.slopeIntensity));
		add (new KeyRecord ("slopeAspect", "" + plotSettings.slopeAspect));

		add (new EmptyRecord ());
		add (new CommentRecord ("SOIL"));
		add (new KeyRecord ("humificationDepth", "" + plotSettings.humificationDepth));
		add (new KeyRecord ("organicNitrogen", "" + plotSettings.organicNitrogen));
		add (new KeyRecord ("albedo", "" + plotSettings.albedo));
		add (new KeyRecord ("evaporationValue", "" + plotSettings.evaporationValue));
		add (new KeyRecord ("rainRunOffFraction", "" + plotSettings.rainRunOffFraction));
		add (new KeyRecord ("cropRootObstruction", "" + plotSettings.cropRootObstruction));
		add (new KeyRecord ("minNo3Concentration", "" + plotSettings.minNo3Concentration));
		add (new KeyRecord ("ph", "" + plotSettings.ph));
		add (new KeyRecord ("capillary", "" + plotSettings.capillary));
		add (new KeyRecord ("capillaryUptake", "" + plotSettings.capillaryUptake));
		add (new KeyRecord ("capillaryUptakeMinWater", "" + plotSettings.capillaryUptakeMinWater));
		add (new KeyRecord ("artificialDrainage", "" + plotSettings.artificialDrainage));
		add (new KeyRecord ("impermeableLayerDepth", "" + plotSettings.impermeableLayerDepth));
		add (new KeyRecord ("drainagePipesSpacing", "" + plotSettings.drainagePipesSpacing));
		add (new KeyRecord ("drainagePipesDepth", "" + plotSettings.drainagePipesDepth));
		add (new KeyRecord ("waterConductivity", "" + plotSettings.waterConductivity));
		add (new KeyRecord ("swellingClaySoil", "" + plotSettings.swellingClaySoil));
		add (new KeyRecord ("nitrification", "" + plotSettings.nitrification));
		add (new KeyRecord ("macroporosity", "" + plotSettings.macroporosity));




		add (new KeyRecord ("no3ConcentrationInWaterTable", "" + plotSettings.no3ConcentrationInWaterTable));
		add (new KeyRecord ("nh4ConcentrationInWaterTable", "" + plotSettings.nh4ConcentrationInWaterTable));
		



		add (new KeyRecord ("waterTable", "" + plotSettings.waterTable));
		add (new KeyRecord ("voxelThicknessMax", "" + plotSettings.voxelThicknessMax));

		add (new EmptyRecord ());
		add (new CommentRecord ("LAYERS"));
		add (new CommentRecord (
				"thick	sand 	clay	limeStone	organicMatter	partSizeSand	stone	stoneType	infiltrability"));
		add (new CommentRecord ("(m)	%	%	%	%	�m	g g-1	%	1-10	mm j-1"));

		int nbLayers = 0;
		int nbLayerMax = settings.NB_LAYER_MAX;
		for (int i = 0; i < nbLayerMax; i++) {

			if (plotSettings.layerThickness[i] > 0) {
				LayerRecord r = new LayerRecord ();

				r.name = "Layer";
				r.thickness = plotSettings.layerThickness[i];
				r.clayPercent = plotSettings.layerClay[i];
				r.sandPercent = plotSettings.layerSand[i];
				r.limeStonePercent = plotSettings.layerLimeStone[i];
				r.organicMatterPercent = plotSettings.layerOrganicMatter[i];
				r.partSizeSand = plotSettings.layerPartSizeSand[i];
				r.stone = plotSettings.layerStone[i];
				r.stoneType = plotSettings.layerStoneType[i];
				r.infiltrability = plotSettings.layerInfiltrability[i];

				add (r);
				nbLayers++;
			}

		}

		add (new EmptyRecord ());
		add (new CommentRecord ("LAYERS INITIALISATION"));
		add (new CommentRecord ("id	waterContent	no3Concentration	nh4concentration"));
		add (new CommentRecord ("	%	kg h-1	kg h-1"));

		for (int i = 0; i < nbLayers; i++) {

			LayerInitialRecord r = new LayerInitialRecord ();

			r.name = "LayerInit";
			r.waterContent = initialValues.layerWaterContent[i];
			r.no3Content = initialValues.layerNo3Content[i];
			r.nh4Content = initialValues.layerNh4Content[i];

			add (r);

		}

		add (new EmptyRecord ());
		int nbTrees = 0;
		int nbTreeMax = plotSettings.nbTrees;
		add (new CommentRecord ("TREES"));
		add (new CommentRecord ("id	species	height	crownBaseHeight	 crownRadius")); 
		add (new CommentRecord ("		m	m	%"));
		for (int i = 0; i < nbTreeMax; i++) {

			if (initialValues.treeSpecies[i] != "") {

				TreeRecord r = new TreeRecord ();

				r.name = "TreeInit";
				r.treeSpecies = initialValues.treeSpecies[i];
				r.height = initialValues.treeHeight[i];
				r.crownBaseHeight = initialValues.treeCrownBaseHeight[i];
				r.crownRadius = initialValues.crownRadius[i]; // gt - 5.10.2009
				r.treeX = initialValues.treeX[i];
				r.treeY = initialValues.treeY[i];

				add (r);
				nbTrees++;
			}
		}

		add (new EmptyRecord ());
		add (new CommentRecord ("ROOT INITIALISATION"));
		add (new CommentRecord ("shape	repartition	paramShape1	paramShape2	paramShape3"));
		add (new CommentRecord ("			m	m	m"));
		for (int i = 0; i < nbTrees; i++) {

			TreeFineRootInitialRecord r = new TreeFineRootInitialRecord ();

			r.name = "RootInit";
			r.rootShape = initialValues.rootShape[i];
			r.rootRepartition = initialValues.rootRepartition[i];
			r.shapeParam1 = initialValues.shapeParam1[i];
			r.shapeParam2 = initialValues.shapeParam2[i];
			r.shapeParam3 = initialValues.shapeParam3[i];
			add (r);

		}
	}

	/**
	 * RecordSet -> SafeCreatePlotSettings Implementation here. Was initialy described in
	 * SafeModel.loadInitStand () To load a stand for another model, recognize real type of model :
	 * if (model instanceof SafeModel) -> this code if (model instanceof MobyDickModel) -> other
	 * code...
	 */
	public GScene load (GModel model) throws Exception {
		return load (model, (SafeInitialParameters) model.getSettings ());
	}
	
	public GScene load (GModel model, SafeInitialParameters safeSettings) throws Exception {


		SafeModel m = (SafeModel) model;
	
		safeSettings.plotSettings = new SafePlotSettings ();
		safeSettings.initialValues = new SafeInitialValues ();
		SafePlotSettings plotSettings = safeSettings.plotSettings;
		SafeInitialValues initialValues = safeSettings.initialValues;

		int nbLayerMax = safeSettings.NB_LAYER_MAX;
		int nbTreeMax = safeSettings.NB_TREE_MAX;
		int nbLayer = 0;

		Set<String> requiredParameters = new HashSet<>();
		requiredParameters.add("layer");
		requiredParameters.add("layerInit");
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

		int layerId = 0;
		int treeId = 0;

		int layerInitId = 0;
		int treeInitId = 0;
		
		for (Iterator i = this.iterator (); i.hasNext ();) {
			Record record = (Record) i.next ();

			// Layer Record
			if (record instanceof SafeInventory.LayerRecord) {
				if (nbLayer==0) requiredParameters.remove("layer");
				SafeInventory.LayerRecord r = (SafeInventory.LayerRecord) record; 
			
				nbLayer++;
				if ((layerId > (nbLayerMax - 1)) || (nbLayer > nbLayerMax))
					throw new Exception ("More than " + nbLayerMax + " layers !"); 

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

			} // Layer initial Record

			else if (record instanceof SafeInventory.LayerInitialRecord) {

				// initialisation first time only
				if (layerInitId == 0) {
					requiredParameters.remove("layerInit");
					for (int l = 0; l < nbLayerMax; l++) {
						initialValues.layerWaterContent[l] = 0;
						initialValues.layerNo3Content[l] = 0;
						initialValues.layerNh4Content[l] = 0;
					}
				}
			
				SafeInventory.LayerInitialRecord r = (SafeInventory.LayerInitialRecord) record; 
				if (layerInitId > (nbLayerMax - 1)) throw new Exception ("More than " + nbLayerMax + " layers !"); 
				initialValues.layerWaterContent[layerInitId] = r.waterContent;
				initialValues.layerNo3Content[layerInitId] = r.no3Content;
				initialValues.layerNh4Content[layerInitId] = r.nh4Content;

				layerInitId++;

			} // Tree Record

			else if (record instanceof SafeInventory.TreeRecord) {

				SafeInventory.TreeRecord r = (SafeInventory.TreeRecord) record; // cast to precise

				// First tree and all trees on his right
				String treeSpecies = r.treeSpecies;

				initialValues.treeSpecies[treeId] = treeSpecies;
				initialValues.treeAge[treeId] = 1;
				initialValues.treeHeight[treeId] = r.height;
				initialValues.treeCrownBaseHeight[treeId] = r.crownBaseHeight;
				initialValues.crownRadius[treeId] = r.crownRadius; // gt - 5.10.2009
				if ((r.treeX==0) && (r.treeY==0)) {
					initialValues.treeX[treeId] = plotSettings.plotWidth/2;
					initialValues.treeY[treeId] = plotSettings.plotHeight/2;					
				}
				else {
					initialValues.treeX[treeId] = r.treeX;
					initialValues.treeY[treeId] = r.treeY;
				}

				treeId++;
				plotSettings.nbTrees = treeId;


			} // Root initial Record

			else if (record instanceof SafeInventory.TreeFineRootInitialRecord) {

				// initialisation first time only
				if (treeInitId == 0) {
					for (int t = 0; t < nbTreeMax; t++) {
						initialValues.rootShape[t] = 0;
						initialValues.rootRepartition[t] = 0;
						initialValues.shapeParam1[t] = 0;
						initialValues.shapeParam2[t] = 0;
						initialValues.shapeParam3[t] = 0;
					}
				}
			

				SafeInventory.TreeFineRootInitialRecord r = (SafeInventory.TreeFineRootInitialRecord) record; 

				initialValues.rootShape[treeInitId] = r.rootShape;
				initialValues.rootRepartition[treeInitId] = r.rootRepartition;
				initialValues.shapeParam1[treeInitId] = r.shapeParam1;
				initialValues.shapeParam2[treeInitId] = r.shapeParam2;
				initialValues.shapeParam3[treeInitId] = r.shapeParam3;
				
				treeInitId++;	

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
					
				} else if (r.key.equals ("minNo3Concentration")) {
					plotSettings.minNo3Concentration = r.getDoubleValue ();
					requiredParameters.remove("minNo3Concentration");
					
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
				throw new Exception ("Unrecognized record : " + record); // automatic toString ()
																			// (or null)
			}
		}

		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing plot parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort

		}
		double latitude = plotSettings.plotLatitude;
		double elevation = plotSettings.plotElevation;

		SafeStand initStand = new SafeStand (latitude, elevation);
		return initStand;
	}

	// //////////////////////////////////////////////// IOFormat stuff
	public boolean isImport () {
		return true;
	}

	public boolean isExport () {
		return false;
	}

}
