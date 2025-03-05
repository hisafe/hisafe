/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.extension.dataextractor;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Locale;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Plot;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.spatial.ClarkEvans;
import capsis.util.methodprovider.CellRelativeHorizontalEnergyProvider;
import capsis.util.methodprovider.InterceptionIndexStatProvider;
import capsis.util.methodprovider.IrradianceStatProvider;
import capsis.util.methodprovider.LayerInterceptionIndexProvider;

/**
 * A stand table report.
 * 
 * @author B.Courbaud - April 2004
 */
public class DELightTable extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DELightTable");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected Collection tables;
	protected Collection titles;
	protected MethodProvider methodProvider;

	protected NumberFormat f;

	/**
	 * Constructor.
	 */
	public DELightTable() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DELightTable(GenericExtensionStarter s) {
		super(s);

		try {
			tables = new ArrayList();
			titles = new ArrayList();

			// Used to format decimal part with 2 digits only
			f = NumberFormat.getInstance(Locale.ENGLISH);
			f.setGroupingUsed(false);
			f.setMaximumFractionDigits(3);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DELightTable.c ()", "Exception during construction : ", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;

			GScene stand = ((Step) m.getProject().getRoot()).getScene();
			if (!(stand instanceof TreeCollection)) {
				return false;
			}

			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof IrradianceStatProvider)) {
				return false;
			}
			if (!(mp instanceof CellRelativeHorizontalEnergyProvider)) {
				return false;
			}
			if (!(mp instanceof InterceptionIndexStatProvider)) {
				return false;
			}
			if (!(mp instanceof LayerInterceptionIndexProvider)) {
				return false;
			}
			if (!(stand.getPlot () instanceof PlotOfCells))
				return false; // fc-26.10.2017

		} catch (Exception e) {
			Log.println(Log.ERROR, "DELightTable.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DELightTable.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B.Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DELightTable.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.1";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.DRTables";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		// addConfigProperty (DataExtractor.HECTARE);
		addBooleanProperty("visibleStepsOnly");
		addDoubleProperty("minThresholdInCm", 17.5);

		addBooleanProperty("Spatial_ClarkEvans", true);

		addBooleanProperty("Irradiance_Mean", true);
		addBooleanProperty("Irradiance_SD", true);
		addBooleanProperty("Irradiance_GapArea");

		addBooleanProperty("Index_Mean", true);
		addBooleanProperty("Index_SD", true);
		addBooleanProperty("Index_L1M");
		addBooleanProperty("Index_L1SD");
		addBooleanProperty("Index_L2M");
		addBooleanProperty("Index_L2SD");
		addBooleanProperty("Index_L3M");
		addBooleanProperty("Index_L3SD");
		addBooleanProperty("Index_L4M");
		addBooleanProperty("Index_L4SD");
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output computes the basal area of the stand versus
	 * date from the root Step to this one.
	 * 
	 * Return false if trouble while extracting.
	 */
	public boolean doExtraction() {
		// ~ System.out.println ("DELightTable.doExtraction ()");
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		// ~ System.out.println ("upToDate="+upToDate+" step="+step);

		// Retrieve method provider
		// methodProvider = MethodProviderFactory.getMethodProvider
		// (step.getScenario ().getModel ());
		methodProvider = step.getProject().getModel().getMethodProvider();

		Log.println("DELightTable : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;
			// if (isSet("perHectare")) {
			coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			// }
			double standArea = getSceneArea(step.getScene()); // fc-2.6.2020

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			int n = steps.size();
			if (isSet("visibleStepsOnly")) {
				n = 0;
				for (Iterator i = steps.iterator(); i.hasNext();) {
					Step s = (Step) i.next();
					if (s.isVisible()) {
						n++;
					}
				}
			}

			int sizeSpatial = 1;
			if (isSet("Spatial_ClarkEvans")) {
				sizeSpatial++;
			}
			int sizeIrradiance = 0;
			if (isSet("Irradiance_Mean")) {
				sizeIrradiance++;
			}
			if (isSet("Irradiance_SD")) {
				sizeIrradiance++;
			}
			if (isSet("Irradiance_GapArea")) {
				sizeIrradiance++;
			}
			int sizeIndex = 0;
			if (isSet("Index_Mean")) {
				sizeIndex++;
			}
			if (isSet("Index_SD")) {
				sizeIndex++;
			}
			if (isSet("Index_L1M")) {
				sizeIndex++;
			}
			if (isSet("Index_L1SD")) {
				sizeIndex++;
			}
			if (isSet("Index_L2M")) {
				sizeIndex++;
			}
			if (isSet("Index_L2SD")) {
				sizeIndex++;
			}
			if (isSet("Index_L3M")) {
				sizeIndex++;
			}
			if (isSet("Index_L3SD")) {
				sizeIndex++;
			}
			if (isSet("Index_L4M")) {
				sizeIndex++;
			}
			if (isSet("Index_L4SD")) {
				sizeIndex++;
			}

			n += 1; // add first line for columns headers
			String[][] tabSpatial = null;
			String[][] tabIrradiance = null;
			String[][] tabIndex = null;
			if (sizeSpatial != 0) {
				tabSpatial = new String[n][sizeSpatial];
			}
			if (sizeIrradiance != 0) {
				tabIrradiance = new String[n][sizeIrradiance];
			}
			if (sizeIndex != 0) {
				tabIndex = new String[n][sizeIndex];
			}

			// Tables titles
			titles.clear();
			if (sizeSpatial != 0) {
				titles.add(Translator.swap("DELightTable.spatial"));
			}
			if (sizeIrradiance != 0) {
				titles.add(Translator.swap("DELightTable.irradiance"));
			}
			if (sizeIndex != 0) {
				titles.add(Translator.swap("DELightTable.index"));
			}

			// Column headers
			int c = 0; // column number
			tabSpatial[0][c++] = "Date";
			if (isSet("Spatial_ClarkEvans")) {
				tabSpatial[0][c++] = "Clark-Evans";
			}

			c = 0; // column number
			if (isSet("Irradiance_Mean")) {
				tabIrradiance[0][c++] = "Mean";
			}
			if (isSet("Irradiance_SD")) {
				tabIrradiance[0][c++] = "SD";
			}
			if (isSet("Irradiance_GapArea")) {
				tabIrradiance[0][c++] = "Gap Area";
			}

			c = 0; // column number
			if (isSet("Index_Mean")) {
				tabIndex[0][c++] = "Mean";
			}
			if (isSet("Index_SD")) {
				tabIndex[0][c++] = "SD";
			}
			if (isSet("Index_L1M")) {
				tabIndex[0][c++] = "L1 Mean";
			}
			if (isSet("Index_L1SD")) {
				tabIndex[0][c++] = "L1 SD";
			}
			if (isSet("Index_L2M")) {
				tabIndex[0][c++] = "L2 Mean";
			}
			if (isSet("Index_L2SD")) {
				tabIndex[0][c++] = "L2 SD";
			}
			if (isSet("Index_L3M")) {
				tabIndex[0][c++] = "L3 Mean";
			}
			if (isSet("Index_L3SD")) {
				tabIndex[0][c++] = "L3 SD";
			}
			if (isSet("Index_L4M")) {
				tabIndex[0][c++] = "L4 Mean";
			}
			if (isSet("Index_L4SD")) {
				tabIndex[0][c++] = "L4 SD";
			}

			// Data extraction

			int line = 1;
			String buffer = "";
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step step = (Step) i.next();
				// if (isSet ("visibleStepsOnly") && !step.isVisible ())
				// {continue;} // next iteration

				// Previous step
				// Step prevStep = null;
				// //if (isSet ("visibleStepsOnly")) {
				// // prevStep = (Step) step.getVisibleFather ();
				// // } else {
				// prevStep = (Step) step.getFather ();
				// // }

				double minThreshold = getDoubleProperty("minThresholdInCm");

				// 1. Retrieve trees in the stand under the step
				GScene stand = step.getScene();
				Collection trees = new ArrayList();

				Iterator i1 = (((TreeCollection) stand).getTrees()).iterator();
				while (i1.hasNext()) {
					Tree t = (Tree) i1.next();
					double d = t.getDbh(); // dbh : cm
					if (d > minThreshold) {
						trees.add(t);
					}
				}

				// 2. Retrieve trees in the stand under the previous step
				/*
				 * GStand prevStand = null; Collection prevTrees = new
				 * ArrayList(); //list based on table
				 * 
				 * // Caution : if step = root, prevStep = null try { prevStand
				 * = prevStep.getStand (); Iterator i2 = (((TreeCollection)
				 * prevStand).getTrees ()).iterator (); while (i2.hasNext ()) {
				 * GTree t = (GTree) i2.next (); double d = t.getDbh (); // dbh
				 * : cm if (d > minThreshold) {prevTrees.add(t);} } } catch
				 * (Exception e) {}
				 */

				// Stand variables
				// ------------------------------------------------------------------
				c = 0; // column number
				int date = stand.getDate();
				tabSpatial[line][c++] = "" + date;

				double CE = -1d; // default = "not calculable"
				if (isSet("Spatial_ClarkEvans")) {

					// works on the iterator made with the trees of the stand
					Iterator treesIter = trees.iterator();
					int pointnb = trees.size();

					// Limited in size! (java initializes each value to 0)
					double x[] = new double[pointnb + 1];
					double y[] = new double[pointnb + 1];
					double xmi = stand.getOrigin().x;
					double xma = xmi + stand.getXSize();
					double ymi = stand.getOrigin().y;
					double yma = ymi + stand.getYSize();

					// Create output data
					int j = -1;
					while (treesIter.hasNext()) {
						j += 1;
						SpatializedTree t = (SpatializedTree) treesIter.next();
						x[j] = t.getX(); // x : m // FG
						y[j] = t.getY(); // y : m // FG
					}
					int pointNumber = j + 1;

					CE = ClarkEvans.computeCE(x, y, pointNumber, xmi, xma, ymi, yma);
					if (Double.isNaN(CE)) {
						tabSpatial[line][c++] = "-";
					} else {
						tabSpatial[line][c++] = "" + f.format(CE);
					}
				}

				// Irradiance variables
				// ------------------------------------------------------------------
				c = 0; // column number

				Vector irradianceStats = ((IrradianceStatProvider) methodProvider).getIrradianceStats(stand);

				double irradianceMean = -1d; // default = "not calculable"
				try {
					irradianceMean = ((Double) irradianceStats.get(0)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Irradiance_Mean")) {
					if (irradianceMean == -1) {
						tabIrradiance[line][c++] = "-";
					} else {
						tabIrradiance[line][c++] = "" + f.format(irradianceMean);
					}
				}
				double irradianceSD = -1d; // default = "not calculable"
				try {
					irradianceSD = ((Double) irradianceStats.get(1)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Irradiance_SD")) {
					if (irradianceSD == -1) {
						tabIrradiance[line][c++] = "-";
					} else {
						tabIrradiance[line][c++] = "" + f.format(irradianceSD);
					}
				}

				double irradAreaSup20 = -1d; // default = "not calculable"
												// Result in % of totalArea
				if (isSet("Irradiance_GapArea")) {

					PlotOfCells plot = stand.getPlot(); // fc-26.10.2017
					
					Collection cells = plot.getCells();
					Iterator ite = cells.iterator();
					int tab[] = new int[5];
					int totalCellNb = 0;
					int numbers = 0;

					// Create output data
					while (ite.hasNext()) {
						Cell cell = (Cell) ite.next();
						double relEnergy = ((CellRelativeHorizontalEnergyProvider) methodProvider)
								.getCellRelativeHorizontalEnergy(cell);
						if (relEnergy == 100) {
							relEnergy = 99.9;
						}
						int category = (int) (relEnergy / 20);
						tab[category] += 1;
						totalCellNb += 1;
					}
					irradAreaSup20 = 100 * (tab[4] + tab[3] + tab[2] + tab[1]) / totalCellNb;
					tabIrradiance[line][c++] = "" + f.format(irradAreaSup20);
				}

				// Interception Index variables
				// ------------------------------------------------------------------
				c = 0; // column number

				Vector interceptionIndexStats = ((InterceptionIndexStatProvider) methodProvider)
						.getInterceptionIndexStats(stand, trees);
				if (isSet("Index_Mean")) {
					double indexMean = -1d; // default = "not calculable"
					try {
						indexMean = ((Double) interceptionIndexStats.get(0)).doubleValue();
					} catch (Exception e) {
					}
					if (indexMean == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(indexMean);
					}
				}
				if (isSet("Index_SD")) {
					double indexSD = -1d; // default = "not calculable"
					try {
						indexSD = ((Double) interceptionIndexStats.get(1)).doubleValue();
					} catch (Exception e) {
					}
					if (indexSD == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(indexSD);
					}
				}

				Vector layerInterceptionIndex = ((LayerInterceptionIndexProvider) methodProvider)
						.getLayerInterceptionIndex(stand, trees);

				double l1Ix = -1d; // default = "not calculable"
				try {
					l1Ix = ((Double) layerInterceptionIndex.get(0)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L1M")) {
					if (l1Ix == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l1Ix);
					}
				}
				double l1IxSD = -1d; // default = "not calculable"
				try {
					l1IxSD = ((Double) layerInterceptionIndex.get(1)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L1SD")) {
					if (l1IxSD == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l1IxSD);
					}
				}
				double l2Ix = -1d; // default = "not calculable"
				try {
					l2Ix = ((Double) layerInterceptionIndex.get(2)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L2M")) {
					if (l2Ix == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l2Ix);
					}
				}
				double l2IxSD = -1d; // default = "not calculable"
				try {
					l2IxSD = ((Double) layerInterceptionIndex.get(3)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L2SD")) {
					if (l2IxSD == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l2IxSD);
					}
				}
				double l3Ix = -1d; // default = "not calculable"
				try {
					l3Ix = ((Double) layerInterceptionIndex.get(4)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L3M")) {
					if (l3Ix == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l3Ix);
					}
				}
				double l3IxSD = -1d; // default = "not calculable"
				try {
					l3IxSD = ((Double) layerInterceptionIndex.get(5)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L3SD")) {
					if (l3IxSD == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l3IxSD);
					}
				}
				double l4Ix = -1d; // default = "not calculable"
				try {
					l4Ix = ((Double) layerInterceptionIndex.get(6)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L4M")) {
					if (l4Ix == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l4Ix);
					}
				}
				double l4IxSD = -1d; // default = "not calculable"
				try {
					l4IxSD = ((Double) layerInterceptionIndex.get(7)).doubleValue();
				} catch (Exception e) {
				}
				if (isSet("Index_L4SD")) {
					if (l4IxSD == -1) {
						tabIndex[line][c++] = "-";
					} else {
						tabIndex[line][c++] = "" + f.format(l4IxSD);
					}
				}

				line++;
			}

			tables.clear();
			if (tabSpatial != null) {
				tables.add(tabSpatial);
			}
			if (tabIrradiance != null) {
				tables.add(tabIrradiance);
			}
			if (tabIndex != null) {
				tables.add(tabIndex);
			}

			// ~ System.out.println ("DELightTable : extraction done");

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DELightTable.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * This prefix is built depending on current settings. ex: "+ 25 years /ha"
	 */
	// fc - 23.4.2004
	// ~ protected String getNamePrefix () {
	// ~ String prefix = "";
	// ~ try {
	// ~ if (isCommonGroup ()
	// ~ && isGroupMode ()
	// ~ && GroupManager.getInstance ().getGroupNames ().contains (getGroupName
	// ())) {
	// ~ prefix += getGroupName ()+" - ";
	// ~ }
	// ~ if (isSet("perHectare")) {prefix += "/ha - ";}
	// ~ } catch (Exception e) {} // if trouble, prefix is empty
	// ~ return prefix;
	// ~ }

	/**
	 * From DataFormat interface.
	 */
	// ~ public String getCaption () {
	// ~ return getStep ().getCaption ();
	// ~ }

	/**
	 * From DFTables interface.
	 */
	public Collection getTables() {
		return tables;
	}

	/**
	 * From DFTables interface.
	 */
	public Collection getTitles() {
		return titles;
	}

}
