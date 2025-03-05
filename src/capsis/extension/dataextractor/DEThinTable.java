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
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.HgProvider;
import capsis.util.methodprovider.KgProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.VProvider;

/**
 * A thinning table report.
 * 
 * @author C. Meredieu, T. Labbé - May 2005
 */
public class DEThinTable extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEThinTable");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected Collection tables;
	protected Collection titles;
	protected MethodProvider methodProvider;

	protected NumberFormat formater;
	protected NumberFormat formater3;

	/**
	 * Constructor.
	 */
	public DEThinTable() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEThinTable(GenericExtensionStarter s) {
		super(s);

		try {
			tables = new ArrayList();
			titles = new ArrayList();
			// Used to format decimal part with 2 digits only

			// add cm + tl 10032005 : US format to export to excel
			formater = NumberFormat.getInstance(Locale.US); // to impose decimal
															// dot instead of
															// "," for french
															// number format
			// formater = NumberFormat.getInstance ();
			formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);
			formater.setMinimumFractionDigits(MAX_FRACTION_DIGITS);
			formater.setGroupingUsed(false);
			formater3 = NumberFormat.getInstance(Locale.US); // to impose
																// decimal dot
																// instead of
																// "," for
																// french number
																// format
			// formater3 = NumberFormat.getInstance ();
			formater3.setMaximumFractionDigits(3);
			formater3.setMinimumFractionDigits(3);
			formater3.setGroupingUsed(false);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEThinTable.c ()", "Exception during construction : ", e);
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
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof NProvider)) {
				return false;
			}
			if (!(mp instanceof GProvider)) {
				return false;
			} // to be refined

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEThinTable.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEThinTable.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "C. Meredieu, T. Labbe";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEThinTable.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.0";

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
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addBooleanProperty("before_Hg");
		addBooleanProperty("before_Hdom");
		addBooleanProperty("before_N", true);
		addBooleanProperty("before_Cg", true);
		addBooleanProperty("before_Cdom");
		addBooleanProperty("before_Dg");
		addBooleanProperty("before_Ddom");
		addBooleanProperty("before_G", true);
		addBooleanProperty("before_V", true);
		addBooleanProperty("current_Hg");
		addBooleanProperty("current_Hdom");
		addBooleanProperty("current_N", true); // "current" for the stand after
												// thinning : alphabetic order
												// before then current; TL
												// 10052005
		addBooleanProperty("current_Cg", true);
		addBooleanProperty("current_Cdom");
		addBooleanProperty("current_Dg", true);
		addBooleanProperty("current_Ddom");
		addBooleanProperty("current_G", true);
		addBooleanProperty("current_V", true);
		addBooleanProperty("thin_Kg", true);
		addBooleanProperty("thin_IntensityN"); // add cm lt 18 05 2015
		addBooleanProperty("thin_IntensityG"); // add cm lt 18 05 2015
		addBooleanProperty("thin_IntensityV"); // add cm lt 18 05 2015

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
		// ~ System.out.println ("DEThinTable.doExtraction ()");
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

		// ~ System.out.println ("DEThinTable : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			int n = steps.size();
			n = 0;
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();
				if (s.getScene().isInterventionResult()) {
					n++;
				}
			}

			int sizeBefore = 1;
			if (isSet("before_Hg")) {
				sizeBefore++;
			}
			if (isSet("before_Hdom")) {
				sizeBefore++;
			}
			if (isSet("before_N")) {
				sizeBefore++;
			}
			if (isSet("before_Cg")) {
				sizeBefore++;
			}
			if (isSet("before_Cdom")) {
				sizeBefore++;
			}
			if (isSet("before_Dg")) {
				sizeBefore++;
			}
			if (isSet("before_Ddom")) {
				sizeBefore++;
			}
			if (isSet("before_G")) {
				sizeBefore++;
			}
			if (isSet("before_V")) {
				sizeBefore++;
			}
			int sizeCurrent = 0;
			if (isSet("current_Hg")) {
				sizeCurrent++;
			}
			if (isSet("current_Hdom")) {
				sizeCurrent++;
			}
			if (isSet("current_N")) {
				sizeCurrent++;
			}
			if (isSet("current_Cg")) {
				sizeCurrent++;
			}
			if (isSet("current_Cdom")) {
				sizeCurrent++;
			}
			if (isSet("current_Dg")) {
				sizeCurrent++;
			}
			if (isSet("current_Ddom")) {
				sizeCurrent++;
			}
			if (isSet("current_G")) {
				sizeCurrent++;
			}
			if (isSet("current_V")) {
				sizeCurrent++;
			}
			int sizeThin = 0;
			if (isSet("thin_Kg")) {
				sizeThin++;
			}
			if (isSet("thin_IntensityN")) {
				sizeThin++;
			}
			if (isSet("thin_IntensityG")) {
				sizeThin++;
			}
			if (isSet("thin_IntensityV")) {
				sizeThin++;
			}

			n += 1; // add first line for columns headers
			String[][] tabBefore = null;
			String[][] tabCurrent = null;
			String[][] tabThin = null;
			if (sizeBefore != 0) {
				tabBefore = new String[n][sizeBefore];
			}
			if (sizeCurrent != 0) {
				tabCurrent = new String[n][sizeCurrent];
			}
			if (sizeThin != 0) {
				tabThin = new String[n][sizeThin];
			}

			// Tables titles
			titles.clear();
			if (sizeBefore != 0) {
				titles.add(Translator.swap("DEThinTable.before"));
			}
			if (sizeCurrent != 0) {
				titles.add(Translator.swap("DEThinTable.current"));
			}
			if (sizeThin != 0) {
				titles.add(Translator.swap("DEThinTable.thin"));
			}

			// Column headers
			int c = 0; // column number
			tabBefore[0][c++] = "Date";

			if (isSet("before_Hg")) {
				tabBefore[0][c++] = "Hg";
			}
			if (isSet("before_Hdom")) {
				tabBefore[0][c++] = "Hdom";
			}
			if (isSet("before_N")) {
				tabBefore[0][c++] = (isSet("perHectare")) ? "N/ha" : "N";
			}
			if (isSet("before_Cg")) {
				tabBefore[0][c++] = "Cg";
			}
			if (isSet("before_Cdom")) {
				tabBefore[0][c++] = "Cdom";
			}
			if (isSet("before_Dg")) {
				tabBefore[0][c++] = "Dg";
			}
			if (isSet("before_Ddom")) {
				tabBefore[0][c++] = "Ddom";
			}
			if (isSet("before_G")) {
				tabBefore[0][c++] = (isSet("perHectare")) ? "G/ha" : "G";
			}
			if (isSet("before_V")) {
				tabBefore[0][c++] = (isSet("perHectare")) ? "V/ha" : "V";
			}

			c = 0; // column number
			if (isSet("current_Hg")) {
				tabCurrent[0][c++] = "Hg";
			}
			if (isSet("current_Hdom")) {
				tabCurrent[0][c++] = "Hdom";
			}
			if (isSet("current_N")) {
				tabCurrent[0][c++] = (isSet("perHectare")) ? "N/ha" : "N";
			}
			if (isSet("current_Cg")) {
				tabCurrent[0][c++] = "Cg";
			}
			if (isSet("current_Cdom")) {
				tabCurrent[0][c++] = "Cdom";
			}
			if (isSet("current_Dg")) {
				tabCurrent[0][c++] = "Dg";
			}
			if (isSet("current_Ddom")) {
				tabCurrent[0][c++] = "Ddom";
			}
			if (isSet("current_G")) {
				tabCurrent[0][c++] = (isSet("perHectare")) ? "G/ha" : "G";
			}
			if (isSet("current_V")) {
				tabCurrent[0][c++] = (isSet("perHectare")) ? "V/ha" : "V";
			}

			c = 0; // column number
			if (isSet("thin_Kg")) {
				tabThin[0][c++] = "Kg";
			}
			if (isSet("thin_IntensityN")) {
				tabThin[0][c++] = "N ratio";
			}
			if (isSet("thin_IntensityG")) {
				tabThin[0][c++] = "G ratio";
			}
			if (isSet("thin_IntensityV")) {
				tabThin[0][c++] = "V ratio";
			}

			// Data extraction

			int line = 1;
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step step = (Step) i.next();
				if (!step.getScene().isInterventionResult()) {
					continue;
				} // next iteration

				// Previous step
				Step prevStep = null;
				prevStep = (Step) step.getFather();

				// Consider restriction to one particular group if needed
				GScene stand = step.getScene();
				Collection trees = null; // fc - 24.3.2004
				try {
					trees = ((TreeCollection) stand).getTrees();
				} catch (Exception e) {
				} // fc - 24.3.2004

				GScene prevStand = null;
				try {
					prevStand = prevStep.getScene();
				} catch (Exception e) {
				}
				Collection prevTrees = null; // fc - 24.3.2004
				try {
					prevTrees = ((TreeCollection) prevStand).getTrees();
				} catch (Exception e) {
				} // fc - 24.3.2004

				// Filtrable fil = doFilter ((Filtrable) stand);
				// stand = (GStand) fil;

				c = 0; // column number
				int date = stand.getDate();
				tabBefore[line][c++] = "" + date;

				// Before variables
				// ------------------------------------------------------------------
				if (isSet("before_Hg")) {
					double Hg = -1d; // default = "not calculable"
					try {
						Hg = ((HgProvider) methodProvider).getHg(prevStand, prevTrees);
					} catch (Exception e) {
					}
					if (Hg == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(Hg);
				}
				if (isSet("before_Hdom")) {
					double Hdom = -1d; // default = "not calculable"
					try {
						Hdom = ((HdomProvider) methodProvider).getHdom(prevStand, prevTrees);
					} catch (Exception e) {
					}
					if (Hdom == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(Hdom);
				}

				double before_N = -1d; // ----- before_N is always computed
				try {
					before_N = ((NProvider) methodProvider).getN(prevStand, prevTrees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("before_N")) {
					if (before_N == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + (int) (before_N);
				}
				if (isSet("before_Cg")) { // ----- before_Cg
					double before_Cg = -1d;
					try {
						before_Cg = Math.PI * ((DgProvider) methodProvider).getDg(prevStand, prevTrees);
					} catch (Exception e) {
					}
					if (before_Cg == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(before_Cg);
				}
				if (isSet("before_Cdom")) {
					double before_Cdom = -1d; // default = "not calculable"
					try {
						before_Cdom = Math.PI * ((DdomProvider) methodProvider).getDdom(prevStand, prevTrees);
					} catch (Exception e) {
					}
					if (before_Cdom == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(before_Cdom);
				}
				if (isSet("before_Dg")) { // ----- before_Dg
					double before_Dg = -1d;
					try {
						before_Dg = ((DgProvider) methodProvider).getDg(prevStand, prevTrees);
					} catch (Exception e) {
					}
					if (before_Dg == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(before_Dg);
				}
				if (isSet("before_Ddom")) {
					double before_Ddom = -1d; // default = "not calculable"
					try {
						before_Ddom = ((DdomProvider) methodProvider).getDdom(prevStand, prevTrees);
					} catch (Exception e) {
					}
					if (before_Ddom == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(before_Ddom);
				}
				double before_G = -1d; // ----- before_G is always computed
				try {
					before_G = ((GProvider) methodProvider).getG(prevStand, prevTrees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("before_G")) {
					if (before_G == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(before_G);
				}
				double before_V = -1d; // ----- before_G is always computed
				try {
					before_V = ((VProvider) methodProvider).getV(prevStand, prevTrees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("before_V")) {
					if (before_V == -1d)
						tabBefore[line][c++] = "";
					else
						tabBefore[line][c++] = "" + formater.format(before_V);
				}

				// Current variables
				// ------------------------------------------------------------------
				// "current" for the stand after thinning : alphabetic order
				// before then current; TL 10052005
				c = 0; // column number

				if (isSet("current_Hg")) {
					double Hg = -1d; // default = "not calculable"
					try {
						Hg = ((HgProvider) methodProvider).getHg(stand, trees);
					} catch (Exception e) {
					}
					if (Hg == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(Hg);
				}
				if (isSet("current_Hdom")) {
					double Hdom = -1d; // default = "not calculable"
					try {
						Hdom = ((HdomProvider) methodProvider).getHdom(stand, trees);
					} catch (Exception e) {
					}
					if (Hdom == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(Hdom);
				}

				double N = -1d; // ----- N is always computed
				double thin_NRatio = -1d; // cm tl 18 05 2015
				try {
					N = ((NProvider) methodProvider).getN(stand, trees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("current_N")) {
					if (N == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + (int) N; // N integer (phd)
				}
				if (isSet("current_Cg")) {
					double Cg = -1d; // default = "not calculable"
					try {
						Cg = Math.PI * ((DgProvider) methodProvider).getDg(stand, trees);
					} catch (Exception e) {
					}
					if (Cg == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(Cg);
				}
				if (isSet("current_Cdom")) {
					double Cdom = -1d; // default = "not calculable"
					try {
						Cdom = Math.PI * ((DdomProvider) methodProvider).getDdom(stand, trees);
					} catch (Exception e) {
					}
					if (Cdom == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(Cdom);
				}
				if (isSet("current_Dg")) {
					double Dg = -1d; // default = "not calculable"
					try {
						Dg = ((DgProvider) methodProvider).getDg(stand, trees);
					} catch (Exception e) {
					}
					if (Dg == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(Dg);
				}
				if (isSet("current_Ddom")) {
					double Ddom = -1d; // default = "not calculable"
					try {
						Ddom = ((DdomProvider) methodProvider).getDdom(stand, trees);
					} catch (Exception e) {
					}
					if (Ddom == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(Ddom);
				}
				double G = -1d; // ----- G is always computed
				double thin_GRatio = -1d;
				try {
					G = ((GProvider) methodProvider).getG(stand, trees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("current_G")) {
					if (G == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(G);
				}
				double V = -1d; // ----- G is always computed
				double thin_VRatio = -1d;
				try {
					V = ((VProvider) methodProvider).getV(stand, trees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("current_V")) {
					if (V == -1d)
						tabCurrent[line][c++] = "";
					else
						tabCurrent[line][c++] = "" + formater.format(V);
				}
				// Stand variables
				// ------------------------------------------------------------------
				c = 0; // column number

				if (isSet("thin_Kg")) {
					double Kg = -1d; // default = "not calculable"
					try {
						Kg = ((KgProvider) methodProvider).getKg(stand, trees);
					} catch (Exception e) {
					}
					if (Kg == -1d)
						tabThin[line][c++] = "";
					else
						tabThin[line][c++] = "" + formater.format(Kg);
				}
				if (isSet("thin_IntensityN")) {
					if (N != -1d && before_N != -1d) {
						thin_NRatio = (before_N - N) / (before_N * 1d);
					}
					if (thin_NRatio == -1d)
						tabThin[line][c++] = "";
					else
						tabThin[line][c++] = "" + formater.format(thin_NRatio);
				}
				if (isSet("thin_IntensityG")) {
					if (G != -1d && before_G != -1d) {
						thin_GRatio = (before_G - G) / before_G;
					}
					if (thin_GRatio == -1d)
						tabThin[line][c++] = "";
					else
						tabThin[line][c++] = "" + formater.format(thin_GRatio);
				}
				if (isSet("thin_IntensityV")) {
					if (V != -1d && before_V != -1d) {
						thin_VRatio = (before_V - V) / before_V;
					}
					if (thin_VRatio == -1d)
						tabThin[line][c++] = "";
					else
						tabThin[line][c++] = "" + formater.format(thin_VRatio);
				}

				line++;
			}

			tables.clear();
			if (tabBefore != null) {
				tables.add(tabBefore);
			}
			if (tabCurrent != null) {
				tables.add(tabCurrent);
			}
			if (tabThin != null) {
				tables.add(tabThin);
			}

			// ~ System.out.println ("DEThinTable : extraction done");

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEThinTable.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * This prefix is built depending on current settings. ex: "+ 25 years /ha"
	 */
	public String getNamePrefix() {
		String prefix = "";
		try {
			if (isCommonGrouper() && isGrouperMode()
					&& GrouperManager.getInstance().getGrouperNames().contains(getGrouperName())) {
				prefix += getGrouperName() + " - ";
			}
			if (isSet("perHectare")) {
				prefix += "/ha - ";
			}
		} catch (Exception e) {
		} // if trouble, prefix is empty
		return prefix;
	}

	/**
	 * From DataFormat interface.
	 */
	// fc - 21.4.2004 - DataExtractor.getCaption () is better
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
