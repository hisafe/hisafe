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
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFTables;
import capsis.extension.datarenderer.DRTables;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.Flag;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.HgProvider;
import capsis.util.methodprovider.KgProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.ProdGProvider;
import capsis.util.methodprovider.ProdVProvider;
import capsis.util.methodprovider.SHBProvider;
import capsis.util.methodprovider.TreeVProvider;
import capsis.util.methodprovider.VProvider;

/**
 * A stand table report.
 * 
 * @author F. de Coligny - February 2002, C. Meredieu, T. Labbé, S. Perret -
 *         January 2004, F. de Coligny - August 2005: added common group
 *         property and use of doFilter (), Mathieu Fortin - May 2009: setDigit
 *         function added and Stand_N format changed to double with one digit,
 *         F. de Coligny - December 2015: removed completely visibleStepsOnly
 *         (was half disabled), reformatted code, removed old comments, checked
 *         the global working
 */
public class DEStandTable extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEStandTable");
	}

	protected Collection tables;
	protected Collection titles;
	protected MethodProvider methodProvider;
	protected NumberFormat formater;

	/**
	 * Constructor.
	 */
	public DEStandTable() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEStandTable(GenericExtensionStarter s) {
		super(s);

		try {
			tables = new ArrayList();
			titles = new ArrayList();
			documentationKeys.add("VProvider"); // tl cm - 29.1.2007

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEStandTable.c ()", "Exception during construction : ", e);
		}
	}

	/**
	 * This method is an example, the code inside may be copied and adapted in
	 * scripts to write the result in an export file. A convenient static method
	 * to apply the extractor on a given step and get the result in a String
	 * array.
	 * 
	 * <pre>
	 * String[][] array = DEStandTable.createStandTable(step);
	 * </pre>
	 */
	static public String[][] createStandTable(Step step) {

		// Create the dataExtractor for this step
		GenericExtensionStarter st = new GenericExtensionStarter();
		st.setStep(step);
		DEStandTable ex = new DEStandTable(st) {
			public void setConfigProperties() {
				// Choose the properties to be enabled, see setConfigProperties
				// () below
				String[] props = { "astand_N", "astand_G", "astand_V", "astand_Ho", "bthi_N", "bthi_G", "bthi_V",
						"cpro_Vecl", "cpro_V", "cpro_G" };
				for (String p : props) {
					addBooleanProperty(p, true);
				}
			}
		};
		
		ex.setBooleanProperty("perHectare", true); // fc-27.8.2021
//		ex.setBooleanProperty("perHectare", true);

		// Run the dataExtractor
		ex.doExtraction();

		return DRTables.createPrintableTable(ex);

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
			Log.println(Log.ERROR, "DEStandTable.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEStandTable.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. de Coligny, C. Meredieu, T. Labbé, S. Perret, M. Fortin, P. Dreyfus";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEStandTable.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.3";
	}

	// nb-08.08.2018
	// fc-2.12.2015 changed version to 1.3
	//public static final String VERSION = "1.3";

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.DRTables";
	}

	/**
	 * This function sets the number of digit of this.formater
	 * 
	 * @param numberDigit
	 *            A numberFormat object
	 */
	private NumberFormat setDigit(int numberDigit) {
		formater = NumberFormat.getInstance(Locale.US); // to impose decimal dot
														// instead of "," for
														// french number format
		formater.setMaximumFractionDigits(numberDigit);
		formater.setMinimumFractionDigits(numberDigit);
		formater.setGroupingUsed(false);
		return formater;
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(TREE_GROUP); // fc - 25.8.2005

		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		// fc-2.12.2015 was half disabled, now disabled
		// addBooleanProperty("visibleStepsOnly");

		// fc-2.12.2015 added more default columns below (more 'true')
		addBooleanProperty("astand_N", true);
		addBooleanProperty("astand_G", true);
		addBooleanProperty("astand_V", true);
		addBooleanProperty("astand_S");
		addBooleanProperty("astand_Ho", true);
		addBooleanProperty("astand_Do", true); // add cm 28-6-03
		addBooleanProperty("astand_Hg", true); // add cm 28-6-03
		addBooleanProperty("astand_Dg", true); // add cm 28-6-03
		addBooleanProperty("astand_Cg"); // add cm tl 19-10-15
		addBooleanProperty("astand_Vg"); // add cm 28-6-03
		addBooleanProperty("astand_Hdom_Dg"); // PhD 2009-06-03
		addBooleanProperty("astand_Hg_Dg"); // add cm tl 19-10-15
		addBooleanProperty("bthi_N", true);
		addBooleanProperty("bthi_G", true);
		addBooleanProperty("bthi_V", true);
		addBooleanProperty("bthi_Vm");
		addBooleanProperty("bthi_Dg"); // add cm 28-6-03
		addBooleanProperty("bthi_Kg"); // PhD 2009-06-03
		addBooleanProperty("bthi_IntensityN"); // add cm lt 18 05 2015
		addBooleanProperty("bthi_IntensityG"); // add cm lt 18 05 2015
		addBooleanProperty("bthi_IntensityV"); // add cm lt 18 05 2015
		addBooleanProperty("cpro_Vecl", true);
		addBooleanProperty("cpro_V", true);
		addBooleanProperty("cpro_G", true);

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
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		// Get the method provider for the model in use
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			Flag flag = new Flag(); // fc-29.9.2005 - method provider flag, see
									// below

			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			int n = steps.size();

			// fc-2.12.2015 was half disabled, now disabled
			// if (isSet("visibleStepsOnly")) {
			// n = 0;
			// for (Iterator i = steps.iterator(); i.hasNext();) {
			// Step s = (Step) i.next();
			// if (s.isVisible()) {
			// n++;
			// }
			// }
			// }

			int sizeStand = 1;
			if (isSet("astand_N")) {
				sizeStand++;
			}
			if (isSet("astand_G")) {
				sizeStand++;
			}
			if (isSet("astand_V")) {
				sizeStand++;
			}
			if (isSet("astand_S")) {
				sizeStand++;
			}
			if (isSet("astand_Ho")) {
				sizeStand++;
			}
			if (isSet("astand_Do")) {
				sizeStand++;
			} // add cm -28-8-03
			if (isSet("astand_Hg")) {
				sizeStand++;
			} // add cm -28-8-03
			if (isSet("astand_Dg")) {
				sizeStand++;
			} // add cm -28-8-03
			if (isSet("astand_Cg")) {
				sizeStand++;
			} // add cm tl 19-10-15
			if (isSet("astand_Vg")) {
				sizeStand++;
			} // add cm -28-8-03
			if (isSet("astand_Hdom_Dg")) {
				sizeStand++;
			} // PhD 2009-06-03
			if (isSet("astand_Hg_Dg")) {
				sizeStand++;
			} // add cm tl 19-10-15
			int sizeThinning = 0;
			if (isSet("bthi_N")) {
				sizeThinning++;
			}
			if (isSet("bthi_G")) {
				sizeThinning++;
			}
			if (isSet("bthi_V")) {
				sizeThinning++;
			}
			if (isSet("bthi_Vm")) {
				sizeThinning++;
			}
			if (isSet("bthi_Dg")) {
				sizeThinning++;
			} // add cm -28-8-03
			if (isSet("bthi_Kg")) {
				sizeThinning++;
			} // PhD 2009-06-03
			if (isSet("bthi_IntensityN")) {
				sizeThinning++;
			} // add cm lt 18 05 2015
			if (isSet("bthi_IntensityG")) {
				sizeThinning++;
			} // add cm lt 18 05 2015
			if (isSet("bthi_IntensityV")) {
				sizeThinning++;
			} // add cm lt 18 05 2015
			int sizeProduction = 0;
			if (isSet("cpro_Vecl")) {
				sizeProduction++;
			}
			if (isSet("cpro_V")) {
				sizeProduction++;
			}
			if (isSet("cpro_G")) {
				sizeProduction++;
			}

			n += 1; // add first line for columns headers
			String[][] tabStand = null;
			String[][] tabThinning = null;
			String[][] tabProduction = null;
			if (sizeStand != 0) {
				tabStand = new String[n][sizeStand];
			}
			if (sizeThinning != 0) {
				tabThinning = new String[n][sizeThinning];
			}
			if (sizeProduction != 0) {
				tabProduction = new String[n][sizeProduction];
			}

			// Tables titles
			titles.clear();
			if (sizeStand != 0) {
				titles.add(Translator.swap("DEStandTable.stand"));
			}
			if (sizeThinning != 0) {
				titles.add(Translator.swap("DEStandTable.thinning"));
			}
			if (sizeProduction != 0) {
				titles.add(Translator.swap("DEStandTable.production"));
			}

			// Column headers
			int c = 0; // column number
			tabStand[0][c++] = "Date";
			if (isSet("astand_N")) {
				tabStand[0][c++] = (isSet("perHectare")) ? "N/ha" : "N";
			}
			if (isSet("astand_G")) {
				tabStand[0][c++] = (isSet("perHectare")) ? "G/ha" : "G";
			}
			if (isSet("astand_V")) {
				tabStand[0][c++] = (isSet("perHectare")) ? "V/ha" : "V";
			}
			if (isSet("astand_S")) {
				tabStand[0][c++] = "S";
			}
			if (isSet("astand_Ho")) {
				tabStand[0][c++] = "Ho";
			}
			if (isSet("astand_Do")) {
				tabStand[0][c++] = "Do";
			} // add cm -28-8-03
			if (isSet("astand_Hg")) {
				tabStand[0][c++] = "Hg";
			} // add cm -28-8-03
			if (isSet("astand_Dg")) {
				tabStand[0][c++] = "Dg";
			} // add cm -28-8-03
			if (isSet("astand_Cg")) {
				tabStand[0][c++] = "Cg";
			} // add cm tl 19-10-15
			if (isSet("astand_Vg")) {
				tabStand[0][c++] = "Vg";
			} // add cm -28-8-03
			if (isSet("astand_Hdom_Dg")) {
				tabStand[0][c++] = "Hdom_Dg";
			} // PhD 2009-06-03
			if (isSet("astand_Hg_Dg")) {
				tabStand[0][c++] = "Hg_Dg";
			} // add cm tl 19-10-15

			c = 0; // column number
			if (isSet("bthi_N")) {
				tabThinning[0][c++] = (isSet("perHectare")) ? "N/ha" : "N";
			}
			if (isSet("bthi_G")) {
				tabThinning[0][c++] = (isSet("perHectare")) ? "G/ha" : "G";
			}
			if (isSet("bthi_V")) {
				tabThinning[0][c++] = (isSet("perHectare")) ? "V/ha" : "V";
			}
			if (isSet("bthi_Vm")) {
				tabThinning[0][c++] = "Vm";
			}
			if (isSet("bthi_Dg")) {
				tabThinning[0][c++] = "Dg";
			} // add cm -28-8-03
			if (isSet("bthi_Kg")) {
				tabThinning[0][c++] = "Kg";
			}
			if (isSet("bthi_IntensityN")) {
				tabThinning[0][c++] = "N ratio";
			} // add cm lt 18 05 2015
			if (isSet("bthi_IntensityG")) {
				tabThinning[0][c++] = "G ratio";
			} // add cm lt 18 05 2015
			if (isSet("bthi_IntensityV")) {
				tabThinning[0][c++] = "V ratio";
			} // add cm lt 18 05 2015

			c = 0; // column number
			if (isSet("cpro_Vecl")) {
				tabProduction[0][c++] = (isSet("perHectare")) ? "V ecl/ha" : "V ecl";
			}
			if (isSet("cpro_V")) {
				tabProduction[0][c++] = (isSet("perHectare")) ? "V prod/ha" : "V prod";
			}
			if (isSet("cpro_G")) {
				tabProduction[0][c++] = (isSet("perHectare")) ? "G prod/ha" : "G prod";
			}

			// Data extraction
			double cumulThiV = 0d;
			double cumulThiG = 0d;

			int line = 1;
			String buffer = "";
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step step = (Step) i.next();

				// fc-2.12.2015 I found this line commented: visibleStepsOnly is
				// half disabled...
				// if (isSet ("visibleStepsOnly") && !step.isVisible ())
				// {continue;} // next iteration

				boolean interventionStep = step.getScene().isInterventionResult();

				// Previous step
				Step prevStep = null;

				// fc-2.12.2015 I found this line commented: visibleStepsOnly is
				// half disabled...
				// if (isSet ("visibleStepsOnly")) {
				// prevStep = (Step) step.getVisibleFather ();
				// } else {

				prevStep = (Step) step.getFather();

				// }

				// Consider restriction to one particular group if needed - fc -
				// 25.8.2005
				GScene stand = step.getScene();
				Collection trees = null;
				try {
					trees = doFilter(stand);
				} catch (Exception e) {
				}

				GScene prevStand = null;
				try {
					prevStand = prevStep.getScene();
				} catch (Exception e) {
				}
				Collection prevTrees = null;
				try {
					prevTrees = doFilter(prevStand);
				} catch (Exception e) {
				}

				// Stand variables
				// ------------------------------------------------------------------
				c = 0; // column number
				int date = stand.getDate();
				tabStand[line][c++] = "" + date;

				double N = -1d; // ----- N is always computed - Nprovider is
								// compulsory (matchWith)
				try {
					N = ((NProvider) methodProvider).getN(stand, trees) * coefHa;
				} catch (Exception e) {
				}
				if (isSet("astand_N")) {
					if (N == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(1).format(N); // N
																			// is
																			// now
																			// a
																			// double
																			// with
																			// one
																			// digit
				}
				double G = -1d; // ----- G is always computed - Gprovider is
								// compulsory (matchWith)
				try {
					G = ((GProvider) methodProvider).getG(stand, trees) * coefHa;
				} catch (Exception e) {
				} // used for cpro_G
				if (isSet("astand_G")) {
					if (G == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(G);
				}

				// - V ------------------
				double V = -1d; // ----- default = "not calculable"
				try {
					V = ((VProvider) methodProvider).getV(stand, trees) * coefHa;
				} catch (Exception e) {
				} // used for bthi_V and cpro_V
				if (isSet("astand_V")) {
					if (V == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(V);
				}
				if (isSet("astand_S")) {
					double S = -1d; // default = "not calculable"
					// try {S = ((SHBProvider) methodProvider).getSHB (stand) *
					// coefHa;} catch (Exception e) {}
					try {
						S = ((SHBProvider) methodProvider).getSHB(stand, trees);
					} catch (Exception e) {
					}
					if (S == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(S);
				}
				if (isSet("astand_Ho")) {
					double Hdom = -1d; // default = "not calculable"
					// -- bug correction - fc - 31.3.2003 - Hdom was * by coefHa
					// -> wrong value
					try {
						Hdom = ((HdomProvider) methodProvider).getHdom(stand, trees);
					} catch (Exception e) {
					}
					if (Hdom == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(Hdom);
				}
				if (isSet("astand_Do")) {
					double Do = -1d; // default = "not calculable"
					try {
						Do = ((DdomProvider) methodProvider).getDdom(stand, trees);
					} catch (Exception e) {
					}
					if (Do == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(Do);
				}
				if (isSet("astand_Hg")) {
					double Hg = -1d; // default = "not calculable"
					try {
						Hg = ((HgProvider) methodProvider).getHg(stand, trees);
					} catch (Exception e) {
					}
					if (Hg == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(Hg);
				}
				if (isSet("astand_Dg")) {
					double Dg = -1d; // default = "not calculable"
					try {
						Dg = ((DgProvider) methodProvider).getDg(stand, trees);
					} catch (Exception e) {
					}
					if (Dg == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(Dg);
				}
				if (isSet("astand_Cg")) {
					double Cg = -1d; // default = "not calculable"
					try {
						Cg = ((DgProvider) methodProvider).getDg(stand, trees) * Math.PI;
					} catch (Exception e) {
					}
					if (Cg == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(2).format(Cg);
				}
				if (isSet("astand_Vg")) {
					double Vg = -1d; // default = "not calculable"
					try {
						Vg = ((TreeVProvider) methodProvider).getTreeV(
								// N.B. : will use the dominant (or main)
								// species of the stand
								((DgProvider) methodProvider).getDg(stand, trees),
								((HgProvider) methodProvider).getHg(stand, trees), stand);
					} catch (Exception e) {
					}
					// if (V != -1 && N != -1) {Vg = V / N;}
					if (Vg == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(3).format(Vg);
				}
				if (isSet("astand_Hdom_Dg")) { // PhD 2009-06-03
					double Hdom_Dg = -1d; // default = "not calculable"
					double _Hdom_ = -1d;
					double _Dg_ = -1d;
					try {
						_Hdom_ = ((HdomProvider) methodProvider).getHdom(stand, trees);
					} catch (Exception e) {
					}
					try {
						_Dg_ = ((DgProvider) methodProvider).getDg(stand, trees);
					} catch (Exception e) {
					}
					if (_Dg_ > 0) {
						Hdom_Dg = Math.floor(_Hdom_ / (_Dg_ / 100d) + 0.5);
					} // N.B. : Math.floor( +0.5) => rounded to nearest int
						// value
					if (Hdom_Dg == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(0).format(Hdom_Dg);
				}
				if (isSet("astand_Hg_Dg")) {
					double Hg_Dg = -1d; // default = "not calculable"
					double _Hg_ = -1d;
					double _Dg_ = -1d;
					try {
						_Hg_ = ((HgProvider) methodProvider).getHg(stand, trees);
					} catch (Exception e) {
					}
					try {
						_Dg_ = ((DgProvider) methodProvider).getDg(stand, trees);
					} catch (Exception e) {
					}
					if (_Dg_ > 0) {
						Hg_Dg = Math.floor(_Hg_ / (_Dg_ / 100d) + 0.5);
					} // N.B. : Math.floor( +0.5) => rounded to nearest int
						// value
					if (Hg_Dg == -1d)
						tabStand[line][c++] = "";
					else
						tabStand[line][c++] = "" + setDigit(0).format(Hg_Dg);
				}

				// Thinning variables
				// ------------------------------------------------------------------
				c = 0; // column number
				double bthi_N = -1d; // ----- bthi_N is always computed
				double bthi_NRatio = -1d; // add cm tl 18 10 2015
				if (interventionStep) {
					// double N = -1d;
					double prevN = -1d;
					// try {N = ((NProvider) methodProvider).getN (stand) *
					// coefHa;} catch (Exception e) {}
					try {
						prevN = ((NProvider) methodProvider).getN(prevStand, prevTrees) * coefHa;
					} catch (Exception e) {
					}
					if (N != -1 && prevN != -1) {
						bthi_N = prevN - N;
						bthi_NRatio = (bthi_N / prevN) * 100d;
					}
				}
				if (isSet("bthi_N")) {
					buffer = "";
					if (interventionStep) {
						// buffer = ""+ setDigit(2).format (bthi_N);
						if (bthi_N == -1d)
							buffer = "";
						else if (bthi_N <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(1).format(bthi_N);
					}
					tabThinning[line][c++] = buffer;
				}

				double bthi_G = -1d; // ----- bthi_G is always computed
				double bthi_GRatio = -1d; // add cm tl 18 10 2015
				if (interventionStep) {
					double prevG = -1d;
					try {
						prevG = ((GProvider) methodProvider).getG(prevStand, prevTrees) * coefHa;
					} catch (Exception e) {
					}
					if (G != -1 && prevG != -1) {
						bthi_G = prevG - G;
					}
					if (bthi_G != -1) {
						cumulThiG += bthi_G;
					}
					if (G != -1 && prevG != -1) {
						bthi_GRatio = (bthi_G / prevG) * 100d;
					}
				}
				if (isSet("bthi_G")) {
					buffer = "";
					if (interventionStep) {
						if (bthi_G == -1d)
							buffer = "";
						else if (bthi_N <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_G);
					}
					tabThinning[line][c++] = buffer;
				}

				// - bthi_V -------------
				double bthi_V = -1d; // -----default = "not calculable"
				double bthi_VRatio = -1d;
				if (interventionStep) {
					double prevV = -1d;
					try {
						prevV = ((VProvider) methodProvider).getV(prevStand, prevTrees) * coefHa;
					} catch (Exception e) {
					}
					if (V != -1 && prevV != -1) {
						bthi_V = prevV - V;
					}
					if (bthi_V != -1) {
						cumulThiV += bthi_V;
						bthi_VRatio = (bthi_V / prevV) * 100d;
					}
				}
				if (isSet("bthi_V")) {
					buffer = "";
					if (interventionStep) {
						if (bthi_V == -1d)
							buffer = "";
						else if (bthi_N <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_V);
					}
					tabThinning[line][c++] = buffer;
				}
				if (isSet("bthi_Vm")) { // ----- bthi_Vm
					buffer = "";
					if (interventionStep) {
						double bthi_Vm = -1d;
						if (bthi_V != -1 && bthi_N != -1) {
							bthi_Vm = bthi_V / bthi_N;
						}
						if (bthi_Vm == -1d)
							buffer = "";
						else if (bthi_N <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(3).format(bthi_Vm);
					}
					tabThinning[line][c++] = buffer;
				}
				if (isSet("bthi_Dg")) { // ----- bthi_Dg
					buffer = "";
					if (interventionStep) {
						double bthi_Dg = -1d;
						if (bthi_G != -1 && bthi_N != -1) {
							bthi_Dg = (Math.sqrt((bthi_G / bthi_N) / Math.PI) * 2) * 100;
						}
						if (bthi_Dg == -1d)
							buffer = "";
						else if (bthi_N <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_Dg);
					}
					tabThinning[line][c++] = buffer;

				}

				double bthi_Kg = -1d; // ----- bthi_Kg // PhD 2009-06-03
				if (interventionStep) {
					try {
						bthi_Kg = ((KgProvider) methodProvider).getKg(stand, trees);
					} catch (Exception e) {
					}// PhD 2009-06-03
				}
				if (isSet("bthi_Kg")) {
					buffer = "";
					if (interventionStep) {
						if (bthi_Kg == -1d)
							buffer = "";
						else if (bthi_N <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_Kg);
					}
					tabThinning[line][c++] = buffer;
				}

				if (isSet("bthi_IntensityN")) {
					buffer = "";
					if (interventionStep) {
						// buffer = ""+ setDigit(2).format (bthi_N);
						if (bthi_NRatio == -1d)
							buffer = "";
						else if (bthi_NRatio <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_NRatio);
					}
					tabThinning[line][c++] = buffer;
				}
				if (isSet("bthi_IntensityG")) {
					buffer = "";
					if (interventionStep) {
						if (bthi_GRatio == -1d)
							buffer = "";
						else if (bthi_GRatio <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_GRatio);
					}
					tabThinning[line][c++] = buffer;
				}
				if (isSet("bthi_IntensityV")) {
					buffer = "";
					if (interventionStep) {
						if (bthi_VRatio == -1d)
							buffer = "";
						else if (bthi_VRatio <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(bthi_VRatio);
					}
					tabThinning[line][c++] = buffer;
				}

				// Production variables
				// ------------------------------------------------------------------
				c = 0; // column number
				if (isSet("cpro_Vecl")) {
					double cpro_Vecl = cumulThiV;
					buffer = "";
					if (interventionStep) {
						if (cpro_Vecl == -1d)
							buffer = "";
						else if (cpro_Vecl <= 0)
							buffer = "0";
						else
							buffer = "" + setDigit(2).format(cpro_Vecl);
					}
					tabProduction[line][c++] = buffer;
				}

				if (isSet("cpro_V")) {
					buffer = "";
					double cpro_V = -1d;
					if (methodProvider instanceof ProdVProvider) {
						// cpro_V = ((ProdVProvider) methodProvider).getProdV
						// (stand, trees, flag);
						cpro_V = ((ProdVProvider) methodProvider).getProdV(stand, trees, flag) * coefHa; // PhD
																											// 30.9.2005
					} else {
						if (V != -1) {
							cpro_V = V + cumulThiV;
						}
					}
					if (cpro_V == -1d) {
						buffer = "";
					} else {
						buffer = "" + setDigit(2).format((cpro_V) /** coefHa */
						);
					} // PhD 30.9.2005
					tabProduction[line][c++] = buffer;
				}
				if (isSet("cpro_G")) {
					buffer = "";
					double cpro_G = -1d;
					if (methodProvider instanceof ProdGProvider) {
						// cpro_G = ((ProdGProvider) methodProvider).getProdG
						// (stand, trees);
						cpro_G = ((ProdGProvider) methodProvider).getProdG(stand, trees) * coefHa; // PhD
																									// 30.9.2005
					} else {
						if (G != -1) {
							cpro_G = G + cumulThiG;
						}
					}
					if (cpro_G == -1d) {
						buffer = "";
					} else {
						buffer = "" + setDigit(2).format((cpro_G) /** coefHa */
						);
					} // PhD 30.9.2005
					tabProduction[line][c++] = buffer;
				}

				line++;
			}

			tables.clear();
			if (tabStand != null) {
				tables.add(tabStand);
			}
			if (tabThinning != null) {
				tables.add(tabThinning);
			}
			if (tabProduction != null) {
				tables.add(tabProduction);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEStandTable.doExtraction ()", "Exception: ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * This prefix is built depending on current settings. e.g. "+ 25 years /ha"
	 * Can be used in getName () in subclasses -> title bar label.
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
	// fc-21.4.2004 DataExtractor.getCaption () is better
	// public String getCaption () {
	// return getStep ().getCaption ();
	// }

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
