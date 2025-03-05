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
import capsis.lib.math.Stat;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.NProvider;

/**
 * A stand table report.
 * 
 * @author F. de Coligny - February 2002, updated C. Meredieu, T. Labbé, S.
 *         Perret - January 2004
 */
public class DEDynamicsAnalysis extends PaleoDataExtractor implements DFTables {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEDynamicsAnalysis");
	}

	protected Collection tables;
	protected Collection titles;
	protected MethodProvider methodProvider;

	protected NumberFormat f;

	/**
	 * Constructor.
	 */
	public DEDynamicsAnalysis() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEDynamicsAnalysis(GenericExtensionStarter s) {
		super(s);

		try {
			tables = new ArrayList();
			titles = null;

			// Used to format decimal part with 2 digits only
			f = NumberFormat.getInstance(Locale.ENGLISH);
			f.setGroupingUsed(false);
			f.setMaximumFractionDigits(3);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEDynamicsAnalysis.c ()", "Exception during construction : ", e);
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

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEDynamicsAnalysis.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEDynamicsAnalysis.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEDynamicsAnalysis.description");
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
		// ~ addBooleanProperty ("visibleStepsOnly");
		addBooleanProperty("g", true);
		addIntProperty("t0", 0);
		// -----addBooleanProperty ("useDateT0", false);
		// -----addBooleanProperty ("dateT0_useDateT0", false);
		// -----addIntProperty ("dateT0_t0", 0);
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

		// Retrieve method provider
		// methodProvider = MethodProviderFactory.getMethodProvider
		// (step.getScenario ().getModel ());
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// ~ StringBuffer consideredSteps = new StringBuffer
			// ("consideredSteps: ");

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);
			int date0 = getIntProperty("t0");
			int date1 = step.getScene().getDate();

			int n = steps.size();

			double[] Ns = null;
			double[] Gs = null;

			int k = 0;
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step step = (Step) i.next();

				// Only one value per date, intervention results in new value
				// for same date -> discard
				boolean interventionStep = step.getScene().isInterventionResult();
				if (interventionStep) {
					continue;
				}

				GScene stand = step.getScene();
				if (stand.getDate() < date0) {
					continue;
				}

				// fc - 8.4.2004 - get trees for tree collections
				Collection trees = null;
				try {
					trees = ((TreeCollection) stand).getTrees();
				} catch (Exception e) {
				}

				double N = ((NProvider) methodProvider).getN(stand, trees) * coefHa;
				if (Ns == null) {
					Ns = new double[n];
				}
				Ns[k] = N;

				if (isSet("g")) {
					try {
						double G = ((GProvider) methodProvider).getG(stand, trees) * coefHa; // fc
																								// -
																								// 24.3.2004
						if (Gs == null) {
							Gs = new double[n];
						}
						Gs[k] = G;
					} catch (Exception e) {
					}
				}

				k++;
			}

			if (Ns != null) {
				double[] aux = new double[k];
				System.arraycopy(Ns, 0, aux, 0, k);
				Ns = aux;
			}
			if (Gs != null) {
				double[] aux = new double[k];
				System.arraycopy(Gs, 0, aux, 0, k);
				Gs = aux;
			}

			// ~ System.out.println (consideredSteps.toString ());

			// Compute number of columns
			//
			int nCol = 2; // At least 2 columns : labels + N column
			if (Gs != null) {
				nCol++;
			}

			String[][] tab = new String[5][nCol]; // Table size

			tab[0][0] = Translator.swap("DEDynamicsAnalysis.dates") + " [" + date0 + ", " + date1 + "]";
			tab[1][0] = Translator.swap("DEDynamicsAnalysis.temporalMean");
			tab[2][0] = Translator.swap("DEDynamicsAnalysis.temporalStandardDeviation");
			tab[3][0] = Translator.swap("DEDynamicsAnalysis.constancy");

			int col = 0;

			// N column
			col++;
			double Nmean = 0;
			double NstandardDeviation = 0;
			double Nconstancy = 0;
			tab[0][col] = Translator.swap("DEDynamicsAnalysis.N") + (isSet("perHectare") ? " (/ha)" : "");
			try {
				Nmean = Stat.mean(Ns);
				tab[1][col] = f.format(Nmean);
			} catch (Exception e) {
				tab[1][col] = "-";
			}
			try {
				NstandardDeviation = Stat.standardDeviation(Nmean, Ns);
				tab[2][col] = f.format(NstandardDeviation);
			} catch (Exception e) {
				tab[2][col] = "-";
			}
			try {
				Nconstancy = Stat.constancy(Nmean, NstandardDeviation);
				tab[3][col] = f.format(Nconstancy);
			} catch (Exception e) {
				tab[3][col] = "-";
			}

			// G column
			if (Gs != null) {
				col++;
				double Gmean = 0;
				double GstandardDeviation = 0;
				double Gconstancy = 0;
				tab[0][col] = Translator.swap("DEDynamicsAnalysis.G") + (isSet("perHectare") ? "/ha)" : ")");
				try {
					Gmean = Stat.mean(Gs);
					tab[1][col] = f.format(Gmean);
				} catch (Exception e) {
					tab[1][col] = "-";
				}
				try {
					GstandardDeviation = Stat.standardDeviation(Gmean, Gs);
					tab[2][col] = f.format(GstandardDeviation);
				} catch (Exception e) {
					tab[2][col] = "-";
				}
				try {
					Gconstancy = Stat.constancy(Gmean, GstandardDeviation);
					tab[3][col] = f.format(Gconstancy);
				} catch (Exception e) {
					tab[3][col] = "-";
				}
			}

			tables.clear();
			if (tab != null) {
				tables.add(tab);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEDynamicsAnalysis.doExtraction ()", "Exception caught : ", exc);
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
