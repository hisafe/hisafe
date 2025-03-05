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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.AgeStandardDeviationProvider;
import capsis.util.methodprovider.CrownBaseHeightStandardDeviationProvider;
import capsis.util.methodprovider.DbhStandardDeviationProvider;
import capsis.util.methodprovider.HeightStandardDeviationProvider;
import capsis.util.methodprovider.InterceptionIndexStandardDeviationProvider;
import capsis.util.methodprovider.MeanAgeProvider;
import capsis.util.methodprovider.MeanCrownBaseHProvider;
import capsis.util.methodprovider.MeanDbhProvider;
import capsis.util.methodprovider.MeanHProvider;
import capsis.util.methodprovider.MeanInterceptionIndexProvider;
import capsis.util.methodprovider.MeanVProvider;
import capsis.util.methodprovider.VStandardDeviationProvider;

/**
 * Size dispersion versus Date.
 * 
 * @author B. Courbaud - February 2003
 */
public class DETimeSizeDispersion extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeSizeDispersion");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeSizeDispersion() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeSizeDispersion(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeSizeDispersion.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;

			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof MeanVProvider))
				return false;

			if (!(mp instanceof VStandardDeviationProvider))
				return false;

			if (!(mp instanceof MeanHProvider))
				return false;

			if (!(mp instanceof HeightStandardDeviationProvider))
				return false;

			if (!(mp instanceof MeanDbhProvider))
				return false;

			if (!(mp instanceof DbhStandardDeviationProvider))
				return false;

			if (!(mp instanceof MeanAgeProvider))
				return false;

			if (!(mp instanceof AgeStandardDeviationProvider))
				return false;

			if (!(mp instanceof MeanInterceptionIndexProvider))
				return false;

			if (!(mp instanceof InterceptionIndexStandardDeviationProvider))
				return false;

			if (!(mp instanceof MeanCrownBaseHProvider))
				return false;

			if (!(mp instanceof CrownBaseHeightStandardDeviationProvider))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeSizeDispersion.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeSizeDispersion.name");
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
		return Translator.swap("DETimeSizeDispersion.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // fc - 9.4.2004
		addBooleanProperty("showVCV");
		addBooleanProperty("showHCV", true); // added true for default opening
												// with one curve at least
		addBooleanProperty("showDbhCV");
		addBooleanProperty("showAgeCV");
		addBooleanProperty("showInterceptIndexCV");
		addBooleanProperty("showCrownBaseHCV");
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step.
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
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y coordinates
			Vector c4 = new Vector(); // y coordinates
			Vector c5 = new Vector(); // y coordinates
			Vector c6 = new Vector(); // y coordinates
			Vector c7 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();
				double meanV = ((MeanVProvider) methodProvider).getMeanV(stand, trees);
				double VSE = ((VStandardDeviationProvider) methodProvider).getVStandardDeviation(stand, trees);
				double meanH = ((MeanHProvider) methodProvider).getMeanH(stand, trees);
				double HSE = ((HeightStandardDeviationProvider) methodProvider)
						.getHeightStandardDeviation(stand, trees);
				double meanDbh = ((MeanDbhProvider) methodProvider).getMeanDbh(stand, trees);
				double DbhSE = ((DbhStandardDeviationProvider) methodProvider).getDbhStandardDeviation(stand, trees);
				double meanAge = ((MeanAgeProvider) methodProvider).getMeanAge(stand, trees);
				double AgeSE = ((AgeStandardDeviationProvider) methodProvider).getAgeStandardDeviation(stand, trees);
				double meanInterceptI = ((MeanInterceptionIndexProvider) methodProvider).getMeanInterceptionIndex(
						stand, trees);
				double InterceptISE = ((InterceptionIndexStandardDeviationProvider) methodProvider)
						.getInterceptionIndexStandardDeviation(stand, trees);
				double meanCrownBaseH = ((MeanCrownBaseHProvider) methodProvider).getMeanCrownBaseH(stand, trees);
				double crownBaseHSE = ((CrownBaseHeightStandardDeviationProvider) methodProvider)
						.getCrownBaseHeightStandardDeviation(stand, trees);

				c1.add(new Integer(date));
				c2.add(new Double(VSE / meanV)); // coef of variation
				c3.add(new Double(HSE / meanH)); // coef of variation
				c4.add(new Double(DbhSE / meanDbh)); // coef of variation
				c5.add(new Double(AgeSE / meanAge)); // coef of variation
				c6.add(new Double(InterceptISE / meanInterceptI)); // coef of
																	// variation
				c7.add(new Double(crownBaseHSE / meanCrownBaseH)); // coef of
																	// variation
			}

			curves.clear();
			curves.add(c1);
			if (isSet("showVCV")) {
				curves.add(c2);
			}
			if (isSet("showHCV")) {
				curves.add(c3);
			}
			if (isSet("showDbhCV")) {
				curves.add(c4);
			}
			if (isSet("showAgeCV")) {
				curves.add(c5);
			}
			if (isSet("showInterceptIndexCV")) {
				curves.add(c6);
			}
			if (isSet("showCrownBaseHCV")) {
				curves.add(c7);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showVCV")) {
				Vector y1Labels = new Vector();
				y1Labels.add("V");
				labels.add(y1Labels);
			}
			if (isSet("showHCV")) {
				Vector y2Labels = new Vector();
				y2Labels.add("H");
				labels.add(y2Labels);
			}
			if (isSet("showDbhCV")) {
				Vector y3Labels = new Vector();
				y3Labels.add("D");
				labels.add(y3Labels);
			}
			if (isSet("showAgeCV")) {
				Vector y4Labels = new Vector();
				y4Labels.add("A");
				labels.add(y4Labels);
			}
			if (isSet("showInterceptIndexCV")) {
				Vector y5Labels = new Vector();
				y5Labels.add("Ix");
				labels.add(y5Labels);
			}
			if (isSet("showCrownBaseHCV")) {
				Vector y6Labels = new Vector();
				y6Labels.add("CBH");
				labels.add(y6Labels);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeSizeDispersion.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeSizeDispersion.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeSizeDispersion.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeSizeDispersion.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
