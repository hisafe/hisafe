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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.CurrentYieldIncrementProvider;

/**
 * Current and mean yield increment over time.
 * 
 * @author F. de Coligny & B. Courbaud - January 2003, modified October 2010,
 *         reviewed February 2011
 */
public class DETimeYield extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeYield");
	}

	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeYield() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeYield(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeYield.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;

			if (!(((Step) m.getProject().getRoot()).getScene() instanceof TreeList))
				return false;

			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof CurrentYieldIncrementProvider))
				return false;

			// Not used
			// if (!(mp instanceof MeanAgeProvider)) {return false;}
			// if (!(mp instanceof VProvider)) {return false;}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeYield.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeYield.name");
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud & F. de Coligny";
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeYield.description");
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.3";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.3"; // fc-1.2.2011

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		// addConfigProperty (PaleoDataExtractor.I_TREE_GROUP); // needs a
		// change in CurrentYieldIncrementProvider, DEFERRED fc-30.11.2011
		addBooleanProperty("showCurrent", true);
		addBooleanProperty("showMean", true);

	}

	private void emptyGraph(String message) {
		Vector c1 = new Vector(); // x coordinates
		Vector c2 = new Vector(); // y coordinates
		c1.add(new Integer(0));
		c2.add(new Double(0));
		curves.clear();
		curves.add(c1);
		curves.add(c2);

		labels.clear();
		labels.add(new Vector()); // no x labels
		Vector y1Labels = new Vector();
		y1Labels.add(message);
		labels.add(y1Labels);

	}

	/**
	 * DataExtractor superclass. Computes the data series. This is the real
	 * output building. Returns false if trouble while extracting.
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
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			// Not available on first step, needs several steps
			if (steps.size() <= 1) {
				emptyGraph("Not available on first step");
				upToDate = true;
				return true;
			}

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y coordinates

			TreeList previousStand = null;
			Collection previousTrees = new ArrayList();

			boolean firstTime = true;
			double cumulIncrement = 0;
			int firstDate = 0;
			int prevDate = 0;
			double firstAge = 0;
			boolean firstStep = true;

			// Data extraction: points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				TreeList stand = (TreeList) s.getScene();
				Collection trees = stand.getTrees();

				// Jump the first step
				if (firstStep) {
					firstStep = false;
					firstDate = stand.getDate();
					previousStand = stand;
					prevDate = previousStand.getDate();
					previousTrees = previousStand.getTrees();

					continue;
				}

				int date = stand.getDate();
				double currentIncrement = 0;
				double meanIncrement = 0;

				if (!trees.isEmpty()) {

					currentIncrement = ((CurrentYieldIncrementProvider) methodProvider).getCurrentYieldIncrement(
							previousStand, stand, 0);

					currentIncrement *= coefHa;

					if ((firstTime) && (date > prevDate)) {
						meanIncrement = currentIncrement;
						cumulIncrement = meanIncrement * (date - firstDate);
						firstTime = false;
						c1.add(new Integer(date));
						c2.add(new Double(currentIncrement));
						c3.add(new Double(meanIncrement));
					} else if (date > prevDate) { // increment = 0 when thinning
													// intervention (2 steps
													// with same date)
						cumulIncrement += currentIncrement * (date - prevDate);
						meanIncrement = cumulIncrement / (date - firstDate);
						c1.add(new Integer(date));
						c2.add(new Double(currentIncrement));
						c3.add(new Double(meanIncrement));
					} // else thinning intervention -> nothing to draw
				}

				previousStand = stand;
				prevDate = previousStand.getDate();
				previousTrees = previousStand.getTrees();

			}

			curves.clear();
			curves.add(c1);
			if (isSet("showCurrent")) {
				curves.add(c2);
			}
			if (isSet("showMean")) {
				curves.add(c3);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showCurrent")) {
				Vector y1Labels = new Vector();
				y1Labels.add("Current");
				labels.add(y1Labels);
			}
			if (isSet("showMean")) {
				Vector y2Labels = new Vector();
				y2Labels.add("Mean");
				labels.add(y2Labels);
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeYield.doExtraction ()", "Exception: ", e);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();

		v.add(Translator.swap("DETimeYield.xLabel"));

		StringBuffer yLabel = new StringBuffer(Translator.swap("DETimeYield.yLabel"));
		if (isSet("perHectare"))
			yLabel.append(" (ha)");
		v.add(yLabel.toString());

		return v;
	}

	/**
	 * DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

	/**
	 * DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
