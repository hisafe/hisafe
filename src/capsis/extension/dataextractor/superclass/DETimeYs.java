/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2012 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.extension.dataextractor.superclass;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.format.DFColoredCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;

/**
 * Base class for generic data extractors with several curves over time. E.g.
 * Hdom and Hg over time.
 * 
 * SEE DOCUMENTATION:
 * http://www.inra.fr/capsis/documentation/howtoaddagrapheasily
 * 
 * @author F. de Coligny - February 2012
 */
abstract public class DETimeYs extends AbstractDataExtractor implements DFColoredCurves {

	protected List<List<String>> labels;
	protected List<List<? extends Number>> curves;
	protected GModel model;

	/**
	 * Init method, passes the project connected to the Project and the Step to be
	 * synchronized on.
	 */
	@Override
	public void init(GModel m, Step s) throws Exception {

		super.init(m, s);
		labels = new ArrayList<List<String>>();
		curves = new ArrayList<List<? extends Number>>();
		model = m;
		// setPropertyEnabled("showIncrement", true);

	}

	/**
	 * This method is called by superclass DataExtractor. May be overriden to add
	 * config properties.
	 */
	@Override
	public void setConfigProperties() {
	}

	/**
	 * Called at the beginning of doExtraction, in case a preparation is needed,
	 * does nothing by default.
	 */
	protected void init(List<Step> steps) {
		// May be overriden if needed
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
		if (upToDate)
			return true;
		if (step == null)
			return false;

		try {

			// Retrieve Steps from root to this step
			List<Step> steps = step.getProject().getStepsFromRoot(step);

			// fc-12.7.2021 In case a preparation is needed
			init(steps);

			int n = getNY(); // number of y variables

			// Init arrays
			List<Number> c1 = new ArrayList<Number>(); // x coordinates
			List<Number>[] c2s = new List[n]; // y coordinates
			for (int i = 0; i < n; i++) {
				c2s[i] = new ArrayList<Number>();
			}

			// Data extraction
			for (Iterator k = steps.iterator(); k.hasNext();) {
				Step s = (Step) k.next();

				// Restriction to a particular group could be added here
				GScene stand = s.getScene();

				int date = stand.getDate();

				for (int i = 0; i < n; i++) {
					c2s[i].add(getValue(model, stand, date, i));
				}

				if (xAxisVariableIsAnInteger()) {
					int v = (int) getXAxisVariable(stand);
					c1.add(v);
				} else {
					c1.add(getXAxisVariable(stand));
				}

			}

			curves.clear();
			curves.add(c1);
			for (int i = 0; i < n; i++) {
				curves.add(c2s[i]);
			}

			labels.clear();
			// No specific labels on the X axis
			labels.add(new ArrayList<String>());
			// Add the labels of each curve (to be displayed on the right)
			String[] names = getYAxisVariableNames();
			for (int i = 0; i < n; i++) {
				Vector labs = new Vector();
				labs.add(names[i]);
				labels.add(labs);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeYs.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * Returns the variable on the X axis. Default returned value is date. Note:
	 * some modules have a specific double date and may want to return it instead of
	 * date. May be overriden, if so see xAxisVariableIsAnInteger ().
	 */
	public double getXAxisVariable(GScene stand) {
		return stand.getDate();
	}

	/**
	 * Returns true if getXAxisVariable(GScene) always return an integer value, else
	 * returns false. May be overriden if needed to return false if getXAxisVariable
	 * (GScene) is overriden and returns double values.
	 */
	public boolean xAxisVariableIsAnInteger() {
		return true;
	}

	/**
	 * If getXAxisVariable(GScene) is overriden, getXAxisVariableName () should be
	 * also overriden to give the correct name of this variable to be printed on the
	 * X axis.
	 */
	public String getXAxisVariableName() {
		return Translator.swap("Time"); // default value
	}

	/**
	 * DFCurves interface.
	 */
	public List<String> getAxesNames() {
		List<String> v = new ArrayList<String>();
		v.add(getXAxisVariableName());

		if (isSet("perHectare")) {
			v.add(getYLabel() + " (ha)");
		} else {
			v.add(getYLabel());
		}

		return v;
	}

	/**
	 * DFCurves interface.
	 */
	@Override
	public List<List<String>> getLabels() {
		return labels;
	}

	/**
	 * DFCurves interface.
	 */
	@Override
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * The name of this data extractor. A translation should be provided, see
	 * Translator.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap(getClass().getSimpleName());
	}

	/**
	 * Returns the name of the Y axis. A translation should be provided, see
	 * Translator.
	 */
	protected String getYLabel() {
		return Translator.swap(getClass().getSimpleName() + ".yLabel");
	}

	/**
	 * DFCurves interface.
	 */
	@Override
	public int getNY() {
		return getYAxisVariableNames().length;
	}

	/**
	 * Must be overriden to tell the name of the given y variable.
	 */
	abstract public String[] getYAxisVariableNames();

	/**
	 * Override this function to fill the data extractor for the ith y variable.
	 */
	abstract protected Number getValue(GModel m, GScene stand, int date, int i);

	/**
	 * Returns a color per curve: getCurves ().size () - 1.
	 */
	public Vector getColors() {
		// fc-28.11.2013 changed DFCurves into DFColoredCurves, provided this
		// default implementation
		// for getcolors () (no change). Subclasses can redefine this method to
		// return a different
		// color for each curve
		Vector v = new Vector();
		Color singleColor = getColor(); // see AbstractDataExtractor
		for (int i = 0; i < getCurves().size() - 1; i++) {
			v.add(singleColor);
		}
		return v;
	}

}
