/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2011  Francois de Coligny & Samuel Dufour-Kowalski
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
package capsis.extension.dataextractor.superclass;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;

/**
 * Base class for y / time generic data extractors.
 * 
 * SEE DOCUMENTATION:
 * http://www.inra.fr/capsis/documentation/howtoaddagrapheasily
 * 
 * @author S. Dufour-Kowalski - November 2009
 */
abstract public class DETimeY extends AbstractDataExtractor implements DFCurves {
	// fc-feb2012 refactored, date on the x axis may be overriden (e.g. double
	// simulation date in Optimist), changed the name from DETimeX to DETimeY.

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
//		setPropertyEnabled("showIncrement", true);

	}

	/**
	 * This method is called by superclass DataExtractor. May be overriden to add
	 * config properties.
	 */
	@Override
	public void setConfigProperties() {

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

			List<Number> c1 = new ArrayList<Number>(); // x coordinates
			List<Number> c2 = new ArrayList<Number>(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			// Note: may be (Double, Double) coordinates if !xAxisVariableIsAnInteger()
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Restriction to a particular group could be added here
				GScene stand = s.getScene();

				int date = stand.getDate();

//				c2.add(getValue(model, stand, date));

				// fc-6.7.2021 manage null value in case there is no value for this step
				Number value = getValue(model, stand, date);
				if (value == null)
					continue;

				c2.add(value);

				// fc-17.2.2012
				if (xAxisVariableIsAnInteger()) {
					int v = (int) getXAxisVariable(stand);
					c1.add(v);
				} else {
					c1.add(getXAxisVariable(stand));
				}

			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(new ArrayList<String>()); // no specific labels on the X axis

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeY.doExtraction ()", "Exception caught : ", exc);
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
	public double getXAxisVariable(GScene stand) { // fc-17.2.2012
		return stand.getDate();
	}

	/**
	 * Returns true if getXAxisVariable(GScene) always return an integer value, else
	 * returns false. May be overriden if needed to return false if getXAxisVariable
	 * (GScene) is overriden and returns double values.
	 */
	public boolean xAxisVariableIsAnInteger() { // fc-17.2.2012
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
	public int getNY() {
		return curves.size() - 1;
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
	 * Override this function to fill the data extractor.
	 */
	abstract protected Number getValue(GModel m, GScene stand, int date);

}
