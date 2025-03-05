/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  INRA
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
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;

/**
 * Numbers of trees per species classes.
 * 
 * @author S. Dufour Kowalski
 */
public abstract class DEMultiTimeX extends AbstractDataExtractor implements DFCurves {

	private List<List<String>> labels;
	private List<List<? extends Number>> curves;
	private GModel model;

	@Override
	public void init(GModel m, Step s) throws Exception {

		super.init(m, s);
		labels = new ArrayList<List<String>>();
		curves = new ArrayList<List<? extends Number>>();
		model = m;
		setPropertyEnabled("showIncrement", true);

	}

	/**
	 * This method is called by superclass DataExtractor.
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
	@Override
	public boolean doExtraction() {
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		try {

			// Retrieve Steps from root to this step

			List<Step> steps = step.getProject().getStepsFromRoot(step);
			GScene stand;

			// x coordinates
			List<Number> c1 = new ArrayList<Number>(); // x coordinates

			for (Step s : steps) {

				stand = s.getScene();
				int date = stand.getDate();
				c1.add(new Integer(date));
			}

			curves.clear();
			curves.add(c1);

			labels.clear();
			labels.add(new ArrayList<String>()); // no x labels

			// for each class a curve
			stand = step.getScene();

			List<Object> classes = getTypes(stand);

			int i = 0;
			for (Object cl : classes) {

				List<Number> c2 = new ArrayList<Number>(); // y coordinates

				// for each step
				for (Step s : steps) {
					stand = s.getScene();
					c2.add(getValue(model, stand, i, cl));
				}

				curves.add(c2);

				List<String> y1Labels = new ArrayList<String>();
				y1Labels.add(cl.toString());
				labels.add(y1Labels);

				i++;
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeY.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		List<String> v = new ArrayList<String>();
		v.add(Translator.swap("Time"));

		if (isSet("perHectare")) {
			v.add(getYLabel() + " (ha)");
		} else {
			v.add(getYLabel());
		}

		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public int getNY() {
		return curves.size() - 1;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<List<String>> getLabels() {
		return labels;
	}

	@Override
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap(getClass().getSimpleName());
	}

	protected String getYLabel() {

		return Translator.swap(getClass().getSimpleName() + ".yLabel");
	}

	/**
	 * Override this function to fill extractor typeId : Type id type : type
	 * object
	 * */
	abstract protected Number getValue(GModel m, GScene stand, int typeId, Object type);

	/**
	 * Return a list of types
	 * 
	 * @param stand
	 */
	abstract protected List<Object> getTypes(GScene stand);

}
