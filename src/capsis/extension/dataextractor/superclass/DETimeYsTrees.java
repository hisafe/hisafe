/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2013 Francois de Coligny
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

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Check;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;

/**
 * Base class for generic data extractors with several curves over time, for one
 * or several trees. E.g. leafBiomass and stemBiomass for 2 different trees over
 * time.
 * 
 * SEE DOCUMENTATION:
 * http://www.inra.fr/capsis/documentation/howtoaddagrapheasily
 * 
 * @author F. de Coligny - November 2013
 */
abstract public class DETimeYsTrees extends AbstractDataExtractor implements DFCurves {

	protected List<List<String>> labels;
	protected List<List<? extends Number>> curves;
	protected GModel model;

	/**
	 * Init method, passes the project connected to the Project and the Step to
	 * be synchronized on.
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
	 * This method is called by superclass DataExtractor. May be overriden to
	 * add config properties.
	 */
	@Override
	public void setConfigProperties() {

		// To choose the tree ids
		addConfigProperty(PaleoDataExtractor.TREE_IDS);

	}

	private void findAtLeastOneTreeId() {
		if (getTreeIds() != null && !getTreeIds().isEmpty())
			return;

		try {
			// Try to get the first treeId to avoid opening an empty graph
			TreeCollection tc = (TreeCollection) step.getScene();
			if (getTreeIds() == null) {
				setTreeIds(new Vector());
			}
			int minId = Integer.MAX_VALUE;
			for (Iterator i = tc.getTrees().iterator(); i.hasNext();) {
				Tree t = (Tree) i.next();
				minId = Math.min(minId, t.getId());
			}
			getTreeIds().add("" + minId);

		} catch (Exception e) {
			// do nothing more
		}

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

			findAtLeastOneTreeId();

			// Retrieve Steps from root to this step
			List<Step> steps = step.getProject().getStepsFromRoot(step);

			int n = getYAxisVariableNames().length; // number of y variables
			int nTrees = getTreeIds().size();
			int nTotal = n * nTrees;

			// Init arrays
			List<Number> c1 = new ArrayList<Number>(); // x coordinates
			List<Number>[] c2s = new List[nTotal]; // y coordinates
			for (int i = 0; i < nTotal; i++) {
				c2s[i] = new ArrayList<Number>();
			}

			// Data extraction
			for (Iterator k = steps.iterator(); k.hasNext();) {
				Step s = (Step) k.next();

				// Restriction to a particular group could be added here
				GScene stand = s.getScene();

				int date = stand.getDate();

				int j = 0;

				for (String treeId : getTreeIds()) { // for each tree
					int id = Check.intValue(treeId);
					for (int i = 0; i < n; i++) { // get each value

						c2s[j].add(getValue(model, stand, id, date, i));

						j++;

					}
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
			for (int i = 0; i < nTotal; i++) {
				curves.add(c2s[i]);
			}

			labels.clear();
			// No specific labels on the X axis
			labels.add(new ArrayList<String>());
			// Add the labels of each curve (to be displayed on the right)
			String[] names = getYAxisVariableNames();
			for (String treeId : getTreeIds()) {
				for (int i = 0; i < n; i++) {
					Vector labs = new Vector();
					labs.add(names[i] + " " + treeId);
					labels.add(labs);
				}
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeYsTrees.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * Returns the variable on the X axis. Default returned value is date. Note:
	 * some modules have a specific double date and may want to return it
	 * instead of date. May be overriden, if so see xAxisVariableIsAnInteger ().
	 */
	public double getXAxisVariable(GScene stand) {
		return stand.getDate();
	}

	/**
	 * Returns true if getXAxisVariable(GScene) always return an integer value,
	 * else returns false. May be overriden if needed to return false if
	 * getXAxisVariable (GScene) is overriden and returns double values.
	 */
	public boolean xAxisVariableIsAnInteger() {
		return true;
	}

	/**
	 * If getXAxisVariable(GScene) is overriden, getXAxisVariableName () should
	 * be also overriden to give the correct name of this variable to be printed
	 * on the X axis.
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
		int nTrees = getTreeIds() == null ? 0 : getTreeIds().size();
		return getYAxisVariableNames().length * nTrees;
	}

	/**
	 * Must be overriden to tell the name of the given y variable.
	 */
	abstract public String[] getYAxisVariableNames();

	/**
	 * Override this function to fill the data extractor for the ith y variable
	 * of the tree with the given treeId.
	 */
	abstract protected Number getValue(GModel m, GScene stand, int treeId, int date, int i);

}
