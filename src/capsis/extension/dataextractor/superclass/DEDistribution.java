/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2012  Francois de Coligny
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
import java.util.Collection;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Classifier;
import jeeb.lib.util.ClassifierCriterion;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * Base class for distribution type data extractors.
 * 
 * SEE DOCUMENTATION:
 * http://www.inra.fr/capsis/documentation/howtoaddagrapheasily
 * 
 * @author F. de Coligny - April 2012
 */
abstract public class DEDistribution extends AbstractDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEDistribution");
	}

	protected GModel model;
	protected List<List<? extends Number>> curves;
	protected List<List<String>> labels;
	protected boolean availableForThisStep = true;

	/**
	 * Init method, passes the project connected to the Project and the Step to
	 * be synchronized on.
	 */
	@Override
	public void init(GModel model, Step step) throws Exception {

		super.init(model, step);
		this.model = model;
		curves = new ArrayList<List<? extends Number>>();
		labels = new ArrayList<List<String>>();

		// setPropertyEnabled("showIncrement", true);

	}

	/**
	 * This method is called by superclass DataExtractor. May be overriden to
	 * add config properties.
	 */
	@Override
	public void setConfigProperties() {

		addBooleanProperty("perHectare", false);

		addDoubleProperty("minThreshold", 0d); // Unit not specified, depends on
												// the value
		addDoubleProperty("classWidth", 5d); // Unit not specified, depends on
												// the value
	}

	/**
	 * Candidate objects may not be instances of Tree, Removed ? extends Tree.
	 */
	protected Collection getCandidateObjects(Step step) {
		
		TreeList treeList = (TreeList) step.getScene();
		Collection<? extends Tree> list = treeList.getTrees();
		return list;
	}
//
//	// getCandidateTrees may return getTrees () or getTrees("cut")...
//	protected Collection<? extends Tree> getCandidateTrees(Step step) {
//		TreeList treeList = (TreeList) step.getScene();
//		Collection<? extends Tree> list = treeList.getTrees();
//		return list;
//	}

	/**
	 * Returns true is the extractor can work on the current Step (e.g. false if
	 * works on cut trees and no cut trees on this step).
	 */
	public boolean isAvailable() {
		return availableForThisStep;
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

			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			double minThreshold = getDoubleProperty("minThreshold");
			double classWidth = getDoubleProperty("classWidth");

			// Security
			if (classWidth <= 0d) {
				classWidth = 1d;
			}
			
			// fc+bc-25.8.2021 Move here to clear if getCandidateObjects () below returns nothing
			curves.clear();
			labels.clear();

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			// Distribution

			// fc-30.10.2014
			// TreeList treeList = (TreeList) step.getScene();
			// Collection<? extends Tree> list = treeList.getTrees();

			// getCandidateTrees may return getTrees () or getTrees("cut")...
			availableForThisStep = true;
			Collection<? extends Tree> list = getCandidateObjects(step);
			if (list == null || list.isEmpty()) {
				availableForThisStep = false;
				upToDate = true;
				return true;
			}

			// fc-30.10.2014

			// Which value must be used to classify the objects in the list:
			// e.g. tree dbh
			ClassifierCriterion cc = getClassifierCriterion(coefHa); // (see
																		// below)

			// min bound, classWidth
			Classifier c = new Classifier(list, cc, minThreshold, classWidth);
			c.execute();

			// fc-1.10.2020 Check the classifier
//			System.out.println("DEDistribution trace, classifier: \n"+c);

			// Turn the distribution into a String
			int[] d = c.getDistribution();
			String[] minLabels = c.getMinLabels(2); // decimalDigits = 2
			String[] maxLabels = c.getMaxLabels(2); // decimalDigits = 2

			for (int i = 0; i < d.length; i++) {
				c1.add(i);
				c2.add(d[i]);
				// fc-1.10.2020 Replaced , by -, else error later in CategoryConverterHelper
				l1.add("[" + minLabels[i] + "-" + maxLabels[i] + "[");
//				l1.add("[" + minLabels[i] + "," + maxLabels[i] + "[");
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

			// fc-1.10.2020 Checking data order (Espace histogram)
			// The bug was in CategoryConverterHelper.RangeComparator, fixed
//			System.out.println("DEDistribution trace, c1: \n"+AmapTools.toString(c1));
//			System.out.println("DEDistribution trace, c2: \n"+AmapTools.toString(c2));
//			System.out.println("DEDistribution trace, l1: \n"+AmapTools.toString(l1));
			
			
		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEDistribution.doExtraction ()", "Exception: ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * Make rely the classification criterion on the getValue () method below
	 * (delegation).
	 */
	private ClassifierCriterion getClassifierCriterion(final double coefHa) {
		return new ClassifierCriterion() {
			@Override
			public double getValue(Object o) {
				return DEDistribution.this.getValue(o).doubleValue() * coefHa;

			}
		};
	}

	/**
	 * DFCurves interface.
	 */
	public List<String> getAxesNames() {
		List<String> v = new ArrayList<String>();
		v.add(getXLabel());

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
		return ("N");
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	/**
	 * Returns the name of the X axis. A translation should be provided, see
	 * Translator.
	 */
	abstract protected String getXLabel();

	/**
	 * Override this function to fill the data extractor.
	 */
	abstract protected Number getValue(Object o); // e.g. return ((Tree)
													// o).getDbh ();

}
