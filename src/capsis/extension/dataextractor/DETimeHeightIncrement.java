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
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.HeightIncrementDomProvider;
import capsis.util.methodprovider.HeightIncrementGProvider;
import capsis.util.methodprovider.HeightIncrementProvider;
import capsis.util.methodprovider.HeightIncrementStandardDeviationProvider;
import capsis.util.methodprovider.MaxHeightIncrementProvider;
import capsis.util.methodprovider.MeanHeightIncrementProvider;
import capsis.util.methodprovider.MinHeightIncrementProvider;

/**
 * Height increment of trees versus Date.
 * 
 * @author S. Turbis - July 2005, enhanced September 2005, review January 2006
 *         to add HIg and HIdom
 */
public class DETimeHeightIncrement extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeHeightIncrement");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	// maybe module does not calculate this property
	private boolean availableHIm;
	private boolean availableHIStdDev;
	private boolean availableHImin;
	private boolean availableHImax;
	private boolean availableHIdom;
	private boolean availableHIg;

	/**
	 * Constructor.
	 */
	public DETimeHeightIncrement() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeHeightIncrement(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider();

			// According to module capabiblities, disable some configProperties
			setPropertyEnabled("showHIm", availableHIm);
			setPropertyEnabled("showHIStdDev", availableHIStdDev);
			setPropertyEnabled("showHImin", availableHImin);
			setPropertyEnabled("showHImax", availableHImax);
			setPropertyEnabled("showHIdom", availableHIdom);
			setPropertyEnabled("showHIg", availableHIg);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeHeightIncrement.c ()", "Exception during construction", e);
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

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty()) {
				return true;
			} // bare soil problem
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) {
				return false;
			}
			if (!(t instanceof HeightIncrementProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeHeightIncrement.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeHeightIncrement.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "S. Turbis";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeHeightIncrement.description");
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

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		addConfigProperty(PaleoDataExtractor.TREE_GROUP); // group
															// multiconfiguration
		// addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
		addBooleanProperty("showHIm");
		addBooleanProperty("showHImin");
		addBooleanProperty("showHImax");
		addBooleanProperty("showHIStdDev");
		addBooleanProperty("showHIdom");
		addBooleanProperty("showHIg");
	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof MeanHeightIncrementProvider) {
			availableHIm = true;
		}
		if (methodProvider instanceof HeightIncrementStandardDeviationProvider) {
			availableHIStdDev = true;
		}
		if (methodProvider instanceof MinHeightIncrementProvider) {
			availableHImin = true;
		}
		if (methodProvider instanceof MaxHeightIncrementProvider) {
			availableHImax = true;
		}
		if (methodProvider instanceof HeightIncrementDomProvider) {
			availableHIdom = true;
		}
		if (methodProvider instanceof HeightIncrementGProvider) {
			availableHIg = true;
		}

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
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {

			// If no curve at all, choose one tree
			// fc - 6.2.2004
			if (!isSet("showHIm") && !isSet("showHImin") && !isSet("showHImax") && !isSet("showHIStdDev")
					&& !isSet("showHIdom") && !isSet("showHIg") && (getTreeIds() == null || getTreeIds().isEmpty())) {
				TreeCollection tc = (TreeCollection) step.getScene();
				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}
				getTreeIds().add("" + tc.getTrees().iterator().next().getId());
			}

			int treeNumber = getTreeIds().size();
			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates (years)
			Vector c2 = new Vector(); // optional: y coordinates (HIm)
			Vector c3 = new Vector(); // optional: y coordinates (HImin)
			Vector c4 = new Vector(); // optional: y coordinates (HImax)
			Vector c5 = new Vector(); // optional: y coordinates (HIStdDev)
			Vector c6 = new Vector(); // optional: y coordinates (HIdom)
			Vector c7 = new Vector(); // optional: y coordinates (HIg)
			Vector cy[] = new Vector[treeNumber]; // y coordinates
													// (treeHeightIncrement)

			for (int i = 0; i < treeNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand); // fc - 5.4.2004

				int year = stand.getDate();

				c1.add(new Integer(year));
				if (isSet("showHIm")) {
					c2.add(new Double(((MeanHeightIncrementProvider) methodProvider).getMeanHeightIncrement(stand,
							trees)));
				}
				if (isSet("showHImin")) {
					c3.add(new Double(((MinHeightIncrementProvider) methodProvider).getMinHeightIncrement(stand, trees)));
				}
				if (isSet("showHImax")) {
					c4.add(new Double(((MaxHeightIncrementProvider) methodProvider).getMaxHeightIncrement(stand, trees)));
				}
				if (isSet("showHIStdDev")) {
					c5.add(new Double(((HeightIncrementStandardDeviationProvider) methodProvider)
							.getHeightIncrementStandardDeviation(stand, trees)));
				}

				if (isSet("showHIdom")) {
					c6.add(new Double(((HeightIncrementDomProvider) methodProvider).getHeightIncrementDom(stand, trees)));
				}
				if (isSet("showHIg")) {
					c7.add(new Double(((HeightIncrementGProvider) methodProvider).getHeightIncrementG(stand, trees)));
				}

				// Get height increment for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						HeightIncrementProvider t2 = (HeightIncrementProvider) t;
						double heightInc = t2.getHeightIncrement(); // Height
																	// increment
																	// in m
						cy[n].add(new Double(heightInc));
					}
					n++;
				}

			}

			curves.clear();
			curves.add(c1);

			if (isSet("showHIm")) {
				curves.add(c2);
			}
			if (isSet("showHImin")) {
				curves.add(c3);
			}
			if (isSet("showHImax")) {
				curves.add(c4);
			}
			if (isSet("showHIStdDev")) {
				curves.add(c5);
			}
			if (isSet("showHIdom")) {
				curves.add(c6);
			}
			if (isSet("showHIg")) {
				curves.add(c7);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showHIm")) {
				Vector y1Labels = new Vector();
				y1Labels.add("HIm");
				labels.add(y1Labels);
			}

			if (isSet("showHImin")) {
				Vector y2Labels = new Vector();
				y2Labels.add("HImin");
				labels.add(y2Labels);
			}

			if (isSet("showHImax")) {
				Vector y3Labels = new Vector();
				y3Labels.add("HImax");
				labels.add(y3Labels);
			}

			if (isSet("showHIStdDev")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Sigma");
				labels.add(y4Labels);
			}

			if (isSet("showHIdom")) {
				Vector y5Labels = new Vector();
				y5Labels.add("HIdom");
				labels.add(y5Labels);
			}

			if (isSet("showHIg")) {
				Vector y6Labels = new Vector();
				y6Labels.add("HIg");
				labels.add(y6Labels);
			}

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeHeightIncrement.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
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

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeHeightIncrement.xLabel"));
		v.add(Translator.swap("DETimeHeightIncrement.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
