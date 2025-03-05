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
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.AgeDomProvider;
import capsis.util.methodprovider.AgeGProvider;
import capsis.util.methodprovider.AgeStandardDeviationProvider;
import capsis.util.methodprovider.MaxAgeProvider;
import capsis.util.methodprovider.MeanAgeProvider;
import capsis.util.methodprovider.MinAgeProvider;

/**
 * Age of trees versus Date.
 * 
 * @author S. Turbis - June 2005, September 2005
 */
public class DETimeAge extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeAge");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	// maybe module does not calculate this property
	private boolean availableAg;
	private boolean availableAdom;
	private boolean availableAm;
	private boolean availableAgeStandardDeviation;
	private boolean availableAmin;
	private boolean availableAmax;

	/**
	 * Constructor.
	 */
	public DETimeAge() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeAge(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider();

			// According to module capabiblities, disable some configProperties
			setPropertyEnabled("showAg", availableAg);
			setPropertyEnabled("showAdom", availableAdom);
			setPropertyEnabled("showMeanAge", availableAm);
			setPropertyEnabled("showAgeStandardDeviation", availableAgeStandardDeviation);
			setPropertyEnabled("showMinAge", availableAmin);
			setPropertyEnabled("showMaxAge", availableAmax);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAge.c ()", "Exception during construction : ", e);
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
			// all trees have getAge
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAge.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeAge.name");
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
		return Translator.swap("DETimeAge.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.0";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {

		checkMethodProvider();

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		
		addConfigProperty(PaleoDataExtractor.TREE_GROUP); // group
															// multiconfiguration
		
		// addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
		addBooleanProperty("showMeanAge");
		addBooleanProperty("showMinAge");
		addBooleanProperty("showMaxAge");
		addBooleanProperty("showAdom");
		addBooleanProperty("showAgeStandardDeviation");
		addBooleanProperty("showAg");

	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof AgeGProvider) {
			availableAg = true;
		}
		if (methodProvider instanceof AgeDomProvider) {
			availableAdom = true;
		}
		if (methodProvider instanceof MeanAgeProvider) {
			availableAm = true;
		}
		if (methodProvider instanceof AgeStandardDeviationProvider) {
			availableAgeStandardDeviation = true;
		}
		if (methodProvider instanceof MinAgeProvider) {
			availableAmin = true;
		}
		if (methodProvider instanceof MaxAgeProvider) {
			availableAmax = true;
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
			if (!isSet("showMeanAge") && !isSet("showMinAge") && !isSet("showMaxAge")
					&& !isSet("showAgeStandardDeviation") && !isSet("showAdom") && !isSet("showAg")
					&& (getTreeIds() == null || getTreeIds().isEmpty())) {

				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}

				// Consider restriction to one particular group if needed
				GScene stand = step.getScene();

				// fc-3.9.2019 no groups in this extractor (see
				// setConfigProperties ())
				TreeCollection tc = (TreeCollection) stand; // see matchWith ()
				Collection trees = tc.getTrees();
				
				// fc-3.9.2019 no groups in this extractor
				// Collection trees = doFilter(stand);

				if (!trees.isEmpty()) {
					getTreeIds().add("" + ((Tree) trees.iterator().next()).getId());
				}

			}

			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // optional: y coordinates (meanAge)
			Vector c3 = new Vector(); // optional: y coordinates (minAge)
			Vector c4 = new Vector(); // optional: y coordinates (maxAge)
			Vector c5 = new Vector(); // optional: y coordinates (AgeStdDev)
			Vector c6 = new Vector(); // optional: y coordinates (Adom)
			Vector c7 = new Vector(); // optional: y coordinates (Ag)
			Vector cy[] = new Vector[treeNumber]; // y coordinates (treeAge)

			for (int i = 0; i < treeNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();

				// fc-3.9.2019 no groups in this extractor (see
				// setConfigProperties ())
				TreeCollection tc = (TreeCollection) stand; // see matchWith ()
				Collection trees = tc.getTrees();

				// fc-3.9.2019 no groups in this extractor
//				Collection trees = doFilter(stand); // fc-5.4.2004

				int year = stand.getDate();

				c1.add(new Integer(year));
				if (isSet("showMeanAge")) {
					c2.add(new Double(((MeanAgeProvider) methodProvider).getMeanAge(stand, trees)));
				}
				if (isSet("showMinAge")) {
					c3.add(new Double(((MinAgeProvider) methodProvider).getMinAge(stand, trees)));
				}
				if (isSet("showMaxAge")) {
					c4.add(new Double(((MaxAgeProvider) methodProvider).getMaxAge(stand, trees)));
				}
				if (isSet("showAgeStandardDeviation")) {
					c5.add(new Double(
							((AgeStandardDeviationProvider) methodProvider).getAgeStandardDeviation(stand, trees)));
				}
				if (isSet("showAdom")) {
					c6.add(new Double(((AgeDomProvider) methodProvider).getAgeDom(stand, trees)));
				}
				if (isSet("showAg")) {
					c7.add(new Double(((AgeGProvider) methodProvider).getAgeG(stand, trees)));
				}

				// Get Age for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						double age = t.getAge(); // age of the tree
						cy[n].add(new Double(age));
					}
					n++;
				}

			}

			curves.clear();
			curves.add(c1);

			if (isSet("showMeanAge")) {
				curves.add(c2);
			}
			if (isSet("showMinAge")) {
				curves.add(c3);
			}
			if (isSet("showMaxAge")) {
				curves.add(c4);
			}
			if (isSet("showAgeStandardDeviation")) {
				curves.add(c5);
			}
			if (isSet("showAdom")) {
				curves.add(c6);
			}
			if (isSet("showAg")) {
				curves.add(c7);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showMeanAge")) {
				Vector y1Labels = new Vector();
				y1Labels.add("Am");
				labels.add(y1Labels);
			}

			if (isSet("showMinAge")) {
				Vector y2Labels = new Vector();
				y2Labels.add("Amin");
				labels.add(y2Labels);
			}

			if (isSet("showMaxAge")) {
				Vector y3Labels = new Vector();
				y3Labels.add("Amax");
				labels.add(y3Labels);
			}

			if (isSet("showAgeStandardDeviation")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Sigma");
				labels.add(y4Labels);
			}

			if (isSet("showAdom")) {
				Vector y5Labels = new Vector();
				y5Labels.add("Adom");
				labels.add(y5Labels);
			}

			if (isSet("showAg")) {
				Vector y6Labels = new Vector();
				y6Labels.add("Ag");
				labels.add(y6Labels);
			}

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeAge.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeAge.xLabel"));
		v.add(Translator.swap("DETimeAge.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
