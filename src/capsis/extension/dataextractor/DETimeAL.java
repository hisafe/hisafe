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
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
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
import capsis.util.methodprovider.ALProvider;
import capsis.util.methodprovider.ALStandardDeviationProvider;
import capsis.util.methodprovider.ALdomProvider;
import capsis.util.methodprovider.ALgProvider;
import capsis.util.methodprovider.MaxALProvider;
import capsis.util.methodprovider.MeanALProvider;
import capsis.util.methodprovider.MinALProvider;

/**
 * Available light to the crown (AL) of trees versus Date.
 * 
 * @author S. Turbis - June 2005, September 2005, review January 2006 to add ALg
 *         and ALdom
 */
public class DETimeAL extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeAL");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	// maybe module does not calculate this property
	private boolean availableALm;
	private boolean availableALStandardDeviation;
	private boolean availableALmin;
	private boolean availableALmax;
	private boolean availableALg;
	private boolean availableALdom;

	/**
	 * Constructor.
	 */
	public DETimeAL() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeAL(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider();

			// According to module capabiblities, disable some configProperties
			setPropertyEnabled("showMeanAL", availableALm);
			setPropertyEnabled("showALStandardDeviation", availableALStandardDeviation);
			setPropertyEnabled("showMinAL", availableALmin);
			setPropertyEnabled("showMaxAge", availableALmax);
			setPropertyEnabled("showALg", availableALg);
			setPropertyEnabled("showALdom", availableALdom);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAL.c ()", "Exception during construction : ", e);
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

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}

			// fc - 31.1.2008 - test more accurately : we need spatialized trees
			// -> e.g. Oakpine2 has a TreeCollection, but trees inside are not
			// spatialized
			Collection trees = ((TreeCollection) s).getTrees();
			Collection reps = Tools.getRepresentatives(trees);
			for (Object o : reps) {
				if (!(o instanceof Spatialized)) {
					return false;
				}
			}

			// st - 01.12.2005 Should I put this code or not
			// (if yes, only those who have ALProvider will see
			// this DataExtractor in their list) -> yes (fc)
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty()) {
				return true;
			} // bare soil problem
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) {
				return false;
			}
			if (!(t instanceof ALProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAL.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeAL.name");
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
		return Translator.swap("DETimeAL.description");
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

		checkMethodProvider();

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		addConfigProperty(PaleoDataExtractor.TREE_GROUP); // group
															// multiconfiguration
		// addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
		addBooleanProperty("showMeanAL");
		addBooleanProperty("showMinAL");
		addBooleanProperty("showMaxAL");
		addBooleanProperty("showALStandardDeviation");
		addBooleanProperty("showALg");
		addBooleanProperty("showALdom");

	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof MeanALProvider) {
			availableALm = true;
		}
		if (methodProvider instanceof ALStandardDeviationProvider) {
			availableALStandardDeviation = true;
		}
		if (methodProvider instanceof MinALProvider) {
			availableALmin = true;
		}
		if (methodProvider instanceof MaxALProvider) {
			availableALmax = true;
		}
		if (methodProvider instanceof ALgProvider) {
			availableALg = true;
		}
		if (methodProvider instanceof ALdomProvider) {
			availableALdom = true;
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
			if (!isSet("showMeanAL") && !isSet("showMinAL") && !isSet("showMaxAL") && !isSet("showALStandardDeviation")
					&& !isSet("showALg") && !isSet("showALdom") && (getTreeIds() == null || getTreeIds().isEmpty())) {

				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}

				// Consider restriction to one particular group if needed
				GScene stand = step.getScene();
				Collection trees = doFilter(stand);
				if (!trees.isEmpty()) {
					getTreeIds().add("" + ((Tree) trees.iterator().next()).getId());
				}

			}

			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // optional: y coordinates (meanAL)
			Vector c3 = new Vector(); // optional: y coordinates (minAL)
			Vector c4 = new Vector(); // optional: y coordinates (maxAL)
			Vector c5 = new Vector(); // optional: y coordinates (ALStdDev)
			Vector c6 = new Vector(); // optional: y coordinates (ALg)
			Vector c7 = new Vector(); // optional: y coordinates (ALdom)
			Vector cy[] = new Vector[treeNumber]; // y coordinates (treeAL)

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
				if (isSet("showMeanAL")) {
					c2.add(new Double(((MeanALProvider) methodProvider).getMeanAL(stand, trees)));
				}
				if (isSet("showMinAL")) {
					c3.add(new Double(((MinALProvider) methodProvider).getMinAL(stand, trees)));
				}
				if (isSet("showMaxAL")) {
					c4.add(new Double(((MaxALProvider) methodProvider).getMaxAL(stand, trees)));
				}
				if (isSet("showALStandardDeviation")) {
					c5.add(new Double(((ALStandardDeviationProvider) methodProvider).getALStandardDeviation(stand,
							trees)));
				}
				if (isSet("showALg")) {
					c6.add(new Double(((ALgProvider) methodProvider).getALg(stand, trees)));
				}
				if (isSet("showALdom")) {
					c7.add(new Double(((ALdomProvider) methodProvider).getALdom(stand, trees)));
				}

				// Get AL for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						ALProvider t2 = (ALProvider) t;
						double AL = t2.getAL(); // AL of the tree
						cy[n].add(new Double(AL));
					}
					n++;
				}

			}

			curves.clear();
			curves.add(c1);

			if (isSet("showMeanAL")) {
				curves.add(c2);
			}
			if (isSet("showMinAL")) {
				curves.add(c3);
			}
			if (isSet("showMaxAL")) {
				curves.add(c4);
			}
			if (isSet("showALStandardDeviation")) {
				curves.add(c5);
			}
			if (isSet("showALg")) {
				curves.add(c6);
			}
			if (isSet("showALdom")) {
				curves.add(c7);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showMeanAL")) {
				Vector y1Labels = new Vector();
				y1Labels.add("ALm");
				labels.add(y1Labels);
			}

			if (isSet("showMinAL")) {
				Vector y2Labels = new Vector();
				y2Labels.add("ALmin");
				labels.add(y2Labels);
			}

			if (isSet("showMaxAL")) {
				Vector y3Labels = new Vector();
				y3Labels.add("ALmax");
				labels.add(y3Labels);
			}

			if (isSet("showALStandardDeviation")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Sigma");
				labels.add(y4Labels);
			}

			if (isSet("showALg")) {
				Vector y5Labels = new Vector();
				y5Labels.add("ALg");
				labels.add(y5Labels);
			}

			if (isSet("showALdom")) {
				Vector y6Labels = new Vector();
				y6Labels.add("ALdom");
				labels.add(y6Labels);
			}

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeAL.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeAL.xLabel"));
		v.add(Translator.swap("DETimeAL.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
