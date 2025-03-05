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
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.DCIProvider;
import capsis.util.methodprovider.DCIStandardDeviationProvider;
import capsis.util.methodprovider.DCIdomProvider;
import capsis.util.methodprovider.DCIgProvider;
import capsis.util.methodprovider.MaxDCIProvider;
import capsis.util.methodprovider.MeanDCIProvider;
import capsis.util.methodprovider.MinDCIProvider;

/**
 * DCI of trees versus Date. (Hegyi's diameter-distance competition index (Hegyi
 * 1974))
 * 
 * @author S. Turbis - June 2005, review January 2006 to add DCIg and DCIdom
 */
public class DETimeDCI extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeDCI");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	// maybe module does not calculate this property
	private boolean availableDCIm;
	private boolean availableDCIStandardDeviation;
	private boolean availableDCImin;
	private boolean availableDCImax;
	private boolean availableDCIg;
	private boolean availableDCIdom;

	/**
	 * Constructor.
	 */
	public DETimeDCI() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeDCI(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider();

			// According to module capabiblities, disable some configProperties
			setPropertyEnabled("showDCIm", availableDCIm);
			setPropertyEnabled("showDCIStandardDeviation", availableDCIStandardDeviation);
			setPropertyEnabled("showDCImin", availableDCImin);
			setPropertyEnabled("showDCImax", availableDCImax);
			setPropertyEnabled("showDCIg", availableDCIg);
			setPropertyEnabled("showDCIdom", availableDCIdom);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeDCI.c ()", "Exception during construction", e);
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
			if (!(mp instanceof MeanDCIProvider) && !(mp instanceof DCIStandardDeviationProvider)
					&& !(mp instanceof MinDCIProvider) && !(mp instanceof MaxDCIProvider)
					&& !(mp instanceof DCIgProvider) && !(mp instanceof DCIdomProvider)) {
				return false;
			}

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

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeDCI.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeDCI.name");
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
		return Translator.swap("DETimeDCI.description");
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
		// st - 13.09.2005 when we choose a group, we must be contient that the
		// DCI
		// is for that group only and it is like if all other trees were cut
		addConfigProperty(PaleoDataExtractor.TREE_GROUP); // group
															// multiconfiguration
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		// addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration

		// st - 13.09.2005 be carefull to avoid having 2 differents ray of
		// neighbor,
		// one from the main parameter initiation and one from this graph
		addDoubleProperty("rayOfNeighboroodInM", 6d);

		// fc-13.10.2010: added all 'true' below (default)
		addBooleanProperty("showDCIm", true);
		addBooleanProperty("showDCIStandardDeviation", true);
		addBooleanProperty("showDCImin", true);
		addBooleanProperty("showDCImax", true);
		addBooleanProperty("showDCIg", true);
		addBooleanProperty("showDCIdom", true);

	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof MeanDCIProvider) {
			availableDCIm = true;
		}
		if (methodProvider instanceof DCIStandardDeviationProvider) {
			availableDCIStandardDeviation = true;
		}
		if (methodProvider instanceof MinDCIProvider) {
			availableDCImin = true;
		}
		if (methodProvider instanceof MaxDCIProvider) {
			availableDCImax = true;
		}
		if (methodProvider instanceof DCIgProvider) {
			availableDCIg = true;
		}
		if (methodProvider instanceof DCIdomProvider) {
			availableDCIdom = true;
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
			if (!isSet("showDCIm") && !isSet("showDCIStandardDeviation") && !isSet("showDCImin")
					&& !isSet("showDCImax") && !isSet("showDCIg") && !isSet("showDCIdom")
					&& (getTreeIds() == null || getTreeIds().isEmpty())) {

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
			double ray = getDoubleProperty("rayOfNeighboroodInM");
			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates (years)
			Vector c2 = new Vector(); // y coordinates (meanDCI)
			Vector c3 = new Vector(); // y coordinates (maxDCI)
			Vector c4 = new Vector(); // y coordinates (minDCI)
			Vector c5 = new Vector(); // y coordinates (DCIStdDev)
			Vector c6 = new Vector(); // y coordinates (DCIg)
			Vector c7 = new Vector(); // y coordinates (DCIdom)
			Vector cy[] = new Vector[treeNumber]; // y coordinates (treeDCI)

			for (int i = 0; i < treeNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				TreeList stand = (TreeList) s.getScene();
				Collection trees = doFilter(stand); // fc - 5.4.2004

				int year = stand.getDate();

				c1.add(new Integer(year));
				if (isSet("showDCIm")) {
					c2.add(new Double(((MeanDCIProvider) methodProvider).getMeanDCI(stand, trees, ray)));
				}
				if (isSet("showDCImax")) {
					c3.add(new Double(((MaxDCIProvider) methodProvider).getMaxDCI(stand, trees, ray)));
				}
				if (isSet("showDCImin")) {
					c4.add(new Double(((MinDCIProvider) methodProvider).getMinDCI(stand, trees, ray)));
				}
				if (isSet("showDCIStandardDeviation")) {
					c5.add(new Double(((DCIStandardDeviationProvider) methodProvider).getDCIStandardDeviation(stand,
							trees, ray)));
				}
				if (isSet("showDCIg")) {
					c6.add(new Double(((DCIgProvider) methodProvider).getDCIg(stand, trees, ray)));
				}
				if (isSet("showDCIdom")) {
					c7.add(new Double(((DCIdomProvider) methodProvider).getDCIdom(stand, trees, ray)));
				}

				// Get DCI for each tree
				double DCI = 0.0;
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					SpatializedTree t = (SpatializedTree) ((TreeCollection) stand).getTree(id);
					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						DCI = ((DCIProvider) methodProvider).getDCI(stand, t, ray);
						cy[n].add(new Double(DCI));
					}
					n++;
				}

			}

			curves.clear();
			curves.add(c1);

			if (isSet("showDCIm")) {
				curves.add(c2);
			}
			if (isSet("showDCImax")) {
				curves.add(c3);
			}
			if (isSet("showDCImin")) {
				curves.add(c4);
			}
			if (isSet("showDCIStandardDeviation")) {
				curves.add(c5);
			}
			if (isSet("showDCIg")) {
				curves.add(c6);
			}
			if (isSet("showDCIdom")) {
				curves.add(c7);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showDCIm")) {
				Vector y1Labels = new Vector();
				y1Labels.add("DCIm");
				labels.add(y1Labels);
			}
			if (isSet("showDCImax")) {
				Vector y2Labels = new Vector();
				y2Labels.add("DCImax");
				labels.add(y2Labels);
			}
			if (isSet("showDCImin")) {
				Vector y3Labels = new Vector();
				y3Labels.add("DCImin");
				labels.add(y3Labels);
			}
			if (isSet("showDCIStandardDeviation")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Sigma");
				labels.add(y4Labels);
			}
			if (isSet("showDCIg")) {
				Vector y5Labels = new Vector();
				y5Labels.add("DCIg");
				labels.add(y5Labels);
			}
			if (isSet("showDCIdom")) {
				Vector y6Labels = new Vector();
				y6Labels.add("DCIdom");
				labels.add(y6Labels);
			}

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeDCI.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeDCI.xLabel"));
		v.add(Translator.swap("DETimeDCI.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
