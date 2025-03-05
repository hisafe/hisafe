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
import capsis.util.methodprovider.DbhIncrementDomProvider;
import capsis.util.methodprovider.DbhIncrementGProvider;
import capsis.util.methodprovider.DbhIncrementProvider;
import capsis.util.methodprovider.DbhIncrementStandardDeviationProvider;
import capsis.util.methodprovider.MaxDbhIncrementProvider;
import capsis.util.methodprovider.MeanDbhIncrementProvider;
import capsis.util.methodprovider.MinDbhIncrementProvider;

/**
 * Dbh increment of trees versus Date.
 * 
 * @author S. Turbis - July 2005, review August 2005 to add boolean variables,
 *         review January 2006 to add DIg and DIdom
 */
public class DETimeDbhIncrement extends PaleoDataExtractor implements DFCurves {
	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeDbhIncrement");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	// maybe module does not calculate this property
	private boolean availableDIm;
	private boolean availableDIStdDev;
	private boolean availableDImin;
	private boolean availableDImax;
	private boolean availableDIg;
	private boolean availableDIdom;

	/**
	 * Constructor.
	 */
	public DETimeDbhIncrement() {		
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeDbhIncrement(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider();

			// According to module capabiblities, disable some configProperties
			setPropertyEnabled("showDIm", availableDIm);
			setPropertyEnabled("showDIStdDev", availableDIStdDev);
			setPropertyEnabled("showDImin", availableDImin);
			setPropertyEnabled("showDImax", availableDImax);
			setPropertyEnabled("showDIg", availableDIg);
			setPropertyEnabled("showDIdom", availableDIdom);
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeDbhIncrement.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) 
				return false;
			
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) 
				return false;

			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty())
				return true; // bare soil problem
			 
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) 
				return false;
			
			if (!(t instanceof DbhIncrementProvider)) 
				return false;
			

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeDbhIncrement.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeDbhIncrement.name");
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
		return Translator.swap("DETimeDbhIncrement.description");
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
		addBooleanProperty("showDIm", true);
		addBooleanProperty("showDImin");
		addBooleanProperty("showDImax");
		addBooleanProperty("showDIStdDev");
		addBooleanProperty("showDIg");
		addBooleanProperty("showDIdom");

	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc-6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof MeanDbhIncrementProvider) 
			availableDIm = true;
		
		if (methodProvider instanceof DbhIncrementStandardDeviationProvider) 
			availableDIStdDev = true;
		
		if (methodProvider instanceof MinDbhIncrementProvider) 
			availableDImin = true;
		
		if (methodProvider instanceof MaxDbhIncrementProvider) 
			availableDImax = true;
		
		if (methodProvider instanceof DbhIncrementGProvider) 
			availableDIg = true;
		
		if (methodProvider instanceof DbhIncrementDomProvider) 
			availableDIdom = true;
		

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
			if (!isSet("showDIm") && !isSet("showDImin") && !isSet("showDImax") && !isSet("showDIStdDev")
					&& !isSet("showDIg") && !isSet("showDIdom") && (getTreeIds() == null || getTreeIds().isEmpty())) {

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
			Vector c2 = new Vector(); // optional: y coordinates (DIm)
			Vector c3 = new Vector(); // optional: y coordinates (DImin)
			Vector c4 = new Vector(); // optional: y coordinates (DImax)
			Vector c5 = new Vector(); // optional: y coordinates (DIStdDev)
			Vector c6 = new Vector(); // optional: y coordinates (DIg)
			Vector c7 = new Vector(); // optional: y coordinates (DIdom)
			Vector cy[] = new Vector[treeNumber]; // y coordinates (treeDbh)

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

				if (isSet("showDIm")) {
					c2.add(new Double(((MeanDbhIncrementProvider) methodProvider).getMeanDbhIncrement(stand, trees)));
				}
				if (isSet("showDImin")) {
					c3.add(new Double(((MinDbhIncrementProvider) methodProvider).getMinDbhIncrement(stand, trees)));
				}
				if (isSet("showDImax")) {
					c4.add(new Double(((MaxDbhIncrementProvider) methodProvider).getMaxDbhIncrement(stand, trees)));
				}
				if (isSet("showDIStdDev")) {
					c5.add(new Double(((DbhIncrementStandardDeviationProvider) methodProvider)
							.getDbhIncrementStandardDeviation(stand, trees)));
				}
				if (isSet("showDIg")) {
					c6.add(new Double(((DbhIncrementGProvider) methodProvider).getDbhIncrementG(stand, trees)));
				}
				if (isSet("showDIdom")) {
					c7.add(new Double(((DbhIncrementDomProvider) methodProvider).getDbhIncrementDom(stand, trees)));
				}

				// Get Dbh increment for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);
					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						DbhIncrementProvider t2 = (DbhIncrementProvider) t;
						double DbhInc = t2.getDbhIncrement(); // Dbh increment
																// in cm
						cy[n].add(new Double(DbhInc));
					}
					n++;
				}

			}

			curves.clear();
			curves.add(c1);

			if (isSet("showDIm")) {
				curves.add(c2);
			}
			if (isSet("showDImin")) {
				curves.add(c3);
			}
			if (isSet("showDImax")) {
				curves.add(c4);
			}
			if (isSet("showDIStdDev")) {
				curves.add(c5);
			}
			if (isSet("showDIg")) {
				curves.add(c6);
			}
			if (isSet("showDIdom")) {
				curves.add(c7);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showDIm")) {
				Vector y1Labels = new Vector();
				y1Labels.add("DIm");
				labels.add(y1Labels);
			}

			if (isSet("showDImin")) {
				Vector y2Labels = new Vector();
				y2Labels.add("DImin");
				labels.add(y2Labels);
			}

			if (isSet("showDImax")) {
				Vector y3Labels = new Vector();
				y3Labels.add("DImax");
				labels.add(y3Labels);
			}

			if (isSet("showDIStdDev")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Sigma");
				labels.add(y4Labels);
			}

			if (isSet("showDIg")) {
				Vector y5Labels = new Vector();
				y5Labels.add("DIg");
				labels.add(y5Labels);
			}

			if (isSet("showDIdom")) {
				Vector y6Labels = new Vector();
				y6Labels.add("DIdom");
				labels.add(y6Labels);
			}

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeDbhIncrement.doExtraction ()", "Exception", exc);
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
		v.add(Translator.swap("DETimeDbhIncrement.xLabel"));
		v.add(Translator.swap("DETimeDbhIncrement.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
