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
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.CrownRadiusProvider;

/**
 * Crown radius of trees over time.
 * 
 * @author C. Madelaine - May 2009
 */
public class DETimeCrownRadius extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCrownRadius");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeCrownRadius() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCrownRadius(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCrownRadius.c ()", "Exception during construction", e);
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
			if (mp instanceof CrownRadiusProvider) {
				return true;
			} else {
				return false;
			}
			// GStand s = ((Step) m.getScenario ().getRoot ()).getStand ();
			// if (!(s instanceof TreeCollection)) {return false;}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCrownRadius.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCrownRadius.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "C. Madelaine";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeCrownRadius.description");
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

		// checkMethodProvider ();

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		// addConfigProperty (DataExtractor.TREE_GROUP); // group
		// multiconfiguration
		// addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
		/*
		 * addBooleanProperty ("showMeanAge"); addBooleanProperty
		 * ("showMinAge"); addBooleanProperty ("showMaxAge"); addBooleanProperty
		 * ("showAdom"); addBooleanProperty ("showAgeStandardDeviation");
		 * addBooleanProperty ("showAg");
		 */

	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	/*
	 * private void checkMethodProvider () { // Retrieve method provider
	 * methodProvider = step.getScenario ().getModel ().getMethodProvider ();
	 * 
	 * if (methodProvider instanceof AgeGProvider) {availableAg = true;} if
	 * (methodProvider instanceof AgeDomProvider) {availableAdom = true;} if
	 * (methodProvider instanceof MeanAgeProvider) {availableAm = true;} if
	 * (methodProvider instanceof AgeStandardDeviationProvider)
	 * {availableAgeStandardDeviation = true;} if (methodProvider instanceof
	 * MinAgeProvider) {availableAmin = true;} if (methodProvider instanceof
	 * MaxAgeProvider) {availableAmax = true;}
	 * 
	 * }
	 */

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
			// If no tree selection, take the first in the tree list of the
			// stand
			if (getTreeIds().isEmpty()) {
				if (step.getScene() instanceof TreeList) {
					TreeList std = (TreeList) step.getScene();
					Collection trees = std.getTrees();
					if (trees != null && !trees.isEmpty()) {
						Tree t = (Tree) trees.iterator().next();
						getTreeIds().add("" + t.getId());
					}
				}
			}

			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector cy[] = new Vector[treeNumber]; // y coordinates

			for (int i = 0; i < treeNumber; i++) {
				cy[i] = new Vector();
			}

			CrownRadiusProvider mp = (CrownRadiusProvider) methodProvider;

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				// Collection trees = doFilter (stand); // fc - 5.4.2004

				int year = stand.getDate();
				c1.add(new Integer(year));

				/*
				 * if (isSet ("showMeanAge")) { c2.add (new Double
				 * (((MeanAgeProvider) methodProvider).getMeanAge (stand,
				 * trees))); }
				 */

				// Get Crown radius for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						double crownRadius = mp.getCrownRadius(t); // crown
																	// radius of
																	// the tree
						cy[n].add(new Double(crownRadius));
					}
					n++;
				}

			}

			curves.clear();
			curves.add(c1);

			labels.clear();
			labels.add(new Vector()); // no x labels

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);

				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCrownRadius.doExtraction ()", "Exception", exc);
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
		v.add(Translator.swap("DETimeCrownRadius.xLabel"));
		v.add(Translator.swap("DETimeCrownRadius.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
