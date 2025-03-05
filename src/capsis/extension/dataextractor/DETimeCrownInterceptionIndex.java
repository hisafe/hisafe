/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.DominantCrownInterceptionIndexProvider;
import capsis.util.methodprovider.MeanCrownInterceptionIndexProvider;
import capsis.util.methodprovider.TreeCrownInterceptionIndexProvider;

/**
 * Individual Interception Index versus Year (for one or several individuals).
 * 
 * @author B.Courbaud - 2003
 */
public class DETimeCrownInterceptionIndex extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCrownInterceptionIndex");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeCrownInterceptionIndex() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCrownInterceptionIndex(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCrownInterceptionIndex.c ()", "Exception during construction", e);
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
			if (!(mp instanceof DominantCrownInterceptionIndexProvider)) {
				return false;
			}
			if (!(mp instanceof MeanCrownInterceptionIndexProvider)) {
				return false;
			}
			if (!(mp instanceof TreeCrownInterceptionIndexProvider)) {
				return false;
			}

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty()) {
				return true;
			} // bare soil problem

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCrownInterceptionIndex.matchWith ()",
					"Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCrownInterceptionIndex.name");
	}

	/**
	 * From Extension interface.	
	 */
	@Override
	public String getAuthor() {
		return "B.Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeCrownInterceptionIndex.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		addBooleanProperty("showDomIx");
		addBooleanProperty("showMeanIx");
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This output computes the Grundflache (basal area) of the
	 * stand versus year from the root Step to this one.
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
			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates (years)
			Vector c2 = new Vector(); // y coordinates (MeanIx)
			Vector c3 = new Vector(); // y coordinates (DomIx)
			Vector cy[] = new Vector[treeNumber]; // y coordinates (treeIx)

			for (int i = 0; i < treeNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();
				Collection trees = doFilter(stand);
				int year = stand.getDate();

				c1.add(new Integer(year));
				c2.add(new Double(((MeanCrownInterceptionIndexProvider) methodProvider).getMeanCrownInterceptionIndex(
						stand, trees)));
				c3.add(new Double(((DominantCrownInterceptionIndexProvider) methodProvider)
						.getDominantCrownInterceptionIndex(stand, trees)));

				// Get Ix for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						// cy[n].add (new Double (((InterceptionIndexProvider)
						// t).getInterceptionIndex ()));
						cy[n].add(new Double(((TreeCrownInterceptionIndexProvider) methodProvider)
								.getTreeCrownInterceptionIndex(t)));
					}
					n++;
				}
			}

			curves.clear();
			curves.add(c1);
			if (isSet("showMeanIx")) {
				curves.add(c2);
			}
			if (isSet("showDomIx")) {
				curves.add(c3);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showMeanIx")) {
				Vector y1Labels = new Vector();
				y1Labels.add("Mean");
				labels.add(y1Labels);
			}
			if (isSet("showDomIx")) {
				Vector y2Labels = new Vector();
				y2Labels.add("Dom");
				labels.add(y2Labels);
			}
			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCrownInterceptionIndex.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	// fc - 21.4.2004 - DataExtractor.getCaption () is better
	// ~ public String getCaption () {
	// ~ String caption = getStep ().getCaption ();
	// ~ if (treeIds != null && !treeIds.isEmpty ()) {
	// ~ caption += " - "+Translator.swap ("DETimeCrownInterceptionIndex.tree")
	// ~ +" "+Tools.toString (treeIds);
	// ~ }
	// ~ return caption;
	// ~ }

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
		v.add(Translator.swap("DETimeCrownInterceptionIndex.xLabel"));
		v.add(Translator.swap("DETimeCrownInterceptionIndex.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
