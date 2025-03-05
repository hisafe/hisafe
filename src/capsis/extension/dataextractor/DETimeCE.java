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
import capsis.commongui.util.Tools;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.spatial.ClarkEvans;
import capsis.util.group.PossiblyGroupableScene;

/**
 * Clark & Evans index versus Date.
 * 
 * @author F. Goreaud - May 2002
 */
public class DETimeCE extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCE");
	}

	/**
	 * Constructor.
	 */
	public DETimeCE() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCE(GenericExtensionStarter s) {
		super(s);

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

			Step root = (Step) m.getProject().getRoot();
			GScene stand = root.getScene();
			if (!(stand instanceof TreeCollection)) {
				return false;
			} // Must be a TreeCollection
			TreeCollection tc = (TreeCollection) stand;

			// fc - 20.11.2003 - If stand is empty, return true until we know
			// more
			// Some simulations (ex: Mountain) may begin with empty stand to
			// test regeneration and this tool may still be compatible later
			//
			if (tc.getTrees().isEmpty()) {
				return true;
			}

			// fc - 20.11.2003 - If stand is not empty, all trees must be
			// GMadTrees
			// Do not limit test to first tree (some modules mix GMaddTrees and
			// GMaidTrees -> must not be compatible)
			//
			Collection reps = Tools.getRepresentatives(tc.getTrees()); // one
																		// instance
																		// of
																		// each
																		// class
			// ~ if (reps.size () == 1
			// ~ && reps.iterator ().next () instanceof GMaddTree) {return
			// true;}
			// Possibly several classes of GMaddTree
			// A. Piboule - 29.3.2004
			//
			for (Iterator i = reps.iterator(); i.hasNext();) {
				if (!(i.next() instanceof SpatializedTree)) {
					return false;
				}
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCE.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCE.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. Goreaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeCE.description");
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
		// addConfigProperty (DataExtractor.HECTARE);
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This extractor computes Number of trees in the stand
	 * versus Date.
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
			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();
				
				GScene stand = s.getScene();

				// fc-14.8.2019 PlantaStand is a TreeCollection, but not
				// PossiblyGroupableScene
				TreeCollection tc = (TreeCollection) stand;
				Collection aux = tc.getTrees(); // default
				
				if (tc instanceof PossiblyGroupableScene) {
					// Restriction to a group if possible and needed
					aux = doFilter(stand);
				}

				int date = stand.getDate();

				// Retrieve trees for this step
				Iterator trees = aux.iterator();
				int pointnb = aux.size();

				// Limited in size! (java initializes each value to 0)
				double x[] = new double[pointnb + 1];
				double y[] = new double[pointnb + 1];
				double xmi = step.getScene().getOrigin().x;
				double xma = xmi + step.getScene().getXSize();
				double ymi = step.getScene().getOrigin().y;
				double yma = ymi + step.getScene().getYSize();

				// Create output data
				int j = -1;
				while (trees.hasNext()) {
					j += 1;
					SpatializedTree t = (SpatializedTree) trees.next();
					x[j] = t.getX(); // x : m // FG
					y[j] = t.getY(); // y : m // FG
				}
				int pointNumber = j + 1;

				Log.println("DETimeCE -> pointNumber : " + pointNumber + " xmi : " + xmi + "xma : " + xma + "ymi : "
						+ ymi + " yma : " + yma);

				c1.add(new Integer(date));
				c2.add(new Double(ClarkEvans.computeCE(x, y, pointNumber, xmi, xma, ymi, yma)));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCE.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeCE.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeCE.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeCE.yLabel"));
		}
		return v;
	}

}
