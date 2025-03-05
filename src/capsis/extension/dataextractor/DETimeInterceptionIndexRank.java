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
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.TreeInterceptionIndexProvider;

/**
 * Individual Interception Index versus Year (for one or several individuals).
 * 
 * @author B.Courbaud - 2003
 */
public class DETimeInterceptionIndexRank extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeInterceptionIndexRank");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeInterceptionIndexRank() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeInterceptionIndexRank(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeInterceptionIndexRank.c ()", "Exception during construction", e);
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
			if (!(mp instanceof NProvider)) {
				return false;
			}
			if (!(mp instanceof TreeInterceptionIndexProvider)) {
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

			// GTree t = tc.getTrees ().iterator ().next ();
			// if (t instanceof Numberable) {return false;}
			// if (!(t instanceof InterceptionIndexProvider)) {return false;}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeInterceptionIndexRank.matchWith ()",
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
		return Translator.swap("DETimeInterceptionIndexRank.name");
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
		return Translator.swap("DETimeInterceptionIndexRank.description");
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
			Vector c2 = new Vector(); // y coordinates (Tree nb)
			Vector cy[] = new Vector[treeNumber]; // y coordinates (Ii)

			for (int i = 0; i < treeNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				TreeList stand = (TreeList) s.getScene();
				Collection trees = ((TreeCollection) stand).getTrees(); // fc -
																		// 9.4.2004

				int year = stand.getDate();

				c1.add(new Integer(year));
				// c2.add (new Double (((NProvider) methodProvider).getN
				// (stand)));
				double Ntot = ((NProvider) methodProvider).getN(stand, trees); // fc
																				// -
																				// 22.8.2006
																				// -
																				// Numberable
																				// is
																				// double
				c2.add(new Double(Ntot));

				Map m = new TreeMap(); // this map is automatically sorted
				for (Iterator ids = stand.getTreeIds().iterator(); ids.hasNext();) {
					int id = ((Integer) ids.next()).intValue();
					Tree t = stand.getTree(id);
					// double index = ((InterceptionIndexProvider)
					// t).getInterceptionIndex ();
					double index = ((TreeInterceptionIndexProvider) methodProvider).getTreeInterceptionIndex(t);
					m.put(new Double(index), new Integer(id)); // the map is
																// built and is
																// directly
																// sorted on
																// index
																// Double () is
																// a constructor
				}
				Map m2 = new HashMap(); // this map is not sorted (un-necessary)
				int rank = 0;
				Iterator keys = m.keySet().iterator();
				while (keys.hasNext()) {
					// double ix = ((Double)keys.next ()).doubleValue ();
					// //"Double" because in a map, there is only a reference to
					// a double
					Double ix = ((Double) keys.next()); // "Double" because in a
														// map, there is only a
														// reference to a double
					Integer idI = (Integer) m.get(ix);
					int id = idI.intValue();
					rank++;
					m2.put(idI, new Integer(rank));
				}
				// map idRank = calculateRanks(stand);

				// Get Ii for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					Integer idI = new Integer((String) ids.next());
					int id = idI.intValue();
					Tree t = ((TreeCollection) stand).getTree(id);
					if (t == null) {
						cy[n].add(new Double(Double.NaN)); // Double.isNaN ()
					} else {
						rank = ((int) Ntot) + 1 - ((Integer) m2.get(idI)).intValue(); // fc
																						// -
																						// 22.8.2006
																						// -
																						// Numberable
																						// is
																						// double
						cy[n].add(new Double(rank));
					}
					n++;
				}
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			y1Labels.add("N");
			labels.add(y1Labels); // y1 : label "N: Tree Nb"

			for (int i = 0; i < treeNumber; i++) {
				curves.add(cy[i]);

				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeInterceptionIndexRank.doExtraction ()", "Exception caught : ", exc);
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
	// ~ caption += " - "+Translator.swap ("DETimeInterceptionIndexRank.tree")
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
		v.add(Translator.swap("DETimeInterceptionIndexRank.xLabel"));
		v.add(Translator.swap("DETimeInterceptionIndexRank.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
