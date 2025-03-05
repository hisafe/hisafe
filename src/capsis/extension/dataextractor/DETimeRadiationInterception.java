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
import capsis.util.methodprovider.DominantInterceptionProvider;
import capsis.util.methodprovider.MeanInterceptionProvider;
import capsis.util.methodprovider.SumInterceptionProvider;
import capsis.util.methodprovider.TreeEnergyProvider;

/**
 * Individual Radiation Interception versus Year (for one or several
 * individuals).
 * 
 * @author B.Courbaud - January 2005
 */
public class DETimeRadiationInterception extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeRadiationInterception");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	private boolean availableMeanInterception; // maybe module does not
												// calculate this property
	private boolean availableDomInterception;

	/**
	 * Constructor.
	 */
	public DETimeRadiationInterception() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeRadiationInterception(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRadiationInterception.c ()", "Exception during construction", e);
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
			if (!(mp instanceof DominantInterceptionProvider))
				return false;
			if (!(mp instanceof MeanInterceptionProvider))
				return false;
			if (!(mp instanceof SumInterceptionProvider))
				return false;
			if (!(mp instanceof TreeEnergyProvider))
				return false;

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection))
				return false;
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty())
				return true; // bare soil problem

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRadiationInterception.matchWith ()",
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
		return Translator.swap("DETimeRadiationInterception.name");
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
		return Translator.swap("DETimeRadiationInterception.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.2";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.2";

	/**
	 * This method is called by superclass DataExtractor constructor. If
	 * previous config was saved in a file, this method may not be called. See
	 * etc/extensions.settings file
	 */
	public void setConfigProperties() {

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		addBooleanProperty("showMeanInterception");
		addBooleanProperty("showSumInterception");
		addBooleanProperty("showDomInterception");
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
			if (!isSet("showMeanInterception") && !isSet("showSumInterception") && !isSet("showDomInterception")
					&& (getTreeIds() == null || getTreeIds().isEmpty())) {
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
			Vector c2 = new Vector(); // optional: y coordinates
										// (MeanInterception)
			Vector c4 = new Vector(); // optional: y coordinates
										// (SumInterception)
			Vector c3 = new Vector(); // optional: y coordinates
										// (DomInterception)

			int treeCurvesNumber = treeNumber;
			Vector cy[] = new Vector[treeCurvesNumber]; // y coordinates
														// (interception)

			for (int i = 0; i < treeCurvesNumber; i++) {
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
				if (isSet("showMeanInterception")) {
					c2.add(new Double(
							((MeanInterceptionProvider) methodProvider).getMeanInterception(stand, trees) / 1000)); // from
																													// MJ/year
																													// to
																													// GJ/year
				}
				if (isSet("showSumInterception")) {
					c4.add(new Double(
							((SumInterceptionProvider) methodProvider).getSumInterception(stand, trees) / 1000)); // from
																													// MJ/year
																													// to
																													// GJ/year
				}
				if (isSet("showDomInterception")) {
					c3.add(new Double(((DominantInterceptionProvider) methodProvider).getDominantInterception(stand,
							trees) / 1000)); // from MJ/year to GJ/year
				}

				// Get interception for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null || t.isMarked()) { // fc & phd - 5.1.2003
						cy[n++].add(new Double(Double.NaN)); // interception
					} else {
						cy[n++].add(new Double(((TreeEnergyProvider) methodProvider).getTreeEnergy(t) / 1000)); // from
																												// JG/year
																												// to
																												// MJ/year
					}
				}
			}

			// Curves
			//
			curves.clear();
			curves.add(c1);
			if (isSet("showMeanInterception")) {
				curves.add(c2);
			}
			if (isSet("showSumInterception")) {
				curves.add(c4);
			}
			if (isSet("showDomInterception")) {
				curves.add(c3);
			}

			// Labels
			//
			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showMeanInterception")) {
				Vector y1Labels = new Vector();
				y1Labels.add("Mean");
				labels.add(y1Labels);
			}
			if (isSet("showSumInterception")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Sum");
				labels.add(y4Labels);
			}
			if (isSet("showDomInterception")) {
				Vector y2Labels = new Vector();
				y2Labels.add("Dom");
				labels.add(y2Labels);
			}

			// n urves = n trees
			for (int i = 0; i < treeCurvesNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeDbh.doExtraction ()", "Exception caught : ", exc);
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
	// ~ caption += " - "+Translator.swap ("DETimeDbh.tree")
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
		v.add(Translator.swap("DETimeRadiationInterception.xLabel"));
		v.add(Translator.swap("DETimeRadiationInterception.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
