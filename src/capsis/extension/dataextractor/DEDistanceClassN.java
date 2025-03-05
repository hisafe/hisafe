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

import java.text.NumberFormat;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.SpatializedTree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.RoundMask;
import capsis.defaulttype.plotofcells.SquareCellHolder;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Plot;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

//~ import capsis.lib.amapsim.*;	// fc - 6.2.2004

/**
 * Number of trees over distance classes
 * 
 * @author F. de Coligny, E. Chambon-Dubreuil - September 2006
 */
public class DEDistanceClassN extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEDistanceClassN");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	private boolean availableHg; // maybe module does not calculate this
									// property
	private boolean availableHdom;
	private boolean availableHamapsim;
	private boolean availableHgAmapsim;
	private boolean availableHm; // st - 30.8.2005
	private boolean availableHeightStandardDeviation; // st - 30.8.2005
	private boolean availableHmin; // st - 30.8.2005
	private boolean availableHmax; // st - 30.8.2005

	protected NumberFormat formater;

	/**
	 * Constructor.
	 */
	public DEDistanceClassN() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEDistanceClassN(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			// Used to format decimal part with 2 digits only
			formater = NumberFormat.getInstance();
			formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEDistanceClassN.c ()", "Exception during construction : ", e);
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
			if (!(s instanceof TreeList)) {
				return false;
			}

			TreeList std = (TreeList) s;
			Collection reps = Tools.getRepresentatives(std.getTrees()); // one
																		// instance
																		// of
																		// each
																		// class
			for (Iterator i = reps.iterator(); i.hasNext();) {
				if (!(i.next() instanceof SpatializedTree)) {
					return false;
				}
			}

			Plot plot = s.getPlot();
			if (!(plot instanceof SquareCellHolder)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEDistanceClassN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return Translator.swap("DEDistanceClassN.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. de Coligny, E. Chambon-Dubreuil";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEDistanceClassN.description");
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
	 * This method is called by superclass DataExtractor constructor. If
	 * previous config was saved in a file, this method may not be called. See
	 * etc/extensions.settings file
	 */
	public void setConfigProperties() {

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);

		// These properties may be disabled in constructor according to module
		// capabilities
		//
		addDoubleProperty("classWidthInM", 0.05d);
		addDoubleProperty("maxDistanceInM", 3d);

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
			if (getTreeIds() == null || getTreeIds().isEmpty()) {
				TreeCollection tc = (TreeCollection) step.getScene();
				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}
				getTreeIds().add("" + tc.getTrees().iterator().next().getId());
			}

			// We need only one tree : take the first one
			// ~ int treeNumber = treeIds.size ();

			// Retrieve Steps from root to this step
			// ~ Vector steps = step.getScenario ().getStepsFromRoot (step);

			Vector c1 = new Vector(); // x coordinates (distance classes)
			// ~ Vector c2 = new Vector (); // y coordinates (N)
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			int nCurves = 1; // later: number of values for the chosen enum
								// property + 1 (cumul)
			Vector cy[] = new Vector[nCurves]; // y coordinates (N)

			for (int i = 0; i < nCurves; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			double maxDistance = getDoubleProperty("maxDistanceInM");
			double classWidth = getDoubleProperty("classWidthInM");
			int nClasses = (int) Math.ceil(maxDistance / classWidth);
			int tab[] = new int[nClasses];
			int maxCat = 0;
			int minCat = nClasses;

			// Get height for each tree
			TreeList stand = (TreeList) step.getScene();
			SquareCellHolder plot = (SquareCellHolder) stand.getPlot();

			int id = new Integer((String) getTreeIds().iterator().next()).intValue();
			SpatializedTree t = (SpatializedTree) ((TreeCollection) stand).getTree(id);

			RoundMask mask = new RoundMask(plot, maxDistance, true);
			Collection neighbours = mask.getTreesNear(t);
			Map shiftMap = mask.getShiftMap(); // fc - 21.6.2006
			for (Iterator j = neighbours.iterator(); j.hasNext();) {
				SpatializedTree nei = (SpatializedTree) j.next();
				double d = mask.getDistance(nei);

				int category = (int) (d / classWidth);
				tab[category] += 1;

				if (category > maxCat) {
					maxCat = category;
				}
				if (category < minCat) {
					minCat = category;
				}
			}

			for (int i = minCat; i <= maxCat; i++) {
				c1.add(new Integer(i));

				for (int c = 0; c < nCurves; c++) {
					cy[c].add(new Integer(tab[i]));
				}

				double classBase = i * classWidth;
				l1.add("" + formater.format(classBase) + "-" + formater.format((classBase + classWidth)));
			}

			// Curves
			//
			curves.clear();
			curves.add(c1);

			for (int i = 0; i < nCurves; i++) {
				curves.add(cy[i]);
			}

			// Labels
			//
			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEDistanceClassN.doExtraction ()", "Exception caught : ", exc);
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
	// ~ caption += " - "+Translator.swap ("DEDistanceClassN.tree")
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
		v.add(Translator.swap("DEDistanceClassN.xLabel"));
		v.add(Translator.swap("DEDistanceClassN.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
