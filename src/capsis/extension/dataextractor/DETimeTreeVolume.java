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
import capsis.util.methodprovider.TreeVolumeProvider;

/**
 * Individual tree volume.
 * 
 * @author G. Ligot - November 2014
 */
public class DETimeTreeVolume extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeTreeVolume");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeTreeVolume() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeTreeVolume(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeTreeVolume.c ()", "Exception during construction", e);
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
			GScene s = ((Step) m.getProject().getRoot()).getScene();

			MethodProvider mp = m.getMethodProvider();
			if (mp instanceof TreeVolumeProvider)
				return true;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeTreeVolume.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return false;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeTreeVolume.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "G. Ligot";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeTreeVolume.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1";
	
	/**
	 * This method is called by superclass DataExtractor constructor. If
	 * previous config was saved in a file, this method may not be called. See
	 * etc/extensions.settings file
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);
	}

	/**
	 * From DataExtractor SuperClass. Computes the data series. This is the real
	 * output building. It needs a particular Step. Return false if trouble
	 * while extracting.
	 */
	public boolean doExtraction() {
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		// Retrieve method provider
		MethodProvider methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// If no curve at all, choose one tree
			// fc - 6.2.2004
			if (getTreeIds() == null || getTreeIds().isEmpty()) {

				TreeCollection tc = (TreeCollection) step.getScene();
				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}
				int minId = Integer.MAX_VALUE;
				for (Iterator i = tc.getTrees().iterator(); i.hasNext();) {
					Tree t = (Tree) i.next();
					minId = Math.min(minId, t.getId()); // fc - 27.9.2006
				}
				getTreeIds().add("" + minId);
			}

			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates (years)

			int treeCurvesNumber = treeNumber;
			Vector cy[] = new Vector[treeCurvesNumber]; // y coordinates
														// (volumes)

			for (int i = 0; i < treeCurvesNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();
				Collection trees = null;
				trees = doFilter(stand);

				int year = stand.getDate();

				c1.add(new Integer(year));

				// Get dbh for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					double number = 1; // fc - 22.8.2006 - Numberable returns
										// double
					if (t instanceof Numberable)
						number = ((Numberable) t).getNumber();
					if (t == null || t.isMarked() || number == 0) { // copied
																	// from fc &
																	// phd -
																	// 5.1.2003
						cy[n++].add(new Double(Double.NaN)); // dbh
					} else {
						cy[n++].add(new Double(((TreeVolumeProvider) methodProvider).getTreeVolume(t)));

					}
				}
			}

			// Curves
			//
			curves.clear();
			curves.add(c1);

			// Labels
			labels.clear();
			labels.add(new Vector()); // no x labels
			for (int i = 0; i < treeCurvesNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeTreeVolume.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeTreeVolume.xLabel"));
		v.add(Translator.swap("DETimeTreeVolume.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
