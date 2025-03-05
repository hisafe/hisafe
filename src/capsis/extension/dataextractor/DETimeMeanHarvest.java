/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2011  Francois de Coligny
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
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.Group;
import capsis.util.group.Grouper;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.VProvider;

/**
 * Mean harvest over Time.
 * 
 * @author B. Courbaud, F. de Coligny - November 2011
 */
public class DETimeMeanHarvest extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeMeanHarvest");
	}

	protected Vector curves;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeMeanHarvest() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeMeanHarvest(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeMeanHarvest.c ()", "Exception during construction", e);
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
			if (!(mp instanceof VProvider)) {
				return false;
			}
			GScene stand = m.getProject().getRoot().getScene();
			if (!(stand instanceof TreeList))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeMeanHarvest.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeMeanHarvest.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud, F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeMeanHarvest.description");
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
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		addGroupProperty(TreeList.GROUP_ALIVE_TREE, INDIVIDUAL);
//		addGroupProperty(TreeList.GROUP_ALIVE_TREE, INDIVIDUAL);
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
		VProvider mp = (VProvider) methodProvider;

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			double v = 0;

			int date0 = -1;

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				if (date0 < 0)
					date0 = s.getScene().getDate();

				// Consider restriction to one particular group if needed
				TreeList stand = (TreeList) s.getScene();

				Collection<Tree> cutTrees = stand.getTrees("cut");

				// Apply group
				cutTrees = filter(cutTrees);

				if (cutTrees != null && !cutTrees.isEmpty()) {

					double cutV = mp.getV(stand, cutTrees);

					v += cutV;

				}

				int date = stand.getDate();

				int d = date - date0 + 1;

				// int n = cutTrees != null ? cutTrees.size () : 0;
				// System.out.println("DETimeMeanHarvest date0: "+date0+" date: "+date+" d: "+d+" nb cutTrees: "+n+" v: "+v);

				if (d > 0) {
					c1.add(new Integer(date));
					c2.add(new Double(v * coefHa / d));
				}
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeMeanHarvest.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	private Collection filter(Collection trees) {

		if (!isGrouperMode())
			return trees; // no grouper selected
		if (trees == null || trees.isEmpty())
			return trees; // nothing to be filtered

		GrouperManager gm = GrouperManager.getInstance();
		Grouper g = gm.getGrouper(getGrouperName()); // if group not found,
		// return a DummyGrouper

		// fc-16.11.2011 - use a copy of the grouper (several data extractors
		// are updated in several threads, avoid concurrence problems)
		Grouper copy = g.getCopy();

		Collection output = copy.apply(trees, getGrouperName().toLowerCase().startsWith("not "));

		return output;

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
		return null; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeMeanHarvest.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeMeanHarvest.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeMeanHarvest.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 1;
	}

}
