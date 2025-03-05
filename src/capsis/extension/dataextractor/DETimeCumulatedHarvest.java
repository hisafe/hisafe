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

import java.awt.Color;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFColoredCurves;
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
 * Cumulated harvest over Time.
 * 
 * @author B. Courbaud, F. de Coligny - November 2011
 */
public class DETimeCumulatedHarvest extends PaleoDataExtractor implements DFCurves, DFColoredCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCumulatedHarvest");
	}

	protected Vector curves;
	protected Vector labels;

	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeCumulatedHarvest() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCumulatedHarvest(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCumulatedHarvest.c ()", "Exception during construction", e);
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
			Log.println(Log.ERROR, "DETimeCumulatedHarvest.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCumulatedHarvest.name");
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
		return Translator.swap("DETimeCumulatedHarvest.description");
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

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drgraph.DRGraph";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		addGroupProperty(TreeList.GROUP_ALIVE_TREE, INDIVIDUAL);
//		addGroupProperty(TreeList.GROUP_ALIVE_TREE, INDIVIDUAL);

		addBooleanProperty("cutAlive", true);
		addBooleanProperty("cutDead", true);

		// fc+bc-21.9.2016 REMOVED total option, replaced by
		// preferred renderer DRStackedAreaGraph
		// addBooleanProperty("total", true);
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
			Vector c3 = new Vector(); // y' coordinates
			Vector c4 = new Vector(); // y' coordinates

			double vAliveCut = 0;
			double vDeadCut = 0;

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				TreeList stand = (TreeList) s.getScene();

				int date = stand.getDate();

				// Cut
				Collection<Tree> cutTrees = stand.getTrees("cut");

				// Apply current group if any
				cutTrees = filter(cutTrees);

				if (cutTrees != null && !cutTrees.isEmpty()) {
					double cutV = mp.getV(stand, cutTrees);
					vAliveCut += cutV;
				}

				// DeadCut
				Collection<Tree> deadCutTrees = stand.getTrees("deadCut");

				// Apply current group if any
				deadCutTrees = filter(deadCutTrees);

				if (deadCutTrees != null && !deadCutTrees.isEmpty()) {
					double deadCutV = mp.getV(stand, deadCutTrees);
					vDeadCut += deadCutV;
				}

				double vTotalCut = vAliveCut + vDeadCut;

				c1.add(new Integer(date));
				c2.add(new Double(vAliveCut * coefHa));
				c3.add(new Double(vDeadCut * coefHa));
				c4.add(new Double(vTotalCut * coefHa));
			}

			curves.clear();
			curves.add(c1);
			if (isSet("cutAlive"))
				curves.add(c2);
			if (isSet("cutDead"))
				curves.add(c3);
			// if (isSet("total"))
			// curves.add(c4);

			labels.clear();
			labels.add(new Vector()); // no x labels

			if (isSet("cutAlive")) {
				Vector y1Labels = new Vector();
				y1Labels.add(Translator.swap("DETimeCumulatedHarvest.aliveHarvested"));
				labels.add(y1Labels);
			}

			if (isSet("cutDead")) {
				Vector y2Labels = new Vector();
				y2Labels.add(Translator.swap("DETimeCumulatedHarvest.deadHarvested"));
				labels.add(y2Labels);
			}

			// if (isSet("total")) {
			// Vector y3Labels = new Vector();
			// y3Labels.add(Translator.swap("DETimeCumulatedHarvest.totalHarvested"));
			// labels.add(y3Labels);
			// }

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCumulatedHarvest.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	private Collection filter(Collection trees) {

		if (!isGrouperMode()) {
			return trees;
		} // no grouper selected

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
		return labels;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeCumulatedHarvest.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeCumulatedHarvest.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeCumulatedHarvest.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

	@Override
	public Vector getColors() {
		Vector v = new Vector();
		Color singleColor = getColor(); // see AbstractDataExtractor
		if (isSet("cutAlive"))
			v.add(Color.RED); // alive
		if (isSet("cutDead"))
			v.add(Color.BLACK); // dead
			// if (isSet("total"))
			// v.add(singleColor); // total
		return v;
	}

}
