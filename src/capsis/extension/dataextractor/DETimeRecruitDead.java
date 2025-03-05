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

import java.awt.Color;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFColoredCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * Number of recruit and dead versus date.
 * 
 * @author B. Courbaud - November 2000
 */
public class DETimeRecruitDead extends DETimeG implements DFColoredCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeRecruitDead");
	}
	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeRecruitDead() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeRecruitDead(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRecruitDead.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeList))
				return false;

			TreeList stand = (TreeList) s;

			// fc - 20.11.2003 - If stand is empty, return true until we know
			// more
			// Some simulations (ex: Mountain) may begin with empty stand to
			// test regeneration and this tool may still be compatible later
			if (stand.getTrees().isEmpty())
				return true;

			// fc - 20.11.2003 - If stand is not empty, no tree must be
			// Numberable
			Collection reps = Tools.getRepresentatives(stand.getTrees());
			for (Iterator i = reps.iterator(); i.hasNext();) {
				if (i.next() instanceof Numberable)
					return false;

			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRecruitDead.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeRecruitDead.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeRecruitDead.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.2";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.2";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addBooleanProperty("showRecruits", true);
		addBooleanProperty("showDeads", true);

		// fc - 30.1.2003 - used methods don't rely on groups (groups are
		// unusable) addConfigProperty (DataExtractor.TREE_GROUP);
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
			// per Ha computation
			double coefHa = 1d;
			if (isSet("perHectare")) {
				coefHa = 10000d / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector<Step> steps = step.getProject().getStepsFromRoot(step);

			Vector<Integer> c1 = new Vector<Integer>(); // x coordinates
			Vector<Double> c2 = new Vector<Double>(); // y coordinates
			Vector<Double> c3 = new Vector<Double>(); // y' coordinates

			boolean firstTime = true;

			// Data extraction : points with (Integer, Double) coordinates
			for (Step s : steps) {

				// Consider restriction to one particular group if needed
				TreeList stand = (TreeList) s.getScene();

				// fc+bc-7.1.2021 Added recruitment in processPostIntervention, removed continue
				// below
//				// fc+bc-21.9.2016 Remove interventions, not expected here
//				if (stand.isInterventionResult())
//					continue;

				int date = stand.getDate();

				double recruitNb = 0d;
				double deadNb = 0d;

				Step prevStep = null;
				TreeList prevStand = null;

				prevStep = (Step) s.getFather();

				if (firstTime) {
					firstTime = false;
					c1.add(new Integer(date));
					if (isSet("showRecruits")) {
						c2.add(0d);
					} // added this to prevent an error when opening on a root
						// step
					if (isSet("showDeads")) {
						c3.add(0d);
					} // added this to prevent an error when opening on a root
						// step

				} else {
					c1.add(new Integer(date));

					prevStand = (TreeList) prevStep.getScene();

					if (isSet("showRecruits")) {
						recruitNb = stand.getNewTrees(prevStand).size() * coefHa;
						c2.add(recruitNb);
					}
					if (isSet("showDeads")) {
						deadNb = stand.getMissingTrees(prevStand).size() * coefHa;
						c3.add(deadNb);
					}
				}
			}

			curves.clear();
			curves.add(c1);
			if (isSet("showRecruits")) {
				curves.add(c2);
			}
			if (isSet("showDeads")) {
				curves.add(c3);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showRecruits")) {
				Vector y1Labels = new Vector();
				y1Labels.add(Translator.swap("DETimeRecruitDead.recruit"));
				labels.add(y1Labels); // y1 : label "R"
			}
			if (isSet("showDeads")) {
				Vector y2Labels = new Vector();
				y2Labels.add(Translator.swap("DETimeRecruitDead.dead"));
				labels.add(y2Labels); // y2 : label "D"
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeRecruitDead.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeRecruitDead.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeRecruitDead.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeRecruitDead.yLabel"));
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
		Vector colors = new Vector();
		colors.add(getColor()); // recruit
		colors.add(Color.BLACK); // dead

		return colors;
	}

}
