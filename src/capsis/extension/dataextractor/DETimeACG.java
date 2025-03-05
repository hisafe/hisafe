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
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.GrouperManager;
import capsis.util.methodprovider.ACGProvider;

/**
 * Current Basal area increment ("Grundflache" (G), "surface terri�re") versus
 * Date.
 * 
 * @author L. Saint-André - August 2002
 */
public class DETimeACG extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeACG");
	}

	protected Vector curves;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeACG() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeACG(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeACG.c ()", "Exception during construction : ", e);
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
			if (!(mp instanceof ACGProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeACG.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeACG.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "L. Saint-André";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeACG.description");
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
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
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

			// Data extraction : points with (Integer, Double) coordinates
			Iterator i = steps.iterator();
			if (!i.hasNext()) {
				throw new Exception("DETimeACG needs at least two steps");
			} // fc - 9.4.2004
			Step s = (Step) i.next();

			GScene previousStand = s.getScene();
			Collection previousTrees = doFilter(previousStand);

			if (!i.hasNext()) {
				throw new Exception("DETimeACG needs at least two steps");
			} // fc - 9.4.2004
			do {
				s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();
				double ACG = ((ACGProvider) methodProvider).getACG(stand, trees, previousStand, previousTrees) * coefHa;

				c1.add(new Integer(date));
				c2.add(new Double(ACG));

				previousStand = stand;
				previousTrees = trees;
			} while (i.hasNext());

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeACG.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * This prefix is built depending on current settings. ex: "+ 25 years /ha"
	 */
	public String getNamePrefix() {
		String prefix = "";
		try {
			if (isCommonGrouper() && isGrouperMode()
					&& GrouperManager.getInstance().getGrouperNames().contains(getGrouperName())) {
				prefix += getGrouperName() + " - ";
			}
			if (isSet("perHectare")) {
				prefix += "/ha - ";
			}
		} catch (Exception e) {
		} // if trouble, prefix is empty
		return prefix;
	}

	/**
	 * From DataFormat interface.
	 */
	// fc - 21.4.2004 - DataExtractor.getCaption () is better
	// ~ public String getCaption () {
	// ~ return getStep ().getCaption ();
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
		return null; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeACG.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeACG.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeACG.yLabel"));
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
