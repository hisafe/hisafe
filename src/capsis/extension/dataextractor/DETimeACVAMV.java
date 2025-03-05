/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Philippe Dreyfus
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
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.ACVProvider;
import capsis.util.methodprovider.AMVProvider;

/**
 * Current volume increment and Mean volume increment versus Date.
 * 
 * @author L. Saint-André - August 2002
 */
public class DETimeACVAMV extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeACVAMV");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeACVAMV() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeACVAMV(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeACVAMV.c ()", "Exception during construction : ", e);
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
			if (!(mp instanceof ACVProvider)) {
				return false;
			}
			if (!(mp instanceof AMVProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeACVAMV.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeACVAMV.name");
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
		return Translator.swap("DETimeACVAMV.description");
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
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
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
		// System.out.print ("DETimeHdomHg : extraction requested...");

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
			Vector c3 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			Iterator i = steps.iterator();
			if (!i.hasNext()) {
				throw new Exception("DETimeACVAMV needs at least two steps");
			} // fc - 9.4.2004
			Step s = (Step) i.next();

			GScene previousStand = s.getScene();
			Collection previousTrees = doFilter(previousStand);

			if (!i.hasNext()) {
				throw new Exception("DETimeACVAMV needs at least two steps");
			} // fc - 9.4.2004
			do {
				s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();

				double ACV = ((ACVProvider) methodProvider).getACV(stand, trees, previousStand, previousTrees);
				double AMV = ((AMVProvider) methodProvider).getAMV(stand, trees);

				c1.add(new Integer(date));
				c2.add(new Double(ACV));
				c3.add(new Double(AMV));

				previousStand = stand;
				previousTrees = trees;
			} while (i.hasNext());

			curves.clear();
			curves.add(c1);
			curves.add(c2);
			curves.add(c3);

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			y1Labels.add("ACV");
			labels.add(y1Labels);
			Vector y2Labels = new Vector();
			y2Labels.add("AMV");
			labels.add(y2Labels);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeACVAMV.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeACVAMV.xLabel"));
		v.add(Translator.swap("DETimeACVAMV.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 2;
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

}
