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
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.TopEndD4NProvider;
import capsis.util.methodprovider.TopEndD6NProvider;

/**
 * Top end diameter 6 number / top end diameter 4 number ratio versus Date.
 * 
 * @author L. Saint-André - August 2002
 */
public class DETimeRatioN6N4 extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeRatioN6N4");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeRatioN6N4() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeRatioN6N4(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRatioN6N4.c ()", "Exception during construction : ", e);
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
			if (!(mp instanceof TopEndD4NProvider))
				return false; // ex NDec4
			if (!(mp instanceof TopEndD6NProvider))
				return false; // ex NDec6
			if (!(mp instanceof NProvider))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeRatioN6N4.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeRatioN6N4.name");
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
		return Translator.swap("DETimeRatioN6N4.description");
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
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		// ~ addConfigProperty (DataExtractor.HECTARE); // fc - 9.4.2004 -
		// unused below
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP);
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
			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();
				int NDec4 = ((TopEndD4NProvider) methodProvider).getTopEndD4N(stand, trees);
				int NDec6 = ((TopEndD6NProvider) methodProvider).getTopEndD6N(stand, trees);
				// int NReal = ((NProvider) methodProvider).getN ((GStand) fil);
				c1.add(new Integer(date));
				c2.add(new Integer(NDec4));
				c3.add(new Integer(NDec6));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);
			curves.add(c3);

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			y1Labels.add("Dec4");
			labels.add(y1Labels);
			Vector y2Labels = new Vector();
			y2Labels.add("Dec6");
			labels.add(y2Labels);
		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeRatioN6N4.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeRatioN6N4.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeRatioN6N4.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeRatioN6N4.yLabel"));
		}
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
