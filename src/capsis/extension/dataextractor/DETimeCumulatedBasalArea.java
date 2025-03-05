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
import capsis.util.methodprovider.CumulatedBasalAreaProvider;

/**
 * Cumulated Basal area over Time.
 * 
 * @author Christine Deleuze, Olivier Pain - June 2007
 */
public class DETimeCumulatedBasalArea extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCumulatedBasalArea");
	}

	protected Vector curves;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeCumulatedBasalArea() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCumulatedBasalArea(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCumulatedBasalArea.c ()", "Exception during construction", e);
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
			if (!(mp instanceof CumulatedBasalAreaProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCumulatedBasalArea.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCumulatedBasalArea.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "Christine Deleuze, Olivier Pain";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeCumulatedBasalArea.description");
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
		// ~ addConfigProperty (DataExtractor.TREE_GROUP); // group
		// multiconfiguration
		// ~ addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
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
		CumulatedBasalAreaProvider mp = (CumulatedBasalAreaProvider) methodProvider;

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
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				// ~ Collection trees = doFilter (stand); // fc - 5.4.2004

				int date = stand.getDate();
				double cumulatedBasalArea = mp.getCumulatedBasalArea(s) * coefHa;

				c1.add(new Integer(date));
				c2.add(new Double(cumulatedBasalArea));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCumulatedBasalArea.doExtraction ()", "Exception caught : ", exc);
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
		return null; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeCumulatedBasalArea.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeCumulatedBasalArea.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeCumulatedBasalArea.yLabel"));
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
