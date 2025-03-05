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
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.VPerronProvider;

/**
 * Perron volume versus Date.
 * 
 * @author S. Turbis - June 2004
 */
public class DETimeVPerron extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeVPerron");
	}

	protected Vector curves;
	protected MethodProvider methodProvider;

	/**
	 * Constructor,
	 */
	public DETimeVPerron() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeVPerron(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVPerron.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof VPerronProvider))
				return false;

			Step root = (Step) m.getProject().getRoot();
			GScene rootStand = root.getScene();
			// Needs a tree list, could be changed
			if (!(rootStand instanceof TreeCollection))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVPerron.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeVPerron.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "S. Turbis";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeVPerron.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.0";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(PaleoDataExtractor.TREE_GROUP); // group
															// multiconfiguration
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// config.
		addDoubleProperty("minDbhInCm", 9.1d);
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
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand); // fc - 5.4.2004

				// restrict to trees higher than minDbh
				double minDbh = getDoubleProperty("minDbhInCm");
				// st - 24.05.2005 put in comments because was causing a problem
				// when calculating non merchant trees, they were removed
				// ~ for (Iterator j = trees.iterator (); j.hasNext ();) {
				// ~ GTree t = (GTree) j.next ();
				// ~ double treeDbh = t.getDbh();
				// ~ if (treeDbh < minDbh){
				// ~ j.remove();
				// ~ }
				// ~}

				int date = stand.getDate();
				// ~double vPerron = ((VPerronProvider)
				// methodProvider).getVPerron
				// ~ (stand, trees) * coefHa; // fc - 24.3.2004
				// st - 24.05.2005 added a limit to merchant trees to be more
				// flexible
				double vPerron = ((VPerronProvider) methodProvider).getVPerron(stand, trees, minDbh) * coefHa;

				c1.add(new Integer(date));
				c2.add(new Double(vPerron));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeVPerron.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeVPerron.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeVPerron.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeVPerron.yLabel"));
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
