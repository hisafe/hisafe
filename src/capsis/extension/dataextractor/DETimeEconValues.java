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
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;


/**
 * Net Value and other econ indicators versus Date.
 * 
 * @author C. Orazio - April 2004
 */
public class DETimeEconValues extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeEconValues");
	}

	/**
	 * Constructor.
	 */
	public DETimeEconValues() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeEconValues(GenericExtensionStarter s) {
		super(s);

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {


		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeEconValues.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "C. Orazio";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeEconValues.description");
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
		// addConfigProperty (DataExtractor.TREE_GROUP);
		// addConfigProperty (DataExtractor.I_TREE_GROUP); // group individual
		// configuration
		String[] tab = { Translator.swap("DETimeEconValues.yLabel1"), Translator.swap("DETimeEconValues.yLabel2"),
				Translator.swap("DETimeEconValues.yLabel3"), Translator.swap("DETimeEconValues.yLabel4") };
		addRadioProperty(tab);
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
		// System.out.print ("DETimeEconValues : extraction requested...");

		if (upToDate) {
			// System.out.println (" upToDate -> NO EXTRACTION");
			return true;
		}
		if (step == null) {
			// System.out.println (" null Step -> NO EXTRACTION");
			return false;
		}

		// Retrieve method provider
		// methodProvider = MethodProviderFactory.getMethodProvider
		// (step.getScenario ().getModel ());
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);
			GModel model = step.getProject().getModel();
			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			// System.out.println ("settings.radioProperties="+Tools.toString
			// (settings.radioProperties));
			// System.out.println ("settings.steps="+steps.toString ());
			// System.out.println
			// ("DETimeEconValues.yLabel1="+isSet(Translator.swap
			// ("DETimeEconValues.yLabel1")));

			// data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				// Filtrable fil = doFilter ((Filtrable) stand);

				int date = stand.getDate();

				// Rounded under
				// GStand reduced = (GStand) fil;
				double V = 0;

				
				c1.add(new Integer(date));
				c2.add(new Double(V));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeEconValues.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		// System.out.println (" MADE");
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeEconValues.xLabel"));
		String ylabel = "empty";
		if (isSet(Translator.swap("DETimeEconValues.yLabel1"))) {
			ylabel = Translator.swap("DETimeEconValues.yLabel1");
		}
		if (isSet(Translator.swap("DETimeEconValues.yLabel2"))) {
			ylabel = Translator.swap("DETimeEconValues.yLabel2");
		}
		if (isSet(Translator.swap("DETimeEconValues.yLabel3"))) {
			ylabel = Translator.swap("DETimeEconValues.yLabel3");
		}
		if (isSet(Translator.swap("DETimeEconValues.yLabel4"))) {
			ylabel = Translator.swap("DETimeEconValues.yLabel4");
		}
		if (isSet("perHectare") && ylabel != Translator.swap("DETimeEconValues.yLabel4")) {
			v.add(ylabel + " (ha)");
		} else {
			v.add(ylabel);
		}
		return v;
	}

}
