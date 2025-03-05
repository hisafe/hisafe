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
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * Relative dominant diameter versus relative density - Parameters used to drive
 * Simmem simulations Still does not group by scene parts!
 * 
 * @author C. Orazio, relative diameter by relative density (SIMMEM drivers)
 *         2014
 */
public class DEDdom01Nha01 extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEDdom01Nha01");
	}

	/**
	 * Constructor.
	 */
	public DEDdom01Nha01() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEDdom01Nha01(GenericExtensionStarter s) {
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
		return getNamePrefix() + Translator.swap("DEDdom01Nha01.name");
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
		return Translator.swap("DEDdom01Nha01.description");
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
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		// addConfigProperty (DataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		// tl 29.1.2007 from DEGirthClassN
		// addBooleanProperty ("roundN"); // fc - 22.8.2006 - n may be double
		// (NZ1) -> round becomes an option
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
		// System.out.print ("DEDdom01Nha01 : extraction requested...");

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
		// methodProvider = step.getProject ().getModel ().getMethodProvider ();

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

			

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEDdom01Nha01.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DEDdom01Nha01.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DEDdom01Nha01.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DEDdom01Nha01.yLabel"));
		}
		return v;
	}

}
