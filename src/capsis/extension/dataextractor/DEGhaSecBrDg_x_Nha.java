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
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.SecBrDg_x_NhaProvider;

/**
 * SecBrDg_x_Nha versus Date.
 * 
 * @author Ph. Dreyfus - June 2002
 */
public class DEGhaSecBrDg_x_Nha extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEGhaSecBrDg_x_Nha");
	}

	/**
	 * Constructor.
	 */
	public DEGhaSecBrDg_x_Nha() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEGhaSecBrDg_x_Nha(GenericExtensionStarter s) {
		super(s);

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
			if (!(mp instanceof SecBrDg_x_NhaProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEGhaSecBrDg_x_Nha.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEGhaSecBrDg_x_Nha.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "Ph. Dreyfus";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEGhaSecBrDg_x_Nha.description");
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
		// addConfigProperty (DataExtractor.HECTARE);
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
		// System.out.print ("DEGhaSecBrDg_x_Nha : extraction requested...");

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
				coefHa = 1;
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			// data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				// ~ Collection trees = null; // fc - 24.3.2004
				// ~ try {trees = ((TreeCollection) fil).getTrees ();} catch
				// (Exception e) {} // fc - 24.3.2004

				double Gha = ((GProvider) methodProvider).getG(stand, trees) * 10000 / getSceneArea(step.getScene()); // fc-2.6.2020 // fc
																													// -
																													// 24.3.2004

				double SecBrDg_x_Nha = ((SecBrDg_x_NhaProvider) methodProvider).getSecBrDg_x_Nha(stand, trees); // (SecBrDg_x_Nha
																												// (m�/ha))

				c1.add(new Double(Gha));
				c2.add(new Double(SecBrDg_x_Nha));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEGhaSecBrDg_x_Nha.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DEGhaSecBrDg_x_Nha.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DEGhaSecBrDg_x_Nha.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DEGhaSecBrDg_x_Nha.yLabel"));
		}
		return v;
	}

}
