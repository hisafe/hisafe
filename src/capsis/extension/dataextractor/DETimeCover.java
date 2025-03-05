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
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.CoverProvider;

/**
 * Evolution of Cover along time.
 * 
 * @author B.Courbaud - December 2002
 */
public class DETimeCover extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCover");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeCover() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCover(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCover.c ()", "Exception during construction", e);
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
			if (!(mp instanceof CoverProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCover.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCover.name");
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
		return Translator.swap("DETimeCover.description");
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

		addBooleanProperty("percentage", false); // fc-23.8.2021
//		addConfigProperty(PaleoDataExtractor.PERCENTAGE);
		
		// addConfigProperty (DataExtractor.CELL_GROUP);

		// fc-13.10.2010: added ',true' in the 4 lines below (default = yes)
		addBooleanProperty("showL1", true);
		addBooleanProperty("showL12", true);
		addBooleanProperty("showL123", true);
		addBooleanProperty("showL1234", true);
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
		// System.out.print ("DETimeCover : extraction requested...");

		if (upToDate) {
			// System.out.println (" upToDate -> NO EXTRACTION");
			return true;
		}
		if (step == null) {
			// System.out.println (" null Step -> NO EXTRACTION");
			return false;
		}

		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// per Ha computation
			double coefHa = 1;
			double coefPercent = 1;
			
			
			// fc-23.8.2021 check perHa, then percentage
			if (isSet("perHectare")) {
//			if (!settings.percentage && isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			} else if (isSet("percentage")) {
				coefPercent = 100 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y coordinates
			Vector c4 = new Vector(); // y coordinates
			Vector c5 = new Vector(); // y coordinates

			// data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				// ~ Filtrable fil = doFilter (stand); // fc - 19.4.2004

				int date = stand.getDate();

				methodProvider = step.getProject().getModel().getMethodProvider();
				Vector coverStatus = ((CoverProvider) methodProvider).getCover(stand);

				c1.add(new Integer(date));
				if (isSet("showL1")) {
					double layer1Cover = ((Double) coverStatus.get(4)).doubleValue();
					c2.add(new Double(layer1Cover * coefHa * coefPercent));
				}
				if (isSet("showL12")) {
					double layer12Cover = ((Double) coverStatus.get(5)).doubleValue();
					c3.add(new Double(layer12Cover * coefHa * coefPercent));
				}
				if (isSet("showL123")) {
					double layer123Cover = ((Double) coverStatus.get(6)).doubleValue();
					c4.add(new Double(layer123Cover * coefHa * coefPercent));
				}
				if (isSet("showL1234")) {
					double totalCover = ((Double) coverStatus.get(0)).doubleValue();
					c5.add(new Double(totalCover * coefHa * coefPercent));
				}
			}

			curves.clear();
			curves.add(c1);
			if (isSet("showL1")) {
				curves.add(c2);
			}
			if (isSet("showL12")) {
				curves.add(c3);
			}
			if (isSet("showL123")) {
				curves.add(c4);
			}
			if (isSet("showL1234")) {
				curves.add(c5);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showL1")) {
				Vector y1Labels = new Vector();
				y1Labels.add("L1");
				labels.add(y1Labels);
			}
			if (isSet("showL12")) {
				Vector y2Labels = new Vector();
				y2Labels.add("L12");
				labels.add(y2Labels);
			}
			if (isSet("showL123")) {
				Vector y3Labels = new Vector();
				y3Labels.add("L123");
				labels.add(y3Labels);
			}
			if (isSet("showL1234")) {
				Vector y4Labels = new Vector();
				y4Labels.add("L1234");
				labels.add(y4Labels);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCover.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeCover.xLabel"));
		
		// fc-23.8.2021 check perHa, then percentage
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeCover.yLabel") + " (m2/ha)");
		} else if (isSet("percentage")) {
//		} else if (settings.percentage) {
			v.add(Translator.swap("DETimeCover.yLabel") + " (% " + Translator.swap("DELayersCover.surface") + ")");
		} else {
			v.add(Translator.swap("DETimeCover.yLabel") + " (m2)");
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
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
