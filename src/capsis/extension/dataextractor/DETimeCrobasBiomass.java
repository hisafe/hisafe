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
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.CrobasBiomassProvider;

/**
 * Crobas biomass: Wf (foliage), Wb (branch), Ws (sapwood), Wr (fine roots), Wt
 * (transport roots).
 * 
 * @author R. Schneider, F. de Coligny - May 2008
 */
public class DETimeCrobasBiomass extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCrobasBiomass");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeCrobasBiomass() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCrobasBiomass(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCrobasBiomass.c ()", "Exception during construction", e);
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
			if (!(mp instanceof CrobasBiomassProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCrobasBiomass.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeCrobasBiomass.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "R. Schneider";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeCrobasBiomass.description");
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

	// nb-14.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(PaleoDataExtractor.TREE_GROUP); // group
															// multiconfiguration
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		addBooleanProperty("showWf", true);
		addBooleanProperty("showWb");
		addBooleanProperty("showWs");
		addBooleanProperty("showWr");
		addBooleanProperty("showWt");
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
			Vector c2 = new Vector(); // y coordinates: Wf (foliage)
			Vector c3 = new Vector(); // y coordinates: Wb (branch)
			Vector c4 = new Vector(); // y coordinates: Ws (sapwood)
			Vector c5 = new Vector(); // y coordinates: Wr (fine roots)
			Vector c6 = new Vector(); // y coordinates: Wt (transport roots)

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand); // fc - 5.4.2004

				int date = stand.getDate();
				double Wf = ((CrobasBiomassProvider) methodProvider).getWf(trees) * coefHa;
				double Wb = ((CrobasBiomassProvider) methodProvider).getWb(trees) * coefHa;
				double Ws = ((CrobasBiomassProvider) methodProvider).getWs(trees) * coefHa;
				double Wr = ((CrobasBiomassProvider) methodProvider).getWr(trees) * coefHa;
				double Wt = ((CrobasBiomassProvider) methodProvider).getWt(trees) * coefHa;

				c1.add(new Integer(date));
				c2.add(new Double(Wf));
				c3.add(new Double(Wb));
				c4.add(new Double(Ws));
				c5.add(new Double(Wr));
				c6.add(new Double(Wt));
			}

			curves.clear();
			curves.add(c1);
			if (isSet("showWf")) {
				curves.add(c2);
			}
			if (isSet("showWb")) {
				curves.add(c3);
			}
			if (isSet("showWs")) {
				curves.add(c4);
			}
			if (isSet("showWr")) {
				curves.add(c5);
			}
			if (isSet("showWt")) {
				curves.add(c6);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showWf")) {
				Vector l1 = new Vector();
				l1.add("Wf");
				labels.add(l1);
			}
			if (isSet("showWb")) {
				Vector l2 = new Vector();
				l2.add("Wb");
				labels.add(l2);
			}
			if (isSet("showWs")) {
				Vector l3 = new Vector();
				l3.add("Ws");
				labels.add(l3);
			}
			if (isSet("showWr")) {
				Vector l4 = new Vector();
				l4.add("Wr");
				labels.add(l4);
			}
			if (isSet("showWt")) {
				Vector l5 = new Vector();
				l5.add("Wt");
				labels.add(l5);
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCrobasBiomass.doExtraction ()", "Exception caught : ", exc);
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
		return labels; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeCrobasBiomass.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeCrobasBiomass.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DETimeCrobasBiomass.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
