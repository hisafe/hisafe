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
import capsis.util.methodprovider.Br2VmoyAmapsimProvider;
import capsis.util.methodprovider.Br3VmoyAmapsimProvider;
import capsis.util.methodprovider.BrVmoyAmapsimProvider;
import capsis.util.methodprovider.BrnVmoyAmapsimProvider;
import capsis.util.methodprovider.VmoyAmapsimProvider;

/**
 * Amapsim Volumes versus Date
 * 
 * @author L. Saint-André and Y. Caraglio - March 2004
 */
public class DETimeAmapsimVol extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeAmapsimVol");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeAmapsimVol() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeAmapsimVol(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAmapsimVol.c ()", "Exception during construction : ", e);
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

			if (!(mp instanceof VmoyAmapsimProvider)) {
				return false;
			}
			if (!(mp instanceof BrVmoyAmapsimProvider)) {
				return false;
			}
			if (!(mp instanceof Br2VmoyAmapsimProvider)) {
				return false;
			}
			if (!(mp instanceof Br3VmoyAmapsimProvider)) {
				return false;
			}
			if (!(mp instanceof BrnVmoyAmapsimProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeAmapsimVol.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeAmapsimVol.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "L. Saint-André and Y. Caraglio";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeAmapsimVol.description");
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
			Vector c2 = new Vector(); // y coordinates Trunk
			Vector c3 = new Vector(); // y coordinates Br
			Vector c4 = new Vector(); // y coordinates Br2
			Vector c5 = new Vector(); // y coordinates Br3
			Vector c6 = new Vector(); // y coordinates Brn
			Vector c7 = new Vector(); // y coordinates Tot

			// Data extraction : points with (Integer, Double) coordinates
			Iterator i = steps.iterator();
			do {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				int date = stand.getDate();

				double trunkVol = ((VmoyAmapsimProvider) methodProvider).getVmoyAmapsim(stand, trees);
				double brVol = ((BrVmoyAmapsimProvider) methodProvider).getBrVmoyAmapsim(stand, trees);
				double br2Vol = ((Br2VmoyAmapsimProvider) methodProvider).getBr2VmoyAmapsim(stand, trees);
				double br3Vol = ((Br3VmoyAmapsimProvider) methodProvider).getBr3VmoyAmapsim(stand, trees);
				double brnVol = ((BrnVmoyAmapsimProvider) methodProvider).getBrnVmoyAmapsim(stand, trees);
				double totVol = trunkVol + brVol;

				Log.println("trunkVol = " + trunkVol);
				Log.println("brVol = " + brVol);
				Log.println("br2Vol = " + br2Vol);
				Log.println("br3Vol = " + br3Vol);
				Log.println("brnVol = " + brnVol);
				Log.println("totVol = " + totVol);

				c1.add(new Integer(date));
				c2.add(new Double(trunkVol));
				c3.add(new Double(brVol));
				c4.add(new Double(br2Vol));
				c5.add(new Double(br3Vol));
				c6.add(new Double(brnVol));
				c7.add(new Double(totVol));

			} while (i.hasNext());

			curves.clear();
			curves.add(c1);
			curves.add(c2);
			curves.add(c3);
			curves.add(c4);
			curves.add(c5);
			curves.add(c6);
			curves.add(c7);

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			y1Labels.add("Trunk");
			labels.add(y1Labels);
			Vector y2Labels = new Vector();
			y2Labels.add("Br Tot");
			labels.add(y2Labels);
			Vector y3Labels = new Vector();
			y3Labels.add("BrOrder 2");
			labels.add(y3Labels);
			Vector y4Labels = new Vector();
			y4Labels.add("BrOrder 3");
			labels.add(y4Labels);
			Vector y5Labels = new Vector();
			y5Labels.add("BrOrder n");
			labels.add(y5Labels);
			Vector y6Labels = new Vector();
			y6Labels.add("Vol Tot");
			labels.add(y6Labels);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeAmapsimVol.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeAmapsimVol.xLabel"));
		v.add(Translator.swap("DETimeAmapsimVol.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 6;
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
