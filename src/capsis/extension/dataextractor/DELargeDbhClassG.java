/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2024  Francois de Coligny
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

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.TreeDiameterClassProvider;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * BasalArea of trees per large diameter classes, based on DELargeDbhClassG.
 * 
 * @author B. Courbaud, F. de Coligny - June 2001
 */
public class DELargeDbhClassG extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.GraphLabels");
	}

	private Vector labels;

	/**
	 * Constructor.
	 */
	public DELargeDbhClassG() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DELargeDbhClassG(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof TreeDiameterClassProvider))
				return false;

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection))
				return false;

			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty())
				return true; // bare soil problem

		} catch (Exception e) {
			Log.println(Log.ERROR, "DELargeDbhClassG.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DELargeDbhClassG.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud, F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DELargeDbhClassG.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

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
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This extractor computes Numbers of trees per diameter
	 * classes.
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

		// System.out.println ("DELargeDbhClassG : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1d;
			if (isSet("perHectare")) {
				coefHa = 10000d / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			double g1 = 0;
			double g2 = 0;
			double g3 = 0;
			double g4 = 0;
			double g5 = 0;
			double g6 = 0;

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: Pin, Hetre...)

			// Consider restriction to one particular group if needed
			GScene stand = step.getScene();
			Collection aux = doFilter(step.getScene());
			Iterator trees = aux.iterator();

			// Limited in size! (java initializes each value to 0)
//			int tab[] = new int[200];
//			int maxCat = 0;
//			int minCat = 200;

			// Create output data
			while (trees.hasNext()) {
				Tree t = (Tree) trees.next();
				int d = ((TreeDiameterClassProvider) methodProvider).getTreeDiameterClass(t);

				if ((d < 1) || (d > 6)) { // fc - 17.10.2001 - warn and bypass
					Log.println(Log.WARNING, "DELargeDbhClassG.doExtraction ()",
							"diameter class=" + d + " should be in [1, 6]. Nearest limit was taken.");
				}

				double r_m = t.getDbh() / 2d / 100d;
				double treeBasalArea_m2 = Math.PI * r_m * r_m;
				
				if (d <= 1) {
					g1 += treeBasalArea_m2;
				} else if (d == 2) {
					g2 += treeBasalArea_m2;
				} else if (d == 3) {
					g3 += treeBasalArea_m2;
				} else if (d == 4) {
					g4 += treeBasalArea_m2;
				} else if (d == 5) {
					g5 += treeBasalArea_m2;
				} else if (d >= 6) {
					g6 += treeBasalArea_m2;
				}
			}

			c1.add(new Integer(1));
			c1.add(new Integer(2));
			c1.add(new Integer(3));
			c1.add(new Integer(4));
			c1.add(new Integer(5));
			c1.add(new Integer(6));

			c2.add(g1 * coefHa);
			c2.add(g2 * coefHa);
			c2.add(g3 * coefHa);
			c2.add(g4 * coefHa);
			c2.add(g5 * coefHa);
			c2.add(g6 * coefHa);

			l1.add(Translator.swap("DELargeDbhClass.largeDbhClass1"));
			l1.add(Translator.swap("DELargeDbhClass.largeDbhClass2"));
			l1.add(Translator.swap("DELargeDbhClass.largeDbhClass3"));
			l1.add(Translator.swap("DELargeDbhClass.largeDbhClass4"));
			l1.add(Translator.swap("DELargeDbhClass.largeDbhClass5"));
			l1.add(Translator.swap("DELargeDbhClass.largeDbhClass6"));

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DELargeDbhClassG.doExtraction ()", "Exception: ", exc);
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
		v.add(Translator.swap("DELargeDbhClassG.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DELargeDbhClassG.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DELargeDbhClassG.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
