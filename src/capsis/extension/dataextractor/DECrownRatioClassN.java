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

import java.text.NumberFormat;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.TreeCrownRatioProvider;

/**
 * Numbers of trees per crown ratio classes.
 * 
 * @author B. Courbaud - February 2002
 */
public class DECrownRatioClassN extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DECrownRatioClassN");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	private Vector labels;
	protected NumberFormat formater;

	/**
	 * Constructor.
	 */
	public DECrownRatioClassN() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DECrownRatioClassN(GenericExtensionStarter s) {
		super(s);

		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance();
		formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);
		labels = new Vector();

	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof TreeCrownRatioProvider)) {
				return false;
			}

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty()) {
				return true;
			} // bare soil problem

		} catch (Exception e) {
			Log.println(Log.ERROR, "DECrownRatioClassN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DECrownRatioClassN.name");
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
		return Translator.swap("DECrownRatioClassN.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.2";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.2";

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
//		addConfigProperty(PaleoDataExtractor.PERCENTAGE); // fc-29.7.2021
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		addBooleanProperty("percentage", false); // fc-29.7.2021
		
		addDoubleProperty("classWidthInPerCent", 5d);
		addDoubleProperty("minThresholdInPerCent", 0d);
		addBooleanProperty("centerClasses");
		addBooleanProperty("displayClassNames", false);
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step. This extractor computes Numbers of trees per EnergyRatio
	 * class.
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
		// System.out.println ("DECrownRatioClassN : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;

			// fc-29.7.2021 when both set, ha and percentage were unclear
			// Now if ha has priority, else if percentage
//			if (isSet("perHectare") && !isSet("percentage")) 
//			if (isSet("percentage")) {

			if (isSet("perHectare")) 
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			
			// fc-29.7.2021 replaced PERCENTAGE
			else if (isSet("percentage")) {
				methodProvider = step.getProject().getModel().getMethodProvider();

				// fc - 8.4.2004
				TreeCollection tc = (TreeCollection) step.getScene();

				double Ntot = ((NProvider) methodProvider).getN(step.getScene(), tc.getTrees()); 
				
				if (Ntot > 0) 
					coefHa = 100.0 / Ntot;
				
			}
			double minThreshold = getDoubleProperty("minThresholdInPerCent");
			double classWidth = getDoubleProperty("classWidthInPerCent");
			// Security
			if (classWidth < 0d) {
				classWidth = 1d;
			}

			double shift = 0;
			if (isSet("centerClasses")) {
				shift = classWidth / 2;
			}
			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			// Restriction to a group if needed
			Collection aux = doFilter(step.getScene());
			Iterator trees = aux.iterator();

			// Limited in size! (java initializes each value to 0)
			int tab[] = new int[200];
			int maxCat = 0;
			int minCat = 200;

			// Create output data
			while (trees.hasNext()) {
				Tree t = (Tree) trees.next();
				double cr = ((TreeCrownRatioProvider) methodProvider).getTreeCrownRatio(t);
				if (cr == 100)
					cr = 99.9;

				if (cr < minThreshold) {
					continue;
				} // fc - 30.4.2003
				int category = (int) ((cr - shift) / classWidth);

				tab[category] += 1;
				if (category > maxCat) {
					maxCat = category;
				}
				if (category < minCat) {
					minCat = category;
				}
			}

			int anchor = 1;
			for (int i = minCat; i <= maxCat; i++) {
				// ~ c1.add (new Integer (anchor++));
				c1.add(new Integer(i)); // fc - 7.8.2003 - bug correction (from
										// PVallet)
				int numbers = (int) (tab[i] * coefHa);
				c2.add(new Integer(numbers));
				double classBase = shift + i * classWidth;
				if (isSet("displayClassNames")) {
					l1.add("" + formater.format(classBase + classWidth / 2d));
				} else {
					l1.add("" + formater.format(classBase) + "-" + formater.format((classBase + classWidth)));
				}
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DECrownRatioClassN.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector<String> v = new Vector<>();

		// fc-24.6.2021 P. Vallet, xl name not clear with displayClassNames option
		String xl = isSet("displayClassNames") ? Translator.swap("Shared.class")
				: Translator.swap("DECrownRatioClassN.xLabel");
		v.add(xl);

		// fc-29.7.2021 replaced PERCENTAGE
		String yl = Translator.swap("DECrownRatioClassN.yLabel");

		if (isSet("perHectare"))
			yl += " (ha)";

		else if (isSet("percentage"))
			yl += " (%)";
		
		v.add(yl);

//		v.add(Translator.swap("DECrownRatioClassN.xLabel"));
//		if (isSet("perHectare") && !settings.percentage) {
//			v.add(Translator.swap("DECrownRatioClassN.yLabel") + " (ha)");
//		} else if (settings.percentage) {
//			v.add(Translator.swap("DECrownRatioClassN.yLabel") + " (%)");
//		} else {
//			v.add(Translator.swap("DECrownRatioClassN.yLabel"));
//		}

		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
