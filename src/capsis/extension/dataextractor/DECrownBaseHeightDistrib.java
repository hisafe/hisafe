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
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.TreeCrownBaseHeightProvider;

/**
 * Crown base height per diameter classes.
 * 
 * @author C. Meredieu, T. Labbé - September 2003
 */
public class DECrownBaseHeightDistrib extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DECrownBaseHeightDistrib");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	private Vector labels;
	protected NumberFormat formater;

	/**
	 * Constructor.
	 */
	public DECrownBaseHeightDistrib() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DECrownBaseHeightDistrib(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance();
		formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);

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
			if (!(mp instanceof TreeCrownBaseHeightProvider)) {
				return false;
			}

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) {
				return true;
			} // fc - 9.4.2003 - compatible for Numberable (GMaidTree)

		} catch (Exception e) {
			Log.println(Log.ERROR, "DECrownBaseHeightDistrib.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DECrownBaseHeightDistrib.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "C. Meredieu, T. Labbé";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DECrownBaseHeightDistrib.description");
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
		// addConfigProperty (DataExtractor.HECTARE);
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		addDoubleProperty("classWidthInCm", 5d);
		addDoubleProperty("minThresholdInCm", 0d);
		addBooleanProperty("centerClasses");

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

		// System.out.println
		// ("DECrownBaseHeightDistrib : extraction being made");

		// Retrieve method provider
		// methodProvider = MethodProviderFactory.getMethodProvider
		// (step.getScenario ().getModel ());
		methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			/*
			 * // per Ha computation double coefHa = 1; if (isSet("perHectare")) {
			 * coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020 }
			 */

			double minThreshold = getDoubleProperty("minThresholdInCm");
			double classWidth = getDoubleProperty("classWidthInCm");

			// Security
			if (classWidth <= 0d) {
				classWidth = 1d;
			}

			double shift = 0;
			if (isSet("centerClasses")) {
				shift = classWidth / 2;
			}

			Vector c0 = new Vector(); // x coordinates
			Vector c1 = new Vector(); // y1 coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			// Restriction to a group if needed
			Collection aux = doFilter(step.getScene());
			Iterator trees = aux.iterator();

			// Limited in size! (java initializes each value to 0)

			double tab1[] = new double[200];
			int maxCat = 0;
			int minCat = 200;
			int tabn[] = new int[200];
			int tabnn[] = new int[200];

			// Create output data
			while (trees.hasNext()) {
				Tree t = (Tree) trees.next();
				// bug correction : some maid trees may have number == 0 ->
				// ignore them - tl 12/09/2005
				if ((t instanceof Numberable) && (((Numberable) t).getNumber() == 0))
					continue; // next iteration

				double d = t.getDbh(); // dbh : cm

				if (d < minThreshold) {
					continue;
				} // fc - 9.4.2003

				int category = (int) ((d - shift) / classWidth);

				Log.println("category " + category);

				double crownBaseHeight = (double) (((TreeCrownBaseHeightProvider) methodProvider)
						.getTreeCrownBaseHeight(t));

				Log.println("category " + category + " crownBaseHeight " + crownBaseHeight);

				// TL 27/08/2014 tab1 [category] += crownBaseHeight;

				// TL 27/08/2014 Log.println("category "+category+" tab1 "+ tab1
				// [category]);

				if (t instanceof Numberable) {
					double number = ((Numberable) t).getNumber(); // fc -
																	// 22.8.2006
																	// -
																	// Numberable
																	// returns
																	// double
					tabn[category] += number; // ex: GMaidTree : the tree
												// represents several
					// TL 27/08/2014
					tab1[category] += (crownBaseHeight * number);
				} else {
					tabn[category] += 1; // ex : GMaddTree : one individual
					// TL 27/08/2014
					tab1[category] += crownBaseHeight;
				}
				tabnn[category] += 1;

				if (category > maxCat) {
					maxCat = category;
				}
				if (category < minCat) {
					minCat = category;
				}
			}
			// ~ System.out.println
			// ("DECrownBaseHeightDistrib : minCat="+minCat+" maxCat="+maxCat);

			int anchor = 1;
			for (int i = minCat; i <= maxCat; i++) {
				// ~ c1.add (new Integer (anchor++));
				c0.add(new Integer(i)); // fc - 7.8.2003 - bug correction (from
										// PVallet)
				// TL 27/08/2014
				// double numbers1 = tab1[i] / (double) tabnn [i];
				double numbers1 = tab1[i] / (double) tabn[i];
				c1.add(new Double(numbers1));

				Log.println("i " + i + " numbers1 " + numbers1 + " tab1 " + tab1[i] + " tabnn " + tabnn[i]);

				double classBase = shift + i * classWidth;
				l1.add("" + formater.format(classBase) + "-" + formater.format((classBase + classWidth)));
			}

			curves.clear();
			curves.add(c0);
			curves.add(c1);

			labels.clear();
			labels.add(l1);
			// Vector y1Labels = new Vector ();
			// y1Labels.add (Translator.swap ("DEBarkBiomassDistrib.y1Label"));
			// labels.add (y1Labels);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DECrownBaseHeightDistrib.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DECrownBaseHeightDistrib.xLabel"));
		v.add(Translator.swap("DECrownBaseHeightDistrib.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
