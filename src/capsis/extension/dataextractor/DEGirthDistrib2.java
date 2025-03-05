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
import capsis.defaulttype.NumberableTree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.OverSamplingValueProvider;

/**
 * Numbers of trees per girth class, with cut/dead trees also.
 * 
 * @author Ph. Dreyfus - June 2001
 */
public class DEGirthDistrib2 extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEGirthDistrib2");
	}

	private Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DEGirthDistrib2() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEGirthDistrib2(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

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
			if (!(mp instanceof OverSamplingValueProvider)) {
				return false;
			}

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			if (!(tc.getTrees().iterator().next() instanceof NumberableTree)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEGirthDistrib2.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEGirthDistrib2.name");
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
		return Translator.swap("DEGirthDistrib2.description");
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
		addConfigProperty(PaleoDataExtractor.STATUS); // fc - 22.4.2004
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		
//		addConfigProperty(PaleoDataExtractor.CLASS_WIDTH);
		addIntProperty ("classWidthInCm", 1); // fc-26.8.2021
		
		addBooleanProperty("roundN"); // fc - 22.8.2006 - n may be double (NZ1)
										// -> round becomes an option
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

		// System.out.println ("DEGirthDistrib2 : extraction being made");

		try {
			// per Ha computation
			GModel model = (GModel) step.getProject().getModel();
			int OSV = ((OverSamplingValueProvider) methodProvider).getOverSamplingValue(model);

			double coefHa = 1d / (double) (OSV);
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

//			if (settings.classWidth < 1) {
//				settings.classWidth = 1;
//			}
//			int classWidth = settings.classWidth;
						
			// fc-26.8.2021 Removed CLASS_WIDTH
			int classWidth = getIntProperty("classWidthInCm");
			// Security
			if (classWidth < 1)
				classWidth = 1;		
			
			double cw = (double) classWidth;

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...
										// or 10, 15, 20 ...)

			// Restriction to a group if needed
			Collection aux = doFilter(step.getScene());
			Iterator trees = aux.iterator();

			// Limited in size! (java initializes each value to 0)
			double tab[] = new double[200]; // fc - 22.8.2006
			int tabb[] = new int[200];
			int maxCat = 0;
			int minCat = 200;

			// Create output data
			while (trees.hasNext()) {
				NumberableTree t = (NumberableTree) trees.next();
				// bug correction : some maid trees may have number == 0 ->
				// ignore them - tl 12/09/2005
				if ((t.getNumber() == 0) && (t.getNumberOfDead() == 0))
					continue; // next iteration

				double c = Math.PI * t.getDbh(); // girth : cm
				// int category = (int) (c / classWidth);
				int category = (int) ((c + 0.5 * cw) / cw);

				tab[category] += t.getNumber();
				tabb[category] += t.getNumber() + t.getNumberOfDead(); // Evolution
																		// :
																		// diff.
																		// =
																		// mortalit�
																		// depuis
																		// 1 an
																		// ?
																		// depuis
																		// le
																		// step
																		// visible
																		// pr�c�dent
																		// ? - A
																		// VERIFIER
																		// !!!
				if (category > maxCat) {
					maxCat = category;
				}
				if (category < minCat) {
					minCat = category;
				}
			}

			for (int i = minCat; i <= maxCat; i++) {
				int classBase = i * classWidth;
				int a = classBase + classWidth / 2;
				c1.add(new Integer(a));

				// fc - 22.8.2006 - Numberable is now double
				// New option: if (roundN), N is rounded to the nearest int
				double numbers = 0;
				if (isSet("roundN")) {
					numbers = (int) (tab[i] * coefHa + 0.5); // fc - 29.9.2004 :
																// +0.5 (sp)
				} else {
					numbers = tab[i] * coefHa; // fc - 22.8.2006 - Numberable is
												// now double
				}
				c2.add(new Double(numbers));
				// fc - 22.8.2006 - Numberable is now double

				int numbersdiff = (int) (tabb[i] * coefHa + 0.5); // fc -
																	// 29.9.2004
																	// : +0.5
																	// (sp)
				c3.add(new Double(numbersdiff)); // fc - 22.8.2006

				// l1.add (""+classBase+"-"+(classBase+classWidth));
				l1.add("" + classBase);
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);
			curves.add(c3);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEGirthDistrib2.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DEGirthDistrib2.xLabel"));
		if (isSet("perHectare")) {
			v.add(Translator.swap("DEGirthDistrib2.yLabel") + " (ha)");
		} else {
			v.add(Translator.swap("DEGirthDistrib2.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 2;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
