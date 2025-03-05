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
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;

/**
 * Numbers of trees per diameter classes.
 * 
 * @author F. de Coligny - November 2000
 */
public class DEDbhClassN extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEDbhClassN");
	}

	public static final int MAX_FRACTION_DIGITS = 2;

	private Vector labels;
	protected NumberFormat formater;

	// fc+bc-12.1.2017 pb in DETimeDbhCumulativePerSpecies, based on
	// several instances of this extractor, restricted to various
	// species groups
	private boolean labelsFromZero;

	/**
	 * Constructor.
	 */
	public DEDbhClassN() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEDbhClassN(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance();
		formater.setMaximumFractionDigits(MAX_FRACTION_DIGITS);

	}

	public void setLabelsFromZero(boolean labelsFromZero) {
		this.labelsFromZero = labelsFromZero;
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
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			TreeCollection tc = (TreeCollection) s;
			
			if (tc.getTrees().isEmpty()) // fa-09.07.2021: to allow for scene without any tree
				return true; // fa-09.07.2021: if no tree, return true // fc-11.10.2021 Checked
							
			Tree t = tc.getTrees().iterator().next();
			if (t instanceof Numberable) {
				return true;
			} // fc - 9.4.2003 - compatible for Numberable (GMaidTree)

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEDbhClassN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEDbhClassN.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEDbhClassN.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.3";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.3";

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
		addDoubleProperty("classWidthInCm", 5d);
		addDoubleProperty("minThresholdInCm", 0d);
		addBooleanProperty("centerClasses");
		addBooleanProperty("displayClassNames", false);
		addBooleanProperty("roundN"); // fc - 22.8.2006 - n may be double (NZ1)
										// -> round becomes an option

		// fc-28.10.2021 For tests
//		setPropertyDeclarationOrderKept();
		
		// TEMPORARILY DESACTIVATED TEMPORARILY DESACTIVATED TEMPORARILY
		// DESACTIVATED
		// TEMPORARILY DESACTIVATED TEMPORARILY DESACTIVATED TEMPORARILY
		// DESACTIVATED
		// TEMPORARILY DESACTIVATED TEMPORARILY DESACTIVATED TEMPORARILY
		// DESACTIVATED
		// ~ addConfigProperty (DataExtractor.STATUS); // fc - 22.3.2004
		// TEMPORARILY DESACTIVATED TEMPORARILY DESACTIVATED TEMPORARILY
		// DESACTIVATED
		// TEMPORARILY DESACTIVATED TEMPORARILY DESACTIVATED TEMPORARILY
		// DESACTIVATED
		// TEMPORARILY DESACTIVATED TEMPORARILY DESACTIVATED TEMPORARILY
		// DESACTIVATED
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

		// System.out.println ("DEDbhClassN : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

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

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			// Restriction to a group if needed
			Collection aux = doFilter(step.getScene());
			Iterator trees = aux.iterator();

			// Consider optional group and status
			// ~ Filtrable fil = doFilter ((Filtrable) step.getStand ());
			// ~ Collection gr = ((TreeCollection) fil).getTrees ();
			// ~ System.out.println ("group: "+Tools.toString (gr));
			// ~ Collection st = ((GTCStand) step.getStand ()).getTrees
			// (getStatusProperty ());
			// ~ System.out.println ("status: "+Tools.toString (st));
			// ~ Collection intersection = new ArrayList (gr);
			// ~ intersection.retainAll (st);
			// ~ System.out.println ("intersection: "+Tools.toString
			// (intersection));

			// ~ Iterator trees = intersection.iterator ();

			// Limited in size! (java initializes each value to 0)
			double tab[] = new double[200]; // fc - 22.8.2006
			int maxCat = 0;
			int minCat = 200;

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

				if (t instanceof Numberable) {
					double number = ((Numberable) t).getNumber(); // fc -
																	// 22.8.2006
																	// -
																	// Numberable
																	// returns
																	// double
					tab[category] += number; // ex: GMaidTree : the tree
												// represents several
				} else {
					tab[category] += 1; // ex : GMaddTree : one individual

				}

				if (category > maxCat) {
					maxCat = category;
				}
				if (category < minCat) {
					minCat = category;
				}
			}
			// ~ System.out.println
			// ("DEDbhClassN : minCat="+minCat+" maxCat="+maxCat);

			// fc+bc-12.1.2017 pb in DETimeDbhCumulativePerSpecies, based on
			// several instances of this extractor, restricted to various
			// species groups
			if (labelsFromZero)
				minCat = 0;

			int anchor = 1;
			for (int i = minCat; i <= maxCat; i++) {
				// ~ c1.add (new Integer (anchor++));
				c1.add(new Integer(i)); // fc - 7.8.2003 - bug correction (from
										// PVallet)

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
			Log.println(Log.ERROR, "DEDbhClassN.doExtraction ()", "Exception caught : ", exc);
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
				: Translator.swap("DEDbhClassN.xLabel");
		v.add(xl);

		String yl = Translator.swap("DEDbhClassN.yLabel");
		if (isSet("perHectare"))
			yl += " (ha)";
		v.add(yl);

//		v.add(Translator.swap("DEDbhClassN.xLabel"));
//		if (isSet("perHectare")) {
//			v.add(Translator.swap("DEDbhClassN.yLabel") + " (ha)");
//		} else {
//			v.add(Translator.swap("DEDbhClassN.yLabel"));
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
