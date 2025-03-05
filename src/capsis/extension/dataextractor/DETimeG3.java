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
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.DdomProvider;

/**
 * Basal area ("Grundfl�che" (G), "surface terri�re" en allemand) over
 * Time(/age) or over Cdom. With possibilty to view annual increment
 * 
 * @author Ph. Dreyfus - January 2009, updated C. Meredieu and T. Labbé - April
 *         2015
 */
public class DETimeG3 extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeG3");
	}

	protected Vector curves;
	protected MethodProvider methodProvider;

	private boolean availableDdom;

	/**
	 * Constructor.
	 */
	public DETimeG3() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeG3(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();

			checkMethodProvider();

			setPropertyEnabled("incrementInsteadOfValue", true); // PhD
																	// 2008-06-25
			setPropertyEnabled("incrementPerYear", true); // PhD 2009-04-16
			setPropertyEnabled("xIsCdom", availableDdom); // cm tl 14 04 2015
															// Ddom provider is
															// available
		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeG3.c ()", "Exception during construction", e);
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
			if (!(mp instanceof GProvider)) {
				return false;
			}
			if (!(mp instanceof DdomProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeG3.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface. From Extension interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeG3.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "C. Meredieu and T. Labbe";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeG3.description");
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

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof DdomProvider) {
			availableDdom = true;
		}

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
		addBooleanProperty("xIsCdom"); // PhD 2008-12-19
		addBooleanProperty("incrementInsteadOfValue"); // PhD 2008-06-25
		addBooleanProperty("incrementPerYear"); // PhD 2009-04-16
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
			Vector c2 = new Vector(); // y coordinates

			// Data extraction : points with (Double, Double) coordinates
			// modified in order to show increment instead of direct value,
			// depending on "incrementInsteadOfValue" button (to be changed in
			// Configuration (Common)) - PhD 2008-06-25
			double read = 0, value, previous = 0;
			double previousdate = 0; // PhD 2009-04-16
			int date;
			double cdom;
			Iterator i = steps.iterator();

			if (i.hasNext()) { // if a least one step (... !)
				Step s = (Step) i.next();
				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);
				date = stand.getDate();
				cdom = (((DdomProvider) methodProvider).getDdom(stand, trees)) * Math.PI;
				read = ((GProvider) methodProvider).getG(stand, trees) * coefHa; // fc
																					// -
																					// 24.3.2004

				if (!i.hasNext()) { // if only 1 step (no second step), the 1st
									// value or a null increment is added, and
									// extraction is finished
					// c1.add (new Integer (date));
					if (isSet("xIsCdom")) { // cm tl 14 04 2015
						c1.add(new Double(cdom));
					} else {
						c1.add(new Double(date));
					}
					if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
						c2.add(new Double(0));
					} else {
						c2.add(new Double(read));
					}
				} else { // there is a 2nd step (and possibly more steps)
					if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
						previous = read;
						previousdate = date; // PhD 2009-04-16
						// ... what was read is assigned to "previous" - nothing
						// is added at now
					} else { // value is added
						if (isSet("xIsCdom")) { // cm tl 14 04 2015
							c1.add(new Double(cdom));
						} else {
							c1.add(new Double(date));
						}
						c2.add(new Double(read));
					}
				}

				while (i.hasNext()) { // ... beginning at the second date, if
										// any
					s = (Step) i.next();
					// Consider restriction to one particular group if needed
					stand = s.getScene();
					trees = doFilter(stand);
					date = stand.getDate();
					cdom = (((DdomProvider) methodProvider).getDdom(stand, trees)) * Math.PI;
					read = ((GProvider) methodProvider).getG(stand, trees) * coefHa; // fc
																						// -
																						// 24.3.2004
					if (isSet("xIsCdom")) { // cm tl 14 04 2015
						c1.add(new Double(cdom));
					} else {
						c1.add(new Double(date));
					}
					if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
						if (isSet("incrementPerYear")) {
							c2.add(new Double((read - previous) / Math.max(1, (date - previousdate))));
						} // PhD 2009-04-16
						else {
							c2.add(new Double(read - previous));
						}
						previous = read;
						previousdate = date; // PhD 2009-04-16
					} else {
						c2.add(new Double(read));
					}

				}
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeG3.doExtraction ()", "Exception caught : ", exc);
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
		return null; // optional : unused
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		if (isSet("xIsCdom")) { // cm tl 14 04 2015
			v.add(Translator.swap("DETimeG3.xLabelCdom"));
		} else {
			v.add(Translator.swap("DETimeG3.xLabelDate"));
		}
		/*
		 * if (isSet("perHectare")) { v.add (Translator.swap
		 * ("DETimeG3.yLabel")+" (/ha)"); } else { v.add (Translator.swap
		 * ("DETimeG3.yLabel")); }
		 */
		String yLab;
		yLab = Translator.swap("DETimeG3.yLabel");
		if (isSet("perHectare")) {
			yLab = yLab + " (/ha)";
		}
		if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
			if (isSet("incrementPerYear")) { // PhD 2009-04-16
				yLab = "1 d " + yLab;
			} else {
				yLab = "d " + yLab;
			}
		}
		v.add(yLab);
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 1;
	}

}
