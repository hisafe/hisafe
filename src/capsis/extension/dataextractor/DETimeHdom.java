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
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.HdomProvider;

/**
 * Dominant and average height versus Date.
 * 
 * @author Ph. Dreyfus - June 2001
 */
public class DETimeHdom extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeHdom");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeHdom() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeHdom(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			setPropertyEnabled("incrementInsteadOfValue", true); // PhD
																	// 2008-06-25
			setPropertyEnabled("incrementPerYear", true); // PhD 2009-04-16

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeHdom.c ()", "Exception during construction", e);
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
			if (!(mp instanceof HdomProvider)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeHdom.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeHdom.name");
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
		return Translator.swap("DETimeHdom.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.2";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.2";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration
		addBooleanProperty("incrementInsteadOfValue"); // PhD 2008-06-25
		addBooleanProperty("incrementPerYear"); // PhD 2009-04-16
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
			Vector c2 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			// modified in order to show increment instead of direct value,
			// depending on "incrementInsteadOfValue" button (to be changed in
			// Configuration (Common)) - PhD 2008-06-25
			double hdom = 0, value, previous = 0;
			double previousdate = 0; // PhD 2009-04-16
			int date;
			Iterator i = steps.iterator();

			if (i.hasNext()) { // if a least one step (... !)
				Step s = (Step) i.next();
				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);
				date = stand.getDate();
				hdom = ((HdomProvider) methodProvider).getHdom(stand, trees);

				if (!i.hasNext()) { // if only 1 step (no second step), the 1st
									// value or a null increment is added, and
									// extraction is finished
					c1.add(new Integer(date));
					if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
						c2.add(new Double(0));
					} else {
						c2.add(new Double(hdom));
					}
				} else { // there is a 2nd step (and possibly more steps)
					if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
						previous = hdom;
						previousdate = date; // PhD 2009-04-16
						// ... what was read is assigned to "previous" - nothing
						// is added at now
					} else { // value is added
						c1.add(new Integer(date));
						c2.add(new Double(hdom));
					}
				}

				while (i.hasNext()) { // ... beginning at the second date, if
										// any
					s = (Step) i.next();
					// Consider restriction to one particular group if needed
					stand = s.getScene();
					trees = doFilter(stand);
					date = stand.getDate();
					hdom = ((HdomProvider) methodProvider).getHdom(stand, trees);
					c1.add(new Integer(date));
					if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
						if (isSet("incrementPerYear")) {
							c2.add(new Double((hdom - previous) / Math.max(1, (date - previousdate))));
						} // PhD 2009-04-16
						else {
							c2.add(new Double(hdom - previous));
						}
						previous = hdom;
						previousdate = date; // PhD 2009-04-16
					} else {
						c2.add(new Double(hdom));
					}
				}
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(new Vector()); // no x labels
			Vector y1Labels = new Vector();
			// y1Labels.add ("Hdom");
			if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
				if (isSet("incrementPerYear")) { // PhD 2009-04-16
					y1Labels.add("1 d Hdom");
				} else {
					y1Labels.add("d Hdom");
				}
			} else {
				y1Labels.add("Hdom");
			}
			labels.add(y1Labels);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeHdom.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeHdom.xLabel"));
		// v.add (Translator.swap ("DETimeHdom.yLabel"));
		if (isSet("incrementInsteadOfValue")) { // PhD 2008-06-25
			if (isSet("incrementPerYear")) { // PhD 2009-04-16
				v.add("1 d " + Translator.swap("DETimeHdom.yLabel"));
			} else {
				v.add("d " + Translator.swap("DETimeHdom.yLabel"));
			}
		} else {
			v.add(Translator.swap("DETimeHdom.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return 1;
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
