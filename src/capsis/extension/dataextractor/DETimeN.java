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

import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.NProvider;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Number of trees versus Date. Since 22.8.2006, Number of tree is double
 * instead of int.
 * 
 * @author F. de Coligny - November 2000, Ph. Dreyfus - January 2010: over
 *         Time(/age) or over Hdom
 */
public class DETimeN extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeN");
	}

	private boolean availableHdom;

	/**
	 * Constructor.
	 */
	public DETimeN() {
	}

	/**
	 * Cnstructor 2, uses the standard Extension starter.
	 */
	public DETimeN(GenericExtensionStarter s) throws Exception {
		super(s);

//		Settings.setProperty("samare.test", true); // tmp

		try {
			curves = new Vector();
			checkMethodProvider();
			setPropertyEnabled("xIsHdom", availableHdom); // PhD 2010-01-25
			setPropertyEnabled("Log(N)", false); // PhD 2014-11-25

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeN.c ()", "Exception during construction", e);
		}
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
			if (!(mp instanceof NProvider))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeN.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeN.name");
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
		return Translator.swap("DETimeN.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.4";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.4";

	// Evaluate MethodProvider's capabilities to propose more or less options
	private void checkMethodProvider() { // PhD 2010-01-25
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		if (methodProvider instanceof HdomProvider) {
			availableHdom = true;
		}

	}

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
															// configuration

		// tl 29.1.2007 from DEGirthClassN
		addBooleanProperty("roundN"); // fc - 22.8.2006 - n may be double (NZ1)
										// -> round becomes an option
		addBooleanProperty("xIsHdom"); // PhD 2010-01-25
		addBooleanProperty("Log(N)"); // PhD 2014-11-25

		// fc-14.9.2021 Removed the ordered properties, rarely used and too complex

//		// test Samare for Sylvain Turbis - fc - 16.10.2006
//		if (Settings.getProperty("samare.test", false)) {
//			try {
//				addBooleanProperty("o-1-1-free11", true);
//				addBooleanProperty("o-1-2-free2", true);
//				addBooleanProperty("o-2-1-free3", true);
//
//				addBooleanProperty("o-block1-1-1-text1", true);
//
//				GrouperManager gm = GrouperManager.getInstance();
//				Collection trees = ((TreeCollection) (step.getScene())).getTrees();
//				Collection grouperNames = gm.getGrouperNames(trees);
//				LinkedList c1 = new LinkedList(grouperNames);
//				addComboProperty("o-block1-1-2-text2", c1);
//
//				LinkedList c2 = new LinkedList();
//				c2.add("crit1");
//				c2.add("crit2");
//				c2.add("crit3");
//				addComboProperty("o-block1-2-1-text3", c2);
//
//				LinkedList c3 = new LinkedList();
//				c3.add("<");
//				c3.add("<=");
//				c3.add("=");
//				c3.add(">");
//				c3.add(">=");
//				addComboProperty("o-block1-2-2-text4", c3);
//				addDoubleProperty("o-block1-2-3-text5", 2);
//
//				addBooleanProperty("o-3-1-free10", true);
//				addBooleanProperty("o-3-2-free11", true);
//				addBooleanProperty("o-4-1-free12", true);
//
//				addBooleanProperty("o-block2-1-1-text1", true);
//
//				// ~ GrouperManager gm = GrouperManager.getInstance ();
//				// ~ Collection trees = ((TreeCollection) (step.getStand
//				// ())).getTrees ();
//				// ~ Collection grouperNames = gm.getGrouperNames (trees);
//				// ~ LinkedList c1 = new LinkedList (grouperNames);
//				addComboProperty("o-block2-1-2-text2", c1);
//
//				// ~ LinkedList c2 = new LinkedList ();
//				// ~ c2.add ("crit1");
//				// ~ c2.add ("crit2");
//				// ~ c2.add ("crit3");
//				addComboProperty("o-block2-2-1-text3", c2);
//
//				// ~ LinkedList c3 = new LinkedList ();
//				// ~ c3.add ("<");
//				// ~ c3.add ("<=");
//				// ~ c3.add ("=");
//				// ~ c3.add (">");
//				// ~ c3.add (">=");
//				addComboProperty("o-block2-2-2-text4", c3);
//				addDoubleProperty("o-block2-2-3-text5", 2);
//
//			} catch (Exception e) {
//			}
//		}
//		// test Samare for Sylvain Turbis - fc - 16.10.2006
	}

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step.
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

			// fc-14.9.2021 Removed the ordered properties, rarely used and too complex

//			// Test for Samare - fc - 16.10.2006
//			if (Settings.getProperty("samare.test", false)) {
//				System.out.println("o-1-1-text1=" + isSet("o-1-1-text1"));
//				System.out.println("o-1-2-text2=" + getComboProperty("o-1-2-text2"));
//				System.out.println("o-2-1-text3=" + getComboProperty("o-2-1-text3"));
//				System.out.println("o-2-2-text4=" + getComboProperty("o-2-2-text4"));
//				System.out.println("o-2-3-text5=" + getDoubleProperty("o-2-3-text5"));
//			}
//			// Test for Samare

			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates

			double hdom = 0; // PhD 2014-11-25
			double previousHdom = 0; // PhD 2014-11-25

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				Collection trees = doFilter(stand);

				// Testing statusMap - fc-29.9.2014 - CM & TL wish to detect
				// "dead" trees
				if (stand instanceof TreeList) {
					// TreeList treeList = (TreeList) stand;
					Collection cutTrees = ((TreeList) stand).getTrees("cut");
					if (cutTrees != null & !cutTrees.isEmpty()) {
						// Found "cut" trees on this step, write a trace in
						// terminal
						//System.out.println("Date: " + stand.getDate() + ", cut trees: " + cutTrees.size());
					}
				}

				// Rounded under
				// ~ int N = (int) (((NProvider) methodProvider).getN (stand,
				// trees) * coefHa);
				double N = ((NProvider) methodProvider).getN(stand, trees) * coefHa; // fc
																						// -
																						// 22.8.2006
																						// -
																						// Numberable
																						// is
																						// double

				// c1.add (new Integer (date));
				if (isSet("xIsHdom")) { // PhD 2010-01-25
					hdom = ((HdomProvider) methodProvider).getHdom(stand, trees);
					if (hdom > 0) {
						c1.add(new Double(hdom));
					} else { // PhD 2014-11-25 : after clearcutting,
								// ((HdomProvider) methodProvider).getHdom
								// (stand, trees) will give hdom = 0
						c1.add(new Double(previousHdom));
					}
				} else {
					int date = stand.getDate();
					c1.add(new Double(date));
				}

				// ~ c2.add (new Integer (N));
				// c2.add (new Double (N)); // fc - 22.8.2006 - Numberable is
				// double
				// tl 29.1.2007 from DEGirthClassN
				// New option: if (roundN), N is rounded to the nearest int
				double numbers = 0;
				// if (isSet ("roundN")) {
				if (isSet("roundN") && !isSet("Log(N)")) { // PhD 2014-11-25
					numbers = (int) (N + 0.5); // fc - 29.9.2004 : +0.5 (sp)
				} else {
					if (isSet("Log(N)")) {
						numbers = Math.log(Math.max(1, N)); // PhD 2014-11-25
					} else {
						numbers = N; // fc - 22.8.2006 - Numberable is now
										// double
					}
					// numbers = N; // fc - 22.8.2006 - Numberable is now double
				}
				c2.add(new Double(numbers));

				previousHdom = hdom; // PhD 2014-11-25
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeN.doExtraction ()", "Exception caught : ", exc);
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
		// v.add (Translator.swap ("DETimeN.xLabel"));
		if (isSet("xIsHdom")) { // PhD 2010-01-25
			v.add(Translator.swap("DETimeN.xLabelHdom"));
		} else {
			v.add(Translator.swap("DETimeN.xLabelDate"));
		}
		if (isSet("perHectare")) {
			// v.add (Translator.swap ("DETimeN.yLabel")+" (ha)");
			if (isSet("Log(N)")) {
				v.add(Translator.swap("DETimeN.yLogHaLabel")); // PhD 2014-11-25
			} else {
				v.add(Translator.swap("DETimeN.yLabel") + " (ha)");
			}
		} else {
			// v.add (Translator.swap ("DETimeN.yLabel"));
			if (isSet("Log(N)")) {
				v.add(Translator.swap("DETimeN.yLogLabel")); // PhD 2014-11-25
			} else {
				v.add(Translator.swap("DETimeN.yLabel"));
			}
		}
		return v;
	}

}
