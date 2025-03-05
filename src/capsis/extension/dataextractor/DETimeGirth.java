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
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;

/**
 * Individual girth versus Time (for one or several individuals).
 * 
 * @author C. Meredieu, T. Labbé - April 2005, modified by F. de Coligny to
 *         comply with models without trees (Lemoine) - October 2006
 */
public class DETimeGirth extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeGirth");
	}

	protected Vector curves;
	protected Vector labels;

	private boolean availableCg; // maybe module does not calculate this
									// property
	private boolean availableCdom;

	private boolean modelWithTrees; // fc - 12.10.2006 (Lemoine...)

	/**
	 * Constructor.
	 */
	public DETimeGirth() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeGirth(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider(step.getProject().getModel().getMethodProvider());

			// According to module capabiblities, disable some configProperties
			//
			setPropertyEnabled("showCg", availableCg);
			setPropertyEnabled("showCdom", availableCdom);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeGirth.c ()", "Exception during construction", e);
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

			GScene s = ((Step) m.getProject().getRoot()).getScene();

			// fc - 12.10.2006
			// ~ if (!(s instanceof TreeCollection)) {return false;}
			checkMethodProvider(m.getMethodProvider());
			if (modelWithTrees) {
				return true;
			}
			if (availableCg || availableCdom) {
				return true;
			}
			// fc - 12.10.2006

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeGirth.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return false; // fc - 8.1.2007 - bug correction [J. Labonne] always
						// returned true
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return Translator.swap("DETimeGirth.name");
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
		return Translator.swap("DETimeGirth.description");
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
	 * This method is called by superclass DataExtractor constructor. If
	 * previous config was saved in a file, this method may not be called. See
	 * etc/extensions.settings file
	 */
	public void setConfigProperties() {

		checkMethodProvider(step.getProject().getModel().getMethodProvider());

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);

		// These properties may be disabled in constructor according to module
		// capabilities
		//
		addBooleanProperty("showCg");
		addBooleanProperty("showCdom");

	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider(MethodProvider methodProvider) {
		// Retrieve method provider
		// fc - 12.10.2006 - methodProvider = step.getScenario ().getModel
		// ().getMethodProvider ();

		if (methodProvider instanceof DgProvider) {
			availableCg = true;
		}
		if (methodProvider instanceof DdomProvider) {
			availableCdom = true;
		}

		try { // fc - 12.10.2006
			TreeCollection tc = (TreeCollection) step.getScene();
			modelWithTrees = true; // fc - 12.10.2006
		} catch (Exception e) {
			// stand may not be instance of TreeCollection (ex: Lemoine)
		}

	}

	// ~ System.out.println ("availableCg "+availableCg);
	// ~ System.out.println ("availableCdom "+availableCdom);

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
		MethodProvider methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// If no curve at all, choose one tree
			// fc - 6.2.2004
			if (modelWithTrees // fc - 12.10.2006
					&& !isSet("showCg") && !isSet("showCdom") && (getTreeIds() == null || getTreeIds().isEmpty())) {
				TreeCollection tc = (TreeCollection) step.getScene();
				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}
				int minId = Integer.MAX_VALUE;
				for (Iterator i = tc.getTrees().iterator(); i.hasNext();) {
					Tree t = (Tree) i.next();
					minId = Math.min(minId, t.getId()); // fc - 27.9.2006
				}
				getTreeIds().add("" + minId);
			}

			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates (years)
			Vector c2 = new Vector(); // optional: y coordinates (Cg)
			Vector c3 = new Vector(); // optional: y coordinates (Cdom)

			int treeCurvesNumber = treeNumber;

			Vector cy[] = new Vector[treeCurvesNumber]; // y coordinates
														// (heights)

			for (int i = 0; i < treeCurvesNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();
				Collection trees = null; // fc - 12.10.2006
				if (modelWithTrees) { // fc - 12.10.2006
					trees = ((TreeCollection) stand).getTrees(); // fc -
																	// 12.10.2006
				} // fc - 12.10.2006

				int year = stand.getDate();

				c1.add(new Integer(year));
				if (isSet("showCg")) {
					c2.add(new Double(Math.PI * ((DgProvider) methodProvider).getDg(stand, trees)));
				}
				if (isSet("showCdom")) {
					c3.add(new Double(Math.PI * ((DdomProvider) methodProvider).getDdom(stand, trees)));
				}

				// Get diameter for each tree
				if (modelWithTrees) {
					int n = 0; // ith tree (O to n-1)
					for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
						int id = new Integer((String) ids.next()).intValue();
						Tree t = ((TreeCollection) stand).getTree(id);
						// bug correction : some maid trees may have number == 0
						// -> ignore them - tl 12/09/2005
						double number = 1; // fc - 22.8.2006 - Numberable
											// returns double
						if (t instanceof Numberable)
							number = ((Numberable) t).getNumber();
						if (t == null || t.isMarked() || number == 0) { // fc &
																		// phd -
																		// 5.1.2003
							cy[n++].add(new Double(Double.NaN)); // height

						} else {
							cy[n++].add(new Double(Math.PI * t.getDbh()));

						}
					}
				}

			}

			// Ccurves
			//
			curves.clear();
			curves.add(c1);
			if (isSet("showCg")) {
				curves.add(c2);
			}
			if (isSet("showCdom")) {
				curves.add(c3);
			}

			// Labels
			//
			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showCg")) {
				Vector y1Labels = new Vector();
				y1Labels.add("Cg");
				labels.add(y1Labels); // y1 : label "Cg"
			}
			if (isSet("showCdom")) {
				Vector y2Labels = new Vector();
				y2Labels.add("Cdom");
				labels.add(y2Labels); // y2 : label "Cdom"
			}

			// n urves = n trees
			for (int i = 0; i < treeCurvesNumber; i++) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(i));
				labels.add(v); // y curve name = matching treeId
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeGirth.doExtraction ()", "Exception caught : ", exc);
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
		return labels;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeGirth.xLabel"));
		v.add(Translator.swap("DETimeGirth.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
