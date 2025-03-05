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
import capsis.defaulttype.TreeList;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.amapsim.AMAPsimRequestableTree;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.HeightStandardDeviationProvider;
import capsis.util.methodprovider.HgAmapsimProvider;
import capsis.util.methodprovider.HgProvider;
import capsis.util.methodprovider.MaxHProvider;
import capsis.util.methodprovider.MeanCrownBaseHProvider;
import capsis.util.methodprovider.MeanHProvider;
import capsis.util.methodprovider.MinHProvider;
import capsis.util.methodprovider.TreeCrownBaseHeightProvider;

/**
 * Individual Height versus Year (for one or several individuals).
 *
 * @author F. de Coligny - November 2000, review January 2004, modified by L.
 *         Saint-andre and Y. Caraglio - March 2004, modified by S. Turbis to
 *         add mean, min and max Height - August 2005, modified by F. de Coligny
 *         to comply with models without trees (Lemoine) - October 2006
 */
public class DETimeH extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeH");
	}

	protected Vector curves;
	protected Vector labels;

	private boolean availableHg; // maybe module does not calculate this
									// property
	private boolean availableHdom;
	private boolean availableHamapsim;
	private boolean availableHgAmapsim;
	private boolean availableHm; // st - 30.8.2005
	private boolean availableHeightStandardDeviation; // st - 30.8.2005
	private boolean availableHmin; // st - 30.8.2005
	private boolean availableHmax; // st - 30.8.2005
	private boolean availableMCBH; // fc + rs - 30.5.2008 - mean tree crown base
									// height
	private boolean availableCBH; // fc + rs - 30.5.2008 - tree crown base
									// height

	private boolean modelWithTrees; // fc - 12.10.2006 (Lemoine...)

	/**
	 * Constructor.
	 */
	public DETimeH() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeH(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeH.c ()", "Exception during construction", e);
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
			if (s instanceof TreeList) {
				return true;
			}
			checkMethodProvider(m.getMethodProvider());
			if (modelWithTrees) {
				return true;
			}
			if (availableHg || availableHdom || availableHamapsim || availableHgAmapsim || availableHm
					|| availableHeightStandardDeviation || availableHmin || availableHmax || availableMCBH
					|| availableCBH) {
				return true;
			}
			// fc - 12.10.2006

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeH.matchWith ()", "Error in matchWith () (returned false)", e);
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
		return Translator.swap("DETimeH.name");
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
		return Translator.swap("DETimeH.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1.4";

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
		addBooleanProperty("showHg");
		addBooleanProperty("showHdom");
		addBooleanProperty("showHamapsim");
		addBooleanProperty("showHgAmapsim");
		addBooleanProperty("showHm");
		addBooleanProperty("showHeightStandardDeviation");
		addBooleanProperty("showHmin");
		addBooleanProperty("showHmax");
		addBooleanProperty("showMCBH");
		addBooleanProperty("showCBH");

		// According to module capabiblities, disable some configProperties
		// fc-30.8.2012
		setPropertyEnabled("showHg", availableHg);
		setPropertyEnabled("showHdom", availableHdom);
		setPropertyEnabled("showHamapsim", availableHamapsim);
		setPropertyEnabled("showHgAmapsim", availableHgAmapsim);
		setPropertyEnabled("showHm", availableHm);
		setPropertyEnabled("showHeightStandardDeviation", availableHeightStandardDeviation);
		setPropertyEnabled("showHmin", availableHmin);
		setPropertyEnabled("showHmax", availableHmax);
		setPropertyEnabled("showMCBH", availableMCBH);
		setPropertyEnabled("showCBH", availableCBH);
	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider(MethodProvider methodProvider) {
		// Retrieve method provider
		// fc - 12.10.2006 - methodProvider = step.getScenario ().getModel
		// ().getMethodProvider ();

		// fc - added these initialisation (bug is true memorized and not
		// instance of HgProvider - Samsara)
		availableHg = false;
		availableHdom = false;
		availableHm = false;
		availableHeightStandardDeviation = false;
		availableHmin = false;
		availableHmax = false;
		availableMCBH = false;
		availableCBH = false;
		availableHamapsim = false;
		availableHgAmapsim = false;

		if (methodProvider instanceof HgProvider) {
			availableHg = true;
		}
		if (methodProvider instanceof HdomProvider) {
			availableHdom = true;
		}
		if (methodProvider instanceof MeanHProvider) {
			availableHm = true;
		}
		if (methodProvider instanceof HeightStandardDeviationProvider) {
			availableHeightStandardDeviation = true;
		}
		if (methodProvider instanceof MinHProvider) {
			availableHmin = true;
		}
		if (methodProvider instanceof MaxHProvider) {
			availableHmax = true;
		}
		if (methodProvider instanceof MeanCrownBaseHProvider) {
			availableMCBH = true;
		}
		if (methodProvider instanceof TreeCrownBaseHeightProvider) {
			availableCBH = true;
		}

		availableHamapsim = false;
		try { // fc - 12.10.2006
			TreeCollection tc = (TreeCollection) step.getScene();

			modelWithTrees = true; // fc - 12.10.2006

			for (Iterator i = tc.getTrees().iterator(); i.hasNext();) {
				Tree t = (Tree) i.next();
				if (t instanceof AMAPsimRequestableTree && ((AMAPsimRequestableTree) t).getAMAPsimTreeData() != null) {
					availableHamapsim = true;
					break;
				}
			}
		} catch (Exception e) {
			// stand may not be instance of TreeCollection (ex: Lemoine)
		}

		availableHgAmapsim = false;
		if (availableHamapsim) {
			if (methodProvider instanceof HgAmapsimProvider) {
				availableHgAmapsim = true;
			}
		}

		// ~ System.out.println ("availableHg "+availableHg);
		// ~ System.out.println ("availableHdom "+availableHdom);
		// ~ System.out.println ("availableHamapsim "+availableHamapsim);
		// ~ System.out.println ("availableHgAmapsim "+availableHgAmapsim);
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
		MethodProvider methodProvider = step.getProject().getModel().getMethodProvider();

		try {
			// If no curve at all, choose one tree with lowest id
			// fc - 6.2.2004
			if (modelWithTrees // fc - 12.10.2006
					&& !isSet("showHg") && !isSet("showHdom")
					&& !isSet("showHm")
					&& !isSet("showHeightStandardDeviation") && !isSet("showHmin")
					&& !isSet("showHmax")
					&& !isSet("showMCBH") && !isSet("showCBH") && (getTreeIds() == null || getTreeIds().isEmpty())) {
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
			Vector c2 = new Vector(); // optional: y coordinates (Hg)
			Vector c3 = new Vector(); // optional: y coordinates (Hdom)
			Vector c4 = new Vector(); // optional: y coordinates (HgAmapsim)
			Vector c5 = new Vector(); // optional: y coordinates (Hm)
			Vector c6 = new Vector(); // optional: y coordinates (Hmin)
			Vector c7 = new Vector(); // optional: y coordinates (Hmax)
			Vector c8 = new Vector(); // optional: y coordinates
										// (HeightStandardDeviation)
			Vector c9 = new Vector(); // optional: y coordinates (Mean crown
										// base height)

			int treeCurvesNumber = treeNumber;
			if (isSet("showHamapsim")) {
				treeCurvesNumber *= 2; // y coordinates (tree heights +
										// optionaly tree amapsim heights)
			}

			// fc + rs - 30.5.2008 - tree crown base height
			if (isSet("showCBH")) {
				treeCurvesNumber += treeNumber;
			}

			Vector cy[] = new Vector[treeCurvesNumber]; // y coordinates
														// (heights) (and cbh if
														// selected)

			for (int i = 0; i < treeCurvesNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();
				Collection trees = null;
				if (modelWithTrees) { // fc - 12.10.2006
					trees = ((TreeCollection) stand).getTrees(); // fc -
																	// 9.4.2004
				}

				int year = stand.getDate();
				c1.add(new Integer(year));

				if (isSet("showHg")) {
					c2.add(new Double(((HgProvider) methodProvider).getHg(stand, trees)));
				}
				if (isSet("showHdom")) {
					c3.add(new Double(((HdomProvider) methodProvider).getHdom(stand, trees)));
				}
				if (isSet("showHgAmapsim")) {
					c4.add(new Double(((HgAmapsimProvider) methodProvider).getHgAmapsim(stand, trees)));
				}
				if (isSet("showHm")) {
					c5.add(new Double(((MeanHProvider) methodProvider).getMeanH(stand, trees)));
				}
				if (isSet("showHmin")) {
					c6.add(new Double(((MinHProvider) methodProvider).getMinH(stand, trees)));
				}
				if (isSet("showHmax")) {
					c7.add(new Double(((MaxHProvider) methodProvider).getMaxH(stand, trees)));
				}
				if (isSet("showHeightStandardDeviation")) {
					c8.add(new Double(((HeightStandardDeviationProvider) methodProvider).getHeightStandardDeviation(
							stand, trees)));
				}
				if (isSet("showMCBH")) {
					c9.add(new Double(((MeanCrownBaseHProvider) methodProvider).getMeanCrownBaseH(stand, trees)));
				}

				// Get height for each tree
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
							if (isSet("showHamapsim")) {
								cy[n++].add(new Double(Double.NaN)); // amapsim
																		// height
							}
							// fc + rs - 30.5.2008
							if (isSet("showCBH")) {
								cy[n++].add(new Double(Double.NaN)); // cbh
							}

						} else {
							cy[n++].add(new Double(t.getHeight()));

							if (isSet("showHamapsim")) {
								try {
									AMAPsimRequestableTree at = (AMAPsimRequestableTree) t;
									double amapsimH = at.getAMAPsimTreeData().treeStep.height;
									cy[n++].add(new Double(amapsimH)); // amapsim
																		// height
								} catch (Exception e) {
									cy[n++].add(new Double(Double.NaN)); // amapsim
																			// height
								}
							}
							// fc + rs - 30.5.2008
							if (isSet("showCBH")) {
								double cbh = ((TreeCrownBaseHeightProvider) methodProvider).getTreeCrownBaseHeight(t);
								cy[n++].add(cbh); // cbh
							}

						}
					}
				}
			}

			// Ccurves
			//
			curves.clear();
			curves.add(c1);
			if (isSet("showHg")) {
				curves.add(c2);
			}
			if (isSet("showHdom")) {
				curves.add(c3);
			}
			if (isSet("showHgAmapsim")) {
				curves.add(c4);
			}
			if (isSet("showHm")) {
				curves.add(c5);
			}
			if (isSet("showHmin")) {
				curves.add(c6);
			}
			if (isSet("showHmax")) {
				curves.add(c7);
			}
			if (isSet("showHeightStandardDeviation")) {
				curves.add(c8);
			}
			if (isSet("showMCBH")) {
				curves.add(c9);
			}

			// Labels
			//
			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showHg")) {
				Vector y1Labels = new Vector();
				y1Labels.add("Hg");
				labels.add(y1Labels); // y1 : label "Hg"
			}
			if (isSet("showHdom")) {
				Vector y2Labels = new Vector();
				y2Labels.add("Hdom");
				labels.add(y2Labels); // y2 : label "Hdom"
			}
			if (isSet("showHgAmapsim")) {
				Vector y3Labels = new Vector();
				y3Labels.add("HgAmapsim");
				labels.add(y3Labels); // y3 : label "HgAmapsim"
			}
			if (isSet("showHm")) {
				Vector y4Labels = new Vector();
				y4Labels.add("Hm");
				labels.add(y4Labels); // y4 : label "Hm"
			}
			if (isSet("showHmin")) {
				Vector y5Labels = new Vector();
				y5Labels.add("Hmin");
				labels.add(y5Labels); // y5 : label "Hmin"
			}
			if (isSet("showHmax")) {
				Vector y6Labels = new Vector();
				y6Labels.add("Hmax");
				labels.add(y6Labels); // y5 : label "Hmax"
			}
			if (isSet("showHeightStandardDeviation")) {
				Vector y7Labels = new Vector();
				y7Labels.add("Sigma");
				labels.add(y7Labels); // y7 : label "HeightStandardDeviation"
			}
			if (isSet("showMCBH")) {
				Vector y9Labels = new Vector();
				y9Labels.add("Mc");
				labels.add(y9Labels); // y9 : label "Mean crown base height"
			}

			// fc + rs - 30.5.2008 - changed the labels definition strategy
			int i = 0; // fc - 6.2.2004
			int treeId = 0;
			while (i < treeCurvesNumber) {
				curves.add(cy[i]);
				Vector v = new Vector();
				v.add((String) getTreeIds().get(treeId));
				labels.add(v); // y curve name = matching treeId
				i++;

				if (isSet("showHamapsim")) { // n curves = 2 * n trees
					curves.add(cy[i]);
					v = new Vector();
					v.add((String) getTreeIds().get(treeId) + " AMAPsim");
					labels.add(v); // y curve name = matching treeId
					i++;
				}

				if (isSet("showCBH")) {
					curves.add(cy[i]);
					v = new Vector();
					v.add((String) getTreeIds().get(treeId) + "c");
					labels.add(v); // y curve name = matching treeId
					i++;
				}
				treeId++;
			}

			// ~ if (isSet ("showHamapsim")) { // n curves = 2 * n trees
			// ~ int i = 0; // fc - 6.2.2004
			// ~ int treeId = 0;
			// ~ while (i < treeCurvesNumber) {
			// ~ curves.add (cy[i]);
			// ~ Vector v = new Vector ();
			// ~ v.add ((String) treeIds.get (treeId));
			// ~ labels.add (v); // y curve name = matching treeId
			// ~ i++;

			// ~ curves.add (cy[i]);
			// ~ v = new Vector ();
			// ~ v.add ((String) treeIds.get (treeId)+" AMAPsim");
			// ~ labels.add (v); // y curve name = matching treeId
			// ~ i++;
			// ~ treeId++;
			// ~ }

			// ~ } else { // n urves = n trees
			// ~ for (int i = 0; i < treeCurvesNumber; i++) {
			// ~ curves.add (cy[i]);
			// ~ Vector v = new Vector ();
			// ~ v.add ((String) treeIds.get (i));
			// ~ labels.add (v); // y curve name = matching treeId

			// ~ }
			// ~ }

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeH.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeH.xLabel"));
		v.add(Translator.swap("DETimeH.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
