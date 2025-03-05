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
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.amapsim.AMAPsimRequestableTree;
import capsis.lib.amapsim.AMAPsimTreeData;
import capsis.util.methodprovider.VmoyAmapsimProvider;

/**
 * Individual Volume versus Year (for one or several individuals).
 * 
 * @author by L. Saint-André and Y. Caraglio, March 2004
 */
public class DETimeVolumeInd extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeVolumeInd");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	private boolean availableVamapsim;
	private boolean availableVmoyAmapsim;

	/**
	 * Constructor.
	 */
	public DETimeVolumeInd() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeVolumeInd(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			checkMethodProvider();

			// According to module capabibilities, disable some configProperties
			setPropertyEnabled("showVamapsim", availableVamapsim);
			setPropertyEnabled("showVmoyAmapsim", availableVmoyAmapsim);

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVolumeInd.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel))
				return false;

			GModel m = (GModel) referent;

			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection))
				return false;

			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees().isEmpty())
				return false;

			Tree t = tc.getTrees().iterator().next();
			if (!(t instanceof AMAPsimRequestableTree))
				return false;

			AMAPsimRequestableTree tree = (AMAPsimRequestableTree) t;
			AMAPsimTreeData data = tree.getAMAPsimTreeData();
			if (data == null)
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeVolumeInd.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return Translator.swap("DETimeVolumeInd.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "L. Saint-André and Y. Caraglio";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeVolumeInd.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.2";
	}

	// nb-09.08.2018
	//public static final String VERSION = "1.2";

	/**
	 * This method is called by superclass DataExtractor constructor. If
	 * previous config was saved in a file, this method may not be called. See
	 * etc/extensions.settings file
	 */
	public void setConfigProperties() {

		checkMethodProvider();

		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_IDS);

		// These properties may be disabled in constructor according to module
		// capabilities
		//
		addBooleanProperty("showVamapsim", true);
		addBooleanProperty("showVmoyAmapsim");
	}

	// Evaluate MethodProvider's capabilities to propose more or less options
	// fc - 6.2.2004 - step variable is available
	//
	private void checkMethodProvider() {
		// Retrieve method provider
		methodProvider = step.getProject().getModel().getMethodProvider();

		availableVamapsim = false;
		TreeCollection tc = (TreeCollection) step.getScene();
		for (Iterator i = tc.getTrees().iterator(); i.hasNext();) {
			Tree t = (Tree) i.next();
			if (t instanceof AMAPsimRequestableTree && ((AMAPsimRequestableTree) t).getAMAPsimTreeData() != null) {
				availableVamapsim = true;
				break;
			}
		}

		availableVmoyAmapsim = false;
		if (availableVamapsim) {
			if (methodProvider instanceof VmoyAmapsimProvider) {
				availableVmoyAmapsim = true;
			}
		}

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
			// If no curve at all, choose one tree
			// fc - 6.2.2004
			if (!isSet("showVamapsim") && !isSet("showVmoyAmapsim") && (getTreeIds() == null || getTreeIds().isEmpty())) {
				TreeCollection tc = (TreeCollection) step.getScene();
				if (getTreeIds() == null) {
					setTreeIds(new Vector());
				}
				getTreeIds().add("" + tc.getTrees().iterator().next().getId());
			}

			int treeNumber = getTreeIds().size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates (years)
			Vector c2 = new Vector(); // optional: y coordinates
										// (VmoyAmapsimTree)

			int treeCurvesNumber = treeNumber;
			if (isSet("showVamapsim")) {
				treeCurvesNumber *= 2; // y coordinates (tree heights +
										// optionaly tree amapsim heights)
			}
			Vector cy[] = new Vector[treeCurvesNumber]; // y coordinates
														// (volumes)

			for (int i = 0; i < treeCurvesNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				GScene stand = s.getScene();
				Collection trees = ((TreeCollection) stand).getTrees(); // fc -
																		// 9.4.2004

				int year = stand.getDate();

				c1.add(new Integer(year));
				if (isSet("showVmoyAmapsim")) {
					c2.add(new Double(((VmoyAmapsimProvider) methodProvider).getVmoyAmapsim(stand, trees)));
				}

				// Get volume for each tree
				int n = 0; // ith tree (O to n-1)
				for (Iterator ids = getTreeIds().iterator(); ids.hasNext();) {
					int id = new Integer((String) ids.next()).intValue();
					Tree t = ((TreeCollection) stand).getTree(id);

					if (t == null || t.isMarked()) { // fc & phd - 5.1.2003
						cy[n++].add(new Double(Double.NaN)); // vol
						if (isSet("showVamapsim")) {
							cy[n++].add(new Double(Double.NaN)); // amapsim vol
						}
					} else {
						cy[n++].add(new Double(Double.NaN)); // NOT VERY NICE
																// !!!!
																// To be further
																// Completed by
																// LSA
																// (Individual
																// volume method
																// provider
																// required)

						if (isSet("showVamapsim")) {
							try {
								AMAPsimRequestableTree at = (AMAPsimRequestableTree) t;
								double amapsimV = at.getAMAPsimTreeData().treeStep.trunkVolume;
								cy[n++].add(new Double(amapsimV)); // amapsim
																	// Volume
							} catch (Exception e) {
								cy[n++].add(new Double(Double.NaN)); // amapsim
																		// Volume
							}
						}

					}
				}
			}

			// Curves
			//
			curves.clear();
			curves.add(c1);
			if (isSet("showVmoyAmapsim")) {
				curves.add(c2);
			}

			// Labels
			//
			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showVmoyAmapsim")) {
				Vector y1Labels = new Vector();
				y1Labels.add("VmoyAmapsim");
				labels.add(y1Labels); // y1 : label "VmoyAmapsim"
			}

			if (isSet("showVamapsim")) { // n curves = 2 * n trees
				int i = 0; // fc - 6.2.2004
				int treeId = 0;
				while (i < treeCurvesNumber) {
					curves.add(cy[i]);
					Vector v = new Vector();
					v.add((String) getTreeIds().get(treeId));
					labels.add(v); // y curve name = matching treeId
					i++;

					curves.add(cy[i]);
					v = new Vector();
					v.add((String) getTreeIds().get(treeId) + " AMAPsim");
					labels.add(v); // y curve name = matching treeId
					i++;
					treeId++;

				}

			} else { // n urves = n trees
				for (int i = 0; i < treeCurvesNumber; i++) {
					curves.add(cy[i]);
					Vector v = new Vector();
					v.add((String) getTreeIds().get(i));
					labels.add(v); // y curve name = matching treeId
				}
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeVolumeInd.doExtraction ()", "Exception caught : ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	// fc - 21.4.2004 - DataExtractor.getCaption () is better
	// ~ public String getCaption () {
	// ~ String caption = getStep ().getCaption ();
	// ~ if (treeIds != null && !treeIds.isEmpty ()) {
	// ~ caption += " - "+Translator.swap ("DETimeVolumeInd.tree")
	// ~ +" "+Tools.toString (treeIds);
	// ~ }
	// ~ return caption;
	// ~ }

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
		v.add(Translator.swap("DETimeVolumeInd.xLabel"));
		v.add(Translator.swap("DETimeVolumeInd.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
