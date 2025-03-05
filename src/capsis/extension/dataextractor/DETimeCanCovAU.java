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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.kernel.GModel;
import capsis.kernel.MethodProvider;
import capsis.kernel.Plot;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.CustomIdentifiable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.methodprovider.AnalysisUnit;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Canopy cover of every AU versus Year.
 * 
 * @author Ph. Dreyfus - October 2007 (... derived from DETimeGirth.java)
 */
public class DETimeCanCovAU extends PaleoDataExtractor implements DFCurves, Serializable {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeCanCovAU");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeCanCovAU() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeCanCovAU(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

			MethodProvider mp = step.getProject().getModel().getMethodProvider();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeCanCovAU.c ()", "Exception during construction", e);
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	public boolean matchWith(Object referent) {

		if (!(referent instanceof GModel)) {
			return false;
		}
		GModel m = (GModel) referent;
		// if (m instanceof VtxModel || m instanceof VtgModel) {return true;}

		Step root = (Step) m.getProject().getRoot();
		
		// fc-26.10.2017
		Plot plot = root.getScene().getPlot();
		if (!(plot instanceof PlotOfCells))
			return false;
		PlotOfCells plotc = root.getScene().getPlot();
		
		Collection cells = plotc.getCellsAtLevel(2);

		if (cells == null || cells.size() == 0) {
			return false;
		}
		Cell c = (Cell) cells.iterator().next();

		if (c != null && c instanceof AnalysisUnit) {
			return true;
		}

		return false;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return Translator.swap("DETimeCanCovAU.name");
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
		return Translator.swap("DETimeCanCovAU.description");
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
	
	/**
	 * This method is called by superclass DataExtractor constructor. If
	 * previous config was saved in a file, this method may not be called. See
	 * etc/extensions.settings file
	 */
	public void setConfigProperties() {
		// fc - 30.11.2007 - create a set property to choose AUnums
		String[] possibleItems = searchPossibleItems();
		String[] selectedItems = new String[0];
		addSetProperty("auNums", possibleItems, selectedItems);

		MethodProvider mp = step.getProject().getModel().getMethodProvider();

	}

	// Make the list of candidate AUs within which the user can choose
	// This list may need to be updated when step changes
	//
	private String[] searchPossibleItems() {
		TreeList stand = (TreeList) step.getScene();
		
		PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017
		
		Collection cells = plotc.getCells();
		Collection<String> candidates = new ArrayList<String>();
		for (Iterator i = cells.iterator(); i.hasNext();) {
			Cell c = (Cell) i.next();
			if (c instanceof CustomIdentifiable) {
				candidates.add(((CustomIdentifiable) c).getCustomId());
			}
		}
		String[] possibleItems = new String[candidates.size()];
		possibleItems = candidates.toArray(possibleItems);
		return possibleItems;
	}

	/**
	 * MultiConfigurable. Redefinition to update possible items.
	 */
	public ConfigurationPanel getSharedConfigPanel(Object param) { // fc -
																	// 14.12.2007
		// Update the list of candidate AUs - fc - 14.12.2007
		String[] possibleItems = searchPossibleItems();
		updateSetProperty("auNums", possibleItems);
		System.out.println("*** DETimeCanCovAU... updateSetProperty ('auNums', possibleItems)");
		return super.getSharedConfigPanel(param);
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

		try {
			Set selectedAUNums = new HashSet (getSetProperty("auNums")); // fc - 30.11.2007

			// Restrict the AUs collection to those selected - fc - 30.11.2007
			
			PlotOfCells plotc = step.getScene().getPlot(); // fc-26.10.2017
			
			Collection AUs = new ArrayList(plotc.getCellsAtLevel(2));
			for (Iterator i = AUs.iterator(); i.hasNext();) {
				CustomIdentifiable c = (CustomIdentifiable) i.next();
				if (!selectedAUNums.contains(c.getCustomId())) {
					i.remove();
				}
			}
			int auNumber = AUs.size();

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			// Curves
			//
			curves.clear();

			Vector c1 = new Vector(); // x coordinates (years)
			curves.add(c1);

			Vector cy[] = new Vector[auNumber]; // y coordinates (heights)
			for (int i = 0; i < auNumber; i++) {
				Vector v = new Vector();
				cy[i] = v;
				curves.add(cy[i]);
			}

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) { // D�but
																// it�ration sur
																// STEPS
				Step s = (Step) i.next();
				TreeList stand = (TreeList) s.getScene();

				// Restrict the AUs collection to those selected - fc -
				// 30.11.2007
				
				plotc = s.getScene().getPlot(); // fc-26.10.2017
				
				AUs = new ArrayList(plotc.getCellsAtLevel(2));
				for (Iterator k = AUs.iterator(); k.hasNext();) {
					CustomIdentifiable c = (CustomIdentifiable) k.next();
					if (!selectedAUNums.contains(c.getCustomId())) {
						k.remove();
					}
				}

				int year = stand.getDate();
				c1.add(new Integer(year));

				// Get Couv for each AnalysisUnit
				int n = 0; // ith tree (0 to n-1)
				for (Iterator j = AUs.iterator(); j.hasNext();) { // D�but
																	// it�ration
																	// sur AUs
					AnalysisUnit au = (AnalysisUnit) j.next();
					cy[n++].add(new Double(au.getCanCov()));
				}
			}

			// Labels
			//
			labels.clear();
			labels.add(new Vector()); // no x labels

			int n = 0;
			for (Iterator i = AUs.iterator(); i.hasNext();) {
				AnalysisUnit au = (AnalysisUnit) i.next();
				Vector yLabels = new Vector(); // y labels
				labels.add(yLabels);
				yLabels.add((String) au.getAUName());
			}

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeCanCovAU.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeCanCovAU.xLabel"));
		v.add(Translator.swap("DETimeCanCovAU.yLabel"));
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
	}

}
