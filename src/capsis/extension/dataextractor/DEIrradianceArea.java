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
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Cell;
import capsis.extension.PaleoDataExtractor;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.group.Group;
import capsis.util.methodprovider.CellRelativeHorizontalEnergyProvider;

/**
 * Number of ground cell per irradiance classes.
 * 
 * @author B.Courbaud - March 2002
 */
public class DEIrradianceArea extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DEIrradianceArea");
	}

	private Vector labels;

	/**
	 * Constructor.
	 */
	public DEIrradianceArea() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DEIrradianceArea(GenericExtensionStarter s) {
		super(s);

		labels = new Vector();

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
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof CellRelativeHorizontalEnergyProvider)) {
				return false;
			}
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!s.hasPlot()) {
				return false;
			}
			// if (!(s.getPlot ().getFirstCell () instanceof MountCell)) {return
			// false;}

		} catch (Exception e) {
			Log.println(Log.ERROR, "DEIrradianceArea.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DEIrradianceArea.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "B. Courbaud";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DEIrradianceArea.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.0";

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

//		addConfigProperty(PaleoDataExtractor.PERCENTAGE); // fc-29.7.2021

		addBooleanProperty("percentage", false); // fc-29.7.2021

		addConfigProperty(PaleoDataExtractor.CELL_GROUP);
		addConfigProperty(PaleoDataExtractor.I_CELL_GROUP);
		
//		addConfigProperty(PaleoDataExtractor.CLASS_WIDTH);
		addIntProperty ("classWidthInCm", 1); // fc-26.8.2021

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
		// System.out.println ("DEIrradianceArea : extraction being made");

		try {
			// per Ha computation
			double coefHa = 1;
			double coefPercentage = 1;
			double totalArea = getSceneArea(step.getScene()); // fc-2.6.2020
			if (isSet("perHectare"))
				coefHa = 10000 / totalArea;

//			if (settings.classWidth < 1)
//				settings.classWidth = 1;
//			int classWidth = settings.classWidth;
			
			// fc-26.8.2021 Removed CLASS_WIDTH
			int classWidth = getIntProperty("classWidthInCm");
			// Security
			if (classWidth < 1)
				classWidth = 1;			
			
			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			// Consider restriction to one particular group if needed
			GScene stand = step.getScene();
			// ~ GPlot plot = stand.getPlot ();
			// ~ Collection cells = doFilter (plot); // uses current group set
			// in config panels

			// fc-1.7.2019
			Collection cells = doFilter(stand, TreeList.GROUP_PLOT_CELL);
			// CELL group - fc-17.9.2004
//			Collection cells = doFilter(stand, Group.CELL); 

			Iterator ite = cells.iterator();

			// Limited in size! (java initializes each value to 0)
			int tab[] = new int[200];
			int maxCat = 0;
			int minCat = 200;
			int totalCellNb = 0;
			int numbers = 0;

			// Create output data
			while (ite.hasNext()) {
				Cell c = (Cell) ite.next();
				double relEnergy = ((CellRelativeHorizontalEnergyProvider) methodProvider)
						.getCellRelativeHorizontalEnergy(c);
				Log.println("relE : " + relEnergy);
				int category = (int) (relEnergy / classWidth);
				tab[category] += 1;
				totalCellNb += 1;

				if (category > maxCat) {
					maxCat = category;
				}
				if (category < minCat) {
					minCat = category;
				}
			}

			double cellArea = totalArea / totalCellNb;

			for (int i = minCat; i <= maxCat; i++) {
				int classBase = i * classWidth;
				int a = classBase + classWidth / 2;
				c1.add(new Integer(a));

				// fc-29.7.2021 Check ha first to prevent ha and % to appear together (title
				// bar)
				// fc-29.7.2021 replaced PERCENTAGE
				if (isSet("perHectare"))
					numbers = (int) (tab[i] * cellArea * coefHa);
				else
					numbers = (int) (100 * tab[i] / totalCellNb);

//				if (isSet("percentage"))
//					numbers = (int) (100 * tab[i] / totalCellNb);
//				else
//					numbers = (int) (tab[i] * cellArea * coefHa);

				c2.add(new Double(numbers));
				l1.add("" + classBase + "-" + (classBase + classWidth));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEIrradianceArea.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DEIrradianceArea.xLabel"));

		// fc-29.7.2021 Check ha first to prevent ha and % to appear together (title
		// bar)
		// fc-29.7.2021 replaced PERCENTAGE
		if (isSet("perHectare")) {
			v.add(Translator.swap("DEIrradianceArea.yLabel") + " (ha)");
		} else if (isSet("percentage")) {
			v.add(Translator.swap("DEIrradianceArea.yLabel") + " (%)");
		} else {
			v.add(Translator.swap("DEIrradianceArea.yLabel"));
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<List<String>> getLabels() {
		return labels;
	}

}
