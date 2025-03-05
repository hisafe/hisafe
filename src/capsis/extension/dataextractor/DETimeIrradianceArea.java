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
 * Irradiance area along time.
 * 
 * @author B.Courbaud - December 2002
 */
public class DETimeIrradianceArea extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeIrradianceArea");
	}

	protected Vector curves;
	protected Vector labels;

	/**
	 * Constructor.
	 */
	public DETimeIrradianceArea() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeIrradianceArea(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeIrradianceArea.c ()", "Exception during construction", e);
		}
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
			Log.println(Log.ERROR, "DETimeIrradianceArea.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeIrradianceArea.name");
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
		return Translator.swap("DETimeIrradianceArea.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.1";
	}

	// nb-08.08.2018
	// public static final String VERSION = "1.1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		// Choose configuration properties
		addBooleanProperty("perHectare"); // fc-27.8.2021 Removed HECTARE property

		addBooleanProperty("percentage", false); // fc-23.8.2021
//		addConfigProperty(PaleoDataExtractor.PERCENTAGE);

		addConfigProperty(PaleoDataExtractor.CELL_GROUP); // fc - 9.4.2004 -
															// uncommented : was
															// used below
		addConfigProperty(PaleoDataExtractor.I_CELL_GROUP);

		// fc-26.8.2021 Added true below to have at least one series at first opening
		addBooleanProperty("showI20", true);
		addBooleanProperty("showI40");
		addBooleanProperty("showI60");
		addBooleanProperty("showI80");
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
			// per Ha computation
			double coefHa = 1;
			double coefPercent = 1;

			// fc-23.8.2021 check perHa, then percentage
			if (isSet("perHectare")) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			} else if (isSet("percentage")) {
				coefPercent = 100 / getSceneArea(step.getScene()); // fc-2.6.2020
			}

			// Retrieve Steps from root to this step
			Vector steps = step.getProject().getStepsFromRoot(step);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector c3 = new Vector(); // y coordinates
			Vector c4 = new Vector(); // y coordinates
			Vector c5 = new Vector(); // y coordinates

			// Data extraction : points with (Integer, Double) coordinates
			for (Iterator i = steps.iterator(); i.hasNext();) {
				Step s = (Step) i.next();

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				// ~ GPlot plot = stand.getPlot ();
				// ~ Collection cells = doFilter (plot); // uses current group
				// set in config panels

				Collection cells = doFilter(stand, TreeList.GROUP_PLOT_CELL);
				// CELL group - fc-17.9.2004
//				Collection cells = doFilter(stand, Group.CELL); 

				Iterator ite = cells.iterator();

				int date = stand.getDate();

				int tab[] = new int[5];
				int totalCellNb = 0;
				int numbers = 0;

				// Create output data
				while (ite.hasNext()) {
					Cell c = (Cell) ite.next();
					double relEnergy = ((CellRelativeHorizontalEnergyProvider) methodProvider)
							.getCellRelativeHorizontalEnergy(c);
					if (relEnergy == 100) {
						relEnergy = 99.9;
					}
					int category = (int) (relEnergy / 20);
					tab[category] += 1;
					totalCellNb += 1;
				}

				// fc-2.6.2020
				double cellArea = getSceneArea(stand) / totalCellNb;
//				double cellArea = stand.getArea() / totalCellNb;

				c1.add(new Integer(date));
				if (isSet("showI80")) {
					double IrradAreaSup80 = tab[4] * cellArea;
					c2.add(new Double(IrradAreaSup80 * coefHa * coefPercent));
				}
				if (isSet("showI60")) {
					double IrradAreaSup60 = (tab[4] + tab[3]) * cellArea;
					c3.add(new Double(IrradAreaSup60 * coefHa * coefPercent));
				}
				if (isSet("showI40")) {
					double IrradAreaSup40 = (tab[4] + tab[3] + tab[2]) * cellArea;
					c4.add(new Double(IrradAreaSup40 * coefHa * coefPercent));
				}
				if (isSet("showI20")) {
					double IrradAreaSup20 = (tab[4] + tab[3] + tab[2] + tab[1]) * cellArea;
					c5.add(new Double(IrradAreaSup20 * coefHa * coefPercent));
				}
			}

			curves.clear();
			curves.add(c1);

			if (isSet("showI80")) {
				curves.add(c2);
			}
			if (isSet("showI60")) {
				curves.add(c3);
			}
			if (isSet("showI40")) {
				curves.add(c4);
			}
			if (isSet("showI20")) {
				curves.add(c5);
			}

			labels.clear();
			labels.add(new Vector()); // no x labels
			if (isSet("showI80")) {
				Vector y1Labels = new Vector();
				y1Labels.add("I>80%");
				labels.add(y1Labels);
			}
			if (isSet("showI60")) {
				Vector y2Labels = new Vector();
				y2Labels.add("I>60%");
				labels.add(y2Labels);
			}
			if (isSet("showI40")) {
				Vector y3Labels = new Vector();
				y3Labels.add("I>40%");
				labels.add(y3Labels);
			}
			if (isSet("showI20")) {
				Vector y4Labels = new Vector();
				y4Labels.add("I>20%");
				labels.add(y4Labels);
			}
		} catch (Exception exc) {
			Log.println(Log.ERROR, "DETimeIrradianceArea.doExtraction ()", "Exception caught : ", exc);
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
		v.add(Translator.swap("DETimeIrradianceArea.xLabel"));

		// fc-23.8.2021 check perHa, then percentage
		if (isSet("perHectare")) {
			v.add(Translator.swap("DETimeIrradianceArea.yLabel") + " (m2/ha)");
		} else if (isSet("percentage")) {
			v.add(Translator.swap("DETimeIrradianceArea.yLabel") + " (% " + Translator.swap("DELayersCover.surface")
					+ ")");
		} else {
			v.add(Translator.swap("DETimeIrradianceArea.yLabel") + " (m2)");
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	public int getNY() {
		return curves.size() - 1;
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
