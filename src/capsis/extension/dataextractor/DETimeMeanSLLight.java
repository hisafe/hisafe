/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Philippe Dreyfus
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package capsis.extension.dataextractor;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.plotofcells.Cell;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.lib.samsaralight.SLTargetLightResult;
import capsis.lib.samsaralight.SLLightableTarget;
import capsis.lib.samsaralight.SLLightableModel;
import capsis.lib.samsaralight.SLLightableScene;
import capsis.lib.samsaralight.SLModel;
import capsis.lib.samsaralight.SLSensor;
import capsis.lib.samsaralight.SLSettings;

/**
 * Mean Percentage of above canopy light received by cell or sensor
 * dataextractor only available to model using SamsaraLight
 * 
 * @author G. Ligot - January 2014
 */
public class DETimeMeanSLLight extends DETimeG {

	static {
		Translator.addBundle("capsis.extension.dataextractor.DETimeMeanSLLight");
	}

	protected Vector curves;
	protected Vector labels;
	protected MethodProvider methodProvider;

	/**
	 * Constructor.
	 */
	public DETimeMeanSLLight() {
	}

	/**
	 * Constructor 2, uses the standard Extension starter.
	 */
	public DETimeMeanSLLight(GenericExtensionStarter s) {
		super(s);

		try {
			curves = new Vector();
			labels = new Vector();

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeMeanIrradiance.c ()", "Exception during construction : ", e);
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
			if (!(m instanceof SLLightableModel))
				return false;

		} catch (Exception e) {
			Log.println(Log.ERROR, "DETimeMeanSLLight.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	/**
	 * From DataFormat interface.
	 */
	@Override
	public String getName() {
		return getNamePrefix() + Translator.swap("DETimeMeanSLLight.name");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "G.Ligot";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return Translator.swap("DETimeMeanSLLight.description");
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	// nb-08.08.2018
	//public static final String VERSION = "1";

	/**
	 * This method is called by superclass DataExtractor.
	 */
	public void setConfigProperties() {
		String[] tab = { "CELL", "SENSOR" };
		addRadioProperty(tab);
	}

	/**
	 * From DataExtractor SuperClass. Computes the data series. This is the real
	 * output building. It needs a particular Step. Return false if trouble
	 * while extracting.
	 */
	public boolean doExtraction() {
		if (upToDate) {
			return true;
		}
		if (step == null) {
			return false;
		}

		Vector steps = step.getProject().getStepsFromRoot(step); // Retrieve
																	// Steps
																	// from root
																	// to this
																	// step
		Vector c1 = new Vector(); // x coordinates
		Vector c2 = new Vector(); // y coordinates

		SLModel slModel = ((SLLightableModel) step.getProject().getModel()).getSLModel();
		SLSettings slSettings = slModel.getSettings();

		// Data extraction : points with (Integer, Double) coordinates
		for (Iterator i = steps.iterator(); i.hasNext();) {
			Step s = (Step) i.next();

			try {
				RectangularPlot plot = (RectangularPlot) s.getScene().getPlot();
				SLLightableScene lightScene = (SLLightableScene) s.getScene();
				List<SLSensor> sensors = lightScene.getSensors();
				Collection<SquareCell> targetCells = new ArrayList<SquareCell>();
				targetCells = lightScene.getCellstoEnlight();
				if (targetCells == null)
					targetCells = plot.getCells();

				// number of light estimates
				int n = 0;
				if (isSet("CELL") && targetCells != null)
					n = targetCells.size();
				else if (sensors != null)
					n = sensors.size();
				else
					throw new Exception("No sensor or no cell with light values. The plot cannot be drawn.");

				// dataextraction
				double sum = 0;
				if (isSet("CELL")) {
					for (Iterator ic = targetCells.iterator(); ic.hasNext();) {
						Cell cell = (Cell) ic.next();
						SLTargetLightResult slCell = ((SLLightableTarget) cell).getLightResult();
						sum += slCell.get_horizontalRelativeEnergy();
					}
				}

				if (isSet("SENSOR")) {
					for (Iterator ic = sensors.iterator(); ic.hasNext();) {
						SLSensor sensor = (SLSensor) ic.next();
						sum += sensor.getLightResult().get_horizontalRelativeEnergy();
					}
				}

				// Consider restriction to one particular group if needed
				GScene stand = s.getScene();
				int date = stand.getDate();

				c1.add(new Integer(date));
				c2.add(new Double(sum / n));

			} catch (Exception exc) {
				Log.println(Log.ERROR, "DETimeMeanSLLight.doExtraction ()", "Exception caught : ", exc);
				return false;
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);
		}
		upToDate = true;
		return true;
	}

	/**
	 * From DFCurves interface.
	 */
	public List<String> getAxesNames() {
		Vector v = new Vector();
		v.add(Translator.swap("DETimeMeanSLLight.xLabel"));
		v.add(Translator.swap("DETimeMeanSLLight.yLabel"));
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
