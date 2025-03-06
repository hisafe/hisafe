/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2021 Her Majesty the Queen in right of Canada
 * Author: Mathieu Fortin, Canadian Wood Fibre Centre
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.extendeddefaulttype.dataextractor;

import java.util.List;
import java.util.Vector;

import capsis.extension.dataextractor.superclass.DEDistribution;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtCompositeStand.PredictorID;
import capsis.util.extendeddefaulttype.ExtMethodProvider;
import capsis.util.extendeddefaulttype.ExtModel;
import jeeb.lib.util.Log;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.thinners.REpiceaThinner;
import repicea.stats.estimates.Estimate;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * This extractor provides the proportion of the area by treatment .
 * @author Mathieu Fortin - March 2021
 */
public class DEAreaProportionTreatment extends DEDistribution {

	private static enum MessageID implements TextableEnum {
		Description("Area proportion by treatment", "Proportion de la surface par traitment"),
		Treatment("Treatment", "Traitement"),
		ExtractorName("Area proportion / treatment","Proportion de la surface / traitement"),
		Proportion("Annual proportion of area", "Proportion annuelle de la surface");
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}
	
    public static boolean matchWith(Object referent) {
    	return referent instanceof ExtModel;
    }

    @Override
	public String getName() {
		return REpiceaTranslator.getString(MessageID.ExtractorName);
	}

	@Override
	public String getAuthor() {
		return "M. Fortin";
	}

	public String getDescription() {
		return REpiceaTranslator.getString(MessageID.Description);
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}


	@Override
	public boolean doExtraction() {
		if (upToDate)
			return true;
		if (step == null)
			return false;

		try {
			ExtCompositeStand stand = (ExtCompositeStand) step.getScene();
			double timeStepFactor = 1; // default value
			if (!stand.isInitialScene()) {
				ExtCompositeStand previousStand = (ExtCompositeStand) ((Step) step.getFather()).getScene();
				timeStepFactor = 1d / (stand.getDateYr() - previousStand.getDateYr());
			}
			ExtMethodProvider methodProvider = (ExtMethodProvider) model.getMethodProvider();
			
			List<Enum> treatmentList = methodProvider.getDefaultTreatmentList(stand);

			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			Estimate<Matrix, SymmetricMatrix, ?> estimate = methodProvider.getAreaProportionByTreatmentEstimate(stand, treatmentList);
			Matrix mean = estimate.getMean();
			List<String> indices = estimate.getRowIndex();
			if (indices != null && !indices.isEmpty()) {
				for (int i = 0; i < indices.size(); i++) {
					c1.add(i);
					l1.add(indices.get(i));
					c2.add(mean.getValueAt(i, 0) * timeStepFactor);
				}
			} else {
				c1.add(0);
				l1.add("Any");
				c2.add(mean.getValueAt(0, 0));
			}

			curves.clear();
			curves.add(c1);
			curves.add(c2);

			labels.clear();
			labels.add(l1);

		} catch (Exception exc) {
			Log.println(Log.ERROR, "DEDistribution.doExtraction ()",
					"Exception: ", exc);
			return false;
		}

		upToDate = true;
		return true;
	}

	@Override
	protected String getXLabel() {
		return REpiceaTranslator.getString(MessageID.Treatment);
	}

	@Override
	protected Number getValue(Object o) {
		return null;
	}

	/**
	 * Returns the name of the Y axis. A translation should be provided, see
	 * Translator.
	 */
	@Override
	protected String getYLabel() {
		return MessageID.Proportion.toString();
	}

}
