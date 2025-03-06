/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
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

import java.util.Collection;
import java.util.Map;
import java.util.Vector;

import jeeb.lib.util.Log;
import repicea.simulation.allometrycalculator.AllometryCalculableTree;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;
import capsis.extension.dataextractor.superclass.DEDistribution;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtMethodProvider;
import capsis.util.extendeddefaulttype.ExtModel;

/**
 * This extractor provides the basal area per species.
 * @author Mathieu Fortin - June 2012
 */
public class DEBasalAreaComposition extends DEDistribution {

	public static enum MessageID implements TextableEnum {
		Description("Basal area distribution per species", "Distribution de la surface terri\u00E8re par esp\u00E8ce"),
		Species("Species", "Esp\u00E8ce"),
		ExtractorName("Basal area composition","Composition en surface terri\u00E8re"),
		Percentage("Percentage", "Pourcentage");
		
		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
	}
	
	// nb-07.08.2018
    //public static final String AUTHOR = "M. Fortin";
    //public static final String VERSION = "1.0";

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

	// nb-15.01.2019
	@Override
	public String getDefaultDataRendererClassName() {
		return "capsis.extension.datarenderer.drcurves.DRHistogram";
	}

	@Override
	public void setConfigProperties() {
		addBooleanProperty("perHectare", false);
		addBooleanProperty(REpiceaTranslator.getString(MessageID.Percentage), true); 
	}

	@Override
	public boolean doExtraction() {
		if (upToDate)
			return true;
		if (step == null)
			return false;

		try {

			// per Ha computation
			double coefHa = 1;
			if (isSet("perHectare") && !isSet(REpiceaTranslator.getString(MessageID.Percentage))) {
				coefHa = 10000 / getSceneArea(step.getScene()); // fc-2.6.2020
			}
			
			ExtCompositeStand stand = (ExtCompositeStand) step.getScene();
			ExtMethodProvider methodProvider = (ExtMethodProvider) model.getMethodProvider();

			double percentageFactor = 1d;
			if (isSet(REpiceaTranslator.getString(MessageID.Percentage))) {
				double basalArea = methodProvider.getG(stand, stand.getTrees());
				percentageFactor = 100d/basalArea;
			}

			Map<String, Collection<AllometryCalculableTree>> oMap = stand.getCollectionsBySpecies();
			
			Vector c1 = new Vector(); // x coordinates
			Vector c2 = new Vector(); // y coordinates
			Vector l1 = new Vector(); // labels for x axis (ex: 0-10, 10-20...)

			int i = 0;
			for (String species : oMap.keySet()) {
				c1.add(i++);
				l1.add(species);
				c2.add(methodProvider.getG(stand, oMap.get(species)) * coefHa * percentageFactor);
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
		return REpiceaTranslator.getString(MessageID.Species);
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
		if (isSet(REpiceaTranslator.getString(MessageID.Percentage))) {
			return("G (%)");
 		} else {
 			return "G (m2)";
 		}
	}

}
