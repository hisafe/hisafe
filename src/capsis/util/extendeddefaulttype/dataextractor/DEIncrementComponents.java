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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import capsis.extension.dataextractor.superclass.DEMultiTimeX;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.extendeddefaulttype.methodprovider.PeriodicAnnualIncrementEstimatesProvider;
import capsis.util.extendeddefaulttype.methodprovider.PeriodicAnnualIncrementEstimatesProvider.GrowthComponent;
import jeeb.lib.util.Log;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.covariateproviders.MethodProviderEnum.VariableForEstimation;
import repicea.stats.estimates.Estimate;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * This data extractor provides the periodic annual increment component of the basal area growth.
 * @author Mathieu Fortin - July 2010
 */
public class DEIncrementComponents extends DEMultiTimeX {

	private static enum MessageID implements TextableEnum {
		yLabelG("PAI (m2/", "AAP (m2/"),
		yLabelN("PAI (stems/", "AAP (arbres/"),
		yLabelV("PAI (m3/", "AAP (m3/"),
		xLabel("Time (yr)", "Temps (ann\u00E9es)"),
		yLabel2("yr)","an)"),
		ExtractorName("Periodic annual increment / Time", 
				"Accroissement annuel p\u00E9riodique / Temps"),
		Description("Periodic annual increment in basal area in terms of mortality, survivor growth and recruitment",
				"Accroissement annuel p\u00E9riodique en surface terri\u00E8re provenant de la mortalit\u00E9, de l'accroissement des survivants et du recrutement"),
		Variable("Variable", "Variable");

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
	}
	
	private final Map<String, VariableForEstimation> matchVariableMap;
	
	public DEIncrementComponents() {
		matchVariableMap = new HashMap<String, VariableForEstimation>();
	}
	
	
	// nb-07.08.2018
	//public static final String AUTHOR = "M. Fortin";
	//public static final String VERSION = "1.0";

	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof ExtModel)) {return false;}
			ExtModel m = (ExtModel) referent;
			MethodProvider mp = m.getMethodProvider ();
			if (!(mp instanceof PeriodicAnnualIncrementEstimatesProvider)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "DEBasalAreaGrowthComponents.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
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
	protected String getYLabel() {
		VariableForEstimation var = findSelectedVariable();
		switch(var) {
		case N:
			return REpiceaTranslator.getString(MessageID.yLabelN);
		case G:
			return REpiceaTranslator.getString(MessageID.yLabelG);
		case V:
			return REpiceaTranslator.getString(MessageID.yLabelV);
		default:
			return "";
		}
	}
	
	@Override
	public void setConfigProperties () {
		LinkedList<String> list = new LinkedList<String>();
		for (VariableForEstimation var : VariableForEstimation.values()) {
			list.add(var.toString());
			matchVariableMap.put(var.toString(), var);
		}
		addComboProperty("variableType", list);
	}

	@Override
	protected List<Object> getTypes(GScene stand) {
		List<Object> outputList = new ArrayList<Object>();
		for (GrowthComponent component : GrowthComponent.values()) {
			outputList.add(component);
		}
		return outputList;
	}

	
	@Override
	public List<String> getAxesNames () {
		List<String> v = new ArrayList<String> ();
		v.add(REpiceaTranslator.getString(MessageID.xLabel));
		v.add (getYLabel() + "ha/" + REpiceaTranslator.getString(MessageID.yLabel2));
		return v;
	}

	@Override
	protected Number getValue(GModel m, GScene stand, int typeId, Object type) {
		VariableForEstimation var = findSelectedVariable();

		PeriodicAnnualIncrementEstimatesProvider provider2 = (PeriodicAnnualIncrementEstimatesProvider) m.getMethodProvider();
		Estimate<Matrix, SymmetricMatrix, ?> est = provider2.getPAIEstimate((ExtCompositeStand) stand, (GrowthComponent) type, var);
		if (est == null) {
			return 0d;
		} else {
			return est.getMean().getValueAt(0, 0);
		}
	}
	
	private VariableForEstimation findSelectedVariable() {
		String selectedVariable = getComboProperty("variableType");
		VariableForEstimation var = matchVariableMap.get(selectedVariable);
		return var;
	}
	
}
