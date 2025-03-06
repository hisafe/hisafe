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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import capsis.extension.PaleoDataExtractor;
import capsis.extension.dataextractor.configuration.ListOfConfigProperties;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.GenericExtensionStarter;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtMethodProvider;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.extendeddefaulttype.methodprovider.AverageProductionProvider;
import capsis.util.extendeddefaulttype.methodprovider.DDomEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.GEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.HDomEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.NEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.VEstimateProvider;
import jeeb.lib.util.Log;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.stats.estimates.ConfidenceInterval;
import repicea.stats.estimates.Estimate;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.Language;
import repicea.util.REpiceaTranslator.TextableEnum;

public abstract class AbstractDETimeWithCI extends AbstractDETime {

	protected boolean isStochastic;
	protected boolean confidenceIntervalsEnabled;
//	protected boolean allRealizationsShowedEnabled;
	
	private enum MessageID implements TextableEnum {
		Time("Time", "Temps"),
		Ha("ha)", "ha)"),
		Mean("Mean", "Moyenne"),
		Lower("Lower", "Inf"),
		Upper("Upper", "Sup"),
		ProbabilityLevel("Probability level", "Niveau de probabilit\u00E9"),
//		ShowAllCurve("Show all realizations", "Afficher toutes les r\u00E9alisations"),
		AreaHa("Population area (ha)", "Surface de la population (ha)"),
//		IncludeSamplingError("Include sampling error", "Inclure l'erreur d'\u00E9chantillonnage"),
		;
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

	protected enum Variable implements TextableEnum {
		V("V (m3)", "V (m3)", true),
		G("G (m2)", "G (m2)", true),
		N("N", "N", true),
		H("H0 (m)", "H0 (m)", false),
		D("D0 (cm)", "D0 (cm)", false),
		P("P (m3/yr)", "P (m3/an)", true),
		AverageRingWidth("width (cm/yr)", "largeur (cm/an)", false);
		
		private final boolean perHa;
		
		Variable(String englishText, String frenchText, boolean perHa) {		// perHa means that the variable can be expressed per hectare
			setText(englishText, frenchText);
			this.perHa = perHa;
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		public boolean isPerHectareAllowed() {return perHa;}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}

	
	protected final Variable variable;
	
	/**
	 * Phantom constructor. Only to ask for extension properties (authorName,
	 * version...).
	 */
	protected AbstractDETimeWithCI(Variable v) {
		variable = v;
	}

	/**
	 * Official constructor. It uses the standard Extension starter.
	 */
	protected AbstractDETimeWithCI(GenericExtensionStarter s, Variable v) {
		super(s);
		variable = v;
		if (s.getModel() instanceof ExtModel) {
			isStochastic = ((ExtModel) s.getModel()).getSettings().isStochastic();
		} else {
			isStochastic = false;
		}
	}


	/**
	 * This method is called by superclass DataExtractor.
	 */
	@Override
	public void setConfigProperties() {
		// Choose configuration properties
		addConfigProperty(PaleoDataExtractor.TREE_GROUP);
		addConfigProperty(PaleoDataExtractor.I_TREE_GROUP); // group individual
		addDoubleProperty(MessageID.ProbabilityLevel.toString(), 0.95);
		if (variable.isPerHectareAllowed()) {
			addIntProperty(MessageID.AreaHa.toString(), 1);
		}
	}

	@Override
	protected void retrieveSettings() {
		super.retrieveSettings();
		correctThisProperty(MessageID.ProbabilityLevel, 1d, getSettings().doubleProperties);
		correctThisProperty(MessageID.AreaHa, 1, getSettings().intProperties);
	}
	
	// fc-30.8.2021 Adapted, propertyMap changed of type, added the filler to select int/double
	protected void correctThisProperty(TextableEnum textableEnum, double filler, ListOfConfigProperties<Double> propertyMap) {
		if (!propertyMap.containsKey(textableEnum.toString())) {
			List<String> retrievedTranslations = retrieveAllTranslations(textableEnum);
			for (String translation : retrievedTranslations) {
				if (propertyMap.containsKey(translation)) {
					propertyMap.put(textableEnum.toString(), propertyMap.get(translation));
					propertyMap.remove(translation);
					break;
				}
			}
		}
	}
	
	// fc-30.8.2021 Adapted, propertyMap changed of type, added the filler to select int/double
	protected void correctThisProperty(TextableEnum textableEnum, int filler, ListOfConfigProperties<Integer> propertyMap) {
		if (!propertyMap.containsKey(textableEnum.toString())) {
			List<String> retrievedTranslations = retrieveAllTranslations(textableEnum);
			for (String translation : retrievedTranslations) {
				if (propertyMap.containsKey(translation)) {
					propertyMap.put(textableEnum.toString(), propertyMap.get(translation));
					propertyMap.remove(translation);
					break;
				}
			}
		}
	}
	
	protected void correctThisProperty(TextableEnum textableEnum, Map propertyMap) {
		if (!propertyMap.containsKey(textableEnum.toString())) {
			List<String> retrievedTranslations = retrieveAllTranslations(textableEnum);
			for (String translation : retrievedTranslations) {
				if (propertyMap.containsKey(translation)) {
					propertyMap.put(textableEnum.toString(), propertyMap.get(translation));
					propertyMap.remove(translation);
					break;
				}
			}
		}
	}
	
	private List<String> retrieveAllTranslations(TextableEnum textableEnum) {
		List<String> translations = new ArrayList<String>();
		for (Language language : Language.values()) {
			translations.add(REpiceaTranslator.getTranslation(textableEnum, language));
		}
		return translations;
	}
	
	
	protected Estimate<Matrix, SymmetricMatrix, ?> getEstimate(GScene stand, Collection trees) {
		switch(variable) {
		case V:
			return ((VEstimateProvider) methodProvider).getVolumePerHaEstimate(stand, trees);
		case G:
			return ((GEstimateProvider) methodProvider).getGPerHaEstimate(stand, trees);
		case N:
			return ((NEstimateProvider) methodProvider).getNPerHaEstimate(stand, trees);
		case H:
			return ((HDomEstimateProvider) methodProvider).getHdomEstimate(stand, trees);
		case D:
			return ((DDomEstimateProvider) methodProvider).getDdomEstimate(stand, trees);
		case P:
			return ((AverageProductionProvider) methodProvider).getAverageProductionPerHa((ExtCompositeStand) stand);
		default:
			return null;
		}
	}
	
	@Override
	protected abstract String getYAxisLabelName();
	
	
	protected int getCoefHa() {
		if (variable.isPerHectareAllowed()) {
			int nbHa = getIntProperty(MessageID.AreaHa.toString());
			if (nbHa >= 1) {
				return nbHa;
			}
		} 
		return 1;
	}

	

	/**
	 * From DataExtractor SuperClass.
	 * 
	 * Computes the data series. This is the real output building. It needs a
	 * particular Step.
	 * 
	 * Return false if trouble while extracting.
	 */
	@Override
	protected Vector<Vector<? extends Number>> extractValues() {
		Vector<Vector<? extends Number>> output = new Vector<Vector<? extends Number>>();

		double confidenceLevel = getDoubleProperty(MessageID.ProbabilityLevel.toString());

		if (confidenceLevel < 0) {
			confidenceLevel = 0;
		} else if (confidenceLevel > 1) {
			confidenceLevel = 1;
		}

		double alpha =  (1 - confidenceLevel); 
		confidenceIntervalsEnabled = alpha < 1d;
//		allRealizationsShowedEnabled = isSet(MessageID.ShowAllCurve.toString());
//		Integer maxNumberOfRealizations = null;


		// Retrieve Steps from root to this step
		Vector steps = step.getProject().getStepsFromRoot(step);

		Vector<Integer> c1 = new Vector<Integer>(); // x coordinates
		Vector<Double> c2 = new Vector<Double>(); // y coordinates
		output.add(c1);
		output.add(c2);

		Vector<Double> c3 = null; // lowerBOUND coordinates
		Vector<Double> c4 = null; // upperBOUND coordinates
//		Vector[] realizedValues = null;

		// Data extraction : points with (Integer, Double) coordinates
		for (Iterator i = steps.iterator(); i.hasNext();) {
			Step s = (Step) i.next();

			// Consider restriction to one particular group if needed
			GScene stand = s.getScene();
			Collection trees = doFilter(stand);

			int date = stand.getDate();
			Estimate<Matrix, SymmetricMatrix, ?> estimate = getEstimate(stand, trees);

			double mean;
			if (estimate == null) {
				mean = 0d;
			} else {
				mean = estimate.getMean().getValueAt(0, 0);
			}
			c1.add(new Integer(date));
			c2.add(new Double(mean * getCoefHa()));

			if (isStochastic) {
				if (confidenceIntervalsEnabled) {
					if (c3 == null) {
						c3 = new Vector<Double>();
						output.add(c3);
					}
					if (c4 == null) {
						c4 = new Vector<Double>();
						output.add(c4);
					}
					if (estimate != null) {
						ConfidenceInterval ci = estimate.getConfidenceIntervalBounds(1d - alpha);
						if (ci != null) {
							c3.add(new Double(ci.getLowerLimit().getValueAt(0, 0) * getCoefHa()));
							c4.add(new Double(ci.getUpperLimit().getValueAt(0, 0) * getCoefHa()));
						}
					} else {
						c3.add(0d);
						c4.add(0d);
					}
				}
			}
		}

		return output;
	}
	
		
	@Override
	protected Vector<String> extractLabels(Vector values) {
		Vector<String> output = new Vector<String>();
		output.add(MessageID.Mean.toString());

		if (isStochastic) {
			if (confidenceIntervalsEnabled) {
				String confidenceLevel = ((Long) Math.round(getDoubleProperty(MessageID.ProbabilityLevel.toString()) * 100)).toString() + "%";
				output.add(MessageID.Lower.toString().concat(confidenceLevel));
				output.add(MessageID.Upper.toString().concat(confidenceLevel));
			}
		}
		return output;
	}			
			


	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<String> getAxesNames() {
		if (variable.isPerHectareAllowed()) {
			return getAxesNames(isSet("perHectare"));
		} else {
			return getAxesNames(false);
		}
	}

	protected List<String> getAxesNames(boolean perHa) {
		Vector v = new Vector();
		v.add(MessageID.Time.toString());
		if (variable.isPerHectareAllowed()) {
			if (getCoefHa() == 1) {
				v.add(getYAxisLabelName() + "(" + MessageID.Ha.toString());
			} else {
				v.add(getYAxisLabelName() + "(" + getCoefHa() + " " + MessageID.Ha.toString());
			}
		} else {
			v.add(getYAxisLabelName());
		}
		return v;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public int getNY() {
		return curves.size() - 1;
	}

	/**
	 * From DFCurves interface.
	 */
	@Override
	public List<List<String>> getLabels() {
		return labels;
	}

	/**	
	 * From DFCurves interface.
	 */
	@Override
	public List<List<? extends Number>> getCurves() {
		return curves;
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	@Override
	public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof ExtModel)) {return false;}
			ExtModel m = (ExtModel) referent;
			MethodProvider mp = m.getMethodProvider();
			if (!(mp instanceof ExtMethodProvider)) {return false;}					
		} catch (Exception e) {
			Log.println (Log.ERROR, "AbstractDETimeWithCI.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}



}
