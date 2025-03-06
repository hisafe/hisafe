/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Authors: M. Fortin, Canadian Forest Service 
 * Copyright (C) 2019 Her Majesty the Queen in right of Canada 
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
package capsis.util.extendeddefaulttype.disturbances;

import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtCompositeStand.PredictorID;
import capsis.util.extendeddefaulttype.ExtModel;
import capsis.util.methodprovider.DdomProvider;
import repicea.simulation.thinners.REpiceaThinner;
import repicea.simulation.thinners.REpiceaTreatmentDefinition;

/**
 * The ModelBasedThinningDisturbanceParameters class handles the thinning through a harvesting
 * model and not through a rule.
 * @author Mathieu Fortin - March 2019
 */
public class ModelBasedThinningDisturbanceParameters extends ThinningDisturbanceParameters {

	private final Map<String, Object> additionalStandParameters;
	
	/**
	 * Constructor for random disturbance mode.
	 * @param recurrence the recurrence of the thinning
	 */
	public ModelBasedThinningDisturbanceParameters(double recurrence) {
		super(DisturbanceMode.Random, recurrence);
		this.additionalStandParameters = new HashMap<String, Object>();
	}

	/**
	 * Constructor for other modes without additional parameters.
	 * @param mode
	 */
	public ModelBasedThinningDisturbanceParameters(DisturbanceMode mode) {
		this(mode, null);
	}

	/**
	 * Constructor for other modes with additional parameters.
	 * @param mode
	 */
	public ModelBasedThinningDisturbanceParameters(DisturbanceMode mode, Map<String, Object> additionalStandParameters) {
		super(mode, -1);
		this.additionalStandParameters = new HashMap<String, Object>();
		if (additionalStandParameters != null) {
			this.additionalStandParameters.putAll(additionalStandParameters);
		}
	}

	/**
	 * Add an entry to the map of additional stand-level harvest parameters.
	 * @param parmId the id of the parameter (see the static variables in this class and the DisturbanceParameters class 
	 * @param parm the value of the parameter
	 */
	public void addStandParameter(String parmId, Object parm) {
		additionalStandParameters.put(parmId, parm);
	}

	/**
	 * Return a stand-level harvest parameter.
	 * @param parmId the id of the parameter (see the static variables in this class and the DisturbanceParameters class 
	 * @return an Object or null if there is no value associated with this parameter id
	 */
	public Object getStandParameter(String parmId) {
		return additionalStandParameters.get(parmId);
	}

	/**
	 * Check if this parameter id exists in the map of stand-level harvest parameters.
	 * @param parmId the id of the parameter (see the static variables in this class and the DisturbanceParameters class 
	 * @return a boolean
	 */
	public boolean containsThisParameter(String parmId) {
		return getStandParameter(parmId) != null;
	}
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public ThinningDisturbanceOccurrences isThereADisturbance(ExtCompositeStand compositeStand, TreeList stand, int stepLengthYrs, Map<String, Object> parms) {
		if (getMode() == DisturbanceMode.FullModelBased) {	// then a stand-level harvest model must predict whether the harvest occurs or not
			Map<String, Object> standLevelParms = new HashMap<String, Object>();
			if (parms != null && !parms.isEmpty()) {
				standLevelParms.putAll(parms);
			} 
			
			if (!additionalStandParameters.isEmpty()) {	
				standLevelParms.putAll(additionalStandParameters);
			}
			
			REpiceaThinner standModel = (REpiceaThinner) compositeStand.getPredictor(PredictorID.STAND_HARVESTING);
			
			Object thinningStandEvent = standModel.predictEvent(stand, null, standLevelParms); 
			if (thinningStandEvent instanceof Double) {
				return new ThinningDisturbanceOccurrences(this, compositeStand.getDateYr(), (Double) thinningStandEvent);	// in deterministic mode, the harvest occurs between the growth steps.
			} else {	// means it's a boolean
				if (((Boolean) thinningStandEvent)) {
					ThinningDisturbanceOccurrences occurrences = new ThinningDisturbanceOccurrences(this, getInternalOccurrenceDateYr(compositeStand, stepLengthYrs));
					return occurrences;  
				} else {
					return null;
				}
			}
		} else if (getMode() == DisturbanceMode.Random) {
			double annualProbability = 1d / recurrenceYrs;		// TODO perhaps implement the exponential survival model here
			double probabilityOverInterval = 1 - Math.pow(1-annualProbability, stepLengthYrs);
			if (ExtModel.RANDOM.nextDouble() < probabilityOverInterval) {
				ThinningDisturbanceOccurrences occurrences = new ThinningDisturbanceOccurrences(this, getInternalOccurrenceDateYr(compositeStand, stepLengthYrs));
				return occurrences;
			} else {
				return null;
			}
		} else if (getMode() == DisturbanceMode.NextStep) {
				ThinningDisturbanceOccurrences occurrences = new ThinningDisturbanceOccurrences(this, getInternalOccurrenceDateYr(compositeStand, stepLengthYrs));
				return occurrences;
		} else {
			return null;
		}
	}


	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	public ThinningDisturbanceOccurrences markTrees(ExtCompositeStand compositeStand,
			TreeList stand, 
			Collection<Tree> trees, 
			int yrs, 
			DdomProvider mp, 
			Map<String, Object> plotLevelParms, 
			Map<String, Object> treeLevelParms) {
		ThinningDisturbanceOccurrences standEvent = isThereADisturbance(compositeStand, stand, yrs, plotLevelParms);
		if (standEvent == null) {
			return null;
		} else {
			boolean finalCut = isFinalCut(stand, trees, mp);
			for (Tree tree : trees) {
				if (finalCut) {
					standEvent.put(tree, true);
				} else {
					REpiceaThinner treeLevelModel = (REpiceaThinner) compositeStand.getPredictor(PredictorID.TREE_HARVESTING);
					REpiceaTreatmentDefinition treatmentDefinition = treeLevelModel.getTreatmentDefinitionForThisHarvestedStand(stand);
					if (treatmentDefinition != null) {
						standEvent.setTreatmentDefinition(treatmentDefinition);
					}
					Object result = treeLevelModel.predictEvent(stand, tree, treeLevelParms); // parms may contain the excluded group in case of validation
					if (((ThinningDisturbanceOccurrences) standEvent).probability != null) { // it means we are working in deterministic mode
						result = ((ThinningDisturbanceOccurrences) standEvent).probability * (Double) result;
					}
					standEvent.put(tree, result);
				}
			}
			return standEvent;
		}
	}

}
