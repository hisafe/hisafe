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

import java.security.InvalidParameterException;
import java.util.HashMap;
import java.util.Map;

import canforservutility.predictor.disturbances.SimpleRecurrenceBasedDisturbancePredictor.SimpleRecurrenceBasedDisturbanceParameters;
import capsis.defaulttype.TreeList;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtPlot;
import repicea.simulation.MonteCarloSimulationCompliantObject;
import repicea.simulation.REpiceaBinaryEventPredictor;
import repicea.simulation.disturbances.DisturbanceOccurrences;
import repicea.simulation.disturbances.DisturbanceParameter;

/**
 * This class handles the parameters of a natural disturbances.
 * @author Mathieu Fortin - April 2019
 */
public class NaturalDisturbanceParameters extends DisturbanceParameters {

	
	private double variance = 0;
	
	/**
	 * Constructor for random mode.
	 * @param type
	 * @param recurrenceYrs
	 */
	public NaturalDisturbanceParameters(DisturbanceType type, double recurrenceYrs) {
		this(type, recurrenceYrs, 0d);
	}

	/**
	 * Enhanced constructor for random mode.
	 * @param type
	 * @param recurrenceYrs
	 */
	public NaturalDisturbanceParameters(DisturbanceType type, double recurrenceYrs, double variance) {
		super(type, DisturbanceMode.Random, recurrenceYrs);
		this.variance = variance;
	}

	/**
	 * Constructor for other modes.
	 * @param type
	 * @param mode
	 */
	public NaturalDisturbanceParameters(DisturbanceType type, DisturbanceMode mode) {
		super(type, mode, -1);
	}


	
	
	/**
	 * This method returns an object that defines whether or not there is a disturbance. In deterministic mode, that object is a double while it is a boolean 
	 * in most stochastic implementations.
	 * @param yrs the length of the growth interval
	 * @param parms eventual parameters
	 * @return an Object either a double or a boolean
	 */
	@Override
	public DisturbanceOccurrences isThereADisturbance(ExtCompositeStand compositeStand, TreeList stand, int stepLengthYrs, Map<String, Object> parms) {
		if (getMode() == DisturbanceMode.Random || getMode() == DisturbanceMode.FullModelBased) {
			Integer initialYear = (Integer) parms.get(DisturbanceParameter.ParmCurrentDateYr);
			if (initialYear == null) {
				throw new InvalidParameterException("The initial year is missing!");
			}
			
			Map<String,Object> parmsCopy = new HashMap<String,Object>();
			parmsCopy.putAll(parms);
			DisturbanceOccurrences occurrences = new DisturbanceOccurrences(this);
			parms.put(DisturbanceParameter.ParmDisturbanceOccurrences, occurrences);
			
			REpiceaBinaryEventPredictor predictor = compositeStand.getNaturalDisturbanceOccurrencePredictor(getDisturbanceType(), getMode());
			for (int dateYr = initialYear + 1; dateYr <= initialYear + stepLengthYrs; dateYr++) {
				parmsCopy.put(DisturbanceParameter.ParmCurrentDateYr, dateYr);
				boolean occurred;
				if (getMode() == DisturbanceMode.Random) {
					parmsCopy.put(DisturbanceParameter.ParmSimpleRecurrenceBasedParameters, new SimpleRecurrenceBasedDisturbanceParameters(recurrenceYrs, variance));
					occurred = (Boolean) predictor.predictEvent(stand, null, parmsCopy);
				} else {
					occurred = (Boolean) predictor.predictEvent(stand, null, parmsCopy);
				}
				if (occurred) {
					occurrences.addOccurrenceDateYr(dateYr);
				}
			}
			if (occurrences.isThereAnyOccurrence()) { 
				return occurrences;
			} else {
				return null;
			}
		} else {
			return super.isThereADisturbance(compositeStand, stand, stepLengthYrs, parms);
		}
	}

//	private Object findParameter(Object[] parms, Class<?> clazz) {
//		if (parms != null && parms.length > 0) {
//			for (int i = 0; i < parms.length; i++) {
//				if (clazz.isInstance(parms[i])) {
//					return parms[i];
//				}
//			}
//		}
//		return null;
//	}

	protected String getBusinessCard() {
		String basicBusinessCard = getDisturbanceType().name() + getMode().name();
		if (getMode() == DisturbanceMode.Random) {
			return basicBusinessCard + recurrenceYrs;
		} else {
			return basicBusinessCard;
		}
	}
	
	@Override
	public boolean isThisPlotAffected(MonteCarloSimulationCompliantObject plot) {
		ExtCompositeStand compositeStand = ((ExtPlot) plot).getStratum();
		REpiceaBinaryEventPredictor vulnerabilityPredictor = compositeStand.getNaturalDisturbanceConditionalVulnerability(getDisturbanceType(), getMode());
		if (vulnerabilityPredictor != null) {	// could be the case of a windstorm or eventually a spruce budworm outbreak
			boolean isVulnerable = (Boolean) vulnerabilityPredictor.predictEvent(plot, null);
			return isVulnerable;
		} else {
			return super.isThisPlotAffected(plot);	// return true otherwise
		}
	}
	
	
}
