/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2017 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin
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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;

/**
 * The DisturbanceRecorder class records the occurrence of events for a particular annual probability and a particular year. 
 * In stochastic simulation, this class is used to make sure that two parallel simulations will face the same events when the
 * annual probability is the same.  
 * @author Mathieu Fortin - March 2017
 */
@Deprecated
class NaturalDisturbanceRecorder implements Serializable {

	private final Random randomGenerator = new Random();
	
	// the first key is required in case the user changes the recurrence 
	private final Map<String, Map<Integer, Boolean>> eventsMap; // first key is the natural disturbance parameters, second key is the year

	private final int initialDateYrs;
	
	private String currentBusinessCard;
	
	/**
	 * Constructor.
	 */
	public NaturalDisturbanceRecorder(int initialDateYrs) {
		eventsMap = new HashMap<String, Map<Integer, Boolean>>();
		this.initialDateYrs = initialDateYrs;
	}

	/**
	 * This method returns the occurrence of a particular disturbance event. If the occurrence has already been 
	 * recorded for a particular combination of year-annual probability, then this recorder occurrence is returned. 
	 * Otherwise, an occurrence is randomly generated and then recorded for future simulations.  
	 * @param dateYr the date 
	 * @param annualProbability the annual probability
 	 * @return a boolean
	 */
	public boolean getDisturbanceOccurrence(NaturalDisturbanceParameters parameters, int dateYr, double annualProbability) {
		String businessCard = parameters.getBusinessCard();
		if (!eventsMap.containsKey(businessCard)) {
			eventsMap.put(businessCard, new TreeMap<Integer, Boolean>());
		}
		Map<Integer, Boolean> innerMap = eventsMap.get(businessCard);
		if (!innerMap.containsKey(dateYr)) {
			innerMap.put(dateYr, randomGenerator.nextDouble() < annualProbability);
		}
		return innerMap.get(dateYr);
	}

	protected void setCurrentBusinessCard(NaturalDisturbanceParameters naturalDisturbanceParameters) {
		this.currentBusinessCard = naturalDisturbanceParameters.getBusinessCard();
		
	}

	private Integer findLatestDate(Map<Integer,Boolean> innerMap, int currentDateYr) {
		List<Integer> dates = new ArrayList<Integer>(innerMap.keySet());
		for (int i = dates.size() - 1; i >= 0; i--) {
			int date = dates.get(i);
			if (date < currentDateYr && innerMap.get(date)) {
				return currentDateYr - date;
			}
		}
		return null;
	}
	

	
}
