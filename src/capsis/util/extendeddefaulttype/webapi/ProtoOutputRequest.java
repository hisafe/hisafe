/*
 * This file is part of the Artemis module for Capsis 4.2.2
 *
 * Authors: M. Fortin - Canadian Forest Service 
 * Copyright (C) 2024 His Majesty the King in right of Canada
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.util.extendeddefaulttype.webapi;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import repicea.simulation.scriptapi.Request;

/**
 * A class to deserialize a JSON entry received from CapsisWebAPI.<p>
 * The ProtoOutputRequest instance can then be passed to ExtScript to 
 * register an output request.
 * @author Mathieu Fortin - January 2024
 */
class ProtoOutputRequest {

	static final String RequestTypeLabel = "requestType"; 
//	static final String StatusClassLabel = "statusClass"; 
//	static final String VariableLabel = "variable"; 
	static final String AggregationPatternsLabel = "aggregationPatterns"; 

	final Request request;
	final LinkedHashMap<String, List<String>> aggregationPatterns;

	/**
	 * Constructor><p>
	 * The constructor deserializes a JSON entry.
	 * @param jsonEntry a Map instance from a JSON string
	 */
	ProtoOutputRequest(Map jsonEntry) {
		if (!jsonEntry.containsKey(RequestTypeLabel)) {
			throw new InvalidParameterException("ProtoOutputRequest: The jsonEntry does not contain key " + RequestTypeLabel + " !");
		} else {
			String requestType = (String) jsonEntry.get(RequestTypeLabel);
			try {
				request = Request.valueOf(requestType);
			} catch (Exception e) {
				throw new InvalidParameterException("ProtoOutputRequest: Request " + requestType + " is not recognized. See the repicea.simulation.scriptapi.Request enum.");
			}
		}
		if (!jsonEntry.containsKey(AggregationPatternsLabel)) {
			aggregationPatterns = null;
		} else {
			aggregationPatterns = new LinkedHashMap<String, List<String>>();
			Map innerMap = (Map) jsonEntry.get(AggregationPatternsLabel);
			for (Object k : innerMap.keySet()) {
				Object v = innerMap.get(k);
				if (!v.getClass().isArray()) {
					throw new InvalidParameterException("ProtoOutputRequest: The value of the aggregation patterns map should be an array!");
				} else {
					List<String> speciesList = new ArrayList<String>();
					for (Object o : (Object[]) v) {
						speciesList.add(o.toString());
					}
					aggregationPatterns.put(k.toString(), speciesList);
				}
			}
		}
	}

}
