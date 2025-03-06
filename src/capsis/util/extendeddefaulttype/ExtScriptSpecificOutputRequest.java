/*
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 *
 * Authors: M. Fortin and J-F Lavoie - Canadian Forest Service 
 * Copyright (C) 2020-21 Her Majesty the Queen in right of Canada
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
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
package capsis.util.extendeddefaulttype;

import java.util.LinkedHashMap;
import java.util.List;

import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.scriptapi.Request;
import repicea.stats.estimates.Estimate;

/**
 * A class for extracting the result from a simulation. The results can be aggregated 
 * according to the status class and the species groups of a ExtScriptOutputRequest instance.
 * @author Mathieu Fortin - March 2021
 */
class ExtScriptSpecificOutputRequest extends ExtScriptAbstractOutputRequest {
	
	protected final LinkedHashMap<String, List<String>> aggregationPatterns;
	
	/**
	 * Constructor. <br>
	 * <br>
	 * The LinkedHashMap instance contains the aggregation patterns. Those drive the
	 * grouping of the species. The keys of the map are the group names and the list
	 * of strings in values are the species contained in each group. Passing an empty
	 * map results in the grouping of all species into a single unnamed group.
	 * 
	 * @param statusClass
	 * @param aggregationPatterns
	 */
	public ExtScriptSpecificOutputRequest(Request request, LinkedHashMap<String, List<String>> aggregationPatterns) {
		super(request);		
		this.aggregationPatterns = new LinkedHashMap<String, List<String>>();
		this.aggregationPatterns.putAll(aggregationPatterns);
	}
	
	@Override
	protected ExtScriptSpecificOutputRequest clone() {
		return new ExtScriptSpecificOutputRequest(getRequest(), aggregationPatterns);
	}
	
	@Override
	public int getNumberOfPatterns() {
		if (aggregationPatterns.isEmpty()) {
			return 1;
		} else {
			return aggregationPatterns.size();
		}
	}
	
	@Override
	protected void addEstimateForThisStand(ExtCompositeStand stand, List<ExtCompositeStand> compositeStands, ExtMethodProvider methodProvider) {
		makeSureEstimateListIsIntantiated(compositeStands);
		Estimate<Matrix, SymmetricMatrix, ?> estimate = methodProvider.getEstimatePerHaForThisVariable(stand, stand.getTrees(request.getStatusClass()), request.getVariableForEstimation(), aggregationPatterns);
		int index = compositeStands.indexOf(stand);
		addEstimateInList(index, estimate);
	}

}
