/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2021 Her Majesty the Queen in right of Canada 
 * 
 * Authors: M. Fortin, Canadian Wood Fibre Centre 
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
package capsis.util.extendeddefaulttype.disturbances;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import capsis.defaulttype.Tree;
import repicea.simulation.disturbances.DisturbanceOccurrences;
import repicea.simulation.thinners.REpiceaTreatmentDefinition;

public class ThinningDisturbanceOccurrences extends DisturbanceOccurrences {

	final Double probability;
	private REpiceaTreatmentDefinition treatmentDefinition;
	Map<Tree, Object> treeResults;

	protected ThinningDisturbanceOccurrences(DisturbanceParameters parms, int dateYr) {
		super(parms, dateYr);	
		this.probability = null;
		treeResults = new HashMap<Tree, Object>();
	}

	protected ThinningDisturbanceOccurrences(DisturbanceParameters parms, int dateYr, double probability) {
		super(parms, dateYr);	
		this.probability = probability;
		treeResults = new HashMap<Tree, Object>();
	}

	/**
	 * Set the treatment description.
	 * @param desc 
	 */
	public void setTreatmentDefinition(REpiceaTreatmentDefinition desc) {
		this.treatmentDefinition = desc;
	}
	
	/**
	 * Provide the description of the treatment if any.
	 * @return an instance or null if there is no description at all.
	 */
	public REpiceaTreatmentDefinition getTreatmentDefinition() {return treatmentDefinition;}
	
	
	protected void put(Tree t, Object result) {
		treeResults.put(t, result);
	}
	
	/**
	 * Filter the entries to keep only those where the value is equal to true.
	 */
	public void filterForValuesEqualToTrue() {
		Map<Tree, Object> tmpMap = treeResults.entrySet()
				.stream()
				.filter(map -> map.getValue() instanceof Boolean && ((Boolean) map.getValue()))	// we keep only the entries with values set to true
				.collect(Collectors.toMap(map -> map.getKey(),  map -> map.getValue()));
		treeResults = tmpMap;
	}

	/**
	 * Check if a tree is contained in the result map.
	 * @param t the Tree instance
	 * @return a boolean
	 */
	public boolean containsTree(Tree t) {
		return treeResults.containsKey(t);
	}
	
	/**
	 * Return the value for a particular Tree instance.
	 * @param t the Tree instance
	 * @return a boolean, a double or null if the Tree instance is not found in the result map.
	 */
	public Object getTreeResult(Tree t) {
		if (containsTree(t)) {
			return treeResults.get(t);
		} else {
			return null;
		}
	}

	/**
	 * Return true if the result map is empty.
	 * @return a boolean
	 */
	public boolean isEmpty() {
		return treeResults.isEmpty();
	}

	/**
	 * Return the list of Tree instances contained in the result map.
	 * @return a List of Tree instances.
	 */
	public List<Tree> getTrees() {
		List<Tree> trees = new ArrayList<Tree>();
		trees.addAll(treeResults.keySet());
		return trees;
	}
	
}
