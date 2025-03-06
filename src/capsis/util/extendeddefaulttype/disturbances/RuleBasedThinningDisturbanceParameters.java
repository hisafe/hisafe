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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.disturbances.ThinningDisturbanceParametersPanel.BoundaryVariable;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.VProvider;
import repicea.simulation.covariateproviders.plotlevel.AreaHaProvider;
import repicea.simulation.thinners.REpiceaThinner;

/**
 * A class for thinning based on a particular rule. For instance, it could be to maintain
 * the basal area between 16 and 24 m2/ha. The harvest is triggered when the upper bound is 
 * crossed and trees are removed in descending order of tree-level probability of
 * harvest as predicted by the tree-level harvest module. The trees are removed until the
 * lower bound is crossed.
 * @author Mathieu Fortin - March 2019
 */
public class RuleBasedThinningDisturbanceParameters extends ThinningDisturbanceParameters {
	
	
	private final double minimumValue;
	private final double maximumValue;
	
	private final BoundaryVariable boundaryVariable;
	private final MethodProvider mp;
	
	private REpiceaThinner treeThinningPredictor;
	
	private static class TreeWrapper implements Comparable<TreeWrapper> {

		private final Tree tree;
		private final double harvestProbability;

		private TreeWrapper(Tree tree, double harvestProbability) {
			this.tree = tree;
			this.harvestProbability = harvestProbability;
		}
		
		@Override
		public int compareTo(TreeWrapper arg0) {
			if (harvestProbability < arg0.harvestProbability) {
				return -1;
			} else if (harvestProbability == arg0.harvestProbability) {
				return 0;
			} else {
				return 1;
			}
		}
		
	}
	
	/**
	 * General constructor.
	 * @param boundaryVariable the variable to be checked
	 * @param minimumValue the lower bound (after harvesting)
	 * @param maximumValue the upper bound (before harvesting)
	 * @param mp a MethodProvider instance that implements the appropriate interface for the boundary variable
	 */
	public RuleBasedThinningDisturbanceParameters(BoundaryVariable boundaryVariable, 
			double minimumValue, 
			double maximumValue,
			MethodProvider mp) {
		super(DisturbanceMode.RuleBased, -1);
		this.boundaryVariable = boundaryVariable;
		this.minimumValue = minimumValue;
		this.maximumValue = maximumValue;
		this.mp = mp;
	}

	
	@Override
	public ThinningDisturbanceOccurrences isThereADisturbance(ExtCompositeStand compositeStand, TreeList stand, int stepLengthYrs, Map<String, Object> parms) {
		Collection<? extends Tree> trees = stand.getTrees();
		double areaFactor = 1d / ((AreaHaProvider) stand).getAreaHa();
		double value = getValue(stand, trees, areaFactor);
		if (value > maximumValue) {
			ThinningDisturbanceOccurrences occurrences = new ThinningDisturbanceOccurrences(this, compositeStand.getDateYr()); // harvest is not stochastic here
			return occurrences;	// in this particular case the thinning is carried out immediately
		} else {
			return null;
		}
	}


	@Override
	public ThinningDisturbanceOccurrences markTrees(ExtCompositeStand compositeStand,
			TreeList stand, 
			Collection<Tree> trees, 
			int yrs, 
			DdomProvider mp, 
			Map<String, Object> plotLevelParms, 
			Map<String, Object> treeLevelParms) {
		ThinningDisturbanceOccurrences standEvent = isThereADisturbance(compositeStand, stand, yrs, null);
		if (standEvent == null) {
			return null;
		} else {
			double areaFactor = 1d / ((AreaHaProvider) stand).getAreaHa();
			List<TreeWrapper> copyList = new ArrayList<TreeWrapper>();
			double prediction;
			for (Tree tree : trees) {
				if (treeThinningPredictor != null) {
					prediction = treeThinningPredictor.predictEventProbability(stand, tree);
				} else {
					prediction = 0d;
				}
				copyList.add(new TreeWrapper(tree, prediction));
			}
			
			if (treeThinningPredictor != null) {		// sort in descending order of predicted probabilities
				Collections.sort(copyList, Collections.reverseOrder());
			} else {
				Collections.shuffle(copyList);		// at random
			}
			
			List<Tree> orderedCopyList = new ArrayList<Tree>();
			for (TreeWrapper wrapper : copyList) {
				orderedCopyList.add(wrapper.tree);
			}
			
			double currentValue;
	 		boolean finalCut = isFinalCut(stand, trees, mp);
			while (!orderedCopyList.isEmpty()) {
				if (finalCut) {
					standEvent.put(orderedCopyList.remove(0), true);
				} else {
					currentValue = getValue(stand, orderedCopyList, areaFactor);
					if (currentValue <= minimumValue) {
						standEvent.put(orderedCopyList.remove(0), false);
					} else {
						standEvent.put(orderedCopyList.remove(0), true);
					}
				}
			}
			return standEvent;
			
		}
	}

	
	private double getValue(GScene stand, Collection<? extends Tree> trees, double areaFactor) {
		switch(boundaryVariable) {
//		case N:
//			return ((NProvider) mp).getN(stand, trees) * areaFactor;
		case G:
			return ((GProvider) mp).getG(stand, trees) * areaFactor;
		case V:
			return ((VProvider) mp).getV(stand, trees) * areaFactor;
		}
		return -1d;
	}
	

	/**
	 * This method sets a model that makes it possible to rank the trees according to their probability of being harvested. This
	 * way, the selection is not made at random.
	 * @param logisticModelBasedSimulator a LogisticModelBasedSimulator instance
	 */
	public void setScorer(REpiceaThinner logisticModelBasedSimulator) {
		this.treeThinningPredictor = logisticModelBasedSimulator;
	}
	
}
