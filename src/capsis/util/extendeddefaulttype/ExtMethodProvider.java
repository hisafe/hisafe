/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * Copyright (C) 2021 Her Majesty the Queen in right of Canada
 * 
 * Authors: M. Fortin, Canadian Wood Fibre Centre
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
package capsis.util.extendeddefaulttype;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import java.util.stream.Collectors;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.methodprovider.AreaProportionByTreatmentEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.AverageProductionProvider;
import capsis.util.extendeddefaulttype.methodprovider.AverageRingWidthCmProvider;
import capsis.util.extendeddefaulttype.methodprovider.DDomEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.GEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.HDomEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.NEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.PeriodicAnnualIncrementEstimatesProvider;
import capsis.util.extendeddefaulttype.methodprovider.VEstimateProviderWithName;
import capsis.util.methodprovider.DdomProvider;
import capsis.util.methodprovider.DgProvider;
import capsis.util.methodprovider.GProvider;
import capsis.util.methodprovider.HdomProvider;
import capsis.util.methodprovider.NProvider;
import capsis.util.methodprovider.TotalAboveGroundVolumeProvider;
import capsis.util.methodprovider.VMerchantProviderWithName;
import capsis.util.methodprovider.VProviderWithName;
import jeeb.lib.util.Log;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.allometrycalculator.AllometryCalculator;
import repicea.simulation.covariateproviders.MethodProviderEnum.VariableForEstimation;
import repicea.simulation.covariateproviders.plotlevel.AreaHaProvider;
import repicea.simulation.covariateproviders.plotlevel.StochasticInformationProvider;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.simulation.thinners.REpiceaTreatmentDefinition;
import repicea.stats.estimates.BootstrapHybridPointEstimate;
import repicea.stats.estimates.Estimate;
import repicea.stats.estimates.MonteCarloEstimate;
import repicea.stats.estimates.PopulationMeanEstimate;
import repicea.stats.sampling.PopulationUnitWithEqualInclusionProbability;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * The MethodProvider implementation for the extended default models.
 * @author Mathieu Fortin 
 */
public class ExtMethodProvider implements GProvider, 
												NProvider, 
												VProviderWithName,
												VMerchantProviderWithName,
												HdomProvider,
												DgProvider,
												DdomProvider,
												TotalAboveGroundVolumeProvider,
												VEstimateProviderWithName, 
												NEstimateProvider,
												GEstimateProvider,
												PeriodicAnnualIncrementEstimatesProvider,
												AverageRingWidthCmProvider,
												HDomEstimateProvider,
												DDomEstimateProvider,
												AverageProductionProvider,
												AreaProportionByTreatmentEstimateProvider {

	private static enum MessageID implements TextableEnum {
		CommercialVolume("Merchantable", "Marchand");

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString (this, englishText, frenchText);
		}

		@Override
		public String toString() {
			return REpiceaTranslator.getString(this); 
		}

	}

//	public static enum VariableForEstimation {N, G, V}
	
	private static AllometryCalculator ac;

	@SuppressWarnings("rawtypes")
	public double getDg(GScene compositeStand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

			double G = getG(compositeStand, trees);
			double N = getN(compositeStand, trees);
			double Dg = Math.sqrt(G *40000 / (N*Math.PI));
			return Dg;
		} catch (Exception e) {
			Log.println (Log.ERROR, "QuebecMRNFMethodProvider.getDg ()", 
					"Error while computing Dg", e);
			return -1d;
		}
	}


	/*	
	 * G: basal area (m2). Dbh must be in cm. 
	 * Square of dbh are weighted by the number of stems(non-Javadoc)
	 * @see capsis.util.methodprovider.DgProvider#getDg(capsis.kernel.GScene, java.util.Collection)
	 */
	@SuppressWarnings({ "rawtypes" })
	public double getG(GScene compositeStand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

			int numberIterationMC = 1;
			if (compositeStand instanceof StochasticInformationProvider) {
				numberIterationMC = ((StochasticInformationProvider) compositeStand).getRealizationIds().size(); //getNumberRealizations();
			}
			ExtTree t;

			double basalArea = 0;
			for (Object tree : trees) {
				t = (ExtTree) tree;
				if (t.getNumber() > 0) {
					basalArea += t.getStemBasalAreaM2() * t.getNumber() * t.getPlotWeight();			// plot weight added mf2009-08-18
				}
			}
			return basalArea / numberIterationMC;
		} catch (Exception e) {
			Log.println(Log.ERROR, "ExtendedMethodProvider.getG ()", "Error while computing basal area", e);
			return -1d;
		}
	}

	/*	
	 * N: number of trees  
	 */
	@SuppressWarnings({ "rawtypes" })
	public double getN(GScene compositeStand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

			int numberIterationMC = 1;
			if (compositeStand instanceof StochasticInformationProvider) {
				numberIterationMC = ((StochasticInformationProvider) compositeStand).getRealizationIds().size();
			}
			ExtTree t;

			double NumberofStems = 0;
			for (Object tree : trees) {
				t = (ExtTree) tree;
				NumberofStems += t.getNumber() * t.getPlotWeight();				// plot weight added mf2009-08-18
			}
			return NumberofStems / numberIterationMC;
		} catch (Exception e) {
			Log.println (Log.ERROR, "ExtendedMethodProvider.getN ()", "Error while computing number of stems", e);
			return -1d;
		}
	}

	/*	
	 * V: Volume (m3).  
	 * Merchantable volume are weighted by the number of stems (non-Javadoc)
	 */
	@SuppressWarnings({"rawtypes" })
	public double getV(GScene compositeStand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

			int numberIterationMC = 1;
			if (compositeStand instanceof StochasticInformationProvider) {
				numberIterationMC = ((StochasticInformationProvider) compositeStand).getRealizationIds().size();
			}
			ExtTree t;

			double volume = 0;
			for (Object tree : trees) {
				t = (ExtTree) tree;
				if (t.getNumber() > 0) {
					volume += t.getCommercialVolumeM3() * t.getNumber() * t.getPlotWeight();				
				}
			}
			return volume / numberIterationMC;
		} catch (Exception e) {
			Log.println (Log.ERROR, "QuebecMRNFMethodProvider.getV ()", 
					"Error while computing plot volume", e);
			return -1d;
		}
	}

	
	@Override
	public final Estimate<Matrix, SymmetricMatrix, ?> getVolumePerHaEstimate(GScene compositeStand, Collection trees) {
		Estimate<Matrix, SymmetricMatrix, ?> estimate = getEstimatePerHaForThisVariable(compositeStand, trees, VariableForEstimation.V);
		return estimate;
	}

	
	protected final Estimate<Matrix, SymmetricMatrix, ?> getEstimatePerHaForThisVariable(GScene compositeStand, Collection trees, VariableForEstimation v) {
		return getEstimatePerHaForThisVariable(compositeStand, trees, v, null);
	}

	/**
	 * Instantiate a results map.
	 * @param extCompositeStand the composite stand in order to retrieve the realization ids and the plot ids
	 * @param outputSize the size of the output
	 * @return a Map with the realization ids (first key), the plot ids (the second key) and empty matrices (value)
	 */
	private Map<Integer, Map<String, Matrix>> instantiateResultsMap(ExtCompositeStand extCompositeStand, int outputSize) {
		Map<Integer, Map<String, Matrix>> results = new TreeMap<Integer, Map<String, Matrix>>();

		List<Integer> realizationIds = new ArrayList<Integer>();
		for (ExtPlotSample ps : extCompositeStand.getPlotSamples()) {
			realizationIds.add(ps.getMonteCarloRealizationId());
		}
		
		Collection<String> plotIds  = extCompositeStand.getExtPlotSample().getPlotIds();
		
		for (Integer i : realizationIds) {
			Map<String, Matrix> valueMap = new TreeMap<String, Matrix>();		
			for (String plotID : plotIds) {
				valueMap.put(plotID, new Matrix(outputSize, 1));	// a column vector with nb rows equal to the nb of species or 1 if the speciesGroupTagNames is null or empty
			}
			results.put(i, valueMap);
		}
		return results;
	}
	
	/**
	 * Compute the sum of a particular tree variable for groups of trees in an ExtCompositeStand instance. <p>
	 * It first produces a Map of Map instance whose first keys are the realization ids and the second keys are
	 * the plot ids. The values are Matrix instances whose elements are all set to 0 initially. The number of 
	 * elements in these matrices depends on the number of aggregation patterns.<p>
	 * Once this map of maps has been generated, the method sums up a particular tree variables within groups 
	 * that correspond to aggregation patterns. If the aggregationPatterns parameter is null, 
	 * then all the species are aggregated in a single group.
	 * @param extCompositeStand an ExtCompositeStand instance
	 * @param aggregationPatterns a LinkedHashMap of String and List of String. The key is the name of the pattern,
	 * whereas the list of strings include all the species code that are included in this aggregation pattern.
	 * @param trees a List of ExtTree instances
	 * @param v a VariableForEstimation enum 
	 * @return a Map with realization id, plot id and prediction as values
	 * @see VariableForEstimation
	 */
	private Map<Integer, Map<String, Matrix>> compileTreesForThisMethod(ExtCompositeStand extCompositeStand,
			LinkedHashMap<String, List<String>> aggregationPatterns,
			List<ExtTree> trees,
			VariableForEstimation v) {
		// reformatting the aggregationPatterns for optimization
		Map<String, List<Integer>> indexMap = null;
		int outputSize = 1;
		if (aggregationPatterns != null && !aggregationPatterns.isEmpty()) {
			outputSize = aggregationPatterns.size();
			indexMap = new HashMap<String, List<Integer>>();
			List<String> groupList = new ArrayList<String>();
			groupList.addAll(aggregationPatterns.keySet());
			for (int i = 0; i < groupList.size(); i++) {
				for (String speciesName : aggregationPatterns.get(groupList.get(i))) {
					if (!indexMap.containsKey(speciesName)) {
						indexMap.put(speciesName, new ArrayList<Integer>());
					}
					indexMap.get(speciesName).add(i);
				}
			}
		} 

		List<Integer> defaultIndexList = new ArrayList<Integer>();
		defaultIndexList.add(0);
		
		Map<Integer, Map<String, Matrix>> results = instantiateResultsMap(extCompositeStand, outputSize);
		
		double expansionFactor;
		
		for (ExtTree t : trees) {
			int monteCarloRealizationID = t.getMonteCarloRealizationId();
			ExtPlot plot = (ExtPlot) t.getScene();

			List<Integer> indexList;
			expansionFactor = 1d / plot.getAreaHa();
			if (t.getNumber() > 0) {
				Matrix oMat = results.get(monteCarloRealizationID).get(plot.getId());
				if (indexMap == null) {
					indexList = defaultIndexList;
				} else {
					indexList = indexMap.get(t.getSpecies().getName());
				}
				double valueForThisTree = getAppropriateMethod(t, v) * t.getNumber() * t.getPlotWeight() * expansionFactor;
				if (indexList != null) { // might be null if the aggregation patterns do not include all the species
					for (int index : indexList) {
						oMat.setValueAt(index, 0, oMat.getValueAt(index, 0) + valueForThisTree);
					}
				}
			}
		}
		return results;	
	}

	
	private Map<Integer, Map<String, Matrix>> compileIncrementForTheseTrees(ExtCompositeStand extCompositeStand,
			Map<ExtTree, ExtTree> treeMatch,
			VariableForEstimation v) {
		Map<Integer, Map<String, Matrix>> results = instantiateResultsMap(extCompositeStand, 1);
		
		double expansionFactor;
		
		for (ExtTree t : treeMatch.keySet()) {
			int monteCarloRealizationID = t.getMonteCarloRealizationId();
			ExtPlot plot = (ExtPlot) t.getScene();

			expansionFactor = 1d / plot.getAreaHa();
			if (t.getNumber() > 0) {
				Matrix oMat = results.get(monteCarloRealizationID).get(plot.getId());
				double sizeNowForThisTree = getAppropriateMethod(t, v) * t.getNumber() * t.getPlotWeight() * expansionFactor;
				double sizeBeforeForThisTree = getAppropriateMethod(treeMatch.get(t), v) * t.getNumber() * t.getPlotWeight() * expansionFactor;
				double increment = sizeNowForThisTree - sizeBeforeForThisTree;
				oMat.setValueAt(0, 0, oMat.getValueAt(0, 0) + increment);
			}
		}
		return results;	
	}

	

	/**
	 * Compute a MonteCarlo or a BootstrapHybridPointEstimate estimate from a results map. The former is selected
	 * id there is only one plot in the results map.
	 * @param results a map with the realization (first key), the plot id (second key) and the result (value)
	 * @return an Estimate
	 */
	private Estimate<Matrix, SymmetricMatrix, ?> produceEstimate(Map<Integer, Map<String, Matrix>> results) {
		Estimate<Matrix, SymmetricMatrix, ?> estimate; 
		if (results.values().iterator().next().size() > 1) {
			estimate = new BootstrapHybridPointEstimate();
		} else {
			estimate = new MonteCarloEstimate();
		}
		for (Integer realizationId : results.keySet()) {
			PopulationMeanEstimate sampleEstimate = new PopulationMeanEstimate();
			Map<String, Matrix> oMap = results.get(realizationId);
			for (String plotId : oMap.keySet()) {
				Matrix mat = oMap.get(plotId);
				// TODO MF20230911 Here could be the right spot to include unequal inclusion probability plot as in a stratified design for instance
				sampleEstimate.addObservation(new PopulationUnitWithEqualInclusionProbability(plotId, mat));
			}
			if (estimate instanceof BootstrapHybridPointEstimate) {
				((BootstrapHybridPointEstimate) estimate).addPointEstimate(sampleEstimate);
			} else {
				((MonteCarloEstimate) estimate).addRealization(sampleEstimate.getMean());
			}
		}
		return estimate;
	}

	/**
	 * Provide an estimate for a particular variable. <br>
	 * <br>
	 * The estimates are provided on the hectare basis. If the aggregationPatterns parameter is null, then all 
	 * the species are aggregated in a single group. Otherwise, the parameter allows for user-specified grouping. 
	 * 
	 * @param compositeStand
	 * @param trees
	 * @param v
	 * @param aggregationPatterns
	 * @return
	 */
	public final Estimate<Matrix, SymmetricMatrix, ?> getEstimatePerHaForThisVariable(GScene compositeStand, Collection trees, VariableForEstimation v, LinkedHashMap<String, List<String>> aggregationPatterns) {
		if (v == VariableForEstimation.HDOM) { //special case for HDOM
			try {
				TreeMap<Integer, Collection<ExtTree>> collections = getTreeCollectionsByRealization((ExtCompositeStand) compositeStand, trees);

				MonteCarloEstimate estimate = new MonteCarloEstimate();
				Matrix realization;
				for (Collection<ExtTree> coll : collections.values()) {
					realization = new Matrix(1,1);
					realization.setValueAt(0, 0, getHdom(compositeStand, coll)); 
					estimate.addRealization(realization);
				}
				
				return estimate;
			} catch (Exception e) {
				Log.println (Log.ERROR, "ExtendedMethodProvider.getHDomEstimate()", "Error while computing dominant height", e);
				return null;
			}
		} else {
			ExtCompositeStand extCompositeStand = (ExtCompositeStand) compositeStand;
			
			Map<Integer, Map<String, Matrix>> results = compileTreesForThisMethod(extCompositeStand,
					aggregationPatterns,
					(List<ExtTree>) trees,
					v);
			Estimate<Matrix, SymmetricMatrix, ?> estimate = produceEstimate(results);
			if (aggregationPatterns!= null && !aggregationPatterns.isEmpty()) {
				List<String> rowIndex = new ArrayList<String>();
				rowIndex.addAll(aggregationPatterns.keySet());
				estimate.setRowIndex(rowIndex);
			}
			return estimate;
		}
	}

	
	
	protected double getAppropriateMethod(ExtTree t, VariableForEstimation v) {
		switch(v) {
			case N:
				return 1d;
			case G:
				return t.getStemBasalAreaM2();
			case V:
				return t.getCommercialVolumeM3();			
			default:
				throw new InvalidParameterException("Type " + v.name() + " is not supported");
		}
	}


	@Override
	public final Estimate<Matrix, SymmetricMatrix, ?> getNPerHaEstimate(GScene compositeStand, Collection trees) {
		Estimate<Matrix, SymmetricMatrix, ?> estimate = getEstimatePerHaForThisVariable(compositeStand, trees, VariableForEstimation.N);	// null for frequencies
		return estimate;
	}

	@Override
	public final Estimate<Matrix, SymmetricMatrix, ?> getGPerHaEstimate(GScene compositeStand, Collection trees) {
		Estimate<Matrix, SymmetricMatrix, ?> estimate = getEstimatePerHaForThisVariable(compositeStand, trees, VariableForEstimation.G);
		return estimate;
	}
	
	@Override
	public final Estimate<Matrix, SymmetricMatrix, ?> getHdomEstimate(GScene compositeStand, Collection trees) {
		Estimate<Matrix, SymmetricMatrix, ?> estimate = getEstimatePerHaForThisVariable(compositeStand, trees, VariableForEstimation.HDOM);
		return estimate;
	}

	@Override
	public Estimate<Matrix, SymmetricMatrix, ?> getAreaProportionByTreatmentEstimate(ExtCompositeStand extCompositeStand, List<Enum> treatmentList) {
		int outputSize = 1;
		boolean isThereATreatmentList = treatmentList != null && !treatmentList.isEmpty();
		if (isThereATreatmentList) {
			outputSize = treatmentList.size();
		} 
		Map<Integer, Map<String, Matrix>> results = instantiateResultsMap(extCompositeStand, outputSize);
		Collection<ExtPlotSample> plotSamples = extCompositeStand.getPlotSamples();
		for (ExtPlotSample sample : plotSamples) {
			Map<String, Matrix> innerMap = results.get(sample.getMonteCarloRealizationId());
			for (String plotId : sample.getPlotIds()) {
				ExtPlot plot = sample.getPlot(plotId);
				Matrix m = innerMap.get(plotId);
				if (plot.isInterventionResult()) {	// we assume equal inclusion probabilities here MF2021-03-19
					REpiceaTreatmentDefinition def = plot.getLastThinningOccurrence().getTreatmentDefinition();
					int index = 0;
					if (def != null) {
						index = treatmentList.indexOf(def.getTreatmentType());
					}
					if (index != -1) {	// if index == -1 means that the treatment has been omitted in the list.
						m.setValueAt(index, 0, plot.getWeight());
					}
				}
				
			}
		}
		Estimate<Matrix, SymmetricMatrix, ?> estimate = produceEstimate(results);
		if (isThereATreatmentList) {
			List<String> indices = treatmentList.stream().
					map(p -> p.name()).
					collect(Collectors.toList());
			estimate.setRowIndex(indices);
		}
		return estimate;
	}
	
	@Override
	public Estimate<Matrix, SymmetricMatrix, ?> getDdomEstimate(GScene compositeStand, Collection trees) {
		try {
			TreeMap<Integer, Collection<ExtTree>> collections = getTreeCollectionsByRealization((ExtCompositeStand) compositeStand, trees);

			MonteCarloEstimate estimate = new MonteCarloEstimate();
			Matrix realization;
			for (Collection<ExtTree> coll : collections.values()) {
				realization = new Matrix(1,1);
				realization.setValueAt(0, 0, getDdom(compositeStand, coll)); 
				estimate.addRealization(realization);
			}
			
			return estimate;
		} catch (Exception e) {
			Log.println (Log.ERROR, "ExtendedMethodProvider.getHDomEstimate()", "Error while computing dominant height", e);
			return null;
		}
	}

	private TreeMap<Integer, Collection<ExtTree>> getTreeCollectionsByRealization(ExtCompositeStand compositeStand, Collection<ExtTree> trees) {
		TreeMap<Integer, Collection<ExtTree>> collections = new TreeMap<Integer, Collection<ExtTree>>();
		for (ExtPlotSample plotSample : compositeStand.getPlotSamples()) {
			collections.put(plotSample.getMonteCarloRealizationId(), new ArrayList<ExtTree>());
		}
		ExtTree t;
		for (Object tree : trees) {
			t = (ExtTree) tree;
			int monteCarloRealizationID = t.getMonteCarloRealizationId();
			collections.get(monteCarloRealizationID).add(t);
		}
		return collections;
	}
	
	
	
	/**
	 * VProviderWithName Returns the name of volume.
	 */
	@Override
	public String getVolumeName() {
		return MessageID.CommercialVolume.toString();
	}

	/*
	 * getHdom provides the average height for the dominant trees which are by definition the
	 * x tallest trees. x value is set in ArtUtility
	 */
	@SuppressWarnings("rawtypes")
	@Override
	public double getHdom(GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

			double plotAreaHa = ((AreaHaProvider) stand).getAreaHa();
			double domHeight = getAllometryCalculator().getDominantHeightM(trees, plotAreaHa, true);
			return domHeight;	
		} catch (Exception e) {
			Log.println (Log.ERROR, "QuebecMRNFMethodProvider.getHdom ()", 
					"Error while computing dominant height", e);
			return -1d;
		}
	}

	/*
	 * getDdom provides the average diameter for the dominant trees which are by definition the
	 * x tallest trees in terms of DBH. x value is set in ArtUtility
	 */
	@SuppressWarnings("rawtypes")
	public double getDdom (GScene stand, Collection trees) {
		try {
			if (trees == null) {return -1d;}
			if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

			double plotAreaHa = ((AreaHaProvider) stand).getAreaHa();
			double domDiameter = getAllometryCalculator().getDominantDiameterCM(trees, plotAreaHa, true);
			return domDiameter;	
		} catch (Exception e) {
			Log.println (Log.ERROR, "ArtMethodProvider.getDdom ()", 
					"Error while computing dominant diameter", e);
			return -1d;
		}
	}

	protected AllometryCalculator getAllometryCalculator() {
		if (ac == null) {
			ac = new AllometryCalculator();
		}
		return ac;
	}


	@Override
	public double getTotalAboveGroundVolume(GScene stand, Collection trees) {
		if (trees == null) {return -1d;}
		if (trees.isEmpty ()) {return 0d;}		// if no trees, return 0

		int numberIterationMC = ((ExtCompositeStand) stand).getNumberRealizations();
		double factor = 1d / numberIterationMC;
		ExtTree t;

		double volume = 0;
		for (Object tree : trees) {
			t = (ExtTree) tree;
			if (t.getNumber() > 0) {
				volume += t.getTotalVolumeM3() * t.getNumber() * t.getPlotWeight();		
			}
		}
		return volume * factor;
	}


	@Override
	public double getVMerchant(GScene stand, Collection trees) {
		return getV(stand, trees);
	}


	@Override
	public String getVMerchantName () {
		return getVolumeName();
	}

	
	private ExtTree getFormerTree(ExtTree currentTree) {
		ExtPlot currentStand = (ExtPlot) currentTree.getScene();
		ExtPlot formerStand = (ExtPlot) getPreviousScene(currentStand.getStratum()).getExtPlot(currentStand.getMonteCarloRealizationId(), currentStand.getId());
		ExtTree formerTree = (ExtTree) formerStand.getTree(currentTree.getId());
		return formerTree;
	}
	
	private ExtCompositeStand getPreviousScene(ExtCompositeStand currentCompositeTreeList) {
		return (ExtCompositeStand) ((Step) currentCompositeTreeList.getStep().getFather()).getScene();
	}
	
	
	@SuppressWarnings({"rawtypes" })
	@Override
	public Estimate<Matrix, SymmetricMatrix, ?> getAverageRingWidthCm(GScene compositeStand, Collection trees) {
		ExtCompositeStand extCompositeStand = (ExtCompositeStand) compositeStand;
		Map<Integer, Double>  output = new TreeMap<Integer, Double>();
		Map<Integer, Double>  n = new TreeMap<Integer, Double>();
		for (ExtPlotSample plotSample : extCompositeStand.getPlotSamples()) {
			output.put(plotSample.getMonteCarloRealizationId(), 0d);
			n.put(plotSample.getMonteCarloRealizationId(), 0d);
		}
		if (!compositeStand.getStep().isRoot() && !compositeStand.isInterventionResult()) {
			int timeInterval = compositeStand.getDate() - getPreviousScene((ExtCompositeStand) compositeStand).getDate();
			double intervalFactor = (double) 1 / timeInterval;
			
			for (Object tree : trees) {
				ExtTree currentTree = (ExtTree) tree;
				ExtPlot currentStand = (ExtPlot) currentTree.getScene();
				ExtTree formerTree = getFormerTree(currentTree);
				if (formerTree != null) {
					int currentMCId = currentStand.getMonteCarloRealizationId();
					double outputToBeAdded = (currentTree.getDbhCm() - formerTree.getDbhCm()) * currentTree.getNumber() * intervalFactor * currentTree.getPlotWeight();
					double nToBeAdded = currentTree.getNumber();
					output.put(currentMCId, output.get(currentMCId) + outputToBeAdded);
					n.put(currentMCId, n.get(currentMCId) + nToBeAdded); 
				}
			}
		}
		
		MonteCarloEstimate outputEstimate = new MonteCarloEstimate();
		Matrix realization;
		for (ExtPlotSample plotSample : extCompositeStand.getPlotSamples()) {
			int i = plotSample.getMonteCarloRealizationId();
			realization = new Matrix(1,1);
			realization.setValueAt(0, 0, output.get(i) / n.get(i)); // we divide the sum of the weighted ring width by the number of trees to get the average ring width
			outputEstimate.addRealization(realization);
		}
		
		return outputEstimate;
	}


	private Estimate<Matrix, SymmetricMatrix, ?> getVariableEstimated(GScene compositeStand, Collection trees, VariableForEstimation var) {
		switch (var) {
		case N:
			return getNPerHaEstimate(compositeStand, trees);
		case G:
			return getGPerHaEstimate(compositeStand, trees);
		case V:
			return getVolumePerHaEstimate(compositeStand, trees);
		default:
			return null;
		}
	}

	@Override
	public final Estimate<Matrix, SymmetricMatrix, ?> getPAIEstimate(ExtCompositeStand compositeStand, GrowthComponent component, VariableForEstimation variable) {
	
		if (!compositeStand.getStep().isRoot() && !compositeStand.isInterventionResult()) {
			int timeInterval = compositeStand.getDate() - getPreviousScene(compositeStand).getDate();
			double intervalFactor = (double) 1 / timeInterval;
				
			Collection<Tree> trees;
			
			switch (component) {

			case Mortality:
				trees = ((ExtCompositeStand) compositeStand).getTrees(StatusClass.dead);
				trees.addAll(((ExtCompositeStand) compositeStand).getTrees(StatusClass.windfall));
				return getVariableEstimated(compositeStand, trees, variable).getProductEstimate(-intervalFactor);
	
			case Harvesting:
				trees = ((ExtCompositeStand) compositeStand).getTrees(StatusClass.cut);
				return getVariableEstimated(compositeStand, trees, variable).getProductEstimate(-intervalFactor);
				
			case Recruitment:
				trees = new ArrayList<Tree>();
				for (Tree t : ((TreeList) compositeStand).getTrees()) {
					if (getFormerTree((ExtTree) t) == null) {			// no match found in previous stand so it is a recruit
						trees.add(t);
					}
				}
				return getVariableEstimated(compositeStand, trees, variable).getProductEstimate(intervalFactor);
				
			case Growth:
				Map<ExtTree, ExtTree> matchMap = new HashMap<ExtTree, ExtTree>();
				Collection<Tree> allTrees = new ArrayList<Tree>();
				for (StatusClass sc : StatusClass.values()) {
					allTrees.addAll(compositeStand.getTrees(sc));
				}
				for (Tree t : allTrees) {
					ExtTree tree = (ExtTree) t;
					if (tree.getStatusClass() == StatusClass.cut) {
						int u = 0;
					}
					ExtTree treeInPreviousStand = getFormerTree(tree);
					if (treeInPreviousStand != null) {   		// a match was found so this tree already existed
						matchMap.put(tree, treeInPreviousStand);
					}
				}
				
				Map<Integer, Map<String, Matrix>> results = compileIncrementForTheseTrees(compositeStand, matchMap, variable);
				Estimate<Matrix, SymmetricMatrix, ?> growthEst = produceEstimate(results).getProductEstimate(intervalFactor);
				return growthEst;
			}
		} 
		return null;
	}

	
	@Override
	public Estimate<Matrix, SymmetricMatrix, ?> getAverageProductionPerHa(ExtCompositeStand stand) {
		Estimate<Matrix, SymmetricMatrix, ?> cumulativeProduction = null;
		ExtCompositeStand s = stand;
		while(!s.isInitialScene()) {
			int timeInterval = s.getDateYr() - getPreviousScene(s).getDateYr();
			Estimate<Matrix, SymmetricMatrix, ?> survEst = getPAIEstimate(s, GrowthComponent.Growth, VariableForEstimation.V);
			Estimate<Matrix, SymmetricMatrix, ?> recrEst = getPAIEstimate(s, GrowthComponent.Recruitment, VariableForEstimation.V);
			Estimate<Matrix, SymmetricMatrix, ?> currentEst = survEst.getSumEstimate(recrEst).getProductEstimate(timeInterval);
			if (cumulativeProduction == null) {
				cumulativeProduction = currentEst;
			} else {
				cumulativeProduction = cumulativeProduction.getSumEstimate(currentEst);
			}
			s = getPreviousScene(s);
		}
		
		if (cumulativeProduction != null) {
			int horizon = stand.getDateYr() - s.getDateYr();
			cumulativeProduction = cumulativeProduction.getProductEstimate(1d / horizon);
		}
		
		return cumulativeProduction;
	}

}
