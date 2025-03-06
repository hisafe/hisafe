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

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.scriptapi.Request;
import repicea.stats.data.DataSet;
import repicea.stats.estimates.BootstrapHybridPointEstimate;
import repicea.stats.estimates.BootstrapHybridPointEstimate.VarianceEstimatorImplementation;
import repicea.stats.estimates.Estimate;
import repicea.stats.estimates.MonteCarloEstimate;

/**
 * An abstract class for extracting the result from a simulation. 
 * @author Mathieu Fortin - March 2021
 */
public abstract class ExtScriptAbstractOutputRequest implements Cloneable {
	
	final Request request;
	List<Estimate<Matrix, SymmetricMatrix, ?>> estimates;
	private List<Integer> dateYrs;
	
	/**
	 * Constructor. <br>
	 * <br>
	 * 
	 * @param request an Enum variable 
	 */
	protected ExtScriptAbstractOutputRequest(Request request) {
		if (request == null) {
			throw new InvalidParameterException("The request argument must be non null!");
		}
		this.request = request;
	}
	
	protected Request getRequest() {return request;}	
 	
	@Override
	protected abstract ExtScriptAbstractOutputRequest clone();
	
	protected static Map<Request, ExtScriptAbstractOutputRequest> copyOutputRequestMap(Map<Request, ExtScriptAbstractOutputRequest> originalOutputRequestMap) {
		Map<Request, ExtScriptAbstractOutputRequest> outputMap = new HashMap<Request, ExtScriptAbstractOutputRequest>();
		for (Request r : originalOutputRequestMap.keySet()) {
			outputMap.put(r, originalOutputRequestMap.get(r).clone());
		}
		return outputMap;
	}

	private Map<Integer, VarianceEstimatorImplementation> getVarianceEstimatorImplementationByRowIndex()  {
		Map<Integer, VarianceEstimatorImplementation> estimatorImplementations = null;
		if (estimates.get(0) instanceof BootstrapHybridPointEstimate) {
			for (Estimate<Matrix, SymmetricMatrix, ?> est : estimates) {
				if (estimatorImplementations == null) {
					estimatorImplementations = new HashMap<Integer, VarianceEstimatorImplementation>();
					for (int i = 0; i < est.getMean().m_iRows; i++) {
						estimatorImplementations.put(i, VarianceEstimatorImplementation.Corrected); // by default
					}
				}
				Matrix varianceMatrix = ((BootstrapHybridPointEstimate) est).getVariance();
				if (varianceMatrix != null) {	// if null means that variance was not calculable
					for (int i = 0; i < varianceMatrix.m_iRows; i++) {
						if (varianceMatrix.getValueAt(i, i) < 0d) { // if any variance < 0 then we cannot use the corrected estimator
							estimatorImplementations.put(i, VarianceEstimatorImplementation.LessBiased);
						}
					}
				}
			}
		} 
		return estimatorImplementations;
	}
	
	
	protected final List<Object[]> generateObservationsFromThisRequest(int initialDateYr) {
		Map<Integer, VarianceEstimatorImplementation> varImpl = getVarianceEstimatorImplementationByRowIndex();
		List<Object[]> outputList = new ArrayList<Object[]>();
		for (int i = 0; i < dateYrs.size(); i++) {
			int dateYr = dateYrs.get(i);
			Estimate<Matrix, SymmetricMatrix, ?> estimate = estimates.get(i);
			
			Matrix mean = estimate.getMean();
			List<String> rowIndex = estimate.getRowIndex();
			for (int j = 0; j < mean.m_iRows; j++) {
				Matrix variance;
				String varianceEstimatorType;
				if (estimate instanceof BootstrapHybridPointEstimate) {
					VarianceEstimatorImplementation vei = varImpl.get(j);
					((BootstrapHybridPointEstimate) estimate).setVarianceEstimatorImplementation(vei);
					variance = estimate.getVariance();	// get the variance estimate under the proper estimator implementation
					varianceEstimatorType = vei.name();
				} else {
					variance = estimate.getVariance();
					varianceEstimatorType = "Unknown";
				}

				String group = request.name();
				if (rowIndex.isEmpty()) { // presumably all the species have been grouped into a single group
					group += "_" + "ALL";		
				} else if (j >= rowIndex.size()) { // index is incomplete and j is out of bound
					group += "_" + "NOT_INDEXED";
				} else {
					group += "_" + rowIndex.get(j);
				}
				List<Object> observation = new ArrayList<Object>();
				observation.add(dateYr);
//				observation.add(nbRealizations);
				observation.add(dateYr - initialDateYr);
				observation.add(group);
				observation.add(mean.getValueAt(j, 0));
				if (variance != null) {
					observation.add(variance.getValueAt(j, j));
				} else {
					observation.add(Double.NaN);
				}
				observation.add(varianceEstimatorType);
				outputList.add(observation.toArray());
			}
			
		}
		return outputList;
	}

	/**
	 * Store the collapsed estimate into a DataSet instance.
	 * @param dataset a DataSet instance
	 * @param initialDateYr the initial date (year) of the simulation
	 */
	protected final void storeEstimateIntoDataSet(DataSet dataset, int initialDateYr) {
		List<Object[]> observations = generateObservationsFromThisRequest(initialDateYr);
		for (Object[] record : observations) {
			dataset.addObservation(record);
		}
	}

	/**
	 * Should be called first in the addEstimateForThisStand method. 
	 * @param compositeStands
	 */
	protected final void makeSureEstimateListIsIntantiated(List<ExtCompositeStand> compositeStands) {
		if (dateYrs == null) {
			dateYrs = new ArrayList<Integer>();
			estimates = new ArrayList<Estimate<Matrix, SymmetricMatrix, ?>>();
			for (ExtCompositeStand s : compositeStands) {
				if (isMeantForInitialStands() || !s.isInitialScene()) {	// will be false if the initial stand and the request is not meant for it
					dateYrs.add(s.getDateYr());
					estimates.add(null);
				}
			}
		}
	}
	
	/**
	 * This is where the estimates are calculated and stored in the list. It should call makeSureEstimateListIsIntantiated 
	 * before calculating and storing the estimates.
	 * @param stand
	 * @param compositeStands
	 * @param methodProvider
	 */
	protected abstract void addEstimateForThisStand(ExtCompositeStand stand, List<ExtCompositeStand> compositeStands, ExtMethodProvider methodProvider);
	
	protected final void addEstimateInList(int index, Estimate<Matrix, SymmetricMatrix, ?> estimate) {
		if (estimate == null) {
			throw new InvalidParameterException("The estimate argument cannot be null!");
		}
		if (estimates.get(index) == null) {
			estimates.set(index, estimate);
		} else {
			Estimate<Matrix, SymmetricMatrix, ?> overallEstimate = estimates.get(index);
			if (!overallEstimate.getClass().equals(estimate.getClass())) {
				throw new InvalidParameterException("The overallEstimate and the estimate instances are incompatible!");
			}
			if (overallEstimate instanceof BootstrapHybridPointEstimate) {
				((BootstrapHybridPointEstimate) overallEstimate).appendBootstrapHybridEstimate((BootstrapHybridPointEstimate) estimate);
			} else  if (overallEstimate instanceof MonteCarloEstimate) {
				for (Matrix realization : ((MonteCarloEstimate) estimate).getRealizations()) {
					((MonteCarloEstimate) overallEstimate).addRealization(realization);
				}
			} else {
				throw new InvalidParameterException("The overallEstimate and estimate arguments should be of the BootstrapHybridPointEstimate or the MonteCarloEstimate classes");
			}
		}
	}
	
	/**
	 * Provide the number of aggregation patterns. 
	 * 
	 * @return an integer
	 */
	public abstract int getNumberOfPatterns();

	protected final Matrix getOverallMean() {
		Matrix output = null;
		for (Estimate<Matrix, SymmetricMatrix, ?> estimate : estimates) {
			if (output == null) {
				output = estimate.getMean();
			} else {
				output = output.matrixStack(estimate.getMean(), true); // stack over;
			}
		}
		return output;
	}

	protected final boolean isMeantForInitialStands() {return getRequest().getIsMeantForInitialStand();}

}
