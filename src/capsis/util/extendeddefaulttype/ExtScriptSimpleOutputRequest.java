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
import java.util.List;

import capsis.util.extendeddefaulttype.methodprovider.PeriodicAnnualIncrementEstimatesProvider.GrowthComponent;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.covariateproviders.MethodProviderEnum.VariableForEstimation;
import repicea.simulation.scriptapi.Request;
import repicea.stats.estimates.Estimate;

/**
 * A class for extracting results from script simulation. 
 * @author Mathieu Fortin - March 2021
 */
class ExtScriptSimpleOutputRequest extends ExtScriptAbstractOutputRequest {

	/**
	 * Constructor for simple plot-level output without specification for statuses or species. 
	 * 
	 * @param request a SimpleRequest enum
	 */
	ExtScriptSimpleOutputRequest(Request request) {
		super(request);
	}
	
	@Override
	protected ExtScriptSimpleOutputRequest clone() {
		return new ExtScriptSimpleOutputRequest(request);
	}
	
	@Override
	protected void addEstimateForThisStand(ExtCompositeStand stand, List<ExtCompositeStand> compositeStands, ExtMethodProvider methodProvider) {
		makeSureEstimateListIsIntantiated(compositeStands);
		Estimate<Matrix, SymmetricMatrix, ?> estimate = null;
		switch(request) {
		case PeriodicAnnualGrowth:
			estimate = methodProvider.getPAIEstimate(stand, GrowthComponent.Growth, VariableForEstimation.V);
			break;
		case PeriodicAnnualMortality:
			estimate = methodProvider.getPAIEstimate(stand, GrowthComponent.Mortality, VariableForEstimation.V);
			break;
		case PeriodicAnnualRecruitment:
			estimate = methodProvider.getPAIEstimate(stand, GrowthComponent.Recruitment, VariableForEstimation.V);
			break;
		case PeriodicAnnualHarvesting:
			estimate = methodProvider.getPAIEstimate(stand, GrowthComponent.Harvesting, VariableForEstimation.V);
			break;
		case ProportionHarvestedAreaPerTreatment:
			estimate = methodProvider.getAreaProportionByTreatmentEstimate(stand, methodProvider.getDefaultTreatmentList(stand));
			break;
		default:
			throw new InvalidParameterException("The request parameter is not recognized!");
		}
		if (estimate == null) {
			throw new UnsupportedOperationException("The estimate is null!");
		}
		int index = compositeStands.indexOf(stand);
		if (!isMeantForInitialStands()) {
			index--;		// we must subtract one since the initial stand is not considered
		}
		addEstimateInList(index, estimate);
	}

	@Override
	public int getNumberOfPatterns() {
		return 1;
	}


}
