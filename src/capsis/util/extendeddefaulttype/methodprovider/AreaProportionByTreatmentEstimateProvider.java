/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2021 Her Majesty the Queen in right of Canada
 * Author: Mathieu Fortin, Canadian Wood Fibre Centre
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.util.extendeddefaulttype.methodprovider;

import java.util.List;

import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.extendeddefaulttype.ExtCompositeStand.PredictorID;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.thinners.REpiceaThinner;
import repicea.stats.estimates.Estimate;

public interface AreaProportionByTreatmentEstimateProvider {

	/**
	 * Provide an estimate of the proportion of the area for each treatment (if the
	 * treatmentList argument is non null and not empty) or globally (if the treatmentList
	 * argument is set to null or is empty).
	 * @param extCompositeStand an ExtCompositeStand instance
	 * @param treatmentList a List of Enum that stand for the treatment. It can be null or empty. In such
	 * a case, the treatment are all pooled together.
	 * @return an Estimate (either a MonteCarloEstimate or a BootstrapHybridPointEstimate instance). 
	 */
	public Estimate<Matrix, SymmetricMatrix, ?> getAreaProportionByTreatmentEstimate(ExtCompositeStand extCompositeStand, List<Enum> treatmentList);

	/**
	 * Return a list of treatment for this stand. <br>
	 * <br>
	 * It first looks for the list of treatments in the plot-level harvest submodule and if the list is null,
	 * it then looks into the tree-level harvest submodule.
	 * 
	 * @param extCompositeStand an ExtCompositeStand instance
	 * @return a List of Enum or null if no list can be found
	 */
	public default List<Enum> getDefaultTreatmentList(ExtCompositeStand extCompositeStand) {
		REpiceaThinner standThinner = (REpiceaThinner) extCompositeStand.getPredictor(PredictorID.STAND_HARVESTING);
		List<Enum> treatmentList = standThinner.getTreatmentList();
		if (treatmentList == null) {
			REpiceaThinner treeThinner = (REpiceaThinner) extCompositeStand.getPredictor(PredictorID.TREE_HARVESTING);
			treatmentList = treeThinner.getTreatmentList();
		}
		return treatmentList;
	}
	
}
