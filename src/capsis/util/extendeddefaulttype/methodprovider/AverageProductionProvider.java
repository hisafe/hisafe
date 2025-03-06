/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2019 Mathieu Fortin
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
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

import capsis.util.extendeddefaulttype.ExtCompositeStand;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.stats.estimates.Estimate;

public interface AverageProductionProvider extends PeriodicAnnualIncrementEstimatesProvider {

	/**
	 * Returns the cumulative production in m3 over time.
	 * @param stand an ExtCompositeStand instance
	 * @return an Estimate instance
	 */
	public Estimate<Matrix, SymmetricMatrix, ?> getAverageProductionPerHa(ExtCompositeStand stand);
	
}
