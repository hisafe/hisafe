/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
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
package capsis.util.extendeddefaulttype.methodprovider;

import java.util.Collection;

import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.stats.estimates.Estimate;
import capsis.kernel.GScene;

public interface AverageRingWidthCmProvider {
	
	/**
	 * This method returns the average ring width (cm)
	 * @param compositeStand a GScene instance
	 * @param trees a Collection of trees
	 * @return an Estimate instance
	 */
	public Estimate<Matrix, SymmetricMatrix, ?> getAverageRingWidthCm(GScene compositeStand, Collection trees);

}
