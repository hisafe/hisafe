/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2019 CWFC-CFS 
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

import capsis.util.extendeddefaulttype.ExtCompositeStand;
import repicea.simulation.climate.REpiceaClimateVariableMap.ClimateVariable;

public interface ClimateVariableProvider {

	/**
	 * Returns the average of the climate variable among all the plots that compose the stand.
	 * @param stand an ExtCompositeStand instance
	 * @param variable a ClimateVariable enum
	 * @return a double
	 */
	public double getClimateVariable(ExtCompositeStand stand, ClimateVariable variable);
}
