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
package capsis.util.extendeddefaulttype;

public interface ExtMultiThreadingStochasticModel {
	
	/**
	 * This method process a part of the stand map that is contained in the CompositeTreeList class.
	 * The part is defined by the parameters iStartMC and iEndMC. This method should handle the mortality, the
	 * growth and the recruitment.
	 * @param currentCompositeStand the current stand
	 * @param newCompositeStand the future stand
	 * @param mainTask an ExtEvolutionTask instance that is the main task. If cancelled, this main task must be able to cancel the ExtMonteCarloWorkerThread instances
	 * @param iStartMC the starting iteration
	 * @param iEndMC the ending iteration
	 */
	public void processStandList(ExtCompositeStand currentCompositeStand, ExtCompositeStand newCompositeStand, ExtEvolutionTask mainTask, int iStartMC, int iEndMC) throws Exception;
	
}
