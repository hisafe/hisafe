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
package capsis.util.extendeddefaulttype.disturbances;

import java.util.Collection;
import java.util.Map;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GScene;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import capsis.util.methodprovider.DdomProvider;


/**
 * The ThinningDisturbanceParameters class is an abstract class that embeds a harvest model or 
 * a harvest rule.
 * @author Mathieu Fortin - July 2018
 */
@SuppressWarnings("serial")
public abstract class ThinningDisturbanceParameters extends DisturbanceParameters {
	
	
	protected double targetDiameterCm = Double.NaN;
	
	
	/**
	 * For derived class.
	 * @param mode
	 * @param recurrenceYrs
	 */
	protected ThinningDisturbanceParameters(DisturbanceMode mode, double recurrenceYrs) {
		super(DisturbanceType.Harvest, mode, recurrenceYrs);
	}
	
	/**
	 * Mark the trees of a particular stand. <br>
	 * <br>
	 * The method first checks if there is thinning at the stand (plot) level. 
	 * If the thinning occurs, the method checks if each tree is harvested or not.
	 * @param compositeStand an ExtCompositeStand instance
	 * @param stand a TreeList instance
	 * @param trees a Collection of Tree instance
	 * @param stepLengthYrs the duration of the growth step (yrs)
	 * @param mp a MethodProvider instance that implements the DdomProvider interface
	 * @param plotLevelParms a Map of plot-level harvest parameters
	 * @param treeLevelParms a Map of tree-level harvest parameters
	 * @return a ThinningDisturbanceOccurrences instance or null if there is no thinning
	 */
	public abstract ThinningDisturbanceOccurrences markTrees(ExtCompositeStand compositeStand,
			TreeList stand, 
			Collection<Tree> trees, 
			int stepLengthYrs, 
			DdomProvider mp, 
			Map<String, Object> plotLevelParms,
			Map<String, Object> treeLevelParms);
	
	
	@Override
	public abstract ThinningDisturbanceOccurrences isThereADisturbance(ExtCompositeStand compositeStand, TreeList stand, int stepLengthYrs, Map<String, Object> parms);
	
	
	public void setTargetDiameterCm(double targetDiameterCm) {this.targetDiameterCm = targetDiameterCm;}
	
	protected boolean isFinalCut(GScene stand, Collection<Tree> trees, DdomProvider mp) {
		boolean finalCut = false;
		if (!Double.isNaN(targetDiameterCm)) {
			if (mp.getDdom(stand, trees) > targetDiameterCm) {
				finalCut = true;
			}
		}
		return finalCut;
	}
	

}
