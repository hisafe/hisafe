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

import java.io.Serializable;
import java.security.InvalidParameterException;

import capsis.defaulttype.TreeCollection;
import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.extendeddefaulttype.ExtEvolutionDialog.BreakVariable;
import capsis.util.extendeddefaulttype.methodprovider.DDomEstimateProvider;
import capsis.util.extendeddefaulttype.methodprovider.HDomEstimateProvider;

public class ExtEvolutionBreakParameters implements Serializable {

	private final int initialDate;
	private final int preferredStepLengthYr;
	private final BreakVariable variable;
	private double limit;
	private int[] stepDurations;
	private final boolean isFlexible;


	public ExtEvolutionBreakParameters(int initialDate, BreakVariable variable, double limit, int preferredStepLengthYr, boolean isFlexible) {
		this.initialDate = initialDate;
		this.variable = variable;
		this.limit = limit;
		this.preferredStepLengthYr = preferredStepLengthYr;
		this.isFlexible = isFlexible;
		if (variable == BreakVariable.Date) {
			if (limit < initialDate) {
				throw new InvalidParameterException("The limit date should be larger than the initial date");
			} else {
				stepDurations = setStepDurations((int) Math.round(this.limit - initialDate));
			}
		}
	}


	private int[] setStepDurations(int nbYears) {
		int nbSteps = (int) Math.round((double) nbYears / preferredStepLengthYr);
		int[] stepDurations;
		if (nbSteps < 1) {
			stepDurations = new int[1];
			stepDurations[0] = nbYears;
			return stepDurations;
		} else {
			stepDurations = new int[nbSteps];
			int sum = 0;
			for (int i = 0; i < stepDurations.length; i++) {
				stepDurations[i] = preferredStepLengthYr;
				sum += stepDurations[i];
			}
			if (isFlexible) {		// then will adapt the step durations to match the nb of years
				int diff = nbYears - sum;
				if (diff != 0) {
					int change;
					if (diff < 0) {
						change = -1;
					} else {
						change = 1;
					}
					int index = 0;
					while (diff != 0) {
						stepDurations[index] += change;
						diff -= change;
						index += 1;
						while (index >= stepDurations.length) {
							index -= stepDurations.length;
						}
					}
				}
			} else {
				limit = initialDate + sum;
			}
			return stepDurations;
		}
	}

//	private int convertStepDurationsIntoInt() {
//		if (stepDurations == null || stepDurations.length == 0) {
//			return 0;
//		} else {
//			int output = 0;
//			for (int i = 0; i < stepDurations.length; i++) {
//				output += (int)  (Math.pow(10, i) * stepDurations[stepDurations.length - 1 - i]);
//			}
//			return output;
//		}
//	}


	protected int[] getStepDurations() {
		return stepDurations;
	}

	public int getStepDuration(int currentDate) {
		if (variable == BreakVariable.Date) {
			int date = initialDate;
			int i = 0;
			while (date != currentDate && i < stepDurations.length) {
				date += stepDurations[i];
				i++;
			}
			if (i >= stepDurations.length) {
				throw new InvalidParameterException("Mismatch between stepDurations and evolution dates!");
			}
			return stepDurations[i];
		} else {
			return preferredStepLengthYr;
		}
	}

	public int getInitialDate() {return initialDate;}

	protected boolean shouldTheSimulationKeepRunning(GScene scene) {
		MethodProvider mp = scene.getStep().getProject().getModel().getMethodProvider();
		switch(variable) {
		case Date:
			return scene.getDate() < limit;
		case D0:
			return ((DDomEstimateProvider) mp).getDdomEstimate(scene, ((TreeCollection) scene).getTrees()).getMean().getValueAt(0, 0) < limit; 
		case H0:
			return ((HDomEstimateProvider) mp).getHdomEstimate(scene, ((TreeCollection) scene).getTrees()).getMean().getValueAt(0, 0) < limit; 
		default:
			throw new InvalidParameterException("Unknown break variable!");
		}
	}
	
}
