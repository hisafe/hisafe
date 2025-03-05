/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.datarenderer.drcurves;




//_________________________________________________________________________
public class GraduationStepAdjuster {

	/**	Step adjusting for axis graduations.
	 */
	static public double adjustStep (double stp, boolean stepMustBeInteger, int maxFractionDigits) {
		double newStep = 0;

		// Integer step needed
		if (stepMustBeInteger) {
			newStep = Math.ceil (stp);
			if (newStep > 10) {		// fc - 16 oct 2001 - replaced 5 by 10
				int iInt = (int) newStep;
				int d = iInt / 10;
				int r = iInt % 10;
				if (r <= 5) {
					r = 5;
				} else {
					r = 10;
				}
				newStep = d * 10 + r;
			}
			return newStep;
		}

		// Decimal step possible
		if (stp <= 0.01) {
			newStep = 0.01;
		} else if (0.01 < stp && stp <= 0.05) {
			newStep = 0.05;
		} else if (0.05 < stp && stp <= 0.1) {
			newStep = 0.1;
		} else if (0.1 < stp && stp <= 0.2) {
			newStep = 0.2;
		} else if (0.2 < stp && stp <= 0.3) {
			newStep = 0.3;
		} else if (0.3 < stp && stp <= 0.5) {
			newStep = 0.5;
		} else if (0.5 < stp && stp <= 1) {
			newStep = 1;
		} else if (1 < stp && stp <= 2) {
			newStep = 2;
		} else if (2 < stp && stp <= 3) {
			newStep = 3;
		} else if (3 < stp && stp <= 4) {
			newStep = 4;
		} else if (4 < stp && stp <= 5) {
			newStep = 5;
		} else if (5 < stp) {
			int iInt = (int) stp;
			int d = iInt / 10;
			int r = iInt % 10;
			if (r <= 5) {
				r = 5;
			} else {
				r = 10;
			}
			newStep = d * 10 + r;
		}
		return newStep;
	}

}




