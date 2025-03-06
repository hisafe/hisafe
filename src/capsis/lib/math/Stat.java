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
package capsis.lib.math;


/**
 * Statistics analysis tools.
 * For questions about methods (add, modify...) in this package,
 * please ask B. Courbaud.
 *
 * @author B. Courbaud - march 2004
 */
public class Stat {

	/**	Mean.
	*	B. Courbaud, F. de Coligny - march 2004
	*/
	static public double mean (double[] values) throws Exception {
		if (values == null) {throw new Exception ("Stat.mean error: values = null");}
		if (values.length < 1) {throw new Exception ("Stat.mean error: values.length < 1");}
		double sum = 0;
		int n = values.length;
		for (int i = 0; i < n; i++) {
			sum += values[i];
		}
		return sum / n;
	}

	/**	Standard deviation.
	*	B. Courbaud, F. de Coligny - march 2004
	*/
	static public double standardDeviation (double mean, double[] values) throws Exception {
		if (values == null) {throw new Exception ("Stat.standardDeviation error: values = null");}
		if (values.length < 1) {throw new Exception ("Stat.standardDeviation error: values.length < 1");}
		double sum = 0;
		int n = values.length;
		for (int i = 0; i < n; i++) {
			double a = values[i] - mean;
			sum += a*a;
		}
		return Math.sqrt (sum / n);
	}



	/**	Variation coefficient.
	*	B. Courbaud, F. de Coligny - march 2004
	*/
	static public double variationCoefficient (double mean, double standardDeviation) throws Exception {
		if (mean == 0) {throw new Exception ("Stat.variationCoefficient error: mean = 0");}
		return standardDeviation / mean;
	}

	/**	Constancy.
	*	B. Courbaud, F. de Coligny - march 2004
	*/
	static public double constancy (double mean, double standardDeviation) throws Exception {
		if (standardDeviation == 0) {throw new Exception ("Stat.constancy error: standardDeviation = 0");}
		return mean / standardDeviation;
	}





}
