/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */
package safe.model;

/**
 * Pedotransfer functions for hydraulic properties of soil
 * (adapted from Wosten et al., 1998)
 * @author D.HARJA			- ICRAF Bogor Indonisia - - July 2004
 */
public class SafePedotransferUtil {

	/**
	 * Return ThetaSat (Total saturated porosity) in m3 water m-3 soil
	 */
	public static double getThetaSat (
		double clay,
		double bulkDensity,
		double silt,
		double organicMatter,
		double topSoil) {
		return 0.7919
			+ 0.001691 * clay
			- 0.29619 * bulkDensity
			- 0.00000149 * silt * silt
			+ 0.0000821 * organicMatter * organicMatter
			+ 0.02427 / clay
			+ 0.01113 / silt
			+ 0.01472 * Math.log(silt)
			- 0.0000733 * organicMatter * clay
			- 0.000619  * bulkDensity   * clay
			- 0.001183  * bulkDensity   * organicMatter
			- 0.0001664 * topSoil       * silt;
	}

	/**
	 * Return Ksat (saturated conductivity)	in	cm d-1
	 */
	public static double getKSat (
		double clay,
		double bulkDensity,
		double silt,
		double organicMatter,
		double topSoil) {
		return Math.exp(
			7.755
				+ 0.0352 * silt
				+ 0.93 * topSoil
				- 0.967 * bulkDensity * bulkDensity
				- 0.000484 * clay * clay
				- 0.000322 * silt * silt
				+ 0.001 / silt
				- 0.0748 / organicMatter
				- 0.643 * Math.log(silt)
				- 0.01398 * bulkDensity * clay
				- 0.1673 * bulkDensity * organicMatter
				+ 0.02986 * topSoil * clay
				- 0.03305 * topSoil * silt);
	}
	/**
	 * Return alpha
	 */
	public static double getAlpha (
		double clay,
		double bulkDensity,
		double silt,
		double organicMatter,
		double topSoil) {
		return Math.exp(
			-14.96
				+ 0.03135 * clay
				+ 0.0351 * silt
				+ 0.646 * organicMatter
				+ 15.29 * bulkDensity
				- 0.192 * topSoil
				- 4.671 * bulkDensity * bulkDensity
				- 0.000781 * clay * clay
				- 0.00687 * organicMatter * organicMatter
				+ 0.0449 / organicMatter
				+ 0.0663 * Math.log(silt)
				+ 0.1482 * Math.log(organicMatter)
				- 0.04546 * bulkDensity * silt
				- 0.4852 * bulkDensity * organicMatter
				+ 0.00673 * topSoil * clay);
	}

	/**
	 * Return Lambda
	 */
	public static double getLambda (
		double clay,
		double bulkDensity,
		double silt,
		double organicMatter) {
		double l =
			((10
				* (Math
					.exp(
						0.0202
							+ 0.0006193 * clay * clay
							- 0.001136 * organicMatter * organicMatter
							- 0.2316 * Math.log(organicMatter)
							- 0.03544 * bulkDensity * clay
							+ 0.00283 * bulkDensity * silt
							+ 0.0488 * bulkDensity * organicMatter)))
				- 10)
				/ (1
					+ (Math
						.exp(
							0.0202
								+ 0.0006193 * clay * clay
								- 0.001136 * organicMatter * organicMatter
								- 0.2316 * Math.log(organicMatter)
								- 0.03544 * bulkDensity * clay
								+ 0.00283 * bulkDensity * silt
								+ 0.0488 * bulkDensity * organicMatter)));
		return l;

	}

	/**
	 * Return n
	 */
	public static double getN (
		double clay,
		double bulkDensity,
		double silt,
		double organicMatter,
		double topSoil) {
		return Math.exp(
			-25.23
				- 0.02195 * clay
				+ 0.0074 * silt
				- 0.194 * organicMatter
				+ 45.5 * bulkDensity
				- 7.24 * bulkDensity * bulkDensity
				+ 0.0003658 * clay * clay
				+ 0.002885 * organicMatter * organicMatter
				- 12.81 / bulkDensity
				- 0.1524 / silt
				- 0.01958 / organicMatter
				- 0.2876 * Math.log(silt)
				- 0.0709 * Math.log(organicMatter)
				- 44.6 * Math.log(bulkDensity)
				- 0.02264 * bulkDensity * clay
				+ 0.0896 * bulkDensity * organicMatter
				+ 0.00718 * topSoil * clay)
			+ 1;
	}


	/**
	 * Return BulkDensity
	 */
	public static double getBulkDensity (
		double meanParticleSize,
		double clay,
		double silt,
		double organicMatter,
		double topSoil) {
		if ((clay + silt) < 50) {
			return 1
				/ (
					- 1.984
					+ 0.01841 * organicMatter
					+ 0.032 * topSoil
					+ 0.00003576 * (clay + silt) * (clay + silt)
					+ 67.5 / meanParticleSize
					+ 0.424 * Math.log(meanParticleSize));
		}
		return 1
			/ (0.603
				+ 0.003975 * clay
				+ 0.00207 * organicMatter * organicMatter
				+ 0.01781 * Math.log(organicMatter));
	}
	/**
	 * Return pf
	 */
	public static double getpF(
		double theta,
		double thetaSat,
		double alpha,
		double n) {
		double pF = ((Math.log(1/alpha)*n)
		+ Math.log(Math.pow(theta/thetaSat, -n / (n - 1)) - 1))
	   / (n*(Math.log(2) + Math.log(5)));
	   if(Double.isNaN(pF)) return 0;
		return pF;

	}
	/**
	 * Return P
	 */
	public static double getP (
		double theta,
		double thetaSat,
		double alpha,
		double n) {
		double pF = getpF(theta, thetaSat, alpha, n);
		return getP(pF);
	}

	public static double getP (double pF) {
		return -Math.pow(10, pF);
	}

	/**
	 * Return conductivity
	 */
	public static double getConductivity (
		double p,
		double kSat,
		double alpha,
		double lambda,
		double n) {

		double conductivity =
			kSat
				* Math.pow(
					(Math
						.pow(
							(1 + Math.pow((Math.abs(alpha * p)), n)),
							(1 - 1 / n))
						- Math.pow((Math.abs(alpha * p)), (n - 1))),
					2)
				/ Math.pow(
					(1 + Math.pow((Math.abs(alpha * p)), n)),
					((1 - 1 / n) * (lambda + 2)));

		return conductivity;
	}

	/**
	 * Return the integral of conductivity from 0 to P in cm2 day-1
	 */
	public static double getPhi (double pF,
		double kSat,
		double alpha,
		double lambda,
		double n,
		double deltaPF,
		double pFi) {

		double sum = 0;
		double p1, p2, k1, k2;

		if (pFi <= pF) return 0;

		while (pFi > pF) {
			p1 = -Math.pow (10, pFi);
			k1 = getConductivity (p1, kSat, alpha, lambda, n);

			pFi = pFi - deltaPF;
			if(pFi < pF) pFi = pF;
			p2 = -Math.pow (10, pFi);
			k2 = getConductivity (p2, kSat, alpha, lambda, n);

			sum = sum + (p2-p1) * (k2+k1)/2d;
		}
		return sum;
	}

	/**
	 * Return Phi
	 */
	public static double getPhi (double pF,
			double kSat,
			double alpha,
			double lambda,
			double n) {

			double deltaPF=0.1;
			double pFi=6.0;

			double sum = 0;
			double p1, p2, k1, k2;
			while (pFi > pF) {
				p1 = -Math.pow (10, pFi);
				k1 = getConductivity (p1, kSat, alpha, lambda, n);

				pFi = pFi - deltaPF;
				if(pFi < pF) pFi = pF;
				p2 = -Math.pow (10, pFi);
				k2 = getConductivity (p2, kSat, alpha, lambda, n);

				sum = sum + (p2-p1) * (k2+k1)/2d;
			}
			return sum;
	}

	/**
	 * Return Theta
	 */
	public static double getTheta (
		double p,
		double thetaSat,
		double alpha,
		double n) {
		return thetaSat / Math.pow(1 + Math.pow(Math.abs(alpha * p), n), 1 - 1 / n);
	}
}
