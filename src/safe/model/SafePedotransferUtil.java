/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.model;

/**
 * PEDOTRANSFERT functions for hydraulic properties of soil
 * (adapted from Wosten et al., 1998)
 * @author : D.HARJA	- ICRAF, Bogor 16001, Indonesia
 */
public class SafePedotransferUtil {

	/**
	 * Return ThetaSat (Total saturated porosity) in m3 water m-3 soil
	 * @param clay Percentage of clay (%)  
	 * @param bulkDensity  Bulk density (kg m-3)
	 * @param silt Percentage of silt (%)  
	 * @param organicMatter Percentage of organicMatter (%)  
	 * @param topSoil 0=no 1=yes
	 */
	public static double getThetaSat (  double clay,
										double bulkDensity,
										double silt,
										double organicMatter,
										int topSoil) {
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
	 * @param clay Percentage of clay (%)  
	 * @param bulkDensity  Bulk density (kg m-3)
	 * @param silt Percentage of silt (%)  
	 * @param organicMatter Percentage of organicMatter (%)  
	 * @param topSoil 0=no 1=yes
	 */
	public static double getKSat (double clay,
								  double bulkDensity,
								  double silt,
								  double organicMatter,
								  int topSoil) {
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
	 * @param clay Percentage of clay (%)  
	 * @param bulkDensity  Bulk density (kg m-3)
	 * @param silt Percentage of silt (%)  
	 * @param organicMatter Percentage of organicMatter (%)  
	 * @param topSoil 0=no 1=yes
	 */
	public static double getAlpha ( double clay,
									double bulkDensity,
									double silt,
									double organicMatter,
									int topSoil) {
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
	 * @param clay Percentage of clay (%)  
	 * @param bulkDensity  Bulk density (kg m-3)
	 * @param silt Percentage of silt (%)  
	 * @param organicMatter Percentage of organicMatter (%)  
	 */
	public static double getLambda (double clay,
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
	 * @param clay Percentage of clay (%)  
	 * @param bulkDensity  Bulk density (kg m-3)
	 * @param silt Percentage of silt (%)  
	 * @param organicMatter Percentage of organicMatter (%)  
	 * @param topSoil 0=no 1=yes
	 */
	public static double getN ( double clay,
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
	 * @param particleSizeSand Particle size of sand (micrometers)
	 * @param clay Percentage of clay (%)  
	 * @param silt Percentage of silt (%)  
	 * @param organicMatter Percentage of organicMatter (%)  
	 * @param topSoil 0=no 1=yes
	 */
	public static double getBulkDensity (double particleSizeSand,
										 double clay,
										 double silt,
										 double organicMatter,
										 int topSoil) {
		if ((clay + silt) < 50) {
			return 1
				/ (
					- 1.984
					+ 0.01841 * organicMatter
					+ 0.032 * topSoil
					+ 0.00003576 * (clay + silt) * (clay + silt)
					+ 67.5 / particleSizeSand
					+ 0.424 * Math.log(particleSizeSand));
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
	public static double getpF( double theta,
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
	public static double getP ( double theta,
								double thetaSat,
								double alpha,
								double n) {
		
		double pF = getpF(theta, thetaSat, alpha, n);
		return getP(pF);
	}

	/**
	 * Return P from Pf
	 * @param pF 
	 */
	public static double getP (double pF) {
		return -Math.pow(10, pF);
	}

	/**
	 * Return conductivity
	 */
	public static double getConductivity (double p,
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
	public static double getTheta (double p,
								   double thetaSat,
								   double alpha,
								   double n) {
		
		return thetaSat / Math.pow(1 + Math.pow(Math.abs(alpha * p), n), 1 - 1 / n);
	}
}
