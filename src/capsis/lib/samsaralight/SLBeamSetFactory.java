/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
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

package capsis.lib.samsaralight;

import java.util.List;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ListMap;

/**
 * A light beam set factory.
 * 
 * @author B. Courbaud, N. Dones, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLBeamSetFactory {

	// Warning: february always has 28 days, even when using a hourly file
	static public int[] NBMONTHDAYS = new int[] { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	// Declination in degrees for each month
	static public double[] DECLINATION_DEG = { -20.8, -12.7, -1.9, 9.9, 18.9, 23.1, 21.3, 13.7, 3.0, -8.8, -18.4, -23 };

	/**
	 * Compute the proportion of each month that correspond to the vegetation
	 * period. validated on 22/06/2012.
	 * 
	 * @author G. Ligot
	 */
	// For testing the calculation of LAD using a correction factor. nb-04.12.2017
	//static protected double[] monthVegetativeProportions(int leafOnDoy, int leafOffDoy) {
	static public double[] monthVegetativeProportions(int leafOnDoy, int leafOffDoy) {

		double[] r = new double[12];
		int sum = 0;
		for (int i = 0; i < 12; i++) {
			int d = NBMONTHDAYS[i];
			sum += d; // day of year of the last day of this month

			// vegetative period?
			if (sum >= leafOnDoy && (sum - d) <= leafOffDoy) {

				// number of vegetative day in a month that is partly inside the
				// vegetation period
				int d1 = sum - leafOnDoy + 1; // number of day between leaf on
												// date and the last day of this
												// month
				int d2 = leafOffDoy - (sum - d); // number of day between leaf
													// off date and the first
													// day of this month

				// is it the first vegetative month?
				if (d1 >= 0 && d1 < d) {

					r[i] = d1 / (double) d;

					// is it the last vegetative month?
				} else if (d2 >= 0 && d2 < d) {

					r[i] = d2 / (double) d;

				} else {
					// the month is fully inside the vegetative period
					r[i] = 1d;
				}
			} else {
				r[i] = 0d;
			}
		}

		SLReporter.printInLog("SamsaraLight", "SLBeamSetFactory monthVegetativeProportions: " + AmapTools.toString(r));
		return r;
	}

	/**
	 * Compute the mean day of year for each month. A correction is applied for
	 * month partly inside the vegetation period.
	 * 
	 * @author G. Ligot
	 */
	static public int[] meanDoyPerMonth(int leafOnDoy, int leafOffDoy) {

		int[] r = new int[12];
		int sum = 0; // day of year for the last day of the previous month

		for (int i = 0; i < 12; i++) {
			int d = NBMONTHDAYS[i];

			// int meanDoy = (int) (d/2d);

			// vegetative period?
			if (sum >= leafOnDoy && (sum - d) <= leafOffDoy) {

				int d1 = (sum + d) - leafOnDoy + 1; // number of day between
													// leaf on date (included)
													// and the last day of this
													// month
				int d2 = leafOffDoy - sum; // number of day between leaf off
											// date and the first day of this
											// month

				// Is it the first vegetative month?
				if (d1 >= 0 && d1 < d) {

					r[i] = sum + d1 / 2;

					// is it the last vegetative month?
				} else if (d2 >= 0 && d2 < d) {

					r[i] = sum + d2 / 2;

				} else {
					// the month is fully inside the vegetative period
					r[i] = sum + d / 2;
				}
				// not within the vegetative period
			} else {
				r[i] = sum + d / 2;
			}

			sum += d;
		}
		SLReporter.printInLog("SamsaraLight", "SLBeamSetFactory : meanDoyPerMonth: " + AmapTools.toString(r));
		return r;
	}

	// /**
	// * Create a classic beam set with data in a file (not activated yet).
	// */
	// public static SLBeamSet getClassicBeamSet(String fileName) {
	// // code here
	// return null;
	// }

	/**
	 * Create a beamSet from hourly record. A mean day is used per month with a
	 * correction for the months that are not totally inside the vegetation
	 * period.
	 * 
	 * @author G. Ligot, 22/06/2012
	 */
	static public SLBeamSet getHourlyBeamSet(SLSettings sets) {
		double latitude_rad = Math.toRadians(sets.getPlotLatitude_deg());
		double longitude_rad = Math.toRadians(sets.getPlotLongitude_deg());
		double heightAngleMin_rad = Math.toRadians(sets.getHeightAngleMin());
		double slope_rad = Math.toRadians(sets.getPlotSlope_deg());
		double northToXAngle_cw_rad = Math.toRadians(sets.getNorthToXAngle_cw_deg());
		double southAzimut_ccw_rad = Math.PI + northToXAngle_cw_rad; // azimut
																		// of
																		// south
																		// counterclockwise
																		// from
																		// x
																		// axis
		double bottomAzimut_rad = Math.toRadians(-sets.getPlotAspect_deg() + sets.getNorthToXAngle_cw_deg()); // azimut
																												// of
																												// the
																												// vector
																												// orthogonal
																												// to
		double diffuseAngleStep_rad = Math.toRadians(sets.getDiffuseAngleStep());
		int GMT = sets.getGMT();

		if (southAzimut_ccw_rad > 2 * Math.PI)
			southAzimut_ccw_rad -= 2 * Math.PI; // GL 27/06/2012

		boolean SOC = sets.isSoc ();

		double[] declination_rad = new double[12];
		for (int i = 0; i < DECLINATION_DEG.length; i++)
			declination_rad[i] = Math.toRadians(DECLINATION_DEG[i]);

		double[] diffuseEnergy = new double[12];
		double[][] hourlyDirectEnergy = new double[12][24]; // direct energy
															// read from the
															// samsaraLight
															// settings
															// first index is
															// the month, second
															// index is the hour
		// compute hourly sum for each month
		ListMap<Integer, SLHourlyRecord> allRecords = sets.getHourlyRecords();

		int doyMonthFirstDay = 0;

		for (int m = 0; m < 12; m++) { // the key used in the listMap
										// corresponds exactly to the month
										// index

			List<SLHourlyRecord> monthlyRecords = (List) allRecords.getObjects(m + 1);// m+1
																					// because
																					// in
																					// the
																					// ListMap
																					// they
																					// are
																					// encoded
																					// with
																					// the
																					// true
																					// month
																					// number

			for (SLHourlyRecord r : monthlyRecords) {

				int doy = doyMonthFirstDay + r.day;

				if (doy >= sets.getLeafOnDoy() && doy <= sets.getLeafOffDoy()) {

					// sum diffuse energy
					diffuseEnergy[m] += r.global * r.diffuseToGlobalRatio;

					// sum hourly direct energy
					double direct = r.global * (1 - r.diffuseToGlobalRatio);
					hourlyDirectEnergy[m][r.hour] += r.global * (1 - r.diffuseToGlobalRatio);
					// System.out.println("SLBEAMSETFactory - m " + m + " h "
					// + r.hour + " direct " + direct);
				}
			}
			doyMonthFirstDay += NBMONTHDAYS[m];

		}

		SLBeamSet bs = new SLBeamSet();

		// sum of diffuse energy
		double totalDiffuse = 0.0;
		for (int i = 0; i < 12; i++) { // for each month
			totalDiffuse += diffuseEnergy[i];
		}

		int[] meanDoy = meanDoyPerMonth(sets.getLeafOnDoy (), sets.getLeafOffDoy ());

		classicDiffuseRaysCreation(SOC, totalDiffuse, diffuseAngleStep_rad, heightAngleMin_rad, slope_rad,
				bottomAzimut_rad, bs);
		directHourRaysCreation(latitude_rad, longitude_rad, declination_rad, hourlyDirectEnergy, heightAngleMin_rad,
				slope_rad, southAzimut_ccw_rad, bottomAzimut_rad, meanDoy, GMT, bs);

		SLReporter.printInLog("SamsaraLight", " ");
		SLReporter.printInLog("SamsaraLight", "---------------------------------------");
		SLReporter.printInLog("SamsaraLight", "SLBeamSetFactory : Created hourly rays");
		SLReporter.printInLog("SamsaraLight", "---------------------------------------");
		SLReporter.printInLog("SamsaraLight", bs.bigCSVString());
		SLReporter.printInLog("SamsaraLight", "---------------------------------------");

		return bs;
	}

	/**
	 * Create a beamset from monthly records.
	 * 
	 * @author FC, GL, BC, 18-21/06/2012
	 */
	static public SLBeamSet getMonthlyBeamSet(SLSettings sets) {
		double latitude_rad = Math.toRadians(sets.getPlotLatitude_deg());
		double slope_rad = Math.toRadians(sets.getPlotSlope_deg());
		double northToXAngle_cw_rad = Math.toRadians(sets.getNorthToXAngle_cw_deg());
		double bottomAzimut_rad = Math.toRadians(-sets.getPlotAspect_deg() + sets.getNorthToXAngle_cw_deg()); // azimut
																												// of
																												// the
																												// vector
																												// orthogonal
																												// to
																												// the
																												// ground
																												// in
																												// the
																												// x,y
																												// system
		double heightAngleMin_rad = Math.toRadians(sets.getHeightAngleMin ()); // angle
																			// min
																			// between
																			// beam
																			// and
																			// soil
		double southAzimut_ccw_rad = Math.PI + northToXAngle_cw_rad; // azimut
																		// of
																		// south
																		// counterclockwise
																		// from
																		// x
																		// axis

		if (southAzimut_ccw_rad > 2 * Math.PI)
			southAzimut_ccw_rad -= 2 * Math.PI; // GL 27/06/2012

		boolean SOC = sets.isSoc ();

		// double plotAspect = Math.toRadians(sets.getPlotAspect_deg());
		// double azimut;
		// double heightAngle;
		// double energy;

		double directAngleStep_rad = Math.toRadians(sets.getDirectAngleStep ()); // hour
																			// angle
																			// between
																			// two
																			// direct
																			// beams
		double diffuseAngleStep_rad = Math.toRadians(sets.getDiffuseAngleStep ());

		double declination_rad[] = new double[12];
		for (int i = 0; i < DECLINATION_DEG.length; i++)
			declination_rad[i] = Math.toRadians(DECLINATION_DEG[i]);

		double[] globalEnergy = new double[12];
		double[] directEnergy = new double[12];
		double[] diffuseEnergy = new double[12];

		// get radiations from the setting file
		int c = 0;
		double[] mVProportion = monthVegetativeProportions(sets.getLeafOnDoy (), sets.getLeafOffDoy ());
		for (SLMonthlyRecord r : sets.getMontlyRecords()) {
			globalEnergy[c] = r.global * mVProportion[c];
			diffuseEnergy[c] = r.global * r.diffuseToGlobalRatio * mVProportion[c];
			directEnergy[c] = globalEnergy[c] - diffuseEnergy[c];
			c++;
		}

		SLBeamSet bs = new SLBeamSet();

		// Calculating total diffuse energy on a horizontal plan for each month
		double totalDiffuse = 0.0;
		for (int i = 0; i < 12; i++) { // for each month
			totalDiffuse += diffuseEnergy[i];
		}

		classicDiffuseRaysCreation(SOC, totalDiffuse, diffuseAngleStep_rad, heightAngleMin_rad, slope_rad,
				bottomAzimut_rad, bs);

		//sq-23.11.2021 added getBeamStartOffset for samsara2 integration
		// fc-13.4.2022 Turned beamStartOffset into radians
		double beamStartOffset_rad = Math.toRadians(sets.getBeamStartOffset());
		directMonthRaysCreation(latitude_rad, declination_rad, directAngleStep_rad, directEnergy, heightAngleMin_rad,
				slope_rad, southAzimut_ccw_rad, bottomAzimut_rad, bs, beamStartOffset_rad);

		SLReporter.printInLog("SamsaraLight", " ");
		SLReporter.printInLog("SamsaraLight", "----------------------------------------");
		SLReporter.printInLog("SamsaraLight", "SLBeamSetFactory : Created monthly rays ");
		SLReporter.printInLog("SamsaraLight", "----------------------------------------");
		SLReporter.printInLog("SamsaraLight", bs.bigCSVString());
		SLReporter.printInLog("SamsaraLight", "----------------------------------------");

		return bs;
	}

	/**
	 * Calculating SLBeam diffuse Energy in MJ/m2 of a plane perpendicular to
	 * beam ray direction for a classical sky Standard Overcast Sky and Uniform
	 * Overcast Sky are possible
	 */
	public static double classicDiffuseEnergy(boolean SOC, double heightAngle_rad, double diffuseAngleStep_rad,
			double totalDiffuse) {

		double energy;
		double meridianNb = 2 * Math.PI / diffuseAngleStep_rad;
		double heightAInf = heightAngle_rad - diffuseAngleStep_rad / 2;
		double sinInf = Math.sin(heightAInf);

		// if(heightAInf < heightAngleMin) heightAInf = heightAngleMin;

		double heightASup = heightAngle_rad + diffuseAngleStep_rad / 2;
		double sinSup = Math.sin(heightASup);
		// System.out.println("tDiff: "+totalDiffuse+" merid: "+meridianNb+" hSup: "+heightASup+" hInf: "+heightAInf);

		if (!SOC) { // Uniform Overcast Sky, per square meter of a
			// horizontal plan
			energy = (2 * totalDiffuse / meridianNb) * (sinSup * sinSup - sinInf * sinInf) / 2;
		} else { // Standard Overcast Sky, Energy per square meter of a
			// horizontal plan
			energy = (6 * totalDiffuse / (7 * meridianNb))
					* ((Math.pow(sinSup, 2) - Math.pow(sinInf, 2)) / 2 + 2 * (Math.pow(sinSup, 3) - Math.pow(sinInf, 3)) / 3);
		}
		// in MJ/m2 on a plane perpendicular to the ray
		energy = energy / Math.sin(heightAngle_rad);

		return energy;
	}

	/**
	 * Diffuse beams are created with a classical sky hemisphere divided by
	 * meridians and parallels Standard Overcast Sky and Uniform Overcast Sky
	 * are possible.
	 */
	public static void classicDiffuseRaysCreation(boolean SOC, double totalDiffuse, double diffuseAngleStep_rad,
			double angleMin_rad, double slope_rad, double bottomAzimut_rad, SLBeamSet bs) {

		int count = 0;
		double horizontalDiffuse = 0;
		double slopeDiffuse = 0;
		double heightAngle_rad = diffuseAngleStep_rad / 2;
		while (heightAngle_rad < Math.PI / 2) {
			double azimut = diffuseAngleStep_rad / 2; // If azimut starts from
			// sq-28.10.2021 opti : moved energy out of second while()
			// (trigonometric) rotation from X axis
			double energy = classicDiffuseEnergy(SOC, heightAngle_rad, diffuseAngleStep_rad, totalDiffuse);
			// 0, there can be round problems with transformation of angleStep
			// from degrees to radians and the last azimut can be very close to
			// 360 (one extra azimut)
			while (azimut < 2 * Math.PI) { // azimut is with anticlockwise
				// The HorizontalDiffuse reference is calculated for
				// heightAngles > angleMin
				if (heightAngle_rad > angleMin_rad) {
					horizontalDiffuse += energy * Math.sin(heightAngle_rad);
				}
				// A beam is created only if it reaches the soil with an angle >
				// angleMin the cosinus of the angle between the vector
				// orthogonal to slope and the beam must be higher than
				// sin(angleMin) this cosinus is given by scalar
				double scalar = Math.cos(slope_rad) * Math.sin(heightAngle_rad) + Math.sin(slope_rad)
						* Math.cos(heightAngle_rad) * Math.cos(azimut - bottomAzimut_rad);

				if (scalar > Math.sin(angleMin_rad)) {
					SLBeam b = new SLBeam(azimut, heightAngle_rad, energy, false);
					slopeDiffuse += scalar * energy;
					count++;
					bs.addBeam(b);
				}

				azimut += diffuseAngleStep_rad;
			}
			heightAngle_rad += diffuseAngleStep_rad;
		}
		bs.setHorizontalDiffuse(horizontalDiffuse);
		bs.setSlopeDiffuse(slopeDiffuse);

		SLReporter.printInLog("SamsaraLight", "Nb rayons diffus : " + count + "  horizontalDiffuse : " + horizontalDiffuse
				+ " slopeDiffuse : " + slopeDiffuse);
	}

	/**
	 * Calculation of sun azimut for a given height angle reference system with
	 * angle origin on X > 0 axis and trigonometric rotation
	 */
	public static double sunAzimut(double latitude_rad, double declination_rad, double hourAngle_rad,
			double heightAngle_rad, double southAzimut_rad) {

		double azimut;
		// Solar position formulas in the reference system with azimut = 0 at
		// south and clockwise rotation
		double sinAz = Math.cos(declination_rad) * Math.sin(hourAngle_rad) / Math.cos(heightAngle_rad);
		double cosAz = (Math.sin(latitude_rad) * Math.cos(declination_rad) * Math.cos(hourAngle_rad) - Math
				.cos(latitude_rad) * Math.sin(declination_rad))
				/ Math.cos(heightAngle_rad);
		if (cosAz >= 0)
			azimut = Math.asin(sinAz);
		else if (sinAz >= 0)
			azimut = Math.PI - Math.asin(sinAz);
		else
			azimut = -Math.PI - Math.asin(sinAz);

		// Log.println("sun azimut in solar system (deg):" +
		// Math.toDegrees(azimut));

		// Reference system with angle origine on X > 0 axis and trigonometric
		// rotation
		// southAzimut_rad gives azimut of south direction in this system
		azimut = southAzimut_rad - azimut;
		if (azimut > 2 * Math.PI)
			azimut = azimut - 2 * Math.PI;
		else if (azimut < 0)
			azimut = 2 * Math.PI + azimut;

		// Log.println("sun azimut in stand system (deg):" +
		// Math.toDegrees(azimut));

		return (azimut);

	}

	/**
	 * Direct rays are created along one solar path per month During a day, ray
	 * energy is proportional to sin(heightAngle)
	 */
	public static void directHourRaysCreation(double latitude_rad, double longitude_rad, double[] declination_rad,
			double[][] hourlyDirectEnergy, double angleMin_rad, double slope_rad, double southAzimut_rad,
			double bottomAzimut_rad, int[] meanDoy, int GMT, SLBeamSet bs) {
		double heightAngle_rad;
		double azimut_rad;
		// double energy;
		double horizontalDirect = 0;
		double slopeDirect = 0;
		int count = 0;
		double lostEnergy = 0; // radiations recorded during the night

		// System.out.println("   . angleMin: "+angleMin+" slope: "+slope
		// +"\n   . southAzimut: "+southAzimut+ " bottomAzimut: "+bottomAzimut);

		SLReporter.printInLog("SamsaraLight", "----------------------------------");
		SLReporter.printInLog("SamsaraLight", "Direct Ray Creation");
		SLReporter.printInLog("SamsaraLight", "----------------------------------");

		SLReporter.printInLog("SamsaraLight", " Month ; hour in the file ; localSolarTime ; hourAngle_deg ; "
				+ "heightAngle_rad ; azimut_rad ; Ray energy ; ground energy ; isInterceptedBySlope ;");

		for (int m = 0; m < 12; m++) { // for each month

			// Log.println ("SamsaraLight","Month number : " + m + " -----");

			lostEnergy = 0;
			for (int h = 0; h < 24; h++) { // for each hour

				double directEnergy = hourlyDirectEnergy[m][h];
				if (directEnergy > 0) {

					// compute hour angle with Teh (2006, p27-29) equations
					// or see http://pvcdrom.pveducation.org/SUNLIGHT/SOLART.HTM
					// nearest standard meridian from the site
					double stdLongitude = ((int) (longitude_rad / (Math.PI / 12))) * (Math.PI / 12); // eq.
																										// 2.5.

					// equation of timee q 2.6 Teh (2006, p27-29)
					// double B = 2 * Math.PI * (meanDoy[m] - 81) / 364;
					// //eq.2.6 Teh (2006, p27-29)
					// double eot = 9.87 * Math.sin(2 * B) - 7.53 * Math.cos(B)
					// - 1.5 * Math.sin(B);

					// equation of time, wikipedia
					double B = 2 * Math.PI * meanDoy[m] / 365.242; // wikipedia
					double eot = -7.657 * Math.sin(B) + 9.862 * Math.sin(2 * B + 3.599); // minutes,
																							// wikipedia

					// 0.5 if the time measure corresponds to the measurement
					// start
					double localClockTime = h + 0.5;
					double daylightSavingTime = 0; // TODO
					double LocalClockTimeOnGreenwichMeridian = localClockTime - GMT - daylightSavingTime;
					double longitudeCorrection = (longitude_rad - stdLongitude) / Math.PI * 12;
					// this will only work in Western Europe!
					double localSolarTime = LocalClockTimeOnGreenwichMeridian + eot / 60 - longitudeCorrection; // hour
					double hourAngle = Math.PI / 12 * (localSolarTime - 12); // hour

					// System.out.println("legal time " + (h+0.5) +
					// " local time " + localTime +" Local solar time " +
					// localSolarTime + " Hour Angle " + hourAngle + " eot (h) "
					// + eot + " TC " + ((- stdLongitude + longitude_rad) /
					// (Math.PI / 12)) );
					// System.out.println("SlBeamSetFactory hourAngle: " +
					// hourAngle + "h " + h);

					heightAngle_rad = Math.asin(Math.sin(latitude_rad) * Math.sin(declination_rad[m])
							+ Math.cos(latitude_rad) * Math.cos(declination_rad[m]) * Math.cos(hourAngle));
					azimut_rad = sunAzimut(latitude_rad, declination_rad[m], hourAngle, heightAngle_rad,
							southAzimut_rad);

					// double hourSinHeightAng = Math.sin(latitude) *
					// Math.sin(declination[m]) * angleStep
					// + Math.cos(latitude) * Math.cos(declination[m]) *
					// (Math.sin(hourAngle + angleStep / 2) - Math.sin(hourAngle
					// - angleStep / 2));
					//
					// energy = directEnergy;

					if (heightAngle_rad > angleMin_rad) {
						horizontalDirect += directEnergy;
					} else {
						// System.out.println("SLBeamSetFactory : directHourRayCreation() - m "
						// + m + " h " + h + " energy " + directEnergy);
						lostEnergy += directEnergy;
					}

					// in MJ/m2 on a plane perpendicular to the ray
					// hourSinHeightAng is very close to Math.sin (heightAngle)
					// so all rays
					// have about the same amount of energy on the plane
					// perpendicular to the ray.
					double perpendicularEnergyRay = directEnergy / Math.sin(heightAngle_rad);

					// a ray is created only if it reaches the soil with an
					// angle > angleMin
					// the cosinus of the angle between the vector orthogonal to
					// slope and the ray must be heigher than sin(angleMin)
					// this cosinus is given by scalar
					double scalar = Math.cos(slope_rad) * Math.sin(heightAngle_rad) + Math.sin(slope_rad)
							* Math.cos(heightAngle_rad) * Math.cos(azimut_rad - bottomAzimut_rad);

					if ((heightAngle_rad > 0) && (scalar > Math.sin(angleMin_rad))) {

						SLBeam b = new SLBeam(azimut_rad, heightAngle_rad, perpendicularEnergyRay, true);
						bs.addBeam(b);

						// System.out.println("   new Beam, height : " +
						// heightAngle + " azimut : " + azimut + " energy : " +
						// perpendicularEnergyRay);

						count++;
						slopeDirect += scalar * perpendicularEnergyRay;

						SLReporter.printInLog("SamsaraLight", (m + 1) + ";" + h + ";" + localSolarTime + ";" + Math.toDegrees(hourAngle) + ";"
										+ heightAngle_rad + ";" + azimut_rad + ";" + perpendicularEnergyRay + ";"
										+ (scalar * perpendicularEnergyRay) + ";" + false);

					} else if ((heightAngle_rad > 0) && (scalar <= Math.sin(angleMin_rad))) {

						SLReporter.printInLog("SamsaraLight", (m + 1) + ";" + h + ";" + localSolarTime + ";" + Math.toDegrees(hourAngle) + ";"
										+ heightAngle_rad + ";" + azimut_rad + ";" + perpendicularEnergyRay + ";"
										+ (scalar * perpendicularEnergyRay) + ";" + true);
					}

					bs.setHorizontalDirect(horizontalDirect);
					bs.setSlopeDirect(slopeDirect);

				}
			}

		}

		SLReporter.printInLog("SamsaraLight", "-----------------------------");
		SLReporter.printInLog("SamsaraLight", "SLBeamSetFactory : directHourRayCreation() - Nb rayons directs : " + count
				+ "  horizontalDirect : " + horizontalDirect + " slopeDirect : " + slopeDirect);
		SLReporter.printInLog("SamsaraLight",
				"SLBeamSetFactory : directHourRayCreation() - Direct energy not taken into account (heightAngle_rad < angleMin_rad) "
						+ lostEnergy);

	}

	/**
	 * Direct rays are created along one solar path per month During a day, ray
	 * energy is proportional to sin(heightAngle)
	 */
	private static void directMonthRaysCreation(double latitude_rad, double[] declination_rad, double angleStep_rad,
			double[] directEnergy, double angleMin_rad, double slope_rad, double southAzimut_rad,
			double bottomAzimut_rad, SLBeamSet bs, double beamStartOffset_rad) {

		// fc-13.4.2022 Changed beamStartOffset by beamStartOffset_rad (expected in radians)
		
		double heightAngle_rad; // rad
		double azimut_rad; // rad
		double energy;
		double horizontalDirect = 0;
		double slopeDirect = 0;
		int count = 0;

		// System.out.println("SlBeamSetFactory directMonthRayCreation () directEnergy: "+AmapTools.toString(directEnergy));
		// System.out.println("   . angleStep: "+angleStep+" angleMin: "+angleMin+" slope: "+slope
		// +"\n   . southAzimut: "+southAzimut+ " bottomAzimut: "+bottomAzimut);

		for (int i = 0; i < 12; i++) { // for each month
			if (directEnergy[i] > 0) {
				// integrating sin(heightAngle) along the day
				double sunRiseHourAng = -Math.acos(-Math.tan(latitude_rad) * Math.tan(declination_rad[i]));
				double sunSetHourAng = -sunRiseHourAng;
				double daySinHeightAng = Math.sin(latitude_rad) * Math.sin(declination_rad[i])
						* (sunSetHourAng - sunRiseHourAng) + Math.cos(latitude_rad) * Math.cos(declination_rad[i])
						* (Math.sin(sunSetHourAng) - Math.sin(sunRiseHourAng));

				// System.out.println("sunRiseHourAng : " + sunRiseHourAng
				// + " sunSetHourAng : " + sunSetHourAng + "daySinHAng : "
				// + daySinHeightAng);

				//sq-23.11.2021 why angleStep has been added ? not there in original samsara2 beamSetFactory
				//added beamStartOffset to works with samsara2
				//checked with Samsara2 data set
				
				// fc-13.4.2022 Changed beamStartOffset by beamStartOffset_rad (expectd in radians)
				// bug found when adapting EcoAF to SamsaraLight: there were no direct beams due to this bug
				double hourAngle = -Math.PI + beamStartOffset_rad; //angleStep_rad / 2d;
				
				while (hourAngle < Math.PI) { // for each hourAngle

					// System.out.println("SlBeamSetFactory hourAngle: " +
					// hourAngle);

					// while (hourAngle < Math.toRadians(15)) { //calculation
					// for one hour only
					heightAngle_rad = Math.asin(Math.sin(latitude_rad) * Math.sin(declination_rad[i])
							+ Math.cos(latitude_rad) * Math.cos(declination_rad[i]) * Math.cos(hourAngle));
					azimut_rad = sunAzimut(latitude_rad, declination_rad[i], hourAngle, heightAngle_rad,
							southAzimut_rad);
					double hourSinHeightAng = Math.sin(latitude_rad) * Math.sin(declination_rad[i]) * angleStep_rad
							+ Math.cos(latitude_rad) * Math.cos(declination_rad[i])
							* (Math.sin(hourAngle + angleStep_rad / 2) - Math.sin(hourAngle - angleStep_rad / 2));

					// System.out.println("   hourSinHeightAng: " +
					// hourSinHeightAng);

					// in MJ/m2 on a horizontal plane
					energy = directEnergy[i] * hourSinHeightAng / daySinHeightAng;

					// The HorizontalDirect reference is calculated for
					// heightAngles > angleMin
					if (heightAngle_rad > angleMin_rad) {
						horizontalDirect += energy;
					}
					// in MJ/m2 on a plane perpendicular to the ray
					// hourSinHeightAng is very close to Math.sin (heightAngle)
					// so all rays
					// have about the same amount of energy on the plane
					// perpendicular to the ray.
					double perpendicularEnergyRay = energy / Math.sin(heightAngle_rad);

					// A ray is created only if it reaches the soil with an
					// angle > angleMin
					// the cosinus of the angle between the vector orthogonal to
					// slope and the ray must be heigher than sin(angleMin)
					// this cosinus is given by scalar
					double scalar = Math.cos(slope_rad) * Math.sin(heightAngle_rad) + Math.sin(slope_rad)
							* Math.cos(heightAngle_rad) * Math.cos(azimut_rad - bottomAzimut_rad);

					if ((heightAngle_rad > 0) && (scalar > Math.sin(angleMin_rad))) {
						SLBeam b = new SLBeam(azimut_rad, heightAngle_rad, perpendicularEnergyRay, true);
						bs.addBeam(b);

						count++;
						slopeDirect += scalar * perpendicularEnergyRay;

						// Log.println("SamsaraLight"," hourAngle_deg "
						// + Math.toDegrees(hourAngle) + " height "
						// + heightAngle_rad + " azimut : " + azimut_rad
						// + " energy : " + perpendicularEnergyRay
						// + " Nb rayons directs : " + count);

						// Log.println("SamsaraLight",
						// " heightAngle_rad ;" + heightAngle_rad +
						// "; azimut_rad ;"
						// + azimut_rad + "; energy ;"
						// + perpendicularEnergyRay
						// );

					}
					hourAngle += angleStep_rad; // next hourAngle in rad
				}
			}
		}
		bs.setHorizontalDirect(horizontalDirect);
		bs.setSlopeDirect(slopeDirect);

		SLReporter.printInLog("SamsaraLight", "Nb rayons directs : " + count + "  horizontalDirect : " + horizontalDirect
				+ " slopeDirect : " + slopeDirect);
	}

}
