package capsis.lib.samsaralight.tag.heterofor;

import java.io.Serializable;
import java.util.Calendar;
import java.util.GregorianCalendar;
import java.util.List;

import capsis.lib.samsaralight.SLBeam;
import capsis.lib.samsaralight.SLBeamSet;
import capsis.lib.samsaralight.SLBeamSetFactory;
import capsis.lib.samsaralight.SLSettings;
import capsis.lib.samsaralight.tag.SLTag;
import capsis.lib.samsaralight.tag.SLTagOptimized;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ListMap;

/**
 * A factory to build beamSets (Created for Heterofor in
 * fineResolutionRadiativeBalance mode, see HetBeamSetFactoryOptimized subclass,
 * SLBeamSetFactoryOptimized also used in PDGLight). BeamSets are rebuilt before
 * each processLighting () (in HetModel, relying on the heterofor meteorology
 * file). A superclass of HetBeamSetFactoryOptimized to allow the use of
 * SamsaraLight Tag mode from PDGLight.
 * 
 * @author F. de Coligny, C. Rouet - January 2022
 */
public class SLBeamSetFactoryOptimized implements Serializable {

	// fc-1.11.2022 Built from HetBeamSetFactoryOptimized to allow the use of Tag
	// mode in PDGLight

	protected static GregorianCalendar calendar;

	protected SLSettings slSettings;

	// fc-11.1.2022 Added, needed for getMonth(year, doy)
	// In HetBeamSetFactoryOptimizedn this is 'startYear'
	protected int yearOfTheBeamSet;

	protected SLRadiation radiation;
	protected SLBeamSet beamSet;

	/**
	 * Constructor
	 */
	public SLBeamSetFactoryOptimized(SLSettings slSettings, int yearOfTheBeamSet) {
		this.slSettings = slSettings;
		this.yearOfTheBeamSet = yearOfTheBeamSet;
	}

	// ------------------------- beams for a given key doy

	/**
	 * Creates diffuse and direct beams for the given key doy.
	 */
	public void addBeamsForKeyDoy(SLBeamSet beamSet, SLKeyDoy keyDoy) {

		HetReporter.printInStandardOutput("addBeamsForKeyDoy(SLBeamSet beamSet, HetKeyDoy keyDoy)...");

		double latitude_rad = Math.toRadians(slSettings.getPlotLatitude_deg());
		double longitude_rad = Math.toRadians(slSettings.getPlotLongitude_deg());
		double heightAngleMin_rad = Math.toRadians(slSettings.getHeightAngleMin());
		double slope_rad = Math.toRadians(slSettings.getPlotSlope_deg());
		double northToXAngle_cw_rad = Math.toRadians(slSettings.getNorthToXAngle_cw_deg());
		double southAzimut_ccw_rad = Math.PI + northToXAngle_cw_rad; // azimut
																		// of
																		// south
																		// counterclockwise
																		// from
																		// x
																		// axis
		double bottomAzimut_rad = Math
				.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg()); // azimut
																									// of
																									// the
																									// vector
		// orthogonal to

		double diffuseAngleStep_rad = Math.toRadians(slSettings.getDiffuseAngleStep());

		int GMT = slSettings.getGMT();

		if (southAzimut_ccw_rad > 2 * Math.PI)
			southAzimut_ccw_rad -= 2 * Math.PI; // GL 27/06/2012

		boolean SOC = slSettings.isSoc();

		double[] declination_rad = new double[12];
		for (int i = 0; i < SLBeamSetFactory.DECLINATION_DEG.length; i++)
			declination_rad[i] = Math.toRadians(SLBeamSetFactory.DECLINATION_DEG[i]);

		// For each direction of diffuse ray, we will have 24 rays with their
		// hour tag
		// So we need 24 sums of energies over the period
		double[] diffuseEnergies = new double[24];

		// For each month, we will have 24 rays with their hour tag
		// Note: hour fractions not yet implemented
		double[] directEnergies = new double[24];

		// Get the 24 radiation lines for the key doy
		List<SLRadiationLine> doyLines = radiation.getLines(keyDoy.doy);

		HetReporter.printInStandardOutput("doyLines: " + AmapTools.toString(doyLines));

		for (SLRadiationLine r : doyLines) {

			int doy = r.doy;
			int hour = r.hour;

			// Sum diffuse energy
			diffuseEnergies[hour] = r.globalRadiation_MJm2 * r.diffuseToGlobalRatio;

			// Sum hourly direct energy
			directEnergies[hour] = r.globalRadiation_MJm2 * (1 - r.diffuseToGlobalRatio);

		}

		HetReporter.printInStandardOutput("diffuseEnergies: " + AmapTools.toString(diffuseEnergies));
		HetReporter.printInStandardOutput("directEnergies: " + AmapTools.toString(directEnergies));

		// Common for keyDoy and leavedPeriod

		double diffuseIncidentEnergy = 1; // fc+fa-3.8.2017

		SLBeamSetFactoryOptimized.classicDiffuseRaysCreation(keyDoy, keyDoy.name, SOC, diffuseIncidentEnergy,
				diffuseEnergies, diffuseAngleStep_rad, heightAngleMin_rad, slope_rad, bottomAzimut_rad, beamSet);

		// Specific for keyDoy
		// fc-11.1.2022 replaced startYear by yearOfTheBeamSet (kept for Heterofor
		// subclass)
		SLBeamSetFactoryOptimized.keyDoyDirectHourRaysCreation(keyDoy, keyDoy.name, yearOfTheBeamSet, latitude_rad,
				longitude_rad, declination_rad, directEnergies, heightAngleMin_rad, slope_rad, southAzimut_ccw_rad,
				bottomAzimut_rad, GMT, beamSet);
//		HetBeamSetFactoryOptimized.keyDoyDirectHourRaysCreation(keyDoy, keyDoy.name, startYear, latitude_rad,
//				longitude_rad, declination_rad, directEnergies, heightAngleMin_rad, slope_rad, southAzimut_ccw_rad,
//				bottomAzimut_rad, GMT, beamSet);

	}

	// -------------------------- beams for leaved period

	/**
	 * Creates diffuse and direct beams for the leaved period.
	 */
	public void addBeamsForPeriod(SLBeamSet beamSet, int leavedPeriodStartDoy, int leavedPeriodEndDoy) {

		double latitude_rad = Math.toRadians(slSettings.getPlotLatitude_deg());
		double longitude_rad = Math.toRadians(slSettings.getPlotLongitude_deg());
		double heightAngleMin_rad = Math.toRadians(slSettings.getHeightAngleMin());
		double slope_rad = Math.toRadians(slSettings.getPlotSlope_deg());
		double northToXAngle_cw_rad = Math.toRadians(slSettings.getNorthToXAngle_cw_deg());
		double southAzimut_ccw_rad = Math.PI + northToXAngle_cw_rad; // azimut
																		// of
																		// south
																		// counterclockwise
																		// from
																		// x
																		// axis
		double bottomAzimut_rad = Math
				.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg()); // azimut
																									// of
																									// the
																									// vector
		// orthogonal to

		double diffuseAngleStep_rad = Math.toRadians(slSettings.getDiffuseAngleStep());

		int GMT = slSettings.getGMT();

		if (southAzimut_ccw_rad > 2 * Math.PI)
			southAzimut_ccw_rad -= 2 * Math.PI; // GL 27/06/2012

		boolean SOC = slSettings.isSoc();

		double[] declination_rad = new double[12];
		for (int i = 0; i < SLBeamSetFactory.DECLINATION_DEG.length; i++)
			declination_rad[i] = Math.toRadians(SLBeamSetFactory.DECLINATION_DEG[i]);

		// For each direction of diffuse ray, we will have 24 rays with their
		// hour tag
		// So we need 24 sums of energies over the period
		double[] diffuseEnergies = new double[24];

		// For each month, we will have 24 rays with their hour tag
		// Note: hour fractions not yet implemented
		double[][] directEnergies = new double[12][24];
		// first index is
		// the month, second
		// index is the hour

		// Dispatch radiation lines per month in a listMap
		ListMap<Integer, SLRadiationLine> radiationsPerMonth = radiation.getRadiationsPerMonth();

		int doyMonthFirstDay = 0;

		for (int m = 0; m < 12; m++) { // the key used in the listMap
										// corresponds exactly to the month
										// index

			// m+1 because in the ListMap they are encoded with the true month
			// number
			List<SLRadiationLine> oneMonth = (List) radiationsPerMonth.getObjects(m + 1);

			for (SLRadiationLine r : oneMonth) {

				int doy = r.doy;
				int hour = r.hour;
				// int doy = doyMonthFirstDay + r.day;

				if (doy >= leavedPeriodStartDoy && doy <= leavedPeriodEndDoy) {

					// Sum diffuse energy
					diffuseEnergies[hour] += r.globalRadiation_MJm2 * r.diffuseToGlobalRatio;

					// Sum hourly direct energy
					directEnergies[m][hour] += r.globalRadiation_MJm2 * (1 - r.diffuseToGlobalRatio);

				}
			}
			doyMonthFirstDay += SLBeamSetFactory.NBMONTHDAYS[m];

		}

		int[] meanDoy = SLBeamSetFactory.meanDoyPerMonth(leavedPeriodStartDoy, leavedPeriodEndDoy);

		// Common for keyDoy and leavedPeriod

		double diffuseIncidentEnergy = 1; // fc+fa-3.8.2017

		SLBeamSetFactoryOptimized.classicDiffuseRaysCreation(null, "LEAVED_PERIOD", SOC, diffuseIncidentEnergy,
				diffuseEnergies, diffuseAngleStep_rad, heightAngleMin_rad, slope_rad, bottomAzimut_rad, beamSet);

		// Specific for leavedPeriod
		SLBeamSetFactoryOptimized.periodDirectHourRaysCreation("LEAVED_PERIOD", latitude_rad, longitude_rad,
				declination_rad, directEnergies, heightAngleMin_rad, slope_rad, southAzimut_ccw_rad, bottomAzimut_rad,
				meanDoy, GMT, beamSet);

	}

	/**
	 * Direct rays are created along one solar path for one single day, ray energy
	 * is proportional to sin(heightAngle)
	 */
	public static void keyDoyDirectHourRaysCreation(SLKeyDoy keyDoy, String tagPrefix, int yearOfTheBeamSet,
			double latitude_rad, double longitude_rad, double[] declination_rad, double[] directEnergies,
			double angleMin_rad, double slope_rad, double southAzimut_rad, double bottomAzimut_rad, int GMT,
			SLBeamSet bs) {
		double heightAngle_rad;
		double azimut_rad;
		// double energy;
		double horizontalDirect = 0;
		double slopeDirect = 0;
		int count = 0;
		double lostEnergy = 0; // radiations recorded during the night

		int m = getMonth(yearOfTheBeamSet, keyDoy.doy); // [0, 11]
		int d = getDay(yearOfTheBeamSet, keyDoy.doy);

		lostEnergy = 0;
		
		//cr-02.02.2022
		double lostSlopeDirect = 0;
		for (int h = 0; h < 24; h++) { // for each hour

			double directEnergy = directEnergies[h];
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
				double B = 2 * Math.PI * keyDoy.doy / 365.242; // wikipedia
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

				heightAngle_rad = Math.asin(Math.sin(latitude_rad) * Math.sin(declination_rad[m])
						+ Math.cos(latitude_rad) * Math.cos(declination_rad[m]) * Math.cos(hourAngle));
				azimut_rad = SLBeamSetFactory.sunAzimut(latitude_rad, declination_rad[m], hourAngle, heightAngle_rad,
						southAzimut_rad);
				
				double heightAngle_deg = Math.toDegrees(heightAngle_rad);
				double azimuth_deg = Math.toDegrees(azimut_rad);

				if (heightAngle_rad > angleMin_rad) {
					horizontalDirect += directEnergy;
				} else {
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
				double scalar = Math.cos(slope_rad) * Math.sin(heightAngle_rad)
						+ Math.sin(slope_rad) * Math.cos(heightAngle_rad) * Math.cos(azimut_rad - bottomAzimut_rad);

				if ((heightAngle_rad > 0) && (scalar > Math.sin(angleMin_rad))) {

					SLBeam b = new SLBeam(azimut_rad, heightAngle_rad, perpendicularEnergyRay, true);

					if (keyDoy != null)
						b.setDoy(keyDoy.doy);
					// b.setLadProportion(keyDoy.getLadProportionMap()); //
					// REMOVED

					b.setTag(new SLTag(tagPrefix + "_" + h));
					bs.addBeam(b);

					count++;

					slopeDirect += scalar * perpendicularEnergyRay;

					HetReporter.printInLog("SamsaraLight",
							(m + 1) + ";" + h + ";" + localSolarTime + ";" + Math.toDegrees(hourAngle) + ";"
									+ heightAngle_rad + ";" + azimut_rad + ";" + perpendicularEnergyRay + ";"
									+ (scalar * perpendicularEnergyRay) + ";" + false);

				} else if ((heightAngle_rad > 0) && (scalar <= Math.sin(angleMin_rad))) {
					HetReporter.printInLog("SamsaraLight",
							(m + 1) + ";" + h + ";" + localSolarTime + ";" + Math.toDegrees(hourAngle) + ";"
									+ heightAngle_rad + ";" + azimut_rad + ";" + perpendicularEnergyRay + ";"
									+ (scalar * perpendicularEnergyRay) + ";" + true);

					//cr-02.02.2022
					lostSlopeDirect += scalar * perpendicularEnergyRay;	
				}else {
					//cr-02.02.2022
					lostSlopeDirect += scalar * perpendicularEnergyRay;
				}

				// horizontal/slope direct/diffuse vars are set only for the
				// leaved period
				// this method is dedicated to keyDoys -> commented 2 lines
				// below
				// bs.setHorizontalDirect(horizontalDirect);
				// bs.setSlopeDirect(slopeDirect);
				// fa-9.8.2017: moved below
			}
		}
		// fc+fa-4.8.2017 for leaved period AND keyDoys
		bs.setHorizontalDirect(bs.getHorizontalDirect() + horizontalDirect);
		bs.setSlopeDirect(bs.getSlopeDirect() + slopeDirect);
		
		//cr-02.02.2022
		if(bs.getLostSlopeEnergyDirect() == -1) // initialization
			bs.setLostSlopeEnergyDirect(lostSlopeDirect);
		else
			bs.setLostSlopeEnergyDirect(bs.getLostSlopeEnergyDirect() + lostSlopeDirect);

		// }

		HetReporter.printInStandardOutput("SLBeamSetFactoryOptimized, keyDoyDirectHourRaysCreation() tagPrefix: "
				+ tagPrefix + " created: " + count + " DIRECT beams, horizontalDirect: " + horizontalDirect
				+ ", slopeDirect: " + slopeDirect);

	} // end-of-directHourRaysCreationForOneDay ()

	/**
	 * Diffuse beams are created with a classical sky hemisphere divided by
	 * meridians and parallels Standard Overcast Sky and Uniform Overcast Sky are
	 * possible. diffuseEnergies contains 24 sums of diffuse energy values for the
	 * period.
	 */
	public static void classicDiffuseRaysCreation(SLKeyDoy keyDoy, String tagPrefix, boolean SOC,
			double diffuseIncidentEnergy, double[] diffuseEnergies, double diffuseAngleStep_rad, double angleMin_rad,
			double slope_rad, double bottomAzimut_rad, SLBeamSet bs) {

		// when this method is called for the leaved period, parameter keyDoy ==
		// null

		int count = 0;
		double horizontalDiffuse = 0; // fa-20.12.2017: converted from float to double to avoid rounding error side
										// effects
		double slopeDiffuse = 0; // fa-20.12.2017: converted from float to double to avoid rounding error side
									// effects
		double lostSlopeDiffuse = 0; // fa-31.05.2017, fa-20.12.2017: converted from float to double to avoid
										// rounding error side effects
		double heightAngle_rad = diffuseAngleStep_rad / 2;
		while (heightAngle_rad < Math.PI / 2) {
			double azimut = diffuseAngleStep_rad / 2; // If azimut starts from
			// 0, there can be round problems with transformation of angleStep
			// from degrees to radians and the last azimut can be very close to
			// 360 (one extra azimut)
			while (azimut < 2 * Math.PI) { // azimut is with anticlockwise

				// fc+fa-3.8.2017 1 beam per day / one for the complete leaf
				// development period only (optimized version)
				// for (int h = 0; h < 24; h++) {

				// (trigonometric) rotation from X axis
				double energy = SLBeamSetFactory.classicDiffuseEnergy(SOC, heightAngle_rad, diffuseAngleStep_rad,
						diffuseIncidentEnergy);

				if (energy <= 0)
					continue; // next

				// A beam is created only if it reaches the soil with an
				// angle >
				// angleMin the cosinus of the angle between the vector
				// orthogonal to slope and the beam must be higher than
				// sin(angleMin) this cosinus is given by scalar
				double scalar = Math.cos(slope_rad) * Math.sin(heightAngle_rad)
						+ Math.sin(slope_rad) * Math.cos(heightAngle_rad) * Math.cos(azimut - bottomAzimut_rad);

				for (int h = 0; h < 24; h++) {
					// The HorizontalDiffuse reference is calculated for
					// heightAngles > angleMin
					if (heightAngle_rad > angleMin_rad)
						horizontalDiffuse += diffuseEnergies[h] * energy * Math.sin(heightAngle_rad);

					// SlopeDiffuse
					if (scalar > Math.sin(angleMin_rad))
						slopeDiffuse += diffuseEnergies[h] * scalar * energy;
					else
						lostSlopeDiffuse += diffuseEnergies[h] * scalar * energy;
				}

				// Beam creation
				if (scalar > Math.sin(angleMin_rad)) {
					SLBeam b = new SLBeam(azimut, heightAngle_rad, energy, false); // false:
																					// diffuse

					if (keyDoy != null)
						b.setDoy(keyDoy.doy);
					// b.setLadProportion(keyDoy.getLadProportionMap()); //
					// REMOVED

					// fc+fa-3.8.2017 Optimized mode results in a deferred
					// tagResult expansion process, will rely on these
					// diffuseEnergies per hour
					b.setTag(new SLTagOptimized(tagPrefix, diffuseEnergies));
					// b.setTag(new SLTag(tagPrefix));

					// b.setTag(new SLTag(tagPrefix + "_" + h));
					bs.addBeam(b);
					count++;
				}

				azimut += diffuseAngleStep_rad;
			}
			heightAngle_rad += diffuseAngleStep_rad;

//				// The HorizontalDiffuse reference is calculated for
//				// heightAngles > angleMin
//				if (heightAngle_rad > angleMin_rad) {
//					for (int h = 0; h < 24; h++) {
//						horizontalDiffuse += diffuseEnergies[h] * energy * Math.sin(heightAngle_rad);
//					}
//				}
//
//
//				if (scalar > Math.sin(angleMin_rad)) {
//					SLBeam b = new SLBeam(azimut, heightAngle_rad, energy, false); // false:
//																					// diffuse
//
//					if (keyDoy != null)
//						b.setDoy(keyDoy.doy);
//					// b.setLadProportion(keyDoy.getLadProportionMap()); //
//					// REMOVED
//
//
//
//					// fc+fa-3.8.2017 Optimized mode results in a deferred
//					// tagResult expansion process, will rely on these
//					// diffuseEnergies per hour
//					b.setTag(new SLTagOptimized(tagPrefix, diffuseEnergies));
//					// b.setTag(new SLTag(tagPrefix));
//
//					// b.setTag(new SLTag(tagPrefix + "_" + h));
//					bs.addBeam(b);
//
//					// fa-08.01.2018
//					for (int h = 0; h < 24; h++) {
//						slopeDiffuse += diffuseEnergies[h] * scalar * energy;
//					}
//					count++;
//				}
//
//				// fa-31.05.2017, 08.01.2018
//				else {
//					for (int h = 0; h < 24; h++) {
//						lostSlopeDiffuse += diffuseEnergies[h] * scalar * energy;
//					}
//				}
//
//				azimut += diffuseAngleStep_rad;
//			}
//			heightAngle_rad += diffuseAngleStep_rad;
		}

		// horizontal/slope direct/diffuse vars are set only for the leaved
		// period
		// this method is called for leaved period and for keyDoys
		// -> run 2 lines below only if leaved period
		// fc+fa-4.8.2017 see below
//		if (keyDoy == null) {
//			bs.setHorizontalDiffuse(horizontalDiffuse);
//			bs.setSlopeDiffuse(slopeDiffuse);
//
//			// fa-31.05.2017
//			System.out.println("lostSlopeDiffuse= " + lostSlopeDiffuse);
//		}

		// fc+fa-4.8.2017 for leaved period AND keyDoys
		bs.setHorizontalDiffuse(bs.getHorizontalDiffuse() + horizontalDiffuse);
		bs.setSlopeDiffuse(bs.getSlopeDiffuse() + slopeDiffuse);
		
		//cr-xx.01.2022
		if(bs.getLostSlopeEnergyDiffuse() == -1) // initialization
			bs.setLostSlopeEnergyDiffuse(lostSlopeDiffuse);
		else
			bs.setLostSlopeEnergyDiffuse(bs.getLostSlopeEnergyDiffuse() + lostSlopeDiffuse);

		HetReporter.printInStandardOutput("SLBeamSetFactoryOptimized, classicDiffuseRaysCreation() tagPrefix: "
				+ tagPrefix + " created: " + count + " DIFFUSE beams, horizontalDiffuse: " + horizontalDiffuse
				+ ", slopeDiffuse: " + slopeDiffuse + ", lostSlopeDiffuse: " + lostSlopeDiffuse);
	} // end-of-classicDiffuseRaysCreation ()

	/**
	 * Direct rays are created along one solar path per month During a day, ray
	 * energy is proportional to sin(heightAngle)
	 */
	public static void periodDirectHourRaysCreation(String tagPrefix, double latitude_rad, double longitude_rad,
			double[] declination_rad, double[][] directEnergies, double angleMin_rad, double slope_rad,
			double southAzimut_rad, double bottomAzimut_rad, int[] meanDoy, int GMT, SLBeamSet bs) {
		double heightAngle_rad;
		double azimut_rad;
		// double energy;
		double horizontalDirect = 0;
		double slopeDirect = 0;
		double lostSlopeDirect = 0; // fa-31.05.2017
		int count = 0;
		double lostEnergy = 0; // radiations recorded during the night

		for (int m = 0; m < 12; m++) { // for each month

			lostEnergy = 0;
			for (int h = 0; h < 24; h++) { // for each hour

				double directEnergy = directEnergies[m][h];
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

					heightAngle_rad = Math.asin(Math.sin(latitude_rad) * Math.sin(declination_rad[m])
							+ Math.cos(latitude_rad) * Math.cos(declination_rad[m]) * Math.cos(hourAngle));
					azimut_rad = SLBeamSetFactory.sunAzimut(latitude_rad, declination_rad[m], hourAngle,
							heightAngle_rad, southAzimut_rad);

					if (heightAngle_rad > angleMin_rad) {
						horizontalDirect += directEnergy;
					} else {
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
					double scalar = Math.cos(slope_rad) * Math.sin(heightAngle_rad)
							+ Math.sin(slope_rad) * Math.cos(heightAngle_rad) * Math.cos(azimut_rad - bottomAzimut_rad);

					if ((heightAngle_rad > 0) && (scalar > Math.sin(angleMin_rad))) {

						SLBeam b = new SLBeam(azimut_rad, heightAngle_rad, perpendicularEnergyRay, true);

						// fc+fa-16.5.2017 period: no keyDoy, no ladProportion

						b.setTag(new SLTag(tagPrefix + "_" + h));
						bs.addBeam(b);

						count++;

						slopeDirect += scalar * perpendicularEnergyRay;

						HetReporter.printInLog("SamsaraLight",
								(m + 1) + ";" + h + ";" + localSolarTime + ";" + Math.toDegrees(hourAngle) + ";"
										+ heightAngle_rad + ";" + azimut_rad + ";" + perpendicularEnergyRay + ";"
										+ (scalar * perpendicularEnergyRay) + ";" + false);

					} else if ((heightAngle_rad > 0) && (scalar <= Math.sin(angleMin_rad))) {

						HetReporter.printInLog("SamsaraLight",
								(m + 1) + ";" + h + ";" + localSolarTime + ";" + Math.toDegrees(hourAngle) + ";"
										+ heightAngle_rad + ";" + azimut_rad + ";" + perpendicularEnergyRay + ";"
										+ (scalar * perpendicularEnergyRay) + ";" + true);

						// fa-31.05.2017
						lostSlopeDirect += scalar * perpendicularEnergyRay;
					}else {
						//cr-02.02.2022
						lostSlopeDirect += scalar * perpendicularEnergyRay;
					}

					// horizontal/slope direct/diffuse vars are set only for the
					// leaved period
					// this method is called for leaved period only
					// -> run 2 lines below
//					bs.setHorizontalDirect(horizontalDirect);
//					bs.setSlopeDirect(slopeDirect);
					// fa-9.8.2017: moved below
				}
			}

		}
		// fc+fa-4.8.2017 for leaved period AND keyDoys
		bs.setHorizontalDirect(bs.getHorizontalDirect() + horizontalDirect);
		bs.setSlopeDirect(bs.getSlopeDirect() + slopeDirect);

		HetReporter.printInStandardOutput("SLBeamSetFactoryOptimized, periodDirectHourRaysCreation() tagPrefix: "
				+ tagPrefix + " created: " + count + " DIRECT beams, horizontalDirect: " + horizontalDirect
				+ ", slopeDirect: " + slopeDirect);

		// fa-31.05.2017
		HetReporter.printInStandardOutput("lostSlopeDirect= " + lostSlopeDirect);
		
		//cr-02.02.2022
		if(bs.getLostSlopeEnergyDirect()  == -1) // initialization
			bs.setLostSlopeEnergyDirect(lostSlopeDirect);
		else
			bs.setLostSlopeEnergyDirect(bs.getLostSlopeEnergyDirect() + lostSlopeDirect);

	} // end-of-directHourRaysCreationForSeveralMonths ()

	/**
	 * Returns the month of the given doy, in [0,11], January is 0.
	 */
	public static int getMonth(int year, int doy) {
		if (calendar == null)
			calendar = new GregorianCalendar();

		calendar.set(GregorianCalendar.YEAR, year);
		calendar.set(GregorianCalendar.DAY_OF_YEAR, doy);

		return calendar.get(Calendar.MONTH); // month in [0,11]
	}

	/**
	 * Returns the day [1, 31] of the given doy [1, 366].
	 */
	public static int getDay(int year, int doy) {
		if (calendar == null)
			calendar = new GregorianCalendar();

		calendar.set(GregorianCalendar.YEAR, year);
		calendar.set(GregorianCalendar.DAY_OF_YEAR, doy);

		return calendar.get(Calendar.DAY_OF_MONTH); // day in [1,31]
	}

	public SLRadiation getRadiation() {
		return radiation;
	}

	public SLBeamSet getBeamSet() {
		return beamSet;
	}

}
