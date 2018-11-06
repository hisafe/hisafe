package safe.model;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.ShiftItem;
import capsis.defaulttype.plotofcells.Neighbour;
import capsis.util.Point2D;

/**
 * LIGHT MODEL PROCESS
 * 
 * @author B. Courbaud CEMAGREF Grenoble - January 2000 - Benoit.Courbaud@grenoble.cemagref.fr
 * 
 *         Updated by G. Talbot INRA Montpellier. February 2007, October 2007 Direct radiation can
 *         be computed as diffuse one. Direct energy is shared between sky discretization sectors.
 *         Toric symetry is now facultative.
 **/
public class SafeLightModel {

	/**
	 * Beam set initialisation
	 * 
	 */
	public static void initialiseBeamSet (SafeMacroClimat macroClimat, SafePlotSettings plotSettings,
			SafeEvolutionParameters simulationSettings, SafeInitialParameters safeSettings, int nbImpact) {
		// 1. Beam Impacts creation for process lighting
		// Because 1 single beam in the center of each cell is not efficient
		createCellImpacts (plotSettings, safeSettings, nbImpact);

		// 2. Create SafeBeamSet
		SafeBeamSet beamSet = createBeamSet (safeSettings, plotSettings);

		macroClimat.setBeamSet (beamSet);
	}

	/**
	 * beamSet and beams creation, calcultation of proportions of diffusePar and InfraRed attributed
	 * to each beam
	 */

	public static SafeBeamSet createBeamSet (SafeInitialParameters safeSettings, SafePlotSettings plotSettings) {
		double heightAngle;
		double azimut;
		float diffuseEnergy;
		double energyConvFactor;


		boolean SOC = safeSettings.SOC;
		boolean turtleOption = safeSettings.turtleOption; // if true : turtle sky else : classical sky
		double angleStep = Math.toRadians (safeSettings.diffuseAngleStep); // used if classical sky

		// if there is a slope
		double slope = Math.toRadians (plotSettings.slopeIntensity); // slope intensity
		double slopeAspect = plotSettings.slopeAspect; // angle between north and slope bottom (clockwise rottion);
		double treeLineOrientation = plotSettings.treeLineOrientation; // angle between north and treeline (clockwise rotation)
		double bottomAzimut = Math.toRadians (-90 + treeLineOrientation - slopeAspect); // azimut of slope bottom (from x axis, trigo rotation)
		double skyDiffuseMask = 0; // proportion of diffuse radiation reaching the scene
		double skyInfraRedMask = 0; // proportion of infra-red radiation reaching the scene

		SafeBeamSet bs = new SafeBeamSet ();

		// TURTLE OPTION IS ACTIVATED
		if (turtleOption) {

			// for each solid directions of the "turtle"
			for (int i = 0; i < safeSettings.NB_TURTLE_BEAM; i++) {

				// height, azimut and energy are fixed in settings
				heightAngle = Math.toRadians (safeSettings.LIGHT_TURTLE_EL[i]);
				azimut = Math.toRadians (safeSettings.LIGHT_TURTLE_AZ[i]);
				if (SOC) // standard overcast sky
					diffuseEnergy = (float) (safeSettings.LIGHT_TURTLE_SOC[i]); // No dimension
				else
					// uniform overcast sky
					diffuseEnergy = (float) (safeSettings.LIGHT_TURTLE_UOC[i]); // No dimension

				energyConvFactor = 1; // relative energy of beam reaching an horizontal surface of 1m2
				energyConvFactor = 1 / Math.sin (heightAngle); // relative energy of a beam of section 1m2

				// section of a beam lightening a unit surface on the slope
				double scalar = Math.cos (slope) * Math.sin (heightAngle) + Math.sin (slope) * Math.cos (heightAngle)
						* Math.cos (azimut - bottomAzimut);

				if (scalar > 0) { // the beam reaches the scene
					if (scalar < 0.01) { // to avoid computation of quasi-parallel to the soil beam
											// computation (to reduce computing time)
						heightAngle += Math.toRadians (1);
						scalar = Math.cos (slope) * Math.sin (heightAngle) + Math.sin (slope) * Math.cos (heightAngle)
								* Math.cos (azimut - bottomAzimut);
					}
					energyConvFactor *= scalar; // relative energy of a beam lightening a unit surface on the scene
				} else { // the beam can't reach the scene
					energyConvFactor = 0; // in this case, beam energy is 0
				}
				energyConvFactor /= Math.cos (slope); // relative energy of a beam lightening a
														// surface which horizontal projection is 1m2
				skyDiffuseMask += energyConvFactor * diffuseEnergy;

				SafeBeam b = new SafeBeam (azimut, heightAngle, diffuseEnergy, 0, 0, (float) energyConvFactor);

				b.setInfraRedEnergy ((float) safeSettings.LIGHT_TURTLE_IR[i]); // infra-red energy is fixed in settings
				skyInfraRedMask += (float) energyConvFactor * safeSettings.LIGHT_TURTLE_IR[i];

				bs.addBeam (b);
			}
		} else // TURTLE OPTION IS DESACTIVATED
		{
			heightAngle = angleStep / 2; // first elevtion angle

			while (heightAngle < Math.PI / 2) {

				azimut = angleStep / 2; // If azimut starts from 0, there can be round problems with
										// transformation of angleStep from degrees to radians and
										// the last azimut can be very close to 360 (one extra
										// azimut)

				while (azimut < 2 * Math.PI) { // azimut is with trigonometric rotation from X axis

					double meridianNb = 2 * Math.PI / angleStep; // number of beams for a given heightAngle
					double heightAInf = heightAngle - angleStep / 2;
					double sinInf = Math.sin (heightAInf);
					double heightASup = heightAngle + angleStep / 2;
					double sinSup = Math.sin (heightASup);

					if (SOC) // standard overcast sky
						diffuseEnergy = (float) ((6 / (7 * meridianNb)) * ((Math.pow (sinSup, 2) - Math.pow (sinInf, 2)) / 2 + 2 * (Math
								.pow (sinSup, 3) - Math.pow (sinInf, 3)) / 3)); // no units
					else
						// uniform overcast sky
						diffuseEnergy = (float) ((2 / meridianNb) * (sinSup * sinSup - sinInf * sinInf) / 2); // no  units

					energyConvFactor = 1; // relative energy of beam reaching an horizontal surface
											// of 1m2
					energyConvFactor /= Math.sin (heightAngle); // relative energy of a beam of
																// section 1m2

					// section of a beam lightening a unit surface on the slope
					double scalar = Math.cos (slope) * Math.sin (heightAngle) + Math.sin (slope)
							* Math.cos (heightAngle) * Math.cos (azimut - bottomAzimut);

					if (scalar > 0) { // the beam reaches the scene
						if (scalar < 0.01) { // to avoid computation of quasi-parallel to the soil
												// beam computation (to reduce computing time)
							heightAngle += Math.toRadians (1);
							scalar = Math.cos (slope) * Math.sin (heightAngle) + Math.sin (slope)
									* Math.cos (heightAngle) * Math.cos (azimut - bottomAzimut);
						}
						energyConvFactor *= scalar; // relative energy of a beam lightening a unit
													// surface on the scene
					} else { // the beam can't reach the scene
						energyConvFactor = 0;
					}

					energyConvFactor /= Math.cos (slope); // relative energy of a beam lightening a
															// surface which horizontal projection
															// is 1m2
					skyDiffuseMask += energyConvFactor * diffuseEnergy;

					SafeBeam b = new SafeBeam (azimut, heightAngle, diffuseEnergy, 0, 0, (float) energyConvFactor);
					float infraRedEnergy = (float) (2 * Math.sin (heightAngle) * Math.cos (heightAngle)
							* Math.sin (angleStep) / meridianNb); // Lambertian emmisivity

					b.setInfraRedEnergy (infraRedEnergy);
					skyInfraRedMask += energyConvFactor * infraRedEnergy;

					bs.addBeam (b);
					azimut += angleStep;
				}
				heightAngle += angleStep;
			}
		}

		bs.setSkyDiffuseMask (Math.max (0, skyDiffuseMask));
		bs.setSkyInfraRedMask (Math.max (0, skyInfraRedMask));

		return bs;
	}

	/**
	 * Add direct energy in SafeBeamSet For each beam of the beamSet, this method adds the
	 * proportion of the daily direct energy allocated to this beam. The proportion of direct energy
	 * reaching the scene (slope computation) is calculated (skyDirectMask) Gregoire Talbot
	 **/
	public static void beamDirectEnergy (SafeInitialParameters safeSettings, SafeBeamSet beamSet,
			SafeDailyClimat dailyClimat, SafePlotSettings plotSettings) {


		double timeStep = safeSettings.timeStep; // time (in hours) between two calculations of Sun position
		double dayLength = (double) dailyClimat.getDayLength (); // hours
		int nbTimeStep = (int) Math.round (dayLength / timeStep); // number of calculations of sun position
		int nbTimeStepMax = safeSettings.nbTimeStepMax; // maximal number of calculations

		if (nbTimeStep > nbTimeStepMax) {
			nbTimeStep = nbTimeStepMax;
			timeStep = dayLength / nbTimeStep; // hours
		}

		double skyDirectMask = 0; // proportion of direct radiation reaching the scene

		int nbBeam = (int) beamSet.getSize ();
		float[] directEnergy = new float[nbBeam]; // no dimension

		// for each beam : reset directEnergy
		int j = 0;
		for (Iterator ite = beamSet.getBeams ().iterator (); ite.hasNext ();) {
			SafeBeam b = (SafeBeam) ite.next ();
			b.resetDirectEnergy ();
			directEnergy[j] = 0;
			j++;
		}

		// azimut of treeLine (= -y axis) from North (radians, clockwise rotation)
		double treeLineOrientation = Math.toRadians (plotSettings.treeLineOrientation);
		// azimut of South direction from x axis (radians, trigonometric rotation)
		double southAzimut = (treeLineOrientation - 3 * Math.PI / 2);

		float sunDeclination = dailyClimat.getSunDeclination (); // radians

		double sidec = Math.sin (sunDeclination); // sine of sun declination
		double codec = Math.cos (sunDeclination); // cosine of sun declination
		double latitude = Math.toRadians (plotSettings.plotLatitude);
		double silat = Math.sin (latitude); // sine of plot latitude
		double colat = Math.cos (latitude); // cosine of plot latitude

		double heightAngle; // beam elevation (radians)
		double azimut; // beam azimut from x axis (radians, trigo rotation)
		float sumPar = 0; // used for distribution of direct radiation between time steps.
		double radius = Math.acos (1 - (1 / ((float) (nbBeam)))); // sector mean radius, expressed as plane angle : acos (1 - 1/nb_sectors)

		double angle; // half angle between a beam and solar position
		double[] weight; // weight of a sky sector for representing a solar position
		double sumWeight;

		double dayTime = -dayLength / 2;

		// for each Time step
		// calculation of sun azimut and elevation
		double[] sunElevationSerie = new double[nbTimeStep]; // radians
		double[] sinSunElevationSerie = new double[nbTimeStep]; // radians
		double[] sunAzimutSerie = new double[nbTimeStep]; // radians
		double[] directParSerie = new double[nbTimeStep]; // directParSerie[i] = fraction of direct radiation allocated to timeStep i.
															// (proportionnal to sin(sunElevationSerie[i]))

		// qm+sa 11.08.2014 log the sun elevation
		// String lineToLog = "";
		// lineToLog += dailyClimat.getYear() + "\t" + dailyClimat.getMonth() + "\t" +
		// dailyClimat.getDay() + "\t" + dailyClimat.getDayLength();

		for (int i = 0; i < nbTimeStep; i++) {

			sinSunElevationSerie[i] = Math.max (silat * sidec + colat * codec * Math.cos ((Math.PI / 12) * dayTime), 0);
			sunElevationSerie[i] = Math.asin (sinSunElevationSerie[i]);

			sumPar += sinSunElevationSerie[i];

			// qm+sa 11.08.2014 log the sun elevation
			// lineToLog += "\t" + sunElevationSerie[i];

			double coasi = (silat * codec * Math.cos ((Math.PI / 12) * dayTime) - colat * sidec)
					/ Math.cos (sunElevationSerie[i]);
			if (dayTime > 0) sunAzimutSerie[i] = Math.acos (coasi) * 1;
			if (dayTime < 0) sunAzimutSerie[i] = Math.acos (coasi) * (-1);
			if (dayTime == 0) sunAzimutSerie[i] = 0;

			// convert azimut from South direction to azimut from x axis.
			sunAzimutSerie[i] += southAzimut;

			// shift sunAzimut into [0, 2PI]
			if (sunAzimutSerie[i] < 0) sunAzimutSerie[i] += 2 * Math.PI;

			if (sunAzimutSerie[i] > (2 * Math.PI)) sunAzimutSerie[i] -= 2 * Math.PI;

			dayTime += timeStep;
		}
		// qm+sa 11.08.2014 log the sun elevation
		// Log.println ("light_model_info", lineToLog);

		// for each time step
		for (int i = 0; i < nbTimeStep; i++) {

			directParSerie[i] = sinSunElevationSerie[i] / sumPar; // fraction of direct radiation allocated to timestep i
			weight = new double[nbBeam];
			sumWeight = 0;
			j = 0;

			// for each beam of the SafeBeamSet
			for (Iterator ite = beamSet.getBeams ().iterator (); ite.hasNext ();) {
				SafeBeam b = (SafeBeam) ite.next ();
				azimut = b.getAzimut_rad ();
				heightAngle = b.getHeightAngle_rad ();
				double deltaAZ = Math.abs (azimut - sunAzimutSerie[i]); // difference between sun and beam azimut
				if (deltaAZ > Math.PI) {
					deltaAZ = 2 * Math.PI - deltaAZ;
				} // shift into [-Pi;Pi]
				double deltaEL = Math.abs (heightAngle - sunElevationSerie[i]); // difference between sun and beam position
				angle = Math.acos (Math.cos (deltaAZ) * Math.cos (deltaEL)) / 2; // half angle between beam and solar position
																					// (uses Pithagore's theorem in spheric triangle)
				if (angle > radius) // if no intersection with beam sector
					weight[j] = 0;
				else {

					double h = Math.sqrt (Math.pow (radius, 2) - Math.pow (angle, 2));
					double alpha = Math.acos (angle / radius);
					weight[j] = 2 * (alpha * (Math.pow (radius, 2)) - (h * angle)); // weight = lumen area (intersection of sun and sector cones)
				}
				sumWeight += weight[j];
				j++;
			}// end of beams

			// for each beam
			j = 0;
			for (Iterator ite = beamSet.getBeams ().iterator (); ite.hasNext ();) {
				SafeBeam b = (SafeBeam) ite.next ();
				if (sumWeight != 0) directEnergy[j] += (float) (directParSerie[i] * weight[j] / sumWeight);
				j++;
			}// end of beams
		}// end of time steps

		// for each beam
		j = 0;
		for (Iterator ite = beamSet.getBeams ().iterator (); ite.hasNext ();) {
			SafeBeam b = (SafeBeam) ite.next ();
			heightAngle = b.getHeightAngle_rad ();

			azimut = b.getAzimut_rad ();

			directEnergy[j] = Math.max (0, directEnergy[j]);

			b.setDirectEnergy (directEnergy[j]);

			skyDirectMask += directEnergy[j] * b.getInitialEnergy ();

			j++;
		}// end of beams

		beamSet.setSkyDirectMask (Math.max (0, skyDirectMask));
	}

	/**
	 * Computes relative cell neighbourhoods for each beam in the beam set. The trees which can
	 * intercept a beam are located in cells with their center located in a competition rectangle of
	 * length L (beam direction) + R (opposite direction) and width R (directions perpendicular to
	 * beam).
	 * 
	 * 
	 * Updated by I.Lecomte INRA Montpellier - April 2003 There is now several impacts on each cell
	 * (not only the center) Rectangle size is now the maximum size of trees on the stand for this
	 * step instead of maximum size for the tree species read in settings
	 **/
	public static void computeRelativeCellNeighbourhoods (SafeBeamSet beamSet, SafePlotSettings plotSettings,
			double heightMax, double crownRadiusMax, boolean isDiffus) {

		double treeLineOrientation = plotSettings.treeLineOrientation;
		double slopeAspect = plotSettings.slopeAspect;
		double bottomAzimut = Math.toRadians (-90 + treeLineOrientation - slopeAspect); // azimut of
																						// the
																						// maximum
																						// slope
																						// line / x
																						// axis
		double slope = Math.toRadians (plotSettings.slopeIntensity); // azimut of the maximum slope
																		// line / x axis
		double width = plotSettings.cellWidth; // cell width (meters)
		// double reach = (double) Math.min (plotSettings.plotWidth, plotSettings.plotHeight); //
		// max reach (meters) : min (plot width, plot height)

		for (Iterator ite = beamSet.getBeams ().iterator (); ite.hasNext ();) {
			SafeBeam b = (SafeBeam) ite.next ();

			// Remove all neighbourgs from last simulation
			b.removeAllNeighbourCell ();

			if ((isDiffus) || (b.getDirectEnergy () != 0)) { // don't compute if only isDirect, and
																// if DirectEnergy==0
				double azimut = b.getAzimut_rad ();
				double hAngle = b.getHeightAngle_rad ();

				// Computes lateral = the boundary to add to the competition rectangle
				// to take into account cells center instead of trees position.
				// The boundary depends on beam azimut.
				double azt;
				if (azimut < Math.PI / 4) {
					azt = azimut;
				} else if ((azimut >= Math.PI / 4) && (azimut < Math.PI / 2)) {
					azt = Math.PI / 2 - azimut;
				} else if ((azimut >= Math.PI / 2) && (azimut < 3 * Math.PI / 4)) {
					azt = azimut - Math.PI / 2;
				} else if ((azimut >= 3 * Math.PI / 4) && (azimut < Math.PI)) {
					azt = Math.PI - azimut;
				} else if ((azimut >= Math.PI) && (azimut < 5 * Math.PI / 4)) {
					azt = azimut - Math.PI;
				} else if ((azimut >= 5 * Math.PI / 4) && (azimut < 3 * Math.PI / 2)) {
					azt = 3 * Math.PI / 2 - azimut;
				} else if ((azimut >= 3 * Math.PI / 2) && (azimut < 7 * Math.PI / 4)) {
					azt = azimut - 3 * Math.PI / 2;
				} else if (azimut >= 7 * Math.PI / 4) {
					azt = 2 * Math.PI - azimut;
				} else
					azt = 0;

				// lateral is multiplied by 2 to aim the corner of neibourgs cell instead of its
				// center
				// width/Math.sqrt(2) is half of the diagonale of the squareCell
				double lateral = 2 * width / Math.sqrt (2) * Math.sin (azt + Math.PI / 4);

				// Beam width = max lateral distance from the beam to a cell center
				// able to own a tree which can intercept the beam
				double R = crownRadiusMax + lateral;

				// Beam reach maximum distance along the beam beyond which the cells cannot own
				// trees which can intercept the beam (too high)
				double L = heightMax / (Math.tan (hAngle) + Math.cos (azimut - bottomAzimut) * Math.tan (slope))
						+ lateral;

				double sinA = Math.sin (azimut);
				double cosA = Math.cos (azimut);

				// Coordinates of the four corners of the competition rectangle
				double x1 = R * sinA + L * cosA;
				double y1 = L * sinA - R * cosA;
				double x2 = L * cosA - R * sinA;
				double y2 = L * sinA + R * cosA;
				double x3 = R * (sinA - cosA);
				double y3 = -R * (sinA + cosA);
				double x4 = -R * (sinA + cosA);
				double y4 = R * (cosA - sinA);

				double xMin = Math.min (x1, x2);
				xMin = Math.min (xMin, x3);
				xMin = Math.min (xMin, x4);

				double xMax = Math.max (x1, x2);
				xMax = Math.max (xMax, x3);
				xMax = Math.max (xMax, x4);

				double yMin = Math.min (y1, y2);
				yMin = Math.min (yMin, y3);
				yMin = Math.min (yMin, y4);

				double yMax = Math.max (y1, y2);
				yMax = Math.max (yMax, y3);
				yMax = Math.max (yMax, y4);

				// Round of xMin, xMax, yMin, yMax with ceil or floor as necessary
				// -> an xCenter relatively to target
				int aux = (int) Math.floor (xMax / width); // xMax > 0 : round to maximum int < xMax
				xMax = aux * width;
				aux = (int) Math.floor (yMax / width);
				yMax = aux * width;
				aux = (int) Math.ceil (xMin / width); // xMin < 0 : round to minimum int > xMin
				xMin = aux * width;
				aux = (int) Math.ceil (yMin / width);
				yMin = aux * width;

				// Creates an array of cell indexes between extreme competition rectangle
				// coordinates
				int iMax = (int) ((yMax - yMin) / width + 0.5) + 1; // i e [0, iMax] : iMax+1
																	// elements
				int jMax = (int) ((xMax - xMin) / width + 0.5) + 1; // j e [0, jMax] : jMax+1
																	// elements

				Point2D.Double[][] tabCell = new Point2D.Double[iMax][jMax];
				int i = 0;
				int j = 0;

				iMax = 0;
				jMax = 0;

				for (double y = yMax; y >= yMin; y -= width) {
					for (double x = xMin; x <= xMax; x += width) {

						tabCell[i][j] = new Point2D.Double (x, y); // a candidate cell (its center)
						j++;
					}
					if (j - 1 > jMax) {
						jMax = j - 1;
					}
					j = 0;
					i++;
				}
				iMax = i - 1;

				// Explores the array of cell indexes to extract the cells which are located
				// inside the competition rectangle (depending of the azimut)
				for (i = 0; i <= iMax; i++) {
					for (j = 0; j <= jMax; j++) {
						double x = tabCell[i][j].x;
						double y = tabCell[i][j].y;
						if ((x * sinA - y * cosA < R) && (x * cosA + y * sinA < L) && (-x * sinA + y * cosA < R)
								&& (x * cosA + y * sinA > -R)) {

							// line index
							int I = (int) (-y / width);
							// column index
							int J = (int) (x / width);
							// ~ b.addNeighbourCell (new Point (I, J));
							b.addSite (new Point (I, J)); // fc - 10.10.2008

						}
					}
				}
			}
		}
	}

	/**
	 * Lighting machine radiation for Direct and diffuse Called in method processEvolution ().
	 * 
	 * Updated by N. Dones INRA PIAF - March 2003 - dones@clermont.inra.fr Separation of diffuse
	 * beam set and light processes
	 * 
	 * Updated by I.Lecomte INRA Montpellier - April 2003 There is now several impacts on each cell
	 * (not only the center)
	 * 
	 * Updated by G. Talbot INRA Montpellier - 2007 = 1000
	 */
	public static void processLighting (SafeStand stand, SafePlotSettings plotSettings,
			SafeEvolutionParameters simulationSettings, SafeInitialParameters settings, SafeBeamSet beamSet,
			boolean isDiffus) {


		SafePlot plot = (SafePlot) stand.getPlot ();

		double plotWidth = plot.getXSize (); // x axis
		double plotHeight = plot.getYSize (); // y axis

		double toricXp = ((double) simulationSettings.toricXp) * plotWidth;
		double toricXn = ((double) simulationSettings.toricXn) * plotWidth;
		double toricYp = ((double) simulationSettings.toricYp) * plotHeight;
		double toricYn = ((double) simulationSettings.toricYn) * plotHeight;

		double cellSurface = plot.getCellSurface ();
		int nbImpact = settings.cellImpacts.size ();
		double impactSize = cellSurface / nbImpact; // to take into acount the surface of an impact
													// point

		double treeLineOrientation = Math.toRadians (plotSettings.treeLineOrientation);
		double slopeAspect = Math.toRadians (plotSettings.slopeAspect);
		double bottomAzimut = -Math.PI / 2 + treeLineOrientation - slopeAspect; // azimut of the
																				// maximum slope
																				// line / x axis
		double slope = Math.toRadians (plotSettings.slopeIntensity);

		// Reset diffuse and direct energy for target cells before their enlightment
		for (Iterator ite = plot.getCells ().iterator (); ite.hasNext ();) {
			SafeCell c = (SafeCell) ite.next ();
			if (isDiffus) {
				c.resetDiffuse ();
				c.getCrop ().resetDiffuse ();
				c.setVisibleSky (0);
			}
			c.resetDirect ();
			c.getCrop ().resetDirect ();
		}

		// Reset diffuse and direct energy and impact number for all trees before new computation
		// (necessary when trees have grown)
		for (Iterator ite = stand.getTrees ().iterator (); ite.hasNext ();) {
			SafeTree t = (SafeTree) ite.next ();
			if (isDiffus) {
				t.resetDiffuse ();
			}
			t.resetDirect ();
		}

		// fc for test - you can leave this here
		int maxSize = 0;
		int maxCcSize = 0;

		// For each target cell to enlight
		for (Iterator c = plot.getCells ().iterator (); c.hasNext ();) { // fc 4.0
			SafeCell cell = (SafeCell) c.next ();

			cell.findShadingCells (beamSet, (SafePlot) stand.getPlot ()); // find shadingCells from
																			// shadingMask (ie real
																			// cells from relative
																			// position)

			// For each beam of discretization of the sky
			for (SafeBeam beam : (Collection<SafeBeam>) beamSet.getBeams ()) {


			
				double azimut = beam.getAzimut_rad ();
				double heightAngle = beam.getHeightAngle_rad ();
				double slopeBeam = -slope * Math.cos (bottomAzimut - azimut); // slope in beam
																				// direction
				double cosSlope = Math.cos (slopeBeam);
				double sinSha = Math.sin (heightAngle - slopeBeam); // sine of beam heightAngle with
																	// respect to the floor

				double energyConvFactor = beam.getInitialEnergy ();
				if ((energyConvFactor != 0) && (isDiffus || beam.getDirectEnergy () != 0)) {
					float beamDirectEnergy = beam.getDirectEnergy ();
					float beamDiffuseEnergy = beam.getDiffuseEnergy ();
					float beamInfraRedEnergy = beam.getInfraRedEnergy ();

					

					

					// ~ Vector concurrentCells = cell.getNeighbourhood (beam, true); // fc only
					// neighbour cells with trees
					Collection<Neighbour> neighbours = beam.getNeighbours (cell, true); // fc only
																						// neighbour
																						// cells
																						// with
																						// trees

					for (Iterator i = beam.getShadingMasks ().iterator (); i.hasNext ();) { // competcell
						// Vertex3d impact = (Vertex3d) i.next ();
						SafeShadingMask shadingMask = (SafeShadingMask) i.next ();
						Vertex3d impact = shadingMask.getImpact ();

						Vector vctI = new Vector ();


						
						// For each concurent cell
						// ~ for (Enumeration cc = concurrentCells.elements (); cc.hasMoreElements
						// ();) {
						// ~ Neighbour neighbour = (Neighbour) cc.nextElement ();
						for (Neighbour neighbour : neighbours) { // fc - 10.10.2008
							SafeCell cCell = (SafeCell) neighbour.cell;
							ShiftItem shift = neighbour.shift; // the shift manages the torus
																// approaches to work on virtual
																// infinite plots

							if (!cCell.isEmpty ()) {

								// For each tree in the concurrent cells
								for (Iterator t = cCell.getTrees ().iterator (); t.hasNext ();) {
									SafeTree tree = (SafeTree) t.next ();

									// Computes interception characteristics
									// for a targetcell / a beam/ a tree
									// (average distance from target cell / pathLength / competitor
									// tree)
									SafeInterceptionItem item;
									item = intercept (plot, cell, tree, plotSettings, beam, impact, shift);
									if (item != null) {
										vctI.add (item);
									}
								}
							}
						}

						if (settings.cropLightMethod) {
							double L0; // competcell
							double L;
							double heightCrop;
							double pathLength;
							// For each ShadingCell
							for (Iterator it = shadingMask.getShadingNeighbours ().iterator (); it.hasNext ();) {
								SafeShadingNeighbour neighbour = (SafeShadingNeighbour) it.next ();
								L0 = neighbour.getLOut ();
								L = neighbour.getLIn ();
								heightCrop = neighbour.getCell ().getCrop ().getHeight ();
								if (L0 * sinSha < heightCrop) {
									if (L * sinSha < heightCrop) {
										pathLength = L - L0;
									} else {
										pathLength = (heightCrop * cosSlope) / sinSha - L0;
									}
									vctI.add (new SafeInterceptionItem (L, pathLength, neighbour.getCell (), null,
											neighbour.getShift (), false) // gt 28/05/2010
									);
									
								}
							}
						}

						// These counters are useful and take no time (measured fc)
						if (vctI.size () > maxSize) maxSize = vctI.size ();

						if (neighbours.size () > maxCcSize) maxCcSize = neighbours.size ();

						// fc tested : use directly tabI instead of vctI : NOT faster and tab
						// dimension pb.
						SafeInterceptionItem[] tabI = new SafeInterceptionItem[vctI.size ()];
						vctI.copyInto (tabI);

						// Interception items are sorted from furthest to nearest tree from the
						// target cell
						// It is necessary to compute progressive energy loss of the beam from
						// canopy top to canopy bottom
						Arrays.sort (tabI);

						// Reduces beam energy for each tree interception,
						// Increases tree energy at the same time and
						// Add the remaining beam energy to the target cell

						double currentPar = 1;
						double parNonInterceptedByTrees = 1;
						double currentNir = 1;
						double currentInfraRed = 1;
						double initEnergy = 1;
						ShiftItem refShift = new ShiftItem (0, 0, 0);

						// For each competing tree or crop
						boolean isTree; // competcell

						for (int k = 0; k < tabI.length; k++) {
							SafeInterceptionItem item = tabI[k];
							SafeTree t = item.getTree ();
							SafeCell sc = item.getCell (); // competcell
							ShiftItem shift = item.getShift ();
							isTree = (item.getTree () != null); // competcell
							double pathLength = item.getPathLength ();

							//if (cell.getTree() != null) {
								//System.out.println("k="+k+" path="+pathLength);
							
							//}
							
							if (isTree) { // if intercepting item is a tree

								boolean trunk = item.isTrunk ();
								if (trunk) { // gt 28/05/2010
									pathLength /= 2;
								}

								double leafParAbsorption = t.getTreeSpecies ().getLeafParAbsorption ();
								double leafNirAbsorption = t.getTreeSpecies ().getLeafNirAbsorption ();
								double leafNirReflection = 0.51; // gt - 28.04.2009 // value from
																	// Monteith's book.
								// we use the approximation from goudriann for computing the
								// transmitted radiation. We assume that a part of non-transmitted
								// radiation is absorbed and an other part is reflected. The
								// reflected fraction is assumed to equal the reflection coefficient
								// of leaves

								double branchDensity = t.getTreeSpecies ().getWoodAreaDensity ();
								double leafDensity = t.getLeafArea () / t.getCrownVolume ();

								// parameter to account for leaf clumping
								double clumpingCoef = t.getTreeSpecies ().getClumpingCoef ();

								// attenuation laws for Par, Nir, and thermical infra-red (black
								// leaves) radiation)
								// 0.5 is because of a spherical leaf angle distribution assumption
								// transmited and intercepted radiation are separated because the
								// wood area density
								// applies only to transmitted radiation...
								double transmittedPar =  Math.exp (-(0.5 * clumpingCoef
										* Math.sqrt (leafParAbsorption) * leafDensity + branchDensity)
										* pathLength);

								double transmittedInfraRed =  Math.exp (-(0.5 * clumpingCoef 
										* leafDensity + branchDensity) * pathLength);
								
								double transmittedNir = Math.exp (-0.5 * clumpingCoef
										* (Math.sqrt (leafNirAbsorption) * leafDensity) * pathLength);

								double interceptedPar =  1 - Math.exp (-0.5 * clumpingCoef
										* (Math.sqrt (leafParAbsorption) * leafDensity) * pathLength);

								double interceptedInfraRed =  1 - Math.exp (-(0.5 * clumpingCoef 
										* leafDensity + branchDensity) * pathLength);

								double interceptedNir =  (1 - Math.exp (-0.5 * clumpingCoef
										* (Math.sqrt (leafNirAbsorption) * leafDensity) * pathLength)) * (1 - leafNirReflection);
								// giving energy to the tree
								if (t.getPhenologicalStage () != 3) { // gt - 01.10.2009 - if
																		// leafFall began, Energy is
																		// no more given to the tree
										
									if (isDiffus) {
										
										t.addDiffuse (currentPar * (interceptedPar) * beamDiffuseEnergy
												* energyConvFactor * impactSize); // GT 2007
									
										t.addDiffuseToLonelyTree ((interceptedPar) * beamDiffuseEnergy
												* energyConvFactor * impactSize); // GT 2007
										t.addDiffuseNir (currentNir * (interceptedNir) * beamDiffuseEnergy
												* energyConvFactor * impactSize);
										t.addInfraRed (currentInfraRed * (interceptedInfraRed) * beamInfraRedEnergy
												* energyConvFactor * impactSize);
									}
									t.addDirect (currentPar * (interceptedPar) * beamDirectEnergy * energyConvFactor
											* impactSize);
									t.addDirectToLonelyTree ((interceptedPar) * beamDirectEnergy * energyConvFactor
											* impactSize);
									t.addDirectNir (currentNir * (interceptedNir) * beamDirectEnergy * energyConvFactor
											* impactSize);
								}

								// decreasing beam energy
								currentPar *= transmittedPar;
								parNonInterceptedByTrees *= transmittedPar;
								currentInfraRed *= transmittedInfraRed;
								currentNir *= transmittedNir;
								if (trunk) {
									currentPar = 0;
									currentNir = 0;
									currentInfraRed = 0;
									parNonInterceptedByTrees = 0;
								}

							} else { // if intercepting item is a crop
								if (settings.cropLightMethod) {
									SafeCrop crop = sc.getCrop ();
									double cropCoef = crop.getParExtinctionCoef ();
									double leafDensity = (crop.getLai () + crop.getEai ()) / crop.getHeight ();

									// Attenuation law for Par radiation
									float transmittedPar = (float) (Math.exp (-cropCoef * leafDensity
											* item.getPathLength ()));

									if (isDiffus)
										crop.addDiffuse ((float) (currentPar * (1 - transmittedPar) * beamDiffuseEnergy
												* energyConvFactor / nbImpact));
									crop.addDirect ((float) (currentPar * (1 - transmittedPar) * beamDirectEnergy
											* energyConvFactor / nbImpact));

									currentPar *= transmittedPar;
								}
							}
							if ((((refShift.x - shift.x) > toricXp) || ((shift.x - refShift.x) > toricXn)
									|| ((refShift.y - shift.y) > toricYp) || ((shift.y - refShift.y) > toricYn))) {
								currentPar = initEnergy;
								currentNir = initEnergy;
								currentInfraRed = initEnergy;
								parNonInterceptedByTrees = initEnergy;
							}

						}

						if (settings.cropLightMethod) {
							if (isDiffus)
								cell.addDiffusePar (parNonInterceptedByTrees * beamDiffuseEnergy * energyConvFactor
										/ nbImpact); // %
							cell.addDirectPar (parNonInterceptedByTrees * beamDirectEnergy * energyConvFactor
									/ nbImpact); // %
						} else {
							if (isDiffus)
								cell.addDiffusePar (currentPar * beamDiffuseEnergy * energyConvFactor / nbImpact); // %
							cell.addDirectPar (currentPar * beamDirectEnergy * energyConvFactor / nbImpact); // %
						}
						cell.addDirectNir (currentNir * beamDirectEnergy * energyConvFactor / nbImpact);
						if (isDiffus) {
							cell.addDiffuseNir (currentNir * beamDiffuseEnergy * energyConvFactor / nbImpact);
							cell.addVisibleSky (currentInfraRed * beamInfraRedEnergy * energyConvFactor / nbImpact);
						}

						

					} // end of impact (=end of ShadingMasks)
				} // end of if(energyConvFactor !=0)
			} // end of beams
		} // end of cells
	}


	/**
	 * SafeInterceptionItem : Interception characteristics for a target cell, a given beam and a
	 * given tree.
	 * 
	 * @author B. Courbaud - March 2000
	 */
	public static class SafeInterceptionItem implements Comparable {

		private double lEnter;
		private double pathLength;
		private SafeTree tree;
		private boolean trunk; // gt 28/05/2010
		private SafeCell cell;
		private ShiftItem shift;

		public SafeInterceptionItem (double le, double pl, SafeCell c, SafeTree t, ShiftItem s, boolean tr) { // gt
																												// 28/05/2010
			lEnter = le;
			pathLength = pl;
			cell = c;
			tree = t;
			shift = s;
			trunk = tr; // gt 28/05/2010
		}

		public int compareTo (Object b) {
			SafeInterceptionItem tb = (SafeInterceptionItem) b;
			double la = lEnter;
			double lb = tb.getLEnter ();
			if (la == lb) {
				return 0;
			} else if (la < lb) {
				return 1;
			} else {
				return -1;
			}
		}

		protected double getLEnter () {
			return lEnter;
		}

		public double getPathLength () {
			return pathLength;
		}

		public SafeCell getCell () {
			return cell;
		}

		public SafeTree getTree () {
			return tree;
		}

		public ShiftItem getShift () {
			return shift;
		}

		public boolean isTrunk () {
			return trunk;
		} // gt 28/05/2010

		public String toString () {
			return "cell=" + cell.toString () + "tree=" + tree.toString () + " pathLength=" + pathLength + " lEnter="
					+ lEnter + "trunk = " + trunk; // gt 28/05/2010
		}

	}

	/**
	 * Returns the characteristics of an interception for a target cell, a given beam and a given
	 * tree. If no interception by the tree, returns null. Interception points are at the same time
	 * elements of the beam and of the crown. An equation is solved and the solutions are the
	 * distances between the cell center and the interception points along the beam
	 * 
	 * Updated by Gregoire VINCENT - January 2004 - (g.vincent@cgiar.org) Modifications made to
	 * account for new profile (TRIAXIAL ellipsoid)
	 * 
	 * AT THIS STAGE ORIENTATION OF THE TREE IS NEGLECTED WHICH IS OK ONLY FOR SYMETRIC TREES. SINCE
	 * WE PLAN TO ALLOW FOR AN ELLIPSOID SECTION OF CROWN (flattened in the row) WE NEED TO CORRECT
	 * THE BEAM AZIMUT BY THE ANGLE MADE BY PLANTING LINE WITH THE NORTH. ALSO NOTE THAT THE PRESENT
	 * IMPLEMENTATION ASSUMES THAT ELLIPSOID IS NOT TRUNCATED!!!
	 * 
	 **/

	private static SafeInterceptionItem intercept (SafePlot plot, SafeCell cell, SafeTree tree,
			SafePlotSettings plotSettings, SafeBeam beam, Vertex3d impact, // beam impact coordinate
			ShiftItem shift // for toric symetry
	) {

		double shiftZ = SafeStand.zCoordinate (shift.x, shift.y, plotSettings); // z.shift is'nt computed by the generic method
																				// squareCell.getNeighbourhood.
		
		// Distance between the beam impact and the gravity center of the tree crown
		double x0 = tree.getX () + shift.x - (cell.getOrigin ().x + impact.x);
		x0 = x0 * 100000;														//IL 28-05-2015 to avoid rounding effect 
		x0 = Math.round(x0);													//bug detected with tree on the edge of the plot 
		x0 = x0 / 100000; 														//compared to centered tree
		double y0 = tree.getY () + shift.y - (cell.getOrigin ().y + impact.y);
		y0 = y0 * 100000;
		y0 = Math.round(y0);
		y0 = y0 / 100000; 
		double z0 = tree.getZ () + shiftZ  - (cell.getOrigin ().z + impact.z);
	
			
		double cBHeight = tree.getCrownBaseHeight ();
		double cosHAngle = Math.cos (beam.getHeightAngle_rad ());
		double sinHAngle = Math.sin (beam.getHeightAngle_rad ());

		// Azimut is already corrected for tree line orientation (TO CHECK)
		double cosAzimut = Math.cos (beam.getAzimut_rad ());
		double sinAzimut = Math.sin (beam.getAzimut_rad ());

		double A = 0;
		double B = 0;
		double C = 0;
		double l1 = 0;
		double l2 = 0;
		double l3 = 0;
		double pathLength = 0.0;
		double potPathLength = 0.0;

		
		if (tree.getTreeSpecies ().getCrownShape () == 1) // Ellipsoide crown shape
		{
			// Ellipso�d : crown equation : (x-x0)^2/R^2 + (y-y0)^2/R^2 +
			// (z-zMidCrown)^2/halfLength^2 = 1
			/*
			 * double zMidCrown = (zTopTree + zTreeBase)/2; double halfLength = (zTopTree -
			 * zTreeBase)/2;
			 * 
			 * A = (cosHAngle*cosHAngle)/(radius*radius) +
			 * (sinHAngle*sinHAngle)/(halfLength*halfLength); B = -2*cosHAngle*(x0*cosAzimut +
			 * y0*sinAzimut)/(radius*radius) - 2*sinHAngle*zMidCrown/(halfLength*halfLength); C =
			 * (x0*x0 + y0*y0)/(radius*radius) + (zMidCrown*zMidCrown)/(halfLength*halfLength) - 1;
			 */

			// (x0,y0,z0) is the ellipsoid center
			// (x,y,z) is the beam impact
			// a,b,c are the ellipsoide radius on x,y,z axis
			// A=[(x-x0)/a]^2 + [(y-y0)/b]^2 + [(z-z0)/c]^2

			// Ellipsoid gravity center
			z0 = z0 + tree.getHeight () - tree.getCrownRadiusVertical ();

			double a = tree.getCrownRadiusTreeLine ();
			double b = tree.getCrownRadiusInterRow ();
			double c = tree.getCrownRadiusVertical ();


			A = Math.pow (b, 2) * Math.pow (c, 2) * Math.pow (cosHAngle, 2) * Math.pow (cosAzimut, 2) + Math.pow (a, 2)
					* Math.pow (c, 2) * Math.pow (cosHAngle, 2) * Math.pow (sinAzimut, 2) + Math.pow (a, 2)
					* Math.pow (b, 2) * Math.pow (sinHAngle, 2);

			B = -2
					* (Math.pow (b, 2) * Math.pow (c, 2) * cosHAngle * cosAzimut * x0 + Math.pow (a, 2)
							* Math.pow (c, 2) * cosHAngle * sinAzimut * y0 + Math.pow (a, 2) * Math.pow (b, 2)
							* sinHAngle * z0);

			C = Math.pow (b, 2) * Math.pow (c, 2) * Math.pow (x0, 2) + Math.pow (a, 2) * Math.pow (c, 2)
					* Math.pow (y0, 2) + Math.pow (a, 2) * Math.pow (b, 2) * Math.pow (z0, 2) - Math.pow (a, 2)
					* Math.pow (b, 2) * Math.pow (c, 2);

		
			
			double delta = B * B - 4 * A * C;
			double sqrtDelta = Math.sqrt (delta); // OPTIMIZATION fc
			double twoA = 2 * A; // OPTIMIZATION fc

			// if (delta < 0) : no interception
			// if (delta == 0) : one interception : the beam is tangent to the crown -> pathlength=0

			if (delta > 0) { // two interceptions
				l1 = (-B + sqrtDelta) / (twoA); // biggest solution
				l2 = (-B - sqrtDelta) / (twoA); // smalest solution

				// distance from interception point to cell center along the beam
				double z1 = l1 * sinHAngle;
				double z2 = l2 * sinHAngle;

				pathLength = 0;
				if (z1 > cBHeight) { // the first ray impact is above crownBaseHeight

					if (z2 > cBHeight) { // the second ray impact is above crownBaseHeight
						pathLength = l1 - l2;

					} else { // the second ray impact is below crownBaseHeight
						double l2p = cBHeight / sinHAngle;
						pathLength = l1 - l2p;
					}
				}
			}

			
			
			
		//	if ((cell.getTree() != null) && (pathLength > 0)) {
			//	System.out.println("a="+a+"b="+b+"c="+c+" path="+pathLength);
			
		//	}
			
			
		} else {// Paraboloide crown shape

			// Equation for the crown (without crown base):
			// (x-xTree)^2 + (y-yTree)^2 = a (zTopTree - z) (parabolo�d formula)
			// equation for the ray z = l*sin(hAngle) ; x = l*cos(hAngle)*cos(azimut) ; y =
			// l*cos(hAngle)*sin(azimut)
			// in crown equation, x,y,z are replaced by ray equation coordinates
			// crown equation becomes a second degree equation in l : A*l^2 + B*l + C = 0

			double zTopTree = z0 + tree.getHeight ();
			double zTreeBase = z0 + tree.getCrownBaseHeight ();
			double radius = tree.getCrownRadius ();
			double a = (radius * radius) / (tree.getHeight () - cBHeight);

			A = cosHAngle * cosHAngle;
			B = (a * sinHAngle) - (2 * cosHAngle) * (x0 * cosAzimut + y0 * sinAzimut);
			C = (x0 * x0) + (y0 * y0) - (a * zTopTree);

			
			double delta = B * B - 4 * A * C;
			double sqrtDelta = Math.sqrt (delta); // OPTIMIZATION fc
			double twoA = 2 * A; // OPTIMIZATION fc

			// if (delta < 0) : no interception
			// if (delta == 0) : one interception : the beam is tangent to the crown -> pathlength=0

			if (delta > 0) { // two interceptions
				l1 = (-B + sqrtDelta) / (twoA); // biggest solution
				l2 = (-B - sqrtDelta) / (twoA); // smalest solution

				// distance from interception point to cell center along the beam
				double z1 = l1 * sinHAngle;
				double z2 = l2 * sinHAngle;

				if ((l1 > 0) && (l2 > 0)) {
					// the ray is intercepted twice before reaching the cell
					potPathLength = l1 - l2;

					if ((z1 > zTreeBase) && (z2 > zTreeBase)) { // interceptions higher than tree
																// base
						pathLength = potPathLength;
					} else if ((z1 > zTreeBase) && (z2 <= zTreeBase)) { // the beam crosses the base
						l3 = zTreeBase / sinHAngle;
						pathLength = l1 - l3;
					} else { // both interceptions are under tree base
						pathLength = 0;
					}
				} else if ((l1 > 0) && (l2 <= 0)) { // the cell is inside the crown
					potPathLength = l1;
					if (z1 > zTreeBase) { // first interception higher than tree base
						if (zTreeBase > 0) {
							l3 = zTreeBase / sinHAngle; // second interception translated to base
														// interception
							pathLength = l1 - l3;
						} else {
							pathLength = l1; // the ray is intercepted only once before reaching the
												// cell
						}
					} else { // first interception under tree base
						pathLength = 0;
					}
				} else { // (l1<=0 && l2<=0) the ray crosses the cell before the tree
					potPathLength = 0;
					pathLength = 0;
				}
			}
		}

		// interception by trunks //gt 28/05/2010
		boolean trunk = false;
		double lt = 0;
		double hdist = Math.sqrt (Math.pow (x0, 2) + Math.pow (y0, 2));
		Vertex3d rayTrunk = new Vertex3d (0, 0, 0);
		rayTrunk.x = hdist * cosAzimut;
		rayTrunk.y = hdist * sinAzimut;
		rayTrunk.z = hdist * sinAzimut / cosHAngle;
		double dRayTrunk = Math.sqrt (Math.pow (rayTrunk.x - x0, 2) + Math.pow (rayTrunk.y - y0, 2));

		z0 = tree.getZ () + shiftZ - (cell.getOrigin ().z + impact.z);
		if ((rayTrunk.z > z0) & (rayTrunk.z < (z0 + tree.getHeight ()))) {
			double drh = tree.getDbhMeters () / 2 * (z0 + tree.getHeight () - rayTrunk.z) / (tree.getHeight () - 1.3); // tree
																														// diameter
																														// at
																														// ray
																														// height;
			if (drh >= dRayTrunk) {
				trunk = true;
				lt = hdist / sinHAngle;
			}
		}

		

		
		if ((pathLength != 0) || (trunk == true)) { // gt 28/05/2010
			if (pathLength == 0) {
				l1 = lt; // gt 28/05/2010
			}
			SafeInterceptionItem item = new SafeInterceptionItem (l1, pathLength, null, tree, shift, trunk); // gt

			return item;
		} else {
			return null;
		}

	}// end of intercept()

	/**
	 * Creation of X * X points of impacts for process Lighting default value nbImpactMultiplication
	 * = 1 (1 points) if = 2 -> 4 points if = 3 -> 9 points
	 */
	public static void createCellImpacts (SafePlotSettings plotSettings, SafeInitialParameters safeSettings,
			int nbImpact) {


		safeSettings.cellImpacts = new ArrayList ();
		double dx;
		double dy;
		double dz;
		double cellWidth = plotSettings.cellWidth;
		int n = Math.max (nbImpact, safeSettings.nbImpactMultiplication);
		// double l = cellWidth / n;
		for (int i = 0; i < n; i++) {
			for (int j = 0; j < n; j++) {
				
	
				
				// dx=l/2+i*l;
				// dy=l/2+j*l;
				dx = cellWidth * (1 + 2 * i) / (2 * n);
				dy = cellWidth * (1 + 2 * j) / (2 * n);
				// dx=cellWidth/2;
				// dy=cellWidth/2;

				dz = SafeStand.zCoordinate (dx, dy, plotSettings); // for slope
				safeSettings.cellImpacts.add (new Vertex3d (dx, dy, dz));
			}
		}
	} // end of createCellImpact()

	/**
	 * optimization procedure for light interception by crops. for each SafeBeam, this method
	 * creates a collection of SafeShadingMask, one for each cellImpact a SafeShadingMask contains
	 * relative coordinates of the neigbour crop potentially intercepting the beam.
	 * 
	 **/
	public static void createShadingMasks (SafeBeamSet beamSet, SafeInitialParameters safeSettings,
			SafePlotSettings plotSettings, double heightCropMax) {

		if (safeSettings.cropLightMethod && (heightCropMax > 0)) {
			// plotSettings
			double treeLineOrientation = Math.toRadians (plotSettings.treeLineOrientation);
			double slopeAspect = Math.toRadians (plotSettings.slopeAspect);
			double bottomAzimut = -Math.PI / 2 + treeLineOrientation - slopeAspect; // azimut of the
																					// maximum slope
																					// line / x axis
			double realSlope = Math.toRadians (plotSettings.slopeIntensity); // slope intensity in
																				// beam direction
																				// (>0 if
																				// zCoordinate
																				// increases in beam
																				// direction)
			double cellWidth = plotSettings.cellWidth; // cell width (meters)

			for (Iterator it = beamSet.getBeams ().iterator (); it.hasNext ();) {
				SafeBeam beam = (SafeBeam) it.next ();

				// remove old masks
				beam.removeShadingMasks ();
				// beam
				double heightAngle = beam.getHeightAngle_rad ();
				double azimut = beam.getAzimut_rad ();
				double cosHa = Math.cos (heightAngle);

				double slope = -realSlope * Math.cos (bottomAzimut - azimut); // slope intensity in
																				// beam direction
																				// (>0 if
																				// zCoordinate
																				// increases in beam
																				// direction)
				double sinSha = Math.sin (heightAngle - slope); // sine of seenHeightAngle = angle
																// between the beam ant the floor.

				// construction of azt : depends on azimut : =azimut if in [0,Pi/4], =Pi/2-azimut if
				// in [Pi/4,Pi/2], =Pi/2+azimut if in [Pi/2,3Pi/4],...
				double azt;
				int pos;

				if (azimut < Math.PI / 4) {
					azt = azimut;
					pos = 1;
				} else if ((azimut >= Math.PI / 4) && (azimut < Math.PI / 2)) {
					azt = Math.PI / 2 - azimut;
					pos = 2;
				} else if ((azimut >= Math.PI / 2) && (azimut < 3 * Math.PI / 4)) {
					azt = azimut - Math.PI / 2;
					pos = 3;
				} else if ((azimut >= 3 * Math.PI / 4) && (azimut < Math.PI)) {
					azt = Math.PI - azimut;
					pos = 4;
				} else if ((azimut >= Math.PI) && (azimut < 5 * Math.PI / 4)) {
					azt = azimut - Math.PI;
					pos = 5;
				} else if ((azimut >= 5 * Math.PI / 4) && (azimut < 3 * Math.PI / 2)) {
					azt = 3 * Math.PI / 2 - azimut;
					pos = 6;
				} else if ((azimut >= 3 * Math.PI / 2) && (azimut < 7 * Math.PI / 4)) {
					azt = azimut - 3 * Math.PI / 2;
					pos = 7;
				} else if (azimut >= 7 * Math.PI / 4) {
					azt = 2 * Math.PI - azimut;
					pos = 8;
				} else {
					azt = 0;
					pos = 1;
				}
				double cosAzt = Math.cos (azt);
				double sinAzt = Math.sin (azt);
				double tanAzt = sinAzt / cosAzt;

				for (Iterator t = safeSettings.cellImpacts.iterator (); t.hasNext ();) {
					Vertex3d impact = (Vertex3d) t.next ();

					SafeShadingMask mask = new SafeShadingMask (impact);

					// position of the impact cell's borders, in the referential cell the
					// impact as origin and x- and y- axis defined so that the beam azimut is azt

					double xEdge = 0;
					double yEdge = 0;
					if (pos == 1) {
						xEdge = cellWidth - impact.x;
						yEdge = cellWidth - impact.y;
					} else if (pos == 2) {
						xEdge = cellWidth - impact.y;
						yEdge = cellWidth - impact.x;
					} else if (pos == 3) {
						xEdge = cellWidth - impact.y;
						yEdge = impact.x;
					} else if (pos == 4) {
						xEdge = impact.x;
						yEdge = cellWidth - impact.y;
					} else if (pos == 5) {
						xEdge = impact.x;
						yEdge = impact.y;
					} else if (pos == 6) {
						xEdge = impact.y;
						yEdge = impact.x;
					} else if (pos == 7) {
						xEdge = impact.y;
						yEdge = cellWidth - impact.x;
					} else if (pos == 8) {
						xEdge = cellWidth - impact.x;
						yEdge = impact.y;
					}


					double L0 = 0;
					double L = 0;
					double h = 0;

					int xRel = 0;
					int yRel = 0;

					while (h < heightCropMax) {

						double trans = 0; // usefull for keeping a value
						boolean isX; // is true if beam goes out of the cell to the leftor the right
						L0 = L;

						if (xEdge / cosAzt <= yEdge / sinAzt) {
							trans = xEdge * tanAzt;
							L += xEdge / (cosHa * cosAzt);
							yEdge -= trans;
							xEdge = cellWidth;
							isX = true;
						} else {
							trans = yEdge / tanAzt;
							L += yEdge / (cosHa * sinAzt);
							xEdge -= trans;
							yEdge = cellWidth;
							isX = false;
						}

						h = L * sinSha;
						SafeShadingNeighbour neighbour;
						if (pos == 1) {
							neighbour = new SafeShadingNeighbour (new Point (xRel, yRel), L, L0);
						} else if (pos == 2) {
							neighbour = new SafeShadingNeighbour (new Point (yRel, xRel), L, L0);
						} else if (pos == 3) {
							neighbour = new SafeShadingNeighbour (new Point (-yRel, xRel), L, L0);
						} else if (pos == 4) {
							neighbour = new SafeShadingNeighbour (new Point (-xRel, yRel), L, L0);
						} else if (pos == 5) {
							neighbour = new SafeShadingNeighbour (new Point (-xRel, -yRel), L, L0);
						} else if (pos == 6) {
							neighbour = new SafeShadingNeighbour (new Point (-yRel, -xRel), L, L0);
						} else if (pos == 7) {
							neighbour = new SafeShadingNeighbour (new Point (yRel, -xRel), L, L0);
						} else {
							neighbour = new SafeShadingNeighbour (new Point (xRel, -yRel), L, L0);
						}
						mask.addShadingNeighbour (neighbour);

						if (isX) {
							xRel++;
						} else {
							yRel++;
						}

					} // end of while(h<heightCropMax)
					beam.addShadingMask (mask);
				} // end of impact
			} // end of beams
		} else {
			for (Iterator it = beamSet.getBeams ().iterator (); it.hasNext ();) {
				SafeBeam beam = (SafeBeam) it.next ();

				// remove old masks
				beam.removeShadingMasks ();

				for (Iterator t = safeSettings.cellImpacts.iterator (); t.hasNext ();) {
					Vertex3d impact = (Vertex3d) t.next ();

					SafeShadingMask mask = new SafeShadingMask (impact);
					beam.addShadingMask (mask);
				} // end of impacts
			} // end of beams
		} // end of if/else

	}// end of CreatShadingMask()

	// When trees are small, space discretisation in large cells leads to overestimation of
	// light intercepted by trees. This method optimize nbImpactMultiplication when there are
	// small trees
	public static int nbImpactOptimization (SafeStand stand) {
		int nbImpact = 1;
		double cellWidth = ((SafePlot) stand.getPlot ()).getCellWidth ();
		double smallerCrownRad = 1.5 * cellWidth;

		for (Iterator t = stand.getTrees ().iterator (); t.hasNext ();) {
			SafeTree tree = (SafeTree) t.next ();
			smallerCrownRad = Math.min (smallerCrownRad, tree.getCrownRadiusTreeLine ());
			smallerCrownRad = Math.min (smallerCrownRad, tree.getCrownRadiusInterRow ());
		}
		//in case tree is not planted 
		if (smallerCrownRad <= 0) nbImpact = 1;
		else if (smallerCrownRad < (1.5 * cellWidth)) nbImpact = Math.round ((float) (1.5 * cellWidth / smallerCrownRad));

		return nbImpact;
	}

}// end of the Class

