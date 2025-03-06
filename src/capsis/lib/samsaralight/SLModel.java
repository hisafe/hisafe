/*
 * Samsaralight library for Capsis4.
 *
 * Copyright (C) 2008 / 2012 Benoit Courbaud.
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.lib.samsaralight;

import java.awt.Point;
import java.io.Serializable;
import java.text.NumberFormat;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import capsis.defaulttype.ShiftItem;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Neighbour;
import capsis.util.Point2D;
// fa-12.07.2017
import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

// //fa-31.05.2017: for trace of interceptItems
// import java.io.BufferedWriter;
// import java.io.FileWriter;

/**
 * SLModel - implementation of the Samsara light model.
 * 
 * @author B. Courbaud, N. Dones, M. Jonard, G. Ligot, F. de Coligny - October
 *         2008 / June 2012
 */
public class SLModel implements Serializable {

	static {
		Translator.addBundle("capsis.lib.samsaralight.SLSettingsDialog");
	}

	private SLBeamSet beamSet;
	private SLSettings slSettings;
	private double cellInitialEnergySum;

	private NumberFormat decimal6 = DefaultNumberFormat.getInstance(6);
	private static final String UNDERSCORE = "_";

	// A map to help calculate PotentialEnergy
	// private Map<SLLightableTree, Double> potCurrentEnergyMap;

	/**
	 * Constructor
	 */
	public SLModel() {
		slSettings = new SLSettings();
	}

	/**
	 * This method should be called at init time, once the main parameters file name
	 * is set in SLSettings.
	 */
	public void init() throws Exception {

		// fc+fa-18.5.2017 Now possible to call init1_loadSettingsFile() and
		// then to create a custom beamset, related with tagMode, this original
		// init () method is unchanged

		init1_loadSettingsFile();

		init2_createLegacyBeamSet();

	}

	/**
	 * Loads the SLSettings init file, needs main parameters file name to be set
	 * before in SLSettings. See init ().
	 */
	public void init1_loadSettingsFile() throws Exception {

		// fc+fa-18.5.2017 init1_loadSettingsFile() and
		// init2_createLegacyBeamSet() were previuosly merged in init (), they
		// can now be called separately

		// Before calling init (), a correct fileName must be set in slSettings
		try {

			// fc-1.10.2024 Refactored SamsaraFile loading
			SLSettingsLoader2024 loader = new SLSettingsLoader2024();
			loader.load(slSettings.getFileName ());
			loader.interpret(slSettings);

//			SLSettingsLoader loader = new SLSettingsLoader(slSettings.getFileName ());
//			loader.interpret(slSettings);
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "SLModel.init ()", "Could not load the settings file: " + slSettings, e);
			throw e;
		}

	}

	/**
	 * Create the beamSet in legacy mode. Possible to create a custom beamset with
	 * other methods instead, related to tagMode.
	 */
	public void init2_createLegacyBeamSet() throws Exception {

		// fc+fa-18.5.2017 init1_loadSettingsFile() and
		// init2_createLegacyBeamSet() were previuosly merged in init (), they
		// can now be called separately

		// Create beamSet with SLBeamSetFactory
		if (getSettings().isWriteStatusDispatcher())
			StatusDispatcher.print(Translator.swap("SLModel.creatingBeamSet"));

		if (slSettings.getMontlyRecords() != null) {
			beamSet = SLBeamSetFactory.getMonthlyBeamSet(slSettings);

		} else if (slSettings.getHourlyRecords() != null) {
			beamSet = SLBeamSetFactory.getHourlyBeamSet(slSettings);

		} else {
			throw new Exception(
					"SLModel.init() could not create the beamSet, please provide monthlyBeamSet or hourlyBeamSet in SLSettings");

		}

	}

	/**
	 * In case the beams were not provided at init () time, they can be set later
	 * with this method.
	 */
	public void setBeamSet(SLBeamSet beamSet, TreeList scene, double treeMaxHeight, double cellWidth,
			double maxCrownRadius) throws Exception {

		this.beamSet = beamSet;

		computeRelativeCellNeighbourhoods(scene, treeMaxHeight, cellWidth, maxCrownRadius);

	}

	// fc-et-al-20.1.2017
	public boolean isVegetationPeriod(int doy) {
		return doy >= getSettings().getLeafOnDoy() && doy <= getSettings().getLeafOffDoy();
	}

	public static final String GLOBAL_RADIATION = "GLOBAL_RADIATION";
	public static final String DIRECT_RADIATION = "DIRECT_RADIATION";
	public static final String DIFFUSE_RADIATION = "DIFFUSE_RADIATION";

	/**
	 * Incident global radiation (MJ/m2/year). Restricted to vegetation period if
	 * vegetationPeriodOnly is true.
	 */
	public double calculateIncidentRadiation(boolean vegetationPeriodOnly, String radiationType) throws Exception {

		if (!radiationType.equals(GLOBAL_RADIATION) && !radiationType.equals(DIRECT_RADIATION)
				&& !radiationType.equals(DIFFUSE_RADIATION))
			throw new Exception("SLModel, wrong radiationType: " + radiationType
					+ ", should be GLOBAL_RADIATION, DIRECT_RADIATION or DIFFUSE_RADIATION");

		double igr = 0;
		if (slSettings.getMontlyRecords() != null) {

			int c = 0;
			double[] mVProportion = SLBeamSetFactory.monthVegetativeProportions(slSettings.getLeafOnDoy(),
					slSettings.getLeafOffDoy());

			for (SLMonthlyRecord r : slSettings.getMontlyRecords()) {

				double rad = 0;

				if (radiationType.equals(GLOBAL_RADIATION))
					rad = r.global * mVProportion[c];
				else if (radiationType.equals(DIRECT_RADIATION))
					rad = r.global * mVProportion[c] * (1 - r.diffuseToGlobalRatio);
				else
					rad = r.global * mVProportion[c] * r.diffuseToGlobalRatio;
				igr += rad;
				c++;
			}

		} else {
			ListMap<Integer, SLHourlyRecord> allRecords = slSettings.getHourlyRecords();

			int doyMonthFirstDay = 0;

			// The key used in the listMap corresponds exactly to the month
			// index
			for (int m = 0; m < 12; m++) {

				// m+1 because in the ListMap they are encoded with the true
				// month number
				List<SLHourlyRecord> hourlyRecords = (List) allRecords.getObjects(m + 1);

				for (SLHourlyRecord r : hourlyRecords) {

					int doy = doyMonthFirstDay + r.day;

					if (vegetationPeriodOnly
							&& !(doy >= slSettings.getLeafOnDoy() && doy <= slSettings.getLeafOffDoy()))
						continue;

					double rad = 0;

					if (radiationType.equals(GLOBAL_RADIATION))
						rad = r.global;
					else if (radiationType.equals(DIRECT_RADIATION))
						rad = r.global * (1 - r.diffuseToGlobalRatio);
					else
						rad = r.global * r.diffuseToGlobalRatio;

					igr += rad;
				}
				doyMonthFirstDay += SLBeamSetFactory.NBMONTHDAYS[m];

			}

		}
		return igr;
	}

	/**
	 * Computes relative cell neighbourhoods (a brand new concept) for each beam in
	 * the beamSet. The trees which can intercept a beam are located in cells with
	 * their center located in a competition rectangle of length L (beam direction)
	 * + R (opposite direction) and width R (directions perpendicular to beam).
	 */
	public void computeRelativeCellNeighbourhoods(TreeList initStand, double treeMaxHeight, double cellWidth,
			double maxCrownRadius) throws Exception {

		if (beamSet == null || beamSet.getBeams() == null || beamSet.getBeams().isEmpty())
			throw new Exception("SLModel: could not compute relative cell neighbourhoods, could not find any beams");

		// GL 22/06/2012 remove bottom azimut from method signature. I get it
		// from the slsettings.
		double bottomAzimut_rad = Math
				.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg());
		// bottomAzimut = Math.toRadians(bottomAzimut); // azimut of the maximum

		if (beamSet == null)
			throw new Exception("SLModel: beamSet is null, check that SLModel.init () was called correctly");

		// max tree height (a little higher than the height at wich the
		// probability of mortality is max)
		double Hmax = treeMaxHeight * 1.25;

		// Slope line / x axis
		double width = cellWidth; // cell width (meters)

		// max reach (meters): min (plot width, plot height)
		double reach = (double) Math.min(initStand.getXSize(), initStand.getYSize());
		double cRadius = maxCrownRadius; // max crown radius (meters)

		double slope_rad = Math.toRadians(slSettings.getPlotSlope_deg());

		for (Iterator it = beamSet.getBeams().iterator(); it.hasNext();) {
			SLBeam b = (SLBeam) it.next();

			// if (b.getSites() != null && !b.getSites().isEmpty ())
			// throw new Exception
			// ("SLModel, error in computeRelativeCellNeighbourhoods (), relative cell
			// neighbourhoods can not be computed twice");

			int count = 0;

			double azimut = b.getAzimut_rad();
			double hAngle = b.getHeightAngle_rad();

			// double lateral = width/Math.sqrt(2);
			// Computes lateral = the boundary to add to the competition
			// rectangle to take into account cells center
			// instead of trees position.
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
			} else {
				azt = 0;
			}

			double lateral = width / Math.sqrt(2) * Math.sin(azt + Math.PI / 4);

			// Beam width = max lateral distance from the beam to a cell center
			// able to own a tree which can intercept the beam.
			double R = cRadius + lateral;

			// Beam reach maximum distance along the beam beyond which the cells
			// cannot own trees which can intercept the beam (too high).
			double L = Hmax / (Math.tan(hAngle) + Math.cos(azimut - bottomAzimut_rad) * Math.tan(slope_rad)) + lateral;

			double sinA = Math.sin(azimut);
			double cosA = Math.cos(azimut);

			// Coordinates of the four corners of the competition rectangle.
			double x1 = R * sinA + L * cosA;
			double y1 = L * sinA - R * cosA;
			double x2 = L * cosA - R * sinA;
			double y2 = L * sinA + R * cosA;
			double x3 = R * (sinA - cosA);
			double y3 = -R * (sinA + cosA);
			double x4 = -R * (sinA + cosA);
			double y4 = R * (cosA - sinA);

			double xMin = Math.min(x1, x2);
			xMin = Math.min(xMin, x3);
			xMin = Math.min(xMin, x4);

			double xMax = Math.max(x1, x2);
			xMax = Math.max(xMax, x3);
			xMax = Math.max(xMax, x4);

			double yMin = Math.min(y1, y2);
			yMin = Math.min(yMin, y3);
			yMin = Math.min(yMin, y4);

			double yMax = Math.max(y1, y2);
			yMax = Math.max(yMax, y3);
			yMax = Math.max(yMax, y4);

			// Round of xMin, xMax, yMin, yMax with ceil or floor as necessary.
			// -> an xCenter relatively to target
			int aux = (int) Math.floor(xMax / width); // xMax > 0 : round to
			// maximum int < xMax
			xMax = aux * width;
			aux = (int) Math.floor(yMax / width);
			yMax = aux * width;
			aux = (int) Math.ceil(xMin / width); // xMin < 0 : round to minimum
			// int > xMin
			xMin = aux * width;
			aux = (int) Math.ceil(yMin / width);
			yMin = aux * width;

			// Creates an array of cell indexes between extreme competition
			// rectangle coordinates.
			int iMax = (int) ((yMax - yMin) / width + 0.5) + 1; // i e [0, iMax]
			// : iMax+1
			// elements
			int jMax = (int) ((xMax - xMin) / width + 0.5) + 1; // j e [0, jMax]
			// : jMax+1
			// elements

			Point2D.Double[][] tabCell = new Point2D.Double[iMax][jMax];
			int i = 0;
			int j = 0;
			iMax = 0;
			jMax = 0;

			for (double y = yMax; y >= yMin; y -= width) {
				for (double x = xMin; x <= xMax; x += width) {
					// System.out.print ("x="+x+" i="+i+" j="+j+" . ");

					tabCell[i][j] = new Point2D.Double(x, y); // a candidate
					// cell (its
					// center)
					j++;
				}
				if (j - 1 > jMax) {
					jMax = j - 1;
				}
				j = 0;
				i++;
			}
			iMax = i - 1;

			// Explores the array of cell indexes to extract the cells which are
			// located
			// inside the competition rectangle (depending of the azimut).
			for (i = 0; i <= iMax; i++) {
				for (j = 0; j <= jMax; j++) {
					double x = tabCell[i][j].x;
					double y = tabCell[i][j].y;
					if ((x * sinA - y * cosA < R) && (x * cosA + y * sinA < L) && (-x * sinA + y * cosA < R)
							&& (x * cosA + y * sinA > -R)) {

						// Line index.
						int I = (int) (-y / width);
						// Column index.
						int J = (int) (x / width);
						// ~ b.addNeighbourCell (new Point (I, J));
						b.addSite(new Point(I, J)); // fc - 10.10.2008
						count++;
						// Log.print (tabCell [i][j]+" ");
					}
				}
			}
		}
	}

	/**
	 * Legacy method, do not rely on a foliageStateManager.
	 */
	public void processLighting(TreeList stand) throws Exception {
		processLighting(stand, null); // fc+fa-18.5.2017
	}

	/**
	 * Implementation of the Samsara light model. fc+fa-18.5.2017 added
	 * foliageStateManager : if null, ladProportion and greenProportion will always
	 * be 1, else gives the ladProportion and greenProportion for a speciesId and a
	 * doy (SLBeams now have optional doys).
	 */
	public void processLighting(TreeList stand, SLFoliageStateManager foliageStateManager) throws Exception {

		// fc+fa-1.8.2017
		// Moved algorithm in several objects to facilitate refactoring and
		// Heterofor tagMode introduction

		// tmp, will be set in each module (Heterofor, Quergus...)
		slSettings.setParallelMode(true);

		boolean acceptable = runLighter(stand, foliageStateManager);

		// fc+fa-3.8.2017
		// In parallel mode, we check the radiative balance error acceptability
		// and re run in Sequential mode if error is too high with regards to
		// a given tolerance
		if (slSettings.isParallelMode() && !acceptable) {
			SLReporter.printInLog(Log.WARNING, "SLModel.processLighting ()",
					"Balance is out of tolerance, relaunching radiative balance in sequential mode");
			SLReporter.printInStandardOutput(
					"Balance is out of tolerance, relaunching radiative balance in sequential mode...");
			slSettings.setParallelMode(false);

			// We do not check acceptable in Sequential mode
			runLighter(stand, foliageStateManager);

			slSettings.setParallelMode(true);
		}

	}

	private boolean runLighter(TreeList stand, SLFoliageStateManager foliageStateManager) throws Exception {

		SLLighter lighter = null;

		if (slSettings.isTagMode())
			lighter = new SLLighterTagOptimized(stand, foliageStateManager, beamSet, slSettings, this);
		// lighter = new SLLighterTag(stand, foliageStateManager, beamSet, slSettings,
		// this);
		else
			lighter = new SLLighterLegacy(stand, foliageStateManager, beamSet, slSettings, this);

		// fc+fa-2.8.2017 This is the original code by G. Ligot, before
		// refactoring by fc and fa
		// lighter = new SLLighterOriginal(stand, foliageStateManager, beamSet,
		// slSettings, this)

		lighter.run();

		// Check (for paralellism)
		boolean acceptable = true;
		if (!slSettings.isSensorLightOnly()) // do not check for isSensorLightOnly()=true: radiative balance does not
												// hold as all cells and all trees may not be illuminated when only
												// sensors are considered
			acceptable = lighter.checkRadiativeBalance(beamSet, stand);

		return acceptable;
	}

	/**
	 * Interception items storing system to save time for several diffuse beams with
	 * identical orientations.
	 */
	private String makeKey(double targetX, double targetY, double targetZ, SLBeam beam, Tree tree, ShiftItem shift) {

		// fc+fa-16.5.2017
		StringBuffer b = new StringBuffer();
		b.append(decimal6.format(targetX));
		b.append(UNDERSCORE);
		b.append(decimal6.format(targetY));
		b.append(UNDERSCORE);
		b.append(decimal6.format(targetZ));
		b.append(UNDERSCORE);
		b.append(decimal6.format(beam.getAzimut_rad()));
		b.append(UNDERSCORE);
		b.append(decimal6.format(beam.getHeightAngle_rad()));
		b.append(UNDERSCORE);
		b.append(tree.getId());
		b.append(UNDERSCORE);
		b.append(decimal6.format(shift.x));
		b.append(UNDERSCORE);
		b.append(decimal6.format(shift.y));
		b.append(UNDERSCORE);
		b.append(decimal6.format(shift.z));

		return b.toString();
	}

	/**
	 * Key for storing of passed occurrences of neighbour loop.
	 */
	private String makeKey(Neighbour neighbour) {

		// fa-31.05.2017
		StringBuffer b = new StringBuffer();
		b.append(decimal6.format(neighbour.shift.x));
		b.append(UNDERSCORE);
		b.append(decimal6.format(neighbour.shift.y));
		b.append(UNDERSCORE);
		b.append(decimal6.format(neighbour.shift.z));
		b.append(UNDERSCORE);
		b.append(decimal6.format(neighbour.cell.getId()));

		return b.toString();
	}

	/**
	 * Beer-Lambert law.
	 */
	public double beerLambert(double incidentEnergy, double extinctionCoef, double clumpingFactor,
			double leafAreaDensity, double pathLength) {

		double interceptedEnergy = incidentEnergy
				* (1 - Math.exp(-extinctionCoef * clumpingFactor * leafAreaDensity * pathLength));

		return interceptedEnergy;
	}

	/**
	 * Returns the characteristics of an interception for a target cell, a given
	 * beam and a given tree. If no interception by the tree, returns null.
	 * Interception points are at the same time elements of the beam and of the
	 * crown. An equation is solved and the solutions are the distances between the
	 * cell center and the interception points along the beam.
	 */
	public Set<SLInterceptionItem> intercept(double targetX, double targetY, double targetZ, SLBeam beam, Tree tree,
			ShiftItem shift) { // modified by GL at Feb 2013
		// change method signature
		Set<SLInterceptionItem> interceptionItems = new HashSet<SLInterceptionItem>();

		Spatialized spa = (Spatialized) tree;
		SLLightableTree lightableTree = ((SLLightableTree) tree);
		SLTreeLightResult light = lightableTree.getLightResult();

		double xShift = shift.x - targetX; // The target coordinates will become
											// the origin in the next
											// computation
		double yShift = shift.y - targetY;
		double zShift = shift.z - targetZ;

		double elevation = beam.getHeightAngle_rad();
		double azimuth = beam.getAzimut_rad();

		for (SLCrownPart part : lightableTree.getCrownParts()) {
			double[] r = part.intercept(xShift, yShift, zShift, elevation, azimuth);

			if (r == null) {
				// no interception
			} else {
				// This part intercepted the beam
				double pathLength = r[0];
				double distance = r[1];
				SLInterceptionItem item = new SLInterceptionItem(distance, pathLength, part, tree);
				interceptionItems.add(item);
				// sq-28.10.2021 add impact number
				light.incrementImpactNumber();
			}
		}

		if (slSettings.isTrunkInterception()) {
			SLTrunk trunk = lightableTree.getTrunk();

			double[] r = trunk.intercept(xShift, yShift, zShift, elevation, azimuth);

			if (r == null) {
				// no interception
			} else {
				// This part intercepted the beam
				double pathLength = r[0];
				double distance = r[1];
				SLInterceptionItem item = new SLInterceptionItem(distance, pathLength, trunk, tree);
				interceptionItems.add(item);
				// sq-28.10.2021 add impact number
				light.incrementImpactNumber();
			}

		}

		return interceptionItems;

	}

	public SLSettings getSettings() {
		return slSettings;
	}

	public SLBeamSet getBeamSet() {
		return beamSet;
	}

	// TO BE CHECKED with Benoit Courbaud
	public double getIncidentEnergy() {
		return cellInitialEnergySum;
	}

	/**
	 * Solves a quadratic equation, returns 0, 1 or 2 roots. Never returns null;
	 */
	static public double[] solveQuadraticEquation(double a, double b, double c) {
		double ro = b * b - 4 * a * c;
		if (ro > 0) { // 2 roots
			double root1 = (-b + Math.sqrt(ro)) / (2 * a);
			double root2 = (-b - Math.sqrt(ro)) / (2 * a);
			return new double[] { root1, root2 };
		} else if (ro == 0) { // 1 root
			double root1 = (-b + Math.sqrt(ro)) / (2 * a);
			return new double[] { root1 };
		} else { // no roots
			return new double[] {};
		}
	}

}
