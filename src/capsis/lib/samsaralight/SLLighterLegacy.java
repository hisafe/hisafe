/* 
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2017 Benoit Courbaud.
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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Vector;

import capsis.defaulttype.ShiftItem;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.Neighbour;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.RectangularPlot;
import capsis.defaulttype.plotofcells.SquareCell;
import capsis.kernel.Step;
import jeeb.lib.util.AdditionMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**
 * An implementation of the legacy SamsaraLight algorithm.
 * 
 * @author F. de Coligny, F. André - August 2017
 */
public class SLLighterLegacy extends SLLighter {

	private TreeList stand;
	private SLFoliageStateManager foliageStateManager;
	private SLBeamSet beamSet;
	private SLSettings slSettings;
	private SLModel slModel;

	/**
	 * Constructor
	 */
	public SLLighterLegacy(TreeList stand, SLFoliageStateManager foliageStateManager, SLBeamSet beamSet,
			SLSettings slSettings, SLModel slModel) throws Exception {
		this.stand = stand;
		this.foliageStateManager = foliageStateManager; // May be null
		this.beamSet = beamSet;
		this.slSettings = slSettings;
		this.slModel = slModel;
	}

	/**
	 * To be called after run(), check the balance and returns true if error is
	 * considered acceptable.
	 */
	public boolean checkRadiativeBalance(SLBeamSet beamSet, TreeList stand) {

		double slope_rad = Math.toRadians(slSettings.getPlotSlope_deg());
		double bottomAzimut_rad = Math
				.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg());

		StringBuffer b = new StringBuffer("\n--- SamsaraLight final report ---");

		b.append("\nlighter: " + this.getClass().getName());
		String mode = slSettings.isParallelMode() ? "Parallel mode" : "Sequential mode";
		b.append(" " + mode);

		double treeEnergySum = 0;
		double treeNumber = stand.getTrees().size();
		for (Object o : stand.getTrees()) {
			SLLightableTree t = (SLLightableTree) o;
//			double v = t.getLightResult().getTrunkEnergy() + t.getLightResult().getCrownEnergy();
			// fa-11.12.2018
			double v = 0d;
			if (slSettings.isTrunkInterception())
				v = t.getLightResult().getTrunkEnergy() + t.getLightResult().getCrownEnergy();
			else
				v = t.getLightResult().getCrownEnergy();

			treeEnergySum += v;
		}
		b.append("\nSum of energies intercepted by " + treeNumber + " trees (MJ, trunk + crown):  " + treeEnergySum);

		double cellEnergySum = 0;

		// SamsaraLight requires a RectangularPlot, i.e. a PlotOfCells
		PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017

		double cellNumber = plotc.getCells().size();
		for (Object o : plotc.getCells()) {
			SLLightableTarget c = (SLLightableTarget) o;
//			double v = c.getLightResult().get_horizontalEnergy();
			double v = c.getLightResult().get_slopeEnergy(); // fa-10.11.2017
			cellEnergySum += v;
		}
		b.append(
				"\nSum of energies intercepted by " + cellNumber + " cells (MJ, horizontal energy):  " + cellEnergySum);

		double sensorEnergySum = 0;
		double sensorNumber = 0;
		List<SLSensor> sensors = ((SLLightableScene) stand).getSensors();
		if (sensors != null) {
			sensorNumber = sensors.size();
			for (Object o : sensors) {
				SLLightableTarget s = (SLLightableTarget) o;
//				double v = s.getLightResult().get_horizontalEnergy();
				double v = s.getLightResult().get_slopeEnergy(); // fa-10.11.2017
				sensorEnergySum += v;
			}
		}
		b.append("\nSum of energies intercepted by " + sensorNumber + " sensors (MJ/m2, horizontal energy):  "
				+ sensorEnergySum);

		// //////////////
		b.append("\n-\n--- SamsaraLight radiative balance check ---");

		// 1. beams
		double beamEnergySum = 0;

		for (SLBeam beam : beamSet.getBeams()) {
//			beamEnergySum += beam.getInitialEnergy() * Math.sin(beam.getHeightAngle_rad()) * stand.getArea();
			double scalar = Math.cos(slope_rad) * Math.sin(beam.getHeightAngle_rad()) + Math.sin(slope_rad)
					* Math.cos(beam.getHeightAngle_rad()) * Math.cos(beam.getAzimut_rad() - bottomAzimut_rad);
			beamEnergySum += beam.getInitialEnergy() * scalar * stand.getArea() / Math.cos(slope_rad); // fa-10.11.2017
		}

		// 2. cells + trees
		double interceptedEnergySum = 0;
		// double cellTagEnergySum = 0;
		// double treeTagEnergySum = 0;

		// SLTag beamTag = null;
		// SLTag energyTag = null;

		for (Object o : plotc.getCells()) {
			SLLightableTarget c = (SLLightableTarget) o;
			SquareCell cell = (SquareCell) o;

//			double v = c.getLightResult().get_horizontalEnergy() * cell.getArea();
			double v = c.getLightResult().get_slopeEnergy() * cell.getArea() / Math.cos(slope_rad); // fa-10.11.2017
			interceptedEnergySum += v;

			// double v2 = SLTagResultInterpretor.getEnergy(c, beamTag,
			// energyTag) * cell.getArea();
			// cellTagEnergySum += v2;
		}

		for (Object o : stand.getTrees()) {
			SLLightableTree t = (SLLightableTree) o;

//			double v = t.getLightResult().getTrunkEnergy() + t.getLightResult().getCrownEnergy();
			// fa-11.12.2018
			double v = 0d;
			if (slSettings.isTrunkInterception())
				v = t.getLightResult().getTrunkEnergy() + t.getLightResult().getCrownEnergy();
			else
				v = t.getLightResult().getCrownEnergy();

			interceptedEnergySum += v;

			// double v2 = SLTagResultInterpretor.getEnergy(t, beamTag,
			// energyTag);
			// treeTagEnergySum += v2;

		}

		// double tagInterceptedEnergySum = cellTagEnergySum + treeTagEnergySum;

		b.append("\nSum of energies of the beamSet (1):     " + beamEnergySum);
		b.append("\nSum of energies intercepted (2):        " + interceptedEnergySum);
		// b.append("\nSum of tag energies intercepted (3): "+tagInterceptedEnergySum);

		double error = 0;

		// if (slSettings.isTagMode ()) {
		// error = beamEnergySum - tagInterceptedEnergySum;
		// b.append("\nRadiative balance in tag mode (1 - 3): "+error);
		//
		// } else {
		error = beamEnergySum - interceptedEnergySum;
		b.append("\nRadiative balance (1 - 2):          " + error);
		b.append("\n");

		// }

		SLReporter.printInStandardOutput(b.toString());

		double EPSILON = 1E-6; // fc+fa-3.8.2017

		boolean acceptable = Math.abs(error) < EPSILON;

		if (!acceptable)
			SLReporter.printInLog(Log.WARNING, "SLModel.checkRadiativeBalance ()",
					mode + ", radiative balance error: " + error + " is larger than tolerance: " + EPSILON);

		b.append("\n");

		return acceptable;

	}

	public void run() throws Exception {

		try { // fc-23.6.2017

			if (slSettings.isTagMode())
				throw new Exception("SLLighterTag: error, make sure slSettings.tagMode is set to false");

			// fc+fa-26.4.2017 added Exception for safe foliageState management
			if (beamSet == null || beamSet.getBeams() == null || beamSet.getBeams().isEmpty())
				throw new Exception("SLLighterLegacy: could not run process lighting, could not find any beams");

			RectangularPlot plot = (RectangularPlot) stand.getPlot();

			// GL : 22/06/2012 - modified from plotLight.getSlope();
			double slope_rad = Math.toRadians(slSettings.getPlotSlope_deg());

			// GL : 22/06/2012 : modified from : plotLight.getCellSlopeArea();
			double cellHorizontalSurface = plot.getCellWidth() * plot.getCellWidth();
			double cellSurface = cellHorizontalSurface / Math.cos(slope_rad);

			// Azimuth of the vector orthogonal to the ground in the x,y system
			double bottomAzimut = Math
					.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg());

			// Search matching cell for all sensors if any
			List<SLSensor> sensors = ((SLLightableScene) stand).getSensors();
			if (sensors != null) {
				for (SLSensor sensor : sensors) {

					// Find the cells corresponding to the sensors
					if (sensor.getMatchingCell() == null) {
						SquareCell squareCell = plot.getCell(sensor.getX(), sensor.getY());
						if (squareCell == null)
							throw new Exception("SLLighterLegacy, found a sensor not belonging"
									+ " to any cell, please correct its coordinates");

						// Needed to compute z coordinate of the sensor
						sensor.setMatchingCell(squareCell);

						if (squareCell.getZCenter() > sensor.getZ())
							throw new Exception("SLLighterLegacy, the sensor " + sensor.getId()
									+ " is below the grid, please correct its height");
					}
				}
			}

			// fc-for test - you may leave this here.
			// int interceptionNumber = 0;
			// int maxInterceptionNumber = 0;
			// int maxConcurrentCellNumber = 0;

			// Make the list of cells and sensors to be targeted
			Collection targets = new ArrayList();

			// Add cells if requested
			boolean cellsIncluded = !slSettings.isSensorLightOnly();
			if (cellsIncluded) {
				// Optional restriction of cells to be processed, GL, 25/06/2012
				Collection<SquareCell> restrictedList = ((SLLightableScene) stand).getCellstoEnlight();
				if (restrictedList != null)
					targets.addAll(restrictedList); // restriction
				else
					targets.addAll(plot.getCells()); // all cells
			}

			// Add all sensors if any
			if (sensors != null)
				targets.addAll(sensors);

			// Reset energy for all targets
			for (Object o : targets) {
				SLLightableTarget target = (SLLightableTarget) o;

				target.getLightResult().resetEnergy();
			}

			// Reset energy for all trees
			for (Object o : stand.getTrees()) {
				SLLightableTree t = (SLLightableTree) o;

				t.getLightResult().resetEnergy();
			}

			// Main process

			long startLightingTime = System.currentTimeMillis(); // fa-26.04.2018

			// fa-14.05.2019: traces for checking crown form outputs
//			System.out.println("a" + "\t" + "b" + "\t" + "xShift" + "\t" + "yShift" + "\t" + "zShift" + "\t" + "x" + "\t" + "y" + "\t" + "z" + "\t" + "x2" + "\t" + "y2" + "\t" + "z2" + "\t" + "zBottom2" + "\t" + "zTop2" + "\t" + "elevation" + "\t" + "azimuth" + "\t" + "distance1" + "\t" + "distance2" + "\t" + "v1.x" + "\t" + "v1.y" + "\t" + "v1.z" + "\t" + "v2.x" + "\t" + "v2.y" + "\t" + "v2.z" + "\t" + "vBase.x" + "\t" + "vBase.y" + "\t" + "vBase.z" + "\t" + "listSize" + "\t" + "w1.x" + "\t" + "w1.y" + "\t" + "w1.z" + "\t" + "w2.x" + "\t" + "w2.y" + "\t" + "w2.z" + "\t" + "inParaboloid(v0shifted,vBasePlane)" + "\t" + "inParaboloid(v0shifted, vTargetPoint)");
//			System.out.println("a" + "\t" + "b" + "\t" + "xShift" + "\t" + "yShift" + "\t" + "zShift" + "\t" + "x0" + "\t" + "y0" + "\t" + "z0" + "\t" + "x2" + "\t" + "y2" + "\t" + "z2" + "\t" + "zBottom2" + "\t" + "zTop2" + "\t" + "elevation" + "\t" + "azimuth" + "\t" + "distance1" + "\t" + "distance2" + "\t" + "v1.x" + "\t" + "v1.y" + "\t" + "v1.z" + "\t" + "v2.x" + "\t" + "v2.y" + "\t" + "v2.z" + "\t" + "vx0.x" + "\t" + "vx0.y" + "\t" + "vx0.z" + "\t" + "vy0.x" + "\t" + "vy0.y" + "\t" + "vy0.z" + "\t" + "vz0.x" + "\t" + "vz0.y" + "\t" + "vz0.z" + "\t" + "list.size()" + "\t" + "w1.x" + "\t" + "w1.y" + "\t" + "w1.z" + "\t" + "w2.x" + "\t" + "w2.y" + "\t" + "w2.z" + "\t" + "inParaboloid(v0Shifted,vx0)" + "\t" + "inParaboloid(v0Shifted,vy0)" + "\t" + "inParaboloid(v0Shifted,vz0)" + "\t" + "inParaboloid(v0Shifted, vtarget)");
//			System.out.println("a" + "\t" + "b" + "\t" + "c" + "\t" + "xShift" + "\t" + "yShift" + "\t" + "zShift" + "\t" + "x0" + "\t" + "y0" + "\t" + "z0" + "\t" + "x2" + "\t" + "y2" + "\t" + "z2" + "\t" + "elevation" + "\t" + "azimuth" + "\t" + "distance1" + "\t" + "distance2" + "\t" + "v1.x" + "\t" + "v1.y" + "\t" + "v1.z" + "\t" + "v2.x" + "\t" + "v2.y" + "\t" + "v2.z" + "\t" + "vx0.x" + "\t" + "vx0.y" + "\t" + "vx0.z" + "\t" + "vy0.x" + "\t" + "vy0.y" + "\t" + "vy0.z" + "\t" + "vz0.x" + "\t" + "vz0.y" + "\t" + "vz0.z" + "\t" + "list.size()" + "\t" + "w1.x" + "\t" + "w1.y" + "\t" + "w1.z" + "\t" + "w2.x" + "\t" + "w2.y" + "\t" + "w2.z" + "\t" + "inEllipsoid(v0Shifted,vx0)" + "\t" + "inEllipsoid(v0Shifted,vy0)" + "\t" + "inEllipsoid(v0Shifted,vz0)" + "\t" + "inEllipsoid(v0Shifted, vtarget)");

			// For each target (cell or sensor) to enlight...
			if (slSettings.isParallelMode()) {

				// Parallel processing
				targets.parallelStream().forEach((o) -> {

					SLLightableTarget target = (SLLightableTarget) o;
					processTarget(target, slope_rad, bottomAzimut, cellSurface, cellHorizontalSurface);

				});

			} else {

				// Sequential processing
				for (Object o : targets) {

					SLLightableTarget target = (SLLightableTarget) o;
					processTarget(target, slope_rad, bottomAzimut, cellSurface, cellHorizontalSurface);

				}

			}

			// fa-26.04.2018
			long endLightingTime = System.currentTimeMillis();
			long lightingTime = (endLightingTime - startLightingTime) / (long) 1000;
			// Print execution time
			SLReporter.printInStandardOutput("\n");
			SLReporter.printInStandardOutput("Execution time for radiative balance: " + lightingTime + " sec");
			SLReporter.printInStandardOutput("\n");

			// fc+fa-2.8.2017 parallelStream () does not like these counters
			// System.out.println("SLLighterLegacy: maximum number of intercepted item "
			// + maxInterceptionNumber);

			// fc+fa-2.8.2017 parallelStream () does not like these counters
			// StatusDispatcher.print(Translator
			// .swap("SLLighter.samsaralightRadiativeBalanceCompletedtotalInterceptionNumber")
			// + " : "
			// + interceptionNumber);

		} catch (Exception e) {
			// fc-23.6.2017 added this message in the log before rethrowing the
			// exception to caller
			StatusDispatcher.print(Translator.swap("SLLighter.samsaralightRadiativeBalanceFailedSeeLog"));
			Log.println(Log.ERROR, "SLLighterLegacy", "SamsaraLight model interruption", e);
			throw new RuntimeException("SLLighterLegacy error", e);
		}

	}

	/**
	 * Run the radiative balance on the given target (cell, sensor...)
	 */
	private void processTarget(SLLightableTarget target, double slope_rad, double bottomAzimut, double cellSurface,
			double cellHorizontalSurface) {

		try {

			boolean isSensor = target instanceof SLSensor;

			SLTargetLightResult targetLightResult = target.getLightResult();

			if (slSettings.isWriteStatusDispatcher()) { // gl 5-6-2013
				// User report message
				StringBuffer msg = new StringBuffer();

				String parallelMode = slSettings.isParallelMode() ? "Parallel" : "Sequential";
				String variant = "Legacy";
				msg.append("SamsaraLight " + parallelMode + " " + variant);

				Step step = stand.getStep();
				if (step != null)
					msg.append(", " + step.getProject().getName());

				msg.append(", " + Translator.swap("SLLighter.step") + " " + stand.getDate() + ", "
						+ Translator.swap("SLLighter.lightingTarget") + " " + target.toString());

				StatusDispatcher.print(msg.toString());
			}

			double targetSlopeEnergy = 0;
			double targetSlopeEnergyDirect = 0;
			double targetSlopeEnergyDiffuse = 0;

			double targetHorizontalEnergy = 0; // GL 3/10/2012
			double targetHorizontalEnergyDiffuse = 0; // GL 3/10/2012
			double targetHorizontalEnergyDirect = 0; // GL 3/10/2012

			// For each beam of the sky...
			for (SLBeam beam : beamSet.getBeams()) {

				// Make a copy, the original beam is unchanged (parallel mode)
				if (slSettings.isParallelMode())
					beam = new SLBeam(beam);

				// Before each target cell enlightment, reset beam
				// energy to initial value
				beam.resetEnergy();

				// true below: only neighbour cells with trees
				Collection<Neighbour> neighbours = beam.getNeighbours(target.getMatchingCell(), true);

				Vector itemVector = new Vector();

				for (Neighbour neighbour : neighbours) {
					SquareCell cCell = (SquareCell) neighbour.cell;

					// The shift manages the torus approaches to work on
					// virtual infinite plots
					ShiftItem shift = neighbour.shift;

					if (!cCell.isEmpty()) {

						// For each tree in the concurrent cells
						for (Iterator t = cCell.getTrees().iterator(); t.hasNext();) {
							Tree tree = (Tree) t.next();

							double targetX = 0;
							double targetY = 0;
							double targetZ = 0;

							if (isSensor) {
								SLSensor sensor = (SLSensor) target;
								targetX = sensor.getX();
								targetY = sensor.getY();
								targetZ = sensor.getZ();

							} else {
								SquareCell cell = (SquareCell) target;
								targetX = cell.getXCenter();
								targetY = cell.getYCenter();
								targetZ = cell.getZCenter();
							}

							// Computes interception characteristics for
							// a target / a beam / a tree (average
							// distance from target / pathLength /
							// competitor tree)
							Set<SLInterceptionItem> items = slModel.intercept(targetX, targetY, targetZ, beam, tree,
									shift);

							if (items != null)
								itemVector.addAll(items);

						}
					}
				}

				// These counters are useful and take no time (measured
				// fc)
				// fc+fa-2.8.2017 parallelStream () does not like these counters
				// interceptionNumber += itemVector.size();
				// maxInterceptionNumber = Math.max(maxInterceptionNumber,
				// itemVector.size());
				// maxConcurrentCellNumber = Math.max(maxConcurrentCellNumber,
				// neighbours.size());

				// fc-tested: use directly itemArray instead of itemVector :
				// NOT faster and tab dimension pb
				SLInterceptionItem[] itemArray = new SLInterceptionItem[itemVector.size()];
				itemVector.copyInto(itemArray);

				// Interception items are sorted from furthest to
				// nearest tree from the target.
				// It is necessary to compute progressive energy loss of
				// the beam from canopy top to canopy bottom
				Arrays.sort(itemArray);

				// Reduce beam energy for each tree interception,
				// increase tree energy at the same time and
				// add the remaining beam energy to the target

				// Compute initial energy in MJ, reaching a target
				// parallel to slope without interception
				double hAngle = beam.getHeightAngle_rad();
				double azimut = beam.getAzimut_rad();

				// Beam energy in MJ/m2 on a plane orthogonal to beam
				// direction
				double beamEnergy = beam.getInitialEnergy();

				// Projection of energy on plane parallel to slope in
				// MJ/m2.
				double slopeScalar = Math.cos(slope_rad) * Math.sin(hAngle)
						+ Math.sin(slope_rad) * Math.cos(hAngle) * Math.cos(azimut - bottomAzimut);
				double slopeInitEnergy = slopeScalar * beamEnergy;

				// Projection of energy on a horizontal plane in MJ/m2
				// (GL - 3 Oct. 2012)
				double horizontalScalar = Math.cos(0) * Math.sin(hAngle)
						+ Math.sin(0) * Math.cos(hAngle) * Math.cos(azimut - bottomAzimut);
				double horizontalInitEnergy = horizontalScalar * beamEnergy;

				// From MJ/m2 to MJ.
				slopeInitEnergy = slopeInitEnergy * cellSurface;
				double slopeCurrentEnergy = slopeInitEnergy;

				horizontalInitEnergy = horizontalInitEnergy * cellHorizontalSurface;
				double horizontalCurrentEnergy = horizontalInitEnergy;

				// fc+fa-26.4.2017 Crown potential energy
				// To calculate CrownPotentialEnergy, see lower
				// double beamPotentialEnergy = slopeInitEnergy;
				// fc+fa-26.4.2017
				// A map to keep for each tree traversed by the beam
				// (key is treeId): the beam potential energy value
				// (i.e. if this tree was alone on the scene). this
				// value is updated each time the beam penetrates a new
				// crown part for the tree
				Set<SLLightableTree> crownPotentialTraversedTrees = new HashSet<>();
				AdditionMap crownPotentialBeamEnergy = new AdditionMap(); // fc+fa-26.4.2017
				AdditionMap crownPotentialBeamEnergyLower = new AdditionMap(); // fc+fa-26.4.2017
				AdditionMap crownPotentialEnergyForThisBeam = new AdditionMap(); // fc+fa-26.4.2017
				AdditionMap crownPotentialEnergyForThisBeamLower = new AdditionMap(); // fc+fa-26.4.2017

				Set<Tree> porousEnvelopTreeMemory = new HashSet<Tree>();

				// For each competing tree
				for (int i = 0; i < itemArray.length; i++) {
					SLInterceptionItem item = itemArray[i];

					Tree t = item.getTree();
					SLLightableTree lightableTree = (SLLightableTree) t;

					crownPotentialTraversedTrees.add(lightableTree); // fc+fa-26.4.2017

					SLTreeLightResult treeLight = lightableTree.getLightResult();
					SLTreePart treePart = item.getTreePart();

					// fc+fa-26.4.2017 Crown potential energy
					// When the beam penetrates a tree not yet
					// traversed, we reset the beam potential energy for
					// this tree with the value of the initial beam
					// energy
					if (!isSensor && !crownPotentialBeamEnergy.getKeys().contains("" + t.getId())) {
						crownPotentialBeamEnergy.overwriteValue("" + t.getId(), slopeInitEnergy);
						crownPotentialBeamEnergyLower.overwriteValue("" + t.getId(), slopeInitEnergy);
					}

					if (treePart instanceof SLTrunk) {

						if (!isSensor) {
							if (beam.isDirect()) {
								treePart.addDirectEnergy(slopeCurrentEnergy);
							} else {
								treePart.addDiffuseEnergy(slopeCurrentEnergy);
							}

						}

						// fc+fa-26.4.2017 Trunk potential energy is
						// unused
						// LATER treePart.addPotentialEnergy(e)

						slopeCurrentEnergy = 0;
						horizontalCurrentEnergy = 0;

					} else {

						// Crown part
						SLCrownPart crownPart = (SLCrownPart) treePart;
						boolean isVoxel = treePart instanceof SLVoxelCrownPart;

						// Turbid medimum
						if (slSettings.isTurbidMedium()) {

							// fc+fa-26.4.2017 introduced ladProportion
							double ladProp = 1;
							// double greenProp = 1; // fa-19.06.2017

							// In tagMode, some beams are built for
							// specific key doys
							int doy = beam.getDoy();
							if (doy > 0) {
								if (foliageStateManager == null)
									throw new RuntimeException(
											"Error in SLLighterLegacy, finding a beam with a doy > 0, requires"
													+ " a foliageState manager");
								// Calculates ladProp at tree level. nb-06.11.2019
								// ladProp =
								// foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(),
								// doy);
								ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
								// greenProp =
								// foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(),
								// doy); // fa-19.06.2017
							}

							// double leafAreaDensity = ((SLCrownPart)
							// treePart).getLeafAreaDensity() * ladProp * greenProp;
							// // fa-19.06.2017
							double leafAreaDensity = ((SLCrownPart) treePart).getLeafAreaDensity() * ladProp; // fa-09.08.2017

							// If the coef = 0.5, it corresponds to a
							// spherical leaf angle distribution
							double extinctionCoef = ((SLCrownPart) treePart).getExtinctionCoefficient(); // /
																											// Math.sin(beam.getHeightAngle_rad());
																											// //
																											// mj+fa-23.10.2017
							
							
							// cr-08.02.2023 add this functionality from tag mode
							// cr+fc-01.04.2022 Add a possibility to get a variable extinction coefficient
							if(foliageStateManager != null && foliageStateManager instanceof SLFoliageStateManagerWithExtinctionCoefficient) {
								SLFoliageStateManagerWithExtinctionCoefficient fsm = (SLFoliageStateManagerWithExtinctionCoefficient) foliageStateManager;
								extinctionCoef = fsm.getExtinctionCoefficientTree(lightableTree, crownPart, hAngle, azimut, doy, beam.isDirect(), beam.getTag());
							}

							double clumpingFactor = 1.0;

							double interceptedE = slModel.beerLambert(slopeCurrentEnergy, extinctionCoef,
									clumpingFactor, leafAreaDensity, item.getPathLength());

							if (!isSensor) {
								if (beam.isDirect()) {
									treePart.addDirectEnergy(interceptedE);
								} else {
									treePart.addDiffuseEnergy(interceptedE);
								}

							}

							slopeCurrentEnergy -= interceptedE;

							horizontalCurrentEnergy -= slModel.beerLambert(horizontalCurrentEnergy, extinctionCoef,
									clumpingFactor, leafAreaDensity, item.getPathLength());

							// fc+fa-26.4.2017 Crown potential energy
							// Beer-Lambert law
							if (!isSensor) {
								double interceptedPotentialEnergy = slModel.beerLambert(
										crownPotentialBeamEnergy.getValue("" + t.getId()), extinctionCoef,
										clumpingFactor, leafAreaDensity, item.getPathLength());

								treeLight.addCrownPotentialEnergy(interceptedPotentialEnergy);
								crownPotentialBeamEnergy.addValue("" + t.getId(), -interceptedPotentialEnergy);
								crownPotentialEnergyForThisBeam.addValue("" + t.getId(), interceptedPotentialEnergy);

								if (crownPart.isLower()) {
									// Beer-Lambert law
									interceptedPotentialEnergy = slModel.beerLambert(
											crownPotentialBeamEnergyLower.getValue("" + t.getId()), extinctionCoef,
											clumpingFactor, leafAreaDensity, item.getPathLength());
									treeLight.addLowerCrownPotentialEnergy(interceptedPotentialEnergy);
									crownPotentialBeamEnergyLower.addValue("" + t.getId(), -interceptedPotentialEnergy);
									crownPotentialEnergyForThisBeamLower.addValue("" + t.getId(),
											interceptedPotentialEnergy);
								}
							}
							// fc+fa-26.4.2017

						} else { // Porous envelop

							// fa-21.6.2017 introduced ladProportion
							double ladProp = 1;
							// double greenProp = 1; // fa-19.06.2017

							// In tagMode, some beams are built for
							// specific key doys
							int doy = beam.getDoy();
							if (doy > 0) {
								if (foliageStateManager == null)
									throw new RuntimeException(
											"Error in SLLighterLegacy, finding a beam with a doy > 0, requires"
													+ " a foliageState manager");
								// Calculates ladProp at tree level. nb-06.11.2019
								// ladProp =
								// foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(),
								// doy);
								ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
								// greenProp =
								// foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(),
								// doy); // fa-19.06.2017
							}

							// In case of voxels, they must all be processed
							// In other cases, porousEnvelop concerns the
							// whole crown, so only one crown part must be
							// processed
							boolean crownPartToBeProcessed = isVoxel || !porousEnvelopTreeMemory.contains(t);

							// Since the crown openness concern the
							// entire crown and not a crownPart, we need
							// to attenuate the energy only once
							if (crownPartToBeProcessed) {

								double transmissivity = 0;
								if (isVoxel) {
									SLVoxelCrownPart voxel = (SLVoxelCrownPart) treePart;
									// transmissivity = voxel.getTransmissivity() *
									// ladProp * greenProp; // fa-19.06.2017;
									transmissivity = voxel.getTransmissivity() * ladProp; // fa-09.08.2017;

								} else {
									// transmissivity =
									// lightableTree.getCrownTransmissivity() *
									// ladProp * greenProp; // fa-21.06.2017;
									transmissivity = lightableTree.getCrownTransmissivity() * ladProp; // fa-09.08.2017;
								}

								double interceptedE = (1 - transmissivity) * slopeCurrentEnergy;

								if (!isSensor) {
									if (beam.isDirect()) {
										treePart.addDirectEnergy(interceptedE);
									} else {
										treePart.addDiffuseEnergy(interceptedE);
									}

								}

								slopeCurrentEnergy -= interceptedE;
								horizontalCurrentEnergy -= horizontalCurrentEnergy * (1 - transmissivity);

								// fc+fa-26.4.2017 Crown potential energy
								// Porous envelop
								if (!isSensor) {
									double interceptedPotentialEnergy = crownPotentialBeamEnergy
											.getValue("" + t.getId()) * (1 - transmissivity);

									treeLight.addCrownPotentialEnergy(interceptedPotentialEnergy);
									crownPotentialBeamEnergy.addValue("" + t.getId(), -interceptedPotentialEnergy);
									crownPotentialEnergyForThisBeam.addValue("" + t.getId(),
											interceptedPotentialEnergy);

									if (crownPart.isLower()) {
										// Porous envelop
										interceptedPotentialEnergy = crownPotentialBeamEnergyLower
												.getValue("" + t.getId()) * (1 - transmissivity);
										treeLight.addLowerCrownPotentialEnergy(interceptedPotentialEnergy);
										crownPotentialBeamEnergyLower.addValue("" + t.getId(),
												-interceptedPotentialEnergy);
										crownPotentialEnergyForThisBeamLower.addValue("" + t.getId(),
												interceptedPotentialEnergy);
									}
								}
								// fc+fa-26.4.2017

								// One crown part has been processed for
								// this tree
								porousEnvelopTreeMemory.add(t);
							}
						}
					}

				}

				// Cell energy is in MJ/m2 and projected on a plane
				// parallel to the slope
				targetSlopeEnergy += slopeCurrentEnergy / cellSurface;
				targetHorizontalEnergy += horizontalCurrentEnergy / cellHorizontalSurface;

				double targetEnergy = slopeCurrentEnergy / cellSurface;

				if (beam.isDirect()) {
					targetSlopeEnergyDirect += targetEnergy;
					targetHorizontalEnergyDirect += horizontalCurrentEnergy / cellHorizontalSurface;

				} else {
					targetSlopeEnergyDiffuse += targetEnergy;
					targetHorizontalEnergyDiffuse += horizontalCurrentEnergy / cellHorizontalSurface;

				}

			} // end of the loop going through every beams

			// Store info for the target (cell or sensor)

			targetLightResult
					.set_aboveCanopyHorizontalEnergy(beamSet.getHorizontalDiffuse() + beamSet.getHorizontalDirect());

			// Horizontal energy
			double relativeHorizontalEnergy = 100 * targetHorizontalEnergy
					/ (beamSet.getHorizontalDiffuse() + beamSet.getHorizontalDirect());

			double relativeHorizontalDiffuseEnergy = 100 * targetHorizontalEnergyDiffuse
					/ beamSet.getHorizontalDiffuse();

			double relativeHorizontalDirectEnergy = 100 * targetHorizontalEnergyDirect / beamSet.getHorizontalDirect();

			if (relativeHorizontalEnergy > 100) {
				SLReporter.printInStandardOutput(
						"SLLighterLegacy: relative horizontal energy was over 100% : " + relativeHorizontalEnergy);
				relativeHorizontalEnergy = 100;
			}

			if (relativeHorizontalDiffuseEnergy > 100)
				relativeHorizontalDiffuseEnergy = 100;

			if (relativeHorizontalDirectEnergy > 100)
				relativeHorizontalDirectEnergy = 100;

			targetLightResult.set_horizontalEnergy(targetHorizontalEnergy);
			targetLightResult.set_horizontalEnergyDirect(targetHorizontalEnergyDirect);
			targetLightResult.set_horizontalEnergyDiffuse(targetHorizontalEnergyDiffuse);

			targetLightResult.set_horizontalRelativeEnergy(relativeHorizontalEnergy);
			targetLightResult.set_horizontalRelativeEnergyDirect(relativeHorizontalDirectEnergy);
			targetLightResult.set_horizontalRelativeEnergyDiffuse(relativeHorizontalDiffuseEnergy);

			// Slope energy
			double relativeSlopeEnergy = 100 * targetSlopeEnergy
					/ (beamSet.getSlopeDiffuse() + beamSet.getSlopeDirect());
			double relativeSlopeDiffuseEnergy = 100 * targetSlopeEnergyDiffuse / beamSet.getSlopeDiffuse();
			double relativeSlopeDirectEnergy = 100 * targetSlopeEnergyDirect / beamSet.getSlopeDirect();

			if (relativeSlopeEnergy > 100) {
				SLReporter.printInStandardOutput(
						"SLLighterLegacy: relative slope energy was over 100% : " + relativeSlopeEnergy);
				relativeSlopeEnergy = 100;
			}

			if (relativeSlopeDiffuseEnergy > 100)
				relativeSlopeDiffuseEnergy = 100;

			if (relativeSlopeDirectEnergy > 100)
				relativeSlopeDirectEnergy = 100;

			targetLightResult.set_slopeEnergy(targetSlopeEnergy);
			targetLightResult.set_slopeEnergyDirect(targetSlopeEnergyDirect);
			targetLightResult.set_slopeEnergyDiffuse(targetSlopeEnergyDiffuse);

			targetLightResult.set_slopeRelativeEnergy(relativeSlopeEnergy);
			targetLightResult.set_slopeRelativeEnergyDirect(relativeSlopeDirectEnergy);
			targetLightResult.set_slopeRelativeEnergyDiffuse(relativeSlopeDiffuseEnergy);

		} catch (Exception e) {
			// fc-15.2.2021 Added a try / catch to search an error in PDGModelLight
			Log.println(Log.ERROR, "SLLighterLegacy.processTarget ()", "An error occurred", e);
			
			// cr+fc-01.04.2022 processTarget can be called by threads and hence throws only RuntimeException
			RuntimeException re = null;
			if(e instanceof RuntimeException) {
				re = (RuntimeException) e;
			}else {
				re = new RuntimeException(e);
			}
			
			throw re;
		}

	}

}
