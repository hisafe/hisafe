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
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.lib.samsaralight.tag.SLTag;
import capsis.lib.samsaralight.tag.SLTagTargetResult;
import capsis.lib.samsaralight.tag.SLTagTreeResult;
import jeeb.lib.util.AdditionMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;


/**
 * An implementation of the original SamsaraLight algorithm, with sensors apart.
 * 
 * @author F. de Coligny, F. André - August 2017
 */
public class SLLighterOriginal extends SLLighter {

	private TreeList stand;
	private SLFoliageStateManager foliageStateManager;
	private SLBeamSet beamSet;
	private SLSettings slSettings;
	private SLModel slModel;

	/**
	 * Constructor
	 */
	public SLLighterOriginal(TreeList stand, SLFoliageStateManager foliageStateManager, SLBeamSet beamSet,
			SLSettings slSettings, SLModel slModel) throws Exception {
		this.stand = stand;
		this.foliageStateManager = foliageStateManager; // May b null
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
		double bottomAzimut_rad = Math.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg());

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

		// SamsaraLight requires a RectangularPlot, i.e. a PlotOfCells
		PlotOfCells plotc = stand.getPlot(); // fc-26.10.2017

		double cellEnergySum = 0;
		double cellNumber = plotc.getCells().size();
		for (Object o : plotc.getCells()) {
			SLLightableTarget c = (SLLightableTarget) o;
//			double v = c.getLightResult().get_horizontalEnergy();
			double v = c.getLightResult().get_slopeEnergy(); //fa-10.11.2017
			cellEnergySum += v;
		}
		b.append("\nSum of energies intercepted by " + cellNumber + " cells (MJ, horizontal energy):  " + cellEnergySum);

		double sensorEnergySum = 0;
		double sensorNumber = 0;
		List<SLSensor> sensors = ((SLLightableScene) stand).getSensors();
		if (sensors != null) {
			sensorNumber = sensors.size();
			for (Object o : sensors) {
				SLLightableTarget s = (SLLightableTarget) o;
//				double v = s.getLightResult().get_horizontalEnergy();
				double v = s.getLightResult().get_slopeEnergy(); //fa-10.11.2017
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
			double scalar = Math.cos(slope_rad) * Math.sin(beam.getHeightAngle_rad()) + Math.sin(slope_rad) * Math.cos(beam.getHeightAngle_rad()) * Math.cos(beam.getAzimut_rad() - bottomAzimut_rad);
			beamEnergySum += beam.getInitialEnergy() * scalar * stand.getArea() / Math.cos(slope_rad); //fa-10.11.2017
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
			double v = c.getLightResult().get_slopeEnergy() * cell.getArea() / Math.cos(slope_rad); //fa-10.11.2017
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
		// b.append("\nSum of tag energies intercepted (3):    "+tagInterceptedEnergySum);

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
			SLReporter.printInLog(Log.WARNING, "SLModel.checkRadiativeBalance ()", mode + ", radiative balance error: " + error
					+ " is larger than tolerance: " + EPSILON);

		b.append("\n");

		return acceptable;


	}

	public void run() throws Exception {

		// fc+fa-3.8.2017 DO NOT use this version, prefer SLLighterLegacy
		//
		// This original version of the SamsaraLight lib was refactored
		// carefully into the SLLighterLegacy version
		// Several little things concerning reseting and result storing have
		// been added in the SLLighterLegacy version which is now the reference
		// version
		// This SLLighterOriginal is just there for checks and should be
		// discarded some day

		try { // fc-23.6.2017

			// fc+fa-26.4.2017 added Exception for safe foliageState management
			if (beamSet == null || beamSet.getBeams() == null || beamSet.getBeams().isEmpty())
				throw new Exception("SLLighterOriginal: could not run process lighting, could not find any beams");

			RectangularPlot plot = (RectangularPlot) stand.getPlot();

			// GL : 22/06/2012 - modified from plotLight.getSlope();
			double slope_rad = Math.toRadians(slSettings.getPlotSlope_deg());

			// GL : 22/06/2012 : modified from : plotLight.getCellSlopeArea();
			double cellHorizontalSurface = plot.getCellWidth() * plot.getCellWidth();
			double cellSurface = cellHorizontalSurface / Math.cos(slope_rad);

			// Azimuth of the vector orthogonal to the ground in the x,y system
			double bottomAzimut = Math
					.toRadians(-slSettings.getPlotAspect_deg() + slSettings.getNorthToXAngle_cw_deg());

			if (slSettings.isWriteStatusDispatcher ())
				SLReporter.printInStandardOutput("SLLighterOriginal.run() : cellSurface = " + cellSurface);

			// Reset energy for target cells before their enlightment
			for (Iterator i = plot.getCells().iterator(); i.hasNext();) {
				SquareCell c = (SquareCell) i.next();
				SLTargetLightResult cellLight = ((SLLightableTarget) c).getLightResult();
				cellLight.resetEnergy();
			}

			// fc+fa-26.4.2017
			// Reset results for all trees before new computation
			for (Iterator i = stand.getTrees().iterator(); i.hasNext();) {
				Tree t = (Tree) i.next();
				SLTreeLightResult treeLight = ((SLLightableTree) t).getLightResult();
				if (treeLight == null) // fc-23.6.2017
					Log.println(Log.ERROR, "SLLighterOriginal.run ()", "Found a tree with a null treeLight, tree: " + t);
				treeLight.resetEnergy();
			}

			// fc-for test - you may leave this here.
			int maxSize = 0;
			int maxCcSize = 0;

			// Define the cells to enlight, optional optimization, GL,
			// 25/06/2012
			Collection<SquareCell> targetCells = new ArrayList<SquareCell>();
			targetCells = ((SLLightableScene) stand).getCellstoEnlight();
			if (targetCells == null)
				targetCells = plot.getCells();

			if (!slSettings.isSensorLightOnly()) {

				// Main process

				// For each target cell to enlight.
				for (SquareCell cell : targetCells) {

					SLTargetLightResult cellLight = ((SLLightableTarget) cell).getLightResult();

					if (slSettings.isWriteStatusDispatcher ()) { // gl 5-6-2013
						// User wait message
						String msg = "";
						try {
							Step step = stand.getStep();
							Project scenario = step.getProject();
							msg = Translator.swap("SLLighterOriginal.scenario") + " " + scenario.getName() + " ";
						} catch (Exception e) {
							// This is not an error: the scene is not supposed
							// to be tied to a Step in processLighting ()
						}

						// Step is being build
						msg += Translator.swap("SLLighterOriginal.step") + " " + stand.getCaption() + " "
								+ Translator.swap("SLLighterOriginal.lightingForCell") + " " + cell.getPosition();
						StatusDispatcher.print(msg);
					}

					double cellSlopeEnergy = 0;
					double cellSlopeEnergyDirect = 0;
					double cellSlopeEnergyDiffuse = 0;

					double cellHorizontalEnergy = 0; // GL 3/10/2012
					double cellHorizontalEnergyDiffuse = 0; // GL 3/10/2012
					double cellHorizontalEnergyDirect = 0; // GL 3/10/2012

					// For each beam of the sky...
					for (SLBeam beam : beamSet.getBeams()) {

						// Before each target cell enlightment, reset beam
						// energy to initial value
						beam.resetEnergy();

						// true below: only neighbour cells with trees
						Collection<Neighbour> neighbours = beam.getNeighbours(cell, true);

						Vector vctI = new Vector();

						for (Neighbour neighbour : neighbours) {
							SquareCell cCell = (SquareCell) neighbour.cell;

							// The shift manages the torus approaches to work on
							// virtual infinite plots
							ShiftItem shift = neighbour.shift;

							if (!cCell.isEmpty()) {

								// For each tree in the concurrent cells
								for (Iterator t = cCell.getTrees().iterator(); t.hasNext();) {
									Tree tree = (Tree) t.next();

									double targetX = cell.getXCenter();
									double targetY = cell.getYCenter();
									double targetZ = cell.getZCenter();

									Set<SLInterceptionItem> items = null;

									// Computes interception characteristics for
									// a targetCell / a beam/ a tree (average
									// distance from target cell / pathLength /
									// competitor tree)
									items = slModel.intercept(targetX, targetY, targetZ, beam, tree, shift);

									if (items != null) {
										vctI.addAll(items);
									}

								}
							}
						}

						// These counters are useful and take no time (measured
						// fc).
						if (vctI.size() > maxSize) {
							maxSize = vctI.size();
						}
						if (neighbours.size() > maxCcSize) {
							maxCcSize = neighbours.size();
						}

						// fc-tested: use directly tabI instead of vctI : NOT
						// faster and tab dimension pb.
						SLInterceptionItem[] tabI = new SLInterceptionItem[vctI.size()];
						vctI.copyInto(tabI);

						// Interception items are sorted from farthest to
						// nearest tree from the target cell.
						// It is necessary to compute progressive energy loss of
						// the beam from canopy top to canopy bottom.
						Arrays.sort(tabI);

						// Reduce beam energy for each tree interception,
						// increase tree energy at the same time and
						// add the remaining beam energy to the target cell

						// Compute initial energy in MJ, reaching a cell
						// parallel to slope without interception.
						double hAngle = beam.getHeightAngle_rad();
						double azimut = beam.getAzimut_rad();

						// Beam energy in MJ/m2 on a plane orthogonal to beam
						// direction
						double beamEnergy = beam.getInitialEnergy();

						// Projection of energy on plane parallel to slope in
						// MJ/m2.
						double slopeScalar = Math.cos(slope_rad) * Math.sin(hAngle) + Math.sin(slope_rad)
								* Math.cos(hAngle) * Math.cos(azimut - bottomAzimut);
						double slopeInitEnergy = slopeScalar * beamEnergy;

						// Projection of energy on a horizontal plane in MJ/m2.
						// (GL - 3 Oct. 2012)
						double horizontalScalar = Math.cos(0) * Math.sin(hAngle) + Math.sin(0) * Math.cos(hAngle)
								* Math.cos(azimut - bottomAzimut);
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
						AdditionMap beamCrownPotentialEnergy = new AdditionMap(); // fc+fa-26.4.2017
						AdditionMap beamLowerCrownPotentialEnergy = new AdditionMap(); // fc+fa-26.4.2017
						AdditionMap crownPotentialEnergyForThisBeam = new AdditionMap(); // fc+fa-26.4.2017
						AdditionMap lowerCrownPotentialEnergyForThisBeam = new AdditionMap(); // fc+fa-26.4.2017

						Set<Tree> treeMemory = new HashSet<Tree>();

						// fc+fa-26.4.2017
						Set<SLLightableTree> traversedTrees = new HashSet<>();

						// For each competing tree.
						for (int i = 0; i < tabI.length; i++) {
							SLInterceptionItem item = tabI[i];

							Tree t = item.getTree();
							SLLightableTree lightableTree = (SLLightableTree) t;

							traversedTrees.add(lightableTree); // fc+fa-26.4.2017

							SLTreeLightResult treeLight = lightableTree.getLightResult();
							SLTreePart treePart = item.getTreePart();

							// fc+fa-26.4.2017 Crown potential energy
							// When the beam penetrates a tree not yet
							// traversed, we reset the beam potential energy for
							// this tree with the value of the initial beam
							// energy
							if (!beamCrownPotentialEnergy.getKeys().contains("" + t.getId())) {
								beamCrownPotentialEnergy.overwriteValue("" + t.getId(), slopeInitEnergy);
								beamLowerCrownPotentialEnergy.overwriteValue("" + t.getId(), slopeInitEnergy);
							}

							if (treePart instanceof SLTrunk) {

								if (beam.isDirect()) {
									treePart.addDirectEnergy(slopeCurrentEnergy);
								} else {
									treePart.addDiffuseEnergy(slopeCurrentEnergy);
								}

								// fc+fa-25.4.2017
								if (slSettings.isTagMode()) {

									SLTag energyTag = beam.isDirect() ? SLTag.DIRECT : SLTag.DIFFUSE;

									treeLight.addTagTreeResult(new SLTagTreeResult(beam.getTag(), SLTag.TRUNK,
											energyTag, slopeCurrentEnergy));
								}

								// fc+fa-26.4.2017 Trunk potential energy is
								// unused
								// LATER treePart.addPotentialEnergy(e)

								slopeCurrentEnergy = 0;
								horizontalCurrentEnergy = 0;

							} else {

								SLCrownPart crownPart = (SLCrownPart) treePart;

								// Crown part
								if (slSettings.isTurbidMedium ()) {

									// fc+fa-26.4.2017 introduced ladProportion
									double ladProp = 1;
//									double greenProp = 1; // fa-19.06.2017

									// In tagMode, some beams are built for
									// specific key doys
									int doy = beam.getDoy();
									if (doy > 0) {
										if (foliageStateManager == null)
											throw new Exception(
													"Error in SLLighterOriginal.run (), finding a beam with a doy > 0, requires"
															+ " a foliageState manager");
										// Calculates ladProp at tree level. nb-06.11.2019
										//ladProp = foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(), doy);
										ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
//										greenProp = foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(), doy); // fa-19.06.2017
									}

//									double leafAreaDensity = ((SLCrownPart) treePart).getLeafAreaDensity() * ladProp* greenProp; // fa-19.06.2017
									double leafAreaDensity = ((SLCrownPart) treePart).getLeafAreaDensity() * ladProp; // fa-09.08.2017

									// If the coef = 0.5, it corresponds to a
									// spherical leaf angle distribution
									double extinctionCoef = ((SLCrownPart) treePart).getExtinctionCoefficient();

									double clumpingFactor = 1.0;

									double interceptedE = slModel.beerLambert(slopeCurrentEnergy, extinctionCoef,
											clumpingFactor, leafAreaDensity, item.getPathLength());

									if (beam.isDirect()) {
										treePart.addDirectEnergy(interceptedE);
									} else {
										treePart.addDiffuseEnergy(interceptedE);
									}

									// fc+fa-25.4.2017
									if (slSettings.isTagMode()) {

										SLTag energyTag = beam.isDirect() ? SLTag.DIRECT : SLTag.DIFFUSE;

										treeLight.addTagTreeResult(new SLTagTreeResult(beam.getTag(), SLTag.CROWN,
												energyTag, interceptedE));
									}

									slopeCurrentEnergy -= interceptedE;

									horizontalCurrentEnergy -= slModel.beerLambert(horizontalCurrentEnergy,
											extinctionCoef, clumpingFactor, leafAreaDensity, item.getPathLength());

									// fc+fa-26.4.2017 Crown potential energy
									// Beer-Lambert law
									double interceptedPotentialEnergy = slModel.beerLambert(
											beamCrownPotentialEnergy.getValue("" + t.getId()), extinctionCoef,
											clumpingFactor, leafAreaDensity, item.getPathLength());

									treeLight.addCrownPotentialEnergy(interceptedPotentialEnergy);
									beamCrownPotentialEnergy.addValue("" + t.getId(), -interceptedPotentialEnergy);
									crownPotentialEnergyForThisBeam
											.addValue("" + t.getId(), interceptedPotentialEnergy);

									if (crownPart.isLower()) {
										// Beer-Lambert law
										interceptedPotentialEnergy = slModel.beerLambert(
												beamLowerCrownPotentialEnergy.getValue("" + t.getId()), extinctionCoef,
												clumpingFactor, leafAreaDensity, item.getPathLength());
										treeLight.addLowerCrownPotentialEnergy(interceptedPotentialEnergy);
										beamLowerCrownPotentialEnergy.addValue("" + t.getId(),
												-interceptedPotentialEnergy);
										lowerCrownPotentialEnergyForThisBeam.addValue("" + t.getId(),
												interceptedPotentialEnergy);
									}
									// fc+fa-26.4.2017

									// Porous crown made of voxels (gl, dec
									// 2016)
								} else if (treePart instanceof SLVoxelCrownPart && !slSettings.isTurbidMedium ()) {

									// fa-21.06.2017 introduced ladProportion
									double ladProp = 1;
//									double greenProp = 1; // fa-19.06.2017

									// In tagMode, some beams are built for
									// specific key doys
									int doy = beam.getDoy();
									if (doy > 0) {
										if (foliageStateManager == null)
											throw new Exception(
													"Error in SLLighterOriginal.run (), finding a beam with a doy > 0, requires"
															+ " a foliageState manager");
										// Calculates ladProp at tree level. nb-06.11.2019
										//ladProp = foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(), doy);
										ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
//										greenProp = foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(), doy); // fa-19.06.2017
									}

									SLVoxelCrownPart voxel = (SLVoxelCrownPart) treePart;

//									double transmissivity = voxel.getTransmissivity() * ladProp * greenProp; // fa-19.06.2017;
									double transmissivity = voxel.getTransmissivity() * ladProp; // fa-09.08.2017;
									double interceptedE = slopeCurrentEnergy * (1 - transmissivity);

									if (beam.isDirect()) {
										treePart.addDirectEnergy(interceptedE);
									} else {
										treePart.addDiffuseEnergy(interceptedE);
									}

									// fa-21.6.2017
									if (slSettings.isTagMode()) {

										SLTag energyTag = beam.isDirect() ? SLTag.DIRECT : SLTag.DIFFUSE;

										treeLight.addTagTreeResult(new SLTagTreeResult(beam.getTag(), SLTag.CROWN,
												energyTag, interceptedE));
									}

									slopeCurrentEnergy -= interceptedE;
									horizontalCurrentEnergy -= horizontalCurrentEnergy * (1 - transmissivity);

									// fc+fa-26.4.2017 Crown potential energy
									// Porous envelop
									double interceptedPotentialEnergy = beamCrownPotentialEnergy.getValue(""
											+ t.getId())
											* (1 - transmissivity);

									treeLight.addCrownPotentialEnergy(interceptedPotentialEnergy);
									crownPotentialEnergyForThisBeam
											.addValue("" + t.getId(), interceptedPotentialEnergy);
									beamCrownPotentialEnergy.addValue("" + t.getId(), -interceptedPotentialEnergy);
									// fc+fa-26.4.2017

									// Porous envelop with crown envelop
								} else {

									// fa-21.6.2017 introduced ladProportion
									double ladProp = 1;
//									double greenProp = 1; // fa-19.06.2017

									// In tagMode, some beams are built for
									// specific key doys
									int doy = beam.getDoy();
									if (doy > 0) {
										if (foliageStateManager == null)
											throw new Exception(
													"Error in SLLighterOriginal.run (), finding a beam with a doy > 0, requires"
															+ " a foliageState manager");
										// Calculates ladProp at tree level. nb-06.11.2019
										//ladProp = foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(), doy);
										ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
//										greenProp = foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(), doy); // fa-19.06.2017
									}

									// Since the crown openness concern the
									// entire crown and not a crownpart, we need
									// to attenuate the energy only once
									if (!treeMemory.contains(t)) {

//										double transmissivity = lightableTree.getCrownTransmissivity() * ladProp * greenProp; // fa-21.06.2017;
										double transmissivity = lightableTree.getCrownTransmissivity() * ladProp; // fa-09.08.2017;

										double interceptedE = (1 - transmissivity) * slopeCurrentEnergy;

										if (beam.isDirect()) {
											treePart.addDirectEnergy(interceptedE);
										} else {
											treePart.addDiffuseEnergy(interceptedE);
										}

										// fc+fa-25.4.2017
										if (slSettings.isTagMode()) {

											SLTag energyTag = beam.isDirect() ? SLTag.DIRECT : SLTag.DIFFUSE;

											treeLight.addTagTreeResult(new SLTagTreeResult(beam.getTag(), SLTag.CROWN,
													energyTag, interceptedE));
										}

										slopeCurrentEnergy -= interceptedE;
										horizontalCurrentEnergy -= horizontalCurrentEnergy * (1 - transmissivity);

										// fc+fa-26.4.2017 Crown potential
										// energy
										// Porous envelop
										double interceptedPotentialEnergy = beamCrownPotentialEnergy.getValue(""
												+ t.getId())
												* (1 - transmissivity);

										treeLight.addCrownPotentialEnergy(interceptedPotentialEnergy);
										beamCrownPotentialEnergy.addValue("" + t.getId(), -interceptedPotentialEnergy);
										crownPotentialEnergyForThisBeam.addValue("" + t.getId(),
												interceptedPotentialEnergy);

										if (crownPart.isLower()) {
											// Porous envelop
											interceptedPotentialEnergy = beamLowerCrownPotentialEnergy.getValue(""
													+ t.getId())
													* (1 - transmissivity);
											treeLight.addLowerCrownPotentialEnergy(interceptedPotentialEnergy);
											beamLowerCrownPotentialEnergy.addValue("" + t.getId(),
													-interceptedPotentialEnergy);
											lowerCrownPotentialEnergyForThisBeam.addValue("" + t.getId(),
													interceptedPotentialEnergy);
										}
										// fc+fa-26.4.2017

										treeMemory.add(t);
									}
								}
							}

						}

						// Add tag results for potential energy in the trees
						// this beam just intercepted
						// fc+fa-26.4.2017
						if (slSettings.isTagMode())
							for (SLLightableTree t : traversedTrees) {
								Tree tree = (Tree) t;
								SLTreeLightResult treeLight = t.getLightResult();

								double crownValue = crownPotentialEnergyForThisBeam.getValue("" + tree.getId());
								treeLight.addTagTreeResult(new SLTagTreeResult(beam.getTag(), SLTag.CROWN,
										SLTag.CROWN_POTENTIAL, crownValue));

								double lowerCrownValue = lowerCrownPotentialEnergyForThisBeam.getValue(""
										+ tree.getId());
								treeLight.addTagTreeResult(new SLTagTreeResult(beam.getTag(), SLTag.CROWN,
										SLTag.LOWER_CROWN_POTENTIAL, lowerCrownValue));
							}

						// Cell energy is in MJ/m2 and projected on a plane
						// parallel to the slope
						cellSlopeEnergy += slopeCurrentEnergy / cellSurface;
						cellHorizontalEnergy += horizontalCurrentEnergy / cellHorizontalSurface;

						double cellEnergy = slopeCurrentEnergy / cellSurface;

						if (beam.isDirect()) {
							cellSlopeEnergyDirect += cellEnergy;
							cellHorizontalEnergyDirect += horizontalCurrentEnergy / cellHorizontalSurface;

						} else {
							cellSlopeEnergyDiffuse += cellEnergy;
							cellHorizontalEnergyDiffuse += horizontalCurrentEnergy / cellHorizontalSurface;

						}

						// fc+fa-25.4.2017
						SLTag energyTag = beam.isDirect() ? SLTag.DIRECT : SLTag.DIFFUSE;

//						cellLight.addTagResult(new SLTagTargetResult(beam.getTag(), energyTag, cellEnergy));
						cellLight.addTagResult(new SLTagTargetResult(beam.getTag(), energyTag, SLTag.SLOPE, slopeCurrentEnergy / cellSurface)); // fa-08.01.2018
						cellLight.addTagResult(new SLTagTargetResult(beam.getTag(), energyTag, SLTag.HORIZONTAL, horizontalCurrentEnergy / cellHorizontalSurface)); // fa-08.01.2018

					} // end of the loop going through every beams

					cellLight.set_aboveCanopyHorizontalEnergy(beamSet.getHorizontalDiffuse()
							+ beamSet.getHorizontalDirect());

					cellLight.set_horizontalEnergy(cellHorizontalEnergy);
					cellLight.set_horizontalEnergyDirect(cellHorizontalEnergyDirect);
					cellLight.set_horizontalEnergyDiffuse(cellHorizontalEnergyDiffuse);

					cellLight.set_slopeEnergyDirect(cellSlopeEnergyDirect);
					cellLight.set_slopeEnergyDiffuse(cellSlopeEnergyDiffuse);
					cellLight.set_slopeEnergy(cellSlopeEnergy);

					double relativeSlopeEnergy = 100 * cellSlopeEnergy
							/ (beamSet.getSlopeDiffuse() + beamSet.getSlopeDirect());
					double relativeSlopeDiffuseEnergy = 100 * cellSlopeEnergyDiffuse / beamSet.getSlopeDiffuse();
					double relativeSlopeDirectEnergy = 100 * cellSlopeEnergyDirect / beamSet.getSlopeDirect();

					if (relativeSlopeEnergy > 100) {
						SLReporter.printInStandardOutput("SLLighterOriginal.run (): relative slope energy was over 100% : "
								+ relativeSlopeEnergy);
						relativeSlopeEnergy = 100;
					}

					if (relativeSlopeDiffuseEnergy > 100)
						relativeSlopeDiffuseEnergy = 100;

					if (relativeSlopeDirectEnergy > 100)
						relativeSlopeDirectEnergy = 100;

					cellLight.set_slopeRelativeEnergy(relativeSlopeEnergy);
					cellLight.set_slopeRelativeEnergyDirect(relativeSlopeDirectEnergy);
					cellLight.set_slopeRelativeEnergyDiffuse(relativeSlopeDiffuseEnergy);

					double relativeHorizontalEnergy = 100 * cellHorizontalEnergy
							/ (beamSet.getHorizontalDiffuse() + beamSet.getHorizontalDirect());

					double relativeHorizontalDiffuseEnergy = 100 * cellHorizontalEnergyDiffuse
							/ beamSet.getHorizontalDiffuse();

					double relativeHorizontalDirectEnergy = 100 * cellHorizontalEnergyDirect
							/ beamSet.getHorizontalDirect();

					if (relativeHorizontalEnergy > 100) {
						SLReporter.printInStandardOutput("SLLighterOriginal.run (): relative horizontal energy was over 100% : "
								+ relativeHorizontalEnergy);
						relativeHorizontalEnergy = 100;
					}

					if (relativeHorizontalDiffuseEnergy > 100)
						relativeHorizontalDiffuseEnergy = 100;

					if (relativeHorizontalDirectEnergy > 100)
						relativeHorizontalDirectEnergy = 100;

					cellLight.set_horizontalRelativeEnergy(relativeHorizontalEnergy);
					cellLight.set_horizontalRelativeEnergyDirect(relativeHorizontalDirectEnergy);
					cellLight.set_horizontalRelativeEnergyDiffuse(relativeHorizontalDiffuseEnergy);

				}

			}

			SLReporter.printInStandardOutput("SLLighterOriginal : maximum number of intercepted item " + maxSize);

			// ----------------------------------------------
			// For each vitual sensors to enlight (GL Feb 2013, May 2013)
			List<SLSensor> sensors = ((SLLightableScene) stand).getSensors();

			SLReporter.printInLog("SamsaraLight", "--------------------------------------");
			SLReporter.printInLog("SamsaraLight", "SLLighterOriginal : Sensor computation");
			SLReporter.printInLog("SamsaraLight", "--------------------------------------");

			if (sensors != null) {
				for (SLSensor sensor : sensors) {

					if (slSettings.isWriteStatusDispatcher ()) { // gl 5-6-2013
						// User wait message.
						String msg = "";
						msg += Translator.swap("SLLighterOriginal.step") + " " + stand.getCaption() // step
																								// is
																								// being
								+ " " + "compute light for the virtual sensor ID =" + " " + sensor.getId();
						StatusDispatcher.print(msg);
					}

					// Initialization
					sensor.getLightResult().resetEnergy();
					double sensorTotalHorizontalEnergy = 0;
					double sensorDiffuseHorizontalEnergy = 0;
					double sensorDirectHorizontalEnergy = 0;
					double sensorTotalSlopeEnergy = 0;
					double sensorDiffuseSlopeEnergy = 0;
					double sensorDirectSlopeEnergy = 0;

					int interceptedItemCount = 0;

					// Find the cells corresponding to the sensors
					if (sensor.getMatchingCell() == null) {
						SquareCell squareCell = plot.getCell(sensor.getX(), sensor.getY());
						if (squareCell == null) {
							Log.println(Log.ERROR, "SLLighterOriginal.run ()",
									"One sensor does not belong to any cell, you must correct its coordinates");
						}

						// Needed to compute z coordinate of the sensor
						sensor.setMatchingCell(squareCell);

						if (squareCell.getZCenter() > sensor.getZ())
							SLReporter.printInLog(Log.WARNING, "SLLighterOriginal.run ()", "The sensor " + sensor.getId()
									+ " is below the grid !");
					}

					// For each beam of the sky...
					for (SLBeam beam : beamSet.getBeams()) {

						beam.resetEnergy();

						// true below means only neighbour cells with trees
						Collection<Neighbour> neighbours = beam.getNeighbours(sensor.getMatchingCell(), true);

						// GL, TODO, could be optimized for the sensors (e.g.
						// HMAX
						// replaced by HMAX - HSensor)

						// List of all item intercepted by the beam
						Vector interceptedItems = new Vector();

						for (Neighbour neighbour : neighbours) {
							SquareCell cCell = (SquareCell) neighbour.cell;

							// The shift manages the torus approaches to work on
							// virtual infinite plots
							ShiftItem shift = neighbour.shift;

							if (!cCell.isEmpty()) {

								// For each tree in the concurrent cells.
								for (Iterator t = cCell.getTrees().iterator(); t.hasNext();) {
									Tree tree = (Tree) t.next();

									// Computes interception characteristics for
									// a target virtual sensor / a beam/ a tree
									// (average distance from target cell /
									// pathLength / competitor tree)
									Set<SLInterceptionItem> items = slModel.intercept(sensor.getX(), sensor.getY(),
											sensor.getZ(), beam, tree, shift);

									if (items != null) {
										interceptedItems.addAll(items);
									}

								}
							}
						}

						// Counter
						interceptedItemCount += interceptedItems.size();

						// fc-tested: use directly tabI instead of
						// interceptedItems : NOT faster
						// and tab dimension pb.
						SLInterceptionItem[] tabI = new SLInterceptionItem[interceptedItems.size()];
						interceptedItems.copyInto(tabI);

						// Interception items are sorted from farthest to
						// nearest tree from the target cell.

						// It is necessary to compute progressive energy loss of
						// the beam from canopy top to canopy bottom.
						Arrays.sort(tabI);

						// light computations
						double hAngle = beam.getHeightAngle_rad();
						double azimut = beam.getAzimut_rad();

						// Beam energy in MJ/m2 on a plane orthogonal to beam
						// direction
						double beamEnergy = beam.getInitialEnergy();

						double scalarSlope = Math.cos(slope_rad) * Math.sin(hAngle) + Math.sin(slope_rad)
								* Math.cos(hAngle) * Math.cos(azimut - bottomAzimut);
						double scalarHorizontal = Math.cos(0) * Math.sin(hAngle) + Math.sin(0) * Math.cos(hAngle)
								* Math.cos(azimut - bottomAzimut);
						double initEnergySlope = scalarSlope * beamEnergy;
						double initEnergyHorizontal = scalarHorizontal * beamEnergy;

						// Beam energy in MJ/m2 on a horizontal plane
						double currentHorizontalEnergy = initEnergyHorizontal;

						// Beam energy in MJ/M2 along the slope
						double currentSlopeEnergy = initEnergySlope;

						// Remember intercepted tree if the Porous envelop algo
						// was used
						Set<Tree> treeMemory = new HashSet<Tree>();

						// For each competing tree.
						for (int i = 0; i < tabI.length; i++) {
							SLInterceptionItem item = tabI[i];

							Tree t = item.getTree();
							SLLightableTree lightableTree = (SLLightableTree) t;
							SLTreeLightResult treeLight = lightableTree.getLightResult();
							SLTreePart treePart = item.getTreePart();

							// trunk
							if (treePart instanceof SLTrunk) {
								currentHorizontalEnergy = 0;
								currentSlopeEnergy = 0;

								// Crown part
							} else {
								// turbid medium
								if (slSettings.isTurbidMedium ()) {

									// fc+fa-26.4.2017 introduced ladProportion
									double ladProp = 1;
//									double greenProp = 1; // fa-19.06.2017

									// In tagMode, some beams are built for
									// specific key doys
									int doy = beam.getDoy();
									if (doy > 0) {
										if (foliageStateManager == null)
											throw new Exception(
													"Error in SLLighterOriginal.run (), finding a beam with a doy > 0, requires"
															+ " a foliageState manager");
										// Calculates ladProp at tree level. nb-06.11.2019
										//ladProp = foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(), doy);
										ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
//										greenProp = foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(), doy); // fa-19.06.2017
									}

//									double leafAreaDensity = ((SLCrownPart) treePart).getLeafAreaDensity() * ladProp * greenProp; // fa-19.06.2017
									double leafAreaDensity = ((SLCrownPart) treePart).getLeafAreaDensity() * ladProp; // fa-09.08.2017

									double extinctionCoef = ((SLCrownPart) treePart).getExtinctionCoefficient();
									double clumpingFactor = 1.0;
									double interceptedFraction = (1 - Math.exp(-extinctionCoef * clumpingFactor
											* leafAreaDensity * item.getPathLength()));
									double interceptedEHorizontal = currentHorizontalEnergy * interceptedFraction;
									double interceptedESlope = currentSlopeEnergy * interceptedFraction;

									currentHorizontalEnergy -= interceptedEHorizontal;
									currentSlopeEnergy -= interceptedESlope;

									// Porous envelop
								} else {

									// fa-10.07.2017 introduced ladProportion
									double ladProp = 1;
//									double greenProp = 1; // fa-19.06.2017

									// In tagMode, some beams are built for
									// specific key doys
									int doy = beam.getDoy();
									if (doy > 0) {
										if (foliageStateManager == null)
											throw new Exception(
													"Error in SLLighterOriginal.run (), finding a beam with a doy > 0, requires"
															+ " a foliageState manager");
										// Calculates ladProp at tree level. nb-06.11.2019
										//ladProp = foliageStateManager.getLadProportionOfNextYear(lightableTree.getSpeciesCode(), doy);
										ladProp = foliageStateManager.getLadProportionOfNextYear(t.getId(), doy);
//										greenProp = foliageStateManager.getGreenProportion(lightableTree.getSpeciesCode(), doy); // fa-19.06.2017
									}

									// Since the crown openess concern the
									// entire crown and not a crownpart, we need
									// to attenuate the energy only once.
									if (!treeMemory.contains(t)) {

//										double transmissivity = lightableTree.getCrownTransmissivity() * ladProp * greenProp; // fa-10.07.2017;
										double transmissivity = lightableTree.getCrownTransmissivity() * ladProp; // fa-09.08.2017;

										double interceptedEHorizontal = (1 - transmissivity) * currentHorizontalEnergy;
										double interceptedESlope = (1 - transmissivity) * currentSlopeEnergy;

										currentHorizontalEnergy -= interceptedEHorizontal;
										currentSlopeEnergy -= interceptedESlope;

										treeMemory.add(t);
									}
								}
							}
						}

						// cumulate the energy for every beam
						sensorTotalHorizontalEnergy += currentHorizontalEnergy; // MJ/M2
						sensorTotalSlopeEnergy += currentSlopeEnergy; // MJ/M2

						if (beam.isDirect()) {
							sensorDirectHorizontalEnergy += currentHorizontalEnergy; // MJ/M2
							sensorDirectSlopeEnergy += currentSlopeEnergy; // MJ/M2
						} else {
							sensorDiffuseHorizontalEnergy += currentHorizontalEnergy; // MJ/M2
							sensorDiffuseSlopeEnergy += currentSlopeEnergy; // MJ/M2
						}

						// fa-12.07.2017
						SLTag energyTag = beam.isDirect() ? SLTag.DIRECT : SLTag.DIFFUSE;
//						sensor.getLightResult().addTagResult(new SLTagTargetResult(beam.getTag(), energyTag, currentSlopeEnergy));
						sensor.getLightResult().addTagResult(new SLTagTargetResult(beam.getTag(), energyTag, SLTag.SLOPE, currentSlopeEnergy)); // fa-08.01.2018
						sensor.getLightResult().addTagResult(new SLTagTargetResult(beam.getTag(), energyTag, SLTag.HORIZONTAL, currentHorizontalEnergy)); // fa-08.01.2018

					} // end of the loop going through every beam

					// sensor energy
					sensor.getLightResult().set_aboveCanopyHorizontalEnergy(
							beamSet.getHorizontalDiffuse() + beamSet.getHorizontalDirect());

					sensor.getLightResult().set_horizontalEnergy(sensorTotalHorizontalEnergy);
					sensor.getLightResult().set_horizontalEnergyDiffuse(sensorDiffuseHorizontalEnergy);
					sensor.getLightResult().set_horizontalEnergyDirect(sensorDirectHorizontalEnergy);

					sensor.getLightResult().set_slopeEnergy(sensorTotalSlopeEnergy);
					sensor.getLightResult().set_slopeEnergyDiffuse(sensorDiffuseSlopeEnergy);
					sensor.getLightResult().set_slopeEnergyDirect(sensorDirectSlopeEnergy);

					sensor.getLightResult().set_horizontalRelativeEnergy(
							sensorTotalHorizontalEnergy
									/ (beamSet.getHorizontalDiffuse() + beamSet.getHorizontalDirect()) * 100);
					sensor.getLightResult().set_horizontalRelativeEnergyDiffuse(
							sensorDiffuseHorizontalEnergy / beamSet.getHorizontalDiffuse() * 100);
					sensor.getLightResult().set_horizontalRelativeEnergyDirect(
							sensorDirectHorizontalEnergy / beamSet.getHorizontalDirect() * 100);

					SLReporter.printInLog("SamsaraLight", "SLLighterOriginal.run () - sensor light - ID = " + sensor.getId()
							+ " height = " + sensor.getHeight() + " PACL = "
							+ sensor.getLightResult().get_horizontalRelativeEnergy() + " intercepted items nb "
							+ interceptedItemCount + " (x " + sensor.getX() + " y " + sensor.getY() + ")");
				}
			}
			SLReporter.printInLog("SamsaraLight", "--------------------------------------");

		} catch (Exception e) {
			// fc-23.6.2017 added this message in the log before rethrowing the
			// exception to caller
			Log.println(Log.ERROR, "SLLighterOriginal.run ()", "SamsaraLight model interruption", e);
			throw e;
		}

	}

}
