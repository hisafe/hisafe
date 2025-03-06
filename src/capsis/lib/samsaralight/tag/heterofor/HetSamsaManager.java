/*
 * The HETEROFOR model.
 *
 * Copyright (C) 2012-2019: M. Jonard (UCL ELIe Forest Science).
 *
 * This file is part of the HETEROFOR model and is free software:  you can redistribute it and/or
 * modifiy it under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, version 2.1 of the License, or (at your option) any later version.
 */

package capsis.lib.samsaralight.tag.heterofor;

import java.awt.Color;
import java.io.Serializable;
import java.util.ArrayList;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.lib.samsaralight.SLFoliageStateManager;
import capsis.lib.samsaralight.SLLightableTarget;
import capsis.lib.samsaralight.SLLightableTree;
import capsis.lib.samsaralight.SLModel;
import capsis.lib.samsaralight.SLSettings;
import capsis.lib.samsaralight.SLTreeLightResult;
import jeeb.lib.util.Alert;
import jeeb.lib.util.RGB;
import jeeb.lib.util.RGBManager;
import jeeb.lib.util.RGBManagerClasses;
import jeeb.lib.util.Translator;

/**
 * A manager for the SamsaraLight radiative balance process in Heterofor. Two
 * different modes are available: Legacy mode for standard processing and Tag
 * mode for more fine resolution, with additional tagged beams around particular
 * dates, possibly related to phenology (bud burst date...).
 *
 * @author F. de Coligny - June 2017
 */
public abstract class HetSamsaManager implements Serializable {

	// Radiation colors
	public static final Color LIGHT1 = new Color(0, 0, 0); // Color.BLACK;
	public static final Color LIGHT2 = new Color(100, 100, 100);
	public static final Color LIGHT3 = new Color(150, 150, 150);
	public static final Color LIGHT4 = new Color(200, 200, 200);
	public static final Color LIGHT5 = new Color(255, 255, 255); // Color.WHITE;

	// Main variables
//	private HetSamsaFileLoader samsaFileLoader; // fc+cr-11.1.2022

	protected SLModel slModel;

	protected SLSettings slSettings;

//	protected int radiationCalculationTimeStep;

	// Subclasses may create and init the manager with various constructors,
	// depending on combinations of legacy / tag mode and phenology options

	/**
	 * Default constructor
	 */
	public HetSamsaManager(/* HetSamsaFileLoader samsaFileLoader, int radiationCalculationTimeStep, */ SLModel slModel)
			throws Exception {

//		this.samsaFileLoader = samsaFileLoader; // fc+cr-11.1.2022
//		this.radiationCalculationTimeStep = radiationCalculationTimeStep;

		this.slModel = slModel;

		// nb-20.12.2017. Caused false calculations with use of two successive
		// Samsaralight models (classical and then linear).
		// slSettings = samsaFileLoader.slSettings;
		slSettings = slModel.getSettings();

		// Done in samsaFileLoader (and things may have been changed since
		// loading time)
		// slModel.init1_loadSettingsFile();

//		System.out.println("");
//		System.out.println("Settings de slModel:");
//		System.out.println("");
//		System.out.println("parallelMode: " + slSettings.isParallelMode());
//		System.out.println("tagMode: " + slSettings.isTagMode());
//		System.out.println("fileName: " + slSettings.getFileName());
//		System.out.println("turbidMedium: " + slSettings.isTurbidMedium());
//		System.out.println("trunkInterception: " + slSettings.isTrunkInterception());
//		System.out.println("directAngleStep: " + slSettings.getDirectAngleStep());
//		System.out.println("heightAngleMin: " + slSettings.getHeightAngleMin());
//		System.out.println("diffuseAngleStep: " + slSettings.getDiffuseAngleStep());
//		System.out.println("soc: " + slSettings.isSoc());
//		System.out.println("leafOnDoy: " + slSettings.getLeafOnDoy());
//		System.out.println("leafOffDoy: " + slSettings.getLeafOffDoy());
//		if (slSettings.getMontlyRecords() != null) {
//			System.out.println("monthlyRecords: " + slSettings.getMontlyRecords().size());
//		} else {
//			System.out.println("monthly records null");
//		}
//		if (slSettings.getHourlyRecords() != null) {
//			System.out.println("hourlyRecords: " + slSettings.getHourlyRecords().size());
//		} else {
//			System.out.println("hourly records null");
//		}
//		System.out.println("writeStatusDispatcher: " + slSettings.isWriteStatusDispatcher());
//		System.out.println("GMT: " + slSettings.getGMT());
//		System.out.println("plotLatitude_deg: " + slSettings.getPlotLatitude_deg());
//		System.out.println("plotLongitude_deg: " + slSettings.getPlotLongitude_deg());
//		System.out.println("plotSlope_deg: " + slSettings.getPlotSlope_deg());
//		System.out.println("plotAspect_deg: " + slSettings.getPlotAspect_deg());
//		System.out.println("northToXAngle_cw_deg: " + slSettings.getNorthToXAngle_cw_deg());
//		System.out.println("sensorLightOnly: " + slSettings.isSensorLightOnly());
	}

	/**
	 * Run the lighting process on the scene, update the cells colors accordingly,
	 * send an exception if some trees got no light at all. foliageStateManager is
	 * optional, may be null. previousScene is used in case a tree gets no light at
	 * all, to copy the previous light values. First time, previousScene is null.
	 */
	public void processLighting(TreeList scene, SLFoliageStateManager foliageStateManager, TreeList previousScene)
			throws Exception {

		// // A HetSamsaManager instance can not run processLighting ()
		// boolean runIsPossible = this instanceof HetSamsaLegacyMode || this
		// instanceof HetSamsaTagMode;
		// if (!runIsPossible)
		// return;

		slModel.processLighting(scene, foliageStateManager);

		if (!slSettings.isSensorLightOnly()) { // fa-27.11.2015: all cells and all trees may not be illuminated when
												// only sensors are considered for radiative balance

			updateCellsColors(scene);

			// fc+cr-12.1.2022
			managementOfTreesHavingReceivedNoEnergy(scene, previousScene);

		}

	}

	/**
	 * Optional process for trees having received no energy.
	 * Trees with no energy are either removed or their energy is copied from
	 * previous scene. Moved this code in a method to be able to override it for
	 * PDGLight (no particular management for these trees)
	 */
	protected void managementOfTreesHavingReceivedNoEnergy(TreeList scene, TreeList previousScene) throws Exception {

		// Check if all the trees got some light,
		// if not, find the tree in previousScene and copy its energy values
		// if trouble (prevScene == null, tree not found...) throw an exception
		// to stop the simulation // fc-7.12.2016 // fc+mj-9.3.2017
//		for (Object o : scene.getTrees()) {
		for (Object o : new ArrayList<>(scene.getTrees())) { // fa-13.09.2021

			Tree tree = (Tree) o; // fc+cr-11.1.2022 tree and t are the same object
			SLLightableTree t = (SLLightableTree) o;

			SLTreeLightResult tl = t.getLightResult();

			// fc+fa-4.8.2017 This copy will not work well in tag mode
			// to be reviewed
			if (tl.getCrownEnergy() <= 0 && tl.getControlTagBasedTreeEnergy() <= 0) { // trouble detection
				try {
					// try to fix trouble
//					SLLightableTree prevTree = (SLLightableTree) previousScene.getTree(t.getId()); // fa-13.09.2021: moved below to avoid exception

					if (previousScene == null || previousScene.getTree(tree.getId()) == null) { // fa-13.09.2021: if
																								// tree was not
																								// found on previous
																								// scene => tree is
																								// considered to die
																								// due to lack of
																								// energy
//					if (previousScene == null)  { // fa-13.09.2021: if tree was not found on previous scene => tree is considered to die due to lack of energy
						scene.removeTree(tree);
						scene.storeStatus(tree, "dead");
						Alert.storeMessage("No energy found for tree " + tree.getId() + " on year " + scene.getDate()
								+ ", was set as dead");
					} else {
						SLLightableTree prevTree = (SLLightableTree) previousScene.getTree(tree.getId()); // fa-13.09.2021:
																											// moved
																											// her
						// from above
						SLTreeLightResult.copyEnergyValues(prevTree, t); // fc+mj-9.3.2017

						HetReporter.printInStandardOutput(
								"HetSamsaManager copied energy values from year " + previousScene.getDate()
										+ " to year " + scene.getDate() + " for tree " + tree.getId() + "...");
					}

				} catch (Exception e) {
					// could not fix trouble
					throw new Exception("HetSamsaManager, simulation stopped at year " + scene.getDate()
							+ ": found no energy intercepted for tree " + tree.getId()
							+ ", could not find a way to fix this, cell size is maybe too large.", e);
				}
			}
		}

	}

	/**
	 * Changes cells colors depending on the received light.
	 */
	private void updateCellsColors(TreeList initScene) {

		// fc+cr-11.1.2022 The scene is a TreeList, contains trees instanceof Tree and
		// SLLightableTree, the plot is a PlotOfCells, the cells are instanceof
		// SLLightableTarget and RGB

		// A color manager for the light on Samsara ground cells
		RGBManagerClasses<SLLightableTarget> lightRGBManager = new RGBManagerClasses<SLLightableTarget>("Light") {

			// Add the classes with their associated color and caption with
			// addClass ().
			@Override
			public void init() {
				addClass(RGBManager.toRGB(LIGHT1), Translator.swap("SVSamsara.light1"), Double.MIN_VALUE, 6.25);
				addClass(RGBManager.toRGB(LIGHT2), Translator.swap("SVSamsara.light2"), 6.25, 12.5);
				addClass(RGBManager.toRGB(LIGHT3), Translator.swap("SVSamsara.light3"), 12.5, 25);
				addClass(RGBManager.toRGB(LIGHT4), Translator.swap("SVSamsara.light4"), 25, 50);
				addClass(RGBManager.toRGB(LIGHT5), Translator.swap("SVSamsara.light5"), 50, Double.MAX_VALUE);
			}

			// Returns the amount of light on the given cell.
			@Override
			public double getValue(SLLightableTarget e) {
				return e.getLightResult().get_horizontalRelativeEnergy();
			}

		};

		// fc+cr-11.1.2022 Replaced HetScene by TreeList before moving this class to
		// samsaralight
		if (initScene.getPlot() instanceof PlotOfCells) {
			PlotOfCells plot = (PlotOfCells) initScene.getPlot();

			for (Object o : plot.getCells()) {

				// fc+cr-11.1.2022 lightableCell and cell are the same object
				SLLightableTarget lightableCell = (SLLightableTarget) o;
				RGB cell = (RGB) o;

				cell.setRGB(lightRGBManager.getRGB(lightableCell));
			}

		}

	}

	/**
	 * Returns true if manager implements SamsaraLight legacy mode.
	 */
	public boolean isLegacyMode() {
		return !slSettings.isTagMode();
	}

	public boolean isTagMode() {
		return slSettings.isTagMode();
	}

	// fc+cr-11.1.2022 Unused, removed before moving this class to samsaralight
//	public HetSamsaFileLoader getSamsaFileLoader() {
//		return samsaFileLoader;
//	}

	public SLModel getSLModel() {
		return slModel;
	}

}
