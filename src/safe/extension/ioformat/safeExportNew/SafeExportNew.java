/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE 
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

package safe.extension.ioformat.safeExportNew;


import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.TreeSet;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import safe.model.*;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.StandRecordSet;

/**
 * SafeExport execute Safe exportation
 *
 * @author Isabelle Lecomte - September 2020
 */
public class SafeExportNew extends StandRecordSet implements OFormat {
	private static final long serialVersionUID = 1L;
	// List of export objects
	public static String OBJECT_TREE = "SafeTree";
	public static String OBJECT_CELL = "SafeCell";
	public static String OBJECT_VOXEL = "SafeVoxel";
	public static String OBJECT_ZONE = "SafeCropZone";
	public static String OBJECT_PLOT = "SafePlot";
	public static String OBJECT_LAYER = "SafeLayer";
	public static String OBJECT_CLIMATE = "SafeMacroClimat";

	private SafeMacroClimat macroClimate;

	static {
		Translator.addBundle("safe.extension.ioformat.safeExportNew.SafeExportNew");
	}

	/**
	 * Phantom constructor. Only to ask for extension properties (authorName,
	 * version...).
	 */
	public SafeExportNew() throws Exception {

//		// fc-9.2.2021 debugging extra blank lines...
//		System.out.println("SafeExportNew -> new SafeExportNew ()...");

		// fc-9.2.2021 Solution.
		// This export is used in a unusual way. Generally, an export is instanciated,
		// then executed once, it writes its file and then it is forgotten. Here, this
		// constructor is called more often (each year ?), and the new instance is used
		// to write in the same file than previously, in append mode. So each year,
		// (e.g.) for the trees, the file is written. And in RecordSet.save (), there is
		// a code to write the source of the data in the file if headerEnabled. And here
		// the source is empty. That's why an empty line appeared 'between each tree' in
		// the test output file: the test was running with a single tree, so the line
		// appeared in fact 'between each year', like in the cells file.
		// Setting header enabled to false will remove this blank line in all files
		// (trees, cells...) between each year.
		setHeaderEnabled(false);

	}

	/**
	 * RecordSet - Stand Implementation here.
	 */
	public GScene load(GModel model) throws Exception { // no import here
		return null;
	}

	public void initExport(GModel m, Step s, SafeExportProfile p) throws Exception {
		SafeStand stand = (SafeStand) s.getScene();
		SafeModel model = (SafeModel) m;
		writeHeaders(model, stand, p);
	}

	public void export(GModel m, Step s, SafeExportProfile p) throws Exception {
		SafeStand stand = (SafeStand) s.getScene();
		prepareExport(stand, p);
	}


	/**
	 * Generate the entire export file
	 */
	public void prepareExport(GScene sc, SafeExportProfile p) throws Exception {

		// fc-12.10.2020 createRecordSet (GScene) was renamed prepareExport (GScene) for
		// clarity

		SafeStand stand = (SafeStand) sc;
		Step s = (Step) stand.getStep();
		PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017

		try {

			// TREE EXPORT
			if (p.getObject().equals(OBJECT_TREE)) {

				SafeTree tree = null;

				TreeSet treeRecords = new TreeSet(new SafeExportRecordComparator());

				Collection trees = stand.getTrees();
				// for all selected treeIds...
				for (Iterator itTree = trees.iterator(); itTree.hasNext();) {
					tree = (SafeTree) itTree.next();
					if (p.getIdsToExport().size() > 0) {
						if (p.getIdsToExport().contains(tree.getId())) {
							createStepTreeRecords(treeRecords, s, p, tree);
						}
					} else
						createStepTreeRecords(treeRecords, s, p, tree);

				}

				writeRecords(treeRecords);
			// PLOT (ZONES)  EXPORT
			} else if (p.getObject().equals(OBJECT_ZONE)) {
			
				
				TreeSet zoneRecords = new TreeSet(new SafeExportRecordComparator());

				// for all selected cellIds...
				Collection zones = ((SafePlot)plotc).getCropZones();
				for (Iterator itCell = zones.iterator(); itCell.hasNext();) {
					SafeCropZone zone = (SafeCropZone) itCell.next();
					if (p.getIdsToExport().size() > 0) {
						if (p.getIdsToExport().contains(zone.getId())) {
							createStepZoneRecords(zoneRecords, s, p, zone);
						}
					} else
						createStepZoneRecords(zoneRecords, s, p, zone);
				}

				writeRecords(zoneRecords);
				
				
				
			// CELL EXPORT
			} else if (p.getObject().equals(OBJECT_CELL)) {

				TreeSet cellRecords = new TreeSet(new SafeExportRecordComparator());

				// for all selected cellIds...
				Collection cells = plotc.getCells();
				for (Iterator itCell = cells.iterator(); itCell.hasNext();) {
					SafeCell cell = (SafeCell) itCell.next();
					if (p.getIdsToExport().size() > 0) {
						if (p.getIdsToExport().contains(cell.getId())) {
							createStepCellRecords(cellRecords, s, p, cell);
						}
					} else
						createStepCellRecords(cellRecords, s, p, cell);
				}

				writeRecords(cellRecords);

				// VOXEL EXPORT
			} else if (p.getObject().equals(OBJECT_VOXEL)) {

				TreeSet voxelRecords = new TreeSet(new SafeExportRecordComparator());

				// for all selected cellIds...
				Collection cells = plotc.getCells();
				for (Iterator itCell = cells.iterator(); itCell.hasNext();) {
					SafeCell cell = (SafeCell) itCell.next();
					Collection voxels = java.util.Arrays.asList(cell.getVoxels());
					if (p.getIdsToExport().size() > 0) {
						if (p.getIdsToExport().contains(cell.getId())) {

							if (p.getVoxelIdsToExport().size() > 0) {
								for (Iterator itv = voxels.iterator(); itv.hasNext();) {
									SafeVoxel voxel = (SafeVoxel) itv.next();
									if (p.getVoxelIdsToExport().contains(voxel.getId())) {
										createStepVoxelRecords(voxelRecords, s, p, voxel);
									}
								}
							} else {
								for (Iterator itv = voxels.iterator(); itv.hasNext();) {
									SafeVoxel voxel = (SafeVoxel) itv.next();
									createStepVoxelRecords(voxelRecords, s, p, voxel);
								}
							}
						}
					} else {
						if (p.getVoxelIdsToExport().size() > 0) {
							for (Iterator itv = voxels.iterator(); itv.hasNext();) {
								SafeVoxel voxel = (SafeVoxel) itv.next();
								if (p.getVoxelIdsToExport().contains(voxel.getId())) {
									createStepVoxelRecords(voxelRecords, s, p, voxel);
								}
							}
						} else {
							for (Iterator itv = voxels.iterator(); itv.hasNext();) {
								SafeVoxel voxel = (SafeVoxel) itv.next();
								createStepVoxelRecords(voxelRecords, s, p, voxel);
							}
						}
					}

				}

				writeRecords(voxelRecords);

			// PLOT EXPORT
			} else if (p.getObject().equals(OBJECT_PLOT)) {

				TreeSet plotRecords = new TreeSet(new SafeExportRecordComparator());

				stand = (SafeStand) s.getScene();
				createStepPlotRecords(plotRecords, s, p, stand);

				writeRecords(plotRecords);

				// CLIMATE EXPORT
				// LAYER EXPORT
			} else if (p.getObject().equals(OBJECT_LAYER)) {

				TreeSet layerRecords = new TreeSet(new SafeExportRecordComparator());

				// for all selected cellIds...
				/*
				 * Collection layers = soil.getLayers(); for (Iterator
				 * itLayer=layers.iterator(); itLayer.hasNext();) { SafeLayer layer =
				 * (SafeLayer) itLayer.next();
				 * 
				 * }
				 */

				writeRecords(layerRecords);

			// CLIMATE EXPORT
			} else if (p.getObject().equals(OBJECT_CLIMATE)) {

				TreeSet climateRecords = new TreeSet(new SafeExportRecordComparator());

				macroClimate = ((SafeModel) (s.getProject().getModel())).getMacroClimat();

				// get year and day from step caption
				int y = ((SafeStand) s.getScene()).getWeatherYear();
				int j = ((SafeStand) s.getScene()).getJulianDay();

				try {
					SafeDailyClimat climat = macroClimate.getDailyWeather(y, j);

					if (climat != null)
						createStepClimateRecords(climateRecords, s, p, climat);
				} catch (Exception exc) {
				}

				writeRecords(climateRecords);

			}

		} catch (Exception e) {
			System.out.println("Error : " + e);
			String err = Translator.swap("SafeExport.thisErrorWasProducedDuringExportation") + "\n" + e + "\n";

		}

	}

	/**
	 * Write header for each type of object
	 */
	public void writeHeaders(SafeModel model, SafeStand stand, SafeExportProfile p) {
		
		if (p.getObject().equals(OBJECT_TREE)) {
			SafeTree firstTree = (SafeTree) stand.getTree(1);
			writeHeader(p, firstTree);
		}
		if (p.getObject().equals(OBJECT_PLOT)) {
			writeHeader(p, stand);
		}
		if (p.getObject().equals(OBJECT_ZONE)) {
			SafePlot plotc = (SafePlot) stand.getPlot(); 
			SafeCropZone firstZone = (SafeCropZone) plotc.getCropZone(0);
			writeHeader(p, firstZone);
		}
		if (p.getObject().equals(OBJECT_CELL)) {
			PlotOfCells plotc = (PlotOfCells) stand.getPlot(); 
			SafeCell firstCell = (SafeCell) plotc.getCell(1);
			writeHeader(p, firstCell);
		}
		if (p.getObject().equals(OBJECT_VOXEL)) {
			PlotOfCells plotc = (PlotOfCells) stand.getPlot(); 
			SafeCell firstCell = (SafeCell) plotc.getCell(1);
			SafeVoxel firstVoxel = firstCell.getFirstVoxel();
			writeHeader(p, firstVoxel);
		}

		if (p.getObject().equals(OBJECT_LAYER)) {
			SafeLayer firstLayer = new SafeLayer(0);
			writeHeader(p, firstLayer);
		}
		if (p.getObject().equals(OBJECT_CLIMATE)) {
			SafeDailyClimat climat = new SafeDailyClimat();
			writeHeader(p, climat);
		}
	}

	private void writeHeader(SafeExportProfile p, Object object) {
		add(new FreeRecord(createHeadLine(p, object)));
	}

	/**
	 * create recordsets for tree objects
	 */
	private void createStepTreeRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = 0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");


			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);

			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);

				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];

					val = new StringBuffer("");

					readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Type", val);

					if (readed) {
						String type = (val.toString());

						// fill the array values with readed variables
						if (type.equals("Double"))
							readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

						if (type.equals("Float"))
							readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);

						if (type.equals("Int"))
							readed = SafeExportTools.readArrayOfIntVariables(object, variableName, values);

						if (readed) {
							val = new StringBuffer("");
							// put the values in a line of fields
							for (int j = 0; j < values.length; j++) {
								val.append(values[j]);
								if (j != values.length - 1) {
									val.append("\t");
								}
							}
						}
					}
				}
			}
			
			if (!readed) {
				// try to read *in SafeCrop* the value using the "get"+nomVariable" method
				val = new StringBuffer("");
				SafeTreeSpecies species = ((SafeTree) object).getTreeSpecies();
				if (species != null) {
					readed = SafeExportTools.readPrimitiveVariable(species, variableName, val);

				}

			}
			if (!readed) {
				// try to read *in SafeRoot* the value using the "get"+nomVariable" method
				val = new StringBuffer("");
				SafePlantRoot safeRoot = null;
				safeRoot = ((SafeTree) object).getPlantRoots();
				readed = SafeExportTools.readPrimitiveVariable(safeRoot, variableName, val);
			}

			if (readed)
				write.append(val.toString());
			if (!readed)
				write.append("error!");

			
			// the readed value contains the treeId (variable added during init subject
			// tree)
			if (variableName.equals("idTree"))
				objectId = Integer.parseInt(val.toString());

			write.append('\t');
		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), objectId, write.toString());
		dest.add(record);

	}
	/**
	 * create recordsets for zone objects
	 */
	private void createStepZoneRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = 0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");
			readed = false;
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);


			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);

				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];

					val = new StringBuffer("");

					readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Type", val);

					if (readed) {
						String type = (val.toString());

						// fill the array values with readed variables
						if (type.equals("Double"))
							readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

						if (type.equals("Float"))
							readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);

						if (type.equals("Int"))
							readed = SafeExportTools.readArrayOfIntVariables(object, variableName, values);

						if (readed) {
							val = new StringBuffer("");
							// put the values in a line of fields
							for (int j = 0; j < values.length; j++) {
								val.append(values[j]);
								if (j != values.length - 1) {
									val.append("\t");
								}
							}
						}
					}
				}
			}


			if (readed)
				write.append(val.toString());
			else
				write.append("error!");

			if (variableName.equals("idZone"))
				objectId = Integer.parseInt(val.toString());

			write.append('\t');

		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), objectId, write.toString());
		dest.add(record);
	}
	/**
	 * create recordsets for cell objects
	 */
	private void createStepCellRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = 0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");
			readed = false;
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);


			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);

				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];

					val = new StringBuffer("");

					readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Type", val);

					if (readed) {
						String type = (val.toString());

						// fill the array values with readed variables
						if (type.equals("Double"))
							readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

						if (type.equals("Float"))
							readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);

						if (type.equals("Int"))
							readed = SafeExportTools.readArrayOfIntVariables(object, variableName, values);

						if (readed) {
							val = new StringBuffer("");
							// put the values in a line of fields
							for (int j = 0; j < values.length; j++) {
								val.append(values[j]);
								if (j != values.length - 1) {
									val.append("\t");
								}
							}
						}
					}
				}
			}
			if (!readed) {
				// try to read *in SafeCrop* the value using the "get"+nomVariable" method
				val = new StringBuffer("");
				SafeCrop safeCrop = ((SafeCell) object).getCrop();
				if (safeCrop != null) {
					readed = SafeExportTools.readPrimitiveVariable(safeCrop, variableName, val);

					if (!readed) {
						// try to read *in SafeRoot* the value using the "get"+nomVariable" method
						val = new StringBuffer("");
						SafePlantRoot safeRoot = null;
						safeRoot = (SafePlantRoot) (safeCrop.getPlantRoots());
						if (safeRoot != null)
							readed = SafeExportTools.readPrimitiveVariable(safeRoot, variableName, val);
					}
				}

			}
			if (readed)
				write.append(val.toString());
			else
				write.append("error!");

			// the readed value contains the cellId (variable added during init subject
			// cell)
			if (variableName.equals("idCell"))
				objectId = Integer.parseInt(val.toString());
			if (variableName.equals("idZone"))
				objectId = Integer.parseInt(val.toString());
			write.append('\t');

		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), objectId, write.toString());
		dest.add(record);
	}

	/**
	 * create recordsets for voxel objects
	 */
	private void createStepVoxelRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = -1;
		int cellId = -1;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);

			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);

				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];

					val = new StringBuffer("");

					readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Type", val);

					if (readed) {
						String type = (val.toString());

						// fill the array values with readed variables
						if (type.equals("Double"))
							readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

						if (type.equals("Float"))
							readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);

						if (type.equals("Int"))
							readed = SafeExportTools.readArrayOfIntVariables(object, variableName, values);

						if (readed) {
							val = new StringBuffer("");
							// put the values in a line of fields
							for (int j = 0; j < values.length; j++) {
								val.append(values[j]);
								if (j != values.length - 1) {
									val.append("\t");
								}
							}
						}
					}
				}
			}

			if (!readed) {
				// try to read *in SafeCrop* the value using the "get"+nomVariable" method
				val = new StringBuffer("");
				SafeLayer safeLayer = ((SafeVoxel) object).getLayer();
				readed = SafeExportTools.readPrimitiveVariable(safeLayer, variableName, val);
			}

			// is the index of the current variable is one of those with which variables
			// have been added in the init of the voxel subject ?
			// Note only 2 of the added variables are processed here because the others are
			// processed like normal variabkles
			if (variableName.equals("idVoxel"))
				objectId = Integer.parseInt(val.toString());
			if (variableName.equals("idCell"))
				cellId = Integer.parseInt(val.toString());
			if (variableName.equals("idZone"))
				cellId = Integer.parseInt(val.toString());			
			if (readed)
				write.append(val.toString());
			if (!readed)
				write.append("error!");

			write.append('\t');
		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), cellId, objectId, write.toString());
		dest.add(record);
	}

	/**
	 * create recordsets for CLIMATE objects
	 */
	private void createStepClimateRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = 0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);
			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);
				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];
					String type = (val.toString());
					
					// fill the array values with readed variables
					if (type.equals("Double"))
						readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

					if (type.equals("Float"))
						readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);
					
					val = new StringBuffer("");
					// put the values in a line of fields
					for (int j = 0; j < values.length; j++) {
						val.append(values[j]);
						if (j != values.length - 1) {
							val.append("\t");
						}
					}
				}
			}

			if (readed)
				write.append(val.toString());
			if (!readed)
				write.append("error!");

			write.append('\t');

		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), objectId, write.toString());
		dest.add(record);
	}

	/**
	 * create recordsets for PLOT objects
	 */
	private void createStepPlotRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = 0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);
			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);
				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];
					String type = (val.toString());
					
					// fill the array values with readed variables
					if (type.equals("Double"))
						readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

					if (type.equals("Float"))
						readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);
					
					val = new StringBuffer("");
					// put the values in a line of fields
					for (int j = 0; j < values.length; j++) {
						val.append(values[j]);
						val.append("\t");
					}
				}
			}

			if (!readed) {
				// try to read *in SafePlot* the value using the "get"+nomVariable" method
				val = new StringBuffer("");
				SafePlot plot = null;
				plot = (SafePlot) ((SafeStand) object).getPlot();
				readed = SafeExportTools.readPrimitiveVariable(plot, variableName, val);
			}

			if (!readed) {
				// try to read *in SafeSoil* the value using the "get"+nomVariable" method
				val = new StringBuffer("");
				SafeSoil soil = null;
				soil = ((SafeStand) object).getPlot().getSoil();
				readed = SafeExportTools.readPrimitiveVariable(soil, variableName, val);
			}

			if (readed)
				write.append(val.toString());
			if (!readed)
				write.append("error!");

			write.append('\t');

		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), objectId, write.toString());
		dest.add(record);
	}

	/**
	 * create recordsets for LAYER objects
	 */

	private void createStepLayerRecords(TreeSet dest, Step step, SafeExportProfile p, Object object) {
		StringBuffer write = new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId = 0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();

		write.append(step.getProject().getName() + '\t' + dateStart + '\t' + step.getScene().getCaption() + '\t' + ts[0]
				+ '\t' + ts[1] + '\t' + ts[2] + '\t' + julian + '\t');

		// for all subject variables
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			val = new StringBuffer("");
			readed = false;
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object, variableName, val);
			if (!readed) {
				val = new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which
				// return array)
				readed = SafeExportTools.readPrimitiveVariable(object, variableName + "Size", val);
				if (readed) {
					arraySize = Integer.parseInt(val.toString());
					values = new String[arraySize];
					String type = (val.toString());
					
					// fill the array values with readed variables
					if (type.equals("Double"))
						readed = SafeExportTools.readArrayOfDoubleVariables(object, variableName, values);

					if (type.equals("Float"))
						readed = SafeExportTools.readArrayOfFloatVariables(object, variableName, values);
					
					val = new StringBuffer("");
					// put the values in a line of fields
					for (int j = 0; j < values.length; j++) {
						val.append(values[j]);
						val.append("\t");
					}
				}
			}

			if (readed)
				write.append(val.toString());
			else
				write.append("error!");

			// the readed value contains the cellId (variable added during init subject
			// cell)
			if (variableName.equals("idLayer"))
				objectId = Integer.parseInt(val.toString());

			write.append('\t');

		}

		// create a record with the written line and add it to the destination vector
		SafeExportRecord record = new SafeExportRecord(step.getId(), objectId, write.toString());
		dest.add(record);
	}

	/**
	 * write in file the collection of string contained in the argument records (and
	 * an empty record at the end) before writting the records : - write a comment
	 * line which contains the subject name - write a line which contains the name
	 * of all readed variables
	 */
	private void writeRecords(Collection records) {

		if (records.size() > 0) {
			for (Iterator i = records.iterator(); i.hasNext();) {
				SafeExportRecord record = (SafeExportRecord) i.next();
				String aux = record.getRecord();
				FreeRecord aux2 = new FreeRecord(aux);
				add(aux2);
			}
		}
	}

	/**
	 * return a string "head/title line" which contained all variable names
	 * (separated by a tab character) - Specific process is define for variables
	 * which returned an array instead of a "single" value - this method is only to
	 * be used with not default subject (default subjects used writeDefaultRecords)
	 */
	private String createHeadLine(SafeExportProfile p, Object o) {
		StringBuffer write = new StringBuffer("");
		boolean readed;
		int sizeVariableArray;
		StringBuffer value;
		// write a title for the step data
		write.append("SimulationName" + '\t' + "SimulationDate" + '\t' + "Date" + '\t' + "Day" + '\t' + "Month" + '\t'
				+ "Year" + '\t' + "JulianDay" + '\t');
		// for all variables...
		for (Iterator i = p.getVariables().iterator(); i.hasNext();) {
			String variableName = (String) i.next();
			readed = false;
			sizeVariableArray = 0;
			value = new StringBuffer("");
			// check if the current variable returns an array of values
			readed = SafeExportTools.readPrimitiveVariable(o, variableName + "Size", value);
			if (readed) {
				// the variable returns an array ; variable names are created using the array
				// size
				sizeVariableArray = Integer.parseInt(value.toString());
				if (sizeVariableArray == 0) {
					write.append(variableName);
				} else {
					for (int j = 0; j < sizeVariableArray; j++) {
						write.append(variableName + "_" + (j + 1));
						if (j != sizeVariableArray - 1) {
							write.append("\t");
						}
					}
				}
			} else {
				// name of the variable is written
				write.append(variableName);
			}
			write.append('\t');
		}
		return write.toString();
	}

	/**
	 * write in the file the default subject records default records are like this :
	 * variableName \t variableValue
	 */
	private void writeDefaultRecords(Collection records, String subjectName) {
		if (records.size() > 0) {
			add(new CommentRecord(subjectName));
			for (Iterator i = records.iterator(); i.hasNext();) {
				String record = (String) i.next();
				add(new FreeRecord(record));
			}
		}
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getName() {
		return "SafeExportNew";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getAuthor() {
		return "Isabelle LECOMTE";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getDescription() {
		return "Nouveau module d'export pour modï¿½le HISAFE";
	}

	/**
	 * From Extension interface.
	 */
	@Override
	public String getVersion() {
		return "1.0";
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks if
	 * the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof SafeModel)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "SafeExportNew.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

}
