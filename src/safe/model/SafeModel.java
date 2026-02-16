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

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.Date;
import java.util.GregorianCalendar;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import safe.extension.ioformat.SafeInventory;
import safe.extension.ioformat.safeExportNew.*;
import safe.stics.SafeSticsClimat;
import safe.stics.SafeSticsParameters;
import safe.stics.SafeSticsParametersFormat;
import safe.stics.SafeSticsStation;
import safe.stics.SafeSticsTransit;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.InitialParameters;
import capsis.kernel.MethodProvider;
import capsis.kernel.Project;
import capsis.kernel.Step;


/**
 * MAIN CLASS for Hi-sAFe 
 * 
 * List of main methods 
 * loadInitStand : load the pld file 
 * loadGeneralParameter : load the hisafe and stics general parameters file 
 * loadWeather : load the weather file 
 * loadExport : load export parameters
 * initializeModel : create and initialize all objects (stand soil voxels  plot cells crops trees) 
 * projectJustOpened : reopen a saved project
 * processEvolution : simulation loop from dayStart to dayEnd
 * processGrowth : daily loop growth for trees and crop 
 * export : create and file export file

 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 * 
 * Last update July 2025 (version 4.4.22087)
 */

public class SafeModel extends GModel {

	private static final long serialVersionUID = 1L;
	private boolean isReStart;			// Set to true if model is restarted from serialized file
	private boolean isDebugMode;		// Set to true for debugging
	private int yearIndex;				// Index of the year (exclude previous project if reopening) 
	private Step  lastStep = null;		// Last step used when a project is restarted from serialized file
	
	//JNA STICS objects
	private SafeTestJNA safeJNA;
	private SafeSticsParameters sticsParam;
	private SafeSticsStation sticsStation;
	private SafeSticsTransit sticsTransit;

	/** Reference to the SafeMacroClimat object : climate entries */
	private SafeMacroClimat climat;
	
	// EXPORT in BATCH mode
	private ArrayList<SafeExportProfile> exports;

	/**
	 * Constructor.
	 */
	public SafeModel() throws Exception {

		super();
				
		isReStart = false;   		// by default, this is a new project	
		isDebugMode = false;

		// creation of general parameters set
		setSettings(new SafeGeneralParameters());
		climat = new SafeMacroClimat();
	}

	/**
	 * Creates a MethodProvider for the module.
	 */
	protected MethodProvider createMethodProvider() {
		return new SafeMethodProvider();
	}

	/**
	 * Checks if the given .pld file can be loaded, returns false if trouble.
	 * @param fileName Name of the pld file 
	 */
	public boolean checkIfLoadable(String fileName) {

		try {
			new SafeInventory(fileName).load(new SafeModel(), new SafeGeneralParameters());
			return true;
		} catch (Exception e) {
			return false;
		}

	}
	
	/**
	 * Loads the inventory (stand - plot - trees) from .pld ascii file 
	 * File format is described in SafeInventory. 
	 * @param fileName Name of the pld file 
	 * @param model Reference to the SafeModel object
	 * @param settings Reference to the SafeGeneralParameters object
	 */
	public SafeStand loadInitStand(String fileName, SafeModel model, SafeGeneralParameters settings) throws Exception {
		    SafeStand initStand = null;
			    
			try {
				System.out.println(fileName);
				 initStand = (SafeStand) new SafeInventory(fileName).load(model, settings);
			} catch (Exception e) {
				System.out.println("STAND initialisation problem ... simulation is canceled !");
				String error = "STAND initialisation problem ";
				Log.println(Log.ERROR, "SafeModel.loadInitStand ()", error);
				System.exit(1);			
			}	
			return initStand;	
	}

	/**
	 * Load STICS dynamic library
	 * Note: stics.dll (or libstics.so) must be in the system PATH or LD_LIRARY_PATH
	 */
	private void loadStics() {

		try {
			safeJNA = new SafeTestJNA();

		} catch (Throwable e) {
			
			System.out.println(e.getMessage());
			System.out.println("STICS LOADING PROBLEM ... simulation is canceled !");
			String error = "STICS LOADING PROBLEM  ";
			Log.println(Log.ERROR, "SafeModel.loadStics ()", error);
			System.exit(1);
		}		
	}

	/**
	 * LOAD general parameter for HISAFE (hisafe.par) and STICS (stics.par) 
	 * @param safeSettings Reference to the SafeGeneralParameters object
	 */
	public void loadGeneralParameter (SafeGeneralParameters safeSettings) throws Exception {
		
		
		//PRIORITY : try to find files in simulation folder 
		try {
			this.loadHISAFEParameter(safeSettings.dataPath+"/generalParameters/hisafe.par");
			System.out.println("HISAFE GENERAL PARAMETERS OK");

		} catch (CancellationException e1) {

			System.out.println("HISAFE GENERAL PARAMETERS reading problem  ... simulation is canceled !");
			String error = "HISAFE GENERAL PARAMETERS reading problem  ";
			Log.println(Log.ERROR, "SafeModel.loadGeneralParameter ()", error);
			System.exit(1);
			
		} catch (Throwable e1) {	
			
			System.out.println("HISAFE GENERAL PARAMETERS reading problem  ... simulation is canceled !");
			String error = "HISAFE GENERAL PARAMETERS reading problem  ";
			Log.println(Log.ERROR, "SafeModel.loadGeneralParameter ()", error);
			System.exit(1);

		}
		//PRIORITY : try to find files in simulation folder 
		try {
	
			this.loadSTICSParameter(safeSettings.dataPath+"/generalParameters/stics.par");
			System.out.println("STICS GENERAL PARAMETERS OK");
	
		} catch (Throwable e1) {	
			
			System.out.println("STICS GENERAL PARAMETERS reading problem  ... simulation is canceled !");
			String error = "STICS GENERAL PARAMETERS reading problem  ";
			Log.println(Log.ERROR, "SafeModel.loadGeneralParameter ()", error);
			System.exit(1);
			
		}
	}	
	
	/**
	 * HISAFE general parameter file loading from ascii file 
	 * @param paramFilePath Path of the parameter file
	 */
	private void loadHISAFEParameter (String paramFilePath) throws Exception {
		new SafeGeneralParameterFormat(paramFilePath).load(getSettings(), this.sticsStation);
	}

	/**
	 * STICS general parameter file loading from ascii file 
	 * @param paramFileName Name of the parameter file
	 */
	private void loadSTICSParameter (String paramFileName) throws Exception {
		new SafeSticsParametersFormat(paramFileName).load(getSettings(), this.sticsTransit, this.sticsParam);
	}
	
	/**
	 * Weather file loading from ascii file
	 * @param weatherFileName Name of the weather file
	 * @param simulationDateStart Simulation date start
	 * @param simulationDateEnd Simulation date end
	 * @param latitude Plot latitude
	 * @param elevation Plot elevation
	 */
	public void loadWeather(String weatherFileName,  
							GregorianCalendar simulationDateStart, 
							GregorianCalendar simulationDateEnd,
							double latitude, 
							double elevation) throws Exception {

		try {

			new SafeMacroFormat(weatherFileName).load(getSettings(), climat,  latitude);
			
			//check all date are in the file 
			int simulationYearStart = simulationDateStart.get(GregorianCalendar.YEAR);
			int simulationDayStart = simulationDateStart.get(GregorianCalendar.DAY_OF_YEAR);
			
			int simulationYearEnd = simulationDateEnd.get(GregorianCalendar.YEAR);
			int simulationDayEnd = simulationDateEnd.get(GregorianCalendar.DAY_OF_YEAR);
		
			for (int year = simulationYearStart; year <= simulationYearEnd; year++) {

				int dayStart = 1;
				int dayEnd = 365;
				double waterTableMax = 0;
				int dayMax = 0;

				if (year==simulationYearStart) dayStart = simulationDayStart;
				if (year==simulationYearEnd) dayEnd = simulationDayEnd;
				if (climat.isLeapYear(year) && dayEnd==365) {
					dayEnd = 366;
				}
				//check all days are in the weather file without error
				for (int julianDay = dayStart; julianDay <= dayEnd; julianDay++) {
					SafeDailyClimat dayClimat = climat.getDailyWeather(year, julianDay);
					if (dayClimat.getWaterTableDepth()<waterTableMax) {
						waterTableMax=dayClimat.getWaterTableDepth();
						dayMax=dayClimat.getJulianDay();
					}
				}
				//update the max water table depth for this year (usefull for faidherbia tree)
				climat.addWaterTableMax(year,dayMax);
			}
				
			System.out.println("WEATHER PARAMETERS OK");
		} catch (Exception e) {

			System.out.println("WEATHER initialisation problem  ... simulation is canceled !");
			String error = "WEATHER initialisation problem  ";
			Log.println(Log.ERROR, "SafeModel.loadWeather ()", error);
			System.exit(1);		
		}
	}

	/**
	 * EXPORT file loading from ascii file
	 * @param exportFileName Name of the export file
	 * @param outputDir Name of the output folder 
	 * @param projectName Name of the project 
	 */
	public void loadExport(String exportFileName, String outputDir, String projectName) throws Exception {
		try {
			exports = new ArrayList<SafeExportProfile>();
			new SafeExportFormat(exportFileName).load(exports, outputDir, projectName);
			System.out.println("EXPORT PARAMETERS OK");

		} catch (Exception e) {

			System.out.println("EXPORT PARAMETERS initialisation problem  ... simulation is canceled !");
			String error = "EXPORT PARAMETERS initialisation problem  ";
			Log.println(Log.ERROR, "SafeModel.loadExport ()", error);
			System.exit(1);		
		}
	}
	
	/**
	 * Model initialization done once after the stand loading
	 * @param ip Reference to the SafeGeneralParameters object
	 */
	@Override
	public Step initializeModel(InitialParameters ip) throws Exception {

		SafeGeneralParameters generalParameters; 

		SafeStand initStand;

		generalParameters = (SafeGeneralParameters) ip;
		
		//In case initial parameter was created in script mode -il 14/09/2015
		setSettings(generalParameters);

		initStand = (SafeStand) generalParameters.getInitScene();

		// Stand init
		initStand.setDate(0);
		initStand.setJulianDay(0);
		initStand.setWeatherDay(0);
		initStand.setWeatherMonth(0);
		initStand.setWeatherYear(0);
		
		//all stics general objets creation
		sticsParam = new SafeSticsParameters();
		sticsStation = new SafeSticsStation(initStand.getPlot().getPlotSettings().elevation, initStand.getPlot().getPlotSettings().latitude);
		sticsTransit = new SafeSticsTransit();
		
		try {
			
			//loading HISAFE and STICS general parameters	INIT
			//DEJA FAIT DANS le SCRIPT
			loadGeneralParameter (generalParameters);
			
			// Create all objects (plot, trees, voxels) attached to the stand
			initStand.createAll(getSettings());
	
			// Initialise all these objects with initial values
			initStand.initialisation();

		}
		catch (Throwable e) {
			throw e;
		}
			
		lastStep = initStand.getStep();				


		// Load stics dynamic library
		this.loadStics();
	
		// Tell user inits are done
		if (getDebugMode()) StatusDispatcher.print(Translator.swap("SafeModel.initsAreDone"));

		return lastStep;
	}

	/**
	 * Initializations when a project is re-opened
	 */
	protected void projectJustOpened() {

		// Get main references on reopened project
		Project project = getProject();
	
		// Load stics dynamic library
		this.loadStics();

		//search the last step of the re-opened project 
		lastStep = project.getRoot();
		for (Iterator t = project.getNodes().iterator(); t.hasNext();) {
			lastStep = (Step) t.next();
		}
		SafeStand stand = (SafeStand) lastStep.getScene();
		((SafeStand) lastStep.getScene()).setStartDate(new Date());

		//all stics general objets creation
		sticsParam = new SafeSticsParameters();
		sticsStation = new SafeSticsStation(stand.getPlot().getPlotSettings().elevation, stand.getPlot().getPlotSettings().latitude);
		sticsTransit = new SafeSticsTransit();
		
		isReStart = true;	
	}
	
	/**
	 * Stand evolution annual and daily loop
	 * @param stp Reference to the Step object
	 * @param ep Reference to the EvolutionParameters object
	 */
	@Override
	public Step processEvolution(Step stp, EvolutionParameters ep) throws Exception {

		SafeEvolutionParameters evolutionParameters = (SafeEvolutionParameters) ep;

		SafeStand initStand = (SafeStand) stp.getScene();

		SafePlot initPlot = (SafePlot) initStand.getPlot();

		initPlot.computeCellsNeighbourg (evolutionParameters);
		initPlot.initialiseCropZone(evolutionParameters);
		initStand.initialiseTreeItk(evolutionParameters);
		
		// check that dates required for the simulation are available in weather file
		int simulationYearStart = evolutionParameters.simulationDateStart.get(GregorianCalendar.YEAR);
		int simulationDayStart = evolutionParameters.simulationDateStart.get(GregorianCalendar.DAY_OF_YEAR);
		int simulationYearEnd= evolutionParameters.simulationDateEnd.get(GregorianCalendar.YEAR);
		int simulationDayEnd= evolutionParameters.simulationDateEnd.get(GregorianCalendar.DAY_OF_YEAR);
		SafeDailyClimat dayTest = climat.getDailyWeather(simulationYearStart, simulationDayStart);
		dayTest = climat.getDailyWeather(simulationYearEnd, simulationDayEnd);
		SafeDailyClimat yesterdayClimat = null;
		double lastSunDeclination = 0;

		//STICS parameters for chaining years (default = No) 
		sticsParam.P_codeinitprec = 1;	

		//*******************************************
		// ANNUAL LOOP 
		//*******************************************
		yearIndex = 1;

		for (int year = simulationYearStart; year <= simulationYearEnd; year++) {

			int dayStart = 1;
			int dayEnd = 365;
			if (year==simulationYearStart) dayStart = simulationDayStart;
			if (year==simulationYearEnd) dayEnd = simulationDayEnd;		
			if (climat.isLeapYear(year) && dayEnd==365) {
				dayEnd = 366;
			}

			//*******************************************
			// DAILY LOOP 
			//*******************************************
			for (int julianDay = dayStart; julianDay <= dayEnd; julianDay++) {

				java.text.DateFormat fmt1 = new java.text.SimpleDateFormat("yyyyDDD");
				java.text.DateFormat fmt2 = new java.text.SimpleDateFormat("yyyy-MM-dd");
				String simulationDateString = fmt2.format(fmt1.parse(year+""+julianDay));
				String [] part1 = simulationDateString.split("-");
				GregorianCalendar simulationDate= new GregorianCalendar();
				simulationDate.set(Integer.parseInt(part1[0]),Integer.parseInt(part1[1])-1,Integer.parseInt(part1[2]) );
				
				int month = simulationDate.get(GregorianCalendar.MONTH)+1;
				int day = simulationDate.get(GregorianCalendar.DAY_OF_MONTH);

				//Daily RAZ 
				for (Iterator j = initStand.getTrees().iterator(); j.hasNext();) {
					SafeTree t = (SafeTree) j.next();
					t.razDaily();
				}
				for (Iterator j = initPlot.getCells().iterator(); j.hasNext();) {		
					SafeCell c = (SafeCell) j.next();
					c.razDaily();	
					SafeVoxel voxels[] = c.getVoxels();
					for (int iz = 0; iz < voxels.length; iz++) {
						voxels[iz].dailyRaz();
					}
				}
				
				//Plot Annual RAZ
				if (initPlot.getPlotSettings().latitude >= 0) {	//north latitude 01/01
					if ((day == 1) &&  (month == 1)) initPlot.razTotalAnnual();
				}
				else {	//south latitude 01/07
					if ((day == 1) &&  (month == 7)) initPlot.razTotalAnnual();	
				}
		
				//Montly RAZ the first day of the month
				if (day == 1) {
					for (Iterator j = initPlot.getCells().iterator(); j.hasNext();) {
						SafeCell cell = (SafeCell) j.next();
						cell.razTotalMonth();
					}
				}
				
				//Do avoid bad test in case of winter crop
				int testDay = julianDay;
				if (julianDay == 366) {
					if (!climat.isLeapYear(year)) testDay = 1;
				}
				if (julianDay > 366) testDay = julianDay-365;
				
				//************************
				//CHECK TREE PLANTING DAY 
				//************************
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					SafeTree tree = (SafeTree) t.next();

					if (!tree.isPlanted()) {
						
						if (yearIndex == tree.getTreeItk().plantingYear && testDay == tree.getTreeItk().plantingDay) {
							
							try {
								StatusDispatcher.print("Planting Tree ID=" + tree.getId()+" julianDay="+testDay);
								tree.plant(initStand, yearIndex, testDay, isDebugMode);							
								// Computing tree fine and coarse roots initialisation
								tree.fineRootsInitialisation (initStand.getPlot(),  evolutionParameters);
								tree.coarseRootsInitialisation (initStand.getPlot());	
								if ((tree.getTotalLeafArea() == 0) && (testDay > 180) && (tree.getTreeSpecies().getPhenologyType()==1)) {
									tree.setPhenologicalStage(4);
								}	

							} catch (Exception exc1) {
								System.out.println(exc1.getMessage());
								System.out.println("Tree planting problem... simulation is canceled !");
								String error = "Tree planting problem problem ";
								Log.println(Log.ERROR, "SafeModel.fineRootsInitialisation ()", error);
								System.exit(1);
							}
						}												
					}
				}
				//*******************************
				//CHECK CROP ZONE NEW ITK DAY 
				//*******************************
				try {
				
					for (Iterator z = initPlot.getCropZones().iterator(); z.hasNext();) {
						
						SafeCropZone zone = (SafeCropZone) z.next();

						//FIRST SIMULATION
						if (year == simulationYearStart && julianDay == dayStart) {

							if (!isReStart) {	
								
								zone.loadFirstItk(year);
								
								//STICS parameters for chaining years (first year = No) 
								sticsParam.P_codeinitprec = 1;	
									
								int julianDayStart = zone.getJulianDayStart(year);
								int julianDayEnd = zone.getJulianDayEnd (year);
								
								//first initialisation of crops in STICS
								//Stics objects are initialised with initial values			
								zone.initialiseSticsCrop (safeJNA, 
										sticsParam, 
										sticsTransit, 
										getSettings(), 
										initPlot.getPlotSettings(), 
										evolutionParameters,
										initPlot.getSoil(),
										julianDayStart, julianDayEnd);
								
								zone.setSimulationDay(julianDay);
								zone.setSticsSimulationDay(1);

							}
							//REOPEN A PROJECT
							else {
								
								initStand.reloadTreeSpecies(evolutionParameters, getSettings());
							 	String previousSpeciesName = "";						 	
							 	zone.loadNextItk(year, true);
								zone.setSticsSimulationDay(1);
								
								//STICS parameters for chaining years (second year and others = Yes) 
								sticsParam.P_codeinitprec = 2;	
								
								int julianDayStart = zone.getJulianDayStart(year);
								int julianDayEnd = zone.getJulianDayEnd (year);
								
								//Re-initialisation of crops in STICS
								//Stics objects are initialised with last day of previous simulation
								zone.reinitialiseSticsCrop (safeJNA, 
										sticsParam, 
										sticsTransit, 
										getSettings(), 
										evolutionParameters,
										initPlot.getSoil(),
										julianDayStart, julianDayEnd,
										previousSpeciesName);

							}
				
						}
						//OTHER SIMULATIONS 
						else {
											
							if (zone.getSimulationFinished()) {							

								String previousSpeciesName = zone.getCropSpecies().getName();							
								zone.loadNextItk(year, false);
								zone.setSticsSimulationDay(1);
								int julianDayStart = zone.getJulianDayStart(year);
								int julianDayEnd = zone.getJulianDayEnd (year);
								
								//STICS parameters for chaining years (second year and others = Yes) 
								sticsParam.P_codeinitprec = 2;	
								
								//Re-initialisation of crops in STICS
								//Stics objects are initialised with last day of previous simulation
								zone.reinitialiseSticsCrop (safeJNA, 
										sticsParam, 
										sticsTransit, 
										getSettings(), 
										evolutionParameters,
										initPlot.getSoil(),
										julianDayStart, julianDayEnd,
										previousSpeciesName);
							}
						}

						//INIT ANNUAL LOOP 
						if (zone.isDayStart(year, month, day)) {

							int julianDayStart = zone.getJulianDayStart(year);
							int julianDayEnd = zone.getJulianDayEnd (year);
							
							System.out.println("==== Simulation year="+year+" month="+month+" day="+day+" julianDay="+julianDay+" zone="+zone.getName()+" crop="+zone.getCropSpecies().getName()+" ====");

							//STICS climat initialisation (365 days max)  
							SafeSticsClimat sticsClimat = new SafeSticsClimat();						
							climat.loadClimate (sticsClimat, year, julianDayStart, julianDayEnd);
							SafeTestJNA.initClimat(sticsParam, sticsTransit, sticsStation, sticsClimat);

							// Initialise SafeBeamSet
							// Optimization of nbImpactMultiplication for small trees
							int nbImpactMultiplication = SafeLightModel.nbImpactOptimization(initStand);				
							SafeLightModel.initialiseBeamSet(initPlot.getPlotSettings(), getSettings(), climat, nbImpactMultiplication);
							
							for (Iterator c = zone.getCellList().iterator(); c.hasNext();) {
								SafeCell cell = (SafeCell) c.next();
								SafeSticsClimat s1 = new SafeSticsClimat(sticsClimat);
								cell.getCrop().sticsClimat = s1;					
								SafeTestJNA.annualLoopStart(sticsParam, sticsTransit, 
															sticsStation, 
															cell.getCrop().sticsClimat,
															cell.getCrop().sticsCommun, 
															cell.getCrop().sticsSoil,
															cell.getCrop().sticsCrop,
															cell.getCropZone().getSticsItk(),
															cell.getCropZone().getPerenialRepetition(),
															julianDayStart, 
															julianDayEnd, 
															cell.getId(),
															evolutionParameters.outputPath,
															evolutionParameters.sticsReport);
								
								//force crop LAI option 					
								if (cell.getCrop().sticsCommun.P_codesimul==2) 
									cell.getCrop().forceLai(year,julianDayStart,julianDayEnd,this.climat.isLeapYear(year));
								
							}							
							sticsClimat = null; 
						}
					}	//end of crop zone loop
	
		
				} catch (Exception exc) {
					String error = "Climat loading problem ";
					Log.println(Log.ERROR, "SafeModel.loadClimat ()", error);
					throw exc;
				}

				// Get Daily climat 
				SafeDailyClimat dayClimat = climat.getDailyWeather(year, julianDay);

				//snow calculation
				dayClimat.calculateSnow (getSettings(), yesterdayClimat);
				yesterdayClimat = dayClimat;
				
				initStand.setWeatherDay(dayClimat.getDay());
				initStand.setWeatherMonth(dayClimat.getMonth());
				initStand.setWeatherYear(dayClimat.getYear());
				initStand.setJulianDay(julianDay);
				initStand.setDate(julianDay);
	
				// User wait message (with updated date: sr 16-01-09)
				if (getDebugMode()) { //DEBUG MODE ONLY
					String msg = "";
					msg += "JulianDay=" + julianDay+ " Date=" + initStand.getCaption();
					StatusDispatcher.print(msg);
				}

				//***********************************
				//CHECK TREE INTERVENTIONS DAYS 
				//***********************************
				Vector<SafeTree> treesToCut = new Vector<SafeTree>();
				
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					
					SafeTree tree = (SafeTree) t.next();

					if (tree.isPlanted() && !tree.isHarvested()) {

						//TREE HARVEST  
						//the day of thinning we keep the value
						if (yearIndex ==  tree.getTreeItk().treeHarvestYear &&  testDay ==  tree.getTreeItk().treeHarvestDay) {
							tree.harvest(initPlot, yearIndex, testDay);
						}
						//the day after we mark the tree to cut 
						if (yearIndex ==  tree.getTreeItk().treeHarvestYear &&  testDay ==  tree.getTreeItk().treeHarvestDay+1) {
							treesToCut.add(tree);																	
						}	
						
						//PRUNING
						for (int i = 0; i < tree.getTreeItk().treePruningYears.size(); i++) {
							if (yearIndex == tree.getTreeItk().treePruningYears.get(i) && testDay ==  tree.getTreeItk().treePruningDays.get(i) ) {
							
							if (getDebugMode()) StatusDispatcher.print("Pruning Tree ID=" + tree.getId());
								tree.pruning(tree.getTreeItk().treePruningProp.get(i),
											tree.getTreeItk().treePruningMaxHeight.get(i),
											tree.getTreeItk().treePruningResiduesIncorporation.get(i),
											tree.getTreeItk().treePruningResiduesSpreading.get(i));
							}
						}
						
						//ROOT PRUNING 
						for (int i = 0; i <  tree.getTreeItk().treeRootPruningYears.size(); i++) {

							if (yearIndex ==  tree.getTreeItk().treeRootPruningYears.get(i) && testDay ==  tree.getTreeItk().treeRootPruningDays.get(i)) {

								if (getDebugMode()) StatusDispatcher.print("Root Pruning Tree ID=" + tree.getId());
									tree.rootPruning(initStand, 
													tree.getTreeItk().treeRootPruningDistance.get(i), 
													tree.getTreeItk().treeRootPruningDepth.get(i) );
											
							}
						}
						
						//SELF PRUNING 
						if (tree.getTreeSpecies().getSelfPruningEffet()) {
							//DONE only ONCE a YEAR
							if (julianDay == dayStart) 
								tree.removeBranchesDeadOnTree(getSettings(), yearIndex, dayEnd);
							
							tree.selfPruning ();

						}
						
						//TREE TOPPING
						for (int i = 0; i < tree.getTreeItk().treeTopingYears.size(); i++) {

							if (yearIndex == tree.getTreeItk().treeTopingYears.get(i) && testDay == tree.getTreeItk().treeTopingDays.get(i)) {
									
								if (tree.getHeight()>tree.getTreeItk().treeTopingHeight.get(i)) {
									if (getDebugMode()) StatusDispatcher.print("TOPPING Tree ID=" + tree.getId());
										tree.toping(tree.getTreeItk().treeTopingHeight.get(i),
													tree.getTreeItk().treeTopingResiduesIncorporation.get(i),
													tree.getTreeItk().treeTopingResiduesSpreading.get(i));
								}
							}
						}
						
						//LEAF AREA REDUCTION  
						for (int i = 0; i < tree.getTreeItk().leafAreaDensityReductionYears.size(); i++) {
							if (yearIndex == tree.getTreeItk().leafAreaDensityReductionYears.get(i) && testDay == tree.getTreeItk().leafAreaDensityReductionDays.get(i)) {

								if (getDebugMode()) StatusDispatcher.print("LEAF AREA REDUCTION Tree ID=" + tree.getId());
									tree.leafAreaDensityReduction (tree.getTreeItk().leafAreaDensityReductionThreshold.get(i), 
																	tree.getTreeItk().leafAreaDensityReductionFraction.get(i),
																	tree.getTreeItk().leafAreaDensityReductionResiduesIncorporation.get(i),
																	tree.getTreeItk().leafAreaDensityReductionResiduesSpreading.get(i));
																	
							}
						}
						
						//CANOPY TRIMMING 
						for (int i = 0; i < tree.getTreeItk().canopyTrimmingYears.size(); i++) {
							if (yearIndex == tree.getTreeItk().canopyTrimmingYears.get(i) && testDay == tree.getTreeItk().canopyTrimmingDays.get(i)) {
	
									if (getDebugMode()) StatusDispatcher.print("CANOPY TRIMMING Tree ID=" + tree.getId());
									double canopyTrimmingTreeLineTrigger = 0;
									double canopyTrimmingInterRowTrigger = 0;
									double canopyTrimmingTreeLineReductionTarget = 0;
									double canopyTrimmingInterRowReductionTarget = 0;
	
									if (tree.getTreeItk().canopyTrimmingTreeLineTrigger.size()>i) {
										canopyTrimmingTreeLineTrigger = tree.getTreeItk().canopyTrimmingTreeLineTrigger.get(i);
										canopyTrimmingTreeLineReductionTarget = tree.getTreeItk().canopyTrimmingTreeLineReductionTarget.get(i);
									}										
									if (tree.getTreeItk().canopyTrimmingInterRowTrigger.size()>i) {
										canopyTrimmingInterRowTrigger = tree.getTreeItk().canopyTrimmingInterRowTrigger.get(i);
										canopyTrimmingInterRowReductionTarget = tree.getTreeItk().canopyTrimmingInterRowReductionTarget.get(i);
									}
																			
									tree.canopyTrimming (canopyTrimmingTreeLineTrigger, 
														canopyTrimmingTreeLineReductionTarget,
														canopyTrimmingInterRowTrigger,
														canopyTrimmingInterRowReductionTarget,
														tree.getTreeItk().canopyTrimmingResiduesIncorporation.get(i),
														tree.getTreeItk().canopyTrimmingResiduesSpreading.get(i));									
							}
						}
						
						//TREE IRRIGATION
						for (int i = 0; i < tree.getTreeItk().treeIrrigationYears.size(); i++) {
							if (yearIndex == tree.getTreeItk().treeIrrigationYears.get(i)) {
		
								//automatic
								if (tree.getTreeItk().treeIrrigationType==1) {			
									if (tree.getNbrDaysSinceLastIrrigation()>2) {
										if (tree.getWaterStress()< tree.getTreeItk().treeIrrigationWaterStressTrigger) {
											if (getDebugMode()) System.out.println("AUTOMATIC TREE IRRIGATION Tree ID=" + tree.getId());
											tree.treeIrrigation(initPlot, tree.getTreeItk().treeIrrigationAutomaticDose, testDay);
										}
									}
								}
								//manual
								if (tree.getTreeItk().treeIrrigationType==2) {	
									for (int j = 0; j < tree.getTreeItk().treeIrrigationDays.size(); j++) {
		
										if (testDay == tree.getTreeItk().treeIrrigationDays.get(j)) {
											if (getDebugMode()) System.out.println("MANUAL TREE IRRIGATION Tree ID=" + tree.getId()+" testDay="+testDay);
											tree.treeIrrigation(initPlot,tree.getTreeItk().treeIrrigationDose.get(j), testDay);
										}
									}								
								}
							}
						}
						
						//TREE FERTIZATION 
						for (int i = 0; i < tree.getTreeItk().treeFertilizationYears.size(); i++) {
						
							if (yearIndex == tree.getTreeItk().treeFertilizationYears.get(i)) {
								
								//automatic
								if (tree.getTreeItk().treeFertilizationType==1) {						
									if (tree.getNbrDaysSinceLastFertilization()>2) {
										if (tree.getNitrogenSatisfaction()< tree.getTreeItk().treeFertilizationNitrogenStressTrigger ) {
											if (getDebugMode()) System.out.println("AUTOMATIC TREE FERTIZATION Tree ID=" + tree.getId());
											tree.treeFertilization(evolutionParameters, initPlot, 
																	tree.getTreeItk().treeFertilizationAutomaticDose,
																	tree.getTreeItk().treeFertilizerAutomaticCode,
																	testDay);
										}
									}								
								}
								//manual
								if (tree.getTreeItk().treeFertilizationType==2) {
									for (int j = 0; j < tree.getTreeItk().treeFertilizationDays.size(); j++) {

										if (testDay == tree.getTreeItk().treeFertilizationDays.get(j)) {
											if (getDebugMode()) System.out.println("MANUAL TREE FERTIZATION Tree ID=" + tree.getId());
											tree.treeFertilization(evolutionParameters, initPlot, 
																	tree.getTreeItk().treeFertilizationDose.get(j),
																	tree.getTreeItk().treeFertilizerCode.get(j),
																	testDay);	
										}
									}
								}
							}
						}
						
						//SPECIAL INTERVENTION FOR FRUIT TREES
						if (tree.getTreeSpecies ().getFruitCompartment()) {	

							//FRUIT THINNING  
							for (int i = 0; i < tree.getTreeItk().fruitThinningYears.size(); i++) {
								if (yearIndex == tree.getTreeItk().fruitThinningYears.get(i)) {
									if (tree.getTreeItk().fruitThinningMethod.get(i) == 1) {	//automatic fruit thinning
						
										int delay =  tree.getTreeItk().fruitThinningDelayAfterSetting.get(i);
										double fruitOptimalLoadLeafArea = tree.getTreeItk().fruitOptimalLoadLeafArea.get(i);
										tree.fruitAutoThinning (testDay, 
																delay, 
																fruitOptimalLoadLeafArea,
																tree.getTreeItk().fruitThinningResiduesIncorporation.get(i),
																tree.getTreeItk().fruitThinningResiduesSpreading.get(i),
																getDebugMode());
				
									}
									//manual fruit thinning or adjustment 
									if (tree.getTreeItk().fruitThinningMethod.get(i) == 2 || tree.getTreeItk().fruitThinningMethod.get(i) == 3) {	
										if (testDay == tree.getTreeItk().fruitThinningDays.get(i)) {
							
												if (getDebugMode()) StatusDispatcher.print("FRUIT THINNING Tree ID=" + tree.getId());
												int nbrFruitTarget = 0;
												double fruitOptimalLoadLeafArea = tree.getTreeItk().fruitOptimalLoadLeafArea.get(i);
												if ( tree.getTreeItk().fruitThinningMethod.get(i) == 3) {
													nbrFruitTarget = tree.getTreeItk().fruitThinningFruitNbrTarget.get(i);
													fruitOptimalLoadLeafArea = 0;
												}
													
												tree.fruitSimpleThinning (fruitOptimalLoadLeafArea, 
																			nbrFruitTarget,
																			tree.getTreeItk().fruitThinningResiduesIncorporation.get(i),
																			tree.getTreeItk().fruitThinningResiduesSpreading.get(i));
										}
									}	
								}
							}
							
							//FRUIT HARVEST
							for (int i = 0; i < tree.getTreeItk().fruitHarvestDays.size(); i++) {
								if ((testDay) == tree.getTreeItk().fruitHarvestDays.get(i)) {
				
									if (getDebugMode()) StatusDispatcher.print("Tree fruit harvest Tree ID=" + tree.getId());									
									tree.setFruitPhenologicalStage(6);
									tree.setFruitHarvestDate (julianDay);
									tree.setCarbonFruitExported(tree.getCarbonFruit());
									tree.setNitrogenFruitExported(tree.getNitrogenFruit());
									tree.setCarbonFruit (0);
									tree.setNitrogenFruit (0);
									tree.setFruitNbr(0);
	
								}
							}
						}						
					}
				}
				
				//TREE HARVEST 
				// Second step cutting trees 
				for (Iterator<SafeTree> ite = treesToCut.iterator(); ite.hasNext();) {
					SafeTree t = ite.next();
					if (getDebugMode()) StatusDispatcher.print("Thinning Tree ID=" + t.getId());
					//remove the tree
					initStand.removeTree(t);			
				}	

				//***********************************************************************************
				// LIGHT MODEL TRIGGER COMPUTATION
				// the processLighting is computed only if :
				// - this is the first day of simulation
				// - there is leaves in the trees (as least one)
				// - for diffuse : delta between leaf area day/last execution are upper than thresholds
				// - for direct : delta between sun declination day/last execution are upper than thresholds
				//***********************************************************************************
				boolean isDirect = false;
				boolean isDiffus = false;
				float dayDeclination = dayClimat.getSunDeclination();
	
				// if sun declination has changed -> direct light computation
				isDirect = (Math.abs(
						dayDeclination - lastSunDeclination) > (getSettings().declinationThreshold * Math.PI / 180));

				if ((julianDay == simulationDayStart) || (julianDay == 1) || (julianDay == 366)) { // first day of simulation
					isDiffus = isDirect = true;
				}

				double heightMax = 0;
				double crownRadiusMax = 0;
	
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {				
					SafeTree tree = (SafeTree) t.next();				
					if (tree.isPlanted() && !tree.isHarvested()) {
			
						double dayLeafArea = tree.getTotalLeafArea();
						double lastLeafArea = tree.getLastLeafArea();
		
						// if there is leaf on the tree
						if (dayLeafArea > 0) {
							if (lastLeafArea == 0) { // leaf area was null the day before
								isDiffus = isDirect = true;
							}
							// leaf area has increase more than the thresold
							else if ((Math.abs(dayLeafArea - lastLeafArea)
									/ lastLeafArea) > (getSettings().leafAreaThreshold / 100)) {
								isDiffus = isDirect = true;
							}
						} else {
							if (lastLeafArea > 0) { // leaf area was not null the day before
								isDiffus = isDirect = true;
							}
						}
		
						// Recalculation of treeHeightMax and crownRadiusMax
						// for limitation of toric symetrie computation
						if (tree.getHeight() > heightMax)
							heightMax = tree.getHeight();
						if (tree.getCrownRadius() > crownRadiusMax)
							crownRadiusMax = tree.getCrownRadius();
					}
				}
				//crop light interception method  1=Hi-sAFe 
				if (getSettings().hisafeLightMethodForCrop) {
					isDiffus = true;
					isDirect = true;
				}
	
				//***********************************************************
				// LIGHT MODEL EXECUTION
				// only if triggers are ON
				//***********************************************************
				SafeBeamSet<SafeBeam> beamSet = climat.getBeamSet();

				if (isDirect) {
	
					SafeLightModel.beamDirectEnergy(getSettings(), initPlot.getPlotSettings(), beamSet, dayClimat ); // GT 2007
	
					// Calculation of par extinction coefficient for crops (if CropLightMethod == 1 Hi-sAFe) 
					if (getSettings().hisafeLightMethodForCrop) {
						for (Iterator i = initPlot.getCells().iterator(); i.hasNext();) {
							SafeCell c = (SafeCell) i.next();
							SafeCrop crop = c.getCrop();
							crop.computeParExtinctionCoef(beamSet, dayClimat);
						}
					}
	
					// Compute relative cell neighbourhoods for competing trees
					SafeLightModel.computeRelativeCellNeighbourhoods(initPlot.getPlotSettings(), beamSet, heightMax, crownRadiusMax,isDiffus);
					
					// if crop light interception method  1=Hi-sAFe 
					// Compute relative cell neighbourhood for competing crops
					float heightCropMax = 0;
					if (getSettings().hisafeLightMethodForCrop) {
						for (Iterator c = initStand.getPlot().getCells().iterator(); c.hasNext();) {
							SafeCrop crop = ((SafeCell) c.next()).getCrop();
							float height = crop.getHeight();
							if (height > heightCropMax)
								heightCropMax = height;
						}
					}

					SafeLightModel.createShadingMasks(beamSet, getSettings(), initPlot.getPlotSettings(), heightCropMax);
					SafeLightModel.processLighting(initStand, getSettings(), evolutionParameters,  initPlot.getPlotSettings(), beamSet,isDiffus);
	
					// we keep the last values of this execution for testing the next thresholds
					lastSunDeclination = dayDeclination; // sun declination
					for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
						SafeTree tree = (SafeTree) t.next();
						tree.setLastLeafArea(tree.getTotalLeafArea()); // tree leaf area
					}
				}
	
				// Update light results for each tree with daily climat
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					SafeTree tree = (SafeTree) t.next();
					tree.updateDailyLightResults(dayClimat, getSettings()); // GT 2007
				}
	
				// Update light results on each cell with daily climat
				for (Iterator c = initPlot.getCells().iterator(); c.hasNext();) {
					SafeCell cell = (SafeCell) c.next();
					cell.updateDailyLightResults(getSettings(),beamSet, dayClimat);
					cell.getCrop().updateDailyInterceptedPar(getSettings(), beamSet, dayClimat);
				}
	
				//***********************************************************
				// TREES AND CROP PROCESS GROWTH
				//***********************************************************
				processGrowth(initStand, dayClimat, evolutionParameters);

				//**********************************
				// CHECK CROP ZONE ANNUAL LOOP END
				//**********************************
				
				for (Iterator z = initPlot.getCropZones().iterator(); z.hasNext();) {
					SafeCropZone zone = (SafeCropZone) z.next();
					
					if (year == simulationYearEnd && julianDay == dayEnd) {
				
						for (Iterator c = zone.getCellList().iterator(); c.hasNext();) {
							SafeCell cell = (SafeCell) c.next();	
							SafeTestJNA.annualLoopEnd(sticsParam, sticsTransit, 
														sticsStation, 
														cell.getCrop().sticsClimat,
														cell.getCrop().sticsCommun, 
														cell.getCrop().sticsSoil,
														cell.getCrop().sticsCrop,
														cell.getCropZone().getSticsItk(),
														cell.getId());							
						}
					}
					else {
						if (zone.isDayEnd(year, month, day)) {

							for (Iterator c = zone.getCellList().iterator(); c.hasNext();) {
								SafeCell cell = (SafeCell) c.next();
								SafeTestJNA.annualLoopEnd(sticsParam, sticsTransit, 
															sticsStation, 
															cell.getCrop().sticsClimat,
															cell.getCrop().sticsCommun, 
															cell.getCrop().sticsSoil,
															cell.getCrop().sticsCrop,
															cell.getCropZone().getSticsItk(),
															cell.getId());	
								
								cell.getCrop().storeValues(zone.getSticsSimulationDay());
								zone.setSimulationFinished(true);	
							}
						}
						else {
							zone.addSticsSimulationDay();
							zone.addSimulationDay();					

						}
					}
				}
	
				
				//***********************************************************
				// EXPORT DATA 
				//***********************************************************
				//calculate tree stem volume per hectare 
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					SafeTree tree = (SafeTree) t.next();
					tree.setStemVolumePerHectare(tree.getStemVolume()*10000/initPlot.getArea());	
					if (tree.getHarvestingYear() > 0 && !tree.isHarvested()) {					
						tree.setStemVolumeHarvestedPerHectare(tree.getStemVolumePerHectare());
						initPlot.addAnnualTreeStemVolumeHarvestedPerHectare(tree.getStemVolumePerHectare());
						tree.setStemVolumePerHectare(0);
						tree.setStemVolume(0);				
					}
				}
				
				export(stp);

				//tree set harvest after export
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					SafeTree tree = (SafeTree) t.next();
					if (tree.getHarvestingYear()>0) {
						tree.setHarvested(true);
					}
				}

				//a la fin on ne refait pas le new step
				if (year == simulationYearEnd && julianDay == dayEnd) {
					
				}
				else {
					String reason = "Daily Step";
					Step newStp = stp.getProject().processNewStep(stp, initStand, reason);
					stp = newStp;
					
					initPlot.processTotalAnnual();
				}
				
			}//// end of daily loop 
					
			yearIndex++;
			
		}//// end of year loop 

		return stp;
	}

	
	/**
	 * Daily growing process for trees and crops
	 * @param newStand Reference on SafeStand object 
	 * @param dayClimat Reference on SafeDailyClimat object 
	 * @param evolutionParameters Reference on SafeEvolutionParameters object 
	 */
	public void processGrowth (SafeStand newStand, 
							   SafeDailyClimat dayClimat,
							   SafeEvolutionParameters evolutionParameters) {

		SafePlot newPlot = (SafePlot) newStand.getPlot();

		// light and microclimat influence on crop
		double cellRad  = dayClimat.getGlobalRadiation (); 	// default value : light transmitted is 100%
		double cellRain = dayClimat.getRain(); 				// default value : rain transmitted is daily rain 
		double cellEtp  = dayClimat.getEtpPenman(); 		// default value : ETP is daily ETP
		double cellVisibleSky = 1;							// default value : visible sky is 100%

		//Search all cells with trees above and calculate lai of tree above each cell
		newStand.computeLaiAboveCells();
		
		// rain interception and stemflow
		//if there is  water entering the soil today 
		if (dayClimat.getWaterEnteringSoil() > 0)  
			climat.rainTreatement(newStand, dayClimat);				

		// WATER TABLE : voxel under water table depth are saturated (z >= waterTableDepth)
		// saturated voxels are set to field capacity (fine soil+stone)
		// if update, voxels will be desagregated in STICS minicouches
		double waterTableDepth = 100;		//NOT 0 !!!!
		if (((SafeSoil) newPlot.getSoil()).isWaterTable()) {
			waterTableDepth = Math.abs(Math.min(-0.2, dayClimat.getWaterTableDepth()));
			if (waterTableDepth > 0) 
				newPlot.computeWaterTable(waterTableDepth);
		}

		//MINERALISATION of deep root and deep stump (bellow ProfHum) 
		newPlot.deepSenescentRootsMineralization(getSettings() , newPlot.getSoil().getHumificationDepth());
	
		// In case of soil management, some fine roots are removed from trees. // gt-09.07.2009
		double humificationDepth = newPlot.getSoil().getHumificationDepth();
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {
			SafeTree tree = (SafeTree) i.next();
			if (tree.getPlantRoots().getFirstRootNode() != null) 
				tree.soilManagement (newPlot, dayClimat.getJulianDay(), humificationDepth);	
		}
		
		// 1) Process growth for the crop on each cell (part I) before water competition	
		for (Iterator c = newPlot.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();

			// rain on this cell is rain transmitted by trees + stemflow by tree
			cellRain = (float) (cell.getRainTransmittedByTrees() + cell.getStemFlowByTrees());
			
			// light transmitted 
			cellRad = (float)  (cell.getRelativeGlobalRadIncident() * dayClimat.getGlobalRadiation());

			// etp on this cell in influenced by trees			
			double newEtp = dayClimat.getEtpPenman() * cell.getRelativeGlobalRadIncident(); // gt 23 sept 2010
			cell.setEtpCalculated(newEtp);
			cellEtp = (float) cell.getEtpCalculated();
					
			//rounding correction 
			if (cellRad  > dayClimat.getGlobalRadiation ()) cellRad = dayClimat.getGlobalRadiation ();
			
			//Tree influence on visible sky 
			cellVisibleSky 	= Math.max(cell.getVisibleSky(),1.0d);	
			
			//to flag the first cell for each species
			//this first cell will determinate automatic irrigation and fertilisation for all other cells of the same species
			
			int flagFirst = 0;
			if (cell.getId() == cell.getCropZone().getFirstCellId()) flagFirst = 1;



			//if automatic irrigation 
			// the fist cell result has to be copy in other cells of the same ZONE	
			if (flagFirst==0 && cell.getCropZone().getSticsItk().P_codecalirrig==1) {
				SafeCell firstCell = cell.getCropZone().getFirstCell();
				System.arraycopy(firstCell.getCrop().getSticsCommun().airg 	, 0, cell.getCrop().getSticsCommun().airg	, 0, 	366);
			}
			//if automatic fertilisation 
			// the fist cell result has to be copy in other cells of the same species
			if (flagFirst==0 && cell.getCropZone().getSticsItk().P_codecalferti==1) {
				SafeCell firstCell = cell.getCropZone().getFirstCell();
				System.arraycopy(firstCell.getCrop().getSticsCommun().anit 	, 0, cell.getCrop().getSticsCommun().anit	, 0, 	366);
			}

			cell.getCrop().processGrowth1(safeJNA, 
										sticsParam, 
										sticsTransit, 
										sticsStation, 
										getSettings(), 
										dayClimat.getJulianDay(),  
										cell.getCropZone().getSticsSimulationDay(), 
										cellRad, 
										cellRain, 
										cellEtp,
										cellVisibleSky,
										flagFirst);

			
			// Compute agregation results from STICS mini-couches to voxels
			// Crop root density, water content, nitrogen concentration
			cell.miniCouchesToVoxelsAfterStics1 (sticsParam,  waterTableDepth, dayClimat.getJulianDay(), cell.getCropZone().getSticsSimulationDay());

			//After STICS crop root growth 
			//Recalculation of crop root topology
			cell.computeCropRootsTopology (dayClimat.getJulianDay());

		}
		
		
		
		// 2) Process growth for each tree before water repartition
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {
			SafeTree tree = (SafeTree) i.next();
			if (tree.isPlanted() &&  (!tree.getHarvested())) {
				tree.processGrowth1 (newStand, 
								    dayClimat, 
								    climat,
								    getSettings(),
								    isDebugMode);

			}
		}

		// Water repartition between trees and crop in each soil voxel
		if (!getSettings().sticsWaterExtractionForCrop)  {
			//Compute water stress turfac 
			SafeWaterCompetitionModel.computeWaterStressTurfac (newStand, getSettings());			
			//Compute water stress senfac
			SafeWaterCompetitionModel.computeWaterStressSenfac (newStand, getSettings());		
			//Compute water and nitrogen repartition
			SafeWaterCompetitionModel.waterNitrogenRepartition (newStand, getSettings());
		}		

		// 3) Process growth for each tree after water repartition
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {
			SafeTree tree = (SafeTree) i.next();
			if (tree.isPlanted() &&  (!tree.getHarvested())) {
				tree.processGrowth2 (newStand, 
									 dayClimat,
								     getSettings(), 
								     yearIndex,
								     dayClimat.getJulianDay());
				tree.computeTotalRoots();
			}
		}


		// 4) Process growth for the crop on each cell (part II) after water repartition
		for (Iterator c = newPlot.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();

			int hisafeWaterExtraction = 0; 

			if (!getSettings().sticsWaterExtractionForCrop)  {
				
				// Water and nitrogen extraction results desagreggation 
				// from voxels to STICS mini-couches
				cell.voxelsToMiniCouches(getSettings() , newPlot.getPlotSettings());
				
				hisafeWaterExtraction = 1;
			}
				
			//Tree influence on visible sky 
			cellVisibleSky 	= Math.max(cell.getVisibleSky(),1.0d);			

			//TREE LITTER SOIL INCORPORATION
			//tree litter all over the plot is assigned to each cell 
			//https://github.com/hisafe/hisafe/issues/162
			cell.setTreeCarbonFoliageLitter(newStand.getTreesCarbonFoliageLitterAllPlot() * 10000 / newPlot.getArea());	//KG C => KG C ha-1
			cell.setTreeNitrogenFoliageLitter(newStand.getTreesNitrogenFoliageLitterAllPlot() * 10000 / newPlot.getArea() );	//KG CN=> KG C=N ha-1
			cell.setTreeCarbonBranchesLitter(newStand.getTreesCarbonBranchesLitterAllPlot() * 10000 / newPlot.getArea() );	
			cell.setTreeNitrogenBranchesLitter(newStand.getTreesNitrogenBranchesLitterAllPlot() * 10000 / newPlot.getArea() );		
			cell.setTreeCarbonFruitLitter(newStand.getTreesCarbonFruitLitterAllPlot() * 10000 / newPlot.getArea() );	
			cell.setTreeNitrogenFruitLitter(newStand.getTreesNitrogenFruitLitterAllPlot() * 10000 / newPlot.getArea() );	
			
			//tree litter under the crown is assigned to each cell under the tree
			//https://github.com/hisafe/hisafe/issues/162
			Collection<SafeTree> treeAbove = cell.getTreeAbove ();		//collection of tree above this cell

			for (Iterator t = treeAbove.iterator(); t.hasNext();) {
				SafeTree tree = (SafeTree) t.next();
				int nbrCells = tree.getNbCellsBellow();
				cell.addTreeCarbonFoliageLitter(tree.getCarbonFoliageLitterUnderTree()  * 10000 / (nbrCells * cell.getArea()));	//KG C => KG C ha-1
				cell.addTreeNitrogenFoliageLitter(tree.getNitrogenFoliageLitterUnderTree() * 10000 / (nbrCells * cell.getArea()) );	//KG N => KG N ha-1
				cell.addTreeCarbonBranchesLitter(tree.getCarbonBranchesLitterUnderTree()  * 10000/ (nbrCells * cell.getArea()) );	
				cell.addTreeNitrogenBranchesLitter(tree.getNitrogenBranchesLitterUnderTree() * 10000/ (nbrCells * cell.getArea()) );	
				cell.addTreeCarbonFruitLitter(tree.getCarbonFruitLitterUnderTree()  * 10000/ (nbrCells * cell.getArea()) );	
				cell.addTreeNitrogenFruitLitter(tree.getNitrogenFruitLitterUnderTree() * 10000 / (nbrCells * cell.getArea()) );	
			}

			double treeRootDepth = newStand.getTreesMaxRootDepth();
			cell.getCrop().soilIncorporation(safeJNA, 
											sticsParam, 
											cell,
											dayClimat.getJulianDay(),
											newPlot.getSoil().getHumificationDepth(), 
											treeRootDepth,
											cell.getTreeCarbonFoliageLitter(), 
											cell.getTreeNitrogenFoliageLitter(),
											cell.getTreeCarbonBranchesLitter(), 
											cell.getTreeNitrogenBranchesLitter(),
											cell.getTreeCarbonFruitLitter(), 
											cell.getTreeNitrogenFruitLitter());

			//STICS PART II
			cell.getCrop().processGrowth2(safeJNA, 
										sticsParam, 
										sticsTransit, 
										sticsStation, 
										dayClimat.getJulianDay(), 
										cell.getCropZone().getSticsSimulationDay(), 
										hisafeWaterExtraction,
										cellVisibleSky
										);

			// Compute agregation results from STICS mini-couches to voxels		
			cell.miniCouchesToVoxelsAfterStics2 (waterTableDepth);

			// Crop transpiration (if no competition)
			if (getSettings().sticsWaterExtractionForCrop) 
				cell.miniCouchesToVoxelsAfterSticsWaterExtraction (getSettings());

		}
	}

	/**
	 * EXPORT initialisation : creating output files and headers
	 * @param stand Reference to the SafeStand object
	 */
	public void initExport(SafeStand stand) throws Exception {
		Step step = stand.getStep();	
		for (Iterator<SafeExportProfile> c = exports.iterator(); c.hasNext();) {
			SafeExportProfile p = c.next();
			p.selectIds(stand);
			SafeExportNew exp = new SafeExportNew();
			exp.initExport(this, step, p);
			exp.save(p.getFileName(), false);
		}
	}
	/**
	 * Export profiles BATCH MODE ONLY
	 * @param step Reference to the Step object
	 */
	private void export (Step step) throws Exception {
	
		int date = step.getScene().getDate();
		SafeStand stand = (SafeStand) step.getScene();
		int day = stand.getWeatherDay();
		int month = stand.getWeatherMonth();
		int year = stand.getWeatherYear();

		for (Iterator<SafeExportProfile> c = exports.iterator(); c.hasNext();) {
			SafeExportProfile p = c.next();
			int exportFrequency = p.getFrequency();
			boolean export = false;
		
			//pour que l'export tous les 365 jours tombe tjrs le 31/12
			if (exportFrequency == 365)  {
				if ((day == 31) && (month == 12)) export = true;
			}	
			//pour que l'export tous les 30 jours tombe tjrs le 28
			else if (exportFrequency == 30)  {
				if (day == 28) export = true;		
			}
			else {
				if (date % exportFrequency == 0) export = true; 
			}

			if ((exportFrequency > 0) && (export)) {
				SafeExportNew exp = new SafeExportNew();
				exp.export(this, step, p);
				exp.save(p.getFileName(), true);
			}
		}


	}
	
	/**
	 * VERIF SIMULATION before RUNNING
	 *  @param initStand Reference to the SafeStand object
	 *  @param ep Reference to the SafeEvolutionParameters object
	 *  @param myfile Reference to the Path object for output
	 */
	public void verifSimulation(SafeStand initStand, SafeEvolutionParameters ep, Path myfile) throws Exception {

		// Load stics dynamic library
		this.loadStics();
		
		//Date simulation start
		int year = ep.simulationDateStart.get(GregorianCalendar.YEAR);
		int month = (ep.simulationDateStart.get(GregorianCalendar.MONTH))+1;
		int day = ep.simulationDateStart.get(GregorianCalendar.DAY_OF_MONTH);
		int simulationDayStart = ep.simulationDateStart.get(GregorianCalendar.DAY_OF_YEAR);
		
		String line = System.getProperty("line.separator")+"Simulation date start = " + year + "-" + month+"-"+ day;
		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
		
		// Computing cell neighbourgs (for root growth)
		SafePlot initPlot = (SafePlot) initStand.getPlot();
		
		initPlot.initialiseCropZone(ep);
		
		//initialise tree itk
		initStand.initialiseTreeItk(ep);
		
		//verif tree itk
		for (Iterator iter1=initStand.getTrees().iterator(); iter1.hasNext(); ) {
			SafeTree tree = (SafeTree) iter1.next();
			int plantingYear = tree.getTreeItk().plantingYear;
			int plantingDay = tree.getTreeItk().plantingDay;
			if (plantingYear==1 && plantingDay<simulationDayStart){
				System.out.println("TREE PLANTING BEFORE SIMULATION START !");
				System.exit(1);	
			}
		}

		//test crop itk files exist and are well loaded 
		for (Iterator z = initPlot.getCropZones().iterator(); z.hasNext();) {
			
			SafeCropZone zone = (SafeCropZone) z.next();
			List<String> itkList = zone.getItkList();
			zone.loadFirstItk(year);
			int julianDayStart = zone.getJulianDayStart(year);
			if (julianDayStart != simulationDayStart) {
				System.out.println("CROP ITK DAYS START="+julianDayStart+" IS DIFFERENT OF SIMULATION DAY START="+simulationDayStart);
				System.out.println(itkList.get(0));
				System.exit(1);	
			}
			int julianDayEnd = zone.getJulianDayEnd(year);
			if (julianDayEnd>365) julianDayEnd=julianDayEnd-365;

			if (julianDayEnd==366 || julianDayEnd==365) julianDayEnd=1;
			else julianDayEnd++;
			
			line = System.getProperty("line.separator")+"##===============================================";
			line = line + System.getProperty("line.separator")+"## ZONE " + zone.getName();
			line = line + System.getProperty("line.separator")+"##===============================================";
			Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
			
			zone.printVerif (myfile); 
			
			for (int itkIndex=1; itkIndex<itkList.size();itkIndex++) {
				zone.loadNextItk(year, false);
				julianDayStart = zone.getJulianDayStart(year);
				if (julianDayStart != julianDayEnd) {
					System.out.println("SIMULATION DAYS PROBLEM BETWEEN "+itkList.get(itkIndex-1)+" AND "+itkList.get(itkIndex));
					System.out.println("JULIAN DAY END="+julianDayEnd+" JULIAN DAY START="+julianDayStart);
					System.exit(1);		
				}
				
				julianDayEnd = zone.getJulianDayEnd(year);

				if (julianDayEnd>365) julianDayEnd=julianDayEnd-365;
				if (julianDayEnd==366 || julianDayEnd==365) julianDayEnd=1;
				else julianDayEnd++;
				
				zone.printVerif (myfile); 

			}
		}
		
		year = ep.simulationDateEnd.get(GregorianCalendar.YEAR);
		month = (ep.simulationDateEnd.get(GregorianCalendar.MONTH))+1;
		day = ep.simulationDateEnd.get(GregorianCalendar.DAY_OF_MONTH);
		line = System.getProperty("line.separator")+"Simulation date end = "  + year + "-" + month+"-"+ day;
		Files.write(myfile, line.getBytes(), StandardOpenOption.APPEND);
	}

	/**
	 * Post intervention processing (ex: after thining).
	 */
	@Override
	public void processPostIntervention(GScene newScene, GScene prevScene) {
		SafeStand stand = (SafeStand) newScene;
		// after thinning, tree roots have to be removed
		Collection cutTrees = stand.getTrees("cut");

		Collection cutTreesIds = new ArrayList();
		for (Iterator i = cutTrees.iterator(); i.hasNext();) {
			SafeTree t = (SafeTree) i.next();
			cutTreesIds.add(t.getId());
		}
		processRazTreeRoots(stand, cutTreesIds);
	}

	/**
	 * after thinning, tree roots have to be removed RAZ of roots in each rooted voxels
	 */
	public void processRazTreeRoots(SafeStand stand, Collection treeIds) {

		// For each tree "cut" object
		for (Iterator i = treeIds.iterator(); i.hasNext();) {
			Integer tree = (Integer) i.next();
			int treeIndex = tree - 1;

			// RAZ of roots in each rooted voxels
			for (Iterator c = stand.getPlot().getCells().iterator(); c.hasNext();) {
				SafeCell cell = (SafeCell) c.next();
				SafeVoxel voxels[] = cell.getVoxels();
				for (int iz = 0; iz < voxels.length; iz++) {
					voxels[iz].setTreeRootsDensity(treeIndex, 0);
				}
			}
		}
	}

	/**
	 * Accessors
	 */
	public SafeMacroClimat getMacroClimat() {return climat;}
	public boolean getReStart() {return isReStart;}	
	public boolean getDebugMode() {return isDebugMode;}		
	public void setReStart(boolean b) {isReStart = b;}
	public void setDebugMode(boolean b) {isDebugMode = b;}
	public SafeGeneralParameters getSettings() {return (SafeGeneralParameters) settings;}
	public SafeSticsParameters getSticsParam() {return sticsParam;}	
	public SafeSticsTransit getSticsTransit() {return sticsTransit;}	
	public SafeSticsStation getSticsStation() {return sticsStation;}

	public Step getLastStep() {return lastStep;}


}
