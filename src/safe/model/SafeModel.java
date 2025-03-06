/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
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
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
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
 * SafeModel is the main class for Hi-sAFe 
 * @author Isabelle LECOMTE -  INRAE Montpellier - July 2002 
 * Last update September 2024 (version 4.3.21238)
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
	
	// CLIMAT ENTRIES 
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
	 * Does not change the SafeModel's SafeGeneralParameters object.
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
	 * LOAD general parameter for STICS and HISAFE
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
			
				//LAST CHANCE try to find files in safe/data/ folder 
				try {

					this.loadHISAFEParameter(safeSettings.dataOriginalPath+"/generalParameters/hisafe.par");
					System.out.println("WARNING HISAFE.PAR read in folder "+safeSettings.dataOriginalPath);

				} catch (Throwable e3) {

					System.out.println("HISAFE GENERAL PARAMETERS reading problem  ... simulation is canceled !");
					String error = "HISAFE GENERAL PARAMETERS reading problem  ";
					Log.println(Log.ERROR, "SafeModel.loadGeneralParameter ()", error);
					System.exit(1);					
				}

		}
		
		try {
	
			this.loadSTICSParameter(safeSettings.dataPath+"/generalParameters/stics.par");
			System.out.println("STICS GENERAL PARAMETERS OK");
	
		} catch (Throwable e1) {	
			try {

				this.loadSTICSParameter(safeSettings.dataOriginalPath+"/generalParameters/stics.par");
				System.out.println("WARNING STICS.PAR read in folder "+safeSettings.dataOriginalPath);
				

					
				} catch (Throwable e3) {
					System.out.println("STICS GENERAL PARAMETERS reading problem  ... simulation is canceled !");
					String error = "STICS GENERAL PARAMETERS reading problem  ";
					Log.println(Log.ERROR, "SafeModel.loadGeneralParameter ()", error);
					System.exit(1);
				}
			}
		
	}	
	
	/**
	 * HISAFE general parameter file loading from ascii file 
	 */
	private void loadHISAFEParameter (String paramFileName) throws Exception {
		new SafeGeneralParameterFormat(paramFileName).load(getSettings(), this.sticsStation);
	}

	/**
	 * STICS general parameter file loading from ascii file 
	 */
	private void loadSTICSParameter (String paramFileName) throws Exception {
		new SafeSticsParametersFormat(paramFileName).load(getSettings(), this.sticsTransit, this.sticsParam);
	}
	
	/**
	 * Weather file loading from ascii file
	 */
	public void loadWeather(double latitude, double elevation, String weatherFileName,  
							GregorianCalendar simulationDateStart, GregorianCalendar simulationDateEnd) throws Exception {

		try {

			new SafeMacroFormat(weatherFileName).load(getSettings(), climat, this.sticsStation, latitude, elevation);
			
			//check all date are in the file 
			int simulationYearStart = simulationDateStart.get(GregorianCalendar.YEAR);
			int simulationDayStart = simulationDateStart.get(GregorianCalendar.DAY_OF_YEAR);
			
			int simulationYearEnd = simulationDateEnd.get(GregorianCalendar.YEAR);
			int simulationDayEnd = simulationDateEnd.get(GregorianCalendar.DAY_OF_YEAR);
		
			for (int year = simulationYearStart; year <= simulationYearEnd; year++) {

				int dayStart = 1;
				int dayEnd = 365;

				if (year==simulationYearStart) dayStart = simulationDayStart;
				if (year==simulationYearEnd) dayEnd = simulationDayEnd;
				if (climat.isLeapYear(year) && dayEnd==365) {
					dayEnd = 366;
				}
				//check all days are in the weather file without error
				for (int julianDay = dayStart; julianDay <= dayEnd; julianDay++) {
					SafeDailyClimat dayClimat = climat.getDailyWeather(year, julianDay);
				}
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
	 * These initializations are done once after the stand loading
	 */
	@Override
	public Step initializeModel(InitialParameters ip) throws Exception {

		SafeGeneralParameters safeSettings; 

		SafeStand initStand;

		safeSettings = (SafeGeneralParameters) ip;
		
		//In case initial parameter was created in script mode -il 14/09/2015
		setSettings(safeSettings);

		//all stics general objets creation
		sticsParam = new SafeSticsParameters();
		sticsStation = new SafeSticsStation();
		sticsTransit = new SafeSticsTransit();

		initStand = (SafeStand) safeSettings.getInitScene();

		// Stand init
		initStand.setDate(0);
		initStand.setJulianDay(0);
		initStand.setWeatherDay(0);
		initStand.setWeatherMonth(0);
		initStand.setWeatherYear(0);
		
		try {
			
			//loading HISAFE and STICS general parameters	INIT
			//DEJA FAIT DANS le SCRIPT
			loadGeneralParameter (safeSettings);
			
			// Create all objects (plot, trees, voxels) attached to the stand
			initStand.createAll(getSettings(), getPlotSettings());
	
			// Initialise all these objects with initial values
			initStand.initialisation(getSettings(), getPlotSettings());

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
			
		//all stics general objets creation
		sticsParam = new SafeSticsParameters();
		sticsStation = new SafeSticsStation();
		sticsTransit = new SafeSticsTransit();
		
		//search the last step of the re-opened project 
		lastStep = project.getRoot();
		for (Iterator t = project.getNodes().iterator(); t.hasNext();) {
			lastStep = (Step) t.next();
		}
		
		((SafeStand) lastStep.getScene()).setStartDate(new Date());

		
		isReStart = true;	

	}
	

	/**
	 * Evolution for SafeModel. 
	 * This evolution performs classically one single rotation.
	 */
	@Override
	public Step processEvolution(Step stp, EvolutionParameters ep) throws Exception {

		SafeEvolutionParameters evolutionParameters = (SafeEvolutionParameters) ep;

		SafeStand initStand = (SafeStand) stp.getScene();

		SafePlot initPlot = (SafePlot) initStand.getPlot();


		// Computing cell neighbourgs (for root growth)
		initPlot.computeCellsNeighbourg (evolutionParameters);
		
		initPlot.initialiseCropZone(evolutionParameters);
		
		//initialise tree itk
		initStand.initialiseTreeItk(evolutionParameters, getSettings());
		
		double lastSunDeclination = 0;

		int simulationYearStart = evolutionParameters.simulationDateStart.get(GregorianCalendar.YEAR);
		int simulationDayStart = evolutionParameters.simulationDateStart.get(GregorianCalendar.DAY_OF_YEAR);
		
		int simulationYearEnd= evolutionParameters.simulationDateEnd.get(GregorianCalendar.YEAR);
		int simulationDayEnd= evolutionParameters.simulationDateEnd.get(GregorianCalendar.DAY_OF_YEAR);


		// check that dates required for the simulation are available in this file
		SafeDailyClimat dayTest = climat.getDailyWeather(simulationYearStart, simulationDayStart);
		dayTest = climat.getDailyWeather(simulationYearEnd, simulationDayEnd);
		
		
		SafeDailyClimat yesterdayClimat = null;

		//ATTENTION ICI Si on a des zones diffï¿½rente, il faudra mettre = 2 mais pas partout si on est en decallage
		sticsParam.P_codeinitprec = 1;		//enchainement=Non

		//*******************************************
		// YEAR loop 
		//*******************************************
		//Initialize the year index
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
			// DAILY loop 
			//*******************************************
			for (int julianDay = dayStart; julianDay <= dayEnd; julianDay++) {

				java.text.DateFormat fmt1 = new java.text.SimpleDateFormat("yyyyDDD");
				java.text.DateFormat fmt2 = new java.text.SimpleDateFormat("yyyy-MM-dd");
				String simulationDateString = fmt2.format(fmt1.parse(year+""+julianDay));
				String [] part1 = simulationDateString.split("-");
				GregorianCalendar simulationDate= new GregorianCalendar();
				simulationDate.set(Integer.parseInt(part1[0]),Integer.parseInt(part1[1])-1,Integer.parseInt(part1[2]) );
				
				int month = simulationDate.get(GregorianCalendar.MONTH);
				month++;
				int day = simulationDate.get(GregorianCalendar.DAY_OF_MONTH);

				//Daily RAZ 
				for (Iterator j = initStand.getTrees().iterator(); j.hasNext();) {
					SafeTree t = (SafeTree) j.next();
					t.dailyRaz();
				}
				for (Iterator j = initPlot.getCells().iterator(); j.hasNext();) {
		
					SafeCell c = (SafeCell) j.next();
					c.dailyRaz();
					SafeCrop crop = c.getCrop();
					crop.dailyRaz();

					SafeVoxel voxels[] = c.getVoxels();
					for (int iz = 0; iz < voxels.length; iz++) {
						voxels[iz].dailyRaz();
					}
					

				}
				//Plot Annual RAZ
				if (initStand.getLatitude() >= 0) {	//north latitude 01/01
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
				//TREE PLANTING 
				//************************
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					SafeTree tree = (SafeTree) t.next();

					
					if (!tree.isPlanted()) {
						
						if (yearIndex == tree.getTreeItk().plantingYear && testDay == tree.getTreeItk().plantingDay) {
							
							try {
								StatusDispatcher.print("Planting Tree ID=" + tree.getId());
								tree.plant(initStand, isDebugMode);							
								// Computing tree fine and coarse roots initialisation
								tree.fineRootsInitialisation (initStand.getPlot(),  getSettings(), getPlotSettings(), evolutionParameters);
								tree.coarseRootsInitialisation (initStand.getPlot(),  getSettings(), getPlotSettings());	// gt - 5.10.2009 - added initialValues		
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

				try {
				
					for (Iterator z = initPlot.getCropZones().iterator(); z.hasNext();) {
						
						SafeCropZone zone = (SafeCropZone) z.next();

						//FIRST SIMULATION
						if (year == simulationYearStart && julianDay == dayStart) {

							System.out.println("=========== Simulation year = "+year+" Zone = "+zone.getName()+" ===================");

							if (!isReStart) {	
								
								zone.loadFirstItk(year);
								
								sticsParam.P_codeinitprec = 1;		//enchainement=Non
									
								//first initialisation of crops in STICS
								//Stics objects are initialised with initial values			
								zone.initialiseSticsCrop (safeJNA, 
										sticsParam, 
										sticsTransit, 
										getSettings(), 
										getPlotSettings(), 
										evolutionParameters,
										initPlot.getSoil());
								
								zone.setSimulationDay(julianDay);
								zone.setSticsDay(1);

							}
							//REOPEN A PROJECT
							else {
								
								initStand.reloadTreeSpecies(evolutionParameters, getSettings());

								zone.loadNextItk(year);
								
								zone.setSticsDay(1);
								
								sticsParam.P_codeinitprec = 2;		//enchainement=Oui
								
								//Re-initialisation of crops in STICS
								//Stics objects are initialised with last day of previous simulation
								zone.reinitialiseSticsCrop (safeJNA, 
										sticsParam, 
										sticsTransit, 
										getSettings(), 
										getPlotSettings(), 
										evolutionParameters,
										initPlot.getSoil());

							}

						}
						//OTHER SIMULATIONS 
						else {
											
							if (zone.getSimulationFinished()) {							

								System.out.println("=========== Simulation year = "+year+" Zone = "+zone.getName()+" ===================");
								
								zone.loadNextItk(year);
								zone.setSticsDay(1);

								sticsParam.P_codeinitprec = 2;		//enchainement=Oui
								
								//Re-initialisation of crops in STICS
								//Stics objects are initialised with last day of previous simulation
								zone.reinitialiseSticsCrop (safeJNA, 
										sticsParam, 
										sticsTransit, 
										getSettings(), 
										getPlotSettings(), 
										evolutionParameters,
										initPlot.getSoil());
							}
						}

						//INIT ANNUAL LOOP 
						if (zone.isDayStart(year, month, day)) {

							int juliandayStart = zone.getJulianDayStart(year);
							int juliandayEnd = zone.getJulianDayEnd (year);

							//STICS climat initialisation (365 days max)  
							SafeSticsClimat sticsClimat = new SafeSticsClimat();
							
							climat.loadClimate (sticsClimat, year, juliandayStart, juliandayEnd);

							safeJNA.initClimat(sticsParam, sticsTransit, sticsStation, sticsClimat);

							// Initialise SafeBeamSet
							// Optimization of nbImpactMultiplication for small trees
							int nbImpactMultiplication = SafeLightModel.nbImpactOptimization(initStand);				
							SafeLightModel.initialiseBeamSet(climat, getPlotSettings(), evolutionParameters, getSettings(), nbImpactMultiplication);

							for (Iterator c = zone.getCellList().iterator(); c.hasNext();) {
								SafeCell cell = (SafeCell) c.next();

								SafeSticsClimat s1 = new SafeSticsClimat(sticsClimat);
								cell.getCrop().sticsClimat = s1;
						
								safeJNA.initBoucleAnnuelle(sticsParam, sticsTransit, 
															sticsStation, 
															cell.getCrop().sticsClimat,
															cell.getCrop().sticsCommun, 
															cell.getCrop().sticsSoil,
															cell.getCrop().sticsCrop,
															cell.getCropZone().getSticsItk(),
															juliandayStart, 
															juliandayEnd, 
															cell.getId(),
															evolutionParameters.exportDir,
															evolutionParameters.sticsReport);
								
								//force LAI 					
								if (cell.getCrop().sticsCommun.P_codesimul==2) 
									cell.getCrop().forceLai(year,juliandayStart,juliandayEnd,this.climat.isLeapYear(year));
								
							}	
							
							sticsClimat = null; 
							

						}
					}	//fin boucle sur les zones
	
		
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

				//************************
				//TREE INTERVENTIONS 
				//************************
				Vector<SafeTree> treesToCut = new Vector<SafeTree>();
				
				for (Iterator t = initStand.getTrees().iterator(); t.hasNext();) {
					
					SafeTree tree = (SafeTree) t.next();

					if (tree.isPlanted() && !tree.isHarvested()) {

						
						//TREE HARVEST  
						//the day of thinning we keep the value
						if (yearIndex ==  tree.getTreeItk().treeHarvestYear &&  testDay ==  tree.getTreeItk().treeHarvestDay) {
							tree.harvest(initPlot, yearIndex, testDay,  this.getSettings());
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
													getSettings(),  
													tree.getTreeItk().treeRootPruningDistance.get(i), 
													tree.getTreeItk().treeRootPruningDepth.get(i) );
											
							}
						}
						
						//SELF PRUNING 
						if (tree.getTreeSpecies().getSelfPruningEffet()) {
							//DONE only ONCE a YEAR
							if (julianDay == dayStart) 
								tree.removeBranchesDeadOnTree(yearIndex, dayEnd, getSettings());
							
							tree.selfPruning (yearIndex);

						}
						
						//TREE TOPPING
						for (int i = 0; i < tree.getTreeItk().treeTopingYears.size(); i++) {

							if (yearIndex == tree.getTreeItk().treeTopingYears.get(i) && testDay == tree.getTreeItk().treeTopingDays.get(i)) {
									
								if (tree.getHeight()>tree.getTreeItk().treeTopingHeight.get(i)) {
									if (getDebugMode()) StatusDispatcher.print("TOPPING Tree ID=" + tree.getId());
										tree.toping(initStand, 
													tree.getTreeItk().treeTopingHeight.get(i),
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

											if (getDebugMode()) System.out.println("MANUAL TREE IRRIGATION Tree ID=" + tree.getId());
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
																getDebugMode(), 
																delay, 
																fruitOptimalLoadLeafArea,
																tree.getTreeItk().fruitThinningResiduesIncorporation.get(i),
																tree.getTreeItk().fruitThinningResiduesSpreading.get(i));
				
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
				
				//TREE THINNING 
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
							if (lastLeafArea > 0) { // leaf area was not null the day
													// before
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
				if (getSettings().cropLightMethod) {
					isDiffus = true;
					isDirect = true;
				}
	

				//***********************************************************
				// LIGHT MODEL EXECUTION
				// only if triggers are ON
				//***********************************************************
				SafeBeamSet<SafeBeam> beamSet = climat.getBeamSet();

				
				if (isDirect) {
	
					SafeLightModel.beamDirectEnergy(getSettings(), beamSet, dayClimat, getPlotSettings()); // GT 2007
	
					// Calculation of extinction coefficient for crops (if CropLightMethod == 1 Hi-sAFe) 
					if (getSettings().cropLightMethod) {
	
						for (Iterator i = initPlot.getCells().iterator(); i.hasNext();) {
							SafeCell c = (SafeCell) i.next();
							SafeCrop crop = c.getCrop();
							crop.findCropLightCoef(beamSet, getSettings(), dayClimat);
						}
					}
	
					// Compute relative cell neighbourhoods for competing trees
					SafeLightModel.computeRelativeCellNeighbourhoods(beamSet, getPlotSettings(), heightMax, crownRadiusMax,
							isDiffus);
					
					// if crop light interception method  1=Hi-sAFe 
					// Compute relative cell neighbourhood for competing crops
					float heightCropMax = 0;
					if (getSettings().cropLightMethod) {
						for (Iterator c = initStand.getPlot().getCells().iterator(); c.hasNext();) {
							SafeCrop crop = ((SafeCell) c.next()).getCrop();
							float height = crop.getHeight();
							if (height > heightCropMax)
								heightCropMax = height;
						}
					}

					
					SafeLightModel.createShadingMasks(beamSet, getSettings(), getPlotSettings(), heightCropMax);
					SafeLightModel.processLighting(initStand, getPlotSettings(), evolutionParameters, getSettings(), beamSet,
							isDiffus);
	
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
					tree.updateDailyLightResults(dayClimat, evolutionParameters, getSettings()); // GT 2007
				}
	
				// Update light results on each cell with daily climat
				for (Iterator c = initPlot.getCells().iterator(); c.hasNext();) {
					SafeCell cell = (SafeCell) c.next();
	
					cell.updateDailyLightResults(beamSet, dayClimat, getSettings());
	
					cell.getCrop().updateDailyInterceptedPar(dayClimat, getSettings(), beamSet);

				}
	
				//***********************************************************
				// TREES AND CROP PROCESS GROWTH
				//***********************************************************

				processGrowth(initStand, dayClimat, evolutionParameters);

				for (Iterator z = initPlot.getCropZones().iterator(); z.hasNext();) {
					SafeCropZone zone = (SafeCropZone) z.next();
					
					if (year == simulationYearEnd && julianDay == dayEnd) {
				
						for (Iterator c = zone.getCellList().iterator(); c.hasNext();) {
							SafeCell cell = (SafeCell) c.next();
							
							safeJNA.finBoucleAnnuelle(sticsParam, sticsTransit, 
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
			
								safeJNA.finBoucleAnnuelle(sticsParam, sticsTransit, 
															sticsStation, 
															cell.getCrop().sticsClimat,
															cell.getCrop().sticsCommun, 
															cell.getCrop().sticsSoil,
															cell.getCrop().sticsCrop,
															cell.getCropZone().getSticsItk(),
															cell.getId());	
								
								cell.getCrop().storeValues(zone.getSticsDay());
								
								
								
								zone.setSimulationFinished(true);	

								
								
							}


						}
						else {
							zone.addSticsDay();
							zone.addSimulationDay();					

						}

					}

				}
	
				
				//***********************************************************
				// EXPORT DATA 
				//***********************************************************
				export(stp);

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
	 * Growing method for SafeModel newStand = the new stand (day j)
	 * 
	 * return true if the step has to be visible (budburst date, sowing or harvesting crop ...)
	 */
	public void processGrowth (SafeStand newStand, 
								  SafeDailyClimat dayClimat,
								  SafeEvolutionParameters evolutionParameters) {

		SafePlot newPlot = (SafePlot) newStand.getPlot();


		// maybe useless... to clean !!!
		int simulationYear = newStand.getWeatherYear(); // gt - 14.01.2009
		int simulationDay = dayClimat.getJulianDay(); // gt - 14.01.2009
		int julianDay = newStand.getJulianDay(); // gt - 14.01.2009


		// light and microclimat influence on crop
		double cellRad  = dayClimat.getGlobalRadiation (); 	// default value : light transmitted is 100%
		double cellRain = dayClimat.getRain(); 				// default value : rain transmitted is daily rain 
		double cellEtp  = dayClimat.getEtpPenman(); 		// default value : ETP is daily ETP
		double cellVisibleSky = 1;							// default value : visible sky is 100%

		//Search all cells with trees above and calculate lai of tree above each cell
		newStand.computeLaiAboveCells();
		
		// rain interception and stemflow
		//if there is  water entering the soil today 
		if (dayClimat.getWaterEnteringSoil() > 0)  {
			climat.rainTreatement(getSettings(), newStand, dayClimat);				
		}


		// WATER TABLE : voxel under water table depth are saturated (z >= waterTableDepth)
		// saturated voxels are set to field capacity (fine soil+stone)
		// if update, voxels will be desagregated in STICS minicouches
		double waterTableDepth = 100;		//NOT 0 !!!!
		if (((SafeSoil) newPlot.getSoil()).isWaterTable()) {
			waterTableDepth = Math.abs(Math.min(-0.2, dayClimat.getWaterTableDepth()));
			if (waterTableDepth > 0) 
				newPlot.computeWaterTable(getSettings(), getPlotSettings(), waterTableDepth, true);
		}



		//MINERALISATION of deep root and deep stump (bellow ProfHum) 
		newPlot.deepSenescentRootsMineralization(newPlot.getSoil().getHumificationDepth(), getSettings());
	
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

			// In case of soil management, some fine roots are removed from trees. // gt-09.07.2009
			// If soil management depth is below the gravity center of a voxel,
			// the coarse root in this voxel and all depending topology are removed
			float soilManagementDepth = cell.getCrop().getSoilManagementDepth(julianDay);
			if (soilManagementDepth > 0) 
				newStand.treeRootSoilManagement (cell, soilManagementDepth,  getSettings());	

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
										cell,
										getSettings(), 
										evolutionParameters,
										simulationYear, 
										julianDay,  
										cell.getCropZone().getSticsDay(), 
										cellRad, 
										cellRain, 
										cellEtp,
										cellVisibleSky,
										flagFirst);

			// Compute agregation results from STICS mini-couches to voxels
			// Crop root density, water content, nitrogen concentration
			cell.miniCouchesToVoxelsAfterStics1 (sticsParam,  waterTableDepth, julianDay, cell.getCropZone().getSticsDay());


			//After STICS crop root growth 
			//Recalculation of crop root topology
			cell.computeCropRootsTopology (julianDay);

		}
		
		
		
		// 2) Process growth for each tree before water repartition
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();

			if (tree.isPlanted() &&  (!tree.getHarvested())) {

			//	tree.drawRootTopology ();


				tree.processGrowth1 (newStand, 
								    getSettings(), 
								    getPlotSettings(), 
								    dayClimat, 
								    climat,
								    isDebugMode); 		
				
			}
		}

		

		
		// Water repartition between trees and crop in each soil voxel
		if (!getSettings().sticsWaterExtraction)  {
			//calcul Turfac 
			SafeWaterCompetitionModel.waterNitrogenRepartitionTurfac(newStand, getSettings(), simulationDay);
			
			//calcul senfac
			SafeWaterCompetitionModel.waterNitrogenRepartitionSenfac(newStand, getSettings(), simulationDay);
			
			//calcul swfac
			SafeWaterCompetitionModel.waterNitrogenRepartition(newStand, getSettings(), simulationDay);

		}		

		
	
		
		// 3) Process growth for each tree after water repartition
		for (Iterator i = newStand.getTrees().iterator(); i.hasNext();) {

			SafeTree tree = (SafeTree) i.next();
			
			if (tree.isPlanted() &&  (!tree.getHarvested())) {
				

			//	tree.drawRootTopology ();
			//	tree.drawRootMap ();
				tree.processGrowth2 (newStand, 
								     getSettings(), 
								     getPlotSettings(), 
								     evolutionParameters,
								     dayClimat, 
								     yearIndex,
								     simulationDay);

			}
		}


		// 4) Process growth for the crop on each cell (part II) after water repartition
		for (Iterator c = newPlot.getCells().iterator(); c.hasNext();) {

			SafeCell cell = (SafeCell) c.next();
	
			SafeVoxel voxels[] = cell.getVoxels();


			int flagInfluence = 0; 

			if (!getSettings().sticsWaterExtraction)  {
				
				// Water and nitrogen extraction results desagreggation 
				// from voxels to STICS mini-couches
				cell.voxelsToMiniCouches(getSettings() , isDebugMode);
				
				flagInfluence = 1;
			}
				
			//Tree influence on visible sky 
			cellVisibleSky 	= Math.max(cell.getVisibleSky(),1.0d);			

	
			

			//*******************************************************
			//TREE LITTER SOIL INCORPORATION
			//*******************************************************
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
											evolutionParameters, 
											cell,
											julianDay,
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
										newStand,
										simulationYear, 
										julianDay, 
										cell.getCropZone().getSticsDay(), 
										flagInfluence,
										cellVisibleSky
										);

			// Compute agregation results from STICS mini-couches to voxels		
			cell.miniCouchesToVoxelsAfterStics2 (waterTableDepth);

			// Crop transpiration (if no competition)
			if (getSettings().sticsWaterExtraction) 
				cell.miniCouchesToVoxelsAfterSticsWaterExtraction ();
			
		}
	}

	/**
	 * EXPORT initialisation : creating output files and headers
	 */
	public void initExport(SafeStand initStand) throws Exception {
		Step step = initStand.getStep();	
		for (Iterator<SafeExportProfile> c = exports.iterator(); c.hasNext();) {
			SafeExportProfile p = c.next();
			p.selectIds(initStand);
			SafeExportNew exp = new SafeExportNew();
			exp.initExport(this, step, p);
			exp.save(p.getFileName(), false);
		}
	}
	/**
	 * Export profiles BATCH MODE ONLY
	 */
	private void export (Step step) throws Exception {
	
		int date = step.getScene().getDate();
		SafeStand stand = (SafeStand) step.getScene();
		int day = stand.getWeatherDay();
		int month = stand.getWeatherMonth();

			
		for (Iterator<SafeExportProfile> c = exports.iterator(); c.hasNext();) {
			SafeExportProfile p = c.next();
			int exportFrequency = p.getFrequency();
			boolean export = false;
			
			//pour que l'export tous les 365 jours tombe tjrs le 31/12
			if (exportFrequency == 365)  {
				if ((day == 31) && (month == 12)) export = true;
			}	
			//pour que l'export tous les 30 jours 
			//tombe tjrs quand le jour suivant est le 1er 
			else if (exportFrequency == 30)  {
				try {
					int year = stand.getWeatherYear();
					int julian = stand.getJulianDay();
					julian++;
					
					// leap year
					int nbDayMax = 365;
					if (climat.isLeapYear(year)) {
						nbDayMax = 366;
					}
					if (julian > nbDayMax) {
						julian = julian - nbDayMax;
						year = year + 1;
					}

					SafeDailyClimat nextDay = climat.getDailyWeather (year, julian);
					if (nextDay.getDay() == 1) export = true;
				
				} catch (Exception exc) {
					export = true;
				}
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
	 */
	public void verifSimulation(SafeStand initStand, InitialParameters ip, SafeEvolutionParameters ep, Path myfile) throws Exception {

		SafeGeneralParameters safeSettings; 

		safeSettings = (SafeGeneralParameters) ip;

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
		initStand.initialiseTreeItk(ep, getSettings());
		
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
				zone.loadNextItk(year);
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
	public SafePlotSettings getPlotSettings() {return getSettings().plotSettings;}

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
