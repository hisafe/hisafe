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
package safe.pgms;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;
import java.util.Locale;
import java.util.TimeZone;
import capsis.app.C4Script;
import capsis.kernel.Engine;
import capsis.kernel.MemorizerFactory;
import jeeb.lib.util.PathManager;
import capsis.kernel.Step;
import jeeb.lib.util.Check;
import jeeb.lib.util.Log;
import jeeb.lib.util.StatusDispatcher;
import safe.model.*;


/**
 * A Capsis script to run Hisafe simulation in BATCH MODE
 * 
 * @author Isabelle Lecomte - INRAE Montpellier - july 2009 
 */
public class ScriptGen {

	// The simulation directory where all usefull files have to be stored
	private String simulationDir;

	// All exports go to the output directory
	private String outputDir;

	public static void main(String[] args) throws Exception {
		new ScriptGen(args);
	}

	public ScriptGen(String[] args) throws Exception {

		long timeStart = System.currentTimeMillis();
		long timeEnd = 0;
		SafeModel model;
		Step step = null;

		//CAPSIS original path for Plot description
		String capsisDataPath = PathManager.getInstallDir () + "/data/safe";

		// Check the parameters
		// args[0] is the name of this script
		if (args == null || args.length < 2) {
			System.out.println("Parameter needed: missing simulation file");
		}
		else {
			// Check the simulation file
			String simulationFileName = args[1];
			if (!Check.isFile(simulationFileName)) {
				System.out.println("Wrong simulation file name: " + simulationFileName);
			}
			else {
				// project name
				String projectName = new File(simulationFileName).getName();
				projectName = projectName.replace(".sim", "");

				// Set the simulationDir
				simulationDir = new File(simulationFileName).getParentFile().getAbsolutePath();

				// outputDir
				outputDir = simulationDir + "/output-" + projectName;
				File mydir = new File(outputDir);
		 	    mydir.delete();
				mydir.mkdir();

				try {
					SafeSimulationLoader loader = new SafeSimulationLoader(simulationFileName);
					loader.load();
				
					// Open an existing project with projectFileName
					C4Script script;
					SafeGeneralParameters ip;
			
					if (loader.projectFileName != "") {
						String projectFileName = simulationDir + "/" + loader.projectFileName;
			
						if (!Check.isFile(projectFileName)) {
								throw new Exception("Wrong project file name: " + projectFileName);			
						}
			
			
						script = C4Script.openProject(projectFileName);
						ip = (SafeGeneralParameters) script.getModel().getSettings();
						ip.resetDataPath(simulationDir);
						model = (SafeModel) script.getModel();
			
						model.setReStart(true);
			
						// Set explicitly a compact memorizer on the loaded
						script.setMemorizer(script.getProject(), MemorizerFactory.createCompactMemorizer());

						//load Hisafe and STICS general parameters
						model.loadGeneralParameter (ip);
						
					}

					// Creating a new project
					else {
						script = new C4Script("safe");
								
						//IL 28-11-2017 
						//Plot file name is search automatically with .pld extension
						String pldFileName = getFileName(simulationDir, ".pld");
						if (pldFileName == "") {
							System.out.println("PLD FILE NOT FOUND in folder "+simulationDir);
						}
						
						pldFileName = simulationDir + "/" +pldFileName;

						ip = new SafeGeneralParameters(simulationDir, pldFileName);

						model = (SafeModel) script.getModel();
			
						model.setReStart(false);
			
						// fc-14.4.2017 moved these two lines here: init () must be called
						// only for a new C4Script and not in ReStart mode	
						script.init(ip, MemorizerFactory.createCompactMemorizer());

					}

					//DEBUGGING
					if (loader.debugMode == 1) model.setDebugMode(true);


					//EXPORT file name is search automatically with .out extension
					String exportFile = getFileName(simulationDir, ".out");
					if (!exportFile.equals("")) exportFile = simulationDir + "/" +exportFile;
					else {
						exportFile = getFileName(capsisDataPath, ".out");
						exportFile = capsisDataPath +   "/" + exportFile; 
						System.out.println("WARNING EXPORT.OUT read in folder "+capsisDataPath);						
					}


					model.loadExport(exportFile, outputDir, projectName);
					
					//IL 28-11-2017 
					//Weather file name is search automatically with .wth extension
					String weatherFile = getFileName(simulationDir, ".wth");	
					if (weatherFile == "") {
						System.out.println("WTH FILE NOT FOUND in folder "+simulationDir);
					}
					weatherFile = simulationDir + "/" +weatherFile;

					
					//copy session.txt
					String sessionOrigin = capsisDataPath + "/session.txt";
					String sessionCopy = outputDir + "/session.txt";
					Path monFichier = Paths.get (sessionOrigin);
					Path monFichierCopie = Paths.get (sessionCopy);
					Files.copy (monFichier, monFichierCopie, StandardCopyOption.REPLACE_EXISTING);

					script.getProject().setName(projectName);

					// evolution
					SafeEvolutionParameters ep = new SafeEvolutionParameters (loader, 
																			simulationDir, 
																			outputDir, 
																			weatherFile);
			
					// start from last step of previous simulation (if reload) 
					if (model.getReStart()) {
						step = model.getLastStep();
					}
					// start from new project root step
					else {
						step = (Step) script.getRoot();
					}
			
					SafeStand stand = (SafeStand) (step.getScene());
					model.loadWeather(stand.getLatitude(), stand.getElevation(), ep.weatherFile, ep.simulationDateStart, ep.simulationDateEnd);

					model.initExport(stand);
					
					//RUN the simulation
					step = runSimulation(model, script, step, ep);
			
					//Execution time in session.txt
					timeEnd = System.currentTimeMillis() - timeStart;
					SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",Locale.US);
					GregorianCalendar calendar = new GregorianCalendar(TimeZone.getTimeZone("US/Central"));
					calendar.setTimeInMillis(timeStart);
				
					String start = System.getProperty("line.separator")+"Start of simulation = " + sdf.format(calendar.getTime());
					String end = System.getProperty("line.separator")+"Duration of simulation in seconds = " + timeEnd/1000;
					System.out.println(end);

					Files.write(monFichierCopie, start.getBytes(), StandardOpenOption.APPEND);
					Files.write(monFichierCopie, end.getBytes(), StandardOpenOption.APPEND);
					
					//save the project 
					if (loader.saveProjectOption == 1) {
						StatusDispatcher.print("Saving project " + projectName + " ...");
						Engine.getInstance().processSaveAsProject(script.getProject(),
								outputDir + File.separator + projectName + ".prj");
					}
							
					script.closeProject(script.getProject());

				} catch (Throwable e1) {
					System.out.println("Probleme loading simulation file "+e1);
					throw e1;			
				}
			}
		}
		
		StatusDispatcher.print("END of Simulation");
	}
	/**
	 * Run the simulation
	 */	
	private Step runSimulation(SafeModel model, C4Script script, Step step, SafeEvolutionParameters ep)
			throws Exception {

		try {


			step = script.evolve(step, ep);

			if (step == null) {
				throw new Exception("ScriptGen: evolve () failed, see Log");
			}


			return step;
			
		} catch (Exception exc) {
			System.out.println("Simulation STOP");
			String error = "Simulation STOP";
			Log.println(Log.ERROR, "SafeModel.runSimulation ()", error);
			throw new Exception("ScriptGen: evolve () failed, see Log");
		}

	}
	
	/**
	 * Searching a file in a folder with extension 
	 */	
	private String getFileName (String folderName, String extension) {
		
		String fileName;
		
		try {
			File folder = new File (folderName);
			File[] files = folder.listFiles ();
			for (int i = 0; i < files.length; i++) {
				File f = files[i];
				fileName = f.getName();	
				int fileNameLength = fileName.length();				
				boolean isGoodProfileExtension = false; 
				//To avoid ghost files from MAC
				if (!fileName.startsWith(".")) {
					if (fileName.contains(".")) {
						isGoodProfileExtension = (fileName.substring(fileNameLength-4,fileNameLength)).equals(extension);	
					}
					if(isGoodProfileExtension){
						return fileName;
					}
				}
			}
		} catch (Exception e) {
			return "";
		}
		return "";
	}
}
