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

package safe.pgms;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import capsis.app.C4Script;
import capsis.kernel.MemorizerFactory;
import jeeb.lib.util.PathManager;
import capsis.kernel.Step;
import jeeb.lib.util.Check;
import jeeb.lib.util.StatusDispatcher;
import safe.model.*;

/**
 * A Capsis script to check Hisafe simulation parameters before running
 * 
 * 
 * @author Isabelle Lecomte - INRAE Montpellier - January 2024 
 */

public class ScriptVerif {

	// The simulation directory where all usefull files have to be stored
	private String simulationPath;

	// All exports go to the output directory
	private String outputPath;

	public static void main(String[] args) throws Exception {
		new ScriptVerif(args);
	}

	public ScriptVerif(String[] args) throws Exception {

		SafeModel model;
		Step step = null;

		//CAPSIS original path for safe project
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
				simulationPath = new File(simulationFileName).getParentFile().getAbsolutePath();

				// outputDir
				outputPath = simulationPath + "/output-" + projectName;
				File mydir = new File(outputPath);
		 	    mydir.delete();
				mydir.mkdir();

				try {
					SafeSimulationLoader loader = new SafeSimulationLoader(simulationFileName);
					loader.load();
				
					// Open an existing project with projectFileName
					C4Script script;
					SafeGeneralParameters ip;
			
					if (loader.projectFileName != "") {
						String projectFileName = simulationPath + "/" + loader.projectFileName;
			
						if (!Check.isFile(projectFileName)) {
								throw new Exception("Wrong project file name: " + projectFileName);			
						}
			
			
						script = C4Script.openProject(projectFileName);
						ip = (SafeGeneralParameters) script.getModel().getSettings();
						ip.setDataPath(simulationPath);
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
						String pldFileName;
						pldFileName = getFileName(simulationPath, ".pld");	
						if (pldFileName == "") {
							System.out.println("PLD FILE NOT FOUND in folder "+simulationPath);
						}
				
						pldFileName = simulationPath + "/" +pldFileName;

						ip = new SafeGeneralParameters(simulationPath, pldFileName);

						model = (SafeModel) script.getModel();
						
						model.setReStart(false);
			
						// fc-14.4.2017 moved these two lines here: init () must be called
						// only for a new C4Script and not in ReStart mode	
						script.init(ip, MemorizerFactory.createCompactMemorizer());

					}

					//DEBUGGING
					if (loader.debugMode == 1) model.setDebugMode(true);

					//EXPORT file name is search automatically with .out extension
					String exportFile = getFileName(simulationPath, ".out");
					if (!exportFile.equals("")) exportFile = simulationPath + "/" +exportFile;
					else {
						exportFile = getFileName(capsisDataPath, ".out");
						exportFile = capsisDataPath +   "/" + exportFile; 
						System.out.println("WARNING EXPORT.OUT read in folder "+capsisDataPath);					
					}

					model.loadExport(exportFile, outputPath, projectName);
					
					//IL 28-11-2017 
					//Weather file name is search automatically with .wth extension
					String weatherPath = getFileName(simulationPath, ".wth");
					if (weatherPath == "") {
						System.out.println("WTH FILE NOT FOUND in folder "+simulationPath);
					}
					weatherPath = simulationPath + "/" +weatherPath;
				
					//in case of lai forcing file 
					String laiPath = "";
					if (loader.laiFileName!= "") laiPath = simulationPath + "/" + loader.laiFileName;
					
					//copy session.txt
					String sessionOrigin = capsisDataPath + "/session.txt";
					String sessionCopy = outputPath + "/session.txt";
					Path monFichier = Paths.get (sessionOrigin);
					Path monFichierVerif = Paths.get (sessionCopy);
					Files.copy (monFichier, monFichierVerif, StandardCopyOption.REPLACE_EXISTING);
					
					script.getProject().setName(projectName);

					// evolution
					SafeEvolutionParameters ep = new SafeEvolutionParameters (loader, 
																			simulationPath, 
																			outputPath, 
																			weatherPath,
																			laiPath);
			
					// start from last step of previous simulation (if reload) 
					if (model.getReStart()) {
						step = model.getLastStep();
					}
					// start from new project root step
					else {
						step = (Step) script.getRoot();
					}
			
					SafeStand stand = (SafeStand) (step.getScene());

					model.loadWeather(stand.getPlot().getPlotSettings().latitude, 
							  stand.getPlot().getPlotSettings().elevation, 
							  ep.weatherPath, ep.simulationDateStart, ep.simulationDateEnd);


					model.initExport(stand);
					
					model.verifSimulation(stand, ip, ep, monFichierVerif);
			
					script.closeProject(script.getProject());

				} catch (Throwable e1) {
					System.out.println("Probleme loading simulation file "+e1);
					throw e1;			
				}
			}
		}

		StatusDispatcher.print("END of VERIFICATION");
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
