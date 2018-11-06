package safe.pgms;

import java.io.File;
import java.io.FileWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;
import java.text.SimpleDateFormat;
import java.util.GregorianCalendar;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import capsis.app.C4Script;
import capsis.extension.memorizer.CompactMemorizer;
import capsis.kernel.Engine;
import capsis.kernel.MemorizerFactory;
import jeeb.lib.util.PathManager;
import capsis.kernel.Step;
import forceps.myscripts.DivProdXavier;
import jeeb.lib.util.Check;
import jeeb.lib.util.ProgressDispatcher;
import jeeb.lib.util.StatusDispatcher;
import safe.model.*;



/**
* Script for paramaters calibration
 * 
 * @author Isabelle LECOMTE - 13.04.2018
 */
public class ScriptCalib {

	// The directory where all usefull files have to be stored
	private String workingDir;

	// All exports go to the output dir
	private String outputDir;

	public static void main(String[] args) throws Exception {
		new ScriptCalib(args);
	}

	public ScriptCalib (String[] args) throws Exception {

		long timeStart = System.currentTimeMillis();
		long timeEnd = 0;
		SafeModel model;
		Step step = null;
		int simulationIdex;

		
		//CAPSIS original path for Plot description
		String capsisDataPath = PathManager.getInstallDir () + "/data/safe";
		


		// Check the parameters
		// args[0] is the name of this script (useless)
		// args[1] is the name of simulation file to be processed
		if (args == null || args.length != 3) {
			throw new Exception("One parameter needed: missing simulation file name or reference folder name");
		}

		// Check the simulation file
		String simulationFileName = args[1];
		if (!Check.isFile(simulationFileName)) {
			throw new Exception("Wrong simulation file name: " + simulationFileName);
		}

		// Check the reference folder name
		String referenceName = args[2];
		String capsisSimulationPath = capsisDataPath + "/simSettings/" + referenceName; 
		File f = new File(capsisSimulationPath);
		if (!f.isDirectory()) {
			throw new Exception("Wrong reference folder name: " + capsisSimulationPath);
		}
		
		// project name
		String projectName = new File(simulationFileName).getName();
		projectName = projectName.replace(".sim", "");

		// Set the workingDir
		workingDir = new File(simulationFileName).getParentFile().getAbsolutePath();

		// outputDir
		outputDir = workingDir + "/output-" + projectName;
		File mydir = new File(outputDir);
 	    mydir.delete();
		mydir.mkdir();

		try{
			SafeSimulationLoader loader = new SafeSimulationLoader(simulationFileName);
			loader.load();
		

			
			// Open an existing project with projectFileName
			C4Script script;
			SafeInitialParameters ip;
	
			if (loader.projectFileName != "") {
				String projectFileName = workingDir + "/" + loader.projectFileName;
	
				if (!Check.isFile(projectFileName)) {
					
					projectFileName = capsisSimulationPath + "/" + loader.projectFileName;
					if (!Check.isFile(projectFileName)) {
						throw new Exception("Wrong project file name: " + projectFileName);
					}
				}
	
	
				script = C4Script.openProject(projectFileName);
				ip = (SafeInitialParameters) script.getModel().getSettings();
	
				model = (SafeModel) script.getModel();
	
				model.setReStart(true);
	
				// fc-14.4.2017 set explicitly a compact memorizer on the loaded
				// project, init() must not be called on a loaded project
				// NOTE: 'projet' is a C4Script, the variable may be renamed in
				// 'script' to write below script.getProject()
				script.setMemorizer(script.getProject(), MemorizerFactory.createCompactMemorizer());
				// fc-14.4.2017
				
				simulationIdex = model.getSimulationIndex();
		
			}
			// Creating a new project
			else {
				script = new C4Script("safe");
	
				
				//IL 28-11-2017 
				//Plot file name is search automatically with .pld extension
				String pldFileName = getFileName(workingDir, ".pld");
	
				if (!pldFileName.equals("")) pldFileName = workingDir + "/" +pldFileName;
				else {
					
					pldFileName = getFileName(capsisSimulationPath, ".pld");
					if (!pldFileName.equals("")) pldFileName = capsisSimulationPath + "/" +pldFileName;
					else {
						pldFileName = capsisDataPath + "/plotDescription/" + projectName+ ".pld"; 
					}
				}
				
				System.out.println("pldFileName="+pldFileName);
				
				ip = new SafeInitialParameters(workingDir, referenceName, pldFileName);
	
				model = (SafeModel) script.getModel();
	
				model.setReStart(false);
	
				// fc-14.4.2017 moved these two lines here: init () must be called
				// only for a new C4Script and not in ReStart mode
	
				script.init(ip, MemorizerFactory.createCompactMemorizer());
				//JUST FOR TESTING !!!!!
				//script.init(ip, MemorizerFactory.createDefaultMemorizer());
				
	
				// fc-14.4.2017
				
				simulationIdex = 0;
				
			}

			//IL 28-11-2017 
			//Weather file name is search automatically with .wth extension
			String weatherFile = getFileName(workingDir, ".wth");
			
			if (!weatherFile.equals("")) weatherFile = workingDir + "/" +weatherFile;
			else {
				weatherFile = getFileName(capsisSimulationPath, ".wth");
				if (!weatherFile.equals("")) weatherFile = capsisSimulationPath + "/" +weatherFile;
				else {
					weatherFile = getFileName(capsisDataPath, ".wth");
					weatherFile = capsisDataPath + "/weather/" + weatherFile+ ".wth"; 
				}
			}
			
			System.out.println("weatherFile="+weatherFile);
			
			//copy session.txt
			
			String sessionOrigin = capsisDataPath + "/session.txt";
			String sessionCopy = outputDir + "/session.txt";
			Path monFichier = Paths.get (sessionOrigin);
			Path monFichierCopie = Paths.get (sessionCopy);
			Path file = Files.copy (monFichier, monFichierCopie, StandardCopyOption.REPLACE_EXISTING);
			
			//*******************************************************************************************
			//IL 17-04-2018
			//Loading ROOT calibration parameters 
			//*******************************************************************************************
			String rootCalibrationFileName = getFileName(workingDir, ".rootcal");
			SafeRootCalibrationLoader rootCalib = null; 
			if (!rootCalibrationFileName.equals("")) rootCalibrationFileName = workingDir + "/" +rootCalibrationFileName;
			else {
				rootCalibrationFileName = getFileName(capsisSimulationPath, ".rootcal");
				if (!rootCalibrationFileName.equals("")) rootCalibrationFileName = capsisSimulationPath + "/" +rootCalibrationFileName;
			}
			
			System.out.println("ROOT calibrationFileName="+rootCalibrationFileName);
			if (!rootCalibrationFileName.equals("")) {
				rootCalib = new SafeRootCalibrationLoader(rootCalibrationFileName);
				rootCalib.load();
			}
			
			//*******************************************************************************************
			//IL 11-07-2018
			//Loading DBH calibration parameters 
			//*******************************************************************************************
			String dbhCalibrationFileName = getFileName(workingDir, ".dbhcal");
			SafeDbhCalibrationLoader dbhCalib = null; 
			if (!dbhCalibrationFileName.equals("")) dbhCalibrationFileName = workingDir + "/" +dbhCalibrationFileName;
			else {
				dbhCalibrationFileName = getFileName(capsisSimulationPath, ".dbhcal");
				if (!dbhCalibrationFileName.equals("")) dbhCalibrationFileName = capsisSimulationPath + "/" +dbhCalibrationFileName;
			}
			
			System.out.println("DBH CalibrationFileName="+dbhCalibrationFileName);
			if (!dbhCalibrationFileName.equals("")) {
				dbhCalib = new SafeDbhCalibrationLoader(dbhCalibrationFileName);
				dbhCalib.load();	
			}
	
			// Project init
			// createCompactMemorizer: only the last step is kept in memory
	
			// fc-14.4.2017 removed the line below: init () must be called
			// only for a new C4Script and not in ReStart mode, see upper
			// projet.init(ip, MemorizerFactory.createCompactMemorizer());
			// fc-14.4.2017
			
			script.getProject().setName(projectName);
	
			model.setBatchMode(true);
			model.setCalibrationMode(true);
			model.razFileNameMap();
	
			SafeInitialParameters safeSettings = (SafeInitialParameters) model.getSettings();
			SafeInitialValues initialValues = safeSettings.initialValues;
	
			// evolution
			SafeEvolutionParameters ep = new SafeEvolutionParameters (safeSettings, 
																		initialValues, 
																		loader, 
																		rootCalib, 
																		dbhCalib,
																		workingDir, 
																		outputDir, 
																		weatherFile);
	
			
			// Propagation of the parameter structure for all rotations
			ep.propagateRotationSettings(safeSettings, workingDir, loader, simulationIdex);
	
			// on redemarre depuis la derniere etape du projet reouvert
			if (model.getReStart()) {
				step = model.getLastStep();
			}
			// on part de l'étape racine du nouveau projet
			else {
				step = (Step) script.getRoot();
			}
	
			//if calibration mode we have some tests to stop the simulation 
			ep.simulationRootStop = false;
			ep.simulationDbhStop = false;
			
			ep.firstSimulation = true;
			for (int r = 0; ((r < ep.nbSimulations) && (!ep.simulationRootStop) && (!ep.simulationDbhStop)  ); r++) {			
				step = runSimulation(model, script, step, ep, r);
				ep.firstSimulation = false;	
			}
	
			if (ep.simulationRootStop) {
				System.out.println("ABORD SIMULATION (ROOTS) !!!!");
				
				try
				{
				File fout = new File(outputDir+"/root.abord");
	
					fout.createNewFile(); // Cette fonction doit être appelée au sein d'un bloc TRY
					FileWriter fw = new FileWriter(fout);
					String message = projectName
							+";"+ep.calibrationRootYear.get(ep.indexRootStop)
							+";"+ep.calibrationRootMonth.get(ep.indexRootStop)
							+";"+ep.calibrationRootDay.get(ep.indexRootStop)
							+";"+ep.calibrationRootIdTree.get(ep.indexRootStop)
							+";"+ep.calibrationRootDistance4m.get(ep.indexRootStop)
							+";"+ep.calibrationRootDistance6m.get(ep.indexRootStop)
							+";"+ep.calibrationRootTotal.get(ep.indexRootStop)
							+";"+ep.maxRootDistanceOnCropLineStop;
					fw.write(message);
					fw.flush();
					fw.close();
	
				}
				catch (Exception e1)
				{
					System.out.println("Impossible de créer le fichier root.abord");
				}
	
			}
			
			
			if (ep.simulationDbhStop) {
				System.out.println("ABORD SIMULATION (DBH) !!!!");
				
				try
				{
				File fout = new File(outputDir+"/dbh.abord");
	
					fout.createNewFile(); // Cette fonction doit être appelée au sein d'un bloc TRY
					FileWriter fw = new FileWriter(fout);
					String message = projectName
							+";"+ep.calibrationDbhYear.get(ep.indexDbhStop)
							+";"+ep.calibrationDbhMonth.get(ep.indexDbhStop)
							+";"+ep.calibrationDbhDay.get(ep.indexDbhStop)
							+";"+ep.calibrationDbhIdTree.get(ep.indexDbhStop)
							+";"+ep.calibrationDbhMin.get(ep.indexDbhStop)
							+";"+ep.calibrationDbhMax.get(ep.indexDbhStop)
							+";"+ep.dbhStop;
					fw.write(message);
					fw.flush();
					fw.close();
	
				}
				catch (Exception e1)
				{
					System.out.println("Impossible de créer le fichier dbh.abord");
				}
	
			}
			

			//ecriture dans session.txt
			timeEnd = System.currentTimeMillis() - timeStart;
			SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss",Locale.US);
			GregorianCalendar calendar = new GregorianCalendar(TimeZone.getTimeZone("US/Central"));
			calendar.setTimeInMillis(timeStart);

			
			String start = System.getProperty("line.separator")+"Start of simulation = " + sdf.format(calendar.getTime());
			String end = System.getProperty("line.separator")+"Duration of simulation in seconds = " + timeEnd/1000;
			System.out.println(end);

			Files.write(monFichierCopie, start.getBytes(), StandardOpenOption.APPEND);
			Files.write(monFichierCopie, end.getBytes(), StandardOpenOption.APPEND);
			

			// save
			//	if ((script.getProject().getMemorizer() instanceof CompactMemorizer) && (loader.saveProjectOption == 1)) {
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
		
		StatusDispatcher.print("END of Simulation");
		
	}

	private Step runSimulation(SafeModel model, C4Script script, Step step, SafeEvolutionParameters ep, int simulationId)
			throws Exception {

		ep.currentDay = ep.simulationDayStart;


		SafeStand stand = (SafeStand) (step.getScene());
		model.loadWeather(stand.getLatitude(), stand.getElevation(), ep.weatherFile);
		step = script.evolve(step, ep);

		if (step == null) {
			throw new Exception("ScriptGen: evolve () failed, see Log");
		}

		ep.simulationDayStart = ep.currentDay + 1;


		if (ep.simulationDayStart >= 365) {
			if (model.getMacroClimat().isLeapYear(ep.simulationYearStart)) {
				ep.simulationDayStart = ep.currentDay - 365;
			} else {
				ep.simulationDayStart = ep.currentDay - 364;
			}
			if (ep.simulationDayStart == 0)
				ep.simulationDayStart = 1;
			ep.simulationYearStart++;

		}

		
		return step;

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
						int index = fileName.indexOf(".");
						isGoodProfileExtension = (fileName.substring(index,fileNameLength)).equals(extension);	
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
