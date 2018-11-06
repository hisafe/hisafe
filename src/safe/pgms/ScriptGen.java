package safe.pgms;

import java.io.File;
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
 * A Capsis script, type 1 (preferred): (a) extends nothing particular, (b) with
 * a main method (can be launched directly with java), (c) uses an instance of
 * C4Script.
 * 
 * Two ways to launch it: (1) sh capsis.sh -p script
 * maddmodel.myscripts.SimpleScript (2) java -cp class:ext/*
 * maddmodel.myscripts.SimpleScript
 * 
 * @author F. de Coligny - 16.9.2010
 */
public class ScriptGen {

	// The directory where all usefull files have to be stored
	private String workingDir;

	// All exports go to the output dir
	private String outputDir;

	

	
	public static void main(String[] args) throws Exception {
		new ScriptGen(args);
	}

	public ScriptGen(String[] args) throws Exception {

		long timeStart = System.currentTimeMillis();
		long timeEnd = 0;
		SafeModel model;
		Step step = null;
		int simulationIdex;

		
		//CAPSIS original path for Plot description
		String capsisDataPath = PathManager.getInstallDir () + "/data/safe";
		String referenceName = "";
		String capsisSimulationPath = capsisDataPath + "/simSettings/" + referenceName; 
		

		// Check the parameters
		// args[0] is the name of this script (useless)
		// args[1] is the name of simulation file to be processed
		if (args == null || args.length < 2) {
			throw new Exception("One parameter needed: missing simulation file name or reference folder name");
		}

		// Check the simulation file
		String simulationFileName = args[1];
		if (!Check.isFile(simulationFileName)) {
			throw new Exception("Wrong simulation file name: " + simulationFileName);
		}

		// project name
		String projectName = new File(simulationFileName).getName();
		projectName = projectName.replace(".sim", "");
		
		// Check the reference folder name
		if (args.length == 3) {
			referenceName = args[2];
			capsisSimulationPath = capsisDataPath + "/simSettings/" + referenceName; 
			File f = new File(capsisSimulationPath);
			if (!f.isDirectory()) {
				throw new Exception("Wrong reference folder name: " + capsisSimulationPath);
			}
		}
		else {
			referenceName = projectName;
			capsisSimulationPath = capsisDataPath + "/simSettings/" + referenceName; 
		}
		


		// Set the workingDir
		workingDir = new File(simulationFileName).getParentFile().getAbsolutePath();

		// outputDir
		outputDir = workingDir + "/output-" + projectName;
		File mydir = new File(outputDir);
 	    mydir.delete();
		mydir.mkdir();


		try {
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

			//DEBUGGING
			if (loader.debugMode == 1) model.setDebugMode(true);

			//STICS REPORT
			if (loader.sticsReport == 1) model.setSticsReport(true);
			
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
			
			
			// Project init
			// createCompactMemorizer: only the last step is kept in memory
	
			// fc-14.4.2017 removed the line below: init () must be called
			// only for a new C4Script and not in ReStart mode, see upper
			// projet.init(ip, MemorizerFactory.createCompactMemorizer());
			// fc-14.4.2017
			
			script.getProject().setName(projectName);
			model.setBatchMode(true);
			model.setCalibrationMode(false);
			model.razFileNameMap();
	
			SafeInitialParameters safeSettings = (SafeInitialParameters) model.getSettings();
			SafeInitialValues initialValues = safeSettings.initialValues;
	
			// evolution
			SafeEvolutionParameters ep = new SafeEvolutionParameters (safeSettings, 
																		initialValues, 
																		loader, 
																		null, null,
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
	
			
			ep.firstSimulation = true;
			for (int r = 0; r < ep.nbSimulations; r++) {	
				System.out.println("******* Simulation " + (r+1) + "\n");
				step = runSimulation(model, script, step, ep, r);
				ep.firstSimulation = false;	
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
