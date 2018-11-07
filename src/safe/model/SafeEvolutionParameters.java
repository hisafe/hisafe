package safe.model;

import java.io.File;
import java.util.Collection;
import java.util.List;
import java.util.ArrayList;

import capsis.kernel.EvolutionParameters;
import jeeb.lib.util.PathManager;
import safe.pgms.SafeSimulationLoader;
import safe.pgms.SafeRootCalibrationLoader;
import safe.pgms.SafeDbhCalibrationLoader;


/**
 * Simulation settings   
 * 
 * @author Isabelle Lecomte - July 2002
 */
public class SafeEvolutionParameters implements EvolutionParameters {

	public static final int NB_SIMULATION_MAX = 100; // max number of simulation
	public static final int NB_PROFIL_MAX = 20; // max number of export profiles

	// EXPORTATION
	public String simulationDir;
	public String exportDir;
	public String catalogExportDir;
	public List<String> profileNames;    	// export profile names
	public int[][] exportFrequencies; 	 	// export frequencies in days for all profiles and all simulations
	public boolean exportStiFiles;			//export STICS sti files 

	// TREE PLANTING 
	public List<Integer> treePlantingYears; 	// tree planting years
	public List<Integer> treePlantingDays;  	// tree planting days
	

	// TREE PRUNING 
	public List<Integer> treePruningYears; 		// tree pruning years
	public List<Integer> treePruningDays; 		// tree pruning days
	public List<Double> treePruningProp; 		// tree pruning proportion
	public List<Double> treePruningMaxHeight; 	// tree pruning max height in meters
	public List<Double> treeDensityReduction; 	// tree density reduction fraction 

	// TREE THINNING 
	public List<Integer> treeThinningIds; 		// tree ids for thinning
	public List<Integer> treeThinningYears; 	// tree thinning years
	public List<Integer> treeThinningDays; 		// tree thinning days

	// TREE ROOT PRUNING 
	public List<Integer> treeRootPruningYears; 	// tree root pruning years
	public List<Integer> treeRootPruningDays; 	// tree root pruning days
	public List<Double> treeRootPruningDistance; // tree root pruning distance
	public List<Double> treeRootPruningDepth; 	// tree root pruning depth

	
	// TREE POLLARDING 
	public int treePollardingMethod;  			//1=regular 2=triggered by the side of tree
	public double treePollardingThreshold;  	//tree pollarding threshold (method=2)	
	public List<Integer> treePollardingYears; 	// tree Pollarding years
	public List<Integer> treePollardingDays; 	// tree Pollarding days
	public List<Double> treePollardingHeight; 	// tree Pollarding height in meters
	public List<Double> treePollardingCanopyLeft; // tree Pollarding canopy left in meters
	

	//CARBON AND NITROGEN LITTER INCORPORATION il-27.03.2018
	public boolean treesLeafLitterIncorporated = true; 			    // tree leaves senescent would be incorporated  in soil
	public boolean treesRootLitterIncorporated = true; 		// tree root senescent would be incorporated in soil
	public boolean treesDeepRootLitterIncorporated = true; 	// tree root senescent would be incorporated in DEEP soil
	
	// SIMULATION PARAMETERS
	public int nbSimulations; 			// total number of simulation
	public int simulationYearStart; 	// simulation starting year
	public int simulationDayStart; 		// simulation starting day (julian)
	public int currentDay;
	public boolean firstSimulation;

	// -------------------------------------------------------------------------------------------------------------------------------------------

	public int[] simulationNbrDays; 	// nbr days
	public double[] treeCropDistance; 	// distance between tree lines and crop (m)
	public double[] treeCropRadius; 	// radius between tree lines and crop (m)
	public String weatherFile; 			// weather data file

	// Crop species and interventions
	public String[] mainCropSpeciesFile; 		// main crop species  file name
	public String[] interCropSpeciesFile; 		// inter crop species file name
	public String[] mainCropInterventionFile; 	// main crop itk file name
	public String[] interCropInterventionFile; 	// inter crop itk file name


	//TORIC symetry parameters

	public static final int TORIC_X_POS = 1000; 				// toric symetry on x axis positive (number of tours allowed)
	public static final int TORIC_X_NEG = 1000; 				// toric symetry on y axis negative (number of tours allowed)
	public static final int TORIC_Y_POS = 1000; 				// toric symetry on x axis positive (number of tours allowed)
	public static final int TORIC_Y_NEG = 1000; 				// toric symetry on y axis negative (number of tours allowed)

	public int toricXp;
	public int toricXn;
	public int toricYp;
	public int toricYn;

	//CALIBRATION (root parameters)
	public List<Integer> calibrationRootYear; 			//year to check
	public List<Integer> calibrationRootMonth; 			//month to check
	public List<Integer> calibrationRootDay; 			//day to check
	public List<Integer> calibrationRootIdTree; 		//tree ID
	public List<Double>  calibrationRootDistance4m; 	//tree root distance to check à 4m (0=no,1=yes)
	public List<Double>  calibrationRootDistance6m; 	//tree root distance to check à 6m (0=no,1=yes)
	public List<Double>  calibrationRootTotal; 			//total scene colonised by tree roots (0=no,1=yes)
	public boolean simulationRootStop;					//true if calibration is aborted because of roots
	public double maxRootDistanceOnCropLineStop;		//max tree root distance when calibration is aborted
	public int indexRootStop;							//index of aborded simulation 
	
	//CALIBRATION (DBH parameters)
	public List<Integer> calibrationDbhYear; 		//year to check
	public List<Integer> calibrationDbhMonth; 		//month to check
	public List<Integer> calibrationDbhDay; 		//day to check
	public List<Integer> calibrationDbhIdTree; 		//tree ID
	public List<Double>  calibrationDbhMin; 		//tree dbh min
	public List<Double>  calibrationDbhMax; 		//tree dbh max
	public boolean simulationDbhStop;				//true if calibration is aborted because of dbh
	public double dbhStop;							//dbh when calibration is aborted
	public int indexDbhStop;						//index of aborded simulation 

	/**
	 * Constructor.
	 */
	public SafeEvolutionParameters(
									SafeInitialParameters safeSettings,
									SafeInitialValues initialValues, 
									SafeSimulationLoader loader,
									SafeRootCalibrationLoader rootCalib,
									SafeDbhCalibrationLoader dbhCalib,
									String simulationDir, 
									String exportDir, 
									String weatherFile)  {

		String capsisRoot = PathManager.getInstallDir(); // e.g.: C:/capsis4
		
		this.weatherFile = weatherFile;
		this.simulationNbrDays = new int[NB_SIMULATION_MAX];
		this.mainCropSpeciesFile = new String[NB_SIMULATION_MAX];
		this.interCropSpeciesFile = new String[NB_SIMULATION_MAX];
		this.treeCropDistance = new double[NB_SIMULATION_MAX];
		this.treeCropRadius = new double[NB_SIMULATION_MAX];
		this.mainCropInterventionFile = new String[NB_SIMULATION_MAX];
		this.interCropInterventionFile = new String[NB_SIMULATION_MAX];
		this.toricXp = TORIC_X_POS; 
		this.toricXn = TORIC_X_NEG;
		this.toricYp = TORIC_Y_POS;
		this.toricYn = TORIC_Y_NEG;
		
		//BATCH MODE
		if (loader != null) {
				this.nbSimulations = loader.nbSimulations;
				this.simulationYearStart = loader.simulationYearStart;
				this.simulationDayStart = loader.simulationDayStart;
				this.simulationDir = simulationDir; 
				this.exportDir = exportDir;
				
				//on cherche en priorité les fichiers profil export dans le répertoire de simulation
				//s'il n'existe pas on cherche ensuite dans le répertoire safe/data/simSettings	
				//en dernier lieu on prend le réperoire pas defaut safe/data/exportParameters
				
				this.catalogExportDir = simulationDir + File.separator + "exportParameters"; 
				File f = new File(this.catalogExportDir);
				if (!f.exists()) {
					this.catalogExportDir = capsisRoot + File.separator + "data" + File.separator + "safe" 
							+ File.separator + "simSettings" 
							+ File.separator + safeSettings.projectName
							+ File.separator + "exportParameters";						
					File f2 = new File(this.catalogExportDir);
					if (!f2.exists()) {						
						this.catalogExportDir = capsisRoot + File.separator + "data" 
								+ File.separator + "safe" 
								+ File.separator + "exportParameters";	
					}
				}
				
				this.profileNames = loader.profileNames;
				this.exportStiFiles = loader.exportStiFiles;
				this.treePlantingYears = loader.treePlantingYears;
				this.treePlantingDays = loader.treePlantingDays;
				this.treePruningYears = loader.treePruningYears;
				this.treePruningProp = loader.treePruningProp;
				this.treePruningMaxHeight = loader.treePruningMaxHeight;
				this.treeDensityReduction = loader.treeDensityReduction;
				this.treePruningDays = loader.treePruningDays;
				this.treeThinningIds = loader.treeThinningIds;
				this.treeThinningYears = loader.treeThinningYears;
				this.treeThinningDays = loader.treeThinningDays;
				this.treeRootPruningYears = loader.treeRootPruningYears;
				this.treeRootPruningDays = loader.treeRootPruningDays;
				this.treeRootPruningDistance = loader.treeRootPruningDistance;
				this.treeRootPruningDepth = loader.treeRootPruningDepth;
				this.treePollardingMethod = loader.treePollardingMethod;
				this.treePollardingThreshold = loader.treePollardingThreshold;
				this.treePollardingYears = loader.treePollardingYears;
				this.treePollardingDays = loader.treePollardingDays;
				this.treePollardingHeight = loader.treePollardingHeight;
				this.treePollardingCanopyLeft = loader.treePollardingCanopyLeft;
				this.exportFrequencies = new int[NB_PROFIL_MAX][NB_SIMULATION_MAX];

				//TORIC SYMETRIE parameter set
				this.toricXp = loader.toricXp * TORIC_X_POS;
				this.toricXn = loader.toricXn * TORIC_X_NEG;
				this.toricYp = loader.toricYp * TORIC_Y_POS;
				this.toricYn = loader.toricYn * TORIC_Y_NEG;
						
				//CARBON AND NITROGEN LITTER INCORPORATION il-27.03.2018
				this.treesLeafLitterIncorporated = loader.treesLeafLitterIncorporated; 
				this.treesRootLitterIncorporated = loader.treesRootLitterIncorporated; 
				this.treesDeepRootLitterIncorporated = loader.treesDeepRootLitterIncorporated; 
 
				//CALIBRATION (root parameters)
				if (rootCalib != null) {
					this.calibrationRootYear = rootCalib.calibrationRootYear; 				//year to check
					this.calibrationRootMonth = rootCalib.calibrationRootMonth; 			//month to check
					this.calibrationRootDay = rootCalib.calibrationRootDay; 				//day to check
					this.calibrationRootIdTree = rootCalib.calibrationRootIdTree; 			//tree id to check 
					this.calibrationRootDistance4m = rootCalib.calibrationRootDistance4m; 	//tree root distance to check à 4m (0=no,1=yes)
					this.calibrationRootDistance6m = rootCalib.calibrationRootDistance6m; 	//tree root distance to check à 6m (0=no,1=yes)
					this.calibrationRootTotal = rootCalib.calibrationRootTotal; 			//tree root on all scene (0=no,1=yes)
				}
				
				//CALIBRATION (dbh parameters)
				if (dbhCalib != null) {
					this.calibrationDbhYear = dbhCalib.calibrationDbhYear; 			//year to check
					this.calibrationDbhMonth = dbhCalib.calibrationDbhMonth; 		//month to check
					this.calibrationDbhDay = dbhCalib.calibrationDbhDay; 			//day to check
					this.calibrationDbhIdTree = dbhCalib.calibrationDbhIdTree; 		//tree id to check 
					this.calibrationDbhMin = dbhCalib.calibrationDbhMin; 			//tree dbh min
					this.calibrationDbhMax = dbhCalib.calibrationDbhMax; 			//tree dbh max
				}				
		}
		
		//INTERACTIVE MODE
		else {
			
			this.nbSimulations = 1;
			this.simulationYearStart = 0;	
			this.simulationDayStart = 1;	
			this.simulationNbrDays[0] = 365;
			this.exportDir = capsisRoot + File.separator + "data" + File.separator + "safe" + File.separator
					+ "output";				

			this.treePlantingYears = new ArrayList<Integer>();
			this.treePlantingDays = new ArrayList<Integer>();
			
			for (int i=0; i<safeSettings.nbTrees; i++) {
				this.treePlantingYears.add(this.simulationYearStart);			
				this.treePlantingDays.add(this.simulationDayStart);
			}

			this.treesLeafLitterIncorporated = true; 
			this.treesRootLitterIncorporated = true; 		
			this.treesDeepRootLitterIncorporated = true; 	

			this.exportStiFiles = false;
		}

		this.currentDay = 0;
		this.treeCropDistance[0] = 0.5;
		this.treeCropRadius[0] = 0;
		this.firstSimulation = true;
	}
	/**
	 * Initialize all simulations parameters with .sim file information
	 * For BATCH MODE only
	*/
	public void propagateRotationSettings(SafeInitialParameters	safeSettings, String dir,  SafeSimulationLoader loader, int simulationIndex)  throws Exception {

		//on cherche en priorité les fichiers cropSpecies dans le répertoire de simulation
		String cropSpeciesDir = dir;
		String repName = dir + "/cropSpecies/"; 
		File f = new File(repName);
		//s'il n'existe pas on cherche ensuite dans le répertoire safe/data/simSettings
		if (!f.exists()) {
			cropSpeciesDir =  safeSettings.dataOriginalPath + "/simSettings/"+safeSettings.projectName;  
			repName = cropSpeciesDir + "/cropSpecies/"; 
			System.out.println(cropSpeciesDir);
			f = new File(repName);
			//en dernier lieu on prend le réperoire pas defaut safe/data
			if (!f.exists()) {
				cropSpeciesDir = safeSettings.dataOriginalPath;  
				System.out.println("WARNING cropSpecies read in folder "+cropSpeciesDir);
			}
			else {
				System.out.println("WARNING cropSpecies read in folder "+cropSpeciesDir);
			}
		}
		
		//on cherche en priorité les fichiers cropInterventions dans le répertoire de simulation
		String cropInterventionsDir = dir;
		String repName2 = dir + "/cropInterventions/"; 
		File f2 = new File(repName2);
		//s'il n'existe pas on cherche ensuite dans le répertoire safe/data/simSettings		
		if (!f2.exists()) {
			cropInterventionsDir =  safeSettings.dataOriginalPath + "/simSettings/"+safeSettings.projectName;  
			repName2 = cropInterventionsDir + "/cropInterventions/"; 
			f2 = new File(repName2);		
			//en dernier lieu on prend le réperoire pas defaut safe/data
			if (!f2.exists()) {
				cropInterventionsDir = safeSettings.dataOriginalPath;  
				System.out.println("WARNING cropInterventions read in folder "+cropInterventionsDir);
			}
			else {
				System.out.println("WARNING cropInterventions read in folder "+cropInterventionsDir);
			}
		}
		
		
		//**************** MAIN CROP SPECIES*********************************/
		// read the maincrop patern (crop1,crop2,crop3...)
		String mc[] = loader.mainCropSpecies.split(",");
		int nbRotation = 1;
		int nbMainCrop = 0;
		int indexMainCrop = simulationIndex;
		int indexLastCrop = simulationIndex+nbSimulations;
		

		for (int i = 0; i < mc.length; i++) {
			String mainCropName = mc[i];
			
			if (mc[i].contains("(") && mc[i].contains(")")) {
				int index1 = mc[i].indexOf("(");
				int index2 = mc[i].indexOf(")");
				nbRotation = Integer.parseInt(mc[i].substring(index1 + 1, index2));
				mainCropName = mc[i].substring(0, index1);
			}

			String mainCropSpeciesFileName = cropSpeciesDir + "/cropSpecies/" + mainCropName;
			System.out.println("mainCropSpeciesFileName="+mainCropSpeciesFileName);
			for (int j = 0; j < nbRotation; j++) {
				mainCropSpeciesFile[indexMainCrop] = mainCropSpeciesFileName;
				nbMainCrop++;
				indexMainCrop++;
			}

		}

		// propagation of maincrop patern for each yeay
		int indexCible = indexMainCrop;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbMainCrop; i++) {
				mainCropSpeciesFile[indexCible] = mainCropSpeciesFile[indexSource]; // crop species
				indexSource++;
				indexCible++;
			}

		}	
		//**************** NBR of DAYS ********************************/		
		nbRotation = 1;
		int nbrDays = 0;
		int nbNbrDays = 0;
		int indexDay = simulationIndex;
		String md[] = loader.simulationNbrDays.split(",");
		
		for (int i = 0; i < md.length; i++) {
			String stringDay = md[i];			
			if (md[i].contains("(") && md[i].contains(")")) {
				int index1 = md[i].indexOf("(");
				int index2 = md[i].indexOf(")");
				nbRotation = Integer.parseInt(md[i].substring(index1 + 1, index2));
				stringDay = md[i].substring(0, index1);
			}

			nbrDays = Integer.parseInt(stringDay);
			for (int j = 0; j < nbRotation; j++) {
				simulationNbrDays[indexDay] = nbrDays;
				indexDay++;
				nbNbrDays++;
			}
		}
		

		// propagation of nbrDays patern for each yeay
		indexCible = indexDay;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbNbrDays; i++) {
				simulationNbrDays[indexCible] = simulationNbrDays[indexSource];
				indexSource++;
				indexCible++;
			}
		}

		
		//**************** MAIN CROP ITK *********************************/
		// read the itk patern (itk1,itk2,itk3...)
		
		mc = loader.mainCropItk.split(",");
		nbMainCrop = 0;
		indexMainCrop = simulationIndex;		

		for (int i = 0; i < mc.length; i++) {
			String mainCropName = mc[i];
			int nb = 1;

			if (mc[i].contains("(") && mc[i].contains(")")) {
				int index1 = mc[i].indexOf("(");
				int index2 = mc[i].indexOf(")");
				nb = Integer.parseInt(mc[i].substring(index1 + 1, index2));
				mainCropName = mc[i].substring(0, index1);
			}

			String mainCropInterventionFileName = cropInterventionsDir + "/cropInterventions/" + mainCropName;
			for (int j = 0; j < nb; j++) {
				mainCropInterventionFile[indexMainCrop] = mainCropInterventionFileName;
				nbMainCrop++;
				indexMainCrop++;
			}

		}
		
		// propagation of itk patern for each yeay
		indexCible = indexMainCrop;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbMainCrop; i++) {
				mainCropInterventionFile[indexCible] = mainCropInterventionFile[indexSource]; // itk
				indexSource++;
				indexCible++;
			}
		}

		//**************** INTER CROP SPECIES *********************************/
		// read intercrop patern (iner1,inter2,inter3...)	

		String ic[] = loader.interCropSpecies.split(",");

		int nbInterCrop = 0;
		int indexInterCrop = simulationIndex;	
		for (int i = 0; i < ic.length; i++) {

			String interCropName = ic[i];
			int nb = 1;
			if (ic[i].contains("(") && ic[i].contains(")")) {
				int index1 = ic[i].indexOf("(");
				int index2 = ic[i].indexOf(")");
				nb = Integer.parseInt(ic[i].substring(index1 + 1, index2));
				interCropName = ic[i].substring(0, index1);
			}

			String interCropSpeciesFileName = cropSpeciesDir + "/cropSpecies/" + interCropName;

			for (int j = 0; j < nb; j++) {
				interCropSpeciesFile[indexInterCrop] = interCropSpeciesFileName; 
				nbInterCrop++;
				indexInterCrop++;
			}
		}

		
		// propagation of intercrop patern for each yeay
		indexCible = indexInterCrop;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbInterCrop; i++) {
				interCropSpeciesFile[indexCible] = interCropSpeciesFile[indexSource]; // crop species
				indexSource++;
				indexCible++;
			}
		}
		
		//**************** INTER CROP ITK *********************************/
		// read intercrop patern (iner1,inter2,inter3...)

		ic = loader.interCropItk.split(",");

		nbInterCrop = 0;
		indexInterCrop = simulationIndex;	
		for (int i = 0; i < ic.length; i++) {

			String interCropName = ic[i];
			int nb = 1;
			if (ic[i].contains("(") && ic[i].contains(")")) {
				int index1 = ic[i].indexOf("(");
				int index2 = ic[i].indexOf(")");
				nb = Integer.parseInt(ic[i].substring(index1 + 1, index2));
				interCropName = ic[i].substring(0, index1);

			}

			String interCropInterventionFileName = cropInterventionsDir + "/cropInterventions/" + interCropName;

			for (int j = 0; j < nb; j++) {
				interCropInterventionFile[indexInterCrop] = interCropInterventionFileName; 
				nbInterCrop++;
				indexInterCrop++;

			}
		}

		// propagation of intercrop patern for each yeay
		indexCible = indexInterCrop;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbInterCrop; i++) {

				interCropInterventionFile[indexCible] = interCropInterventionFile[indexSource]; // crop species
				indexSource++;
				indexCible++;
			}
		}
			
		//**************** TREE CROP DISTANCE *********************************/
		// read the tree crop distance (distance1,distance2,distance3...)
		String td[] = loader.treeCropDistance.split(",");
		int nbDistance = 0;
		int indexDistance = simulationIndex;

		for (int i = 0; i < td.length; i++) {
			String distance = td[i];
			int nb = 1;

			if (td[i].contains("(") && td[i].contains(")")) {
				int index1 = td[i].indexOf("(");
				int index2 = td[i].indexOf(")");
				nb = Integer.parseInt(td[i].substring(index1 + 1, index2));
				distance = td[i].substring(0, index1);
			}

			for (int j = 0; j < nb; j++) {
				treeCropDistance[indexDistance] = Double.parseDouble(distance);
				nbDistance++;
				indexDistance++;
			}

		}
	
		// propagation of TREE CROP DISTANCE patern for each yeay
		indexCible = indexDistance;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbDistance; i++) {

				treeCropDistance[indexCible] = treeCropDistance[indexSource]; // crop species
				indexSource++;
				indexCible++;
			}
		}

		//**************** WEEDED AREA RADIUS *********************************/
		// read the weeded area radius (radius1,radius2,radius3...)
		String wa[] = loader.treeCropRadius.split(",");
		int indexRadius = simulationIndex;
		int nbRadius = 0;

		for (int i = 0; i < wa.length; i++) {
			String radius = wa[i];
			int nb = 1;

			if (wa[i].contains("(") && wa[i].contains(")")) {
				int index1 = wa[i].indexOf("(");
				int index2 = wa[i].indexOf(")");
				nb = Integer.parseInt(wa[i].substring(index1 + 1, index2));
				radius = wa[i].substring(0, index1);
			}

			for (int j = 0; j < nb; j++) {
				treeCropRadius[indexRadius] = Double.parseDouble(radius);
				nbRadius++;
				indexRadius++;
			}

		}
		
		// propagation of  weeded area radius  patern for each yeay
		indexCible = indexRadius;
		
		while (indexCible < indexLastCrop) {
			int indexSource = simulationIndex;
			for (int i = 0; i < nbRadius; i++) {

				treeCropRadius[indexCible] = treeCropRadius[indexSource]; // crop species
				indexSource++;
				indexCible++;
			}
		}

		//****EXPORT FREQUENCIES *********************************/
		// read the  export frequencies
		if (loader.exportFrequencies != null) {
			for (int nbexport = 0; nbexport < loader.exportFrequencies.size(); nbexport++) {
				String export = loader.exportFrequencies.get(nbexport);

				if (export.contains(")")) {
					int nb = 0;
					if (export.endsWith(")")) {
						export = export.substring(0, export.length()-1);
					}
					String[] st = export.split("\\)");
					for (int j = 0; j < st.length; j++) {
						String value = st[j];
						int repet = 1; 
						if (st[j].contains("(")) {
							String[] st2 = st[j].split("\\(");
							value = st2[0];
							repet = Integer.parseInt(st2[1]);
						}										
						for (int r=0;r<repet;r++) {
							exportFrequencies[nbexport][nb] = Integer.parseInt(value);
							nb++;
						}
					}
				}
				else {
					exportFrequencies[nbexport][0] = Integer.parseInt(export);
				}

				//on rempli pour le nombre de simul MAX 
				for (int i = 1; i < NB_SIMULATION_MAX; i++) {
					if (exportFrequencies[nbexport][i] == 0) {
						exportFrequencies[nbexport][i] = exportFrequencies[nbexport][i - 1];
					}
				}
			}
		}
	}
}
