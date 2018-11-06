package safe.pgms;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;

/**
 * A loader for an HiSAFE simulation.
 * 
 * @author : S.Roux, June 2009
 */
public class SafeSimulationLoader extends RecordSet {

	
	//REQUIRED
	public String projectFileName;				//project file name if restarted

	public int saveProjectOption; 				//1 project will be saved 
	public int debugMode;						//0=NO 1=debug traces
	public int sticsReport;						//0=NO 1=YES
	
	public int nbSimulations;					//number of simulation (years) 
	public int simulationYearStart;				//simulation Year Start
	public int simulationDayStart;				//simulation day Start (1-365)
	public String simulationNbrDays;		    //simulation day number (1-365)
	public String mainCropSpecies;				//crop species beside tree lines
	public String interCropSpecies;				//crop species under tree lines
	public String mainCropItk;					//crop ITK beside tree lines
	public String interCropItk;					//crop ITK under tree lines	
	public String treeCropDistance;				//distance between tree and crop 
	public String treeCropRadius;				//radius between tree and crop  


	//TREE PLANTING il-31.1.2018
	public List<Integer> treePlantingYears; 	// tree planting years
	public List<Integer> treePlantingDays; 		// tree planting days (DOY) 
	
	//EXPORTS
	public List<String>  profileNames;			//export profile names
	public List<String>  exportFrequencies;		//export frequencies in string
	public boolean exportStiFiles;				//export STICS sti files 
	
	//OPTIONALS
	//TREE PRUNING il-15.4.2015
	public List<Integer> treePruningYears; 		// tree pruning years
	public List<Double> treePruningProp; 		// tree pruning proportion
	public List<Double> treePruningMaxHeight; 	// tree pruning max height in meters
	public List<Integer> treePruningDays; 		// tree pruning days
	public List<Double> treeDensityReduction; 	// tree density reduction fraction 

	//TREE THINNING il-16.4.2015
	public List<Integer> treeThinningIds; 		// tree ids for thinning 
	public List<Integer> treeThinningYears; 	// tree thinning years
	public List<Integer> treeThinningDays; 		// tree thinning days
	
	//TREE ROOT PRUNING il-12.5.2015
	public List<Integer> treeRootPruningYears; 		// tree root pruning years
	public List<Integer> treeRootPruningDays;  		// tree root pruning  days
	public List<Double> treeRootPruningDistance;  	// tree root pruning distance
	public List<Double> treeRootPruningDepth;  		// tree root pruning depth
	
	//TREE POLLARDING il-27.09.2017
	public int treePollardingMethod;  				//1=regular 2=triggered by the side of tree
	public double treePollardingThreshold;  		//tree pollarding threshold (method=2)
	public List<Integer> treePollardingYears; 		// tree pollarding years (method=1)
	public List<Integer> treePollardingDays;  		// tree pollarding  days
	public List<Double> treePollardingHeight;  	    // tree pollarding height in meters
	public List<Double> treePollardingCanopyLeft;  	// tree pollarding canopy left in meters
	
	//CARBON AND NITROGEN LITTER INCORPORATION il-27.03.2018
	public boolean treesLeafLitterIncorporated = true; 			    // tree leaves senescent would be incorporated  in soil
	public boolean treesRootLitterIncorporated = true; 		// tree root senescent would be incorporated in soil
	public boolean treesDeepRootLitterIncorporated = true; 	// tree root senescent would be incorporated in DEEP soil


	// toric symetry activation
	public int toricXn; 
	public int toricXp;
	public int toricYn;
	public int toricYp;


	
	/**
	 * Constructor.
	 */
	public SafeSimulationLoader(String fileName) throws Exception {
		createRecordSet(fileName);
	}

	public void load() throws Exception {

		profileNames = new ArrayList<String>();
		exportFrequencies = new ArrayList<String>();
		treePlantingYears = new ArrayList<Integer>();
		treePlantingDays = new ArrayList<Integer>();
		treePruningYears = new ArrayList<Integer>();
		treePruningProp = new ArrayList<Double>();
		treeDensityReduction = new ArrayList<Double>();
		treePruningMaxHeight = new ArrayList<Double>();
		treePruningDays = new ArrayList<Integer>();
		treeThinningIds = new ArrayList<Integer>();
		treeThinningYears = new ArrayList<Integer>();
		treeThinningDays = new ArrayList<Integer>();
		treeRootPruningYears = new ArrayList<Integer>();
		treeRootPruningDays = new ArrayList<Integer>();
		treeRootPruningDistance = new ArrayList<Double>();
		treeRootPruningDepth = new ArrayList<Double>();
		treePollardingYears = new ArrayList<Integer>();
		treePollardingDays = new ArrayList<Integer>();
		treePollardingHeight = new ArrayList<Double>();
		treePollardingCanopyLeft = new ArrayList<Double>();
	

		
		Set<String> requiredParameters = new HashSet<>();
		requiredParameters.add("nbSimulations");
		requiredParameters.add("simulationYearStart");
		requiredParameters.add("simulationNbrDays");
		requiredParameters.add("simulationDayStart");
		requiredParameters.add("mainCropSpecies");
		requiredParameters.add("interCropSpecies");
		requiredParameters.add("mainCropItk");
		requiredParameters.add("interCropItk");		
		requiredParameters.add("treeCropDistance");
		requiredParameters.add("treeCropRadius");		
		requiredParameters.add("toricXp");
		requiredParameters.add("toricXn");
		requiredParameters.add("toricYp");
		requiredParameters.add("toricYn");
		
		requiredParameters.add("treesRootLitterIncorporated");
		requiredParameters.add("treesDeepRootLitterIncorporated");
		requiredParameters.add("treesLeafLitterIncorporated");
		
		// String capsisRoot = PathManager.getInstallDir (); // ex: c:/capsis4
		projectFileName = "";

		exportStiFiles = false;
		
		debugMode = 0;
		
		sticsReport = 0;
		
		
		
		for (Iterator i = this.iterator(); i.hasNext();) {
			Record record = (Record) i.next();

			if (record instanceof SafeSimulationLoader.KeyRecord) {

				SafeSimulationLoader.KeyRecord r = (SafeSimulationLoader.KeyRecord) record;

				String param = r.key;

				
				// required parameters
				if (param.equals("projectFileName")) {
					projectFileName = r.value;
					
				} else if (param.equals("nbSimulations")) {
					nbSimulations = r.getIntValue();
					requiredParameters.remove("nbSimulations");

				} else if (param.equals("simulationYearStart")) {
					simulationYearStart = r.getIntValue();
					requiredParameters.remove("simulationYearStart");

				} else if (param.equals("simulationNbrDays")) {
					simulationNbrDays = r.value;
					requiredParameters.remove("simulationNbrDays");

				} else if (param.equals("simulationDayStart")) {
					simulationDayStart = r.getIntValue();
					requiredParameters.remove("simulationDayStart");
					
				} else if (param.equals("mainCropSpecies")) {
					mainCropSpecies = r.value;
					requiredParameters.remove("mainCropSpecies");

					
				} else if (param.equals("interCropSpecies")) {
					interCropSpecies = r.value;
					requiredParameters.remove("interCropSpecies");
				
				} else if (param.equals("mainCropItk")) {
					mainCropItk = r.value;
					requiredParameters.remove("mainCropItk");
					
				} else if (param.equals("interCropItk")) {
					interCropItk = r.value;
					requiredParameters.remove("interCropItk");					
					
				} else if (param.equals("treeCropDistance")) {
					treeCropDistance = r.value;
					requiredParameters.remove("treeCropDistance");
					
				} else if (param.equals("treeCropRadius")) {
					requiredParameters.remove("treeCropRadius");
					treeCropRadius = r.value;

				// others parameters (optional)
				} else if (param.equals("profileNames")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						if (st[k].equals("sti")) exportStiFiles = true;
						else profileNames.add(st[k]);
					}
					
				// others parameters (optional)
				} else if (param.equals("exportFrequencies")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						exportFrequencies.add(st[k]);
					}					

				} else if (param.equals("saveProjectOption")) {
					saveProjectOption = r.getIntValue();

				} else if (param.equals("debugMode")) {
					debugMode = r.getIntValue();
					
				} else if (param.equals("sticsReport")) {
					sticsReport = r.getIntValue();				
					
					
				} else if (param.equals("treesLeafLitterIncorporated")) {
					int b = r.getIntValue();
					treesLeafLitterIncorporated = true;
					if (b==0) treesLeafLitterIncorporated = false;
					requiredParameters.remove("treesLeafLitterIncorporated");
					
				} else if (param.equals("treesRootLitterIncorporated")) {
					int b = r.getIntValue();
					treesRootLitterIncorporated = true;
					if (b==0) treesRootLitterIncorporated = false;
					requiredParameters.remove("treesRootLitterIncorporated");

				} else if (param.equals("treesDeepRootLitterIncorporated")) {
					int b = r.getIntValue();
					treesDeepRootLitterIncorporated = true;
					if (b==0) treesDeepRootLitterIncorporated = false;
					requiredParameters.remove("treesDeepRootLitterIncorporated");
					
				// PLANTING
				} else if (param.equals("treePlantingYears")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePlantingYears.add(new Integer(Integer.parseInt(st[k])));
					}					
				} else if (param.equals("treePlantingDays")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePlantingDays.add(new Integer(Integer.parseInt(st[k])));
					}	
					
				// PRUNING
				} else if (param.equals("treePruningYears")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePruningYears.add(new Integer(Integer.parseInt(st[k])));
					}
				} else if (param.equals("treePruningDays")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePruningDays.add(new Integer(Integer.parseInt(st[k])));
					}
				} else if (param.equals("treePruningProp")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePruningProp.add(new Double(Double.parseDouble(st[k])));
					}
				} else if (param.equals("treePruningMaxHeight")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePruningMaxHeight.add(new Double(Double.parseDouble(st[k])));
					}
				} else if (param.equals("treeDensityReduction")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeDensityReduction.add(new Double(Double.parseDouble(st[k])));
					}

					// THINNING
				} else if (param.equals("treeThinningIds")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeThinningIds.add(new Integer(Integer.parseInt(st[k])));
					}

				} else if (param.equals("treeThinningYears")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeThinningYears.add(new Integer(Integer.parseInt(st[k])));
					}

				} else if (param.equals("treeThinningDays")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeThinningDays.add(new Integer(Integer.parseInt(st[k])));
					}

					// ROOT PRUNING
				} else if (param.equals("treeRootPruningYears")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeRootPruningYears.add(new Integer(Integer.parseInt(st[k])));
					}

				} else if (param.equals("treeRootPruningDays")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeRootPruningDays.add(new Integer(Integer.parseInt(st[k])));
					}
				} else if (param.equals("treeRootPruningDistance")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeRootPruningDistance.add(new Double(Double.parseDouble(st[k])));
					}
				} else if (param.equals("treeRootPruningDepth")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treeRootPruningDepth.add(new Double(Double.parseDouble(st[k])));
					}

					
				// POLLARDING
				} else if (param.equals("treePollardingMethod")) {
					treePollardingMethod = r.getIntValue();
				
				} else if (param.equals("treePollardingThreshold")) {
					treePollardingThreshold = new Double(Double.parseDouble(r.value));
				
				} else if (param.equals("treePollardingYears")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePollardingYears.add(new Integer(Integer.parseInt(st[k])));
					}

				} else if (param.equals("treePollardingDays")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePollardingDays.add(new Integer(Integer.parseInt(st[k])));
					}
				} else if (param.equals("treePollardingHeight")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePollardingHeight.add(new Double(Double.parseDouble(st[k])));
					}
				} else if (param.equals("treePollardingCanopyLeft")) {
					String[] st = (r.value).split(",");
					for (int k = 0; k < st.length; k++) {
						treePollardingCanopyLeft.add(new Double(Double.parseDouble(st[k])));
					}

				} else if (param.equals("toricXp")) {
					toricXp = r.getIntValue();
					requiredParameters.remove("toricXp");

				} else if (param.equals("toricXn")) {
					toricXn = r.getIntValue();
					requiredParameters.remove("toricXn");

				} else if (param.equals("toricYp")) {
					toricYp = r.getIntValue();
					requiredParameters.remove("toricYp");

				} else if (param.equals("toricYn")) {
					toricYn = r.getIntValue();
					requiredParameters.remove("toricYn");
	
				}
			}
		}

		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing simulation parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort
		}
		//missing export frequencies 
		if (profileNames.size() > exportFrequencies.size()) {
			throw new Exception("Missing export frequencies ! ");

		}
	}




}
