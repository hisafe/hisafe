/*
* Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
*
* Copyright (C) 2000-2003  Francois de Coligny
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied
* warranty of MERCHANTABILITY or FITNESS FOR A
* PARTICULAR PURPOSE. See the GNU Lesser General Public
* License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/

package safe.extension.ioformat.safeExport;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.TreeSet;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;
import safe.model.SafeCell;
import safe.model.SafeCrop;
import safe.model.SafeDailyClimat;
import safe.model.SafePlantRoot;
import safe.model.SafeLayer;
import safe.model.SafeMacroClimat;
import safe.model.SafeModel;
import safe.model.SafePlot;
import safe.model.SafeSoil;
import safe.model.SafeStand;
import safe.model.SafeTree;
import safe.model.SafeVoxel;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import jeeb.lib.util.PathManager;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;
import capsis.util.StandRecordSet;

/**
  * SafeExport execute Safe exportation 
  *
  * @author R. Tuquet Laburre - august 2003
  */
public class SafeExport extends StandRecordSet implements OFormat {

	// fc-4.12.2020 Checked, is an export, added OFormat

	// Subject Ids from which are made specific processing
	public static String SUBJECT_TREE_INDEX 	= "SafeTree";
	public static String SUBJECT_CELL_INDEX 	= "SafeCell";
	public static String SUBJECT_VOXEL_INDEX 	= "SafeVoxel";
	public static String SUBJECT_PLOT_INDEX 	= "SafePlot";
	public static String SUBJECT_LAYER_INDEX 	= "SafeLayer";
	public static String SUBJECT_CLIMATE_INDEX 	= "SafeMacroClimat";

	private Vector subjects;			// all available subjects (readed in subjects.txt)
	private SafeExportProfile profile; 	// export profile's content
	private SafeExportProfileCatalog catalog; 
	private String folderFileName;		// export folder filename
	private String profileFileName;		// export profile filename
	private int cptSteps=0;				// count of steps
	private SafeMacroClimat macroClimate;


	// voxel variable positions in the output record
	private static int VOXEL_INDEX_ID_CELL=0;
	private static int VOXEL_INDEX_ID_VOXEL=1;
	
	private boolean isBatchMode = false;

	SafeExportProfileCatalogEditor dlgProfileCatalog;	// Profile Catalog dialog box
	SafeExportDialog dlgProgress;		// not modal dialog, displayed during export processing

	private boolean headerEnabled;

	static { Translator.addBundle("safe.extension.ioformat.safeExport.SafeExport"); }



	/**
	* Phantom constructor.
	* Only to ask for extension properties (authorName, version...).
	*/
	public SafeExport () throws Exception {
		// load variables (for all subjects)

		this.folderFileName =  PathManager.getDir("data")
								+File.separator+"safe"
								+File.separator+"exportParameters";
		

		String variablesFileName = 	getFilesPath() + File.separator+"variables.txt";

		Collection variables = new SafeExportVariablesInputRecordSet(variablesFileName).getVariables();

		String subjectsFileName = getFilesPath() + File.separator+"subjects.txt";

		// load subjects with variables and set variables collection for each of them
		subjects = new SafeExportSubjectsInputRecordSet(subjectsFileName,variables).getSubjects();

		
		for (int i=0;i<subjects.size();i++) {
			SafeExportSubject subject = (SafeExportSubject) subjects.get(i);
			subject.getVariableItemsForList ();
		}
		isBatchMode = false;
	}



	
	public SafeExport (String folderFileName, String profileFileName) throws Exception {
		
		this();	
		isBatchMode = true;
		this.folderFileName = folderFileName;
		this.profileFileName = profileFileName;
		if (catalog == null) catalog = new SafeExportProfileCatalog(folderFileName);
		this.profile = catalog.getProfileFromName (profileFileName);
	}
		
	@Override
	public void initExport(GModel m, Step s) throws Exception {				

		GScene stand = s.getScene ();
		
		if(!isBatchMode) {

			// open the profile catalog dialog box
			dlgProfileCatalog = new SafeExportProfileCatalogEditor (subjects, stand);
			if (dlgProfileCatalog.isValidDialog ()) {
				
				this.profileFileName = dlgProfileCatalog.getCurrentSelection();
				this.profile = dlgProfileCatalog.getProfileFromName (profileFileName);
				dlgProfileCatalog.dispose ();
				
				
			} else {
				dlgProfileCatalog.dispose ();
				return;		// user cancel
			}
		}
		setHeaderEnabled(true); 
		this.headerEnabled= true;		// fc - 16.12.2008 - capsis header	
		
		//remove headers (IL+KW 05.07.2018)
		//	createAndWriteProfileRecords();
		prepareExport (stand);

	}

	public void export(GModel m, Step s) throws Exception {	
		setHeaderEnabled(false);
		GScene stand = s.getScene ();
		this.headerEnabled= false;		// fc - 16.12.2008 - capsis header
		prepareExport (stand);

	}	

	/**
	* Extension dynamic compatibility mechanism.
	* This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof SafeModel)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "SafeExport.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	/**
	* From Extension interface.
	*/
	@Override
	public String getName () {
		return Translator.swap ("SafeExport.name");
	}

	/**
	* From Extension interface.
	*/
	@Override
	public String getAuthor () {
		return "R. Tuquet Laburre";
	}

	/**
	* From Extension interface.
	*/
	@Override
	public String getDescription () {
		return Translator.swap("SafeExport.description");
	}

	/**
	* From Extension interface.
	*/
	@Override
	public String getVersion () {
		return "1.0";
	}

	// nb-22.08.2018
	//public static final String VERSION = "1.0";

	//
	// RecordSet - File
	// is described in superclass (save (fileName)).
	//

	/**
	* RecordSet - Stand
	* Implementation here.
	*/
	public GScene load (GModel model) throws Exception {	// no import here
		return null;
	}

	// fc-7.12.2020 Deprecated
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}

	/**
	 * @return the filepath of subjects and varaibles files
	 */
	public static String getFilesPath () {
		return PathManager.getDir("data")
			+File.separator+"safe"
			+File.separator+"exportParameters";
	}


	/**
	 * Generate the entire export file
	 */
	@Override
	public void prepareExport (GScene sc) throws Exception {
		SafeStand s = (SafeStand) sc;

		//super.createRecordSet (stand); // deals with RecordSet's source
		TreeList stand = (TreeList) s;	// Real type of Safe stands


		Step step;				// current step used by the export processing
		Vector stepsFromRoot;	// contains all steps from root to the selected step
		Vector stepsToExport;   // contains all steps concerned by the export processing

		// set the available subjects for eache SafeExportSubjectToExport
		profile.setSubjectsOfSubjectsToExport(subjects);

		// variables used for default subjects
		SafeExportSubjectToExport defaultSubject=null;
		Vector defaultRecords=null;

		// variables used for Tree subject
		SafeExportSubjectTreeToExport treeSubject=null;
		TreeSet treeRecords=null;
		int[] treeIdsToExport=new int[1];

		// variables used for Cell subject
		SafeExportSubjectCellToExport cellSubject=null;
		TreeSet cellRecords=null;
		String[] cellsToExport = null;
		int[]  cellIdsToExport=new int[1];

		// variables used for Layer subject
		SafeExportSubjectLayerToExport layerSubject=null;
		TreeSet layerRecords=null;
		int[]  layerIdsToExport=new int[1];
		
		
		// variables used for Voxel subject
		SafeExportSubjectVoxelToExport voxelSubject=null;
		TreeSet voxelRecords=null;
		int[] voxelCellIdsToExport = new int[1];
		double[] voxelDepthIdsToExport = new double[1];
		
		String[] voxelDepthToExport = null;
		String[] voxelsCellsToExport = null;
		
		ArrayList<Integer> voxelIdsToExport=new ArrayList<Integer>();

		// variables used for Climate subject
		SafeExportSubjectToExport climateSubject=null;
		TreeSet climateRecords=null;

		// variables used for plot subject
		SafeExportSubjectToExport plotSubject=null;
		TreeSet plotRecords=null;



		try {
			/*************** SELECTION OF STEPS *************************/
			step = (Step) stand.getStep();
			macroClimate = ((SafeModel) (step.getProject().getModel())).getMacroClimat();
			stepsFromRoot = step.getProject().getStepsFromRoot(step);
			this.cptSteps=0;
			stepsToExport=new Vector();

			// root step to export ?
			//if (profile.extendRootStepToExport) {
			//	stepsToExport.add(stepsFromRoot.get(0));
			//}
			// only current step to export ?
			if (profile.extendCurrentStep) {
				stepsToExport.add(step);
			} else {

				// calculate the steps to export using the profile's extend datas
				int posStep;
				for (int i=1; i < stepsFromRoot.size() ; i++) {
					step=(Step) stepsFromRoot.get(i);

					//to avoid exporting intervention steps
					if (! step.getName().contains("*")) {

					   // Give/return a state information about the step passed in argument according to the profile extend
					   // @return -1 if the step is before the beginning of the profile extend
					   // @return 0  if the step is in the profile extend AND to export
					   // @return 1  if the step is in the profile extend BUT not to export
					   // @return 2  if the step is after the end of the profile extend
					   //

						int j = ((SafeStand) step.getScene()).getJulianDay();						
						posStep = getStepStateAboutExport (step, j);
						if (posStep==0) {
							stepsToExport.add (step);						
						} else if (posStep==2) {
							i=stepsFromRoot.size();
						}
					}
				}
			}

			// display a not modial dialog box which will display informations during the export processing
			if (! isBatchMode)
				dlgProgress= new SafeExportDialog(profile.getCountOfSubjectsToExport());

			// init some variables before the beginning of the export processing
			int countOfStepsToExport=0;					// count of steps to export


			/*************** SELECTION OF OBJECTS *************************/
			for (int i=0; i< profile.subjectsToExport.size() ; i++) {

			// init some vars about the current subject
				SafeExportSubjectToExport profileSubject = (SafeExportSubjectToExport) profile.subjectsToExport.get(i);
				String subjectName=profileSubject.getSubject().getName();
				String subjectLocalName=profileSubject.getSubject().getLocalName();
				
				// TREE OBJECTS
				if (subjectName.equals(SafeExport.SUBJECT_TREE_INDEX)) {

					countOfStepsToExport=stepsToExport.size();
					treeRecords = new TreeSet (new SafeExportRecordComparator ());
					treeSubject = (SafeExportSubjectTreeToExport) profileSubject;

					// define the collection of treeIds to export
					if (!treeSubject.exportAll) {
						treeIdsToExport = SafeExportTools.collectionToArrayOfIntegers(treeSubject.ids);
					} else {
						treeIdsToExport = new int[stand.getTrees().size()];
						int cpt=0;
						for (Iterator it2=stand.getTrees().iterator();it2.hasNext();cpt++) {
							treeIdsToExport[cpt]=Integer.parseInt(""+ ((SafeTree) it2.next()).getId());
						}
					}
					// add a variable which will contain the itree ID
					treeSubject.addVariable(new SafeExportVariable(subjectName,"idTree","-","-","idTree","idTree"),0);
					

				// CELL OBJECT
				} else if (subjectName.equals(SafeExport.SUBJECT_CELL_INDEX)) {
					
					PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017
					
					countOfStepsToExport=stepsToExport.size();
					cellRecords = new TreeSet (new SafeExportRecordComparator ());
					cellSubject = (SafeExportSubjectCellToExport) profileSubject;

					cellIdsToExport = new int[plotc.getCells().size()];
					
					if (cellSubject.ids.size() > 0) {
						
						try {

							cellsToExport = SafeExportTools.collectionToArrayOfString(cellSubject.ids);
														
							for (int j=0; j<cellsToExport.length; j++) { 
								
								String [] cellToExport = cellsToExport[j].split("-");	

								int cellMin = Integer.parseInt(cellToExport[0]);
								int cellMax = Integer.parseInt(cellToExport[1]);							
								int cpt=0;
								for (Iterator it2=plotc.getCells().iterator();it2.hasNext();cpt++) {
									SafeCell cell = (SafeCell) it2.next();
				
									if ((cell.getId() >= cellMin) && (cell.getId() <= cellMax)) {
										cellIdsToExport[cpt]=cell.getId();
									}
								}
							}	
						}
						catch (Exception e) {
							cellIdsToExport = SafeExportTools.collectionToArrayOfIntegers(cellSubject.ids);
						}
					}
					//all cells exported
					else {						
						int cpt=0;
						for (Iterator it2=plotc.getCells().iterator();it2.hasNext();cpt++) {
							cellIdsToExport[cpt]=Integer.parseInt(""+ ((SafeCell) it2.next()).getId());
						}
					}
					

					// add a variable which will contain the Cell ID
					cellSubject.addVariable(new SafeExportVariable(subjectName,"idCell","-","-","idCell","idCell"),0);
					cellSubject.addVariable(new SafeExportVariable(subjectName,"x","m","m","X","Y"),1);
					cellSubject.addVariable(new SafeExportVariable(subjectName,"y","m","m","X","Y"),2);


				// VOXEL OBJECT
				} else if (subjectName.equals(SafeExport.SUBJECT_VOXEL_INDEX)) {

					PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017
					
					countOfStepsToExport=stepsToExport.size();
					voxelRecords = new TreeSet (new SafeExportRecordComparator ());
					voxelSubject = (SafeExportSubjectVoxelToExport) profileSubject;

					// define the collection of voxel cellIds to export
					//ALL by default
					voxelCellIdsToExport = new int[plotc.getCells().size()];
					

					if (voxelSubject.cellIds.size() > 0) {
						
						try {

							voxelsCellsToExport = SafeExportTools.collectionToArrayOfString(voxelSubject.cellIds);
														
							for (int j=0; j<voxelsCellsToExport.length; j++) { 
								
								String [] cellToExport = voxelsCellsToExport[j].split("-");	

								int cellMin = Integer.parseInt(cellToExport[0]);
								int cellMax = Integer.parseInt(cellToExport[1]);

								int cpt=0;
								for (Iterator it2=plotc.getCells().iterator();it2.hasNext();cpt++) {
									SafeCell cell = (SafeCell) it2.next();
				
									if ((cell.getId() >= cellMin) && (cell.getId() <= cellMax)) {
										voxelCellIdsToExport[cpt]=cell.getId();
									}
								}
							}	
						}
						catch (Exception e) {

							voxelCellIdsToExport = SafeExportTools.collectionToArrayOfIntegers(voxelSubject.cellIds);
						}
					}
					//all cells exported
					else {
						int cpt = 0;
						for (Iterator it2=plotc.getCells().iterator();it2.hasNext();cpt++) {
							voxelCellIdsToExport[cpt]=Integer.parseInt(""+ ((SafeCell) it2.next()).getId());
						}
					}
					
					if (voxelSubject.depths.size() > 0) {

						
						try {
							voxelDepthToExport = SafeExportTools.collectionToArrayOfString(voxelSubject.depths);

							for (int j=0; j<voxelDepthToExport.length; j++) { 

								String [] depthToExport = voxelDepthToExport[j].split("-");	
								double depthMin = Double.parseDouble(depthToExport[0]);
								double depthMax = Double.parseDouble(depthToExport[1]);
								int cpt = 0; 
								for (Iterator it2=plotc.getCells().iterator();it2.hasNext();cpt++) {
									SafeCell cell = (SafeCell) it2.next();
									SafeVoxel[] voxels = cell.getVoxels();
									//FOR EACH VOXEL
									for (int v = 0; v < voxels.length; v++) {
										//check if voxel depth is in the limts 
										double depthVoxel = voxels[v].getZ();
										if ((depthVoxel >= depthMin) && (depthVoxel <= depthMax)) {
											 voxelIdsToExport.add(voxels[v].getId());
										}
									}
								}
							}	
						
						}
						catch (Exception e) {
							voxelDepthIdsToExport = SafeExportTools.collectionToArrayofFloats(voxelSubject.depths);
							
							for (int j=0; j<voxelDepthIdsToExport.length; j++) {
								int cpt = 0;
								for (Iterator it2=plotc.getCells().iterator();it2.hasNext();cpt++) {
									SafeCell cell = (SafeCell) it2.next();
									SafeVoxel[] voxels = cell.getVoxels();
									//FOR EACH VOXEL
									for (int v = 0; v < voxels.length; v++) {
										//check if voxel depth is in the limts 
										double depthVoxel = voxels[v].getZ();

										if ((depthVoxel == voxelDepthIdsToExport[j])) {
											 voxelIdsToExport.add(voxels[v].getId());
										}
									}
								}
							}
							
						}
						
					}
					
					
					// add variables which will contain Cell ID, Voxel ID, depth and thickness of the voxel
					voxelSubject.addVariable(new SafeExportVariable(subjectName,"idCell","-","-","idCell","idCell"),0);
					voxelSubject.addVariable(new SafeExportVariable(subjectName,"idVoxel","-","-","idVoxel","idVoxel"),1);
					voxelSubject.addVariable(new SafeExportVariable(subjectName,"x","m","m","X","Y"),2);
					voxelSubject.addVariable(new SafeExportVariable(subjectName,"y","m","m","X","Y"),3);
					voxelSubject.addVariable(new SafeExportVariable(subjectName,"z","m","m","Z","Z"),4);

				//PLOT OBJECT = no selection
				} else if (subjectName.equals(SafeExport.SUBJECT_PLOT_INDEX)) {
					countOfStepsToExport=1;
					plotSubject = (SafeExportSubjectToExport) profileSubject;
					plotRecords = new TreeSet (new SafeExportRecordComparator ());

				// LAYER OBJECT 
				} else if (subjectName.equals(SafeExport.SUBJECT_LAYER_INDEX)) {
		
					countOfStepsToExport=stepsToExport.size();
					layerRecords = new TreeSet (new SafeExportRecordComparator ());
					layerSubject = (SafeExportSubjectLayerToExport) profileSubject;
				
					// define the collection of cellIds to export
					if (!layerSubject.exportAll) {
						layerIdsToExport = SafeExportTools.collectionToArrayOfIntegers(layerSubject.ids);
					} else {
						layerIdsToExport = new int[s.getPlot().getSoil().getLayers().size()];
						int cpt=0;
						for (Iterator it2=s.getPlot().getSoil().getLayers().iterator();it2.hasNext();cpt++) {
							layerIdsToExport[cpt]=Integer.parseInt(""+ ((SafeLayer) it2.next()).getId());
						}
					}

					
					// add a variable which will contain the Layer ID
					layerSubject.addVariable(new SafeExportVariable(subjectName,"idLayer","-","-","idLayer","idLayer"),0);
					layerSubject.addVariable(new SafeExportVariable(subjectName,"thickness","m","m","thickness","thickness"),1);
					layerSubject.addVariable(new SafeExportVariable(subjectName,"surfaceDepth","m","m","surfaceDepth","surfaceDepth"),2);
						
				// CLIMATE OBJECT : no selection
				} else if (subjectName.equals(SafeExport.SUBJECT_CLIMATE_INDEX)) {
					countOfStepsToExport=1;
					climateSubject = (SafeExportSubjectToExport) profileSubject;				
					climateSubject.addVariable(new SafeExportVariable(subjectName,"realYear","-","-","realYear","realYear"),0);
					climateRecords = new TreeSet (new SafeExportRecordComparator ());

				// OTHERS : no selection
				} else {
					countOfStepsToExport=1;
					defaultSubject = (SafeExportSubjectToExport) profileSubject;
					defaultRecords = new Vector();
				}

				/**********************   EXPORTATION  ******************************/
				// refresh the "progress dialog box" with current subject name and count of steps to export
				if (!isBatchMode)
					dlgProgress.initCurrent(subjectLocalName,countOfStepsToExport);

				int cptSteps=0;

				// TREE EXPORT
				if (subjectName.equals(SafeExport.SUBJECT_TREE_INDEX)) {
					
					if (headerEnabled) {
						Step rootstep = (Step) stepsFromRoot.get(0);
						stand = (SafeStand) rootstep.getScene();
						SafeTree firstTree = (SafeTree) stand.getTree(1);
						writeHeader(treeSubject.getSafeExportVariables(),firstTree);
					}
					
					SafeTree tree=null;
					// for all steps to export...
					for (Iterator it2=stepsToExport.iterator();it2.hasNext();cptSteps++) {
						step = (Step) it2.next();
						stand = (SafeStand) step.getScene();
						if (!isBatchMode)
							dlgProgress.setCurrent(getStepCaptionForProgress(step));

						Collection trees = stand.getTrees();
						// for all selected treeIds...
						for (Iterator itTree=trees.iterator(); itTree.hasNext();) {
							tree = (SafeTree) itTree.next();
							for (int idTree=0; idTree < treeIdsToExport.length; idTree++) {
								// current treeId is to export ? create recordSet
								if (treeIdsToExport[idTree]==tree.getId())
									createStepTreeRecords(treeRecords,treeSubject.getSafeExportVariables(),step,cptSteps,tree);
							}
						}
					}
					// write the treeRecords
					writeRecords(treeRecords);

				// CELL EXPORT
				} else if (subjectName.equals(SafeExport.SUBJECT_CELL_INDEX)) {
					
					cellSubject = (SafeExportSubjectCellToExport) profileSubject;
					
					if (headerEnabled) {
						Step rootstep = (Step) stepsFromRoot.get(0);
						stand = (SafeStand) rootstep.getScene();
						PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017
						SafeCell firstCell = (SafeCell) plotc.getCell(1);
						writeHeader(cellSubject.getSafeExportVariables(),firstCell);
					}
					
					
					SafeCell cell=null;
					

					// for all steps to export...
					for (Iterator it2=stepsToExport.iterator();it2.hasNext();cptSteps++) {
						step = (Step) it2.next();
						stand = (SafeStand) step.getScene();

						PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017

						if (!isBatchMode)
							dlgProgress.setCurrent(getStepCaptionForProgress(step));

						// for all selected cellIds...
						Collection cells = plotc.getCells();
						for (Iterator itCell=cells.iterator(); itCell.hasNext();) {
							cell = (SafeCell) itCell.next();
							for (int idCell=0; idCell < cellIdsToExport.length ; idCell++) {
								// current cellId is to export ? create recordSet
								if (cellIdsToExport[idCell]==cell.getId())
									createStepCellRecords(cellRecords,cellSubject.getSafeExportVariables(),step,cptSteps,cell, false);
							}
						}
					}

					// write the cellRecords
					writeRecords(cellRecords);


				// VOXEL EXPORT
				} else if (subjectName.equals(SUBJECT_VOXEL_INDEX)) {
					
					voxelSubject = (SafeExportSubjectVoxelToExport) profileSubject;
					
					if (headerEnabled) {
						Step rootstep = (Step) stepsFromRoot.get(0);
						stand = (SafeStand) rootstep.getScene();
						PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017
						SafeCell firstCell = (SafeCell) plotc.getCell(1);
						SafeVoxel firstVoxel = firstCell.getFirstVoxel();
						writeHeader(voxelSubject.getSafeExportVariables(),firstVoxel);
					}
					
					
					SafeVoxel voxel=null;
					
					// for all steps to export...
					for (Iterator it2=stepsToExport.iterator();it2.hasNext();cptSteps++) {
						step = (Step) it2.next();
						stand = (SafeStand) step.getScene();

						PlotOfCells plotc = (PlotOfCells) stand.getPlot(); // fc-30.10.2017

						SafeCell cell;
						Collection voxels;

						if (!isBatchMode)
							dlgProgress.setCurrent(getStepCaptionForProgress(step));

						Collection cells = plotc.getCells();
						// for all selected cellIds...
						for (Iterator itCell=cells.iterator(); itCell.hasNext();) {
							cell = (SafeCell) itCell.next();
							for (int idCell=0; idCell < voxelCellIdsToExport.length ; idCell++) {
								// current cellId is to export ?
								if (voxelCellIdsToExport[idCell]==cell.getId()) {
									voxels = java.util.Arrays.asList(cell.getVoxels());
									// for all voxels (of a cell)...
									
									for (Iterator itVoxel = voxels.iterator() ; itVoxel.hasNext();) {
										voxel = (SafeVoxel) itVoxel.next();
										if (voxelIdsToExport.size() == 0) 
												createStepVoxelRecords(voxelRecords,voxelSubject.getSafeExportVariables(),step,cptSteps,voxel);
										else {

											for (int idVoxel=0; idVoxel < voxelIdsToExport.size() ; idVoxel++) {
											// is the depth of the voxel is to export : create recordSet
												if (voxelIdsToExport.get(idVoxel)==voxel.getId()) {
													createStepVoxelRecords(voxelRecords,voxelSubject.getSafeExportVariables(),step,cptSteps,voxel);
												}
											}
										}
									}
								}
							}
						}
					}
					// write the voxelRecords
				writeRecords(voxelRecords);


				// PLOT EXPORT
				} else if (subjectName.equals(SUBJECT_PLOT_INDEX)) {
					
					if (headerEnabled) {
						Step rootstep = (Step) stepsFromRoot.get(0);
						SafeStand firstStand = (SafeStand) rootstep.getScene();					
						writeHeader(plotSubject.getSafeExportVariables(),firstStand);
					}
					
					
					stand=null;
					for (Iterator it2=stepsToExport.iterator();it2.hasNext();cptSteps++) {
						step = (Step) it2.next();

						//get year and day from step caption
						try {
							stand =   (SafeStand) step.getScene();
							createStepPlotRecords(plotRecords, plotSubject.getSafeExportVariables(), step, cptSteps, stand);
						}
						catch (Exception exc)  {}
					}
				// write the plotRecords
				writeRecords(plotRecords);

				// LAYER EXPORT
				} else if (subjectName.equals(SUBJECT_LAYER_INDEX)) {

					layerSubject = (SafeExportSubjectLayerToExport) profileSubject;
					
					if (headerEnabled) {
						SafeLayer firstLayer = new SafeLayer(0);
						writeHeader(layerSubject.getSafeExportVariables(),firstLayer);
					}
					
					SafeLayer layer=null;
					// for all steps to export...
					for (Iterator it2=stepsToExport.iterator();it2.hasNext();cptSteps++) {
						step = (Step) it2.next();
						stand = (SafeStand) step.getScene();
						
						SafeSoil soil = (SafeSoil) s.getPlot().getSoil(); // fc-30.10.2017

						if (!isBatchMode)
							dlgProgress.setCurrent(getStepCaptionForProgress(step));

						// for all selected cellIds...
						Collection layers = soil.getLayers();
						for (Iterator itLayer=layers.iterator(); itLayer.hasNext();) {
							layer = (SafeLayer) itLayer.next();
							if (layer != null) {
								for (int idLayer=0; idLayer < layerIdsToExport.length ; idLayer++) {
									// current cellId is to export ? create recordSet
									if (layerIdsToExport[idLayer]==layer.getId())
										createStepLayerRecords(layerRecords,layerSubject.getSafeExportVariables(),step,cptSteps,layer, false);
								}
							}
						}
					}

					// write the layerRecords
					writeRecords(layerRecords);
					
				

				// CLIMATE EXPORT
				} else if (subjectName.equals(SUBJECT_CLIMATE_INDEX)) {

					SafeDailyClimat climat=null;
					for (Iterator it2=stepsToExport.iterator();it2.hasNext();cptSteps++) {
						step = (Step) it2.next();

						//get year and day from step caption
						int y = ((SafeStand) step.getScene()).getWeatherYear();
						int j = ((SafeStand) step.getScene()).getJulianDay();
					
						try {
							climat = macroClimate.getDailyWeather(y, j);
							if (climat != null)
								createStepClimateRecords(climateRecords,climateSubject.getSafeExportVariables(),step,cptSteps,climat);
						}
						catch (Exception exc)  {}
				}

				// write the climateRecords
				if (headerEnabled) writeHeader(climateSubject.getSafeExportVariables(),climat);
				
				writeRecords(climateRecords);

				// OTHER EXPORT
				} else {
					if (!isBatchMode)
						dlgProgress.setCurrent(defaultSubject.getSubject().getLocalName());

					// create recordSet (in defaultRecords)
					createDefaultRecords(defaultRecords,defaultSubject.getName(),defaultSubject.getSafeExportVariables(),step);
					// write the defaultRecords
					writeDefaultRecords(defaultRecords,subjectLocalName);
				}
			}
		} catch (Exception e) {
			System.out.println ("Error : " + e);
			String err = Translator.swap ("SafeExport.thisErrorWasProducedDuringExportation")
					+ "\n" + e + "\n";
		MessageDialog.print (this, err);
		}
		if (!isBatchMode) dlgProgress.dispose();
	}

	/**
	 * @return a string which contains step "title" used in the progress dialog box
	 */
	private String getStepCaptionForProgress(Step step) {
		return Translator.swap("SafeExport.step") + " " + step.getName();
	}

	/**
	 * Give/return a state information about the step passed in argument according to the profile extend
	 * @return -1 if the step is before the beginning of the profile extend
	 * @return 0  if the step is in the profile extend AND to export
	 * @return 1  if the step is in the profile extend BUT not to export
	 * @return 2  if the step is after the end of the profile extend
	 */
	private int getStepStateAboutExport(Step step, int j)
	{
		if (step == null) {
			return 2; 			// step is after the end of the extend;
		}
		
		if (profile.extendFrequency) {
			if (j < profile.extendFrequencyBegin) {
				return -1; 		// step is before the beginning of extend
			}
			if (j == profile.extendFrequencyBegin) {
				return 0; 		// the step is in the profile extend AND to export
			}
			if (j%profile.extendFrequencyValue==0) {
				return 0; 		// the step is in the profile extend AND to export
			} else {
				return 1; 		// the step is in the profile extend BUT not to export
			}
		} else if (profile.extendPeriod) {
			if (j >= profile.extendPeriodFrom && j <= profile.extendPeriodTo) {
				return 0; 		// the step is in the profile extend AND to export
			}
			if (j < profile.extendPeriodFrom) {
				return -1; 		// step is before the beginning of extend
			}
			if (j > profile.extendPeriodTo) {
				return 2; 		// the step is after the end of the profile extend
			}
		}
		cptSteps++;
		return 0; 				// the step is in the profile extend AND to export
	}

	/**
	 * create and write records which contains informations about the profile file used by the export processing
	 */
	private void createAndWriteProfileRecords () {

		profile.setSubjectsOfSubjectsToExport(subjects);
		add(new CommentRecord(Translator.swap ("SafeExport.exportProfile")));
		StringBuffer sb = new StringBuffer ("");
		sb.append ("> " + Translator.swap("SafeExport.profileName")+ " : " + this.profileFileName+"\n");
		// extend informations
		sb.append ("> " + Translator.swap ("SafeExport.extend") + " : ");
		if (profile.extendAllSteps) {
			sb.append (Translator.swap ("SafeExport.extendAllSteps"));
		} else  if (profile.extendCurrentStep) {
			sb.append (Translator.swap ("SafeExport.extendCurrentStep"));
		} else if (profile.extendFrequency) {
			sb.append (Translator.swap ("SafeExport.extendFrequency")+" : ");
			sb.append (Translator.swap ("SafeExport.extendFrequencyBegin")+" = ");
			sb.append (profile.extendFrequencyBegin+ " ; ");
			sb.append (Translator.swap ("SafeExport.extendFrequencyValue") + " = ");
			sb.append (profile.extendFrequencyValue + " ");
			sb.append (Translator.swap ("SafeExport.extendFrequencyUnity"));
		} else if (profile.extendPeriod) {
			sb.append (Translator.swap ("SafeExport.extendperiod")+" : ");
			sb.append (Translator.swap ("SafeExport.extendPeriodFrom")+" ");
			sb.append (profile.extendPeriodFrom+ " ");
			sb.append (Translator.swap ("SafeExport.extendPeriodTo")+" ");
			sb.append (profile.extendPeriodTo);
		}
		sb.append ("\n");
		sb.append ("> " + Translator.swap ("SafeExport.extendRootStepToExport")+ " : ");
		if (profile.extendRootStepToExport)
			sb.append (Translator.swap("Shared.yes"));
		else
			sb.append (Translator.swap("Shared.no"));
		sb.append ("\n");

		// subjects informations
		for (int i=0 ; i < profile.subjectsToExport.size() ; i++) {
			SafeExportSubjectToExport subjectToExport = (SafeExportSubjectToExport) profile.subjectsToExport.get(i);
			String subjectName = subjectToExport.getName();
			sb.append("> " + subjectToExport.getSubject().getLocalName() + " : ");
			if (subjectName.equals(SafeExport.SUBJECT_TREE_INDEX)) {
				SafeExportSubjectTreeToExport treeSubject = (SafeExportSubjectTreeToExport) subjectToExport;
				subjectToExport=treeSubject;
				if (treeSubject.exportAll) {
					sb.append(Translator.swap("SafeExport.all")) ;
				}
				else {
					sb.append(Translator.swap("SafeExport.treeIds") + " : ");
					sb.append(SafeExportTools.collectionToString(treeSubject.ids));
				}
			} else if (subjectName.equals(SafeExport.SUBJECT_CELL_INDEX)) {
				SafeExportSubjectCellToExport cellSubject = (SafeExportSubjectCellToExport) subjectToExport;
				subjectToExport=cellSubject;
				if (cellSubject.exportAll) {
					sb.append(Translator.swap("SafeExport.all")) ;
				}
				else {
					sb.append(Translator.swap("SafeExport.cellIds") + " : ");
					sb.append(SafeExportTools.collectionToString(cellSubject.ids));
				}
			} else if (subjectName.equals(SafeExport.SUBJECT_VOXEL_INDEX)) {
				SafeExportSubjectVoxelToExport voxelSubject = (SafeExportSubjectVoxelToExport) subjectToExport;
				subjectToExport=voxelSubject;
				if (voxelSubject.exportAll) {
					sb.append(Translator.swap("SafeExport.all")) ;
				}
				else {
					sb.append(Translator.swap("SafeExport.cellIds") + " : ");
					sb.append(SafeExportTools.collectionToString(voxelSubject.cellIds));
					sb.append(" ; ");
					sb.append(Translator.swap("SafeExport.depths")+ " : ") ;
					sb.append(SafeExportTools.collectionToString(voxelSubject.depths));
				}
			} else {
				SafeExportSubjectToExport defaultSubject = (SafeExportSubjectToExport) subjectToExport;
				subjectToExport=defaultSubject;;
			}
			sb.append(" (");
			sb.append(subjectToExport.getSafeExportVariables().length);
			sb.append(" " + Translator.swap("SafeExport.variables")+")");
			sb.append("\n");
		}
		// Subjects variables informations
//		sb.append("> " + Translator.swap("SafeExport.variables")+" : \n");
//		sb.append(Translator.swap("SafeExport.subject")+"\t");
//		sb.append(Translator.swap("SafeExport.subjectId")+"\t");
//		sb.append(Translator.swap("SafeExport.variableName")+"\t");
//		sb.append(Translator.swap("SafeExport.variableUnity")+"\t");
//		sb.append(Translator.swap("SafeExport.variableDescription")+"\n");
//		for (int i=0; i < profile.subjectsToExport.size();i++) {
//			SafeExportSubjectToExport subjectToExport = (SafeExportSubjectToExport) profile.subjectsToExport.get(i);
//			SafeExportVariable[] variables = subjectToExport.getSafeExportVariables();
//			for (int j=0;j<variables.length;j++) {
//				sb.append(subjectToExport.getSubject().getName()+ "\t");
//				sb.append(subjectToExport.getSubject().getLocalName()+ "\t");
//				sb.append(variables[j].getVariable ()+"\t");
//				sb.append(variables[j].getUnity ()+"\t");
//				sb.append(variables[j].getDescription ()+"\n");
//			}
//		}
		add (new FreeRecord (sb.toString ()));
	}

	/**
	 * create recordsets for default export subjects
	 */
	private void createDefaultRecords(Collection dest, String subjectName,SafeExportVariable[] variables,Step step) {
		String subjectName2= subjectName;
		// Source object used for calling the subjectName method is the model
		SafeModel model = (SafeModel) step.getProject().getModel();
		Object object=null;
		Method method;

		try {
			// the method called subjectName doesn't exist ?
			method = SafeExportTools.getMethod(model,subjectName2);
			if (method==null) {
				subjectName2 = subjectName2.toUpperCase().charAt(0) + subjectName2.substring(1,subjectName2.length());
				subjectName2 = "get"+subjectName2;
				// try the method called "get"+subjectName
				method = SafeExportTools.getMethod(model,subjectName2);
			}
			// the method has been found ?  get the object returned by the method
			if (method!=null)
				object = method.invoke (model);	// fc - 2.12.2004 - varargs
		} catch (Exception e) {
			System.out.println("Error in SafeExport.createDefaultRecords : " + e);
			object=null;
		}

		// method not found or not success during the invoke of the found method
		if (object==null) {
			dest.add("Access error to object " + subjectName + "\n");
			return;
		}

		boolean readed;
		StringBuffer write= new StringBuffer("");

		// for all subject's variables to export
		for (int i=0 ; i < variables.length ; i++) {

			// write the variableName and a separator
			write.append(variables[i].getVariable() +" \t");
			StringBuffer varEcr= new StringBuffer("");

			// try to invoke the "get"+VariableName method
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),varEcr);
			if (!readed) {
				varEcr= new StringBuffer("");
				// try to read the variable value as it would be a field/member of the object
				readed= SafeExportTools.readPrimitiveField(object,variables[i].getVariable(),varEcr);
			}
			if (readed)
				write.append(varEcr);
			else
				write.append("error!");

			write.append("\n");
		}
		// add all what have been written in the destination vector
		dest.add(write.toString());
	}

	/**
	 * create recordsets for tree subjects
	 */
	private void createStepTreeRecords(TreeSet dest, SafeExportVariable[] variables, Step step, int stepId, Object object) {
		StringBuffer write= new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId=0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();
		
		write.append(step.getProject().getName()+ '\t' +dateStart+ '\t' +step.getScene().getCaption()+ '\t'  +ts[0] +  '\t' +ts[1] +  '\t'+ts[2] +   '\t'+julian +  '\t');	

		// for all subject variables
		for (int i=0 ; i < variables.length ; i++) {
			val=new StringBuffer("");

			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),val);
			if (!readed) {
				val=new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which return array)
				readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Size",val);
				if (readed) {
					 arraySize=Integer.parseInt(val.toString());
					 values = new String [arraySize];
					 // fill the array values with readed variables
					 readed=SafeExportTools.readArrayOfFloatVariables(object,variables[i].getVariable(),values);
					 val=new StringBuffer("");
					 // put the values in a line of fields
					 for (int j=0;j<values.length;j++) {
					 	val.append(values[j]);
					 	if (j!=values.length-1) { val.append("\t"); }
					 }
				}
			}

			if (!readed) {
				// try to read *in SafeRoot* the value using the "get"+nomVariable" method
				val=new StringBuffer("");
				SafePlantRoot safeRoot=null;
			 	safeRoot = ((SafeTree) object).getPlantRoots();
			 	readed = SafeExportTools.readPrimitiveVariable (safeRoot,variables[i].getVariable(),val);
			}



			if (readed) write.append(val.toString());
			if (!readed) write.append("error!");


			// the readed value contains the treeId (variable added during init subject tree)
			if (i==0)
				objectId=Integer.parseInt(val.toString());

			// is it the last variable ?
			if (i!=variables.length-1)
				write.append('\t');
		}
		// create a record with the written line and add it to the destination vector
		SafeExportRecord record= new SafeExportRecord(stepId,objectId,write.toString());
		dest.add(record);
	}

	/**
	 * create recordsets for cell subjects
	 */
	private void createStepCellRecords(TreeSet dest, SafeExportVariable[] variables,Step step,
										int stepId, Object object, boolean control) {
		StringBuffer write= new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId=0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();
				
		write.append(step.getProject().getName()+ '\t' +dateStart+ '\t' +step.getScene().getCaption()+ '\t'  +ts[0] +  '\t' +ts[1] +  '\t'+ts[2] +   '\t'+julian +  '\t');	

		// for all subject variables
		for (int i=0 ; i < variables.length ; i++) {
			val=new StringBuffer("");
			readed=false;
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),val);
			if (!readed) {
					val=new StringBuffer("");
					// try to read the variable called "get"+nomVariable+"Size" (for variables which return array)
					readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Size",val);
					if (readed) {
						 arraySize=Integer.parseInt(val.toString());
						 values = new String [arraySize];
						 // fill the array values with readed variables
						 readed=SafeExportTools.readArrayOfFloatVariables(object,variables[i].getVariable(),values);
						 val=new StringBuffer("");
						 // put the values in a line of fields
						 for (int j=0;j<values.length;j++) {
							val.append(values[j]);
							if (j!=values.length-1) { val.append("\t"); }
						 }
					}
				}
			if (!readed) {
				// try to read *in SafeCrop* the value using the "get"+nomVariable" method
				val=new StringBuffer("");
				SafeCrop safeCrop = ((SafeCell) object).getCrop();
				if (safeCrop != null) {
					readed = SafeExportTools.readPrimitiveVariable(safeCrop,variables[i].getVariable(),val);
	
	
					if (!readed) {
						// try to read *in SafeRoot* the value using the "get"+nomVariable" method
						val=new StringBuffer("");
						 SafePlantRoot safeRoot=null;
						 safeRoot = (SafePlantRoot) (safeCrop.getPlantRoots());
						 if (safeRoot != null) readed = SafeExportTools.readPrimitiveVariable(safeRoot,variables[i].getVariable(),val);
					}
				}


			}
			if (readed)
				write.append(val.toString());
			else
				write.append("error!");

			// the readed value contains the cellId (variable added during init subject cell)
			if ((i==0) && (val !=null))
				objectId=Integer.parseInt(val.toString());

			// is it the last variable ?
			if (i !=variables.length-1)
				write.append('\t');

		}
		// create a record with the written line and add it to the destination vector
		SafeExportRecord record= new SafeExportRecord(stepId,objectId,write.toString());
		dest.add(record);
	}


	/**
	 * create recordsets for voxel subjects
	 */
	private void createStepVoxelRecords(TreeSet dest, SafeExportVariable[] variables ,Step step, int stepId, Object object) {
		StringBuffer write= new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId=-1;
		int cellId=-1;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();
		
		write.append(step.getProject().getName()+ '\t' +dateStart+ '\t' +step.getScene().getCaption()+ '\t'  +ts[0] +  '\t' +ts[1] +  '\t'+ts[2] +   '\t'+julian +  '\t');	


	// for all subject variables
		for (int i=0 ; i < variables.length ; i++) {
			val=new StringBuffer("");
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),val);

			if (!readed) {
				val=new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which return array)
				readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Size",val);

				if (readed) {
					 arraySize=Integer.parseInt(val.toString());
					 values = new String [arraySize];
					 					 
					val=new StringBuffer("");

					readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Type",val);	 

					if (readed) {
						 String type=(val.toString());
				 
						 // fill the array values with readed variables
						 if (type.equals("Double")) 
							 readed=SafeExportTools.readArrayOfDoubleVariables(object,variables[i].getVariable(),values);
	
						 if (type.equals("Float")) 
							 readed=SafeExportTools.readArrayOfFloatVariables(object,variables[i].getVariable(),values);
				
						 if (type.equals("Int")) 
							 readed=SafeExportTools.readArrayOfIntVariables(object,variables[i].getVariable(),values);

						if (readed) {
							val=new StringBuffer("");
							 // put the values in a line of fields
							 for (int j=0;j<values.length;j++) {
							 	val.append(values[j]);
							 	if (j!=values.length-1) { val.append("\t"); }
							 }
					 	}
					}
				}
				
			}

			
			if (!readed) {
				// try to read *in SafeCrop* the value using the "get"+nomVariable" method
				val=new StringBuffer("");
				SafeLayer safeLayer = ((SafeVoxel) object).getLayer();
				readed = SafeExportTools.readPrimitiveVariable(safeLayer,variables[i].getVariable(),val);
			}
			
	
			
			// is the index of the current variable is one of those with which variables
			// have been added in the init of the voxel subject ?
			// Note only 2 of the added variables are processed here because the others are processed like normal variabkles
			if (i==VOXEL_INDEX_ID_VOXEL) objectId=Integer.parseInt(val.toString());
			if (i==VOXEL_INDEX_ID_CELL) cellId=Integer.parseInt(val.toString());
			if (readed) write.append (val.toString());
			if (!readed) write.append ("error!");

			// is it the last variable ? if not add a separator
			if (i !=variables.length-1) write.append('\t');
		}
		// create a record with the written line and add it to the destination vector
		SafeExportRecord record= new SafeExportRecord(stepId,cellId,objectId,write.toString());
		dest.add(record);
	}

	


	/**
	 * create recordsets for CLIMATE subjects
	 */
	private void createStepClimateRecords(TreeSet dest, SafeExportVariable[] variables, Step step, int stepId, Object object) {
		StringBuffer write= new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId=0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();
		
		write.append(step.getProject().getName()+ '\t' +dateStart+ '\t' +step.getScene().getCaption()+ '\t'  +ts[0] +  '\t' +ts[1] +  '\t'+ts[2] +   '\t'+julian +  '\t');	


		// for all subject variables
		for (int i=0 ; i < variables.length ; i++) {
			val=new StringBuffer("");
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),val);
			if (!readed) {
				val=new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which return array)
				readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Size",val);
					if (readed) {
						 arraySize=Integer.parseInt(val.toString());
						 values = new String [arraySize];
						 // fill the array values with readed variables
						 readed=SafeExportTools.readArrayOfFloatVariables(object,variables[i].getVariable(),values);
						 val=new StringBuffer("");
						 // put the values in a line of fields
						 for (int j=0;j<values.length;j++) {
							val.append(values[j]);
							if (j!=values.length-1) { val.append("\t"); }
						 }
					}
				}


				if (readed) write.append(val.toString());
				if (!readed) write.append("error!");

						// is it the last variable ? if not add a separator
				if (i !=variables.length-1) write.append('\t');

			// try to read the value using the "get"+nomVariable" method

		}
		// create a record with the written line and add it to the destination vector
		SafeExportRecord record= new SafeExportRecord(stepId,objectId,write.toString());
		dest.add(record);

	}

	/**
	 * create recordsets for PLOT subjects
	 */
	private void createStepPlotRecords(TreeSet dest, SafeExportVariable[] variables, Step step, int stepId, Object object) {
		StringBuffer write= new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId=0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();
		
		write.append(step.getProject().getName()+ '\t' +dateStart+ '\t' +step.getScene().getCaption()+ '\t'  +ts[0] +  '\t' +ts[1] +  '\t'+ts[2] +   '\t'+julian +  '\t');	


		// for all subject variables
		for (int i=0 ; i < variables.length ; i++) {
			val=new StringBuffer("");
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),val);
			if (!readed) {
				val=new StringBuffer("");
				// try to read the variable called "get"+nomVariable+"Size" (for variables which return array)
				readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Size",val);
				if (readed) {
					 arraySize=Integer.parseInt(val.toString());
					 values = new String [arraySize];
					 // fill the array values with readed variables
					 readed=SafeExportTools.readArrayOfFloatVariables(object,variables[i].getVariable(),values);
					 val=new StringBuffer("");
					 // put the values in a line of fields
					 for (int j=0;j<values.length;j++) {
						val.append(values[j]);
						if (j!=values.length-1) { val.append("\t"); }
					 }
				}
			}

			if (!readed) {
				// try to read *in SafePlot* the value using the "get"+nomVariable" method
				val=new StringBuffer("");
				SafePlot plot=null;
			 	plot = (SafePlot) ((SafeStand) object).getPlot();
			 	readed = SafeExportTools.readPrimitiveVariable(plot, variables[i].getVariable(), val);
			}

			if (!readed) {
				// try to read *in SafeSoil* the value using the "get"+nomVariable" method
				val=new StringBuffer("");
				SafeSoil soil = null;
			 	soil = ((SafeStand) object).getPlot().getSoil();
			 	readed = SafeExportTools.readPrimitiveVariable(soil, variables[i].getVariable(), val);
			}


			if (readed) write.append(val.toString());
			if (!readed) write.append("error!");

			// is it the last variable ? if not add a separator
			if (i !=variables.length-1) write.append('\t');

		}
		// create a record with the written line and add it to the destination vector
		SafeExportRecord record= new SafeExportRecord(stepId,objectId,write.toString());
		dest.add(record);

	}

	/**
	 * create recordsets for LAYER subjects
	 */
	

	private void createStepLayerRecords(TreeSet dest, SafeExportVariable[] variables,Step step,
										int stepId, Object object, boolean control) {
		StringBuffer write= new StringBuffer("");
		StringBuffer val;
		boolean readed;
		int objectId=0;
		int arraySize;
		String values[];

		// write step information (and a separator)
		String s = step.getScene().getCaption();
		String ts[] = s.split("/");
		int julian = ((SafeStand) step.getScene()).getJulianDay();
		Date dateStart = ((SafeStand) step.getScene()).getStartDate();
		
		write.append(step.getProject().getName()+ '\t' +dateStart+ '\t' +step.getScene().getCaption()+ '\t'  +ts[0] +  '\t' +ts[1] +  '\t'+ts[2] +   '\t'+julian +  '\t');	

		// for all subject variables
		for (int i=0 ; i < variables.length ; i++) {
			val=new StringBuffer("");
			readed=false;
			// try to read the value using the "get"+nomVariable" method
			readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable(),val);
			if (!readed) {
					val=new StringBuffer("");
					// try to read the variable called "get"+nomVariable+"Size" (for variables which return array)
					readed = SafeExportTools.readPrimitiveVariable(object,variables[i].getVariable()+"Size",val);
					if (readed) {
						 arraySize=Integer.parseInt(val.toString());
						 values = new String [arraySize];
						 // fill the array values with readed variables
						 readed=SafeExportTools.readArrayOfFloatVariables(object,variables[i].getVariable(),values);
						 val=new StringBuffer("");
						 // put the values in a line of fields
						 for (int j=0;j<values.length;j++) {
							val.append(values[j]);
							if (j!=values.length-1) { val.append("\t"); }
						 }
					}
				}
			
			if (readed)
				write.append(val.toString());
			else
				write.append("error!");

			// the readed value contains the cellId (variable added during init subject cell)
			if ((i==0) && (val !=null))
				objectId=Integer.parseInt(val.toString());

			// is it the last variable ?
			if (i !=variables.length-1)
				write.append('\t');

		}
		// create a record with the written line and add it to the destination vector
		SafeExportRecord record= new SafeExportRecord(stepId,objectId,write.toString());
		dest.add(record);
	}
	
	/**
	 * write in file the collection of string contained in the argument records (and an empty record at the end)
	 * before writting the records :
	 * - write a comment line which contains the subject name
	 * - write a line which contains the name of all readed variables
	 */
	private void writeHeader(SafeExportVariable[] variables, Object object) {
			add(new FreeRecord(createHeadLine(variables,object)));
	}
	
	/**
	 * write in file the collection of string contained in the argument records (and an empty record at the end)
	 * before writting the records :
	 * - write a comment line which contains the subject name
	 * - write a line which contains the name of all readed variables
	 */
	private void writeRecords(Collection records) {

		if (records.size()>0) {
			for (Iterator i = records.iterator (); i.hasNext () ; ) {
				SafeExportRecord record = (SafeExportRecord) i.next();
				add(new FreeRecord(record.getRecord()));
			}
		}
	}

	/**
	 * return a string "head/title line" which contained all variable names (separated by a tab character)
	 * - Specific process is define for variables which returned an array instead of a "single" value
	 * - this method is only to be used with not default subject (default subjects used writeDefaultRecords)
	 */
	private String createHeadLine(SafeExportVariable[] variables,Object o) {
		StringBuffer write= new StringBuffer("");
		boolean readed;
		int sizeVariableArray;
		StringBuffer value;
		// write a title for the step data
		write.append("SimulationName" + '\t' + "SimulationDate"+  '\t' + "Date"+ '\t' + "Day" + '\t' + "Month" + '\t'+ "Year" + '\t'+ "JulianDay" + '\t');
		// for all variables...
		for (int i=0 ; i < variables.length ; i++) {
			readed=false;
			sizeVariableArray=0;
			value=new StringBuffer("");
			// check if the current variable returns an array of values
			readed=SafeExportTools.readPrimitiveVariable(o,variables[i].getVariable()+"Size",value);
			if (readed) {
				// the variable returns an array ; variable names are created using the array size
				sizeVariableArray= Integer.parseInt(value.toString());
				if (sizeVariableArray==0) {
					write.append(variables[i].getVariable());
				}
				else {
					for (int j=0;j <sizeVariableArray;j++) {
						write.append(variables[i].getVariable()+"_"+(j+1));
						if (j !=sizeVariableArray-1) { write.append("\t");}
					}
				}
			} else {
				// name of the variable is written
				write.append(variables[i].getVariable());
			}
			if (i !=(variables.length-1)) { write.append('\t');}
		}
		return write.toString ();
	}

	/**
	 * write in the file the default subject records
	 * default records are like this : variableName \t variableValue
	 */
	private void writeDefaultRecords(Collection records,String subjectName) {
		if (records.size()>0) {
			add(new CommentRecord(subjectName));
			for (Iterator i = records.iterator (); i.hasNext () ;)
			{
				String record = (String) i.next();
				add(new FreeRecord(record));
			}
			add(new FreeRecord(""));
		}
	}

	public String getFolderFileName() {
		return folderFileName; 	
	}

}
