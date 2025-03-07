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
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import jeeb.lib.util.Translator;
import safe.model.*;
import capsis.defaulttype.plotofcells.PlotOfCells;

/**
  * SafeExportProfile
  *
  * @author Isabelle Lecomte - January 2021
  */
public class SafeExportProfile implements  Serializable {

	private static final long serialVersionUID = 1L;

	// List of export objects 
	public static String OBJECT_TREE 	= "SafeTree";
	public static String OBJECT_CELL 	= "SafeCell";
	public static String OBJECT_VOXEL 	= "SafeVoxel";
	public static String OBJECT_ZONE 	= "SafeCropZone";
	public static String OBJECT_PLOT 	= "SafePlot";
	public static String OBJECT_LAYER 	= "SafeLayer";
	public static String OBJECT_CLIMATE = "SafeMacroClimat";

	private String fileName;				//name of the file where to export
	private String subject;					//subject (free name) 
	private String object;					//safe object 
	private int frequency;					//export frequency
	private ArrayList<String> variables;	//list of variables
	private List<String> ids;				//id selection
	private List<String> depths;			//depth selection
	private List<Integer> idsToExport;			//cell or tree id selection
	private List<Integer> voxelIdsToExport;		//voxel id selection
	
	static { Translator.addBundle("safe.extension.ioformat.safeExportNew.SafeExportProfile"); }


	/**
	* Phantom constructor.
	* Only to ask for extension properties (authorName, version...).
	*/
	public SafeExportProfile () throws Exception {

	}


	public SafeExportProfile (String outputDir, String projectName, String subject, String object, int frequency) throws Exception  {

		this.fileName = outputDir+"/"+ projectName + '_'+subject+".txt";
		this.subject = subject;
		this.object = object;
		this.frequency = frequency;
		
		this.variables = new ArrayList<String>();
		this.idsToExport = new ArrayList<Integer>();
		this.voxelIdsToExport = new ArrayList<Integer>();
		
		this.addDefault ();
	}
	
	public SafeExportProfile (String outputDir, String projectName, String subject,  String object, int frequency, String ids) throws Exception  {
		this.fileName = outputDir+"/"+ projectName + '_'+subject+".txt";
		this.subject = subject;
		this.object = object;
		this.frequency = frequency;
		this.ids = new ArrayList<String>(Arrays.asList(ids.split(";")));

		this.variables = new ArrayList<String>();
		this.idsToExport = new ArrayList<Integer>();
		this.voxelIdsToExport = new ArrayList<Integer>();
		
		this.addDefault ();
	}
	
	public SafeExportProfile (String outputDir, String projectName, String subject,  String object, int frequency, String ids, String dps) throws Exception  {	
		this.fileName = outputDir+"/"+ projectName + '_'+subject+".txt";
		this.subject = subject;
		this.object = object;
		this.frequency = frequency;
		this.ids = new ArrayList<String>(Arrays.asList(ids.split(";")));
		this.depths = new ArrayList<String>(Arrays.asList(dps.split(";")));
		
		this.variables = new ArrayList<String>();
		this.idsToExport = new ArrayList<Integer>();
		this.voxelIdsToExport = new ArrayList<Integer>();
		
		this.addDefault ();
	}
	

	
	
	//adding default columns
	private void addDefault () {
		if (object.equals(OBJECT_TREE)) this.addVariable("idTree");
		if (object.equals(OBJECT_ZONE)) {
			this.addVariable("id");
			this.addVariable("name");
		}
		if (object.equals(OBJECT_CELL))  {
			this.addVariable("idZone");
			this.addVariable("zoneName");
			this.addVariable("idCell");
			this.addVariable("x");
			this.addVariable("y");
		}
		if (object.equals(OBJECT_VOXEL)) {
			this.addVariable("idZone");
			this.addVariable("idCell");
			this.addVariable("idVoxel");
			this.addVariable("x");
			this.addVariable("y");
			this.addVariable("z");
		}
		if (object.equals(OBJECT_LAYER)) {

			this.addVariable("idLayer");
			this.addVariable("thickness");
			this.addVariable("surfaceDepth");
		}
		if (object.equals(OBJECT_CLIMATE)) this.addVariable("realYear");
		
	}
	
	
	
	/**
	 * Selection of ids (tree, cell, voxels)
	 */
	public void selectIds(SafeStand s) {

		PlotOfCells plotc = (PlotOfCells) s.getPlot(); // fc-30.10.2017
		

		//ids selection
		if ((ids!= null) && (ids.size()>0) ){
			for (Iterator c = ids.iterator(); c.hasNext();) {
				String ids = (String) c.next();
				String [] objectToExport = ids.split("-");
	
				int idmin = Integer.parseInt(objectToExport[0]);
				int idmax = idmin;			
				if (objectToExport.length == 2)  idmax = Integer.parseInt(objectToExport[1]);
				
				if (this.getObject().equals(OBJECT_TREE)) {
					for (Iterator it2=s.getTrees().iterator();it2.hasNext();) {
						SafeTree tree = (SafeTree) it2.next();				
						if ((tree.getId() >= idmin) && (tree.getId() <= idmax)) {
							idsToExport.add(tree.getId());
						}
					}
				}
				else if (this.getObject().equals(OBJECT_ZONE)) {
				
					for (Iterator it2=((SafePlot)plotc).getCropZones().iterator();it2.hasNext();) {
						SafeCropZone zone = (SafeCropZone) it2.next();
						if ((zone.getId() >= idmin) && (zone.getId() <= idmax)) {
							idsToExport.add(zone.getId());
						}
					}

					
				} else if (this.getObject().equals(OBJECT_CELL)) {

					
					for (Iterator it2=plotc.getCells().iterator();it2.hasNext();) {
						SafeCell cell = (SafeCell) it2.next();
	
						if ((cell.getId() >= idmin) && (cell.getId() <= idmax)) {
							idsToExport.add(cell.getId());
						}
					}
					
				} else if (this.getObject().equals(OBJECT_VOXEL)) {
					for (Iterator it2=plotc.getCells().iterator();it2.hasNext();) {
						SafeCell cell = (SafeCell) it2.next();
	
						if ((cell.getId() >= idmin) && (cell.getId() <= idmax)) {
							idsToExport.add(cell.getId());
						}
					}
				}
			}
		}
		//depth selection 
		if ((depths!= null) && (depths.size()>0) ){
			for (Iterator c = depths.iterator(); c.hasNext();) {
				String dps = (String) c.next();
				String [] depthToExport = dps.split("-");
				
				int depthmin = Integer.parseInt(depthToExport[0]);
				int depthmax = depthmin;			
				if (depthToExport.length == 2)  depthmax = Integer.parseInt(depthToExport[1]);


				if (this.getObject().equals(OBJECT_VOXEL)) {
					for (Iterator it2=plotc.getCells().iterator();it2.hasNext();) {
						SafeCell cell = (SafeCell) it2.next();
						SafeVoxel[] voxels = cell.getVoxels();
						//FOR EACH VOXEL
						for (int v = 0; v < voxels.length; v++) {
							//check if voxel depth is in the limts 
							double depthVoxel = voxels[v].getZ();
							if ((depthVoxel >= depthmin) && (depthVoxel <= depthmax)) {
								voxelIdsToExport.add(voxels[v].getId());
							}
						}
					}
				}
			}
		}	
	}
	
	//adding a variable to export
	public void addVariable (String name) {
		variables.add(name); 
	}

	public ArrayList<String> getVariables() {
		return variables;
	}
	
	public String getObject() {
		return object;
	}

	
	public String getSubject() {
		return subject;
	}

	public int getFrequency() {
		return frequency;
	}
	
	public List<Integer> getIdsToExport() {
		return idsToExport;
	}
	
	public List<Integer> getVoxelIdsToExport() {
		return voxelIdsToExport;
	}
	
	public String toString(){
		String str = "";
		str = "Subject= "+getSubject()+" fileName="+fileName+" Frequency="+getFrequency()+" Variable="+this.variables;
		return str;
	}
	
	
	public String getFileName () {
		return fileName;
	}
	


}
