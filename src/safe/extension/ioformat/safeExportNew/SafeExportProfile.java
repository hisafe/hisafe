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

	// fc-4.12.2020 Checked, is an export, added OFormat

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
