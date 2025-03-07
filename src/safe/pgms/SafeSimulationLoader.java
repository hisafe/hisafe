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

package safe.pgms;

import java.util.ArrayList;
import java.util.GregorianCalendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.Import;
import jeeb.lib.util.Record;
import jeeb.lib.util.RecordSet;


/**
 * A loader for an HiSAFE BATCH simulation.
 * 
 * @author Isabelle Lecomte - INRAE Montpellier - july 2009 
 */
public class SafeSimulationLoader extends RecordSet {

	@Import
	static public class CropZone extends Record {
		private static final long serialVersionUID = 1L;
		public String lib;    		//lib = Zones
		public String name;    		//zone name 
		public String listCell;    	//list of cell ids composing the zone 
		public String listItk;    	//list of crop itk file names for the zone
		
		public CropZone () {super ();}
		public CropZone (String line) throws Exception {super (line);}
	}
	@Import
	static public class TreeTec extends Record {
		private static final long serialVersionUID = 1L;
		public String lib;    		//lib = TreeTec
		public String itkFileName;  //name of tree itk file name 
		
		public TreeTec () {super ();}
		public TreeTec (String line) throws Exception {super (line);}
	}
	private static final long serialVersionUID = 1L;

	//SIMULATION PARAMETERS 
	public GregorianCalendar simulationDateStart;		//simulation date start (YYYY/MM/DD) 
	public GregorianCalendar simulationDateEnd;			//simulation date end (YYYY/MM/DD) 
	public String projectFileName;						//project file name if restarted
	public int saveProjectOption; 						//0=NO 1=YES project will be saved 
	public int debugMode;								//0=NO 1=debug traces
	public int sticsReport;								//0=NO 1=Edit STICS report (1 for each cell each year) 
	
	// CROP ZONE DEFINITION
	public List<Integer>  	zoneId;	
	public List<String>  	zoneName;	
	public List<String> 	zoneCellList;	
	public List<String> 	zoneTecList;
	
	//TREE TEC LIST
	public List<String> 	treeTecList;
	
	//TORIC SYMETRY OPTIONS ACTIVATION (0/1)
	public int toricXn; 
	public int toricXp;
	public int toricYn;
	public int toricYp;
	
	/**
	 * Constructor
	 */
	public SafeSimulationLoader(String fileName) throws Exception {
		prepareImport (fileName);
	}
	/**
	 * Load simulation parameters
	 */
	public void load() throws Exception {

		zoneId = new ArrayList<Integer>();
		zoneName = new ArrayList<String>();
		zoneCellList = new ArrayList<String>();
		zoneTecList = new ArrayList<String>();
		treeTecList = new ArrayList<String>();

		Set<String> requiredParameters = new HashSet<>();
		requiredParameters.add("simulationDateStart");
		requiredParameters.add("simulationDateEnd");
		requiredParameters.add("ZONE");	
		requiredParameters.add("toricXp");
		requiredParameters.add("toricXn");
		requiredParameters.add("toricYp");
		requiredParameters.add("toricYn");			
		
		projectFileName = "";
		debugMode = 0;
		sticsReport = 0;
		int zoneIndex = 0;

		for (Iterator<Record> i = this.iterator(); i.hasNext();) {
			Record record = i.next();

			// CROP ZONE DEFINITION
			if (record instanceof SafeSimulationLoader.CropZone) {
				
				SafeSimulationLoader.CropZone r = (SafeSimulationLoader.CropZone) record;	// cast to precise type

				if (r.lib.equals("ZONE")) {
					requiredParameters.remove("ZONE");
					zoneIndex++;
					zoneId.add(zoneIndex);
					zoneName.add(r.name);
					zoneCellList.add(r.listCell);
					zoneTecList.add(r.listItk);
				}
			}
			//TREE TEC LIST
			else if (record instanceof SafeSimulationLoader.TreeTec) {
				
				SafeSimulationLoader.TreeTec r = (SafeSimulationLoader.TreeTec) record;	// cast to precise type

				if (r.lib.equals("TREETEC")) {
					treeTecList.add(r.itkFileName);
				}			
			}
			
			//SIMULATION PARAMETERS 
			else if (record instanceof SafeSimulationLoader.KeyRecord) {

				SafeSimulationLoader.KeyRecord r = (SafeSimulationLoader.KeyRecord) record;

				String param = r.key;

				if (param.equals("projectFileName")) {
					projectFileName = r.value;
	
				} else if (param.equals("simulationDateStart")) {
					String [] part1 = r.value.split("-");
					simulationDateStart= new GregorianCalendar();
					simulationDateStart.set(Integer.parseInt(part1[0]),Integer.parseInt(part1[1])-1,Integer.parseInt(part1[2]) );
					requiredParameters.remove("simulationDateStart");

				} else if (param.equals("simulationDateEnd")) {
					String [] part1 = r.value.split("-");
					simulationDateEnd= new GregorianCalendar();
					simulationDateEnd.set(Integer.parseInt(part1[0]),Integer.parseInt(part1[1])-1,Integer.parseInt(part1[2]) );
					requiredParameters.remove("simulationDateEnd");

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

				} else if (param.equals("saveProjectOption")) {
					saveProjectOption = r.getIntValue();

				} else if (param.equals("debugMode")) {
					debugMode = r.getIntValue();
					
				} else if (param.equals("sticsReport")) {
					sticsReport = r.getIntValue();				
					
				}
			}
		}

		//missing required parameters
		if (!requiredParameters.isEmpty()) {
			System.out.println("Missing simulation parameters : " + AmapTools.toString(requiredParameters));
			throw new CancellationException();	// abort
		}
	}
}
