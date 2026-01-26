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
 * SIM file loader for Hi-sAFe BATCH simulation.
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

	// SIMULATION PARAMETERS
	/** Simulation date start (AAAA/MM/JJ) */
	public GregorianCalendar simulationDateStart;		
	/** Simulation date end (AAAA/MM/JJ) */
	public GregorianCalendar simulationDateEnd;			
	/** 0=NO 1=debug traces */
	public int debugMode;							
	/** 0=NO 1=YES export STICS logs and reports  */
	public int sticsReport;
	/** 0=NO 1=YES project will be saved   */
	public int saveProjectOption; 					
	/** project file name if restarted  */
	public String projectFileName;		
	/** lai file name to force lai option  */
	public String laiFileName = "";	
	/** Simulation toric symmetry on X axis positive (number of tours allowed) */
	public int toricXp;
	/** Simulation toric symmetry on X axis negative (number of tours allowed) */
	public int toricXn;
	/** Simulation toric symmetry on Y axis positive (number of tours allowed) */
	public int toricYp;
	/** Simulation toric symmetry on Y axis negative (number of tours allowed) */
	public int toricYn;
	
	// CROP ZONE DEFINITION
	/** Zones id list */
	public List<Integer>  	zonesIds;
	/** Zones names list */
	public List<String>  	zonesNames;	  
	/** list of the cells ids attached to the zones (one per zone) separated by , */
	public List<String> 	zonesCellsList;	
	/** list of the tec file attached to the zones (one per zone) separated by , */
	public List<String> 	zonesTecsList;
	/** list of the tec file attached to the trees (one per tree) separated by , */
	public List<String> 	treeTecsList;	
	
	
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

		zonesIds = new ArrayList<Integer>();
		zonesNames = new ArrayList<String>();
		zonesCellsList = new ArrayList<String>();
		zonesTecsList = new ArrayList<String>();
		treeTecsList = new ArrayList<String>();

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
					zonesIds.add(zoneIndex);
					zonesNames.add(r.name);
					zonesCellsList.add(r.listCell);
					zonesTecsList.add(r.listItk);
				}
			}
			//TREE TEC LIST
			else if (record instanceof SafeSimulationLoader.TreeTec) {
				
				SafeSimulationLoader.TreeTec r = (SafeSimulationLoader.TreeTec) record;	// cast to precise type

				if (r.lib.equals("TREETEC")) {
					treeTecsList.add(r.itkFileName);
				}			
			}
			
			//SIMULATION PARAMETERS 
			else if (record instanceof SafeSimulationLoader.KeyRecord) {

				SafeSimulationLoader.KeyRecord r = (SafeSimulationLoader.KeyRecord) record;
				
				String param = r.key;

				if (param.equals("projectFileName")) {
					projectFileName = r.value;
				} else if (param.equals("laiFileName")) {
					laiFileName = r.value;
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
