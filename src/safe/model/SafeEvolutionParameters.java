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

package safe.model;

import java.util.List;
import java.util.GregorianCalendar;
import capsis.kernel.EvolutionParameters;
import jeeb.lib.util.CancellationException;
import safe.pgms.SafeSimulationLoader;

/**
 * EVOLUTION parameters   
 * 
 * @author : Isabelle Lecomte - INRAE (UMR-SYSTEM), University of Montpellier, France
 */
public class SafeEvolutionParameters implements EvolutionParameters {

	// STATIC VALUES
	/** Max number of crop zone */
	public static final int NB_ZONE_MAX = 10; 			
	/** Default toric symmetry on X axis positive (number of tours allowed) */
	public static final int TORIC_X_POS = 1000; 		// 
	/** Default toric symmetry on X axis negative (number of tours allowed) */
	public static final int TORIC_X_NEG = 1000; 		
	/** Default toric symmetry on Y axis positive (number of tours allowed) */
	public static final int TORIC_Y_POS = 1000; 		
	/** Default toric symmetry on Y axis negative (number of tours allowed) */
	public static final int TORIC_Y_NEG = 1000; 		

	// SIMULATION PARAMETERS
	/** Simulation date start (AAAA/MM/JJ) */
	public GregorianCalendar simulationDateStart;		
	/** Simulation date end (AAAA/MM/JJ) */
	public GregorianCalendar simulationDateEnd;			
	/** if TRUE this is the first simulation */
	public boolean firstSimulation;	
	/** Simulation folder path */
	public String simulationPath;	
	/** Weather file path  */
	public String weatherPath; 							
	/** LAI file path  */
	public String laiPath; 	
	/** Output folder path */
	public String outputPath;							
	/** if TRUE = export STICS report and logs  */
	public boolean sticsReport;
	/** Simulation toric symmetry on X axis positive (number of tours allowed) */
	public int toricXp;
	/** Simulation toric symmetry on X axis negative (number of tours allowed) */
	public int toricXn;
	/** Simulation toric symmetry on Y axis positive (number of tours allowed) */
	public int toricYp;
	/** Simulation toric symmetry on Y axis negative (number of tours allowed) */
	public int toricYn;
	
	// ZONES DEFINITION
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
	 * Constructor.
	 * @param loader Reference to SafeSimulationLoader object (parameters load in batch mode)
	 * @param simulationPath Simulation folder path
	 * @param outputPath Output folder path
	 * @param weatherPath Weather file path
	 * @param laiPath Lai observed file path
	 */
	public SafeEvolutionParameters(SafeSimulationLoader loader,
									String simulationPath, 
									String outputPath, 
									String weatherPath,
									String laiPath) throws Exception {

		//BATCH MODE
		if (loader != null) {
			
			this.simulationDateStart = loader.simulationDateStart;	
			this.simulationDateEnd = loader.simulationDateEnd;
			this.simulationPath = simulationPath; 
			this.outputPath = outputPath;
			this.weatherPath = weatherPath;
			this.laiPath = laiPath;
			this.sticsReport = false;
			if (loader.sticsReport==1) this.sticsReport = true;
	
			//CROP ZONE
			this.zonesIds = loader.zonesIds;
			this.zonesNames = loader.zonesNames;
			this.zonesCellsList = loader.zonesCellsList;
			this.zonesTecsList = loader.zonesTecsList;
			
			//TREE ITK
			this.treeTecsList = loader.treeTecsList;
			
			//TORIC SYMETRIE parameter set
			this.toricXp = loader.toricXp * TORIC_X_POS;
			this.toricXn = loader.toricXn * TORIC_X_NEG;
			this.toricYp = loader.toricYp * TORIC_Y_POS;
			this.toricYn = loader.toricYn * TORIC_Y_NEG;
		}
		
		//INTERACTIVE MODE
		else {
			System.out.println("Interactive mode is no more possible wuth this version ");
			throw new CancellationException();	// abort

		}
	}
}
