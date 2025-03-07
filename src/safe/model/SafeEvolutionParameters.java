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

package safe.model;

import java.util.List;
import java.util.GregorianCalendar;
import capsis.kernel.EvolutionParameters;
import jeeb.lib.util.CancellationException;
import safe.pgms.SafeSimulationLoader;


/**
 * Evolution settings   
 * 
 * @author Isabelle Lecomte - INRAE Montpellier - July 2002
 */
public class SafeEvolutionParameters implements EvolutionParameters {

	public static final int NB_ZONE_MAX = 10; 			// max number of crop zone
	
	public static final int TORIC_X_POS = 1000; 		// toric symetry on x axis positive (number of tours allowed)
	public static final int TORIC_X_NEG = 1000; 		// toric symetry on y axis negative (number of tours allowed)
	public static final int TORIC_Y_POS = 1000; 		// toric symetry on x axis positive (number of tours allowed)
	public static final int TORIC_Y_NEG = 1000; 		// toric symetry on y axis negative (number of tours allowed)
	public int toricXp;
	public int toricXn;
	public int toricYp;
	public int toricYn;
	
	// SIMULATION PARAMETERS
	public String simulationDir;						//path to the simulation directory
	public GregorianCalendar simulationDateStart;		//simulation Date Start (AAAA/MM/JJ) 
	public GregorianCalendar simulationDateEnd;			//simulation Date END (AAAA/MM/JJ) 
	public boolean firstSimulation;						//if TRUE this is the first simulation
	public String weatherFile; 							//path to the weather file name
	
	// CROP ZONE DEFINITION
	public List<Integer>  	zoneId;						//id of the zone
	public List<String>  	zoneName;					//name of the zone  
	public List<String> 	zoneCellList;	 			//list of the cells composing the zone separated by , and -
	public List<String> 	zoneTecList;				//list of the crop itk attached to the zone (one per crop rotation) separated by , 
	
	//TREE ITK
	public List<String> 	treeTecList;				//list of the tree itk attached to the trees (one per tree) separated by , 


	// EXPORTATION
	public String exportDir;							//path to the export directory
	public boolean sticsReport; 						//if TRUE = export STICS report and logs   

	/**
	 * Constructor.
	 */
	public SafeEvolutionParameters(SafeSimulationLoader loader,
									String simulationDir, 
									String exportDir, 
									String weatherFile) throws Exception {
		
		this.weatherFile = weatherFile;
		this.toricXp = TORIC_X_POS; 
		this.toricXn = TORIC_X_NEG;
		this.toricYp = TORIC_Y_POS;
		this.toricYn = TORIC_Y_NEG;
		this.sticsReport = false;
		
		//BATCH MODE
		if (loader != null) {
			
			if (loader.sticsReport==1) this.sticsReport = true;
			
			this.simulationDir = simulationDir; 
			this.exportDir = exportDir;
			
			this.simulationDateStart = loader.simulationDateStart;	
			this.simulationDateEnd = loader.simulationDateEnd;	

			//CROP ZONE
			this.zoneId = loader.zoneId;
			this.zoneName = loader.zoneName;
			this.zoneCellList = loader.zoneCellList;
			this.zoneTecList = loader.zoneTecList;
			
			//TREE ITK
			this.treeTecList = loader.treeTecList;
			
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
