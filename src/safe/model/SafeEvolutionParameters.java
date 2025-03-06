/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
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
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
 *
 */
package safe.model;

import java.util.List;
import java.util.GregorianCalendar;
import capsis.kernel.EvolutionParameters;
import jeeb.lib.util.CancellationException;
import jeeb.lib.util.PathManager;
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
