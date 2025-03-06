/*
 * This file is part of the Artemis module for Capsis 4.2.2
 *
 * Authors: M. Fortin and J-F Lavoie - Canadian Forest Service 
 * Copyright (C) 2020-24 His Majesty the King in right of Canada
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.util.extendeddefaulttype.webapi;

import java.io.File;
import java.io.PrintWriter;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import com.fasterxml.jackson.databind.ObjectMapper;

import capsis.kernel.Engine;
import capsis.util.extendeddefaulttype.ExtScript;
import repicea.app.Executable;
import repicea.io.javacsv.CSVReader;
import repicea.simulation.ApplicationScaleProvider.ApplicationScale;
import repicea.simulation.climate.REpiceaClimateGenerator.ClimateChangeScenarioHelper;
import repicea.simulation.scriptapi.CapsisWebAPICompatibleScript;
import repicea.simulation.scriptapi.ScriptResult;

/**
 * Handle simulation called from CapsisWebAPI.
 * @author J.-F. Lavoie - February 2023
 * @author M. Fortin - January 2024
 */
public class CapsisWebAPIScriptSimulation implements Executable {
	
	static final String InitialDateYrLabel = "initialDateYr";  	// Long from JSON
	static final String IsStochasticLabel = "isStochastic";		// Boolean from JSON
	static final String NbRealizationsLabel = "nbRealizations"; // Long from JSON
	static final String ApplicationScaleLabel = "applicationScale"; // String from JSON
	static final String ClimateChangeLabel = "climateChange";	// String from JSON
	static final String FinalDateYrLabel = "finalDateYr";	// Long from JSON
	static final String FieldMatchesLabel = "fieldMatches"; // Array of Long from JSON
	static final String FilenameLabel = "fileName";			// String from JSON
	static final String OutputRequestsLabel = "outputRequestList"; 
	
	final CapsisWebAPICompatibleScript script;
	final PrintWriter out;
	
	final int initialDateYr;
	final boolean isStochastic;
	final int nbRealizations;
	final ApplicationScale scale;
	final String climateChange;
	final int finalDateYr;
	final int[] fieldMatches;
	final String fileName; 
	final List<ProtoOutputRequest> protoOutputRequests;
	ScriptResult scriptResult;
	Exception failure;
	
	/**
	 * Constructor.
	 * @param script an ArtScript instance which calls an CapsisWebAPIScriptSimulation 
	 * @param simParams a series of parameters coming from a JSON string
	 * @param out a PrintWriter instance
	 */
	public CapsisWebAPIScriptSimulation(CapsisWebAPICompatibleScript script, String simParams, PrintWriter out) throws Exception {
		
		this.script = script;
		this.out = out;
		
		Map paramsMap = JsonReader.jsonToMaps(simParams);
						
		Long initialDateYr = (Long)paramsMap.get(InitialDateYrLabel);
		if (initialDateYr == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: initialDateYr not found in provided params");
		}
		this.initialDateYr = initialDateYr.intValue();
		
		Boolean isStochastic = (Boolean)paramsMap.get(IsStochasticLabel);
		if (isStochastic == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: isStochastic not found in provided params");
		}
		this.isStochastic = isStochastic.booleanValue();
		
		Long nbRealizations = (Long)paramsMap.get(NbRealizationsLabel);
		if (nbRealizations == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: nbRealizations not found in provided params");
		}
		this.nbRealizations = nbRealizations.intValue();
		
		
		String scaleStr = (String)paramsMap.get(ApplicationScaleLabel);
		if (scaleStr == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: applicationScale not found in provided params");
		} else {
			try {
				scale = ApplicationScale.valueOf(scaleStr);
			} catch (Exception ex) {
				throw new InvalidParameterException("CapsisWebAPIScriptSimulation: applicationScale value " + scaleStr + " cannot be cast to ApplicationScale enum");
			}
		}		
				
		String climateChangeStr = (String)paramsMap.get(ClimateChangeLabel);
		if (climateChangeStr == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: climateChange not found in provided params");
		} else {
			try {
				ClimateChangeScenarioHelper.getClimateChangeScenarioFromString(climateChangeStr); // just to test if the value can be converted into a ClimateChangeScenario instance.
				this.climateChange = climateChangeStr;
			} catch (Exception ex) {			
				throw new InvalidParameterException("CapsisWebAPIScriptSimulation: climateChange value " + climateChangeStr + " cannot be cast to ClimateChangeScenario enum");
			}
		}
				
		// evolution parameters
		Long finalDateYr = (Long)paramsMap.get(FinalDateYrLabel);
		if (finalDateYr == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: finalDateYr not found in provided params");
		}
		this.finalDateYr = finalDateYr.intValue(); 
			
		// input data
		Object[] fieldMatchesObj = (Object[])paramsMap.get(FieldMatchesLabel);
		if (fieldMatchesObj == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: fieldMatches not found in provided params");
		}
		
		fieldMatches = new int[fieldMatchesObj.length];
		for (int i = 0; i < fieldMatches.length; i++) {
			fieldMatches[i] = ((Long)fieldMatchesObj[i]).intValue();
		}	
		
		fileName = (String)paramsMap.get(FilenameLabel);
		if (fileName == null) {
			throw new InvalidParameterException("CapsisWebAPIScriptSimulation: fileName not found in provided params");
		}

		protoOutputRequests = new ArrayList<ProtoOutputRequest>();
		if (paramsMap.containsKey(OutputRequestsLabel)) {
			Object arr = paramsMap.get(OutputRequestsLabel);
			if (!arr.getClass().isArray()) {
				throw new InvalidParameterException("CapsisWebAPIScriptSimulation: The " + OutputRequestsLabel + " entry should contain an array!");
			} else {
				for (Object o : (Object[]) arr) {
					protoOutputRequests.add(new ProtoOutputRequest((Map) o));
				}
			}
		} 
		
		initializeSimulation();
	}

	private void initializeSimulation() throws Exception {
		for (ProtoOutputRequest por : protoOutputRequests) {
			script.registerOutputRequest(por.request, por.aggregationPatterns);
		}
		readData(script, fileName, fieldMatches);
//		script.totalNumberOfYears = finalDateYr - initialDateYr;
		script.setFieldMatches(fieldMatches);
		script.setInitialParameters(initialDateYr, isStochastic, nbRealizations, scale, climateChange);
		script.setEvolutionParameters(finalDateYr);
	}
	
	@Override
	public void run() {
		try {											
			ExtScript.Verbose = true;
			
			scriptResult = script.runSimulation();
			
			// delete the csv input file
			File csvFile = new File(fileName);
			csvFile.delete();
			
			ObjectMapper mapper = new ObjectMapper();						
			CapsisWebAPIMessage msg = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(scriptResult));			
			out.println(JsonWriter.objectToJson(msg));						
		} catch (Exception ex) {
			// delete the csv input file
			try {
				File csvFile = new File(fileName);
				csvFile.delete();
			} catch (Exception e) {}

			failure = ex;
			StringBuilder stackTraceStrBuilder = new StringBuilder();
			for (StackTraceElement e : ex.getStackTrace()) {
				stackTraceStrBuilder.append(e.toString());
				stackTraceStrBuilder.append(System.lineSeparator());
			}
			
			CapsisWebAPIMessage msg = CapsisWebAPIMessage.CreateMessageError("Simulate: Exception occurred during simulation : " + ex.getMessage() + System.lineSeparator() + stackTraceStrBuilder.toString());
			out.println(JsonWriter.objectToJson(msg));	
			return;						
		}		
	}

	@Override
	public boolean isCorrectlyTerminated() {
		return failure == null;
	}
	
	private void readData(CapsisWebAPICompatibleScript realScript, String filename, int[] matches) throws Exception {
		realScript.setFieldMatches(matches);
		CSVReader reader = new CSVReader(filename);
		Object[] record;
		int i = 0;
		while ((record = reader.nextRecord()) != null) {
			realScript.addRecord(record);
			i++;
		}
		reader.close();
		//System.out.println("Nb rows read = " + i);
	}

	@Override
	public Exception getFailureReason() {
		return failure;
	}

}
