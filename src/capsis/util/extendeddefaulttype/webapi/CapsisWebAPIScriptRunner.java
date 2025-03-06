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

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketTimeoutException;
import java.security.InvalidParameterException;
import java.time.Duration;
import java.time.Instant;
import java.util.Arrays;
import java.util.List;

import com.cedarsoftware.util.io.JsonReader;
import com.cedarsoftware.util.io.JsonWriter;
import com.fasterxml.jackson.databind.ObjectMapper;

import capsis.kernel.Engine;
import capsis.util.extendeddefaulttype.ExtScript;
import capsis.util.extendeddefaulttype.webapi.CapsisWebAPIMessage.CapsisWebAPIMessageType;
import jeeb.lib.util.Log;
import repicea.io.tools.ImportFieldElement.ImportFieldElementIDCard;
import repicea.lang.REpiceaSystem;
import repicea.simulation.covariateproviders.treelevel.SpeciesTypeProvider.SpeciesType;
import repicea.simulation.scriptapi.CapsisWebAPICompatibleScript;
import repicea.simulation.scriptapi.Request;

/**
 * A script to be used to launch Artemis2009 simulations.<p>
 * 
 * The class contains a main method that is used by CapsisWebAPI.
 * 
 * @see ExtScript
 * 
 * @author Mathieu Fortin - November 2020
 * @author Jean-Francois Lavoie - February 2023
 */
public class CapsisWebAPIScriptRunner {

	private enum Model {ARTEMIS, ARTEMIS2014}
	
	private static CapsisWebAPICompatibleScript getScript(Model m) throws Exception {
		switch(m) {
		case ARTEMIS:
			return null;
		case ARTEMIS2014:
			return null;
		default:
			throw new InvalidParameterException("Model " + m.name() + " is not supported!");
		}
	}
	
	public static void main(String[] args) throws Exception {
		try {
			boolean disableWatchdog = false;
			int port = 0;
			List<String> argsList = Arrays.asList(args);
			String portArg = REpiceaSystem.retrieveArgument("--port", argsList);
			if (portArg != null)		
			{
				try {
					port = Integer.parseInt(portArg);
				}
				catch (Exception e) {
					System.out.println("invalid 'port' option " + portArg + " specified.");
					System.exit(1);
				}
			}
			int timeoutSec = -1; 	
			String timeoutArg = REpiceaSystem.retrieveArgument("--timeout", argsList);
			if (timeoutArg != null) {
				try {
					timeoutSec = Integer.parseInt(timeoutArg);
				} catch (Exception e) { // timeout was specified but cannot be parsed
					System.out.println("invalid 'timeout' option " + timeoutArg + " specified.");
					System.out.println("Timeout will be set to default value (10 seconds)");
					timeoutSec = 10;
				}
			} else { // timeout was not specified
				System.out.println("Timeout will be set to default value (10 seconds)");
				timeoutSec = 10;
			}
			
			String variant = REpiceaSystem.retrieveArgument("--variant", argsList);
			Model m;
			if (variant == null) {
				throw new InvalidParameterException("A variant should be specified. Use --variant myVariant in the call to main method!");
			} else {
				m = Model.valueOf(variant.toUpperCase().trim());
			}
			
			if (argsList.contains("--disableWatchdog"))		
				disableWatchdog = true;		
			
			ServerSocket socket = new ServerSocket(port);
					
			Socket clientSocket = null;
			int retries = 0;
			while (clientSocket == null && (retries < 10 || disableWatchdog)) {		
				try {
					CapsisWebAPIMessage portMsg = CapsisWebAPIMessage.CreateMessagePort(socket.getLocalPort());			
					System.out.println(JsonWriter.objectToJson(portMsg));	// this message must be sent out on the stdout to inform the host what the selected port is
							
					socket.setSoTimeout(1000);
				    clientSocket = socket.accept();		    
				    
				}
				catch (SocketTimeoutException e) {
					retries++;
				}
			}
			
			if (clientSocket == null) {
				System.out.println("No connection from host received.  Exiting...");
				System.exit(1);
			}
			
		    PrintWriter out = new PrintWriter(clientSocket.getOutputStream(), true);
		    BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));	    
		    
			Instant lastStatusInstant = Instant.now();
			boolean stopProcess = false;
			
			CapsisWebAPICompatibleScript script = getScript(m);		
			
			// send a first COMPLETED message to tell the client we`re ready for processing requests
			CapsisWebAPIMessage readyMsg = CapsisWebAPIMessage.CreateMessageCompleted(null);			        			
			out.println(JsonWriter.objectToJson(readyMsg));
			
			while (!stopProcess) {
				CapsisWebAPIMessage incomingMsg = null;
				// read incoming messages			
		        while (in.ready()){	        	
		        	try {	        			        		
		        		String str = in.readLine();
		        		System.out.println("ArtScript received line " + str);
		        		
		        		incomingMsg = new CapsisWebAPIMessage(JsonReader.jsonToMaps(str));		        		
		        		
		        		if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_STOP.name())) {
		        			stopProcess = true;
		        			System.out.println("Stopping after receiving STOP message from client");
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_STATUS.name())) {
		        			lastStatusInstant = Instant.now();
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_SIMULATE.name())) {	
		        			CapsisWebAPIScriptSimulation runnable = new CapsisWebAPIScriptSimulation(script, incomingMsg.getPayload(), out);
		        			new Thread(runnable).start();	// we start the simulation	
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageSimulationStarted();			        			
		        			out.println(JsonWriter.objectToJson(response));
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_GET_SPECIES_OF_TYPE.name())) {
		        			List<String> speciesList = incomingMsg.getPayload() == null ?
		        					script.getSpeciesOfType(new SpeciesType[] {SpeciesType.ConiferousSpecies, SpeciesType.BroadleavedSpecies}) :
		        						script.getSpeciesOfType(SpeciesType.valueOf(incomingMsg.getPayload()));		        							        					
		        			ObjectMapper mapper = new ObjectMapper();						
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(speciesList));			        			
		        			out.println(JsonWriter.objectToJson(response));
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_GET_VERSION.name())) {
		        			ObjectMapper mapper = new ObjectMapper();						
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(Engine.getVersionAndRevision()));			        			
		        			out.println(JsonWriter.objectToJson(response));
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_GET_FIELDS.name())) {
		        			List<ImportFieldElementIDCard> fieldsList = script.getFieldDescriptions();		        				
		        			ObjectMapper mapper = new ObjectMapper();						
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(fieldsList));			
		        			out.println(JsonWriter.objectToJson(response));
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_GET_REQUESTS.name())) {
		        			List<Request> requests = script.getPossibleRequests();
		        			ObjectMapper mapper = new ObjectMapper();
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(requests));
		        			out.println(JsonWriter.objectToJson(response));
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_GET_POSSIBLE_MESSAGES.name())) {
		        			ObjectMapper mapper = new ObjectMapper();
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(CapsisWebAPIMessageType.getPossibleIncomingMessages()));
		        			out.println(JsonWriter.objectToJson(response));
		        		} else if (incomingMsg.getMessage().equals(CapsisWebAPIMessageType.MESSAGE_GET_SCOPE.name())) {
		        			ObjectMapper mapper = new ObjectMapper();
		        			CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageCompleted(mapper.writeValueAsString(script.getScope()));
		        			out.println(JsonWriter.objectToJson(response));
		        		}
		        	} catch (Exception e) {
	    				CapsisWebAPIMessage response = CapsisWebAPIMessage.CreateMessageError("ArtScript : Exception occured while processing " + (incomingMsg != null ? incomingMsg.message : "Unknown") + " : " + e.getMessage());			
	        			out.println(JsonWriter.objectToJson(response));
		        	}
		        }
				
		        if (!disableWatchdog && Duration.between(lastStatusInstant, Instant.now()).getSeconds() > timeoutSec)
		        {
		        	stopProcess = true;
		        	System.out.println("Stopping after lost heartbeat from client");
		        }
		        
		        if (!stopProcess) {
		        	Thread.sleep(1000);
					CapsisWebAPIMessage msg = CapsisWebAPIMessage.CreateMessageStatus(script.getSimulationProgress());
					out.println(JsonWriter.objectToJson(msg));				
		        }
			}
			
			CapsisWebAPIMessage msg = CapsisWebAPIMessage.CreateMessageStop();
			out.println(JsonWriter.objectToJson(msg));
			out.flush();
			socket.close();		
			System.exit(0);
		} catch (Exception e) {
			Log.println(Log.ERROR, "CapsisWebAPIScriptRunner.main()", e.getMessage(), e); 
		}
	}
	
}
