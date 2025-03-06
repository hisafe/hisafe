/*
 * This file is part of the Artemis module for Capsis 4.2.2
 *
 * Copyright (C) 2023 His Majesty the King in right of Canada
 * Author: J-F Lavoie - Effixa
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

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.core.JsonProcessingException;

/**
 * A package class to read messages from and send message to CapsisWebAPI.
 * @author Jean-Francois Lavoie - February 2023
 */
class CapsisWebAPIMessage {
	
	enum CapsisWebAPIMessageType {
		MESSAGE_STATUS(true),
		MESSAGE_STOP(true),
		MESSAGE_SIMULATE(true),
		MESSAGE_SIMULATION_STARTED(false),
		MESSAGE_GET_SPECIES_OF_TYPE(true),
		MESSAGE_GET_FIELDS(true),
		MESSAGE_GET_REQUESTS(true),
		MESSAGE_GET_VERSION(true),
		MESSAGE_GET_POSSIBLE_MESSAGES(true),
		MESSAGE_ERROR(false),
		MESSAGE_COMPLETED(false),
		MESSAGE_PORT(false),
		MESSAGE_GET_SCOPE(true);
	
		static List<String> PossibleIncomingMessages;
		
		final boolean possiblyIncoming;
		
		CapsisWebAPIMessageType(boolean possiblyIncoming) {
			this.possiblyIncoming = possiblyIncoming;
		}
		
		static List<String> getPossibleIncomingMessages() {
			if (PossibleIncomingMessages == null) {
				PossibleIncomingMessages = new ArrayList<String>();
				for (CapsisWebAPIMessageType m : CapsisWebAPIMessageType.values()) {
					if (m.possiblyIncoming) {
						PossibleIncomingMessages.add(m.name());
					}
				}
			}
			return PossibleIncomingMessages;
		}
	}
	
	final String message;
	final String payload; 
	
	/**
	 * Constructor.
	 * @param message a CapsisWebAPIMessageType enum
	 * @param payload the additional information to be passed in this message (can be null)
	 */
	private CapsisWebAPIMessage(CapsisWebAPIMessageType message, String payload) {
		if (message == null) {
			throw new InvalidParameterException("The messageType argument must be non null!");
		}
		this.message = message.name();
		this.payload = payload;
	}
	
	/**
	 * Constructor for incoming messages.
	 * @param jsonToMaps a Map constructed from a JSON object
	 */
	CapsisWebAPIMessage(Map<?,?> jsonToMaps) {
		this.message = (String)jsonToMaps.get("message"); 
		this.payload = (String)jsonToMaps.get("payload");		
	}

	/**
	 * Provide the message of this instance.
	 * @return a String
	 */
	String getMessage() {return message;}
	
	/**
	 * Provide the data of this message.
	 * @return a String
	 */
	String getPayload() {return payload;}
	
	/**
	 * Produce a CapsisWebAPIMessage instance to be sent to receiver.<p>
	 * This message indicates the progress in the simulation.
	 * 
	 * @param progress a double representing the progress of the simulation (ranges from 0 to 1)
	 * @return a CapsisWebAPIMessage instance
	 */
	static CapsisWebAPIMessage CreateMessageStatus(double progress)	{
		String payload = new Double(progress).toString(); 
		return new CapsisWebAPIMessage(CapsisWebAPIMessageType.MESSAGE_STATUS, payload);
	}
	
	/**
	 * Produce a CapsisWebAPIMessage instance to be sent to receiver.<p>
	 * This message indicates that the simulation has come to an end.
	 * @return a CapsisWebAPIMessage instance
	 */
	static CapsisWebAPIMessage CreateMessageStop() {
		return new CapsisWebAPIMessage(CapsisWebAPIMessageType.MESSAGE_STOP, null);
	}
	
	/**
	 * Produce a CapsisWebAPIMessage instance to be sent to receiver.<p>
	 * This message indicates that an error occurred during the simulation.
	 * @return a CapsisWebAPIMessage instance
	 */
	static CapsisWebAPIMessage CreateMessageError(String error)	{
		return new CapsisWebAPIMessage(CapsisWebAPIMessageType.MESSAGE_ERROR, error);
	}
	
	/**
	 * Produce a CapsisWebAPIMessage instance to be sent to receiver.<p>
	 * This message indicates that the request has been completed.
	 * @param result the result of the request
	 * @return a CapsisWebAPIMessage instance
	 */
	static CapsisWebAPIMessage CreateMessageCompleted(String result) throws JsonProcessingException	{
		return new CapsisWebAPIMessage(CapsisWebAPIMessageType.MESSAGE_COMPLETED, result);
	}
	
	/**
	 * Produce a CapsisWebAPIMessage instance to be sent to receiver.<p>
	 * This message indicates the port for communication with this process.
	 * @param port the port number
	 * @return a CapsisWebAPIMessage instance
	 */
	static CapsisWebAPIMessage CreateMessagePort(int port) {
		String payload = new Integer(port).toString();
		return new CapsisWebAPIMessage(CapsisWebAPIMessageType.MESSAGE_PORT, payload);
	}
	
	/**
	 * Produce a CapsisWebAPIMessage instance to be sent to receiver.<p>
	 * This message indicates the simulation has started.
	 * @return a CapsisWebAPIMessage instance
	 */
	static CapsisWebAPIMessage CreateMessageSimulationStarted()	{		
		return new CapsisWebAPIMessage(CapsisWebAPIMessageType.MESSAGE_SIMULATION_STARTED, null);
	}
}
