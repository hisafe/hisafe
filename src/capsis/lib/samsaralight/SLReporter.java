/*
 * Samsaralight library for Capsis4.
 * 
 * Copyright (C) 2008 / 2018 Benoit Courbaud.
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.lib.samsaralight;

import java.util.logging.Level;

import jeeb.lib.util.Log;

/**
 * Contains static methods allowing to print in the Log and in the standard output depending on the value of the {@link #activated} boolean. 
 * The {@link #activated} boolean can be set to false in order to deactivate these displays: for example when running optimization scripts in 
 * order to decrease calculation times. 
 * 
 * @author N. Beudez
 */
public class SLReporter {

	/**
	 * If true, allows to activate the displays in Log and standard output.
	 */
	public static boolean activated = false;

	/**
	 * Prints the given message in the Log with name logName if {@link #activated} is true, does nothing otherwise.
	 * @param logName The log's name
	 * @param message The message to be printed
	 */
	public static void printInLog(String logName, String message) {

		if (activated) {			
			Log.println(logName, message);
		}
	}

	/**
	 * Prints the given message in the Log with name logName of type type if {@link #activated} is true, does nothing otherwise.
	 * @param type The log's type
	 * @param methodName The name of the method from which message is printed
	 * @param message The message to be printed
	 */
	public static void printInLog(Level type, String sourceMethodName, String message) {

		if (activated) {
			Log.println(type, sourceMethodName, message);
		}
	}

	/**
	 * Prints the given message in the standard output if {@link #activated} is true, does nothing otherwise.
	 * @param message The message to be printed
	 */
	public static void printInStandardOutput(String message) {

		if (activated) {
			System.out.println(message);
		}
	}
}
