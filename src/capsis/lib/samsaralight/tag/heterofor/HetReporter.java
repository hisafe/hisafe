/*
 * The HETEROFOR model.
 *
 * Copyright (C) 2012-2019: M. Jonard (UCL ELIe Forest Science).
 *
 * This file is part of the HETEROFOR model and is free software:  you can redistribute it and/or
 * modifiy it under the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, version 2.1 of the License, or (at your option) any later version.
 */

package capsis.lib.samsaralight.tag.heterofor;

import java.util.logging.Level;

import jeeb.lib.util.Log;
import jeeb.lib.util.ProgressDispatcher;
import jeeb.lib.util.StatusDispatcher;

/**
 * Contains static methods allowing to print in the Log, in the standard output, in the {@link ProgressDispatcher} and in the {@link StatusDispatcher},
 * depending on the value of the {@link #activated} boolean.
 * The {@link #activated} boolean can be set to false in order to deactivate these displays: for example when running optimization scripts in order to
 * decrease calculation times.
 *
 * @author N. Beudez
 */
public class HetReporter {

	/**
	 * If true, allows to activate the displays in Log, standard output, {@link ProgressDispatcher} and {@link StatusDispatcher}.
	 */
	public static boolean activated = true;

	/**
	 * Activates/deactivates the displays in {@link ProgressDispatcher} and {@link StatusDispatcher}.
	 */
	public static void manageDispatchers () {

		ProgressDispatcher.setMute(!activated);
		StatusDispatcher.setMute(!activated);
	}

	/**
	 * Prints the given message in the default Log if {@link #activated} is true, does nothing otherwise.
	 * @param message The message to be printed
	 */
	public static void printInLog(String message) {

		if (activated) {
			Log.println(message);
		}
	}

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
