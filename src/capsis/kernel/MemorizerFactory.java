/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.kernel;

import capsis.extension.memorizer.CompactMemorizer;
import capsis.extension.memorizer.DefaultMemorizer;
import capsis.extension.memorizer.FrequencyMemorizer;
import capsis.extension.memorizer.InterventionMemorizer;
import capsis.extension.memorizer.MinimalMemorizer;
import capsis.kernel.extensiontype.Memorizer;

/**
 * Memorizer factory creates memorizers.
 * 
 * @author F. de Coligny, S. Dufour - October 2002, September 2010
 */
public class MemorizerFactory {

	/**
	 * Returns a Memorizer from the given code.
	 */
	static public Memorizer createMemorizer(String code) {
		if (code == null || code.startsWith("DefaultMemorizer")) {
			return createDefaultMemorizer();

		} else if (code.startsWith("CompactMemorizer")) {
			return createCompactMemorizer();

		} else if (code.startsWith("MinimalMemorizer")) {
			return createMinimalMemorizer();

		} else if (code.startsWith("FrequencyMemorizer")) {
			// e.g. "FrequencyMemorizer f=5"
			int i = code.indexOf("=");
			String f = code.substring(i + 1);
			try {
				int freq = new Integer(f).intValue();
				return createFrequencyMemorizer(freq);
			} catch (Exception e) {
			}
			return createFrequencyMemorizer(5);

		} else if (code.startsWith("InterventionMemorizer")) {
			// fc-23.2.2023 Added this to help restoring the last memorizer at new project
			// time
			return createInterventionMemorizer();
		}

		return createDefaultMemorizer();

	}

	/**
	 * Returns an instance of DefaultMemorizer.
	 */
	static public Memorizer createDefaultMemorizer() {

		try {

			return new DefaultMemorizer();

		} catch (Exception e) {
			return null;
		}
	}

	/**
	 * Returns an instance of CompactMemorizer.
	 */
	static public Memorizer createCompactMemorizer() {

		try {

			return new CompactMemorizer();

		} catch (Exception e) {
			return createDefaultMemorizer();
		}
	}

	/**
	 * Returns an instance of CompactMemorizer.
	 */
	static public Memorizer createMinimalMemorizer() {

		try {

			return new MinimalMemorizer();

		} catch (Exception e) {
			return createDefaultMemorizer();
		}
	}

	/**
	 * Returns an instance of FrequencyMemorizer.
	 */
	static public Memorizer createFrequencyMemorizer(int freq) {

		try {
			return new FrequencyMemorizer(freq);

		} catch (Exception e) {
			return createDefaultMemorizer();
		}
	}

	// fa-22.02.2023
	/**
	 * Returns an instance of InterventionMemorizer.
	 */
	static public Memorizer createInterventionMemorizer() {

		try {
			return new InterventionMemorizer();

		} catch (Exception e) {
			return createDefaultMemorizer();
		}
	}

}
