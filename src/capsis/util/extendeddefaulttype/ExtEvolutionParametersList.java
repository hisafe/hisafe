/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
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
package capsis.util.extendeddefaulttype;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import jeeb.lib.util.Log;
import repicea.io.FormatReader;
import capsis.kernel.EvolutionParameters;
import jeeb.lib.util.Automatable;


public class ExtEvolutionParametersList<P extends ExtEvolutionParameters> extends ArrayList<P> implements Automatable, EvolutionParameters {

	/**
	 * Default constructor for GUI mode.
	 * @param parms
	 */
	public ExtEvolutionParametersList(P parms) {
		this();
		add(parms);
	}

	/**
	 * Protected constructor to hide the default constructor.
	 */
	protected ExtEvolutionParametersList() {
		super();
	}

	public ExtEvolutionParametersList(String filename) throws IOException { 
		this();
		addAll(importScenario(filename));
	}
	
	/**
	 * This static method is used to import a scenario from a particular file whose format must be either
	 * .dbf or .csv
	 * @param filename the name of the file from which the scenario is imported 
	 * @return a Vector of ExtendedEvolutionParameters instances
	 * @exception IOException if the file is not found or cannot be read
	 */
	public List<P> importScenario(String filename) throws IOException {
		List<P> oVec = new ArrayList<P>();

		try {
			FormatReader reader = FormatReader.createFormatReader(filename);

			// Now, lets start reading the rows
			Object[] rowObjects;
			while((rowObjects = reader.nextRecord()) != null) {
				oVec.add(deserializeFromRecord(rowObjects));
			}

			// By now, we have iterated through all of the rows
			reader.close();
		} catch (FileNotFoundException e1) {
			Log.println (Log.ERROR, "QuebecMRNFDBFImport.importScenario()", "Could not find file : " + filename, e1);
			throw e1;
		} catch (IOException e2) {
			Log.println (Log.ERROR, "QuebecMRNFDBFImport.importScenario()", "Error reading file : " + filename, e2);
			throw e2;
		}
		return oVec;
	}
	
	
	protected P deserializeFromRecord(Object[] record) {return null;}

	public int getNbSteps() {
		int nbSteps = 0;
		for (ExtEvolutionParameters evolParms : this) {
			nbSteps += evolParms.getNbSteps();
		}
		return nbSteps;
	}
}
