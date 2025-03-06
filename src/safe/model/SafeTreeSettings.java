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

import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import capsis.kernel.AbstractSettings;

/**
 * TREE SPECIES parameters
 *
 * @author Isabelle Lecomte -  INRAE Montpellier France - July 2002
 */
public class SafeTreeSettings extends AbstractSettings {


	private static final long serialVersionUID = 1L;
	private Map<String, SafeTreeSpecies> species; // Map treeSpeciesName -> treeSpecies object


	/** Constructor
	*/
	public SafeTreeSettings () {
		species= new Hashtable<String, SafeTreeSpecies> ();
	}


	/** Add a species
	*/
	public void addSpecies (SafeTreeSpecies s) {
		species.put (s.getName (), s);
	}

	/** return the Species objet with this name
	*/
	public SafeTreeSpecies getSpecies (String treeSpeciesName) {
		return (SafeTreeSpecies) species.get (treeSpeciesName);
	}

	/**	Return the complete Species list
	*/
	public Collection<SafeTreeSpecies> getList () {
		return species.values ();
	}


	/**	To check the list in the Capsis inspector
	*/
	public String toString() {
		String s = "Tree Species list = ";
		for (Iterator<SafeTreeSpecies> i = species.values ().iterator (); i.hasNext ();) {
			SafeTreeSpecies sp =  i.next ();

			s+=sp.getValue ()+" "+sp.getName ();
			if (i.hasNext ()) {s+=" - ";}
		}
		return s;
	}

}



