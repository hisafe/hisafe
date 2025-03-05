/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2012 INRA 
 * 
 * Author: F. de Coligny
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
package capsis.defaulttype;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * An explorer for SpeciesPropsOwner instances. It can explore a
 * SpeciesPropsOwnerList with many SpeciesPropsOwners inside and can be asked
 * for species names, props names and the values of different properties for the
 * different species in the SpeciesPropsOwners.
 * 
 * @author F. de Coligny - January 2012
 */
public class SpeciesPropsExplorer {

	private Map<String,Integer> speciesMap;
	private Set<String> props; // no duplicates
	
	/**	
	 * Constructor
	 */
	public SpeciesPropsExplorer (SpeciesPropsOwnerList spoList) {
		
		speciesMap = new HashMap<String, Integer>();
		props = new HashSet<String>();
		
		int count = 0;
		for (SpeciesPropsOwner owner : spoList.getSpeciesPropsOwners()) {
			
			// Memo available species
			List<String> speciesNames = owner.getSpeciesNames();
			for (String name : speciesNames) {
				if (!speciesMap.containsKey(name)) {
					speciesMap.put(name, count++);
				}
			}
			
			// Memo available props
			props.addAll(owner.getPropNames()); // a set -> no duplicates
			
		}
	}

	public int getSpeciesId (String speciesName) {
		Integer id = speciesMap.get (speciesName);
		return id != null ? id : -1; // not found -> -1
	}
	
	public String getSpeciesName (int speciesId) {
		for (String name : speciesMap.keySet ()) {
			int i = speciesMap.get (name);
			if (i == speciesId) {return name;}
		}
		return null; // not found -> null
		
	}
	
	public Set<String> getSpeciesNames () {
		return new HashSet<String> (speciesMap.keySet()); // returns a copy
	}
	
	public List<String> getSpeciesNames (SpeciesPropsOwner owner) {
		return owner.getSpeciesNames();
	}
	
	public Set<String> getProps () {
		return new HashSet<String> (props); // returns a copy
	}
	
	public List<String> getProps (SpeciesPropsOwner owner) {
		return owner.getPropNames();
	}
	
	public double getValue (SpeciesPropsOwner owner, String speciesName, String propName) {
		return owner.getValue(speciesName, propName);
	}
	
	public String toString () {
		String v = "SpeciesPropsExplorer";
		v += "\nspeciesMap: ";
		for (String speciesName : speciesMap.keySet ()) {
			v += speciesName;
			v += ": ";
			v += speciesMap.get (speciesName);
			v += " ";
		}
		v+= "\nprops: ";
		for (String p : props) {
			v += p;
			v += " ";
		}
		v += "\n";
		
		return v;
	}
	
	
}
