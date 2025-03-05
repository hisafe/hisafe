/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA 
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
package capsis.defaulttype;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jeeb.lib.util.Log;

/**	A Map for SceneParts in a MultipartScenes (GScenes made of several parts).
 *
 *	@author F. de Coligny - january 2011
 */
public class ScenePartMap {
	private Map<String,Integer> map;
	
	
	/**	Constructor
	 */
	public ScenePartMap (MultipartScene s) {
		map = new HashMap<String,Integer> ();
		
		int id = 0;
		for (String name : s.getPartNames()) {
			map.put (name, id++);
		}
		
	}
	
	/**	Constructor 2
	 */
	public ScenePartMap (List<ScenePart> parts) {
		map = new HashMap<String,Integer> ();
		
		int id = 0;
		for (ScenePart p : parts) {
			map.put (p.getName (), id++);
		}
		
	}
	
	public int getId (String name) {
		Integer id = map.get (name);
		if (id == null) {
			Log.println (Log.ERROR, "ScenePartMap.getId ()", "could not find the id for name: "+name+", returned -1"); // not found, log it
		}
		return id != null ? id : -1;
	}
	
	public String getName (int id) {
		for (String name : map.keySet ()) {
			int i = map.get (name);
			if (i == id) {return name;}
		}
		return null;
	}
	
	public Set getNames () {return map.keySet ();}
	
	
}
