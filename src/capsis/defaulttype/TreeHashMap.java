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

package capsis.defaulttype;

import java.util.Collection;
import java.util.HashMap;

import jeeb.lib.util.Log;

/**	An implementation of TreeCollection.
 *	Trees are managed in a Map.
 *	@author F. de Coligny - december 2000, reviewed january 2011
 */
public class TreeHashMap extends HashMap<Integer,Tree> implements TreeCollection, Cloneable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;
	
	/**	Constructor
	 */
	public TreeHashMap () {
		super ();
	}
	
	/**	TreeCollection interface.
	 */
	public boolean addTree (Tree tree) {
		this.put (tree.getId (), tree);
		return true;
	}

	/**	TreeCollection interface.
	 */
	public void removeTree (Tree tree) {
		this.remove (tree.getId ());
	}	

	/**	TreeCollection interface.
	 */
	public void clearTrees () {
		this.clear ();		
	}

	/**	TreeCollection interface.
	 */
	public Collection<Tree> getTrees () {
		return this.values ();
	}

	/**	TreeCollection interface.
	 */
	public Tree getTree (int treeId){
		return (Tree) this.get (treeId);
	}
	
	/**	TreeCollection interface.
	 */
	public Collection<Integer> getTreeIds () {
		return keySet ();
	}
	
	/**	Clones a TreeHashMap
	 */
	public Object clone () {
		try {
		
			TreeHashMap m = (TreeHashMap) super.clone ();  // calls protected Object.clone () {}
			m.clear ();
			
			for (Tree t : values()) {
				Tree t1 = (Tree) t.clone ();
				m.addTree (t1);
			}
			
			return m;
		} catch (Exception exc) {
			Log.println (Log.ERROR, "TreeHashMap.clone ()", "Exception, source="+this, exc);
			return null;
		}
	}

	
	// REMOVED
//	public Tree getFirstTree () {
//		Tree t = null;
//		// if empty iterator, NoSuchElementException
//		try {t = (Tree) this.values ().iterator ().next ();} catch (Exception exc) {}
//		return t;	
//	}

	// REMOVED
//	public boolean contains (Tree tree) {
//		return containsValue ((Object) tree);
//	}

}

