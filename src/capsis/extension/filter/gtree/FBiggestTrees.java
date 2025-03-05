/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.extension.filter.gtree;

import java.io.Serializable;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.extensiontype.Filter;
import capsis.util.TreeDbhThenIdComparator;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.RelativeFilter;

/**
 * Selects the n biggest trees. This filter checks rules between 
 * the individuals in the collection, it is a RelativeFilter.
 * 
 * @author F. de Coligny - september 2004
 */
public class FBiggestTrees implements Filter, Configurable, Serializable, RelativeFilter {

	static {
		Translator.addBundle("capsis.extension.filter.gtree.FBiggestTrees");
	}

	// nb-06.08.2018
	//public static final String NAME = "FBiggestTrees";
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR =  "F. de Coligny";
	//public static final String DESCRIPTION = "FBiggestTrees.description";
	
	// fc - 21.5.2003 - added transient : referent must not be serialized (huge)
	//
    transient protected Object referent;		// complete original object (a complete GStand or GPlot...)
    transient protected Collection candidates;	// individuals to filter
	
	protected int number;		// number of trees to be considered : the main parameter
	protected Set<Integer> treeIds;			// set in preset (), used in retain () -> the ids of the trees to keep
		
	private boolean readyToUse;

	public FBiggestTrees () {}
		
	/**	Constructor for non GUI mode. 
	*	No more configuration required begore preset () and retain ().
	*/
	public FBiggestTrees (int num) {
		number = num;
		
		// fc - 20.4.2004 - if (highValue >= lowValue) {
		if (number < 1) {
			Log.println (Log.INFO, "FBiggestTrees.c ()", "number ("+number
					+") should be > 1");
			return;
		}	// won't work
		
		readyToUse = true;		// will work
	}
	
	public Object clone () {
		try {
			// We just need another instance to prevent preset () ant retain () from
			// being called by several threads at the same time. 
			return super.clone ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "FBiggestTrees.clone ()", "Could not clone", e);
			return null;
		}
	}

	/**	Used by extensionManager to look for compatibilities.
	*/
	static public boolean matchWith (Object o) {
		if (o instanceof Collection) {
			Collection c = (Collection) o;
			if (c.isEmpty ()) {return false;}
			Collection reps = Tools.getRepresentatives (c);	// one instance of each class
			//
			// Accept GTrees but no numberable -> we need individuals
			for (Iterator i = reps.iterator (); i.hasNext ();) {
				Object item = i.next ();
				if (!(item instanceof Tree)) {return false;}
				if (item instanceof Numberable) {return false;}
			}
			return true;
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("FBiggestTrees.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FBiggestTrees.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	/**	Pre process : sort the trees and memorize the ids of the n biggest.
	*	Use of the "dbh then id comparator" ensures consistency : the 10 biggest 
	*	are always the same even if more trees have same dbh.
	*/
	@Override
	public void preset (Collection individuals) throws Exception {	// <- the good prototype from GFilter
		Comparator dbhComparator = new TreeDbhThenIdComparator (false);	// ascending = false
		TreeSet c = new TreeSet (dbhComparator);
		c.addAll (individuals);	// sorted on dbh
		
		treeIds = new HashSet ();	// fast on contains
		Iterator i = c.iterator ();
		int index = 0;
		while (i.hasNext () && index < number) {
			index++;
			Tree t = (Tree) i.next ();
			treeIds.add (new Integer (t.getId ()));
		}
	}
	
	/**	Return true if the individual is a tree with its id in the list of the biggest trees.
	*	The list was created in preset ().
	*/
	@Override
	public boolean retain (Object individual) throws Exception {
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FBiggestTrees.retain () - bad configuration for "+toString ());}
		if (treeIds == null) {throw new Exception (
				"FBiggestTrees.retain () - treeIds should not be null, check preset () method "+toString ());}
		if (treeIds.isEmpty ()) {return false;}		// should not happen
		
		// Real test is here
		Tree t = (Tree) individual;
		if (treeIds.contains (new Integer (t.getId ()))) {return true;}
		return false;
	}
		
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	public ConfigurationPanel getConfigPanel (Object param) {
		// fc - 20.4.2004
		Object[] refAndCandidates = (Object[]) param;
		referent = refAndCandidates[0];
		candidates = (Collection) refAndCandidates[1];
		
		return new FBiggestTreesDialog (this);
	}
	
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.	*/
	@Override
	public void applyConfig (ConfigurationPanel panel) {
		FBiggestTreesDialog p = (FBiggestTreesDialog) panel;
		this.number = p.getNumber ();
		
		readyToUse = true;		// it will work
	}
	

	/**	From Configurable interface.
	*/
	@Override
	public String getConfigLabel () {
		// nb-06.08.2018
		//return Translator.swap(NAME);
		return Translator.swap(getName());
	}

	// Needed because of Configurable and Extension interfaces, but unused
	@Override
	public void postConfig () {}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}


