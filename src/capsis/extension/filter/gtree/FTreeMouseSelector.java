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
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.extensiontype.Filter;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Interactive selector fot GMaddTrees (with mouse). "Martellodrome".
 * FTreeMouseSelector allows to choose trees with mouse operations.
 * 
 * @author F. de Coligny - march 2001
 */
public class FTreeMouseSelector implements Filter, Configurable, Serializable {
	// fc-5.7.2012 replaced SpatializedTree by Tree AND Spatialized for Jackpine
	// compatibility

	static {
		Translator.addBundle("capsis.extension.filter.gtree.FTreeMouseSelector");
	}

	// nb-06.08.2018
	//public static final String NAME = "FTreeMouseSelector";
	//public static final String VERSION = "1.3";
	//public static final String AUTHOR = "F. de Coligny";
	//public static final String DESCRIPTION = "FTreeMouseSelector.description";

	// Filter can be configured by a config panel using its protected variables
	// wo accessors

	// fc - 21.5.2003 - added transient : referent / candidates must not be
	// serialized (huge)
	//
	transient protected Object referent; // reference stand: a GScene AND
											// TreeCollection
	transient protected Collection candidates; // candidates may result of
												// previous filters action

	protected Set<Integer> treeIds; // selected trees ids
	private boolean readyToUse;

	public FTreeMouseSelector() {
		treeIds = new HashSet();
	}

	/**
	 * Constructor for non GUI mode. No more configuration required begore
	 * preset () and retain ().
	 */
	public FTreeMouseSelector(Set<Integer> s) {
		treeIds = s;
		readyToUse = true; // it will work
	}

	public Object clone() {
		try {
			// We just need another instance to prevent preset () ant retain ()
			// from
			// being called by several threads at the same time.
			return super.clone();
		} catch (Exception e) {
			Log.println(Log.ERROR, "FTreeMouseSelector.clone ()",
					"Could not clone", e);
			return null;
		}
	}

	/**
	 * Used by extensionManager to look for compatibilities. Target must be a
	 * GTCStand (see detail below).
	 */
	static public boolean matchWith(Object o) {
		if (o instanceof Collection) {
			Collection c = (Collection) o;
			if (c.isEmpty()) {
				return false;
			}
			Collection reps = Tools.getRepresentatives(c); // one instance of
															// each class
			//
			// Possibly several subclasses of Tree
			for (Iterator i = reps.iterator(); i.hasNext();) {
				Object r = i.next();
				if (!(r instanceof Tree && r instanceof Spatialized)) {
					return false;
				}

				// Particular case: some trees are Spatialized depending on the
				// module initialisation (e.g. Jackpine). If s.getX () does not
				// throw an exception, it is supposed to be good.
				Spatialized s = (Spatialized) r;
				try {
					s.getX();
				} catch (Exception e) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
	
	@Override
	public String getName() {
		return Translator.swap("FTreeMouseSelector.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FTreeMouseSelector.description");
	}

	@Override
	public String getVersion() {
		return "1.3";
	}

	/**
	 * Return true if the filter keeps the given individual. This means that the
	 * object corresponds to the rules of the filter.
	 */
	@Override
	public boolean retain(Object individual) throws Exception {
		// Check if can be applied
		if (!readyToUse) {
			throw new Exception(
					"FTreeMouseSelector.retain () - bad configuration for "
							+ toString());
		}
		Tree t = (Tree) individual;
		if (treeIds.contains(new Integer(t.getId()))) {
			return true;
		}
		return false;
	}

	/**
	 * GUI use : configuration panel configures the filter before preset () and
	 * retain () are called.
	 */
	public ConfigurationPanel getConfigPanel(Object param) {
		// fc - 20.4.2004
		Object[] refAndCandidates = (Object[]) param;
		referent = refAndCandidates[0];
		candidates = (Collection) refAndCandidates[1];

		return new FTreeMouseSelectorConfigPanel(this);
	}

	/**
	 * GUI use : configuration panel configures the filter before preset () and
	 * retain () are called.
	 */
	public void applyConfig(ConfigurationPanel panel) {
		FTreeMouseSelectorConfigPanel p = (FTreeMouseSelectorConfigPanel) panel;
		readyToUse = true; // it should work
	}

	/**
	 * From Configurable interface.
	 */
	public String getConfigLabel() {
		// nb-06.08.2018
		//return NAME;
		return getName();
	}

	// Needed because of Configurable and Extension interfaces, but unused
	@Override
	public void postConfig() {
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	@Override
	public void preset(Collection individuals) throws Exception {
		// TODO Auto-generated method stub

	}

}
