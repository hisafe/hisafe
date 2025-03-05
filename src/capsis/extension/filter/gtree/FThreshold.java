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
import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.extensiontype.Filter;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Filter for a GTree(s)' container.
 * (i.e. a Filtrable object containing GTree(s), ex: GStand). 
 * Applies a low/high threshold on tree age, dbh or height.
 * 
 * @author F. de Coligny - October 2000
 */
public class FThreshold implements Filter, Configurable, Serializable {

	static {
		Translator.addBundle("capsis.extension.filter.gtree.FThreshold");
	} 

	// nb-06.08.2018
	//public static final String NAME = "FThreshold";
	//public static final String VERSION = "1.2";
	//public static final String AUTHOR =  "F. de Coligny";
	//public static final String DESCRIPTION = "FThreshold.description";
	
	public static final int AGE = 0;
	public static final int DBH = 1;
	public static final int HEIGHT = 2;
	public static final String UNUSED = "";
	
	// fc - 21.5.2003 - added transient : referent must not be serialized (huge)
	//
    transient protected Object referent;		// complete original object (a complete GStand or GPlot...)
    transient protected Collection candidates;	// individuals to filter
	
	// Filter can be configured by a config panel using its protected variables wo accessors
	protected int mode;			// AGE, DBH or HEIGHT
	protected double lowValue;		// possibly Double.MIN_VALUE
	protected double highValue;		// possibly Double.MAX_VALUE
	
	private boolean readyToUse;
	
	public FThreshold () {
		
	}
	
	/**	Constructor for non GUI mode. 
	*	No more configuration required begore preset () and retain ().
	*/
	public FThreshold (int m, double lowval, double highval) {
		mode = m;
		lowValue = lowval;
		highValue = highval;
		
		// fc - 20.4.2004 - if (highValue >= lowValue) {
		if (highValue < lowValue) {
			Log.println (Log.INFO, "FThreshold.c ()", "highValue ("+highValue
					+") < lowValue ("+lowValue+")");
			return;
		}	// won't work
		
		// Check input
		if (mode != FThreshold.AGE 
				&& mode != FThreshold.DBH 
				&& mode != FThreshold.HEIGHT) {
			Log.println (Log.INFO, "FThreshold.c ()", "mode ("+mode+") is unknown (should be AGE ("+AGE+"), DBH ("+DBH+") or HEIGHT ("+HEIGHT+"))");
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
			Log.println (Log.ERROR, "FThreshold.clone ()", "Could not clone", e);
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
			// Possibly several subclasses of GTree
			for (Iterator i = reps.iterator (); i.hasNext ();) {
				if (!(i.next () instanceof Tree)) {return false;}
			}
			return true;
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("FThreshold.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FThreshold.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	/**	Return true if the filter keeps the given individual.
	*	This means that the object corresponds to the rules of the filter.
	*/
	@Override
	public boolean retain (Object individual) throws Exception {
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FThreshold.retain () - bad configuration for "+toString ());}
		Tree t = (Tree) individual;
		if (mode == FThreshold.AGE) {
			if (t.getAge () >= lowValue && t.getAge () < highValue) {return true;}
		} else if (mode == FThreshold.DBH) {
			if (t.getDbh () >= lowValue && t.getDbh () < highValue) {return true;}
		} else if (mode == FThreshold.HEIGHT) {
			if (t.getHeight () >= lowValue && t.getHeight () < highValue) {return true;}
		}
		return false;
	}
		
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	public ConfigurationPanel getConfigPanel (Object param) {
		// fc - 20.4.2004
		Object[] refAndCandidates = (Object[]) param;
		referent = refAndCandidates[0];
		candidates = (Collection) refAndCandidates[1];
		
		return new FThresholdConfigPanel (this);
	}
	
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	public void applyConfig (ConfigurationPanel panel) {
		FThresholdConfigPanel p = (FThresholdConfigPanel) panel;
		this.mode = p.getMode ();
		this.lowValue = p.getLowValue ();
		this.highValue = p.getHighValue ();
		
		readyToUse = true;		// it will work
	}
	
	
	/**	From Configurable interface.
	*/
	@Override
	public String getConfigLabel () {
		// nb-06.08.2018
		//return NAME ;
		return getName();
	}

	// Needed because of Configurable and Extension interfaces, but unused
	@Override
	public void postConfig () {}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	/**	Normalized toString () method : should allow to rebuild a filter with
	*	same parameters.
	*/
	public String toString () {
		return "class="+getClass().getName()
				// nb-06.08.2018
				//+" name=\""+NAME+"\""
				+" name=\""+getName()+"\""
				+" mode="+mode
				+ (mode == 0?"(AGE)":(mode == 1?"(DBH)":(mode == 2?"(HEIGHT)":"(unknown)")))
				+" lowValue="+lowValue
				+" highValue="+highValue;
	}

	@Override
	public void preset(Collection individuals) throws Exception {
		// TODO Auto-generated method stub
		
	}
	
}


