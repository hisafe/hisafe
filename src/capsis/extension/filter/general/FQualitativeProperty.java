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

package capsis.extension.filter.general;

import java.io.Serializable;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.extensiontype.Filter;
import capsis.util.QualitativeProperty;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Filter for a filtrable with Qualitative Properties.
 * For each property, choose the values to consider.
 * The result is Q1 (v1 || v2 ... ||vn) && Q2 (v'1 || v'2 ... || v'm) ... 
 * 
 * @author F. de Coligny - march 2001
 */
public class FQualitativeProperty implements Filter, Configurable, Serializable {

	static {
		Translator.addBundle("capsis.extension.filter.general.FQualitativeProperty");
	}

	// nb-06.08.2018
	//public static final String NAME = "FQualitativeProperty";
	//public static final String VERSION = "1.2";
	//public static final String AUTHOR =  "F. de Coligny";
	//public static final String DESCRIPTION = "FQualitativeProperty.description";
	
	// Filter can be configured by a config panel using its protected variables wo accessors
	
	// Class - Collection of valid values (Integer)
	// ex: ventoux.model.VtxSpecies - (0, 3, 4) -> pinusNigra || abiesAlba || acerOpalus
	//     ventoux.model.OtherQual.Val. - (2) -> some other qualitative var. with value == 2
	// -> (VtxSpecies == pinusNigra || abiesAlba || acerOpalus) && (OtherQual.Val. == 2)
	protected Map class_validValues;
	
	// fc - 21.5.2003 - added transient : referent must not be serialized (huge)
	//
    transient protected Object referent;		// complete original object (a complete GStand or GPlot...)
    transient protected Collection candidates;	// individuals to filter
	
	transient private Collection acsrs;		// fc - 26.3.2004 - transient : Method(s) are not Serializable
	private boolean readyToUse;

	public FQualitativeProperty () {
        class_validValues = new Hashtable ();
	} 
	
	/**	Constructor for non GUI mode. 
	*	No more configuration required before preset () and retain ().
	*	Needed in starter : class_validValues and type (ex: TreeList.GROUP_ALIVE_TREE). 
	*	Optional : referent and candidates (ex: in script mode).
	*/
	public FQualitativeProperty (Map classValidValues) {
        class_validValues = classValidValues;
		
        // Check input
        if (class_validValues == null) {return;}
        if (class_validValues.isEmpty ()) {return;}
		
		readyToUse = true;		// it will work (warcheinlich)
	}
	
	public Object clone () {
		try {
			// We just need another instance to prevent preset () ant retain () from
			// being called by several threads at the same time. 
			return super.clone ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "FQualitativeProperty.clone ()", "Could not clone", e);
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
			// Possibly several subclasses of same class
			for (Iterator i = reps.iterator (); i.hasNext ();) {
				Object elt = i.next ();
				
				boolean someAccessorsReturnQP = false;
				Collection ac = Tools.getPublicAccessors (elt.getClass ());	
				for (Iterator j = ac.iterator (); j.hasNext ();) {
					Method m = (Method) j.next ();
					if (Tools.returnsType (m, QualitativeProperty.class)) {
						someAccessorsReturnQP = true;
					}
				}
				// if one elt has no accessors returning a QP -> not compatible
				if (!someAccessorsReturnQP) {return false;}
			}
			return true;
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("FQualitativeProperty.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FQualitativeProperty.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	/**	Pre process : what different object classes in the filtrable ?	
	*	What methods of these objects return Qualitative Properties ?
	*/
	@Override
	public void preset (Collection individuals) throws Exception {	// <- the good prototype from GFilter
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FQualitativeProperty.preset () - bad configuration for "+toString ());}
		
		// 1. Pre process : what different object classes in the filtrable ?	// fc - 7.10.2003
		// fc - 7.10.2003 - for Luberon : LubTree1.getSpecies() and LubTree2.getSpecies() are different Methods
		Set classes = new HashSet ();
		Set objects = new HashSet ();
		for (Iterator i = individuals.iterator (); i.hasNext ();) {
			Object o = i.next ();
			if (!classes.contains (o.getClass ())) {
				classes.add (o.getClass ());	// LubTree1, LubTree2
				objects.add (o);				// one lubTree1, one lubTree2
			}
		}
		
		// 2. Pre process : what methods of these objects return Qualitative Properties ? (-> in acsrs)
		// ex: LubTree1.getSpecies () & LubTree2.getSpecies () -> 2 methods
		acsrs = new ArrayList ();
		for (Iterator obs = objects.iterator (); obs.hasNext ();) {
			Object o = obs.next ();
			
			Collection acs = Tools.getPublicAccessors (o.getClass ());	// public accessors
			for (Iterator j = acs.iterator (); j.hasNext ();) {
				Method m = (Method) j.next ();
				if (!Tools.returnsType (m, QualitativeProperty.class)) {j.remove ();}	// good return type
			}
			acsrs.addAll (acs);
		}
		
	}
	
	/**	Return true if the filter keeps the given individual.
	*	This means that the object corresponds to the rules of the filter.
	*/
	@Override
	public boolean retain (Object individual) throws Exception {
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FTreeGridSelector.retain () - bad configuration for "+toString ());}
		
		// Nothing configured : retain nothing - fc - 5.4.2004
		if (class_validValues == null || class_validValues.isEmpty ()) {return false;}
		
		boolean	keepIt = true;
			
		// 1. Invoke every selected method on current element
		for (Iterator k = acsrs.iterator (); k.hasNext ();) {
			Method m = (Method) k.next ();		// one of the selected methods
			try {
				QualitativeProperty p = (QualitativeProperty) m.invoke (individual);	// fc - 2.12.2004 - varargs
				
				// 2. Check if result is valid
				Collection validValues = (Collection) class_validValues.get (p.getClass ());
				if (validValues != null
					&& !validValues.contains (new Integer (p.getValue ()))) {
					keepIt = false;		// Forget it
					break;	// important: first invalid value is sufficient
				}
				
			} catch (Exception e) {	
				// if we invoke method LubTree1.getSpecies on LubTree2 -> exception -> next acsr
			}
		}
		
		return keepIt;
	}
	
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	public ConfigurationPanel getConfigPanel (Object param) {
		// fc - 20.4.2004
		Object[] refAndCandidates = (Object[]) param;
		referent = refAndCandidates[0];
		candidates = (Collection) refAndCandidates[1];
		
        return new FQualitativePropertyConfigPanel (this);
	}
	
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	public void applyConfig (ConfigurationPanel panel) {
        FQualitativePropertyConfigPanel p = (FQualitativePropertyConfigPanel) panel;
        class_validValues = p.getClass_validValues ();
		
        readyToUse = true;		// it will work
	}
	

	/**	From Configurable interface.
	*/
	@Override
	public String getConfigLabel () {
		// nb-06.08.2018
		//return NAME;
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
		return "class="+getClass().getName ()
				// nb-06.08.2018
				//+" name=\""+NAME+"\""
				+" name=\""+getName()+"\""
				+" class_validValues="+class_validValues;
	}

}


