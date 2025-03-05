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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.extensiontype.Filter;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Filter for objects with numeric properties inside.
 * Considered numeric types are double, float, long, int, short and byte.
 * Chose low / high thresholds for each chosen variable.
 * 
 * @author F. de Coligny - september 2004
 */
public class FNumericThresholds implements Filter, Configurable, Serializable {

	static {
		Translator.addBundle("capsis.extension.filter.general.FNumericThresholds");
	} 

	// nb-06.08.2018
	//public static final String NAME = "FNumericThresholds";
	//public static final String VERSION = "1.1";
	//public static final String AUTHOR =  "F. de Coligny";
	//public static final String DESCRIPTION = "FNumericThresholds.description";
	
	protected Map methodName2acceptedValues;	// String -> double[2], ex:  getId -> [0, 55[
	
	// fc - 21.5.2003 - added transient : referent must not be serialized (huge)
	//
    transient protected Object referent;		// complete original object (a complete GStand or GPlot...)
    transient protected Collection candidates;	// individuals to filter
	
	// methods which return numeric types and which are
	// implemented by every object in the collection to be filtered
	transient private Collection methods;		// fc - 26.3.2004 - transient

	private boolean readyToUse;
	
	/**	A constructor for GUI use.
	*	Configuration is needed before preset () and retain ().
	*/
	public FNumericThresholds () {
        methodName2acceptedValues = new HashMap ();
	} 
	
	/**	Constructor for non GUI mode. 
	*	No more configuration required before preset () and retain ().
	*	Needed in starter : methodName2acceptedValues.
	*	Optional : referent and candidates (ex: in script mode).
	*/
	public FNumericThresholds (Map methodName2acceptedVals) {
        methodName2acceptedValues = methodName2acceptedVals;
		
        // Check input
        if (methodName2acceptedValues == null) {return;}
        if (methodName2acceptedValues.isEmpty ()) {return;}
		
		readyToUse = true;		// it will work (warcheinlich)
	}
	
	public Object clone () {
		try {
			// We just need another instance to prevent preset () ant retain () from
			// being called by several threads at the same time. 
			return super.clone ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "FNumericThresholds.clone ()", "Could not clone", e);
			return null;
		}
	}
	
	/**	Used by extensionManager to look for compatibilities.
	*/
	static public boolean matchWith (Object o) {
		if (o instanceof Collection) {
			Collection c = (Collection) o;
			if (c.isEmpty ()) {return false;}
			
			Collection numericMethods = searchCommonNumericMethods (c);
			if (numericMethods != null && !numericMethods.isEmpty ()) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("FNumericThresholds.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FNumericThresholds.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}

	/**	What methods of these individuals return numeric types ? Memorize them for retain ().
	*/
	@Override
	public void preset (Collection individuals) throws Exception {	// <- the good prototype from GFilter
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FNumericThresholds.preset () - bad configuration for "+toString ());}
		
		methods = searchCommonNumericMethods (individuals);
	}
	
	// Build a collection containing every common numeric methods 
	// for all the elements in the given collection
	// Warning : elements can be subclasses of a common superclass or interface
	// Tool method, used several times
	// protected because usable in FNumericThresholdsDialog
	static protected Collection searchCommonNumericMethods (Collection c) {
		Collection reps = Tools.getRepresentatives (c);	// one instance of each class

		// Possibly several subclasses of same class
		Collection commonNumericMethods = new ArrayList ();
		
		// Consider int methods in first representative
		Iterator i = reps.iterator ();
		if (i.hasNext ()) {
			Object indiv = i.next ();	// first representative
			commonNumericMethods = Tools.getNumericAccessors (indiv.getClass ());	
		}
		
		// If several representatives, retain only common numeric methods.
		// Test must be performed on method names ! 
		// (Two methods in two subclasses may have the same name but they are 
		// different Method instances).
		while (i.hasNext ()) {
			Object indiv = i.next ();
			Collection numericMethods = Tools.getNumericAccessors (indiv.getClass ());	
			
			Collection names = new HashSet ();	// HashSet is fast on contains ()
			for (Iterator j = numericMethods.iterator (); j.hasNext ();) {
				Method m = (Method) j.next ();
				names.add (m.getName ());
			}
			
			for (Iterator k = commonNumericMethods.iterator (); k.hasNext ();) {
				Method m = (Method) k.next ();
				if (!names.contains (m.getName ())) {k.remove ();}
			}
		}
		return commonNumericMethods;
	}
	
	
	
	
	/**	Return true if the filter keeps the given individual.
	*	This means that the object corresponds to the rules of the filter.
	*/
	@Override
	public boolean retain (Object individual) throws Exception {
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FNumericThresholds.retain () - bad configuration for "+toString ());}
		
		// Nothing configured : retain nothing
		if (methodName2acceptedValues == null || methodName2acceptedValues.isEmpty ()) {return false;}
		
		boolean	keepIt = true;
			
		// 1. Invoke every selected method (see preset) on current element
		for (Iterator k = methods.iterator (); k.hasNext ();) {
			Method m = (Method) k.next ();		// one of the selected methods
			try {
				// o can be a Double, Float, Long, Integer, Short or Byte instance 
				// -> consider a double
				Object o = m.invoke (individual);	// fc - 2.12.2004 - varargs
				double v = ((Number) o).doubleValue ();
				
				// 2. Check if result is valid
				double[] acceptedValues = (double[]) methodName2acceptedValues.get (m.getName ());
				if (acceptedValues != null) {
					double low = acceptedValues[0];
					double high = acceptedValues[1];
					
					if (v < low || v >= high) {
						keepIt = false;		// Forget it
						break;	// important: first difference is enough
					}
				}
				
			} catch (Exception e) {	
				// if we invoke method LubTree1.isSomething () on LubTree2 -> exception -> next method
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
		
        return new FNumericThresholdsDialog (this);
	}
	
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	@Override
	public void applyConfig (ConfigurationPanel panel) {
        FNumericThresholdsDialog p = (FNumericThresholdsDialog) panel;
        methodName2acceptedValues = p.getMethodName2acceptedValues ();
		
        readyToUse = true;		// it will work
	}
	
	

	/**	From Configurable interface.	*/
	@Override
	public String getConfigLabel () {
		// nb-22.08.2018
		//return Translator.swap(NAME);
		return Translator.swap(getName());
	}
	
	// Needed because of Configurable and Extension interfaces, but unused
	@Override
	public void postConfig () {}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	/**	Normalized toString () method.
	*/
	public String toString () {
		return "class="+getClass().getName ()
				// nb-22.08.2018
				//+" name=\""+ NAME +"\""
				+" name=\""+ getName() +"\""
				+" methodName2acceptedValues="+methodName2acceptedValues;
	}

}


