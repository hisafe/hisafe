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

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Tools;
import capsis.extensiontype.Filter;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Filter for objects with integer properties inside
 * 
 * @author F. de Coligny - september 2004
 */
public class FIntegers implements Filter, Configurable, Serializable {

	static {
		Translator.addBundle("capsis.extension.filter.general.FIntegers");
	}

	// nb-06.08.2018
	//public static final String NAME = "FIntegers";
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR =  "F. de Coligny";
	//public static final String DESCRIPTION = "FIntegers.description";
	
	protected Map methodName2acceptedValue;
	
	// fc - 21.5.2003 - added transient : referent must not be serialized (huge)
	//
    transient protected Object referent;		// complete original object (a complete GStand or GPlot...)
    transient protected Collection candidates;	// individuals to filter
	
	// methods which return int type and which are
	// implemented by every object in the collection to be filtered
	transient private Collection methods;		// fc - 26.3.2004 - transient

	private boolean readyToUse;

	public FIntegers () {
        methodName2acceptedValue = new Hashtable ();
	} 
	
//	/**	Constructor for non GUI mode. 
//	*	No more configuration required before preset () and retain ().
//	*	Needed in starter : methodName2acceptedValue.
//	*	Optional : referent and candidates (ex: in script mode).
//	*/
//	public FIntegers (FIntegersStarter s) {
//        methodName2acceptedValue = s.getMethodName2acceptedValue ();
//		
//        // Check input
//        if (methodName2acceptedValue == null) {return;}
//        if (methodName2acceptedValue.isEmpty ()) {return;}
//		
//		readyToUse = true;		// it will work (warcheinlich)
//	}
	
	public Object clone () {
		try {
			// We just need another instance to prevent preset () ant retain () from
			// being called by several threads at the same time. 
			return super.clone ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "FIntegers.clone ()", "Could not clone", e);
			return null;
		}
	}
	
	/**	Used by extensionManager to look for compatibilities.
	*/
	static public boolean matchWith (Object o) {
		if (o instanceof Collection) {
			Collection c = (Collection) o;
			if (c.isEmpty ()) {return false;}
			
			Collection intMethods = searchCommonIntMethods (c);
			if (intMethods != null && !intMethods.isEmpty ()) {
				return true;
			}
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("FIntegers.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FIntegers.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**	What methods of these individuals return ints ? Memorize them for retain ().
	*/
	@Override
	public void preset (Collection individuals) throws Exception {	// <- the good prototype from GFilter
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FIntegers.preset () - bad configuration for "+toString ());}
		
		methods = searchCommonIntMethods (individuals);
	}
	
	// Build a collection containing every common int methods 
	// for all the elements in the given collection
	// Warning : elements can be subclasses of a common superclass or interface
	// Tool method, used several times
	// protected because usable in FIntegersDialog
	static protected Collection searchCommonIntMethods (Collection c) {
		Collection reps = Tools.getRepresentatives (c);	// one instance of each class

		// Possibly several subclasses of same class
		Collection commonIntMethods = new ArrayList ();
		
		// Consider int methods in first representative
		Iterator i = reps.iterator ();
		if (i.hasNext ()) {
			Object indiv = i.next ();	// first representative
			commonIntMethods = Tools.getIntAccessors (indiv.getClass ());	
		}
		
		// If several representatives, retain only common int methods.
		// Test must be performed on method names ! 
		// (Two methods in two subclasses may have the same name but they are 
		// different Method instances).
		while (i.hasNext ()) {
			Object indiv = i.next ();
			Collection intMethods = Tools.getIntAccessors (indiv.getClass ());	
			
			Collection names = new HashSet ();	// HashSet is fast on contains ()
			for (Iterator j = intMethods.iterator (); j.hasNext ();) {
				Method m = (Method) j.next ();
				names.add (m.getName ());
			}
			
			for (Iterator k = commonIntMethods.iterator (); k.hasNext ();) {
				Method m = (Method) k.next ();
				if (!names.contains (m.getName ())) {k.remove ();}
			}
		}
		return commonIntMethods;
	}
	
	
	
	
	/**	Return true if the filter keeps the given individual.
	*	This means that the object corresponds to the rules of the filter.
	*/
	@Override
	public boolean retain (Object individual) throws Exception {
		// Check if can be applied
		if (!readyToUse) {throw new Exception (
				"FIntegers.retain () - bad configuration for "+toString ());}
		
		// Nothing configured : retain nothing
		if (methodName2acceptedValue == null || methodName2acceptedValue.isEmpty ()) {return false;}
		
		boolean	keepIt = true;
			
		// 1. Invoke every selected method (see preset) on current element
		for (Iterator k = methods.iterator (); k.hasNext ();) {
			Method m = (Method) k.next ();		// one of the selected methods
			try {
				Integer v = (Integer) m.invoke (individual);	// fc - 2.12.2004 - varargs
				
				// 2. Check if result is valid
				Integer acceptedValue = (Integer) methodName2acceptedValue.get (m.getName ());
				if (acceptedValue != null
						&& acceptedValue.intValue () != v.intValue ()) {
					keepIt = false;		// Forget it
					break;	// important: first difference is enough
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
		
        return new FIntegersDialog (this);
	}
	
	/**	GUI use : configuration panel configures the filter before preset () and retain () are called.
	*/
	public void applyConfig (ConfigurationPanel panel) {
        FIntegersDialog p = (FIntegersDialog) panel;
        methodName2acceptedValue = p.getMethodName2acceptedValue ();
		
        readyToUse = true;		// it will work
	}
	

	/**	From Configurable interface.
	*/
	public String getConfigLabel () {
		// nb-22.08.2018
		//return Translator.swap(NAME);
		return Translator.swap(getName());
	}
	
	// Needed because of Configurable and Extension interfaces, but unused
	public void postConfig () {}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}


