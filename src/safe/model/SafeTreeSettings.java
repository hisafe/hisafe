package safe.model;

import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import capsis.kernel.AbstractSettings;

/**
 * SafeTreeSettings - List of tree species settings.
 *
 * @author Isabelle Lecomte - July 2002
 */
public class SafeTreeSettings extends AbstractSettings {

	private Map species; // Map treeSpeciesName -> treeSpecies object


	/** Constructor
	*/
	public SafeTreeSettings () {
		species= new Hashtable ();
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
	public Collection getList () {
		return species.values ();
	}


	/**	To check the list in the Capsis inspector
	*/
	public String toString() {
		String s = "Tree Species list = ";
		for (Iterator i = species.values ().iterator (); i.hasNext ();) {
			SafeTreeSpecies sp = (SafeTreeSpecies) i.next ();

			s+=sp.getValue ()+" "+sp.getName ();
			if (i.hasNext ()) {s+=" - ";}
		}
		return s;
	}

}



