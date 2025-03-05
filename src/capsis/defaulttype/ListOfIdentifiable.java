package capsis.defaulttype;

import java.util.Collection;

import jeeb.lib.util.Identifiable;

/**
 * An interface for objects containing a list of objects instanceof Identifiable.
 * 
 * @author F. de Coligny - February 2020
 */
public interface ListOfIdentifiable {

	// fc-13.2.2020 This interface should be moved to jeeb.lib.util
	
	/**
	 * Returns the Identifiable object with the given id.
	 */
	public Identifiable getIdentifiable (int id);
	
	/**
	 * Returns the list of available ids.
	 */
	public Collection<Integer> getIds (); // fc-15.10.2020
	
	/**
	 * Returns the list of all identifiables.
	 */
	public Collection<Identifiable> getIdentifiables (); // fc-16.12.2020
	
}
