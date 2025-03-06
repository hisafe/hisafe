package capsis.util.group;

import java.io.Serializable;

import jeeb.lib.util.Translator;

/**
 * An interface for elements compatible with the Capsis grouping system. See
 * PossiblyGroupableScene.
 * 
 * @author F. de Coligny - July 2019
 */
public class GroupableType implements Serializable {

	// The key is a String, must be unique and the same if Capsis is
	// launched in french or english, see getName () for a possible translation
	// in the user interface
	private String key;

	/**
	 * Constructor.
	 */
	public GroupableType(String key) {
		this.key = key;
	}

	public String getKey() {
		return key;
	}

	// fc-29.8.2019 Two GroupableTypes with same key must be equal
	@Override
	public boolean equals(Object obj) {

		// fc-29.8.2019
		// This method was overriden to fix a bug in
		// TreeList.getGroupableElement (), was returning EMPTY_LIST: two
		// GroupableTypes with the same key were considered different

		try {
			GroupableType o = (GroupableType) obj;
			return o.getKey().equals(this.getKey());
		} catch (Exception e) {
			return false;
		}
	}

	// fc-29.8.2019 Two GroupableTypes with same key must be equal
	@Override
	public int hashCode() {

		// fc-29.8.2019
		// This method was overriden to be consistent with the overriden
		// equals()

		return getKey().hashCode();
	}

	/**
	 * The name is the key translated by the Translator. The object implementing
	 * GroupableType is responsible for providing translations for the keys it
	 * declares.
	 */
	public String getName() {
		return Translator.swap(key);
	}

	@Override
	public String toString() {
		return getName();
	}
	
}
