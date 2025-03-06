package capsis.util.group;

import java.util.Collection;

import capsis.kernel.GScene;

/**
 * An interface for GScene subclasses which contains elements compatible with
 * the Capsis grouping system.
 * 
 * @author F. de Coligny - July 2019
 */
public interface PossiblyGroupableScene extends GScene {

	// fc-1.7.2019 Groups review

	/**
	 * Returns true if the scene is groupable
	 */
	public boolean isGroupable();

	/**
	 * Returns the types of the elements that can be grouped in this scene.
	 * Returns null if !isGroupable ().
	 */
	public Collection<GroupableType> getGroupableTypes();

	/**
	 * Returns the collection of elements in this groupable scene matching the given
	 * type. Returns null if !isGroupable ().
	 */
	public Collection<GroupableElement> getGroupableElements(GroupableType type);

}
