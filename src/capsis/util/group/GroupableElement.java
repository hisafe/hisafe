package capsis.util.group;

/**
 * An interface for elements compatible with the Capsis grouping system. See
 * PossiblyGroupableScene.
 * 
 * @author F. de Coligny - July 2019
 */
public interface GroupableElement {

	// fc-1.7.2019 Groups review

	/**
	 * Returns the type of the groupable element.
	 */
	public GroupableType getGroupableType();

}
