package capsis.defaulttype;

import capsis.kernel.GScene;

/**
 * An interface for objects with a getScene () method, returning a Capsis scene
 * (a subclass of GScene).
 * 
 * @author F. de Coligny - June 2018
 */
public interface SceneConnected {

	/**
	 * Returns the GScene object this object is connected to.
	 */
	public GScene getScene ();
	
}
