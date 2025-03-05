package capsis.kernel;

import java.io.Serializable;

/**
 * A superclass for cloneable objects, proposes a default implementation for
 * clone (), only for primitive types (can be overriden to take care of other
 * types if needed), also Serializable. This replaces AbstractSettings for the
 * tools settings, e.g. Panel2DSettings. AbstractSettings is the superclass of
 * the modules' settings in Capsis modules architecture 1.0.
 * 
 * @author F. de Coligny - October 2020
 */
public class AbstractCloneable implements Cloneable, Serializable {

	/**
	 * A default public implementation for clone () (Object's implementation is
	 * protected), was previously in AbstractSettings.
	 */
	public Object clone() {
		Object o = null;
		try {
			o = super.clone();
		} catch (Exception exc) {
		}
		return o;
	}

}
