package capsis.lib.cstability.function.util;

/**
 * Variables of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
public abstract class Variables {
	
	/**
	 * checkIf()
	 */
	public void checkIf(Class klass) throws Exception {
		if (!(this.getClass ().isAssignableFrom(klass)))
			throw new Exception("Variables, expecting " + klass + ", found "+ this.getClass().getName());
	}
	
}
