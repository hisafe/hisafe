package capsis.lib.cstability.function.interfaces;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.function.util.Variables;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;

/**
 * An interface PrimitiveAvailable of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - June 2021
 */
public interface PrimitiveAvailable {

	/**
	 * executePrimitive(): in C-Stability, a primitive is always relative to second variable of the kernel
	 */
	public abstract double executePrimitive(Parameters p, Context c, State s, double[] discretization, Variables v) throws Exception;

	/**
	 * primitiveExpectedVariables(): return OneVariable or TwoVariables depending on what is
	 * expected in executePrimitive()
	 */
	public abstract Class primitiveExpectedVariables();
	
}
