package capsis.lib.cstability.context.poolinput;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.state.State;

/**
 * A manager of pool temporal inputs of C-STABILITY.
 * 
 * @author J. Sainte-Marie , F. de Coligny - May 2021
 */
public abstract class PoolInputManager {
	
	protected String bcName;
	protected String accessibilityKey;

	/**
	 * Constructor
	 */
	public PoolInputManager(String bcName, String accessibilityKey) {
		this.bcName = bcName;
		this.accessibilityKey = accessibilityKey;
	}

	/**
	 * getInput()
	 */
	public abstract PoolInput getInput(Parameters p, Context c, State s, double date) throws Exception;
	
}
