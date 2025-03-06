package capsis.lib.cstability.util;

import java.io.Serializable;

import capsis.lib.cstability.state.State;

/**
 * An interface to be told when a new state is stored in C-STABILITY Simulator
 *
 * @author J. Sainte-Marie, F. de Coligny - May 2021
 */
public interface StateStorageListener extends Serializable {

	/**
	 * stateStored(): called when a new state is stored in the Simulator.
	 */
	public void stateStored(State s);

}
