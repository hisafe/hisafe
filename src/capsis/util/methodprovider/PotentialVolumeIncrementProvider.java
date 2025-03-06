package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;

/**
 * An interface to return the sum of potential volume increment for a collection
 * of trees.
 * 
 * @author F. de Coligny, B. Courbaud - September 2022
 */
public interface PotentialVolumeIncrementProvider {

	/**
	 * Returns the sum of potential volume increment (m3) for the given trees.
	 */
	public double getPotentialVolumeIncrement(GScene stand, Collection trees);

}
