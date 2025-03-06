package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;

/**
 * An interface to return the sum of potential value increment for a collection
 * of trees.
 * 
 * @author F. de Coligny, B. Courbaud - September 2022
 */
public interface PotentialValueIncrementProvider {

	/**
	 * Returns the sum of potential value increment (euro) for the given trees.
	 */
	public double getPotentialValueIncrement(GScene stand, Collection trees);


}
