package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;

/**
 * An interface to return the mean potential height increment for a collection of
 * trees.
 * 
 * @author F. de Coligny, B. Courbaud - September 2022
 */
public interface PotentialHeightIncrementMeanProvider {

	/**
	 * Returns the mean potential height increment (m) for the given trees.
	 */
	public double getPotentialHeightIncrementMean(GScene stand, Collection trees);

}
