package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;
import capsis.util.categorycalculator.ListOfCategories;

/**
 * An interface to return the mean potential dbh increment for a collection of
 * trees.
 * 
 * @author F. de Coligny, B. Courbaud - September 2022
 */
public interface PotentialDbhIncrementMeanProvider {

	/**
	 * Returns the mean potential dbh increment (cm) for the given trees.
	 */
	public double getPotentialDbhIncrementMean(GScene stand, Collection trees);

}
