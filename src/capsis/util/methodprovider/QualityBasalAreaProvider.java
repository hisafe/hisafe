package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;
import capsis.kernel.MethodProvider;
import capsis.util.categorycalculator.ListOfCategories;

/**
 * An interface to return the list of basal area (m2) per quality categories for a collection of
 * trees.
 * 
 * @author F. de Coligny - May 2021
 */
public interface QualityBasalAreaProvider extends MethodProvider {

	/**
	 * Returns a list of basalArea (m2) per quality categories for the given trees.
	 */
	public ListOfCategories getListOfQualityBasalArea(GScene stand, Collection trees);

}
