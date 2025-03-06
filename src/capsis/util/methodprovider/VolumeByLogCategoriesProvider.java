package capsis.util.methodprovider;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import capsis.kernel.GScene;

/**
 * This interface ensures the methods required for the extractor DETimeVolumeByLogCategory
 * are available in the MethodProvider object.
 * @author Mathieu Fortin - October 2009
 */
public interface VolumeByLogCategoriesProvider {
	
	/**
	 * This method returns a map that contains the different tree log category names with their respective
	 * volume (m3). NOTE : this method should always return a map even if this map is empty.
	 */
	public Map<String,Double> getVolumeByLogCategories(GScene stand, Collection trees);
	
	/**
	 * This method returns the different log category names.
	 * @param param an Object instance that may serve as parameter (optional)
	 * @return a List of String
	 * @throws Exception
	 */
	public List<String> getTreeLogCategories(Object param);
	
}
