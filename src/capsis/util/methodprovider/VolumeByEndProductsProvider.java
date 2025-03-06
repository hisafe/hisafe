package capsis.util.methodprovider;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import lerfob.carbonbalancetool.productionlines.EndUseWoodProductCarbonUnitFeature.UseClass;
import capsis.kernel.GScene;

/**
 * This interface ensures the methods required for the extractor DETimeVolumeByEndUseProducts
 * are available in the MethodProvider object.
 * @author Mathieu Fortin - August 2010
 */
public interface VolumeByEndProductsProvider {
	/**
	 * This method returns a map that contains the different end use products with their respective
	 * volume (m3). NOTE : this method should always return a map even if this map is empty.
	 */
	@SuppressWarnings("unchecked")
	public Map<UseClass,Double> getVolumeByEndUseProductClasses(GScene stand, Collection trees);
	public List<UseClass> getEndProductClasses(GScene stand) throws Exception;
}
