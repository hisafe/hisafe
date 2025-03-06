package capsis.util.methodprovider;

import java.util.Map;

import lerfob.carbonbalancetool.productionlines.EndUseWoodProductCarbonUnit;
import capsis.kernel.GScene;

/**
 * This interface aims at providing the carbon by end use product
 * in a particular stand.
 * @author Mathieu Fortin - January 2010
 */
public interface EndUseProductProvider {
	/** 
	 * This method return a map whose key and value are 
	 * the product name and its volume respectively. 
	 * Note that this interface requires the stand only
	 * because the trees are by default selected from
	 * the collection "cut".
	 */
	public Map<Integer,EndUseWoodProductCarbonUnit> getInitialCarbonByEndUseProducts(GScene stand);
}
