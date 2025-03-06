package capsis.util.methodprovider;

import java.util.Collection;

import capsis.kernel.GScene;

/** 
 * This interface is to be implemented in the different MethodProvider classes that want to be compatible
 * with the DETimeBiomass dataextractor. 
 */
public interface BiomassProvider {
	
	/**
	 * This method returns the dry biomass in Mg (ton). 
	 * @param stand the GStand instance that comes from a particular step
	 * @param trees a Collection of Tree-derived instances
	 * @return a double (the dry biomass in tons)
	 */
	public double getBiomass(GScene stand, Collection trees);

}
