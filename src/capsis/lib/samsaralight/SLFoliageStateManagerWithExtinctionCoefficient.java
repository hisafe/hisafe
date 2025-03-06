package capsis.lib.samsaralight;

import capsis.lib.samsaralight.tag.SLTag;

/**
 * A SLFoliageStateManager with an extra feature : it gives an extinction
 * coefficient related to a given tree, beam, doy..
 * 
 * Used only in Tag mode
 * 
 * @author C. Rouet, F. de Coligny - April 2022
 */
public interface SLFoliageStateManagerWithExtinctionCoefficient extends SLFoliageStateManager {

	/**
	 * Returns the tree crown extinction coefficient, depending on height angle,
	 * azimuth, etc.
	 */
	public double getExtinctionCoefficientTree(SLLightableTree tree, SLCrownPart crownPart, double heightAngle_rad,
			double azimuth_rad, int doy, boolean isDirect, SLTag beamTag) throws Exception;

}
