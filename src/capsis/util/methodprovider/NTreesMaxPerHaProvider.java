package capsis.util.methodprovider;

import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;

/**
 * Max number of trees per ha a stand can have.
 *
 * @author F. de Coligny - December 2021
 */
public interface NTreesMaxPerHaProvider {

	/**
	 * NTreesMaxPerHaProvider: returns the max number of trees per hectare the given
	 * stand can contain, matching the given cg (girth of average tree). Note: cg
	 * may be matching a subset of trees of the stand (e.g. coming from an
	 * intervener working on a group of trees, e.g. a parcel in a Luberon2 stand).
	 */
	public double getNTreesMaxPerHa(GModel model, TreeList stand, double cg);

}
