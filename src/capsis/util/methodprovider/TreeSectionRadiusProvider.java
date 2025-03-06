package capsis.util.methodprovider;

import repicea.simulation.covariateproviders.treelevel.CrownBaseHeightProvider;
import repicea.simulation.treelogger.LoggableTree;


/**
 * This interface applies for a Tree derived class and
 * provides the method that returns the radius in cm
 * of a given section in a tree
 * @author Mathieu Fortin - March 2010
*/
public interface TreeSectionRadiusProvider extends LoggableTree, CrownBaseHeightProvider {
	
	/**
	 * This method returns the radius (cm) for a given section along the bole.
	 * @param height_m the height of the cross section
	 * @param overBark a boolean to indicate whether the radius should be calculated over or under bark
	 * @return the radius in cm (double)
	 */
	public double getSectionRadiusCm(double height_m, boolean overBark);
}
