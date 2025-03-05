package capsis.kernel.extensiontype;

import capsis.kernel.GScene;
import capsis.util.group.GroupableIntervener;
import capsis.util.group.Grouper;

/**
 * A superclass for groupable interveners. Groupable interveners can be
 * restricted to elements in a group.
 * 
 * @author F. de Coligny - June 2020
 */
public abstract class AbstractGroupableIntervener implements GroupableIntervener {

	// fc-8.6.2020 Added this superclass for Groupers management in the
	// interveners (related to getSceneArea ())
	// A grouper may be added with setGrouper (), or not (grouper stays null)

	private Grouper grouper; // may be null

	public void setGrouper(Grouper grouper) {
		this.grouper = grouper;
	}

	public Grouper getGrouper() {
		return grouper;
	}

	/**
	 * Returns the area of the scene in m2, or a smaller area if a grouper is active
	 * and has an area (e.g. a grouper based on a polygon). This area is to be used
	 * for per hectare computation.
	 */
	public double getSceneArea(GScene scene) {

		return AbstractGroupableIntervener.getSceneArea(grouper, scene);

	}

	/**
	 * This static method can be called if a subclasses already extends some class
	 * and can not extend AbstractGroupableIntervener. E.g. FiEmpiricalFireEffect.
	 */
	static public double getSceneArea(Grouper grouper, GScene scene) {

		if (grouper == null)
			return scene.getArea();

//		System.out.println("[AbstractGroupableIntervener] getSceneArea(): " + grouper.getArea(scene)); // commented by gl 11-08-2022

		return grouper.getArea(scene);

	}

}
