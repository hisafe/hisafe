/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.group;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;

import capsis.kernel.GScene;

/**
 * Group knows what composite objects can be grouped in Capsis. A composite is
 * an object containing a Collection of individuals. The individuals must be
 * instance of Identifiable. Composite type is the name of one individual in the
 * composite (ex: TREE, CELL...). The "which" methods makes it possible to find
 * the Collection of individuals in the composite and their type. Ex: If the
 * composite is a TreeList (instanceof TreeCollection), composite type is TREE
 * and Collection is ((TreeCollection) composite).getTrees (). The Collection of
 * individuals can be grouped with Groupers (see Grouper, Identifier and
 * Filtrer).
 * 
 * @author F. de Coligny - April 2004
 */
public class Group implements Serializable {
	
	// fc-1.7.2019 Groups refactoring, complete review
	
	// fc - 6.1.2009 - corrected bug #147 by Frederic Mothe - WQW and Viewing
	// toolkit should not depend on mustard model

	// Possible types for individuals concerned by grouping.
	// Each name should be translated in capsis/Labels_fr/en.properties.
	//
	// ~ static public final String ALL_TYPES = "allTypes"; // fc - 3.6.2008 -
	// see GrouperChooser
	// static public final String UNKNOWN = "unknown";
	// static public final String TREE = "tree"; // TreeCollection contains
	// GTree
	// static public final String CELL = "cell"; // GPlot contains GCell
	// static public final String FISH = "fish"; // FishComposite contains GFish
	// static public final String REACH = "reach"; // ReachComposite contains
	// // GReach
	// static public final String WEIR = "weir"; // WeirComposite contains GWeir
	// static public final String MANAGEMENT_UNIT = "managementUnit"; // Mustard
	// // module -
	// // fc -
	// // 3.6.2008

	static public final byte AND = 0; // AND constant, see ComplexGrouper
	static public final byte OR = 1; // OR constant, see ComplexGrouper

	// static private Collection possibleTypes = new HashSet();
	// static {
	// possibleTypes.add(TREE);
	// possibleTypes.add(CELL);
	// possibleTypes.add(FISH);
	// possibleTypes.add(REACH);
	// possibleTypes.add(WEIR);
	// possibleTypes.add(MANAGEMENT_UNIT);
	// }

	/**
	 * Return true if given scene is known by the grouping system.
	 */
	static public boolean isGroupable(GScene scene) {

		// fc-1.7.2019 Groups refactoring
		if (!(scene instanceof PossiblyGroupableScene))
			return false;

		return ((PossiblyGroupableScene) scene).isGroupable();

		// if (scene instanceof TreeCollection) {
		// return true;
		// } // TREE
		//
		// if (scene.getPlot() != null && scene.getPlot() instanceof
		// PlotOfCells) {
		//
		// PlotOfCells plotc = scene.getPlot(); // fc-26.10.2017
		//
		// return plotc.hasCells ();
		// } // CELL
		//
		// if (scene instanceof FishComposite) {
		// return true;
		// } // FISH
		//
		// if (scene instanceof ReachComposite) {
		// return true;
		// } // REACH
		//
		// if (scene instanceof WeirComposite) {
		// return true;
		// } // WEIR
		//
		// try { // fc-6.1.2009
		// if (scene instanceof MustForest) {
		// return true;
		// } // MANAGEMENT_UNIT
		// } catch (Error e) {
		// }
		//
		// return false;
	}

	// /**
	// * Return the Collection of possible group types.
	// */
	// static public Collection getPossibleTypes() {
	// return possibleTypes;
	// }

	/**
	 * Return the Collection of possible group types for this stand.
	 */
	static public Collection<GroupableType> getPossibleTypes(GScene scene) {

		if (!isGroupable(scene))
			return new ArrayList<GroupableType>();

		// fc-1.7.2019 Groups refactoring
		return ((PossiblyGroupableScene) scene).getGroupableTypes();

		// Collection c = new ArrayList();
		//
		// // fc-11.1.2017
		// if (!isGroupable(scene))
		// return c;
		//
		//
		// if (scene instanceof TreeCollection) {
		// c.add(TreeList.GROUP_ALIVE_TREE);
		// }
		// if (scene.getPlot() != null) {
		// c.add(Group.CELL);
		// }
		// if (scene instanceof FishComposite) {
		// c.add(FishGroupHelper.GROUP_FISH);
		// }
		// if (scene instanceof ReachComposite) {
		// c.add(Group.REACH);
		// }
		// if (scene instanceof WeirComposite) {
		// c.add(Group.WEIR);
		// }
		// try { // fc - 6.1.2009
		// if (scene instanceof MustForest) {
		// c.add(Group.MANAGEMENT_UNIT);
		// }
		// } catch (Error e) {
		// }
		// return c;
	}

	/**
	 * Return the possible types of individuals the grouping system can handle.
	 * Referent must be a GroupableElement.
	 */
	static public GroupableType whichType(Object individual) {

		if (!(individual instanceof GroupableElement))
			return null;

		return ((GroupableElement) individual).getGroupableType();

		// if (individual instanceof Tree) {
		// return TreeList.GROUP_ALIVE_TREE;
		// }
		// if (individual instanceof Cell) {
		// return Group.CELL;
		// }
		// if (individual instanceof GFish) {
		// return FishGroupHelper.GROUP_FISH;
		// }
		// if (individual instanceof GReach) {
		// return Group.REACH;
		// }
		// if (individual instanceof GWeir) {
		// return Group.WEIR;
		// }
		// try { // fc - 6.1.2009
		// if (individual instanceof MustManagementUnit) {
		// return Group.MANAGEMENT_UNIT;
		// }
		// } catch (Error e) {
		// }
		// Log.println(Log.ERROR, "Group.whichType ()", "can not find type for
		// individual: " + individual
		// + ", returned Group.UNKNOWN");
		// return Group.UNKNOWN;
	}

	/**
	 * Return the Collection of individuals of the given type in the stand.
	 * Note: individuals must be instance of Identifiable.
	 */
	static public Collection<GroupableElement> whichCollection(GScene scene, GroupableType type) {

		if (!isGroupable(scene))
			return new ArrayList<GroupableElement>();

		// fc-1.7.2019 Groups refactoring
		return ((PossiblyGroupableScene) scene).getGroupableElements(type);

		// // fc-11.1.2017
		// if (!isGroupable(scene))
		// return new ArrayList();
		//
		//
		// if (type == null) {
		// return new ArrayList();
		// } // fc - 2.6.2008
		// if (type.equals(TREE) && scene instanceof TreeCollection) {
		// return ((TreeCollection) scene).getTrees();
		// }
		// if (type.equals(CELL) && scene.getPlot() != null && scene.getPlot()
		// instanceof PlotOfCells) {
		//
		// PlotOfCells plotc = scene.getPlot(); // fc-26.10.2017
		//
		// return plotc.getCells();
		// }
		// if (type.equals(FISH) && scene instanceof FishComposite) {
		// return ((FishComposite) scene).getFishes();
		// }
		// if (type.equals(REACH) && scene instanceof ReachComposite) {
		// return ((ReachComposite) scene).getReachMap().values();
		// }
		// if (type.equals(WEIR) && scene instanceof WeirComposite) {
		// return ((WeirComposite) scene).getWeirMap().values();
		// }
		// try { // fc - 6.1.2009
		// if (type.equals(MANAGEMENT_UNIT) && scene instanceof MustForest) {
		// return ((MustForest) scene).getManagementUnits();
		// }
		// } catch (Error e) {
		// }
		// Log.println(Log.ERROR, "Group.whichCollection ()", "can not find
		// Collection of type: " + type + " in stand: "
		// + scene + ", returned null");
		// return null;
	}

}
