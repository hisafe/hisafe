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

package capsis.defaulttype;

import java.io.Serializable;
import java.text.NumberFormat;
import java.util.List;

import capsis.defaulttype.plotofcells.CellTree;
import capsis.defaulttype.plotofcells.PlotOfCells;
import capsis.defaulttype.plotofcells.TreeListCell;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.kernel.PlotSubdivision;
import capsis.util.group.GroupableElement;
import capsis.util.group.GroupableType;
import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;

/**
 * The Tree description is for a tree characterized by only basic dendrometric
 * data. In the modules, it is possible to extend Tree or a subclass:
 * SpatializedTree or NumberableTree.
 * 
 * WARNING : if subclassed and subclass holds any object instance variables (not
 * primitive types), subclass must redefine "public Object clone ()" to provides
 * clonage for these objects (see RectangularPlot.clone () for template).
 *
 * @author F. de Coligny - june 1999
 */
public abstract class Tree
		implements Cloneable, Serializable, Identifiable, CellTree, SceneConnected, GroupableElement, Comparable {

	// fc-30.6.2020 added Comparable
	// fc-1.7.2019 GroupableElement

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a logical Tree.
	 * 
	 * A logical Tree is the set of Trees which share the same id, i.e. several
	 * consecutive states of the same initial tree. Some of their fields change and
	 * some are shared by all the occurrences of the logical tree: immutable.
	 */
	public static class Immutable implements Cloneable, Serializable {

		// nb-27.08.2018
		// private static final long serialVersionUID = 1L;

		public int id;
	}

	// The properties that do not change in time (e.g. id)
	protected Immutable immutable;

	// Tree main characteristics
	// Float variables are accessed with access methods (casts to double)
	protected GScene scene;
	protected int age; // (years)
	protected float height; // (m) total tree height
	protected float dbh; // (cm) diameter 1.3m (diameter at breast height)
	protected boolean marked; // a marked tree is considered dead by generic
								// tools
	protected TreeListCell cell; // may become immutable, using logical cell
									// #... differed

	/**
	 * Constructor for new logical tree. Immutable object is created.
	 */
	public Tree(int id, GScene scene, int age, double height, double dbh, boolean marked) {

		createImmutable();
		immutable.id = id; // do not redo it in subclass, just redefine
							// createImmutable () (same body)

		setScene(scene);
		setAge(age);
		setHeight(height);
		setDbh(dbh);
		setMarked(marked);
	}

	/**
	 * Constructor for new instance of existing logical tree. Immutable object is
	 * retrieved from model.
	 */
	public Tree(Tree modelTree, // contains immutable to retrieve
			GScene stand, int age, double height, double dbh, boolean marked) {
		this.immutable = modelTree.immutable; // that's why immutable objects
												// exist

		setScene(stand);
		setAge(age);
		setHeight(height);
		setDbh(dbh);
		setMarked(marked);
	}

	/**
	 * Create an Immutable object whose class is declared at one level of the
	 * hierarchy. This is called only in constructor for new logical object in
	 * superclass. If an Immutable is declared in a subclass, subclass must redefine
	 * this method (same body) to create an Immutable defined in the subclass.
	 */
	protected void createImmutable() {
		immutable = new Immutable();
	}

	/**
	 * Returns the type of the groupable element.
	 */
	public GroupableType getGroupableType() {
		return TreeList.GROUP_ALIVE_TREE;
	}

	/**
	 * Clone a Tree: first calls super.clone (), then clone the Tree instance
	 * variables.
	 */
	public Object clone() {
		try {
			Tree t = (Tree) super.clone(); // calls protected Object
											// Object.clone () {}
			t.immutable = immutable; // immutable variables

			t.scene = null; // MUST BE SET EXPLICITLY AFTER CLONING
			t.cell = null; // MUST BE SET EXPLICITLY AFTER CLONING

			return t;
		} catch (Exception e) {
			Log.println(Log.ERROR, "Tree.clone ()", "Error while cloning this tree: " + this, e);
			return null;
		}
	}

	/**
	 * Make an identical clone of a Tree. This special method can be used for status
	 * management for Numberable trees. The result musy be handled with care :
	 * changing some of the instance variables of the clone will alter the ones of
	 * the original : nothing must be altered. This device is for properties read
	 * only purpose. fc - 5.7.2005
	 */
	public Tree identicalClone() {
		try {
			Tree t = (Tree) super.clone(); // calls protected Object
											// Object.clone () {}
			t.immutable = immutable; // immutable variables

			return t;
		} catch (CloneNotSupportedException exc) {
			Log.println(Log.ERROR, "Tree.identicalClone ()",
					"Error while cloning tree." + " Source tree=" + toString() + " " + exc.toString(), exc);
			return null;
		}
	}

	public int getId() {
		return immutable.id;
	} // direct access -> fast

	public GScene getScene() {
		return scene;
	}

	public int getAge() {
		return age;
	}

	public double getHeight() {
		return (double) height;
	}

	public double getDbh() {
		return (double) dbh;
	}

	public boolean isMarked() {
		return marked;
	}

	public TreeListCell getCell() {
		return cell;
	}

	// No setId () : immutable variable can not be change
	public void setScene(GScene std) {
		scene = std;
	}

	public void setAge(int a) {
		age = a;
	}

	public void setHeight(double h) {
		height = (float) h;
	}

	public void setDbh(double d) {
		dbh = (float) d;
	}

	public void setMarked(boolean b) {
		marked = b;
	}

	public void setCell(TreeListCell c) {
		cell = c;
	}

	/**
	 * Returns the status of this tree in the TreeList statusMap if it is in a
	 * TreeList. If not, returns "".
	 */
	public String getStatusInScene() {
		try {
			TreeList l = (TreeList) scene;
			return l.getStatus(this);
		} catch (Exception e) {
		}
		return "";
	}

	/**
	 * Determine the cell of the tree. Only works for spatialized trees.
	 */
	// fc-8.9.2017 several matching cells now possible in several layers
	public void registerInPlot(Plot<? extends PlotSubdivision> plot) {

		PlotOfCells plotc = (PlotOfCells) plot; // fc-30.10.2017

		List cells = plotc.matchingCells((Spatialized) this);

		if (cells != null && !cells.isEmpty())
			for (Object o : cells)
				((TreeListCell) o).registerTree(this);

		// return cell;

	}

	@Override
	public int compareTo(Object other) {

		// fc-23.9.2020
		if (!(other instanceof Tree))
			return this.toString().compareTo(other.toString());

		Tree o = (Tree) other;

		// Tree natural ordering is ascending id, then dbh, then height

		if (this.getId() < o.getId())
			return -1;
		else if (this.getId() > o.getId())
			return 1;

		else if (this.getDbh() < o.getDbh())
			return -1;
		else if (this.getDbh() > o.getDbh())
			return 1;

		else if (this.getHeight() < o.getHeight())
			return -1;
		else if (this.getHeight() > o.getHeight())
			return 1;
		else
			return 0;

	}

	// fc-8.9.2017 ORIGINAL, see upper
	// /**
	// * Determine the cell of the tree.
	// * Only works for spatialized trees.
	// */
	// public TreeListCell registerInPlot (Plot<? extends TreeListCell> plot) {
	//
	// cell = plot.matchingCell((Spatialized)this);
	// if(cell != null) cell.registerTree (this);
	// return cell;
	//
	// }

	// fc-9.11.2017 REMOVED, see PlotOfCells.removeTree
	// public void unregisterFromPlot() {
	// if (isPlotRegistered()) {
	// cell.unregisterTree(this);
	// }
	// cell = null; // already done in GCell.unregisterTree
	// }

	// fc-9.11.2017 REMOVED, see PlotOfCells.removeTree
	// public boolean isPlotRegistered() {
	// return cell != null;
	// }

	public String toString() {
		return getClass().getSimpleName() + '_' + getId();
	}

	public String bigString() {
		NumberFormat nf = DefaultNumberFormat.getInstance();

		StringBuffer sb = new StringBuffer(toString());
		sb.append(" scene: ");
		sb.append(scene);
		sb.append(" age: ");
		sb.append(age);
		sb.append(" height: ");
		sb.append(nf.format(height));
		sb.append(" dbh: ");
		sb.append(nf.format(dbh));
		sb.append(" marked: ");
		sb.append(marked);
		if (cell != null) {
			sb.append(" cell: ");
			sb.append(cell);
		} else {
			sb.append(" *** NO CELL");
		}

		return sb.toString();
	}

}
