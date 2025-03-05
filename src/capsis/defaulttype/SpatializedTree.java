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

import java.text.NumberFormat;

import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.Spatialized;
import capsis.kernel.GScene;

/**
 * SpatializedTree is a tree with spatial coordinates : for distance-dependent
 * models It inherits the properties of Tree. It can be subclassed to add
 * properties. Immutable properties can be stored in the immutable object
 * associated to Tree.
 * 
 * WARNING : if subclassed and subclass holds any object instance variables (not
 * primitive types), subclass must redefine "public Object clone ()" to provides
 * clonage for these objects (see GPlot.clone () for template).
 * 
 * @author F. de Coligny - june 1999
 */
public abstract class SpatializedTree extends Tree implements Spatialized {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	/**
	 * This class contains immutable instance variables for a logical
	 * SpatializedTree.
	 * 
	 * @see Tree
	 */
	public static class Immutable extends Tree.Immutable {

		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		public float x;
		public float y;
		public float z;
	}

	/**
	 * Constructor for new logical SpatializedTree. Immutable object is created.
	 * 
	 * @see Tree
	 */
	public SpatializedTree(int id, // Tree.Immutable
			GScene scene, int age, double height, double dbh, boolean marked, double x, // Immutable
			double y, // Immutable
			double z) { // Immutable

		super(id, scene, age, height, dbh, marked);

		// We complete this level of the Tree hierarchy

		// fc+bc-9.8.2017 allows to have x,y,z out of the immutable in a sub
		// class to change trees positions (see StandSpatializer)
		setX(x);
		setY(y);
		setZ(z);

		// fc+bc-9.8.2017 REMOVED
		// getImmutable().x = (float) x;
		// getImmutable().y = (float) y;
		// getImmutable().z = (float) z;

	}

	/**
	 * Constructor for new instance of existing logical SpatializedTree.
	 * Immutable object is retrieved from model.
	 * 
	 * @see Tree
	 */
	public SpatializedTree(SpatializedTree modelTree, // contains immutable to
														// retrieve
			GScene stand, int age, double height, double dbh, boolean marked) {
		super((Tree) modelTree, stand, age, height, dbh, marked);
	}

	/**
	 * Create an Immutable object whose class is declared at one level of the
	 * hierarchy. This is called only in constructor for new logical object in
	 * superclass. If an Immutable is declared in a subclass, the subclass must
	 * redefine this method (same body) to create an Immutable defined in
	 * subclass.
	 */
	protected void createImmutable() {
		immutable = new Immutable();
	}

	/**
	 * Returns the immutable reference casted for this level. getImmutable ().x
	 * may be used instead of ((Immutable) immutable).x. If an Immutable is
	 * declared in a subclass, the subclass can redefine this method with the
	 * same body.
	 */
	protected Immutable getImmutable() {
		return (Immutable) immutable;
	}

	/**
	 * Clone a SpatializedTree: first calls super.clone (), then clone the
	 * SpatializedTree instance variables.
	 */
	public Object clone() {
		SpatializedTree t = (SpatializedTree) super.clone();
		// t.cell = null; // MUST BE SET EXPLICITLY AFTER CLONING
		// see GStand.makeTreesPlotRegister ()

		return t;
	}

	public double getX() {
		return (double) getImmutable().x;
	}

	public double getY() {
		return (double) getImmutable().y;
	}

	public double getZ() {
		return (double) getImmutable().z;
	}

	public void setX(double v) {
		getImmutable().x = (float) v;
	}

	public void setY(double v) {
		getImmutable().y = (float) v;
	}

	public void setZ(double v) {
		getImmutable().z = (float) v;
	}

	public void setXYZ(double x, double y, double z) {
		setX(x);
		setY(y);
		setZ(z);
	}

	// No setX (), setY (), setZ () : immutable variables can not be changed

	// fc-11.10.2017
	public String toString() {
		NumberFormat nf = DefaultNumberFormat.getInstance();
		return super.toString() + ", x/y/z: "+nf.format(getX())+"/"+nf.format(getY())+"/"+nf.format(getZ());
	}

	public String bigString() {
		NumberFormat nf = DefaultNumberFormat.getInstance();

		StringBuffer sb = new StringBuffer(super.bigString());
		sb.append(" x: ");
		sb.append(nf.format(getX()));
		sb.append(" y: ");
		sb.append(nf.format(getY()));
		sb.append(" z: ");
		sb.append(nf.format(getZ()));

		return sb.toString();
	}

}
