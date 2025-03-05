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
import capsis.kernel.GScene;

/**
 * NumberableTree is a tree for distance-independent models This tree has no
 * coordinates. It's characterized by basic dendrometric data (inherited from
 * Tree) and a number (it represents a number of trees). It can be subclassed to
 * add properties. Immutable properties can be stored in the immutable object
 * associated to Tree.
 * 
 * @author F. de Coligny - june 1999/march 2001
 */
public abstract class NumberableTree extends Tree implements Numberable {
	// WARNING: if references to objects (not primitive types) are added here,
	// implement a "public Object clone ()" method (see GPlot.clone () for template)

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	protected double number;		// number of trees in this category
	protected int numberOfDead;		// since previous step


	/**
	 * Constructor for new logical NumberableTree. 
	 * Immutable object is created.
	 * @see Tree
	 */
	public NumberableTree (int 	id, 			// immutable
						GScene 	stand, 
						int 	age, 
						double 	height, 
						double 	dbh,
						boolean	marked,
						double	number, 
						int		numberOfDead) {
		
		super (id, stand, age, height, dbh, marked);
		this.number = number;
		this.numberOfDead = numberOfDead;
	}
	
	/**
	 * Constructor for new instance of existing logical NumberableTree. 
	 * Immutable object is retrieved from model.
	 * @see Tree
	 */
	public NumberableTree (	NumberableTree	modelTree, 		// contains immutable to retrieve
						GScene 		stand, 
						int 		age, 
						double 		height, 
						double 		dbh,
						boolean		marked,
						double		number, 
						int			numberOfDead) {
		super ((Tree) modelTree, stand, age, height, dbh, marked);
		this.number = number;
		this.numberOfDead = numberOfDead;
	}

	public double getNumber () {return number;}
	public void setNumber (double eff) {number = eff;}

	public int getNumberOfDead () {return numberOfDead;}
	public void setNumberOfDead (int eff) {numberOfDead = eff;}

	public String bigString () {
		NumberFormat nf = DefaultNumberFormat.getInstance ();
		
		StringBuffer sb = new StringBuffer (super.bigString ());
		sb.append (" number: ");
		sb.append (nf.format (number));
		sb.append (" numberOfDead: ");
		sb.append (numberOfDead);
		return sb.toString ();
	}
	
	
}
