/* 
 * The Genetics library for Capsis4
 * 
 * Copyright (C) 2002-2004  Ingrid Seynave, Christian Pichot
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.lib.genetics;

import java.util.Map;

import capsis.defaulttype.SpatializedTree;
import capsis.kernel.GScene;

/**
 * GeneticTree is a SpatializedTree with genotype, mId, pId, creationDate,
 * genoValue and phenoValue for distance dependant models (i.e. MADD :
 * "Modèle Arbre Dépendant des Distances"). It inherits the properties of GTree.
 * It can be subclassed to add properties. Immutable properties can be stored in
 * the immutable object associated to GTree.
 * 
 * WARNING : if subclassed and subclass holds any object instance variables (not
 * primitive types), subclass must redefine "public Object clone ()" to provides
 * clonage for these objects (see GPlot.clone () for template).
 * 
 * @author I. Seynave - June 2002, C. Pichot - March 2003 & January 2004, F. de
 *         Coligny - November 2004
 */
public abstract class GeneticTree extends SpatializedTree implements Genotypable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	// fc - 5.11.2004 - some static methods were moved to GeneticTools
	// fc - 16.11.2004 - methods implementation moved to GenotypableImpl,
	// removed Immutable inner class

	protected MultiGenotype multiGenotype;
	protected Map phenoValue;

	private GenotypableImpl geeImpl; // fc - 16.11.2004

	/**
	 * Constructor for new logical GeneticTree.
	 */
	public GeneticTree(int id, GScene scene, int age, double height, double dbh, boolean marked, double x, double y,
			double z, Genotype genotype, int mId, int pId, int creationDate) throws Exception {

		super(id, scene, age, height, dbh, marked, x, y, z);

		geeImpl = new GenotypableImpl();
		geeImpl.init(this, genotype, mId, pId, creationDate, -1, -1, null, null);
	}

	/**
	 * Constructor for new instance of existing logical GeneticTree.
	 */
	public GeneticTree(GeneticTree modelTree, // contains immutable to retrieve
			GScene scene, int age, double height, double dbh, boolean marked) {
		super((SpatializedTree) modelTree, scene, age, height, dbh, marked);

		geeImpl = modelTree.getImpl();
		geeImpl.update(this);
	}

	/**
	 * Accessor for genotypable implementation.
	 */
	public GenotypableImpl getImpl() {
		return geeImpl;
	}

	/**
	 * Tell if a tree is genotyped.
	 */
	public boolean isGenotyped() {
		return geeImpl.isGenotyped(this);
	}

	/**
	 * Tell if a tree is a MultiGenotyped.
	 */
	public boolean isMultiGenotyped() {
		return geeImpl.isMultiGenotyped(this);
	} // fc & phd - 14.3.2003

	// --------------------------------------------------------------------------------------------------------->
	// A C C E S S O R S
	// --------------------------------------------------------------------------------------------------------->

	public int getMId() {
		return geeImpl.getMId();
	}

	public int getPId() {
		return geeImpl.getPId();
	}

	public int getCreationDate() {
		return geeImpl.getCreationDate();
	}

	// this method must be defined in subclass.
	public abstract double getNumber(); // fc - 22.8.2006 - Numberable returns
										// double

	public AlleleParameters getAlleleParameters() {
		return geeImpl.getAlleleParameters(this);
	}

	/**
	 * This method is only used in tool methods. IMPORTANT: From everywhere,
	 * everyone MUST call getGenotype () !
	 */
	// fc - 5.11.2004 - changed modifier to public for access from GeneticTools
	public MultiGenotype getMultiGenotype() {
		return multiGenotype;
	}

	/**
	 * Set the given MultiGenotype as the genotype of this object.
	 */
	public void setMultiGenotype(MultiGenotype mg) {
		multiGenotype = mg;
		if (mg != null) { // fc - 16.11.2004
			geeImpl.genotype = null;
		}
	}

	/**
	 * Set the given IndividualGenotype as the genotype of this object.
	 */
	public void setIndividualGenotype(IndividualGenotype ig) {
		geeImpl.genotype = ig;
		if (ig != null) { // fc - 16.11.2004
			multiGenotype = null;
		}
	}

	public Map getPhenoValues() {
		return phenoValue;
	}

	public void setPhenoValues(Map m) {
		phenoValue = m;
	}

	// --------------------------------------------------------------------------------------------------------->
	// G E N O T Y P E C O M P U T A T I O N
	// --------------------------------------------------------------------------------------------------------->

	/**
	 * Return the genotype of the genotypable (main method, considers differed
	 * genotype computation).
	 */
	public Genotype getGenotype() {
		return geeImpl.getGenotype(this);
	}

	// --------------------------------------------------------------------------------------------------------->
	// C O N S A N G U I N I T Y
	// --------------------------------------------------------------------------------------------------------->

	/**
	 * Return consanguinity. If tree is a mean tree, consanguinity is equal to
	 * mean individual consanguinity.
	 */
	public double getConsanguinity() {
		return geeImpl.getConsanguinity(this);
	}

	public void setConsanguinity(double f) {
		geeImpl.setConsanguinity(f);
	}

	/**
	 * Return global consanguinity.
	 */
	public double getGlobalConsanguinity() {
		return geeImpl.getGlobalConsanguinity(this);
	}

	public void setGlobalConsanguinity(double f) {
		geeImpl.setGlobalConsanguinity(f);
	}

	// --------------------------------------------------------------------------------------------------------->
	// T O O L S T O C O M P U T E Q U A N T I T A T I V E G E N E T I C P A R A
	// M E T E R
	// --------------------------------------------------------------------------------------------------------->

	/**
	 * Return genetic value for the given caractere name.
	 */
	public double getGeneticValue(String caractereName) {
		return geeImpl.getGeneticValue(this, caractereName);
	}

	/**
	 * Return fixed environmental value for the given parameter name.
	 */
	public double getFixedEnvironmentalValue(String parameterName) {
		return geeImpl.getFixedEnvironmentalValue(this, parameterName);
	}

	/**
	 * Return pheno value for the given caractere name.
	 */
	public double getPhenoValue(String caractereName) {
		return geeImpl.getPhenoValue(this, caractereName);
	}

	/**
	 * Version of the genetics library.
	 */
	public String getGeneticsVersion() {
		return geeImpl.getGeneticsVersion(); // fc - 5.11.2004 - genetics
												// version can be checked in an
												// inspector
	}

	/**
	 * String representation.
	 */
	public String toString() {
		StringBuffer b = new StringBuffer("gee[id=");
		b.append(getId());
		b.append(",mId=");
		b.append(geeImpl.mId);
		b.append(",pId=");
		b.append(geeImpl.pId);
		b.append("] ");
		return b.toString();
	}

}
