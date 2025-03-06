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

package capsis.util;

import java.util.Map;

import capsis.defaulttype.Species;
import capsis.kernel.GScene;
import capsis.lib.genetics.AlleleParameters;
import capsis.lib.genetics.GenoSpecies;
import capsis.lib.genetics.GenotypableImpl;
import capsis.lib.genetics.Genotype;
import capsis.lib.genetics.IndividualGenotype;
import capsis.lib.genetics.MultiGenotype;
import capsis.util.group.GroupableElement;

/**
 * A marker interface for fishes (for grouping).
 *
 * @author F. de Coligny - september 2004
 */
public interface GFish  extends GroupableElement {

	public int getId ();
	public float getForkLength ();
	public byte getAge ();
	public float getSurvivalProba ();
	public byte getSpawnCount ();
	public FishStand getFishStand();


	/**	Accessor for genotypable implementation.
	*/
	public GenotypableImpl getImpl ();

	/**	Tell if a tree is genotyped.
	*/
	public boolean isGenotyped () ;

	/**	Tell if a tree is a MultiGenotyped.
	*/
	public boolean isMultiGenotyped () ;

	public int getMId () ;
	public int getPId () ;
	public int getCreationDate () ;

	// this method must be defined
	public double getNumber () ;	// fc - 22.8.2006 - Numberable returns double

	public AlleleParameters getAlleleParameters () ;

	/**	This method is only used in tool methods.
	*	IMPORTANT: From everywhere, everyone MUST call getGenotype () !
	*/
	public MultiGenotype getMultiGenotype () ;

	/**	Set the given MultiGenotype as the genotype of this object.
	*/
	public void setMultiGenotype (MultiGenotype mg) ;


	/**	Set the given IndividualGenotype as the genotype of this object.
	*/
	public void setIndividualGenotype (IndividualGenotype ig) ;


	public Map getPhenoValues () ;
	public void setPhenoValues (Map m) ;

	/**	Return the genotype of the genotypable (main method, considers differed
	*	genotype computation).
	*/
	public Genotype getGenotype () ;

	/**	Return consanguinity.
	*	If tree is a mean tree, consanguinity is equal to mean individual consanguinity.
	*/
	public double getConsanguinity () ;

	public void setConsanguinity (double f) ;

	/**	Return global consanguinity.
	*/
	public double getGlobalConsanguinity () ;

	public void setGlobalConsanguinity (double f) ;

	/**	Return genetic value for the given caractere name.
	*/
	public double getGeneticValue (String caractereName) ;

	/**	Return fixed environmental value for the given parameter name.
	*/
	public double getFixedEnvironmentalValue (String parameterName) ;

	/**	Return pheno value for the given caractere name.
	*/
	public double getPhenoValue (String caractereName) ;

	public GScene getScene();
	//public void setScene (GStand stand) ;		// fc - 3.10.2005
//	public void setScene (FishStand stand) ;		// fc - 3.10.2005

	/**	Genotypable  interface.
	*/
	public GenoSpecies getGenoSpecies () ;	// for genetics2_0

	/**	Specieable  interface.
	*/
	public Species getSpecies () ;	// for genetics2_0

	/**	Version of the genetics library.
	*/
	public String getGeneticsVersion () ;


	public String toString () ;

}










