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

/**
 * Genotypable is an interface for objects with a genotype.
 * 
 * @see GeneticTree
 * @author F. de Coligny - November 2004
 */
public interface Genotypable {

	public int getId();

	public GeneticScene getGeneticScene();

	public GenotypableImpl getImpl(); // fc - 16.11.2004

	public GenoSpecies getGenoSpecies(); // fc + cp - 10.11.2004

	public boolean isGenotyped();

	public boolean isMultiGenotyped();

	public int getMId();

	public int getPId();

	public int getCreationDate();

	public double getNumber(); // fc - 22.8.2006 - Numberable returns double

	// ~ public GeneticMap getGeneticMap (); // moved to GenoSpecies
	// ~ public void setGeneticMap (GeneticMap gm);

	public AlleleParameters getAlleleParameters(); // if IndividualGenotype
													// return GeneticMap else
													// AlleleDiversity

	// ~ public AlleleDiversity getAlleleDiversity (); // moved to GenoSpecies
	// ~ public void setAlleleDiversity (AlleleDiversity ad);

	// ~ public AlleleEffect getAlleleEffect (); // moved to GenoSpecies
	// ~ public void setAlleleEffect (AlleleEffect ae);

	// ~ public Kinship getKinship (); // moved to GenoSpecies

	public MultiGenotype getMultiGenotype();

	public void setMultiGenotype(MultiGenotype mg);

	public void setIndividualGenotype(IndividualGenotype ig);

	public Genotype getGenotype();

	public double getConsanguinity();

	public void setConsanguinity(double f);

	public double getGlobalConsanguinity();

	public void setGlobalConsanguinity(double f);

	public double getGeneticValue(String caractereName);

	public double getFixedEnvironmentalValue(String parameterName);

	public Map getPhenoValues(); // fc - 16.11.2004

	public void setPhenoValues(Map m); // fc - 16.11.2004

	public double getPhenoValue(String caractereName);

}
