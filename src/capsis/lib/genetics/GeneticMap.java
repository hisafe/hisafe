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

import java.io.Serializable;

/**
 * GeneticMap contains an array with the probabilities of recombination between
 * successive loci, and an array with the probabilities of crossing.
 * 
 * @author I. Seynave - July 2002, F. de Coligny - November 2004
 */
public class GeneticMap extends AlleleParameters implements Serializable {

	private float[] recombinationProbas;
	private float[] crossingProbas;

	/**
	 * Constructor for new GeneticMap.
	 */
	public GeneticMap(float[] rp, float[] cp) {
		recombinationProbas = rp;
		crossingProbas = cp;
	}

	public float[] getRecombinationProbas() {
		return recombinationProbas;
	}

	public float[] getCrossingProbas() {
		return crossingProbas;
	}

	/**
	 * Compute a default GeneticMap vhen it is not given in load file. By
	 * default all recombination propabilities are equal to 0.5.
	 */
	public static void computeDefautlRecombinationProbas(Genotypable gee) {
		Genotype genotype = gee.getGenotype();
		int nbloci;
		if (genotype instanceof IndividualGenotype) {
			short[][] nuclearDNA = ((IndividualGenotype) genotype).getNuclearDNA();
			nbloci = nuclearDNA.length;
		} else {
			int[][] nuclearAlleleFrequency = ((MultiGenotype) genotype).getNuclearAlleleFrequency();
			nbloci = nuclearAlleleFrequency.length;
		}
		float[] recombinationProba = new float[nbloci - 1];
		for (int i = 0; i < nbloci - 1; i++) {
			recombinationProba[i] = (float) 0.5;
		}
		GeneticMap gm = new GeneticMap(recombinationProba, null);
		gee.getGenoSpecies().setGeneticMap(gm);
	}

}
