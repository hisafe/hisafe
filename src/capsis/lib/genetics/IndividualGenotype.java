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

import java.util.Random;

/**
 * IndividualGenotype is a genotype of individual genotypable where, for each
 * locus, the allele is defined. It inherits the properties of Genotype.
 * 
 * @author I. Seynave - July 2002, F. de Coligny - November 2004
 */
public class IndividualGenotype extends Genotype {
	private short[][] nuclearDNA;
	private short[] mCytoplasmicDNA;
	private short[] pCytoplasmicDNA;

	private static Random random = new Random();

	/**
	 * Constructor for new IndividualGenotype.
	 */
	public IndividualGenotype(short[][] n, short[] m, short[] p) {
		nuclearDNA = n;
		mCytoplasmicDNA = m;
		pCytoplasmicDNA = p;
	}

	/**
	 * Should never be called. It's only technical support for
	 * EmptyIndividualGenotype
	 */
	public IndividualGenotype() {
	}

	/**
	 * Compute the genotype of a gamete from IndividualGenotype of its parents
	 * (i.e. simulate meiose processus).
	 */
	public IndividualGenotype getGamete(AlleleParameters ap) {

		short[][] nuclearDNA = getNuclearDNA();

		if (nuclearDNA != null && ap instanceof GeneticMap) {
			GeneticMap map = (GeneticMap) ap;
			int ploidy = (int) nuclearDNA[0].length;
			short[][] gamete = new short[nuclearDNA.length][ploidy / 2];

			float[] recombinationProbas = map.getRecombinationProbas();
			int proba1 = random.nextInt(2);

			// fix allele in first locus.
			for (int i = 0; i < gamete[0].length; i++) {
				gamete[0][i] = nuclearDNA[0][proba1];
			}

			// fix alleles in locus 2 to n.
			if (gamete.length > 1) {
				for (int i = 1; i < gamete.length; i++) {
					double proba = random.nextDouble();
					if (proba < recombinationProbas[i - 1]) {
						proba1 = Math.abs(proba1 - 1);
					}
					for (int j = 0; j < gamete[i].length; j++) {
						gamete[i][j] = nuclearDNA[i][proba1];
					}
				}
			}
			IndividualGenotype g = new IndividualGenotype(gamete, mCytoplasmicDNA, pCytoplasmicDNA);
			return g;
		} else {
			return null;
		}
	}

	public short[][] getNuclearDNA() {
		return nuclearDNA;
	}

	public short[] getMCytoplasmicDNA() {
		return mCytoplasmicDNA;
	}

	public short[] getPCytoplasmicDNA() {
		return pCytoplasmicDNA;
	}

	/**
	 * Compute a default IndividualGenotype for an individual genotypable of
	 * initial scene with empty IndividuelGenotype.
	 */
	public static IndividualGenotype computeDefaultIndividualGenotype(Genotypable gee) {
		IndividualGenotype genotype = (IndividualGenotype) gee.getGenotype();
		short[][] nuclearDNA = genotype.getNuclearDNA();
		short[] mCytoplasmicDNA = genotype.getMCytoplasmicDNA();
		short[] pCytoplasmicDNA = genotype.getPCytoplasmicDNA();

		short[][] defaultNuclearDNA = new short[nuclearDNA.length][nuclearDNA[0].length];
		short[] defaultMCytoplasmicDNA = new short[mCytoplasmicDNA.length];
		short[] defaultPCytoplasmicDNA = new short[pCytoplasmicDNA.length];

		for (int j = 0; j < nuclearDNA.length; j++) {
			for (int k = 0; k < nuclearDNA[0].length; k++) {
				defaultNuclearDNA[j][k] = -1;
			}
		}
		for (int j = 0; j < mCytoplasmicDNA.length; j++) {
			defaultMCytoplasmicDNA[j] = -1;
		}
		for (int j = 0; j < pCytoplasmicDNA.length; j++) {
			defaultPCytoplasmicDNA[j] = -1;
		}
		IndividualGenotype ng = new IndividualGenotype(defaultNuclearDNA, defaultMCytoplasmicDNA,
				defaultPCytoplasmicDNA);

		return ng;
	}

}
