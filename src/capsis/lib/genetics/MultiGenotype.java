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

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

/**
 * MultiGenotype is a genotype of a mean genotypable where the number of alleles
 * is defined for each locus. The list of potential alleles must be given in
 * specific parameters (AlleleDiversity). It inherits the properties of Genotype.
 * 
 * @author I. Seynave - June 2002 & C. Pichot - October 2003, F. de Coligny -
 *         November 2004
 */
public class MultiGenotype extends Genotype {
	private int[][] nuclearAlleleFrequency;
	private int[][] mCytoplasmicAlleleFrequency;
	private int[][] pCytoplasmicAlleleFrequency;
	private static Random random = new Random();

	/**
	 * Constructor for new MultiGenotype.
	 */
	public MultiGenotype(int[][] n, int[][] m, int[][] p) {
		nuclearAlleleFrequency = n;
		mCytoplasmicAlleleFrequency = m;
		pCytoplasmicAlleleFrequency = p;
	}

	/**
	 * Should never be called. It's only technical support for
	 * EmptyMultiGenotype.
	 */
	public MultiGenotype() {
	}

	public int[][] getNuclearAlleleFrequency() {
		return nuclearAlleleFrequency;
	}

	public int[][] getMCytoplasmicAlleleFrequency() {
		return mCytoplasmicAlleleFrequency;
	}

	public int[][] getPCytoplasmicAlleleFrequency() {
		return pCytoplasmicAlleleFrequency;
	}

	private void setNuclearAlleleFrequence(int[][] naf) {
		nuclearAlleleFrequency = naf;
	}

	private void setMCytoplasmicAlleleFrequence(int[][] maf) {
		mCytoplasmicAlleleFrequency = maf;
	}

	private void setPCytoplasmicAlleleFrequence(int[][] paf) {
		pCytoplasmicAlleleFrequency = paf;
	}

	/**
	 * Returns an image of the given MultiGenotype for the Genetics memory
	 * system (returns null if original is null).
	 */
	public static MultiGenotype getMemo(MultiGenotype original) { // fc+som-22.5.2017
		if (original == null) {
			return null;
		} else {
			MultiGenotype copy = new MultiGenotype(GeneticTools.arrayCopy(original.nuclearAlleleFrequency),
					GeneticTools.arrayCopy(original.mCytoplasmicAlleleFrequency),
					GeneticTools.arrayCopy(original.pCytoplasmicAlleleFrequency));

			// fc+som-22.5.2017 could not find a way to get an exact copy of
			// this random with its internal state, does not matter
			copy.random = original.random;
			return copy;

		}
	}

	/**
	 * Compute the IndividualGenotype of a gamete from the multigenotype of its
	 * parents.
	 */
	public IndividualGenotype getGamete(AlleleParameters ap) {

		int[][] nuclearAlleleFrequency = getNuclearAlleleFrequency();
		int[][] mCytoplasmicAlleleFrequency = getMCytoplasmicAlleleFrequency();
		int[][] pCytoplasmicAlleleFrequency = getPCytoplasmicAlleleFrequency();

		AlleleDiversity ad;

		if (ap instanceof AlleleDiversity) {
			ad = (AlleleDiversity) ap;
			short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity();
			short[][] mCytoplasmicAlleleDiversity = ad.getMCytoplasmicAlleleDiversity();
			short[][] pCytoplasmicAlleleDiversity = ad.getPCytoplasmicAlleleDiversity();

			short[][] gameteNuclear = new short[nuclearAlleleDiversity.length][1];
			short[] gameteMCytoplasmic = new short[mCytoplasmicAlleleDiversity.length];
			short[] gametePCytoplasmic = new short[pCytoplasmicAlleleDiversity.length];

			// compute nuclearDNA of gamete.
			if (nuclearAlleleFrequency != null && nuclearAlleleDiversity != null) {
				for (int i = 0; i < gameteNuclear.length; i++) {
					int[] cumFrequence = new int[nuclearAlleleFrequency[i].length];
					cumFrequence[0] = nuclearAlleleFrequency[i][0];
					for (int j = 1; j < nuclearAlleleFrequency[i].length; j++) {
						cumFrequence[j] = cumFrequence[j - 1] + nuclearAlleleFrequency[i][j];
					}
					int allele = random.nextInt(cumFrequence[cumFrequence.length - 1]);
					int compteur = 0;
					while (allele > cumFrequence[compteur]/*
														 * &&
														 * compteur<cumFrequence
														 * .length-1
														 */) {
						compteur++;
					}
					gameteNuclear[i][0] = nuclearAlleleDiversity[i][compteur];
				}
			} else {
				gameteNuclear = null;
			}

			// compute mCytoplasmicDNA of gamete.
			if (mCytoplasmicAlleleFrequency != null && mCytoplasmicAlleleDiversity != null) {
				for (int i = 0; i < gameteMCytoplasmic.length; i++) {
					int[] cumFrequence = new int[mCytoplasmicAlleleFrequency[i].length];
					cumFrequence[0] = mCytoplasmicAlleleFrequency[i][0];
					for (int j = 1; j < mCytoplasmicAlleleFrequency[i].length; j++) {
						cumFrequence[j] = cumFrequence[j - 1] + mCytoplasmicAlleleFrequency[i][j];
					}
					int allele = random.nextInt(cumFrequence[cumFrequence.length - 1]);
					int compteur = 0;
					while (allele > cumFrequence[compteur]/*
														 * &&
														 * compteur<cumFrequence
														 * .length-1
														 */) {
						compteur++;
					}
					gameteMCytoplasmic[i] = mCytoplasmicAlleleDiversity[i][compteur];
				}
			} else {
				gameteMCytoplasmic = null;
			}

			// compute pCytoplasmicDNA of gamete.
			if (pCytoplasmicAlleleFrequency != null && pCytoplasmicAlleleDiversity != null) {
				for (int i = 0; i < gametePCytoplasmic.length; i++) {
					int[] cumFrequence = new int[pCytoplasmicAlleleFrequency[i].length];
					cumFrequence[0] = pCytoplasmicAlleleFrequency[i][0];
					for (int j = 1; j < pCytoplasmicAlleleFrequency[i].length; j++) {
						cumFrequence[j] = cumFrequence[j - 1] + pCytoplasmicAlleleFrequency[i][j];
					}
					int allele = random.nextInt(cumFrequence[cumFrequence.length - 1]);
					int compteur = 0;
					while (allele > cumFrequence[compteur] /*
															 * &&
															 * compteur<cumFrequence
															 * .length-1
															 */) {
						compteur++;
					}
					gametePCytoplasmic[i] = pCytoplasmicAlleleDiversity[i][compteur];
				}
			} else {
				gametePCytoplasmic = null;
			}

			IndividualGenotype g = new IndividualGenotype(gameteNuclear, gameteMCytoplasmic, gametePCytoplasmic);
			return g;
		} else {
			return null;
		}
	}

	// pichot october 2003
	/**
	 * Rebuild MultiGenotype (of initial scene).
	 */
	public static void rebuildMultiGenotype(Genotypable gee) {
		// Genotypable t = (Genotypable) geesWithGenotype.iterator ().next ();
		// Object _t = null; Iterator _i_ = geesWithGenotype.iterator (); do {
		// _t = _i_.next (); } while (!(_t instanceof Genotypable)); Genotypable
		// t = (Genotypable) _t; // phd 2003_03_17
		AlleleDiversity ad = gee.getGenoSpecies().getAlleleDiversity();
		short[][] naf = ad.getNuclearAlleleDiversity();
		short[][] maf = ad.getMCytoplasmicAlleleDiversity();
		short[][] paf = ad.getPCytoplasmicAlleleDiversity();
		Set loci = new TreeSet();

		for (int i = 0; i < naf.length; i++) {
			loci.add("n_" + (i + 1));
		}
		int nLoci = loci.size();
		for (int i = 0; i < maf.length; i++) {
			loci.add("m_" + (i + 1));
		}
		int mLoci = loci.size() - nLoci;
		for (int i = 0; i < paf.length; i++) {
			loci.add("p_" + (i + 1));
		}
		int pLoci = loci.size() - nLoci - mLoci;
		// compute UNSCALED allele frequencies
		Collection singleGeeCollection = new Vector();
		singleGeeCollection.add(gee);
		Map alleleFrequencies = GeneticTools.computeAlleleFrequencies(singleGeeCollection, loci, true);

		// compute MultiGenotype of each gee
		// for (Iterator iteT = geesWithoutGenotype.iterator (); iteT.hasNext
		// ();) {
		// Genotypable gee = (Genotypable) iteT.next();
		// _t = iteT.next(); if(!(_t instanceof Genotypable)) {continue;}
		// Genotypable gee = (Genotypable) _t; // phd 2003_03_17
		int[][] nuclearAlleleFrequencies = new int[nLoci][];
		int[][] mCytoplasmicAlleleFrequencies = new int[mLoci][];
		int[][] pCytoplasmicAlleleFrequencies = new int[pLoci][];

		for (Iterator iteL = loci.iterator(); iteL.hasNext();) {
			String locus = (String) iteL.next();
			double[][] tab = (double[][]) alleleFrequencies.get(locus);

			if ("n".equals(locus.substring(0, 1))) {
				int nn = Integer.parseInt(locus.substring(2, locus.length())) - 1;
				nuclearAlleleFrequencies[nn] = new int[tab[0].length];
				int eff = 0;
				double[] cumFrequency = new double[tab[0].length];
				cumFrequency[0] = tab[1][0];
				for (int i = 1; i < tab[0].length; i++) {
					cumFrequency[i] = cumFrequency[i - 1] + tab[1][i];
				}
				// scaling of frequencies
				for (int i = 0; i < tab[0].length; i++) {
					tab[1][i] = tab[1][i] / cumFrequency[cumFrequency.length - 1];
					cumFrequency[i] = cumFrequency[i] / cumFrequency[cumFrequency.length - 1];
				}
				for (int i = 0; i < tab[0].length; i++) {
					nuclearAlleleFrequencies[nn][i] = (int) (tab[1][i] * 2 * gee.getNumber() + 0.0000001);
					eff = eff + (int) (tab[1][i] * 2 * gee.getNumber() + 0.0000001);
				}
				for (int i = 0; i < (2 * gee.getNumber() - eff); i++) {
					double proba = random.nextDouble();
					int compteur = 0;
					while (proba > cumFrequency[compteur]) {
						compteur++;
					}
					nuclearAlleleFrequencies[nn][compteur] = nuclearAlleleFrequencies[nn][compteur] + 1;
				}

			} else if ("m".equals(locus.substring(0, 1))) {
				int mn = Integer.parseInt(locus.substring(2, locus.length())) - 1;
				mCytoplasmicAlleleFrequencies[mn] = new int[tab[0].length];
				int eff = 0;
				double[] cumFrequency = new double[tab[0].length];
				cumFrequency[0] = tab[1][0];
				for (int i = 1; i < tab[0].length; i++) {
					cumFrequency[i] = cumFrequency[i - 1] + tab[1][i];
				}
				// scaling of frequencies
				for (int i = 0; i < tab[0].length; i++) {
					tab[1][i] = tab[1][i] / cumFrequency[cumFrequency.length - 1];
					cumFrequency[i] = cumFrequency[i] / cumFrequency[cumFrequency.length - 1];
				}
				for (int i = 0; i < tab[0].length; i++) {
					mCytoplasmicAlleleFrequencies[mn][i] = (int) (tab[1][i] * gee.getNumber() + 0.0000001);
					eff = eff + (int) (tab[1][i] * gee.getNumber() + 0.0000001);
				}
				for (int i = 0; i < (gee.getNumber() - eff); i++) {
					double proba = random.nextDouble();
					int compteur = 0;
					while (proba > cumFrequency[compteur]) {
						compteur++;
					}
					mCytoplasmicAlleleFrequencies[mn][compteur] = mCytoplasmicAlleleFrequencies[mn][compteur] + 1;
				}
			} else {
				int pn = Integer.parseInt(locus.substring(2, locus.length())) - 1;
				pCytoplasmicAlleleFrequencies[pn] = new int[tab[0].length];
				int eff = 0;
				double[] cumFrequency = new double[tab[0].length];
				cumFrequency[0] = tab[1][0];
				for (int i = 1; i < tab[0].length; i++) {
					cumFrequency[i] = cumFrequency[i - 1] + tab[1][i];
				}
				// scaling of frequencies
				for (int i = 0; i < tab[0].length; i++) {
					tab[1][i] = tab[1][i] / cumFrequency[cumFrequency.length - 1];
					cumFrequency[i] = cumFrequency[i] / cumFrequency[cumFrequency.length - 1];
				}

				for (int i = 0; i < tab[0].length; i++) {
					pCytoplasmicAlleleFrequencies[pn][i] = (int) (tab[1][i] * gee.getNumber() + 0.0000001);
					eff = eff + (int) (tab[1][i] * gee.getNumber() + 0.0000001);
				}
				for (int i = 0; i < (gee.getNumber() - eff); i++) {
					double proba = random.nextDouble();
					int compteur = 0;
					while (proba > cumFrequency[compteur]) {
						compteur++;
					}
					pCytoplasmicAlleleFrequencies[pn][compteur] = pCytoplasmicAlleleFrequencies[pn][compteur] + 1;
				}
			}
		}
		// create new MultiGenotype and attribute to genotypable
		MultiGenotype multiGenotype = new MultiGenotype(nuclearAlleleFrequencies, mCytoplasmicAlleleFrequencies,
				pCytoplasmicAlleleFrequencies);
		gee.setMultiGenotype(multiGenotype);
	}

	// pichot october2003 END

	/**
	 * Compute default MultiGenotype for mean genotypable, with empty
	 * MultiGenotype, of initial scene.
	 */
	public static void computeDefaultMultiGenotype(Collection geesWithGenotype, Collection geesWithoutGenotype) {

		Object _t = null;
		Iterator _i = geesWithGenotype.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		Genotypable t = (Genotypable) _t; // phd 2003_03_17

		AlleleDiversity ad = t.getGenoSpecies().getAlleleDiversity();
		short[][] naf = ad.getNuclearAlleleDiversity();
		short[][] maf = ad.getMCytoplasmicAlleleDiversity();
		short[][] paf = ad.getPCytoplasmicAlleleDiversity();
		Set loci = new TreeSet();

		for (int i = 0; i < naf.length; i++) {
			loci.add("n_" + (i + 1));
		}
		int nLoci = loci.size();
		for (int i = 0; i < maf.length; i++) {
			loci.add("m_" + (i + 1));
		}
		int mLoci = loci.size() - nLoci;
		for (int i = 0; i < paf.length; i++) {
			loci.add("p_" + (i + 1));
		}
		int pLoci = loci.size() - nLoci - mLoci;
		// compute allele frequencies in geesWithGenotype population
		Map alleleFrequencies = GeneticTools.computeAlleleFrequencies(geesWithGenotype, loci, true);
		// compute MultiGenotype of each genotypable
		for (Iterator iteT = geesWithoutGenotype.iterator(); iteT.hasNext();) {

			_t = iteT.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			Genotypable gee = (Genotypable) _t; // phd 2003_03_17

			int[][] nuclearAlleleFrequencies = new int[nLoci][];
			int[][] mCytoplasmicAlleleFrequencies = new int[mLoci][];
			int[][] pCytoplasmicAlleleFrequencies = new int[pLoci][];
			for (Iterator iteL = loci.iterator(); iteL.hasNext();) {
				String locus = (String) iteL.next();
				double[][] tab = (double[][]) alleleFrequencies.get(locus);
				if ("n".equals(locus.substring(0, 1))) {
					int nn = Integer.parseInt(locus.substring(2, locus.length())) - 1;
					nuclearAlleleFrequencies[nn] = new int[tab[0].length];
					int eff = 0;
					double[] cumFrequency = new double[tab[0].length];
					cumFrequency[0] = tab[1][0];
					for (int i = 1; i < tab[0].length; i++) {
						cumFrequency[i] = cumFrequency[i - 1] + tab[1][i];
					}
					if (gee.getNumber() > 10000) {
						for (int i = 0; i < tab[0].length; i++) {
							nuclearAlleleFrequencies[nn][i] = (int) (tab[1][i] * 2 * gee.getNumber());
							eff = eff + (int) (tab[1][i] * 2 * gee.getNumber());
						}
					}
					for (int i = 0; i < (2 * gee.getNumber() - eff); i++) {
						double proba = random.nextDouble();
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						nuclearAlleleFrequencies[nn][compteur] = nuclearAlleleFrequencies[nn][compteur] + 1;
					}
				} else if ("m".equals(locus.substring(0, 1))) {
					int mn = Integer.parseInt(locus.substring(2, locus.length())) - 1;
					mCytoplasmicAlleleFrequencies[mn] = new int[tab[0].length];
					int eff = 0;
					double[] cumFrequency = new double[tab[0].length];
					cumFrequency[0] = tab[1][0];
					for (int i = 1; i < tab[0].length; i++) {
						cumFrequency[i] = cumFrequency[i - 1] + tab[1][i];
					}
					if (gee.getNumber() > 10000) {
						for (int i = 0; i < tab[0].length; i++) {
							mCytoplasmicAlleleFrequencies[mn][i] = (int) (tab[1][i] * gee.getNumber());
							eff = eff + (int) (tab[1][i] * gee.getNumber());
						}
					}
					for (int i = 0; i < (gee.getNumber() - eff); i++) {
						double proba = random.nextDouble();
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						mCytoplasmicAlleleFrequencies[mn][compteur] = mCytoplasmicAlleleFrequencies[mn][compteur] + 1;
					}
				} else {
					int pn = Integer.parseInt(locus.substring(2, locus.length())) - 1;
					pCytoplasmicAlleleFrequencies[pn] = new int[tab[0].length];
					int eff = 0;
					double[] cumFrequency = new double[tab[0].length];
					cumFrequency[0] = tab[1][0];
					for (int i = 1; i < tab[0].length; i++) {
						cumFrequency[i] = cumFrequency[i - 1] + tab[1][i];
					}
					if (gee.getNumber() > 10000) {
						for (int i = 0; i < tab[0].length; i++) {
							pCytoplasmicAlleleFrequencies[pn][i] = (int) (tab[1][i] * gee.getNumber());
							eff = eff + (int) (tab[1][i] * gee.getNumber());
						}
					}
					for (int i = 0; i < (gee.getNumber() - eff); i++) {
						double proba = random.nextDouble();
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						pCytoplasmicAlleleFrequencies[pn][compteur] = pCytoplasmicAlleleFrequencies[pn][compteur] + 1;
					}
				}
			}
			// create new MultiGenotype and attribute to genotypable
			MultiGenotype multiGenotype = new MultiGenotype(nuclearAlleleFrequencies, mCytoplasmicAlleleFrequencies,
					pCytoplasmicAlleleFrequencies);
			gee.setMultiGenotype(multiGenotype);
		}
	}

}
