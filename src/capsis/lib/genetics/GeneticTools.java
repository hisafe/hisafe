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
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.Vector;

import capsis.kernel.Step;
import capsis.lib.genetics.memory.GeneticMemory;
import jeeb.lib.util.Log;

/**
 * This class contains methods that compute characteristic of a collection of
 * genotypables (for example allele frequency or heterozygote deficit). Methods
 * from GeneticTree were moved here for Genotypable considerations in november
 * 2004.
 * 
 * @author I. Seynave - July 2002, C. Pichot - March, September 2003, F. de
 *         Coligny - November 2004
 */
public class GeneticTools {

	private static boolean logEnabled = true;

	public static void setLogEnabled(boolean v) {
		logEnabled = v;
	}

	public static boolean isLogEnabled() {
		return logEnabled;
	}

	/**
	 * Last known step is now part of GeneticTools (was previously inside
	 * GeneticTree) and is now public. Use GeneticTools.lasKnownStep when
	 * working on a scene which is not yet tied in a project (see searchScene ()
	 * and searchGee ()).
	 */
	public static Step lastKnownStep; // fc - 4.11.2004

	public static Random random = new Random();

	/**
	 * Compute allele frequencies for loci in nuclear DNA in a collection of
	 * genotypables. The computation can include or not unknown allele. In the
	 * result array, for each locus, the allele frequencies are in the same
	 * order than the potential allele in
	 * AlleleDiversity.nuclearAlleleDiversity.
	 */
	public static double[][] computeNuclearAlleleFrequencies(Collection gees, int[] loci, boolean with) {

		// test if all gees belong to the same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ //Genotypable first = (Genotypable) gees.iterator ().next (); //
		// re_next !? bizarre ... phd 2003_03_17
		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable t = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) t;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ Genotype genotype = t.getGenotype ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not be done because all genotypables of Collection do not have same species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeNuclearAlleleFrequencies ()",
						"aborted: all genotypables of Collection do not have same species");
			return null;
		}

		// ~ if (test == 0) {
		// ~ int nbGees = gees.size ();

		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleDiversity ad = ((Genotypable) _t).getGenoSpecies().getAlleleDiversity(); // phd
																						// 2003_03_17

		short[][] nad = ad.getNuclearAlleleDiversity();
		double[][] frequence = new double[loci.length][];
		double[][] frequence2 = new double[loci.length][];
		double[] number = new double[loci.length]; // fc - 22.8.2006 -
													// Numberable returns double
		int[] number2 = new int[loci.length];

		// built result array.
		for (int i = 0; i < loci.length; i++) {
			short[] allele = nad[loci[i] - 1];
			frequence[i] = new double[allele.length];
			frequence2[i] = new double[allele.length];
		}

		// for each locus, calculate the occurence number of each potential
		// allele in population of genotypables
		// with IndividualGenotype and sum allele frequence of genotypable with
		// MultiGenotype.
		int nig = 0;
		double nmg = 0; // fc - 22.8.2006 - Numberable returns double
		for (Iterator i = gees.iterator(); i.hasNext();) {

			_t = i.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			Genotypable gee = (Genotypable) _t; // phd 2003_03_17

			int id = gee.getId();
			Genotype genotype = gee.getGenotype();
			if (genotype instanceof IndividualGenotype) {
				short[][] nuclearDNA = ((IndividualGenotype) genotype).getNuclearDNA();
				for (int j = 0; j < loci.length; j++) {
					short[] allele = nad[loci[j] - 1];
					int rang = loci[j];
					for (int k = 0; k < nuclearDNA[0].length; k++) {
						int l = 0;
						while (nuclearDNA[rang - 1][k] != allele[l]) {
							l = l + 1;
						}
						if (nuclearDNA[rang - 1][k] != -1) {
							frequence[j][l] = frequence[j][l] + 1;
							number[j] = number[j] + gee.getNumber();
						} else {
							if (!with) {
								frequence[j][l] = 0;
							} else {
								frequence[j][l] = frequence[j][l] + 1;
							}
						}
					}
				}
				nig = nig + 1;
			}
			if (genotype instanceof MultiGenotype) {
				int[][] nuclearAlleleFrequency = ((MultiGenotype) genotype).getNuclearAlleleFrequency();
				for (int j = 0; j < loci.length; j++) {
					int rang = loci[j];
					for (int k = 0; k < nuclearAlleleFrequency[rang - 1].length; k++) {
						if (nad[rang - 1][k] != -1) {
							frequence2[j][k] = frequence2[j][k] + nuclearAlleleFrequency[rang - 1][k];
							number2[j] = number2[j] + nuclearAlleleFrequency[rang - 1][k];
						} else {
							if (!with) {
								frequence2[j][k] = 0;
							} else {
								frequence2[j][k] = frequence2[j][k] + nuclearAlleleFrequency[rang - 1][k];
							}
						}
					}
				}
				nmg = nmg + gee.getNumber();
			}
		}

		// divide the occurence number of each potential allele by 2*genotypable
		// number.
		for (int i = 0; i < frequence.length; i++) {
			for (int j = 0; j < frequence[i].length; j++) {
				if (!with) {
					frequence[i][j] = (frequence[i][j] + frequence2[i][j]) / (double) (number2[i] + number[i]);
				} else {
					frequence[i][j] = (frequence[i][j] + frequence2[i][j]) / (double) (2 * nig + 2 * nmg); // pichot
																											// october
																											// 2003
				}
			}
		}

		// SOM 12/05/11: standardize allelic frequencies so that sum =1
		for (int i = 0; i < frequence.length; i++) {
			double sumfloci = 0;
			for (int j = 0; j < frequence[i].length; j++) {
				sumfloci += frequence[i][j];
			}

			if (sumfloci > 0) {
				for (int j = 0; j < frequence[i].length; j++) {
					frequence[i][j] = frequence[i][j] / sumfloci;
				}
			}

		}

		return frequence;
		// ~ } else {
		// ~ return null;
		// ~ }
	}

	/**
	 * Compute allele frequencies for loci in cytoplasmic maternal or paternal
	 * DNA in a collection of genotypables. The computing can include or not
	 * unknown allele. In the result array, for each locus, the allele
	 * frequencies are in the same order than the potential allele in
	 * AlleleDiversity.nuclearAlleleDiversity.
	 */
	public static double[][] computeCytoplasmicAlleleFrequencies(Collection gees, int[] loci, String parent,
			boolean with) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable t = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) t;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ Genotype genotype = t.getGenotype ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not be done because all genotypables of Collection do not have same species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeCytoplasmicAlleleFrequencies ()",
						"aborted: all genotypables of Collection do not have same species");
			return null;
		}

		// ~ if (test == 0){
		int nbGees = gees.size();
		short[][] cytoplasmicAlleleDiversity = new short[nbGees][loci.length];

		// AlleleDiversity ad = ((Genotypable) gees.iterator ().next
		// ()).getAlleleDiversity ();
		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleDiversity ad = ((Genotypable) _t).getGenoSpecies().getAlleleDiversity(); // phd
																						// 2003_03_17

		short[][] mad;
		if ("maternal".equals(parent)) {
			mad = ad.getMCytoplasmicAlleleDiversity();
		} else {
			mad = ad.getPCytoplasmicAlleleDiversity();
		}

		double[][] frequence = new double[loci.length][];
		double[][] frequence2 = new double[loci.length][];
		int[] number = new int[loci.length];
		int[] number2 = new int[loci.length];

		// built result array.
		for (int i = 0; i < loci.length; i++) {
			short[] allele = mad[loci[i] - 1];
			frequence[i] = new double[allele.length];
			frequence2[i] = new double[allele.length];
		}

		// for each locus, calculate the occurence number of each potential
		// allele in population of genotypables
		// with IndividualGenotype and sum allele frequence of genotypable with
		// MultiGenotype.
		int nig = 0;
		double nmg = 0; // fc - 22.8.2006 - Numberable returns double
		for (Iterator i = gees.iterator(); i.hasNext();) {

			_t = i.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			Genotypable gee = (Genotypable) _t; // phd 2003_03_17

			int id = gee.getId();
			Genotype genotype = gee.getGenotype();
			short[] cytoplasmicDNA;
			int[][] cytoplasmicAlleleFrequence;
			if (genotype instanceof IndividualGenotype) {
				if ("maternal".equals(parent)) {
					cytoplasmicDNA = ((IndividualGenotype) genotype).getMCytoplasmicDNA();
				} else {
					cytoplasmicDNA = ((IndividualGenotype) genotype).getPCytoplasmicDNA();
				}

				for (int j = 0; j < loci.length; j++) {
					short[] allele = mad[loci[j] - 1];
					int rang = loci[j];
					int l = 0;
					while (cytoplasmicDNA[rang - 1] != allele[l]) {
						l = l + 1;
					}
					if (cytoplasmicDNA[rang - 1] != -1) {
						frequence[j][l] = frequence[j][l] + 1;
						number[j] = number[j] + 1;
					} else {
						if (!with) {
							frequence[j][l] = 0;
						} else {
							frequence[j][l] = frequence[j][l] + 1;
						}
					}
				}
				nig = nig + 1;
			}
			if (genotype instanceof MultiGenotype) {
				if ("maternal".equals(parent)) {
					cytoplasmicAlleleFrequence = ((MultiGenotype) genotype).getMCytoplasmicAlleleFrequency();
				} else {
					cytoplasmicAlleleFrequence = ((MultiGenotype) genotype).getPCytoplasmicAlleleFrequency();
				}
				for (int j = 0; j < loci.length; j++) {
					int rang = loci[j];
					for (int k = 0; k < cytoplasmicAlleleFrequence[rang - 1].length; k++) {
						if (mad[rang - 1][k] != -1) {
							frequence2[j][k] = frequence2[j][k] + cytoplasmicAlleleFrequence[rang - 1][k];
							number2[j] = number2[j] + cytoplasmicAlleleFrequence[rang - 1][k];
						} else {
							if (!with) {
								frequence2[j][k] = 0;
							} else {
								frequence2[j][k] = frequence2[j][k] + cytoplasmicAlleleFrequence[rang - 1][k];
							}
						}
					}
				}
				nmg = nmg + gee.getNumber();
			}
		}

		// divide the occurence number of each potential allele by genotypable
		// number.
		for (int i = 0; i < frequence.length; i++) {
			for (int j = 0; j < frequence[i].length; j++) {
				if (!with) {
					frequence[i][j] = (frequence[i][j] + frequence2[i][j]) / (double) (number[i] + number2[i]);
				} else {
					frequence[i][j] = (frequence[i][j] + frequence2[i][j]) / (double) (nig + nmg);
				}
			}
		}
		return frequence;
		// ~ } else {
		// ~ return null;
		// ~ }
	}

	/**
	 * Compute allele frequencies for loci in a collection of genotypables. The
	 * computation can include or not the unknown allele frequency. The result
	 * is a map that contains : (i) keys contain the number of the loci and its
	 * DNA (n_ for nuclear, m_ for maternal cytoplasmic and p_ for paternal
	 * cytoplasmic), (ii) arrays contain the allele (first line) and frequency
	 * (second line).
	 */
	public static Map computeAlleleFrequencies(Collection gees, Set loci, boolean with) {

		// test if all genotypables have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ Genotype genotype = gee.getGenotype ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not execute because all genotypables of Collection do not have same species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeAlleleFrequencies ()",
						"aborted: all genotypables of Collection do not have same species");
			return null;
		}

		// ~ if (test == 0) {
		
		// fc-3.7.2018 LinkedHashMap will keep the insertion order, easier for charts
		Map allelicFrequences = new LinkedHashMap();
//		Map allelicFrequences = new Hashtable();
		
		String parent;
		int[] nuclearLociT = new int[loci.size()];
		int[] mCytoplasmicLociT = new int[loci.size()];
		int[] pCytoplasmicLociT = new int[loci.size()];

		// calculate 3 arrays whici contain locus serial number from set of
		// loci. In this set, the locus number is preceded by n_, m_ or p_.
		int n = 0;
		int m = 0;
		int p = 0;
		for (Iterator i = loci.iterator(); i.hasNext();) {
			String s = (String) i.next();
			String ss = s.substring(0, 1);
			if ("n".equals(ss)) {
				nuclearLociT[n] = Integer.parseInt(s.substring(2, s.length()));
				n = n + 1;
			} else if ("m".equals(ss)) {
				mCytoplasmicLociT[m] = Integer.parseInt(s.substring(2, s.length()));
				m = m + 1;
			} else if ("p".equals(ss)) {
				pCytoplasmicLociT[p] = Integer.parseInt(s.substring(2, s.length()));
				p = p + 1;
			}
		}

		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleDiversity ad = ((Genotypable) _t).getGenoSpecies().getAlleleDiversity(); // phd
																						// 2003_03_17

//		System.out.println("GeneticTools nuclearLociT: "+nuclearLociT);
		
		// calculate allele frequencies
		// for nuclear DNA loci
		if (n > 0) {
			int[] nuclearLoci = new int[n];
			System.arraycopy(nuclearLociT, 0, nuclearLoci, 0, n);
			
//			System.out.println("GeneticTools nuclearLoci:  "+nuclearLoci);
			
			double[][] nuclearAlleleFrequences = computeNuclearAlleleFrequencies(gees, nuclearLoci, with);
			short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity();
			for (int i = 0; i < nuclearLoci.length; i++) {
				double[][] tab = new double[2][];
				tab[0] = new double[nuclearAlleleDiversity[nuclearLoci[i] - 1].length];
				tab[1] = new double[nuclearAlleleDiversity[nuclearLoci[i] - 1].length];
				for (int j = 0; j < nuclearAlleleDiversity[nuclearLoci[i] - 1].length; j++) {
					tab[0][j] = nuclearAlleleDiversity[nuclearLoci[i] - 1][j];
				}
				tab[1] = nuclearAlleleFrequences[i];
				allelicFrequences.put(("n_" + nuclearLoci[i]), tab);
			}
		}
		// for maternal cytoplasmic loci
		if (m > 0) {
			parent = "maternal";
			int[] mCytoplasmicLoci = new int[m];
			System.arraycopy(mCytoplasmicLociT, 0, mCytoplasmicLoci, 0, m);
			double[][] mCytoplasmicAlleleFrequences = computeCytoplasmicAlleleFrequencies(gees, mCytoplasmicLoci,
					parent, with);
			short[][] mCytoplasmicAlleleDiversity = ad.getMCytoplasmicAlleleDiversity();
			for (int i = 0; i < mCytoplasmicLoci.length; i++) {
				double[][] tab = new double[2][];
				tab[0] = new double[mCytoplasmicAlleleDiversity[mCytoplasmicLoci[i] - 1].length];
				tab[1] = new double[mCytoplasmicAlleleDiversity[mCytoplasmicLoci[i] - 1].length];
				for (int j = 0; j < mCytoplasmicAlleleDiversity[mCytoplasmicLoci[i] - 1].length; j++) {
					tab[0][j] = mCytoplasmicAlleleDiversity[mCytoplasmicLoci[i] - 1][j];
				}
				tab[1] = mCytoplasmicAlleleFrequences[i];
				allelicFrequences.put(("m_" + mCytoplasmicLoci[i]), tab);
			}
		}
		// for paternal cytoplasmic loci
		if (p > 0) {
			parent = "paternal";
			int[] pCytoplasmicLoci = new int[p];
			System.arraycopy(pCytoplasmicLociT, 0, pCytoplasmicLoci, 0, p);
			double[][] pCytoplasmicAlleleFrequences = computeCytoplasmicAlleleFrequencies(gees, pCytoplasmicLoci,
					parent, with);
			short[][] pCytoplasmicAlleleDiversity = ad.getPCytoplasmicAlleleDiversity();
			for (int i = 0; i < pCytoplasmicLoci.length; i++) {
				double[][] tab = new double[2][];
				tab[0] = new double[pCytoplasmicAlleleDiversity[pCytoplasmicLoci[i] - 1].length];
				tab[1] = new double[pCytoplasmicAlleleDiversity[pCytoplasmicLoci[i] - 1].length];
				for (int j = 0; j < pCytoplasmicAlleleDiversity[pCytoplasmicLoci[i] - 1].length; j++) {
					tab[0][j] = pCytoplasmicAlleleDiversity[pCytoplasmicLoci[i] - 1][j];
				}
				tab[1] = pCytoplasmicAlleleFrequences[i];
				allelicFrequences.put(("p_" + pCytoplasmicLoci[i]), tab);
			}
		}
		return allelicFrequences;
		// ~ } else {
		// ~ return null;
		// ~ }
	}

	/**
	 * Compute genotype frequencies for loci in a collection of genotypables.
	 * The computation doesn't include genotype with unknown allele. The result
	 * is a map that contains : (i) keys contain the number of the loci and its
	 * DNA (n_ for nuclear, m_ for maternal cytoplasmic and p_ for paternal
	 * cytoplasmic), (ii) arrays contain the first allele (first line), the
	 * second allele (second line) and frequency (third line).
	 */
	public static Map computeGenotypeFrequencies(Collection gees, Set loci) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ Genotype genotype = gee.getGenotype ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not be done because genotypables belong to different species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeGenotypeFrequencies ()",
						"aborted: all genotypables of Collection do not have same species");
			return null;
		}

		// ~ if (test == 0) {
		Map genoticFrequences = new Hashtable();
		Set multiGenotype = new TreeSet();

		// built, from Set of loci, a array of int who contains loci serial
		// number.
		int[] lociT = new int[loci.size()];
		int n = 0;
		for (Iterator i = loci.iterator(); i.hasNext();) {
			String s = (String) i.next();
			String ss = s.substring(0, 1);
			lociT[n] = Integer.parseInt(s.substring(2, s.length()));
			n = n + 1;
		}

		// AlleleDiversity ad = ((Genotypable) gees.iterator ().next
		// ()).getAlleleDiversity ();
		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleDiversity ad = ((Genotypable) _t).getGenoSpecies().getAlleleDiversity(); // phd
																						// 2003_03_17

		short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity();
		int nb = gees.size();

		// for each locus, calculate genotype frequencies;
		for (int i = 0; i < lociT.length; i++) {
			int nba = nuclearAlleleDiversity[lociT[i] - 1].length;
			int combinaison = nba * (nba + 1) / 2;

			// for each locus, built array of genotype frequencies and complete
			// the first and second line of array.
			double[][] tab = new double[3][combinaison];
			for (int j = 0; j < nba; j++) {
				for (int k = j; k < nba; k++) {
					tab[0][(nba * j) - (j * (j + 1) / 2) + k] = nuclearAlleleDiversity[lociT[i] - 1][j];
					tab[1][(nba * j) - (j * (j + 1) / 2) + k] = nuclearAlleleDiversity[lociT[i] - 1][k];
				}
			}

			// test the type of genotype.
			// and calculate the occurence number of each potential genotype.
			int number = 0;
			for (Iterator ite = gees.iterator(); ite.hasNext();) {

				_t = ite.next();
				if (!(_t instanceof Genotypable)) {
					continue;
				}
				Genotypable t = (Genotypable) _t; // phd 2003_03_17

				Genotype genotype = t.getGenotype();
				if (genotype instanceof IndividualGenotype) {
					short[][] nuclearDNA = ((IndividualGenotype) genotype).getNuclearDNA();
					int a1 = nuclearDNA[lociT[i] - 1][0];
					int a2 = nuclearDNA[lociT[i] - 1][1];
					int k = 0;
					int l = 0;
					while (a1 != tab[1][k]) {
						k = k + 1;
					}
					while (a2 != tab[1][l]) {
						l = l + 1;
					}
					int r1 = Math.min(k, l);
					int r2 = Math.max(k, l);
					if (a1 != -1 && a2 != -1) {
						tab[2][(r1 * nba) - (r1 * (r1 + 1) / 2) + r2] = tab[2][(r1 * nba) - (r1 * (r1 + 1) / 2) + r2] + 1;
						number = number + 1;
					} else {
						tab[2][(r1 * nba) - (r1 * (r1 + 1) / 2) + r2] = 0;
					}
				}
				if (genotype instanceof MultiGenotype) {
					multiGenotype.add("" + t.getId());
				}
			}

			for (int j = 0; j < tab[2].length; j++) {
				tab[2][j] = (double) tab[2][j] / (double) number;
			}

			genoticFrequences.put(("n_" + lociT[i]), tab);

			if (multiGenotype != null && !multiGenotype.isEmpty()) { // fc-28.3.2012
				if (logEnabled)
					Log.println("The Collection given in parameter to computeGenotypeFrequences () method contains genotypables with MultiGenotype : "
							+ multiGenotype + ". These genotypables were ignored");
			}
		}
		return genoticFrequences;
		// ~ } else {
		// ~ return null;
		// ~ }
	}

	/**
	 * Compute expected heterozygote frequencies for nuclearDNA loci in a
	 * population of genotypables.
	 */
	public static double[] computePanmicticHeterozygoteFrequencies(Collection gees, Set loci) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ Genotype genotype = gee.getGenotype ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not execute because genotypables belong to several species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computePanmicticHeterozygoteFrequencies ()",
						"aborted: all genotypables of Collection do not have same species");
			return null;
		}

		// ~ if (test == 0) {
		double[] freq = new double[loci.size()];

		int[] lociT = new int[loci.size()];
		int n = 0;
		for (Iterator i = loci.iterator(); i.hasNext();) {
			String s = (String) i.next();
			String ss = s.substring(0, 1);
			lociT[n] = Integer.parseInt(s.substring(2, s.length()));
			n = n + 1;
		}

		double[][] frequence = computeNuclearAlleleFrequencies(gees, lociT, false);

		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleDiversity ad = ((Genotypable) _t).getGenoSpecies().getAlleleDiversity(); // phd
																						// 2003_03_17

		short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity();

		int nb = gees.size();
		for (int i = 0; i < lociT.length; i++) {
			int nba = nuclearAlleleDiversity[lociT[i] - 1].length;
			double f = 0;
			double[] tab = new double[nba];
			for (int j = 0; j < nba; j++) {
				f = f + frequence[i][j] * frequence[i][j];
			}
			freq[i] = 1 - f;
		}
		return freq;
		// ~ } else {
		// ~ return null;
		// ~ }
	}

	/**
	 * Compute F : heterozygote deficiency.
	 */
	public static double[] computeF(Collection gees, Set loci) {

		// test if all gees belong to the same species
		// and have IndividualGenotype
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ Genotype genotype = gee.getGenotype ();
		// ~ if (genotype instanceof MultiGenotype) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeF () could not execute because Collection contains genotypables with MultiGenotype");
		// ~ }
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeF () could not execute because genotypables belong to different species");
		// ~ }
		// ~ }
		// ~ } else {
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1) {

		// ~ Object _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ Genotype genotype = gee.getGenotype ();
		// ~ if (genotype instanceof MultiGenotype) {
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeF () could not be done because Collection contains genotypables with MultiGenotype");
		// ~ test = 1;
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeF ()",
						"aborted: all genotypables of Collection do not have same species");
			return null;
		}

		if (!GeneticTools.haveAllAnIndividualGenotype(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeF ()",
						"aborted: all genotypables of Collection do not have an individual genotype");
			return null;
		}

		// ~ if (test == 0) {
		Map genoticFrequence = computeGenotypeFrequencies(gees, loci);
		double[] hete = computePanmicticHeterozygoteFrequencies(gees, loci);
		double[] f = new double[loci.size()];

		int l = 0;
		// for each locus :
		for (Iterator ite = loci.iterator(); ite.hasNext();) {
			String s = (String) ite.next();
			double[][] tab = (double[][]) genoticFrequence.get(s);
			int v = (int) tab[0][0];
			int nbgenotypes = tab[0].length;
			int nballeles = (int) ((-1 + Math.sqrt(1 + 8 * nbgenotypes)) / 2);
			double freq = 0;
			// compute homozygote frequency in population of gees
			for (int i = 0; i < nballeles; i++) {
				freq = freq + tab[2][(i * nballeles) - (i * (i + 1) / 2) + i];
			}
			// compute F
			f[l] = (double) 1 - ((1 - freq) / (hete[l]));
			l = l + 1;
		}
		return f;
		// ~ } else {
		// ~ return null;
		// ~ }
	}

	/**
	 * Compute panmictic genetic mean in a collection of genotypables and for a
	 * parameter.
	 */
	public static double computePanmicticGeneticMean(Collection gees, String parameterName) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not execute because genotypables belong to different species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computePanmicticGeneticMean ()",
						"aborted: all genotypables of Collection do not have same species");
			return -1;
		}

		// ~ if (test == 0) {
		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleEffect ae = ((Genotypable) _t).getGenoSpecies().getAlleleEffect(); // phd
																					// 2003_03_17

		AlleleEffect.ParameterEffect pe = ae.getParameterEffect(parameterName);
		String parent;
		double geneticMean = 0;

		// C. Pichot 03/03
		// compute genetic mean from nuclear allele.
		short[][] effectValue = pe.getNuclearAlleleEffect();
		int[] nuclearLoci = new int[effectValue.length];
		if (nuclearLoci.length > 0) {
			for (int i = 0; i < effectValue.length; i++) {
				nuclearLoci[i] = effectValue[i][0];
			}
			double[][] allelicNuclearFrequence = GeneticTools.computeNuclearAlleleFrequencies(gees, nuclearLoci, false);

			for (int i = 0; i < allelicNuclearFrequence.length; i++) {
				for (int j = 0; j < allelicNuclearFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticMean = geneticMean + 2 * allelicNuclearFrequence[i][j] * effectValue[i][j + 1] * pe.getEffectCoefficient();
				}
			}
		}

		// compute genetic mean from maternal cytoplasmic allele and sum to
		// nuclear.
		short[][] mEffectValue = pe.getMCytoplasmicAlleleEffect();
		int[] mCytoplasmicLoci = new int[mEffectValue.length];
		if (mCytoplasmicLoci.length > 0) {
			for (int i = 0; i < mEffectValue.length; i++) {
				mCytoplasmicLoci[i] = mEffectValue[i][0];
			}
			parent = "maternal";
			double[][] allelicMCytoplasmicFrequence = GeneticTools.computeCytoplasmicAlleleFrequencies(gees,
					mCytoplasmicLoci, parent, false);
			for (int i = 0; i < allelicMCytoplasmicFrequence.length; i++) {
				for (int j = 0; j < allelicMCytoplasmicFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticMean = geneticMean + allelicMCytoplasmicFrequence[i][j] * mEffectValue[i][j + 1] * pe.getEffectCoefficient();
				}
			}
		}

		// compute genetic mean from paternal cytoplasmic allele and sum to
		// nuclear and maternal cytoplasmic.
		short[][] pEffectValue = pe.getPCytoplasmicAlleleEffect();
		int[] pCytoplasmicLoci = new int[pEffectValue.length];
		if (pCytoplasmicLoci.length > 0) {
			for (int i = 0; i < pEffectValue.length; i++) {
				pCytoplasmicLoci[i] = pEffectValue[i][0];
			}
			parent = "paternal";
			double[][] allelicPCytoplasmicFrequence = GeneticTools.computeCytoplasmicAlleleFrequencies(gees,
					pCytoplasmicLoci, parent, false);
			for (int i = 0; i < allelicPCytoplasmicFrequence.length; i++) {
				for (int j = 0; j < allelicPCytoplasmicFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticMean = geneticMean + allelicPCytoplasmicFrequence[i][j] * pEffectValue[i][j + 1] * pe.getEffectCoefficient();
				}
			}
		}
		// C. Pichot 03/03
		// System.out.println ("genetic Mean = " + geneticMean);
		return geneticMean;
		// ~ } else {
		// ~ return -1;
		// ~ }
	}

	/**
	 * Compute panmictic genetic variance in a collection of genotypables and
	 * for a parameter.
	 */
	public static double computePanmicticGeneticVariance(Collection gees, String parameterName) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () couls not be done because all genotypables of Collection have not same species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computePanmicticGeneticMean ()",
						"aborted: all genotypables of Collection do not have same species");
			return -1;
		}

		// ~ if (test == 0) {
		// double geneticMean = computePanmicticGeneticMean (gees,
		// parameterName);
		double meanAlleleEffectForALocus;

		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		AlleleEffect ae = ((Genotypable) _t).getGenoSpecies().getAlleleEffect(); // phd
																					// 2003_03_17

		AlleleEffect.ParameterEffect pe = ae.getParameterEffect(parameterName);
		String parent;
		double geneticVariance = 0;

		// C. Pichot 03/03
		// compute genetic variance from nuclear allele.
		short[][] effectValue = pe.getNuclearAlleleEffect();
		int[] nuclearLoci = new int[effectValue.length];
		if (nuclearLoci.length > 0) {
			for (int i = 0; i < effectValue.length; i++) {
				nuclearLoci[i] = effectValue[i][0];
			}
			double[][] allelicNuclearFrequence = GeneticTools.computeNuclearAlleleFrequencies(gees, nuclearLoci, false);
			for (int i = 0; i < allelicNuclearFrequence.length; i++) {
				meanAlleleEffectForALocus = 0;
				for (int j = 0; j < allelicNuclearFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					meanAlleleEffectForALocus = meanAlleleEffectForALocus + allelicNuclearFrequence[i][j]
							* effectValue[i][j + 1] * pe.getEffectCoefficient();
				}
				for (int j = 0; j < allelicNuclearFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticVariance = geneticVariance + 2 * allelicNuclearFrequence[i][j]
							* Math.pow((effectValue[i][j + 1] * pe.getEffectCoefficient() - meanAlleleEffectForALocus), 2);
				}
			}
		}

		// compute genetic variance from maternal cytoplasmic allele and sum to
		// nuclear.
		short[][] mEffectValue = pe.getMCytoplasmicAlleleEffect();
		int[] mCytoplasmicLoci = new int[mEffectValue.length];
		if (mCytoplasmicLoci.length > 0) {
			for (int i = 0; i < mEffectValue.length; i++) {
				mCytoplasmicLoci[i] = mEffectValue[i][0];
			}
			parent = "maternal";
			double[][] allelicMCytoplasmicFrequence = GeneticTools.computeCytoplasmicAlleleFrequencies(gees,
					mCytoplasmicLoci, parent, false);
			for (int i = 0; i < allelicMCytoplasmicFrequence.length; i++) {
				meanAlleleEffectForALocus = 0;
				for (int j = 0; j < allelicMCytoplasmicFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					meanAlleleEffectForALocus = meanAlleleEffectForALocus + allelicMCytoplasmicFrequence[i][j]
							* mEffectValue[i][j + 1] * pe.getEffectCoefficient();
				}
				for (int j = 0; j < allelicMCytoplasmicFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.					
					geneticVariance = geneticVariance + allelicMCytoplasmicFrequence[i][j]
							* Math.pow((mEffectValue[i][j + 1] * pe.getEffectCoefficient() - meanAlleleEffectForALocus), 2);
				}
			}
		}

		// compute genetic variance from paternal cytoplasmic allele and sum to
		// nuclear and maternal cytoplasmic.
		short[][] pEffectValue = pe.getPCytoplasmicAlleleEffect();
		int[] pCytoplasmicLoci = new int[pEffectValue.length];
		if (pCytoplasmicLoci.length > 0) {
			for (int i = 0; i < pEffectValue.length; i++) {
				pCytoplasmicLoci[i] = pEffectValue[i][0];
			}
			parent = "paternal";
			double[][] allelicPCytoplasmicFrequence = GeneticTools.computeCytoplasmicAlleleFrequencies(gees,
					pCytoplasmicLoci, parent, false);
			for (int i = 0; i < allelicPCytoplasmicFrequence.length; i++) {
				meanAlleleEffectForALocus = 0;
				for (int j = 0; j < allelicPCytoplasmicFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					meanAlleleEffectForALocus = meanAlleleEffectForALocus + allelicPCytoplasmicFrequence[i][j]
							* pEffectValue[i][j + 1] * pe.getEffectCoefficient();
				}
				for (int j = 0; j < allelicPCytoplasmicFrequence[i].length; j++) {
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticVariance = geneticVariance + allelicPCytoplasmicFrequence[i][j]
							* Math.pow((pEffectValue[i][j + 1] * pe.getEffectCoefficient() - meanAlleleEffectForALocus), 2);
				}
			}
		}

		// System.out.println ("genetic Variance = " + geneticVariance);
		// C. Pichot 03/03
		return geneticVariance;
		// ~ } else {
		// ~ return -1;
		// ~ }
	}

	/**
	 * Compute genetic mean in a collection of genotypables and for a parameter.
	 */
	public static double computeObservedGeneticMean(Collection gees, String parameterName) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ if (gee.getGenotype () instanceof MultiGenotype) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not be done because all genotypables are not individual");
		// ~ }
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not be done because all genotypables of Collection have not same species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computePanmicticGeneticMean ()",
						"aborted: all genotypables of Collection do not have same species");
			return -1;
		}

		// C. Pichot 03/03
		// ~ if (test == 0) {
		double geneticMean = 0;
		int nbGees = 0;
		for (Iterator i = gees.iterator(); i.hasNext();) {
			Object _t = i.next();
			if (_t instanceof Genotypable) {
				Genotypable gee = (Genotypable) _t;
				geneticMean = geneticMean + gee.getGeneticValue(parameterName);
				nbGees = nbGees + 1;
			}
		}
		geneticMean = geneticMean / nbGees;
		// C. Pichot 03/03
//		System.out.println("ObservedGeneticMean = " + geneticMean);
		return geneticMean;
		// ~ } else {
		// ~ return -1;
		// ~ }
	}

	/**
	 * Compute genetic variance in a collection of genotypables and for a
	 * parameter.
	 */
	public static double computeObservedGeneticVariance(Collection gees, String parameterName) {

		// test if all gees have same species
		// ~ int test = 0;
		// ~ if ((gees.iterator ().next ()) instanceof SpeciesDefined ) {

		// ~ Object _t = null;
		// ~ Iterator _i = gees.iterator ();
		// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
		// ~ Genotypable first = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined sd = (SpeciesDefined) first;
		// ~ QualitativeProperty species = sd.getSpecies ();
		// ~ Iterator i = gees.iterator ();
		// ~ while (test != 1 && i.hasNext ()) {

		// ~ _t = i.next();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		// ~ SpeciesDefined s = (SpeciesDefined) gee;
		// ~ QualitativeProperty esp = s.getSpecies ();
		// ~ if (gee.getGenotype () instanceof MultiGenotype) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not execute because some genotypables are not individual");
		// ~ }
		// ~ if (esp != species) {
		// ~ test = 1;
		// ~ if (logEnabled)
		// Log.println("GeneticTools.computeNuclearAlleleFrequencies () could not execute because genotypables belong to different species");
		// ~ }
		// ~ }
		// ~ }

		if (!GeneticTools.haveAllSameSpecies(gees)) {
			if (logEnabled)
				Log.println(Log.WARNING, "GeneticTools.computeObservedGeneticVariance ()",
						"aborted: all genotypables of Collection do not have same species");
			return -1;
		}

		// C. Pichot 03/03
		// ~ if (test == 0) {
		double geneticMean = computeObservedGeneticMean(gees, parameterName);
		double geneticVariance = 0;
		int nbGees = 0;
		for (Iterator i = gees.iterator(); i.hasNext();) {

			Object _t = i.next();

			if (_t instanceof Genotypable) {
				Genotypable gee = (Genotypable) _t;
				geneticVariance = geneticVariance + Math.pow((gee.getGeneticValue(parameterName) - geneticMean), 2);
				nbGees = nbGees + 1;
			}
		}
		geneticVariance = geneticVariance / nbGees;
		// C. Pichot 03/03
		return geneticVariance;
		// ~ } else {
		// ~ return -1;
		// ~ }
	}

	/**
	 * Draw ntodraw (int) values among ntot (int) and without replacement
	 * according to frequencies(double[]) (real freq. (sum=1) or numbers
	 * (sum=ntot)) return randomly ordered (by a second drawing)
	 * drawnValues(int[])
	 */
	public static int[] drawWithoutReplacement(int ntot, int ntodraw, double frequencies[]) {
		int[] drawnValuesFirstDrawing = new int[ntodraw];
		int[] drawnValues = new int[ntodraw];
		double cumulatedFrequencies[] = new double[frequencies.length];
		int[] cumulatedNumber = new int[frequencies.length];

		double proba = 0;
		int order = 0;
		int ok = 0;

		// cumulated frequencies
		cumulatedFrequencies[0] = frequencies[0];
		for (order = 1; order < frequencies.length; order++) {
			cumulatedFrequencies[order] = cumulatedFrequencies[order - 1] + frequencies[order];
		}
		// cumulated "frequencies" taken as numbers
		cumulatedNumber[0] = (int) frequencies[0];
		for (order = 1; order < frequencies.length; order++) {
			cumulatedNumber[order] = cumulatedNumber[order - 1] + (int) Math.round(frequencies[order]);
		}

		// check for number instead of frequencies (sum = ntot ?)
		if (cumulatedNumber[cumulatedNumber.length - 1] == ntot) {
			// System.out.println("NUMBER !");
			ok = 1;

			// check for real frequencies
		} else if (Math.abs(1d - cumulatedFrequencies[cumulatedFrequencies.length - 1]) < 1E-8) {
			ok = 1;
			cumulatedFrequencies[cumulatedFrequencies.length - 1] = 1d;
			// build number from frequencies
			cumulatedNumber[0] = (int) (ntot * frequencies[0]);
			for (order = 1; order < frequencies.length; order++) {
				cumulatedNumber[order] = cumulatedNumber[order - 1] + (int) (ntot * frequencies[order]);
			}
			// complete numberOfEvents by drawing missing events (up to ntot)
			for (int i = cumulatedNumber[cumulatedNumber.length - 1] + 1; i < ntot + 1; i++) {
				proba = random.nextDouble();
				order = 0;
				while (proba > cumulatedFrequencies[order]) {
					order++;
				}
				for (int j = order; j < cumulatedNumber.length; j++) {
					cumulatedNumber[j] = cumulatedNumber[j] + 1;
				}
			}
		}

		if (ok == 1 & ntot >= ntodraw) {
			// drawing without replacement
			int max = ntot;
			double value = 0;
			for (int i = 0; i < ntodraw; i++) {
				value = random.nextDouble() * max;
				order = 0;
				while (value > (double) cumulatedNumber[order]) {
					order++;
				}
				drawnValuesFirstDrawing[i] = order + 1;
				for (int j = order; j < cumulatedNumber.length; j++) {
					cumulatedNumber[j] = cumulatedNumber[j] - 1;
				}
				max = max - 1;
			}
			// randomize the drawnValues in order to avoid
			// that the first values always come from the highest frequencies
			max = ntodraw;
			int rank = 0;
			for (int i = 0; i < ntodraw; i++) {
				rank = random.nextInt(max);
				drawnValues[i] = drawnValuesFirstDrawing[rank];
				drawnValuesFirstDrawing[rank] = drawnValuesFirstDrawing[max - 1];
				max = max - 1;
			}
		} else {
//			System.out.println("pb : frequencies (or number) not valid");
			drawnValues[0] = -1;
		}
		return drawnValues;
	}

	// Methods moved from Genotypable
	// ------------------------------------------------------ fc - november 2004
	// Introduction of Genotypable interface
	//

	/**
	 * Compute MultiGenotype of a new mean genotypable from the list of its
	 * parents. This method calculates mId, pId, consanguinity and global
	 * consanguinity too.
	 */
	static public Object[] computeNewMultiGenotype(GeneticScene s, int[][] parents) {
		int nbCouples = parents.length;

		// int populationSize = 0;

		Set cohort = new HashSet();
		double consanguinity = 0;
		double globalConsanguinity = 0;
		AlleleDiversity ad = null;
		Object[] result = new Object[5];
		int m = 0;
		boolean related = true;
		Map parent = new Hashtable();

		// // if (parents [0].length>2) { cp 20 03 03
		// if (parents [0].length<3) {
		// populationSize = nbCouples;
		// } else {
		// for (int i = 0; i<parents.length; i++) {
		// populationSize = populationSize + parents [i][2];
		// }
		// }

		// from a mId and a pId, compute an IndividualGenotype
		for (int i = 0; i < nbCouples; i++) {
			int mId = parents[i][0];
			int pId = parents[i][1];

			// fc+som-23.5.2017 GeneticMemory
			Genotypable mGee = s.getGeneticMemory().searchGee(s, mId);

			if (i == 0) {
				ad = mGee.getGenoSpecies().getAlleleDiversity();
				if (mGee.getGenoSpecies().getKinship() == null) {
					related = false;
				}
			}
			Genotypable pGee = s.getGeneticMemory().searchGee(s, pId);
			Genotype mGenotype = mGee.getGenotype();
			Genotype pGenotype = pGee.getGenotype();
			int n;
			if (parents[i].length > 2) {
				n = parents[i][2];
			} else {
				n = 1;
			}
			m = m + n; // CP 02/03 cumulated number of generated gees

			// if parents have Multigenotype and number of descendants is above
			// 10000,
			// compute a MultiGenotype of descendants population using allele
			// frequencies
			// cp february 2004
			// if (mGenotype instanceof MultiGenotype && pGenotype instanceof
			// MultiGenotype && n>10000) {

			if (n > 10000) {
				int[][] mNuclearAlleleFrequency;
				int[][] mMCytoplasmicAlleleFrequency;
				int[][] mPCytoplasmicAlleleFrequency;
				int[][] pNuclearAlleleFrequency;
				int[][] pMCytoplasmicAlleleFrequency;
				int[][] pPCytoplasmicAlleleFrequency;

				if (mGenotype instanceof IndividualGenotype) {
					Set singleton = new HashSet();
					singleton.add(mGee.getGenotype());
					MultiGenotype indivToMultiGenotype = computeAlleleFrequencies(singleton, ad);
					mNuclearAlleleFrequency = indivToMultiGenotype.getNuclearAlleleFrequency();
					mMCytoplasmicAlleleFrequency = indivToMultiGenotype.getMCytoplasmicAlleleFrequency();
					mPCytoplasmicAlleleFrequency = indivToMultiGenotype.getPCytoplasmicAlleleFrequency();
				} else {
					mNuclearAlleleFrequency = ((MultiGenotype) mGenotype).getNuclearAlleleFrequency();
					mMCytoplasmicAlleleFrequency = ((MultiGenotype) mGenotype).getMCytoplasmicAlleleFrequency();
					mPCytoplasmicAlleleFrequency = ((MultiGenotype) mGenotype).getPCytoplasmicAlleleFrequency();
				}
				if (pGenotype instanceof IndividualGenotype) {
					Set singleton = new HashSet();
					singleton.add(pGee.getGenotype());
					MultiGenotype indivToMultiGenotype = computeAlleleFrequencies(singleton, ad);
					pNuclearAlleleFrequency = indivToMultiGenotype.getNuclearAlleleFrequency();
					pMCytoplasmicAlleleFrequency = indivToMultiGenotype.getMCytoplasmicAlleleFrequency();
					pPCytoplasmicAlleleFrequency = indivToMultiGenotype.getPCytoplasmicAlleleFrequency();
				} else {
					pNuclearAlleleFrequency = ((MultiGenotype) pGenotype).getNuclearAlleleFrequency();
					pMCytoplasmicAlleleFrequency = ((MultiGenotype) pGenotype).getMCytoplasmicAlleleFrequency();
					pPCytoplasmicAlleleFrequency = ((MultiGenotype) pGenotype).getPCytoplasmicAlleleFrequency();
				}

				// END cp february 2004

				/**
				 * cp february 2004 int[][] mNuclearAlleleFrequency =
				 * ((MultiGenotype) mGenotype).getNuclearAlleleFrequency ();
				 * int[][] mMCytoplasmicAlleleFrequency = ((MultiGenotype)
				 * mGenotype).getMCytoplasmicAlleleFrequency (); int[][]
				 * mPCytoplasmicAlleleFrequency = ((MultiGenotype)
				 * mGenotype).getPCytoplasmicAlleleFrequency (); int[][]
				 * pNuclearAlleleFrequency = ((MultiGenotype)
				 * pGenotype).getNuclearAlleleFrequency (); int[][]
				 * pMCytoplasmicAlleleFrequency = ((MultiGenotype)
				 * pGenotype).getMCytoplasmicAlleleFrequency (); int[][]
				 * pPCytoplasmicAlleleFrequency = ((MultiGenotype)
				 * pGenotype).getPCytoplasmicAlleleFrequency (); END cp february
				 * 2004
				 */

				int[][] naf = new int[mNuclearAlleleFrequency.length][];
				int[][] maf = new int[mMCytoplasmicAlleleFrequency.length][];
				int[][] paf = new int[mPCytoplasmicAlleleFrequency.length][];

				for (int j = 0; j < mNuclearAlleleFrequency.length; j++) {
					naf[j] = new int[mNuclearAlleleFrequency[j].length];
					// long mEff = 0; c. pichot jv 2004
					int mEff = 0;
					int pEff = 0;
					for (int k = 0; k < mNuclearAlleleFrequency[j].length; k++) {
						naf[j][k] = (int) (n * (mNuclearAlleleFrequency[j][k] / (double) 2 / mGee.getNumber()))
								+ (int) (n * (pNuclearAlleleFrequency[j][k] / (double) 2 / pGee.getNumber()));
						mEff = mEff + (int) (n * (mNuclearAlleleFrequency[j][k] / (double) 2 / mGee.getNumber()));
						pEff = pEff + (int) (n * (pNuclearAlleleFrequency[j][k] / (double) 2 / pGee.getNumber()));
					}

					int[] mCumFrequency = new int[mNuclearAlleleFrequency[j].length]; // CP
																						// 02/03
																						// stochastic
																						// adjustment
																						// to
																						// the
																						// expected
																						// number
																						// of
																						// nuclear
																						// seed
																						// gees
					mCumFrequency[0] = mNuclearAlleleFrequency[j][0];
					for (int l = 1; l < mNuclearAlleleFrequency[j].length; l++) {
						mCumFrequency[l] = mCumFrequency[l - 1] + mNuclearAlleleFrequency[j][l];
					}
					for (int k = 0; k < (n - mEff); k++) {
						int proba = random.nextInt(mCumFrequency[mCumFrequency.length - 1]);
						int compteur = 0;
						while (proba > mCumFrequency[compteur]) {
							compteur++;
						}
						naf[j][compteur] = naf[j][compteur] + 1;
					}

					int[] pCumFrequency = new int[pNuclearAlleleFrequency[j].length]; // CP
																						// 02/03
																						// stochastic
																						// adjustment
																						// to
																						// the
																						// expected
																						// number
																						// of
																						// nuclear
																						// pollen
																						// gees
					pCumFrequency[0] = pNuclearAlleleFrequency[j][0];
					for (int l = 1; l < pNuclearAlleleFrequency[j].length; l++) {
						pCumFrequency[l] = pCumFrequency[l - 1] + pNuclearAlleleFrequency[j][l];
					}
					for (int k = 0; k < (n - pEff); k++) {
						int proba = random.nextInt(pCumFrequency[pCumFrequency.length - 1]);
						int compteur = 0;
						while (proba > pCumFrequency[compteur]) {
							compteur++;
						}
						naf[j][compteur] = naf[j][compteur] + 1;
					}
				}
				for (int j = 0; j < mMCytoplasmicAlleleFrequency.length; j++) {
					maf[j] = new int[mMCytoplasmicAlleleFrequency[j].length];
					int eff = 0;
					for (int k = 0; k < mMCytoplasmicAlleleFrequency[j].length; k++) {
						maf[j][k] = (int) ((mMCytoplasmicAlleleFrequency[j][k] / (double) mGee.getNumber()) * n);
						eff = eff + maf[j][k];
					}
					int[] cumFrequency = new int[mMCytoplasmicAlleleFrequency[j].length]; // CP
																							// 02/03
																							// stochastic
																							// adjustment
																							// to
																							// the
																							// expected
																							// number
																							// of
																							// maternally
																							// inherited
																							// DNA
					cumFrequency[0] = mMCytoplasmicAlleleFrequency[j][0];
					for (int l = 1; l < mMCytoplasmicAlleleFrequency[j].length; l++) {
						cumFrequency[l] = cumFrequency[l - 1] + mMCytoplasmicAlleleFrequency[j][l];
					}
					for (int k = 0; k < (n - eff); k++) {
						int proba = random.nextInt(cumFrequency[cumFrequency.length - 1]);
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						maf[j][compteur] = maf[j][compteur] + 1;
					}
				}
				for (int j = 0; j < pPCytoplasmicAlleleFrequency.length; j++) {
					paf[j] = new int[pPCytoplasmicAlleleFrequency[j].length];
					int eff = 0;
					for (int k = 0; k < pPCytoplasmicAlleleFrequency[j].length; k++) {
						paf[j][k] = (int) ((pPCytoplasmicAlleleFrequency[j][k] / (double) pGee.getNumber()) * n);
						eff = eff + paf[j][k];
					}
					int[] cumFrequency = new int[pPCytoplasmicAlleleFrequency[j].length]; // CP
																							// 02/03
																							// stochastic
																							// adjustment
																							// to
																							// the
																							// expected
																							// number
																							// of
																							// paternally
																							// inherited
																							// DNA
					cumFrequency[0] = pPCytoplasmicAlleleFrequency[j][0];
					for (int l = 1; l < pPCytoplasmicAlleleFrequency[j].length; l++) {
						cumFrequency[l] = cumFrequency[l - 1] + pPCytoplasmicAlleleFrequency[j][l];
					}
					for (int k = 0; k < (n - eff); k++) {
						int proba = random.nextInt(cumFrequency[cumFrequency.length - 1]);
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						paf[j][compteur] = paf[j][compteur] + 1;
					}
				}
				MultiGenotype mg = new MultiGenotype(naf, maf, paf);
				cohort.add(mg);

				// cp 02/04 } else {// if one parent has IndividualGenotype or
				// number of descendants, n, is lower than 10000,
				// compute IndividualGenotype of n descendants from parents
				// genotype, getGamete () and fusesGametes ().

			} else {// n is lower than 10001,compute IndividualGenotype of n
					// descendants from parents genotype, getGamete () and
					// fusesGametes ().
				Genotype mam;
				Genotype pap;
				if (mGenotype instanceof IndividualGenotype || pGenotype instanceof IndividualGenotype) {
					if (mGenotype instanceof IndividualGenotype) {

						// cp 02/03 mam = new IndividualGenotype
						// (((IndividualGenotype) mGenotype).getNuclearDNA (),
						// null, ((IndividualGenotype)
						// mGenotype).getPCytoplasmicDNA ());
						mam = new IndividualGenotype(((IndividualGenotype) mGenotype).getNuclearDNA(),
								((IndividualGenotype) mGenotype).getMCytoplasmicDNA(), null);
						// mam = new IndividualGenotype (((IndividualGenotype)
						// mGenotype).getNuclearDNA (), ((IndividualGenotype)
						// mGenotype).getMCytoplasmicDNA (),
						// ((IndividualGenotype) pGenotype).getPCytoplasmicDNA
						// ());

						if (pGenotype instanceof IndividualGenotype) {
							pap = new IndividualGenotype(((IndividualGenotype) pGenotype).getNuclearDNA(), null,
									((IndividualGenotype) pGenotype).getPCytoplasmicDNA());
							// pap = new IndividualGenotype
							// (((IndividualGenotype) pGenotype).getNuclearDNA
							// (), ((IndividualGenotype)
							// pGenotype).getMCytoplasmicDNA (),
							// ((IndividualGenotype)
							// pGenotype).getPCytoplasmicDNA ());

						} else {
							pap = new MultiGenotype(((MultiGenotype) pGenotype).getNuclearAlleleFrequency(), null,
									((MultiGenotype) pGenotype).getPCytoplasmicAlleleFrequency());
							// pap = new MultiGenotype (((MultiGenotype)
							// pGenotype).getNuclearAlleleFrequency (),
							// ((MultiGenotype)
							// pGenotype).getMCytoplasmicAlleleFrequency (),
							// ((MultiGenotype)
							// pGenotype).getPCytoplasmicAlleleFrequency ());

						}
					} else {
						mam = new MultiGenotype(((MultiGenotype) mGenotype).getNuclearAlleleFrequency(),
								((MultiGenotype) mGenotype).getMCytoplasmicAlleleFrequency(), null);
						// mam = new MultiGenotype (((MultiGenotype)
						// mGenotype).getNuclearAlleleFrequency (),
						// ((MultiGenotype)
						// mGenotype).getMCytoplasmicAlleleFrequency (),
						// ((MultiGenotype)
						// mGenotype).getPCytoplasmicAlleleFrequency ());

						pap = new IndividualGenotype(((IndividualGenotype) pGenotype).getNuclearDNA(), null,
								((IndividualGenotype) pGenotype).getPCytoplasmicDNA());
						// pap = new IndividualGenotype (((IndividualGenotype)
						// pGenotype).getNuclearDNA (), ((IndividualGenotype)
						// pGenotype).getMCytoplasmicDNA (),
						// ((IndividualGenotype) pGenotype).getPCytoplasmicDNA
						// ());

					}
				} else {
					mam = new MultiGenotype(((MultiGenotype) mGenotype).getNuclearAlleleFrequency(),
							((MultiGenotype) mGenotype).getMCytoplasmicAlleleFrequency(),
							((MultiGenotype) mGenotype).getPCytoplasmicAlleleFrequency());
					pap = new MultiGenotype(((MultiGenotype) pGenotype).getNuclearAlleleFrequency(),
							((MultiGenotype) pGenotype).getMCytoplasmicAlleleFrequency(),
							((MultiGenotype) pGenotype).getPCytoplasmicAlleleFrequency());
				}
				for (int j = 0; j < n; j++) {
					IndividualGenotype mGamete = mam.getGamete(mGee.getAlleleParameters());
					IndividualGenotype pGamete = pap.getGamete(pGee.getAlleleParameters());
					IndividualGenotype g = Genotype.fuseGametes(mGamete, pGamete);

					// cp 02/03 a verifier
					if (mGenotype instanceof IndividualGenotype) {
						g = new IndividualGenotype(g.getNuclearDNA(),
								((IndividualGenotype) (mGee.getGenotype())).getMCytoplasmicDNA(),
								g.getPCytoplasmicDNA());
					}
					if (pGenotype instanceof IndividualGenotype) {
						g = new IndividualGenotype(g.getNuclearDNA(), g.getMCytoplasmicDNA(),
								((IndividualGenotype) (pGee.getGenotype())).getPCytoplasmicDNA());
					}
					// g = new IndividualGenotype (g.getNuclearDNA (),
					// g.getMCytoplasmicDNA (), g.getPCytoplasmicDNA ()); //cp
					// end cp 02/03

					cohort.add(g);
				}
			}
			if (related) {
				// compute cumulated individual consanguinity
				consanguinity = consanguinity + n * phi(s, mId, pId, s.getDate());
				// built map of parents with their gametic contribution
				if (!parent.keySet().contains(new Integer(mId))) {
					parent.put(new Integer(mId), new Integer(n));
				} else {
					parent.put(new Integer(mId), new Integer(((Integer) parent.get(new Integer(mId))).intValue() + n));
				}
				if (!parent.keySet().contains(new Integer(pId))) {
					parent.put(new Integer(pId), new Integer(n));
				} else {
					parent.put(new Integer(pId), new Integer(((Integer) parent.get(new Integer(pId))).intValue() + n));
				}
			}
		}

		if (related) {
			// cp 02/03 compute mean individual consanguinity
			consanguinity = consanguinity / m;
			// cp 02/03 compute cumulated global consanguinity using parent
			// gametic contributions
			for (Iterator ite = parent.keySet().iterator(); ite.hasNext();) {
				int id = ((Integer) ite.next()).intValue();
				Genotypable t = (Genotypable) s.getGenotypable(id);
				double number = t.getNumber(); // fc - 22.8.2006 - Numberable
												// returns double
				int contribution = ((Integer) parent.get(new Integer(id))).intValue();
				// cp 02/03 2 gametes from the same parent
				globalConsanguinity = globalConsanguinity
						+ (contribution / (double) 2 / m * (contribution - 1) / ((double) 2 * m - 1) * (1 / (double) 2
								/ number + (1 - 1 / (double) 2 / number) * t.getGlobalConsanguinity()));
				// cp 02/03 2 gametes from 2 parents
				for (Iterator i = parent.keySet().iterator(); i.hasNext();) {
					int d = ((Integer) i.next()).intValue();
					if (d != id) {
						Genotypable tt = (Genotypable) s.getGenotypable(d);
						double e = tt.getNumber(); // fc - 22.8.2006 -
													// Numberable returns double
						int c = ((Integer) parent.get(new Integer(d))).intValue();
						globalConsanguinity = globalConsanguinity + contribution / (double) 2 / m * c
								/ ((double) 2 * m - 1) * phi(s, id, d, s.getDate());
					}
				}
			}
		} else {
			consanguinity = -1;
			globalConsanguinity = -1;
		}
		MultiGenotype newMultiGenotype = computeAlleleFrequencies(cohort, ad);
		result[0] = (Object) newMultiGenotype;
		if (nbCouples == 1) {
			result[1] = new Integer(parents[0][0]);
			result[2] = new Integer(parents[0][1]);
		} else {
			result[1] = new Integer(-1);
			result[2] = new Integer(-1);
		}
		result[3] = (Object) (new Double(consanguinity));
		result[4] = (Object) (new Double(globalConsanguinity));
		return result;
	} // end-of-computeNewMultiGenotype

	/**
	 * Compute allelic frequencies in a collection of Genotypes. This method is
	 * used by actualizeMultiGenotype () and computeNewMultiGenotype ().
	 */
	static private MultiGenotype computeAlleleFrequencies(Collection genotypes, AlleleDiversity ad) {
		short[][] nad = ad.getNuclearAlleleDiversity();
		int[][] frequenceN = new int[nad.length][];
		short[][] mad = ad.getMCytoplasmicAlleleDiversity();
		int[][] frequenceM = new int[mad.length][];
		short[][] pad = ad.getPCytoplasmicAlleleDiversity();
		int[][] frequenceP = new int[pad.length][];
		int taille = genotypes.size();

		for (int i = 0; i < nad.length; i++) {
			frequenceN[i] = new int[nad[i].length];
		}
		for (int i = 0; i < mad.length; i++) {
			frequenceM[i] = new int[mad[i].length];
		}
		for (int i = 0; i < pad.length; i++) {
			frequenceP[i] = new int[pad[i].length];
		}

		for (Iterator i = genotypes.iterator(); i.hasNext();) {
			Genotype g = (Genotype) i.next();
			if (g instanceof IndividualGenotype) {
				IndividualGenotype ig = (IndividualGenotype) g;
				short[][] nuclearDNA = ig.getNuclearDNA();
				for (int j = 0; j < nuclearDNA.length; j++) {
					for (int k = 0; k < nuclearDNA[j].length; k++) {
						int l = 0;
						while (nuclearDNA[j][k] != nad[j][l]) {
							l = l + 1;
						}
						frequenceN[j][l] = frequenceN[j][l] + 1;
					}
				}
				short[] mCytoplasmicDNA = ig.getMCytoplasmicDNA();
				for (int j = 0; j < mCytoplasmicDNA.length; j++) {
					int l = 0;
					while (mCytoplasmicDNA[j] != mad[j][l]) {
						l = l + 1;
					}
					frequenceM[j][l] = frequenceM[j][l] + 1;
				}
				short[] pCytoplasmicDNA = ig.getPCytoplasmicDNA();
				for (int j = 0; j < pCytoplasmicDNA.length; j++) {
					int l = 0;
					while (pCytoplasmicDNA[j] != pad[j][l]) {
						l = l + 1;
					}
					frequenceP[j][l] = frequenceP[j][l] + 1;
				}
			} else {
				MultiGenotype mg = (MultiGenotype) g;
				int[][] naf = mg.getNuclearAlleleFrequency();
				for (int j = 0; j < naf.length; j++) {
					for (int k = 0; k < naf[j].length; k++) {
						frequenceN[j][k] = frequenceN[j][k] + naf[j][k]; // cp
																			// january
																			// 2004
					}
				}
				int[][] maf = mg.getMCytoplasmicAlleleFrequency();
				for (int j = 0; j < maf.length; j++) {
					for (int k = 0; k < maf[j].length; k++) {
						frequenceM[j][k] = frequenceM[j][k] + maf[j][k]; // cp
																			// january
																			// 2004
					}
				}
				int[][] paf = mg.getPCytoplasmicAlleleFrequency();
				for (int j = 0; j < paf.length; j++) {
					for (int k = 0; k < paf[j].length; k++) {
						frequenceP[j][k] = frequenceP[j][k] + paf[j][k]; // cp
																			// january
																			// 2004
					}
				}
			}
		}
		MultiGenotype newMultiGenotype = new MultiGenotype(frequenceN, frequenceM, frequenceP);
		return newMultiGenotype;
	}

	/**
	 * Compute kinship coefficient between two genotypables.
	 */
	static public double phi(GeneticScene fromScene, int mId, int pId, int date) {

		// fc+som-23.5.2017 GeneticMemory
		GeneticScene s = (GeneticScene) fromScene;

		GeneticMemory geneticMemory = s.getGeneticMemory();

		Genotypable mGee = geneticMemory.searchGee(s, mId);
		Genotypable pGee = geneticMemory.searchGee(s, pId);

		// fc-31.7.2018 removed these 4 lines, unused
//		Step step = fromScene.getStep();
//		if (step == null) {
//			step = GeneticTools.lastKnownStep;
//		}

		// Step initialStep = (Step) step.getProject().getRoot();
		// GeneticScene initialScene = (GeneticScene) initialStep.getScene();

		double consanguinity;
		if (mId == -1 || pId == -1) {
			consanguinity = 0;

		} else if (mId == pId) {

			consanguinity = 0.5 + 0.5 * mGee.getGlobalConsanguinity();

			// Log.println ("Relatdness", "tree " + mGee.getId () + "  & tree "
			// + pGee.getId () + " relatdness" + consanguinity);

		} else if (mGee != null && pGee != null && geneticMemory.initialSceneContains(mId)
				&& geneticMemory.initialSceneContains(pId)) {

			GenoSpecies genoSpecies = mGee.getGenoSpecies();

			consanguinity = getInitialPhi(genoSpecies, mId, pId);

			// Log.println ("Relatdness", "tree " + mGee.getId () + "  & tree "
			// + pGee.getId () + " relatdness" + consanguinity);

		} else if (!geneticMemory.initialSceneContains(mId)) {

			// pichot march 2004 consanguinity = 0.5 * phi (fromScene,
			// mGee.getMId (), pId, -1) + 0.5 * phi (fromScene, mGee.getPId (),
			// pId, -1);

			consanguinity = 0.5 * phi(fromScene, mGee.getMId(), pId, mGee.getCreationDate()) + 0.5
					* phi(fromScene, mGee.getPId(), pId, mGee.getCreationDate());

			// Log.println ("Relatdness", "tree " + mGee.getId () + "  & tree "
			// + pGee.getId () + " relatdness" + consanguinity);

		} else if (!geneticMemory.initialSceneContains(pId)) {

			consanguinity = 0.5 * phi(fromScene, pGee.getMId(), mId, pGee.getCreationDate()) + 0.5
					* phi(fromScene, pGee.getPId(), mId, pGee.getCreationDate());

			// Log.println ("Relatdness", "tree " + mGee.getId () + "  & tree "
			// + pGee.getId () + " relatdness" + consanguinity);
			// END pichot march 2004

		} else {
			consanguinity = -1;
		}

		return consanguinity;
	}

	/**
	 * Return kinship coefficient between two genotypables of initial scene.
	 */
	static private double getInitialPhi(GenoSpecies genoSpecies, int mId, int pId) {

		// fc+som-27.6.2017 PDG does not keep the initialScene, replaced by
		// GenoSpecies just upper
		// Genotypable t = (Genotypable) s.getGenotypable(mId);

		double[][] initialPhiArray = genoSpecies.getKinship().getInitialPhiArray();
		double defaultPhi = genoSpecies.getKinship().getDefaultPhi();
		double phi = -1;
		int i = 0;
		boolean found = false;
		while (i < initialPhiArray.length && !found) {
			if (mId == initialPhiArray[i][0] && pId == initialPhiArray[i][1]) {
				phi = initialPhiArray[i][2];
				found = true;
			}
			if (mId == initialPhiArray[i][1] && pId == initialPhiArray[i][0]) {
				phi = initialPhiArray[i][2];
				found = true;
			}
			i = i + 1;
		}
		if (!found) {
			phi = defaultPhi;
		}
		return phi;
	}

	/**
	 * Find a genotypable given its id and a scene. If the genotypable is not in
	 * the scene (dead or cut at this date), go back in the history to find a
	 * scene containing an occurence of the genotypable at a previous date.
	 */
	// fc+som-22.5.2017 MOVED to GeneticMemory, better memory management
	// static public Genotypable searchGee (GeneticScene fromScene, int geeId) {
	// if (geeId == -1) {return null;}
	// Genotypable t = (Genotypable) fromScene.getGenotypable (geeId);
	// if (t != null) {
	// return t;
	// } else {
	// Step step = fromScene.getStep ();
	// Step previousStep ;
	// if (step != null) {
	// previousStep = (Step) step.getFather ();
	// } else { // fromScene may not yet be tied to a step.
	// previousStep = GeneticTools.lastKnownStep;
	// }
	// if (previousStep == null) {
	// return null;
	// } else {
	// GeneticScene previousScene = (GeneticScene) previousStep.getScene ();
	// return GeneticTools.searchGee (previousScene, geeId);
	// }
	// }
	// }

	/**
	 * Find the scene containing the parents of a genotypable, given a scene and
	 * creationDate of genotypable. If needed, go back in the history to find a
	 * scene matching the given creation date.
	 */
	// fc+som-22.5.2017 REMOVED, unused
	// static public GeneticScene searchScene (GeneticScene scene, int
	// creationDate) {
	// if (scene.getDate () == creationDate) {
	// return scene;
	// } else {
	// Step step = scene.getStep ();
	// Step previousStep;
	// if (step != null) {
	// previousStep = (Step) step.getFather ();
	// } else {
	// previousStep = GeneticTools.lastKnownStep;
	// }
	// if (previousStep == null) {
	// return null;
	// } else {
	// GeneticScene previousScene = (GeneticScene) previousStep.getScene ();
	// return GeneticTools.searchScene (previousScene, creationDate);
	// }
	// }
	// }

	/**
	 * This class defines the objects contained in MultiGenotypeTracing.
	 */
	static protected class MultiGenotypeTracing {
		public MultiGenotype lastKnownMultiGenotype;
		public Vector steps;

		public MultiGenotypeTracing(MultiGenotype mg, Vector s) {
			lastKnownMultiGenotype = mg;
			steps = s;
		}

		public MultiGenotype getLastKnownMultiGenotype() {
			return lastKnownMultiGenotype;
		}

		public Vector getSteps() {
			return steps;
		}

		public void setLastKnowMultiGenotype(MultiGenotype mg) {
			lastKnownMultiGenotype = mg;
		}

		public void setSteps(Vector s) {
			steps = s;
		}
	}

	/**
	 * Find last known MultiGenotype of a mean genotypable.
	 */
	// fc - 5.11.2004 - added parameter to make the method static and changed
	// method modifier to public
	static public MultiGenotypeTracing searchLastMultiGenotype(Genotypable gee) throws Exception {
		MultiGenotype multiGenotype = null;
		Vector steps = new Vector();
		GeneticScene scene = gee.getGeneticScene();
		Step step = scene.getStep();
		steps.add(step);
		Step previousStep = null;
		boolean found = false;
		try {
			while (!found && !scene.isInitialScene()) {
				if (step == null) {
					previousStep = GeneticTools.lastKnownStep;
				} else {
					previousStep = (Step) step.getFather();
				}
				steps.add(previousStep);
				GeneticScene previousScene = (GeneticScene) previousStep.getScene();
				Genotypable previousGee = (Genotypable) previousScene.getGenotypable(gee.getId());
				multiGenotype = (MultiGenotype) previousGee.getMultiGenotype();
				if (multiGenotype == null) {
					if (logEnabled)
						Log.println(Log.ERROR, "searchLastMultiGenotype ()",
								"MultiGenotype should not be null during simulation for genotyped object.");
					throw new Exception("MultiGenotype should not be null during simulation for genotyped object.");
				} else if (!multiGenotype.equals(Genotype.EMPTY_MULTI_GENOTYPE)) {
					found = true;
				} else {
					step = previousStep;
					scene = previousScene;
				}
			}

			if (found) {
				MultiGenotypeTracing result = new MultiGenotypeTracing(multiGenotype, steps);
				return result;
			}
		} catch (Exception exc) {
			if (logEnabled)
				Log.println(Log.ERROR, "searchLastMultiGenotype ()", "Exception was caught : " + exc);
			throw (exc);
		}
		return null;
	}

	/**
	 * Between current Step and the last Step where MultiGenotype was known,
	 * this method computes the multiGenotype as often as size of mean
	 * genotypable change. If size is equal to size at previous Step,
	 * multiGenotype is saved as empty MultiGenotype. So evolution of
	 * MultiGenotype is conditionned only by initial multiGenotype and by size
	 * of genotypable. Note that size of genotypable must be decreasing.
	 */
	static protected MultiGenotype actualizeMultiGenotype(Genotypable gee) throws Exception {
		int geeId = gee.getId();
		GeneticScene currentScene = gee.getGeneticScene();

		// Step currentStep = currentScene.getStep(); // fc+som-27.6.2017 unused
		// AlleleDiversity ad = gee.getGenoSpecies().getAlleleDiversity(); //
		// fc+som-27.6.2017 unused

		// fc+som-27.6.2017 TODO: searchLastMultiGenotype() must be rewritten
		// with the GeneticMemory instead of the steps to allow the use of the
		// Capsis memorizers (all steps may not be kept in memory)
		GeneticTools.MultiGenotypeTracing trace = GeneticTools.searchLastMultiGenotype(gee);

		Vector steps = trace.getSteps();
		int nbSteps = steps.size();

		MultiGenotype mg = trace.getLastKnownMultiGenotype();
		Step step = (Step) steps.get(nbSteps - 1);
		GeneticScene scene = (GeneticScene) step.getScene();
		Genotypable gee2 = (Genotypable) scene.getGenotypable(geeId);
		double number = gee2.getNumber(); // fc - 22.8.2006 - Numberable returns
											// double
		for (int i = nbSteps - 2; i > -1; i--) {
			Step nextStep = (Step) steps.get(i);
			GeneticScene nextScene;
			if (nextStep == null && i == 0) {
				nextScene = currentScene;
			} else {
				nextScene = (GeneticScene) nextStep.getScene();
			}
			Genotypable nextGee = (Genotypable) nextScene.getGenotypable(geeId);
			double nextNumber = nextGee.getNumber(); // fc - 22.8.2006 -
														// Numberable returns
														// double
			if (nextNumber < number) {
				Collection cohort = new HashSet();
				int nbGee = (int) (number - nextNumber); // fc - 22.8.2006 -
															// Numberable
															// returns double
				int[][] nuclearAlleleFrequence = mg.getNuclearAlleleFrequency();
				int[][] mCytoplasmicAlleleFrequence = mg.getMCytoplasmicAlleleFrequency();
				int[][] pCytoplasmicAlleleFrequence = mg.getPCytoplasmicAlleleFrequency();
				int[][] naf = arrayCopy(nuclearAlleleFrequence);
				int[][] maf = arrayCopy(mCytoplasmicAlleleFrequence);
				int[][] paf = arrayCopy(pCytoplasmicAlleleFrequence);
				for (int j = 0; j < nuclearAlleleFrequence.length; j++) {
					int eff = 0;
					if (nbGee > 10000) {
						for (int k = 0; k < nuclearAlleleFrequence[j].length; k++) {
							naf[j][k] = naf[j][k] - (int) (nuclearAlleleFrequence[j][k] / (double) number * nbGee);
							eff = eff + (int) (nuclearAlleleFrequence[j][k] / (double) number * nbGee);
						}
					}
					for (int k = 0; k < (2 * nbGee - eff); k++) {
						int[] cumFrequency = new int[naf[j].length];
						cumFrequency[0] = naf[j][0];
						for (int l = 1; l < naf[j].length; l++) {
							cumFrequency[l] = cumFrequency[l - 1] + naf[j][l];
						}
						int proba = random.nextInt(cumFrequency[cumFrequency.length - 1]);
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						naf[j][compteur] = naf[j][compteur] - 1;
					}
				}
				for (int j = 0; j < mCytoplasmicAlleleFrequence.length; j++) {
					int eff = 0;
					if (nbGee > 10000) {
						for (int k = 0; k < mCytoplasmicAlleleFrequence[j].length; k++) {
							maf[j][k] = maf[j][k] - (int) (mCytoplasmicAlleleFrequence[j][k] / (double) number * nbGee);
							eff = eff + (int) (mCytoplasmicAlleleFrequence[j][k] / (double) number * nbGee);
						}
					}
					for (int k = 0; k < (nbGee - eff); k++) {
						int[] cumFrequency = new int[maf[j].length];
						cumFrequency[0] = maf[j][0];
						for (int l = 1; l < maf[j].length; l++) {
							cumFrequency[l] = cumFrequency[l - 1] + maf[j][l];
						}
						int proba = random.nextInt(cumFrequency[cumFrequency.length - 1]);
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						maf[j][compteur] = maf[j][compteur] - 1;
					}
				}
				for (int j = 0; j < pCytoplasmicAlleleFrequence.length; j++) {
					int eff = 0;
					if (nbGee > 10000) {
						for (int k = 0; k < pCytoplasmicAlleleFrequence[j].length; k++) {
							paf[j][k] = paf[j][k] - (int) (pCytoplasmicAlleleFrequence[j][k] / (double) number * nbGee);
							eff = eff + (int) (pCytoplasmicAlleleFrequence[j][k] / (double) number * nbGee);
						}
					}
					for (int k = 0; k < (nbGee - eff); k++) {
						int[] cumFrequency = new int[paf[j].length];
						cumFrequency[0] = paf[j][0];
						for (int l = 1; l < paf[j].length; l++) {
							cumFrequency[l] = cumFrequency[l - 1] + paf[j][l];
						}
						int proba = random.nextInt(cumFrequency[cumFrequency.length - 1]);
						int compteur = 0;
						while (proba > cumFrequency[compteur]) {
							compteur++;
						}
						paf[j][compteur] = paf[j][compteur] - 1;
					}
				}
				mg = new MultiGenotype(naf, maf, paf);
				nextGee.setMultiGenotype(mg);
				gee2 = nextGee;
				number = nextNumber;
			}
			if (nextNumber > number) {
				throw new Exception("number of genotypable (id=" + geeId + ") can't increase. wrong number " + number);
			}
		}
		return mg;
	}

	// Array duplication
	//
	static public int[][] arrayCopy(int[][] tab) {
		
		// fc-31.7.2018
		if (tab == null)
			return null;
		
		int[][] newTab = new int[tab.length][];
		for (int i = 0; i < tab.length; i++) {
			newTab[i] = new int[tab[i].length];
			for (int j = 0; j < tab[i].length; j++) {
				newTab[i][j] = tab[i][j];
			}
		}
		return newTab;
	}

	// New utility methods
	// ------------------------------------------------------ fc - november 2004
	//

	/**
	 * Check if all Genotypables in the collection have same species. Caution:
	 * collection may contain objects which are not Genotypable instances :
	 * ignore them.
	 */
	static public boolean haveAllSameSpecies(Collection gees) {
		boolean sameSpecies = true;
		GenoSpecies firstSpecies = null;

		Iterator i = gees.iterator();
		while (sameSpecies && i.hasNext()) {

			Object o = i.next();
			if (!(o instanceof Genotypable)) {
				continue;
			}
			Genotypable gee = (Genotypable) o;

			if (firstSpecies == null) {
				firstSpecies = gee.getGenoSpecies();
				continue;
			}

			GenoSpecies species = gee.getGenoSpecies();
			if (!species.equals(firstSpecies)) {
				sameSpecies = false;
			}
		}
		return sameSpecies;
	}

	/**
	 * Check if all Genotypables in the collection have individual genotypes.
	 * Caution: collection may contain objects which are not Genotypable
	 * instances : ignore them.
	 */
	static public boolean haveAllAnIndividualGenotype(Collection gees) {
		boolean all = true;

		Iterator i = gees.iterator();
		while (all && i.hasNext()) {

			Object o = i.next();
			if (!(o instanceof Genotypable)) {
				continue;
			}
			Genotypable gee = (Genotypable) o;

			if (!(gee.getGenotype() instanceof IndividualGenotype)) {
				all = false;
			}
		}
		return all;
	}

}
