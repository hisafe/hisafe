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
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jeeb.lib.util.Translator;

/**
 * This class contains the method that test if genetic data are compatible. It
 * is applied by Validate.validateInitialData ().
 * 
 * @author I. Seynave - September 2002, F. de Coligny - November 2004
 */
public class AreGeneticDataCompatible {

	static {
		Translator.addBundle("capsis.lib.genetics.Validate");
	}

	/**
	 * Test if genetic initial data are compatible.
	 */
	public static void testIfInitialDataNotValid(Genotypable gee, Map speciesIndividualWithGenotype,
			Map speciesIndividualWithEmptyGenotype, Map speciesPopulationWithGenotype,
			Map speciesPopulationWithEmptyGenotype, Set speciesWithGenotype) throws Exception {

		// 1- test if, by species, Genotype have same format

		// stop if, by species, all IndividualGenotype have not same format
		// (i.d. same number of loci)
		if (!speciesIndividualWithGenotype.isEmpty()) {
			for (Iterator i = speciesIndividualWithGenotype.values().iterator(); i.hasNext();) {
				Collection geesWithSameSpecies = (Collection) i.next();

				Object _t = geesWithSameSpecies.iterator().next();
				if (!(_t instanceof Genotypable)) {
					continue;
				}
				Genotypable t = (Genotypable) _t; // phd 2003_03_17

				int nbNuclearLoci = (((IndividualGenotype) t.getGenotype()).getNuclearDNA()).length;
				int nbMCytoplasmicLoci = (((IndividualGenotype) t.getGenotype()).getMCytoplasmicDNA()).length;
				int nbPCytoplasmicLoci = (((IndividualGenotype) t.getGenotype()).getPCytoplasmicDNA()).length;
				for (Iterator ite = geesWithSameSpecies.iterator(); ite.hasNext();) {

					_t = ite.next();
					if (!(_t instanceof Genotypable)) {
						continue;
					}
					Genotypable tr = (Genotypable) _t; // phd 2003_03_17

					int nbn = (((IndividualGenotype) tr.getGenotype()).getNuclearDNA()).length;
					int nbm = (((IndividualGenotype) tr.getGenotype()).getMCytoplasmicDNA()).length;
					int nbp = (((IndividualGenotype) tr.getGenotype()).getPCytoplasmicDNA()).length;
					if (nbn != nbNuclearLoci || nbm != nbMCytoplasmicLoci || nbp != nbPCytoplasmicLoci) {
						// ~ if (gee instanceof SpeciesDefined) {
						// ~ throw new Exception("\n" + Translator.swap
						// ("Validate.species") + " " + ((SpeciesDefined)
						// t).getSpecies () +
						// ~ ", "+ Translator.swap ("Validate.IGFormat"));
						// ~ } else {
						// ~ throw new Exception("\n" + Translator.swap
						// ("Validate.IGFormat"));
						// ~ }
						throw new Exception("\n" + Translator.swap("Validate.species") + " " + t.getGenoSpecies()
								+ ", " + Translator.swap("Validate.IGFormat"));
					}
				}
			}
		}

		if (!speciesPopulationWithGenotype.isEmpty()) {
			for (Iterator i = speciesPopulationWithGenotype.values().iterator(); i.hasNext();) {
				Collection geesWithSameSpecies = (Collection) i.next();

				Object _t = null;
				Iterator _i = geesWithSameSpecies.iterator();
				do {
					_t = _i.next();
				} while (!(_t instanceof Genotypable));
				Genotypable t = (Genotypable) _t; // phd 2003_03_17

				// stop if there is gee with multiGenotype but any allele
				// diversity defined
				if (t.getGenoSpecies().getAlleleDiversity() == null
						|| (t.getGenoSpecies().getAlleleDiversity()).isEmpty()) {
					// ~ if (gee instanceof SpeciesDefined) {
					// ~ throw new Exception("\n" + Translator.swap
					// ("Validate.species") + ((SpeciesDefined) t).getSpecies ()
					// ~ + ", " + Translator.swap ("Validate.ADDefined"));
					// ~ } else {
					// ~ throw new Exception("\n" + Translator.swap
					// ("Validate.ADDefined"));
					// ~ }
					throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
							+ Translator.swap("Validate.ADDefined"));

				} else {
					// stop if, by species, all MultiGenotype have not same
					// format
					int[][] nuclearAF = ((MultiGenotype) t.getGenotype()).getNuclearAlleleFrequency();
					int[][] mCytoAF = ((MultiGenotype) t.getGenotype()).getMCytoplasmicAlleleFrequency();
					int[][] pCytoAF = ((MultiGenotype) t.getGenotype()).getPCytoplasmicAlleleFrequency();
					int nbNuclearLoci = nuclearAF.length;
					int nbMCytoplasmicLoci = mCytoAF.length;
					int nbPCytoplasmicLoci = pCytoAF.length;
					for (Iterator ite = geesWithSameSpecies.iterator(); ite.hasNext();) {

						_t = ite.next();
						if (!(_t instanceof Genotypable)) {
							continue;
						}
						Genotypable tr = (Genotypable) _t; // phd 2003_03_17

						int[][] nuclear = ((MultiGenotype) tr.getGenotype()).getNuclearAlleleFrequency();
						int[][] mCyto = ((MultiGenotype) tr.getGenotype()).getMCytoplasmicAlleleFrequency();
						int[][] pCyto = ((MultiGenotype) tr.getGenotype()).getPCytoplasmicAlleleFrequency();
						int nbn = nuclear.length;
						int nbm = mCyto.length;
						int nbp = pCyto.length;
						if (nbn != nbNuclearLoci || nbm != nbMCytoplasmicLoci || nbp != nbPCytoplasmicLoci) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap
							// ("Validate.MGFormat"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.MGFormat"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.MGFormat"));
						}
						for (int j = 0; j < nuclear.length; j++) {
							int nb = nuclear[j].length;
							if (nb != nuclearAF[j].length) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.MGFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.MGFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.MGFormat"));
							}
						}
						for (int j = 0; j < mCyto.length; j++) {
							int nb = mCyto[j].length;
							if (nb != mCytoAF[j].length) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.MGFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.MGFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.MGFormat"));
							}
						}
						for (int j = 0; j < pCyto.length; j++) {
							int nb = pCyto[j].length;
							if (nb != pCytoAF[j].length) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.MGFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.MGFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.MGFormat"));
							}
						}
					}
				}
			}
		}

		// stop if, by species, IndividualGenotype and MultiGenotype have not
		// same number of lines
		if (!speciesIndividualWithGenotype.isEmpty() && !speciesPopulationWithGenotype.isEmpty()) {
			for (Iterator i = speciesIndividualWithGenotype.values().iterator(); i.hasNext();) {
				Collection geesWithSameSpecies = (Collection) i.next();

				Object _t = geesWithSameSpecies.iterator().next();
				if (!(_t instanceof Genotypable)) {
					continue;
				}
				Genotypable t = (Genotypable) _t; // phd 2003_03_17

				// ~ SpeciesDefined sdGee = (SpeciesDefined) t;
				// ~ QualitativeProperty espGee = sdGee.getSpecies ();
				GenoSpecies espGee = t.getGenoSpecies();

				int nbNuclearLoci = (((IndividualGenotype) t.getGenotype()).getNuclearDNA()).length;
				int nbMCytoplasmicLoci = (((IndividualGenotype) t.getGenotype()).getMCytoplasmicDNA()).length;
				int nbPCytoplasmicLoci = (((IndividualGenotype) t.getGenotype()).getPCytoplasmicDNA()).length;
				int test = 0;
				Iterator ite = speciesPopulationWithGenotype.values().iterator();
				while (ite.hasNext() && test == 0) {
					Collection populationWithSameSpecies = (Collection) ite.next();

					_t = populationWithSameSpecies.iterator().next();
					if (!(_t instanceof Genotypable)) {
						continue;
					}
					Genotypable pop = (Genotypable) _t; // phd 2003_03_17

					// fc - 10.11.2004 - all genotypables now have a
					// getGenoSpecies () method
					// ~ if (gee instanceof SpeciesDefined) {

					// ~ SpeciesDefined sdPop = (SpeciesDefined) pop;
					// ~ QualitativeProperty espPop = sdPop.getSpecies ();
					GenoSpecies espPop = pop.getGenoSpecies();

					// ~ if (espGee == espPop) {
					if (espGee.equals(espPop)) {
						test = 1;
					}

					// ~ } else {
					// ~ test = 1;
					// ~ }

					if (test == 1) {
						int nbn = (((MultiGenotype) pop.getGenotype()).getNuclearAlleleFrequency()).length;
						int nbm = (((MultiGenotype) pop.getGenotype()).getMCytoplasmicAlleleFrequency()).length;
						int nbp = (((MultiGenotype) pop.getGenotype()).getPCytoplasmicAlleleFrequency()).length;
						if (nbn != nbNuclearLoci || nbm != nbMCytoplasmicLoci || nbp != nbPCytoplasmicLoci) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap
							// ("Validate.IG-MGFormat"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.IG-MGFormat"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.IG-MGFormat"));

						}
					}
				}
			}
		}

		// 2- test if AlleleDiversity are valid

		// stop if alleleDiversity is not compatible with IndividualGenotype
		if (!speciesIndividualWithGenotype.isEmpty()) {
			for (Iterator i = speciesIndividualWithGenotype.values().iterator(); i.hasNext();) {
				Collection geesWithSameSpecies = (Collection) i.next();

				Object _t = geesWithSameSpecies.iterator().next();
				if (!(_t instanceof Genotypable)) {
					continue;
				}
				Genotypable t = (Genotypable) _t; // phd 2003_03_17

				int nbNuclearLoci = (((IndividualGenotype) t.getGenotype()).getNuclearDNA()).length;
				int nbMCytoplasmicLoci = (((IndividualGenotype) t.getGenotype()).getMCytoplasmicDNA()).length;
				int nbPCytoplasmicLoci = (((IndividualGenotype) t.getGenotype()).getPCytoplasmicDNA()).length;
				if (t.getGenoSpecies().getAlleleDiversity() != null
						&& !t.getGenoSpecies().getAlleleDiversity().isEmpty()) {
					short[][] nuclearAlleleDiversity = t.getGenoSpecies().getAlleleDiversity()
							.getNuclearAlleleDiversity();
					short[][] mCytoplasmicAlleleDiversity = t.getGenoSpecies().getAlleleDiversity()
							.getMCytoplasmicAlleleDiversity();
					short[][] pCytoplasmicAlleleDiversity = t.getGenoSpecies().getAlleleDiversity()
							.getPCytoplasmicAlleleDiversity();
					int n = nuclearAlleleDiversity.length;
					int m = mCytoplasmicAlleleDiversity.length;
					int p = pCytoplasmicAlleleDiversity.length;
					// stop if, by species, IndividualGenotype and
					// AlleleDiversity have not same format (i.d. same number of
					// lines)
					if (n != nbNuclearLoci || m != nbMCytoplasmicLoci || p != nbPCytoplasmicLoci) {
						// ~ if (gee instanceof SpeciesDefined) {
						// ~ throw new Exception("\n" + Translator.swap
						// ("Validate.species") + ((SpeciesDefined)
						// t).getSpecies ()
						// ~ + ", " + Translator.swap ("Validate.IG-ADFormat"));
						// ~ } else {
						// ~ throw new Exception("\n" + Translator.swap
						// ("Validate.IG-ADFormat"));
						// ~ }
						throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
								+ Translator.swap("Validate.IG-ADFormat"));
					}
					// stop if, by species, alleleDiversity doesn't contain all
					// allele that are in default alleleDiversity
					short[][] defaultNuclearAlleleDiversity = AlleleDiversity
							.computeNuclearAlleleDiversity(geesWithSameSpecies);
					short[][] defaultMCytoplasmicAlleleDiversity = AlleleDiversity.computeCytoplasmicAlleleDiversity(
							geesWithSameSpecies, "maternal");
					short[][] defaultPCytoplasmicAlleleDiversity = AlleleDiversity.computeCytoplasmicAlleleDiversity(
							geesWithSameSpecies, "paternal");
					Set[] nuclearAlleleDiversitySet = new Set[nuclearAlleleDiversity.length];
					Set[] mCytoplasmicAlleleDiversitySet = new Set[mCytoplasmicAlleleDiversity.length];
					Set[] pCytoplasmicAlleleDiversitySet = new Set[pCytoplasmicAlleleDiversity.length];
					Set[] defaultNuclearAlleleDiversitySet = new Set[defaultNuclearAlleleDiversity.length];
					Set[] defaultMCytoplasmicAlleleDiversitySet = new Set[defaultMCytoplasmicAlleleDiversity.length];
					Set[] defaultPCytoplasmicAlleleDiversitySet = new Set[defaultPCytoplasmicAlleleDiversity.length];

					for (int j = 0; j < nuclearAlleleDiversity.length; j++) {
						nuclearAlleleDiversitySet[j] = new HashSet();
						for (int k = 0; k < nuclearAlleleDiversity[j].length; k++) {
							nuclearAlleleDiversitySet[j].add(new Integer(nuclearAlleleDiversity[j][k]));
						}
					}
					for (int j = 0; j < mCytoplasmicAlleleDiversity.length; j++) {
						mCytoplasmicAlleleDiversitySet[j] = new HashSet();
						for (int k = 0; k < mCytoplasmicAlleleDiversity[j].length; k++) {
							mCytoplasmicAlleleDiversitySet[j].add(new Integer(mCytoplasmicAlleleDiversity[j][k]));
						}
					}
					for (int j = 0; j < pCytoplasmicAlleleDiversity.length; j++) {
						pCytoplasmicAlleleDiversitySet[j] = new HashSet();
						for (int k = 0; k < pCytoplasmicAlleleDiversity[j].length; k++) {
							pCytoplasmicAlleleDiversitySet[j].add(new Integer(pCytoplasmicAlleleDiversity[j][k]));
						}
					}
					for (int j = 0; j < defaultNuclearAlleleDiversity.length; j++) {
						defaultNuclearAlleleDiversitySet[j] = new HashSet();
						for (int k = 0; k < defaultNuclearAlleleDiversity[j].length; k++) {
							defaultNuclearAlleleDiversitySet[j].add(new Integer(defaultNuclearAlleleDiversity[j][k]));
						}
						if (!nuclearAlleleDiversitySet[j].containsAll(defaultNuclearAlleleDiversitySet[j])) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap ("Validate.lineAD") +
							// j + Translator.swap ("Validate.nuclearDNA"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.IG-ADFormat") + j +
							// ~ Translator.swap ("Validate.nuclearDNA"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.lineAD") + j + Translator.swap("Validate.nuclearDNA"));
						}
					}
					for (int j = 0; j < defaultMCytoplasmicAlleleDiversity.length; j++) {
						defaultMCytoplasmicAlleleDiversitySet[j] = new HashSet();
						for (int k = 0; k < defaultMCytoplasmicAlleleDiversity[j].length; k++) {
							defaultMCytoplasmicAlleleDiversitySet[j].add(new Integer(
									defaultMCytoplasmicAlleleDiversity[j][k]));
						}
						if (!mCytoplasmicAlleleDiversitySet[j].containsAll(defaultMCytoplasmicAlleleDiversitySet[j])) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap ("Validate.lineAD") +
							// j + Translator.swap ("Validate.MCytoDNA"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.IG-ADFormat") + j +
							// ~ Translator.swap ("Validate.MCytoDNA"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.lineAD") + j + Translator.swap("Validate.MCytoDNA"));
						}
					}
					for (int j = 0; j < defaultPCytoplasmicAlleleDiversity.length; j++) {
						defaultPCytoplasmicAlleleDiversitySet[j] = new HashSet();
						for (int k = 0; k < defaultPCytoplasmicAlleleDiversity[j].length; k++) {
							defaultPCytoplasmicAlleleDiversitySet[j].add(new Integer(
									defaultPCytoplasmicAlleleDiversity[j][k]));
						}
						if (!pCytoplasmicAlleleDiversitySet[j].containsAll(defaultPCytoplasmicAlleleDiversitySet[j])) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap ("Validate.lineAD") +
							// j + Translator.swap ("Validate.PCytoDNA"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.IG-ADFormat") + j +
							// ~ Translator.swap ("Validate.PCytoDNA"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.lineAD") + j + Translator.swap("Validate.PCytoDNA"));
						}
					}
				}
			}
		}

		// stop if, by species, MultiGenotype and AlleleDiversity have not same
		// format
		if (!speciesPopulationWithGenotype.isEmpty()) {
			for (Iterator i = speciesPopulationWithGenotype.values().iterator(); i.hasNext();) {
				Collection geesWithSameSpecies = (Collection) i.next();

				Object _t = geesWithSameSpecies.iterator().next();
				if (!(_t instanceof Genotypable)) {
					continue;
				}
				Genotypable t = (Genotypable) _t; // phd 2003_03_17

				int[][] nuclearAF = ((MultiGenotype) t.getGenotype()).getNuclearAlleleFrequency();
				int[][] mCytoAF = ((MultiGenotype) t.getGenotype()).getMCytoplasmicAlleleFrequency();
				int[][] pCytoAF = ((MultiGenotype) t.getGenotype()).getPCytoplasmicAlleleFrequency();
				int nbNuclearLoci = nuclearAF.length;
				int nbMCytoplasmicLoci = mCytoAF.length;
				int nbPCytoplasmicLoci = pCytoAF.length;
				if (t.getGenoSpecies().getAlleleDiversity() != null
						&& !t.getGenoSpecies().getAlleleDiversity().isEmpty()) {
					short[][] nuclearAllele = t.getGenoSpecies().getAlleleDiversity().getNuclearAlleleDiversity();
					short[][] mCytoplasmicAllele = t.getGenoSpecies().getAlleleDiversity()
							.getMCytoplasmicAlleleDiversity();
					short[][] pCytoplasmicAllele = t.getGenoSpecies().getAlleleDiversity()
							.getPCytoplasmicAlleleDiversity();
					int n = nuclearAllele.length;
					int m = mCytoplasmicAllele.length;
					int p = pCytoplasmicAllele.length;
					if (n != nbNuclearLoci || m != nbMCytoplasmicLoci || p != nbPCytoplasmicLoci) {
						// ~ if (gee instanceof SpeciesDefined) {
						// ~ throw new Exception("\n" + Translator.swap
						// ("Validate.species") + ((SpeciesDefined)
						// t).getSpecies ()
						// ~ + ", " + Translator.swap ("Validate.MG-ADFormat"));
						// ~ } else {
						// ~ throw new Exception("\n" + Translator.swap
						// ("Validate.MG-ADFormat"));
						// ~ }
						throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
								+ Translator.swap("Validate.MG-ADFormat"));
					} else {
						for (int j = 0; j < nuclearAF.length; j++) {
							int nb = nuclearAF[j].length;
							if (nb != nuclearAllele[j].length) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.MG-ADFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.MG-ADFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.MG-ADFormat"));
							}
						}
						for (int j = 0; j < mCytoAF.length; j++) {
							int nb = mCytoAF[j].length;
							if (nb != mCytoplasmicAllele[j].length) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.MG-ADFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.MG-ADFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.MG-ADFormat"));
							}
						}
						for (int j = 0; j < pCytoAF.length; j++) {
							int nb = pCytoAF[j].length;
							if (nb != pCytoplasmicAllele[j].length) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.MG-ADFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.MG-ADFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.MG-ADFormat"));
							}
						}
					}
				}
			}
		}

		// 3- test if AlleleEffect are valid

		for (Iterator i = speciesWithGenotype.iterator(); i.hasNext();) {
			Genotypable t;

			// fc - 10.11.2004 - all genotypables now have a getGenoSpecies ()
			// method
			// ~ if (gee instanceof SpeciesDefined) {

			GenoSpecies esp = (GenoSpecies) i.next();
			if (speciesIndividualWithGenotype.keySet().contains(esp)) {
				Object _t = null;
				Iterator _i = ((Collection) speciesIndividualWithGenotype.get(esp)).iterator();
				do {
					_t = _i.next();
				} while (!(_t instanceof Genotypable));
				t = (Genotypable) _t; // phd 2003_03_17
			} else {
				Object _t = null;
				Iterator _i = ((Collection) speciesPopulationWithGenotype.get(esp)).iterator();
				do {
					_t = _i.next();
				} while (!(_t instanceof Genotypable));
				t = (Genotypable) _t; // phd 2003_03_17
			}

			// ~ } else {
			// ~ if (! speciesIndividualWithGenotype.isEmpty ()) {
			// ~ Object _t = null;
			// ~ Iterator _i = ((Collection) speciesIndividualWithGenotype.get
			// ("IndividualTreeWithGenotype")).iterator ();
			// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
			// ~ t = (Genotypable) _t; // phd 2003_03_17
			// ~ } else {
			// ~ Object _t = null;
			// ~ Iterator _i = ((Collection) speciesPopulationWithGenotype.get
			// ("PopulationWithGenotype")).iterator ();
			// ~ do {_t = _i.next();} while (!(_t instanceof Genotypable));
			// ~ t = (Genotypable) _t; // phd 2003_03_17
			// ~ }
			// ~ }

			AlleleEffect ae = t.getGenoSpecies().getAlleleEffect();

			if (ae != null && !ae.getParameterNames().isEmpty()) {
				AlleleDiversity ad = t.getGenoSpecies().getAlleleDiversity();

				// stop if allele effect is defined but allele diversity is not
				if (ad == null || ad.isEmpty()) {
					// ~ if (gee instanceof SpeciesDefined) {
					// ~ throw new Exception("\n" + Translator.swap
					// ("Validate.species") + ((SpeciesDefined) t).getSpecies ()
					// ~ + ", " + Translator.swap ("Validate.ADDefined"));
					// ~ } else {
					// ~ throw new Exception("\n" + Translator.swap
					// ("Validate.ADDefined"));
					// ~ }
					throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
							+ Translator.swap("Validate.ADDefined"));
				} else {
					short[][] nuclearAllele = ad.getNuclearAlleleDiversity();
					short[][] mCytoplasmicAllele = ad.getMCytoplasmicAlleleDiversity();
					short[][] pCytoplasmicAllele = ad.getPCytoplasmicAlleleDiversity();
					// fc+cg-31.5.2018 changed Set by list in line below (order now guaranted)
					List parameters = ae.getParameterNames();
					for (Iterator ite = parameters.iterator(); ite.hasNext();) {
						String para = (String) ite.next();
						AlleleEffect.ParameterEffect pe = t.getGenoSpecies().getAlleleEffect().getParameterEffect(para);

						// stop if alleleEffect and AlleleDiversity are not
						// compatible
						short[][] nuclearEffect = pe.getNuclearAlleleEffect();
						short[][] mCytoplasmicEffect = pe.getMCytoplasmicAlleleEffect();
						short[][] pCytoplasmicEffect = pe.getPCytoplasmicAlleleEffect();
						for (int j = 0; j < nuclearEffect.length; j++) {
							int nb = nuclearEffect[j][0];
							if (nuclearAllele[nb - 1].length != nuclearEffect[j].length - 1) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.AE-ADFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.AE-ADFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.AE-ADFormat"));
							}
						}
						for (int j = 0; j < mCytoplasmicEffect.length; j++) {
							int nb = mCytoplasmicEffect[j][0];
							if (mCytoplasmicAllele[nb - 1].length != mCytoplasmicEffect[j].length - 1) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.AE-ADFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.AE-ADFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.AE-ADFormat"));
							}
						}
						for (int j = 0; j < pCytoplasmicEffect.length; j++) {
							int nb = pCytoplasmicEffect[j][0];
							if (pCytoplasmicAllele[nb - 1].length != pCytoplasmicEffect[j].length - 1) {
								// ~ if (gee instanceof SpeciesDefined) {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.species") + ((SpeciesDefined)
								// t).getSpecies ()
								// ~ + ", " + Translator.swap
								// ("Validate.AE-ADFormat"));
								// ~ } else {
								// ~ throw new Exception("\n" + Translator.swap
								// ("Validate.AE-ADFormat"));
								// ~ }
								throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies()
										+ ", " + Translator.swap("Validate.AE-ADFormat"));
							}
						}

						// stop if alleleEffecct is not complete
						if ((pe.getNuclearAlleleEffect()).length == 0 && (pe.getMCytoplasmicAlleleEffect()).length == 0
								&& (pe.getPCytoplasmicAlleleEffect()).length == 0) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap ("Validate.AEvalues")
							// + " " + para
							// ~ + " " + Translator.swap
							// ("Validate.givenAEvalues"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.AEvalues") + " "
							// ~ + para + " " + Translator.swap
							// ("Validate.givenAEvalues"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.AEvalues") + " " + para + " "
									+ Translator.swap("Validate.givenAEvalues"));
						}
						if (pe.getHeritability() == -1 && pe.getTotalEnvironmentalVariance() == -1) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap ("Validate.HTev") +
							// " " + para + " " +
							// ~ Translator.swap ("Validate.givenHTev"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.HTev") + " "
							// ~ + para + " " + Translator.swap
							// ("Validate.givenHTev"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.HTev") + " " + para + " "
									+ Translator.swap("Validate.givenHTev"));
						}
						if (pe.getInterEnvironmentalVariance() == -1) {
							// ~ if (gee instanceof SpeciesDefined) {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.species") + ((SpeciesDefined)
							// t).getSpecies ()
							// ~ + ", " + Translator.swap ("Validate.%Variance")
							// + " " + para + " " +
							// ~ Translator.swap ("Validate.given%Variance"));
							// ~ } else {
							// ~ throw new Exception("\n" + Translator.swap
							// ("Validate.%Variance") + " "
							// ~ + para + " " + Translator.swap
							// ("Validate.given%Variance"));
							// ~ }
							throw new Exception("\n" + Translator.swap("Validate.species") + t.getGenoSpecies() + ", "
									+ Translator.swap("Validate.%Variance") + " " + para + " "
									+ Translator.swap("Validate.given%Variance"));
						}
					}
				}
			}
		}
	}

}
