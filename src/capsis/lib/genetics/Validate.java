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
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import jeeb.lib.util.Alert;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * This class review all initial genetic data to test that all Genetics methods
 * can be applied. It must be applied in module after data loading.
 * 
 * @author I. Seynave - September 2002, F. de Coligny - November 2004
 */
public class Validate {

	static {
		Translator.addBundle("capsis.lib.genetics.Validate");
	}

	/**
	 * This method does several validations on initial genetic data.
	 */
	public static void validateInitialData(GeneticScene initScene) throws Exception {

		// fc+hd+nb-12.12.2016
		if (initScene.getGenotypables() == null || initScene.getGenotypables().isEmpty())
			throw new Exception("in Validate: no genotypable in genotypable list: " + initScene.getGenotypables());

		Object _t = null;
		Iterator _i = initScene.getGenotypables().iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		Map speciesIndividualWithGenotype = new Hashtable();
		Map speciesIndividualWithEmptyGenotype = new Hashtable();
		Map speciesPopulationWithGenotype = new Hashtable();
		Map speciesPopulationWithEmptyGenotype = new Hashtable();
		Map speciesWithoutGenotype = new Hashtable();
		Set speciesWithGenotype = new HashSet();
		Set completedIndividualGenotype = new HashSet();
		String warning = " ";

		// 0- build map species

		// build five species map :
		// - individual gee with genotype
		// - individual gee with empty genotype
		// - population with genotype
		// - population with empty genotype
		// - population and individual gee without genotype (i.d. genotype =
		// null)
		// In this map, the key are species name and values are set of
		// Genotypable.

		// fc - 10.11.2004 - all genotypables now have a getGenoSpecies ()
		// method
		// ~ if (gee instanceof SpeciesDefined) {

		for (Iterator i = initScene.getGenotypables().iterator(); i.hasNext();) {

			_t = i.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			Genotypable t = (Genotypable) _t; // phd 2003_03_17

			// ~ SpeciesDefined sd = (SpeciesDefined) t;
			// ~ QualitativeProperty esp = sd.getSpecies ();
			GenoSpecies esp = t.getGenoSpecies(); // fc - 10.11.2004 - all
													// genotypables now have a
													// getGenoSpecies () method

			if (t.getGenotype() == null) {
				ValidateTools.buildMapSpecies(speciesWithoutGenotype, esp, t);
			} else {
				speciesWithGenotype.add(esp);
				if (t.getGenotype() instanceof IndividualGenotype) {
					if ((t.getGenotype()).equals(Genotype.EMPTY_INDIVIDUAL_GENOTYPE)) {
						ValidateTools.buildMapSpecies(speciesIndividualWithEmptyGenotype, esp, t);
					} else {
						ValidateTools.buildMapSpecies(speciesIndividualWithGenotype, esp, t);
					}
				} else {
					if ((t.getGenotype()).equals(Genotype.EMPTY_MULTI_GENOTYPE)) {
						ValidateTools.buildMapSpecies(speciesPopulationWithEmptyGenotype, esp, t);
					} else {
						ValidateTools.buildMapSpecies(speciesPopulationWithGenotype, esp, t);
					}
				}
			}
		}

		// ~ } else {
		// ~ for (Iterator i = initScene.getGenotypables ().iterator ();
		// i.hasNext ();) {

		// ~ _t = i.next ();
		// ~ if (!(_t instanceof Genotypable)) {continue;}
		// ~ Genotypable t = (Genotypable) _t; // phd 2003_03_17

		// ~ speciesWithGenotype. add ("one");
		// ~ if (t.getGenotype () == null) {
		// ~ ValidateTools.buildMapSpecies (speciesWithoutGenotype,
		// "WithoutGenotype", t);
		// ~ } else if (t.getGenotype () instanceof MultiGenotype) {
		// ~ if ((t.getGenotype ()).equals (Genotype.EMPTY_MULTI_GENOTYPE)) {
		// ~ ValidateTools.buildMapSpecies (speciesPopulationWithEmptyGenotype,
		// "PopulationWithEmptyGenotype", t);
		// ~ } else {
		// ~ ValidateTools.buildMapSpecies (speciesPopulationWithGenotype,
		// "PopulationWithGenotype", t);
		// ~ }
		// ~ } else {
		// ~ if ((t.getGenotype ()).equals (Genotype.EMPTY_INDIVIDUAL_GENOTYPE))
		// {
		// ~ ValidateTools.buildMapSpecies (speciesIndividualWithEmptyGenotype,
		// "IndividualTreeWithEmptyGenotype", t);
		// ~ } else {
		// ~ ValidateTools.buildMapSpecies (speciesIndividualWithGenotype,
		// "IndividualTreeWithGenotype", t);
		// ~ }
		// ~ }
		// ~ }
		// ~ }

		// stop if, for a same species, there are genotypables with genotype
		// (genotype is defined or empty)
		// and also genotypables without genotype (genotype = null),

		// fc - 10.11.2004 - all genotypables now have a getGenoSpecies ()
		// method
		// ~ if (gee instanceof SpeciesDefined) {

		Set notGenotypedSpecies = speciesWithoutGenotype.keySet();
		for (Iterator i = notGenotypedSpecies.iterator(); i.hasNext();) {
			GenoSpecies species = (GenoSpecies) i.next();
			if (speciesIndividualWithGenotype.keySet().contains(species)
					|| speciesIndividualWithEmptyGenotype.keySet().contains(species)
					|| speciesPopulationWithGenotype.keySet().contains(species)
					|| speciesPopulationWithEmptyGenotype.keySet().contains(species)) {
				throw new Exception("in Validate : for species " + species
						+ ", some geenotypables have genotype equal to null");
			}
		}

		// ~ }

		// PICHOT october 03

		// 0- (re)build allele number in multigenotype ensuring that total
		// number of alleles per locus fits with the number of individuals in
		// the population (.getNumber())

		for (Iterator i = speciesPopulationWithGenotype.values().iterator(); i.hasNext();) {
			Collection geesWithSameSpecies = (Collection) i.next();
			for (Iterator ii = geesWithSameSpecies.iterator(); ii.hasNext();) {
				Genotypable t = (Genotypable) ii.next();
				// System.out.println ("Rebuild genotype for id= "+t.getId
				// ()+" number of genotypables =  "+t.getNumber ());
				t.getMultiGenotype().rebuildMultiGenotype(t);
			}
		}
		// PICHOT october 03 end

		// 1- test if initial data are valid

		AreGeneticDataCompatible.testIfInitialDataNotValid(gee, speciesIndividualWithGenotype,
				speciesIndividualWithEmptyGenotype, speciesPopulationWithGenotype, speciesPopulationWithEmptyGenotype,
				speciesWithGenotype);

		// 2- calculate default AlleleDiversity

		// if alleleDiversity is not defined, calculate a default
		// alleleDiversity
		// this can only be done for species for which there are individual
		// gees.
		// for other species, there is at least one population and so
		// alleleDiversity is defined (else error).
		// after this, alleleDiversity is defined for all species studied on
		// genetic level.
		for (Iterator i = speciesIndividualWithGenotype.values().iterator(); i.hasNext();) {
			Collection geesWithSameSpecies = (Collection) i.next();
			Genotypable t = (Genotypable) geesWithSameSpecies.iterator().next();
			if (t.getGenoSpecies().getAlleleDiversity() == null || t.getGenoSpecies().getAlleleDiversity().isEmpty()) {
				AlleleDiversity.computeAlleleDiversity(geesWithSameSpecies);
				// ~ if (t instanceof SpeciesDefined) {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.species") + " " + ((SpeciesDefined) t).getSpecies
				// () +
				// ~ ", " + Translator.swap ("Validate.defaultAD");
				// ~ } else {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.defaultAD");
				// ~ }
				warning = warning + "\n" + Translator.swap("Validate.species") + " " + t.getGenoSpecies() + ", "
						+ Translator.swap("Validate.defaultAD");
			}
		}

		// 3- calculate default IndividualGenotype

		// for species studied on genetic level, complete IndividualGenotype of
		// genotypable with empty genotype
		for (Iterator i = speciesIndividualWithEmptyGenotype.keySet().iterator(); i.hasNext();) {
			Collection geesWithEmptyGenotype;
			Collection geesWithGenotype;

			// fc - 10.11.2004 - all genotypables now have a getGenoSpecies ()
			// method
			// ~ if (gee instanceof SpeciesDefined) {

			GenoSpecies species = (GenoSpecies) i.next();
			geesWithEmptyGenotype = (Collection) speciesIndividualWithEmptyGenotype.get(species);
			completedIndividualGenotype.add(species);
			if (speciesIndividualWithGenotype.keySet().contains(species)) {
				geesWithGenotype = (Collection) speciesIndividualWithGenotype.get(species);
			} else {
				geesWithGenotype = new HashSet();
				speciesIndividualWithGenotype.put(species, geesWithGenotype);
			}

			// ~ } else {
			// ~ completedIndividualGenotype.add ("true");
			// ~ geesWithEmptyGenotype = (Collection)
			// speciesIndividualWithEmptyGenotype.get
			// ("IndividualTreeWithEmptyGenotype");
			// ~ if (! speciesIndividualWithGenotype.isEmpty ()) {
			// ~ geesWithGenotype = (Collection)
			// speciesIndividualWithGenotype.get ("IndividualTreeWithGenotype");
			// ~ } else {
			// ~ geesWithGenotype = new HashSet ();
			// ~ speciesIndividualWithGenotype.put
			// ("IndividualTreeWithGenotype", geesWithGenotype);
			// ~ }
			// ~ }

			Genotypable t = (Genotypable) geesWithEmptyGenotype.iterator().next();
			AlleleDiversity ad = t.getGenoSpecies().getAlleleDiversity();
			short[][] nuclearAlleleDiversity = ad.getNuclearAlleleDiversity();
			short[][] mCytoplasmicAlleleDiversity = ad.getMCytoplasmicAlleleDiversity();
			short[][] pCytoplasmicAlleleDiversity = ad.getPCytoplasmicAlleleDiversity();
			short[][] nuclearDNA = new short[nuclearAlleleDiversity.length][2];
			short[] mCytoplasmicDNA = new short[mCytoplasmicAlleleDiversity.length];
			short[] pCytoplasmicDNA = new short[pCytoplasmicAlleleDiversity.length];
			for (int j = 0; j < nuclearDNA.length; j++) {
				nuclearDNA[j][0] = -1;
				nuclearDNA[j][1] = -1;
			}
			for (int j = 0; j < mCytoplasmicDNA.length; j++) {
				mCytoplasmicDNA[j] = -1;
			}
			for (int j = 0; j < pCytoplasmicDNA.length; j++) {
				pCytoplasmicDNA[j] = -1;
			}
			IndividualGenotype ig = new IndividualGenotype(nuclearDNA, mCytoplasmicDNA, pCytoplasmicDNA);
			t.setIndividualGenotype(ig);
			geesWithGenotype.add(t);
			Iterator ite = geesWithEmptyGenotype.iterator();
			Genotypable s = (Genotypable) ite.next();
			while (ite.hasNext()) {
				Genotypable tr = (Genotypable) ite.next();
				tr.setIndividualGenotype(ig);
				geesWithGenotype.add(tr);
			}
			// ~ if (gee instanceof SpeciesDefined) {
			// ~ Log.println (geesWithEmptyGenotype.size () +
			// " individual gee of species " + ((SpeciesDefined) t).getSpecies
			// ()
			// ~ +
			// " have empty genotype in initial data. A default IndividualGenotype has been calculated");
			// ~ warning = warning + "\n" + Translator.swap ("Validate.species")
			// + " " + ((SpeciesDefined) t).getSpecies () +
			// ~ ", " + Translator.swap ("Validate.defaultIG");
			// ~ } else {
			// ~ Log.println (geesWithEmptyGenotype.size ()
			// ~ +
			// " individual genotypable has empty genotype in initial data. A default IndividualGenotype has been calculated");
			// ~ warning = warning + "\n" + Translator.swap
			// ("Validate.defaultIG");
			// ~ }
			Log.println(geesWithEmptyGenotype.size() + " individual genotypable of species " + t.getGenoSpecies()
					+ " have empty genotype in initial data. A default IndividualGenotype has been calculated");
			warning = warning + "\n" + Translator.swap("Validate.species") + " " + t.getGenoSpecies() + ", "
					+ Translator.swap("Validate.defaultIG");
		} // after this, all individual genotypables have an IndividualGenotype
			// not empty and are in speciesIndividualWithGenotype

		// 4- if default IndividualGenotype have been calculated...

		for (Iterator i = completedIndividualGenotype.iterator(); i.hasNext();) {
			// verify that alleleDiversity contains unknown allele and complete
			// it if needed
			Collection geesWithSameSpecies;
			Collection populationsWithSameSpecies = new HashSet();

			// fc - 10.11.2004 - all genotypables now have a getGenoSpecies ()
			// method
			// ~ if (gee instanceof SpeciesDefined) {

			// ~ QualitativeProperty species = (QualitativeProperty) i.next ();
			// // fc - 10.1.2005
			GenoSpecies species = (GenoSpecies) i.next(); // fc - 10.1.2005
			geesWithSameSpecies = (Collection) speciesIndividualWithGenotype.get(species);
			if ((speciesPopulationWithGenotype.keySet()).contains(species)) {
				populationsWithSameSpecies = (Collection) speciesPopulationWithGenotype.get(species);
			}

			// ~ } else {
			// ~ geesWithSameSpecies = (Collection)
			// speciesIndividualWithGenotype.get ("IndividualTreeWithGenotype");
			// ~ if (! speciesPopulationWithGenotype.isEmpty ()) {
			// ~ populationsWithSameSpecies = (Collection)
			// speciesPopulationWithGenotype.get ("PopulationWithGenotype");
			// ~ }
			// ~ }
			Genotypable t = (Genotypable) geesWithSameSpecies.iterator().next();
			CompleteInitialData.completeAlleleDiversity(t, warning);

			// verify that alleleFrequency contains frequency of unknown allele
			// and if needed complete multiGenotype with frequence of allele -1
			// equal to 0
			if (populationsWithSameSpecies.size() > 0) {
				Genotypable pop = (Genotypable) populationsWithSameSpecies.iterator().next();
				AlleleDiversity ad = pop.getGenoSpecies().getAlleleDiversity();
				short[][] nad = ad.getNuclearAlleleDiversity();
				short[][] mad = ad.getMCytoplasmicAlleleDiversity();
				short[][] pad = ad.getPCytoplasmicAlleleDiversity();
				for (Iterator ite = populationsWithSameSpecies.iterator(); ite.hasNext();) {
					Genotypable tr = (Genotypable) ite.next();
					CompleteInitialData.completeMultiGenotype(tr, nad, mad, pad, warning);
				}
			}

			// verify that alleleEffect contains the effect of unknown allele
			// and if needed complete alleleEffect with effect equal to 0
			if (t.getGenoSpecies().getAlleleEffect() != null
					&& !(t.getGenoSpecies().getAlleleEffect().getParameterNames()).isEmpty()) {
				short[][] nad = t.getGenoSpecies().getAlleleDiversity().getNuclearAlleleDiversity();
				short[][] mad = t.getGenoSpecies().getAlleleDiversity().getMCytoplasmicAlleleDiversity();
				short[][] pad = t.getGenoSpecies().getAlleleDiversity().getPCytoplasmicAlleleDiversity();
				CompleteInitialData.completeAlleleEffect(t, nad, mad, pad, warning);
			}
		}

		// 5- calculate default MultiGenotype

		// for species studied on genetic level, calculate MultiGenotype of
		// population with empty Genotype
		// from collection of genotypables with IndividualGenotype and
		// MultiGenotype
		// by default, multiGenotype equal allele frequencies of all
		// genotypables (individual genotypables and populations with genotype)
		if (!speciesPopulationWithEmptyGenotype.isEmpty()) {
			for (Iterator i = speciesPopulationWithEmptyGenotype.values().iterator(); i.hasNext();) {
				Collection populationsWithEmptyGenotype = (Collection) i.next();
				Genotypable t = (Genotypable) populationsWithEmptyGenotype.iterator().next();
				Collection geesWithGenotype = new HashSet();
				Collection populationsWithGenotype = new HashSet();

				// fc - 10.11.2004 - all genotypables now have a getGenoSpecies
				// () method
				// ~ if (gee instanceof SpeciesDefined) {

				// ~ SpeciesDefined sd = (SpeciesDefined) t;
				// ~ QualitativeProperty esp = sd.getSpecies ();
				GenoSpecies esp = t.getGenoSpecies();

				if (speciesIndividualWithGenotype.containsKey(esp)) {
					geesWithGenotype.addAll((Collection) speciesIndividualWithGenotype.get(esp));
				}
				if (speciesPopulationWithGenotype.containsKey(esp)) {
					populationsWithGenotype = (Collection) speciesPopulationWithGenotype.get(esp);
					geesWithGenotype.addAll(populationsWithGenotype);
				} else {
					speciesPopulationWithGenotype.put(esp, populationsWithGenotype);
				}

				// ~ } else {
				// ~ geesWithGenotype.addAll ((Collection)
				// speciesIndividualWithGenotype.get("IndividualTreeWithGenotype"));
				// ~ if (! speciesPopulationWithGenotype.isEmpty ()) {
				// ~ populationsWithGenotype = (Collection)
				// speciesPopulationWithGenotype.get("PopulationWithGenotype");
				// ~ geesWithGenotype.addAll (populationsWithGenotype);
				// ~ } else {
				// ~ speciesPopulationWithGenotype.put
				// ("PopulationWithGenotype", populationsWithGenotype);
				// ~ }
				// ~ }

				MultiGenotype.computeDefaultMultiGenotype(geesWithGenotype, populationsWithEmptyGenotype);
				// ~ if (t instanceof SpeciesDefined) {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.species") + " " + ((SpeciesDefined) t).getSpecies
				// () +
				// ~ ", " + Translator.swap ("Validate.defaultMG");
				// ~ } else {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.defaultMG");
				// ~ }
				warning = warning + "\n" + Translator.swap("Validate.species") + " " + t.getGenoSpecies() + ", "
						+ Translator.swap("Validate.defaultMG");
				populationsWithGenotype.addAll(populationsWithEmptyGenotype);
			}
		} // after this, all populations have MultiGenotype not empty and are in
			// speciesPopulationWithGenotype

		// 6- calculate default GeneticMap and complete AlleleEffect

		for (Iterator i = speciesWithGenotype.iterator(); i.hasNext();) {
			Collection gees = new HashSet();

			// fc - 10.11.2004 - all genotypables now have a getGenoSpecies ()
			// method
			// ~ if (gee instanceof SpeciesDefined) {

			GenoSpecies species = (GenoSpecies) i.next();
			if (speciesIndividualWithGenotype.keySet().contains(species)) {
				gees = (Collection) speciesIndividualWithGenotype.get(species);
			}
			if (speciesPopulationWithGenotype.keySet().contains(species)) {
				gees.addAll((Collection) speciesPopulationWithGenotype.get(species));
			}

			// ~ } else {
			// ~ gees = (Collection) speciesIndividualWithGenotype.get
			// ("IndividualTreeWithGenotype");
			// ~ gees.addAll ((Collection) speciesPopulationWithGenotype.get
			// ("PopulationWithGenotype"));
			// ~ }

			Genotypable t = (Genotypable) gees.iterator().next();

			// if geneticMap is not defined, calculate a geneticMap
			// by default, all recombination propabilities are equal to 0.5
			if (t.getGenoSpecies().getGeneticMap() == null
					|| t.getGenoSpecies().getGeneticMap().getRecombinationProbas() == null
					|| (t.getGenoSpecies().getGeneticMap().getRecombinationProbas()).length == 0) {
				GeneticMap.computeDefautlRecombinationProbas(t);
				// ~ if (t instanceof SpeciesDefined) {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.species") + " "+ ((SpeciesDefined) t).getSpecies
				// () +
				// ~ ", " + Translator.swap ("Validate.defaultGeneticMap");
				// ~ } else {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.defaultGeneticMap");
				// ~ }
				warning = warning + "\n" + Translator.swap("Validate.species") + " " + t.getGenoSpecies() + ", "
						+ Translator.swap("Validate.defaultGeneticMap");
			}

			// compute heritability (respectively environmental variance) from
			// environmental variance (respectively heritability)
			// if it is not given in load file (ie = -1)
			AlleleEffect ae = t.getGenoSpecies().getAlleleEffect();
			if (ae != null) {
				for (Iterator ite = ae.getParameterNames().iterator(); ite.hasNext();) {
					String parameterName = (String) ite.next();
					ae.getHeritability(gees, parameterName);
					ae.getTotalEnvironmentalVariance(gees, parameterName);
					// ~ if (t instanceof SpeciesDefined) {
					// ~ warning = warning + "\n" + Translator.swap
					// ("Validate.species") + " " + ((SpeciesDefined)
					// t).getSpecies () +
					// ~ ", " + Translator.swap ("Validate.completeAE");
					// ~ } else {
					// ~ warning = warning + "\n" + Translator.swap
					// ("Validate.completeAE");
					// ~ }
					warning = warning + "\n" + Translator.swap("Validate.species") + " " + t.getGenoSpecies() + ", "
							+ Translator.swap("Validate.completeAE")+" "+parameterName; // fc+cg-31.5.2018 added parameterName
				}
			}
		}

		if (!warning.equals(" ")) {
			warning = (warning + "\n").trim();
			StringBuffer message = new StringBuffer();
			for (StringTokenizer st = new StringTokenizer(warning, "\n"); st.hasMoreTokens();) {
				StringBuffer line = new StringBuffer(st.nextToken().trim());
				line.setCharAt(0, Character.toUpperCase(line.charAt(0)));
				if (line.charAt(line.length() - 1) != '.') {
					line.append('.');
				}
				line.append("\n");
				message.append(line);
			}

			// fc - 8.10.2004 - Alert can be used either in gui or script mode
			// MessageDialog makes an error in script -> replaced by Alert
			// ~ MessageDialog.promptInfo (message.toString ());
			Alert.print(message.toString());
		}
	}

}
