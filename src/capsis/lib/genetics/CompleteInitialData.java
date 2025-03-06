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

import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.Translator;

/**
 * This class contains methods that complete initial data if necessary. It is
 * used by Validate.validateInitialData.
 * 
 * @author I. Seynave - September 2002, F. de Coligny - November 2004
 */
public class CompleteInitialData {

	static {
		Translator.addBundle("capsis.lib.genetics.Validate");
	}

	/**
	 * This method tests if AlleleDiversity contains unknown allele for each
	 * locus and complete it if necessary. This method is used by
	 * Validate.validateInitialData () if default IndividualGenotype was
	 * computed.
	 */
	public static void completeAlleleDiversity(Genotypable gee, String warning) {
		short[][] nuclearAlleleDiversity = gee.getGenoSpecies().getAlleleDiversity().getNuclearAlleleDiversity();
		short[][] mCytoplasmicAlleleDiversity = gee.getGenoSpecies().getAlleleDiversity()
				.getMCytoplasmicAlleleDiversity();
		short[][] pCytoplasmicAlleleDiversity = gee.getGenoSpecies().getAlleleDiversity()
				.getPCytoplasmicAlleleDiversity();
		Set[] nuclearAlleleDiversitySet = new Set[nuclearAlleleDiversity.length];
		Set[] mCytoplasmicAlleleDiversitySet = new Set[mCytoplasmicAlleleDiversity.length];
		Set[] pCytoplasmicAlleleDiversitySet = new Set[pCytoplasmicAlleleDiversity.length];
		int test = 0;

		for (int j = 0; j < nuclearAlleleDiversity.length; j++) {
			nuclearAlleleDiversitySet[j] = new HashSet();
			for (int k = 0; k < nuclearAlleleDiversity[j].length; k++) {
				nuclearAlleleDiversitySet[j].add(new Integer(nuclearAlleleDiversity[j][k]));
			}
			if (!nuclearAlleleDiversitySet[j].contains(new Integer(-1))) {
				test = 1;
				ValidateTools.addValueInLineOfArray(nuclearAlleleDiversity, -1, j);
			}
		}
		for (int j = 0; j < mCytoplasmicAlleleDiversity.length; j++) {
			mCytoplasmicAlleleDiversitySet[j] = new HashSet();
			for (int k = 0; k < mCytoplasmicAlleleDiversity[j].length; k++) {
				mCytoplasmicAlleleDiversitySet[j].add(new Integer(mCytoplasmicAlleleDiversity[j][k]));
			}
			if (!mCytoplasmicAlleleDiversitySet[j].contains(new Integer(-1))) {
				test = 1;
				ValidateTools.addValueInLineOfArray(mCytoplasmicAlleleDiversity, -1, j);
			}
		}
		for (int j = 0; j < pCytoplasmicAlleleDiversity.length; j++) {
			pCytoplasmicAlleleDiversitySet[j] = new HashSet();
			for (int k = 0; k < pCytoplasmicAlleleDiversity[j].length; k++) {
				pCytoplasmicAlleleDiversitySet[j].add(new Integer(pCytoplasmicAlleleDiversity[j][k]));
			}
			if (!pCytoplasmicAlleleDiversitySet[j].contains(new Integer(-1))) {
				test = 1;
				ValidateTools.addValueInLineOfArray(pCytoplasmicAlleleDiversity, -1, j);
			}
		}
		if (test == 1) {
			// ~ if (gee instanceof SpeciesDefined) {
			// ~ warning = warning + "\n" + Translator.swap ("Validate.species")
			// + " " + ((SpeciesDefined) gee).getSpecies () +
			// ~ ", " + Translator.swap ("Validate.completeAD");
			// ~ } else {
			// ~ warning = warning + "\n" + Translator.swap
			// ("Validate.completeAD");
			// ~ }
			warning = warning + "\n" + Translator.swap("Validate.species") + " " + gee.getGenoSpecies() + ", "
					+ Translator.swap("Validate.completeAD");
		}
	}

	/**
	 * This method tests if MultiGenotype contains number of unknown allele for
	 * each locus and complete it if necessary. This method is used by
	 * Validate.validateInitialData () if default IndividualGenotype was
	 * computed.
	 */
	public static void completeMultiGenotype(Genotypable gee, short[][] nad, short[][] mad, short[][] pad,
			String warning) {
		MultiGenotype genotype = (MultiGenotype) gee.getGenotype();
		int[][] nuclearFrequence = genotype.getNuclearAlleleFrequency();
		int[][] mCytoplasmicFrequence = genotype.getMCytoplasmicAlleleFrequency();
		int[][] pCytoplasmicFrequence = genotype.getPCytoplasmicAlleleFrequency();
		int test = 0;

		if (nuclearFrequence.length > 0) {
			for (int j = 0; j < nuclearFrequence.length; j++) {
				if (nad[j].length > nuclearFrequence[j].length) {
					test = 1;
					ValidateTools.addValueInLineOfArray(nuclearFrequence, 0, j);
				}
			}
		}
		if (mCytoplasmicFrequence.length > 0) {
			for (int j = 0; j < mCytoplasmicFrequence.length; j++) {
				if (mad[j].length > mCytoplasmicFrequence[j].length) {
					test = 1;
					ValidateTools.addValueInLineOfArray(mCytoplasmicFrequence, 0, j);
				}
			}
		}
		if (pCytoplasmicFrequence.length > 0) {
			for (int j = 0; j < pCytoplasmicFrequence.length; j++) {
				if (pad[j].length > pCytoplasmicFrequence[j].length) {
					test = 1;
					ValidateTools.addValueInLineOfArray(pCytoplasmicFrequence, 0, j);
				}
			}
		}
		if (test == 1) {
			// ~ if (gee instanceof SpeciesDefined) {
			// ~ warning = warning + "\n" + Translator.swap ("Validate.species")
			// + " " + ((SpeciesDefined) gee).getSpecies () +
			// ~ ", " + Translator.swap ("Validate.completeMG");
			// ~ } else {
			// ~ warning = warning + "\n" + Translator.swap
			// ("Validate.completeMG");
			// ~ }
			warning = warning + "\n" + Translator.swap("Validate.species") + " " + gee.getGenoSpecies() + ", "
					+ Translator.swap("Validate.completeMG");
		}
	}

	/**
	 * This method tests if AlleleEffect contains effect of unknown allele and
	 * complete it if necessary. This method is used by
	 * Validate.validateInitialData () if default IndividualGenotype was
	 * computed.
	 */
	public static void completeAlleleEffect(Genotypable gee, short[][] nad, short[][] mad, short[][] pad, String warning) {
		AlleleEffect ae = gee.getGenoSpecies().getAlleleEffect();
		// fc+cg-31.5.2018 changed Set by list in line below (order now guaranted)
		List parameters = gee.getGenoSpecies().getAlleleEffect().getParameterNames();

		for (Iterator i = parameters.iterator(); i.hasNext();) {
			int test = 0;
			String para = (String) i.next();
			AlleleEffect.ParameterEffect pe = gee.getGenoSpecies().getAlleleEffect().getParameterEffect(para);
			short[][] nuclearAlleleEffect = pe.getNuclearAlleleEffect();
			short[][] mCytoplasmicAlleleEffect = pe.getMCytoplasmicAlleleEffect();
			short[][] pCytoplasmicAlleleEffect = pe.getPCytoplasmicAlleleEffect();
			if (nuclearAlleleEffect.length > 0) {
				for (int j = 0; j < nuclearAlleleEffect.length; j++) {
					int rang = nuclearAlleleEffect[j][0];
					if (nad[rang - 1].length == nuclearAlleleEffect[j].length) {
						test = 1;
						ValidateTools.addValueInLineOfArray(nuclearAlleleEffect, 0, j);
					}
				}
			}
			if (mCytoplasmicAlleleEffect.length > 0) {
				for (int j = 0; j < mCytoplasmicAlleleEffect.length; j++) {
					int rang = mCytoplasmicAlleleEffect[j][0];
					if (mad[rang - 1].length == mCytoplasmicAlleleEffect[j].length) {
						test = 1;
						ValidateTools.addValueInLineOfArray(mCytoplasmicAlleleEffect, 0, j);
					}
				}
			}
			if (pCytoplasmicAlleleEffect.length > 0) {
				for (int j = 0; j < pCytoplasmicAlleleEffect.length; j++) {
					int rang = pCytoplasmicAlleleEffect[j][0];
					if (pad[rang - 1].length == pCytoplasmicAlleleEffect[j].length) {
						ValidateTools.addValueInLineOfArray(pCytoplasmicAlleleEffect, 0, j);
						test = 1;
					}
				}
			}
			if (test == 1) {
				// ~ if (gee instanceof SpeciesDefined) {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.species") + " " + ((SpeciesDefined)
				// gee).getSpecies () +
				// ~ ", " + Translator.swap ("Validate.completeAE") + " " +
				// para;
				// ~ } else {
				// ~ warning = warning + "\n" + Translator.swap
				// ("Validate.completeMG") + " "+ para;
				// ~ }
				warning = warning + "\n" + Translator.swap("Validate.species") + " " + gee.getGenoSpecies() + ", "
						+ Translator.swap("Validate.completeAE") + " " + para;
			}
		}
	}

}
