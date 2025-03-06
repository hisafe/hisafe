/*
 * The Genetics library for Capsis4
 * 
 * Copyright (C) 2002-2004 Ingrid Seynave, Christian Pichot
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.lib.genetics;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.TreeSet;

import jeeb.lib.util.Log;

/**
 * Allele effect contains for each species and each parameter : (i) the effect
 * values of all potential alleles for the loci who influence this parameter.
 * (ii) the parameter heritability. These data are loaded from a file.
 * 
 * @author I. Seynave - July 2002, F. de Coligny - November 2004
 */
public class AlleleEffect implements Serializable, Cloneable {

	private Map effect;
	private static Random random = new Random();

	/**
	 * This inner class defines the values contained in map effect.
	 */
	public class ParameterEffect implements Serializable {

		// Allows to use real values (not only entire) of allele effects. nb-09.07.2018
		private double effectCoefficient;
		
		// The first element of each line is the locus number. Locus Number is
		// in [1, nbLocus] (fc+som-7.7.2017).
		// Next elements are the effect values
		// for each potential allele.
		// The effect values are in the same order than the potential alleles in
		// AlleleDiversity.alleleDiversity.
		private short[][] nuclearAlleleEffect;
		private short[][] mCytoplasmicAlleleEffect;
		private short[][] pCytoplasmicAlleleEffect;
		private double heritability;
		private double totalEnvironmentalVariance;
		private double interEnvironmentalVariance;

		/**
		 * Constructor for new ParameterEffect.
		 */
		public ParameterEffect(short[][] na, short[][] ma, short[][] pa, double h2, double ev, double interEV) {
			nuclearAlleleEffect = na;
			mCytoplasmicAlleleEffect = ma;
			pCytoplasmicAlleleEffect = pa;
			heritability = h2;
			totalEnvironmentalVariance = ev;
			interEnvironmentalVariance = interEV;
			
			// Default value for modules that use entire values of allele effects (no effect coefficient value 
			// in their inventories files. nb-09.07.2018
			effectCoefficient = 1.0;
		}

		/**
		 * Returns a copy of a ParameterEffect, added with J. Labonne for
		 * mutation, see GenoSpecies.clone (). fc-30.8.2013
		 */
		public ParameterEffect getCopy() {

			ParameterEffect copy = new ParameterEffect(copy(nuclearAlleleEffect), copy(mCytoplasmicAlleleEffect),
					copy(pCytoplasmicAlleleEffect), heritability, totalEnvironmentalVariance,
					interEnvironmentalVariance);

			copy.setEffectCoefficient(effectCoefficient); // nb-09.07.2018
			
			return copy;
		}

		// User method to copy short arrays
		private short[][] copy(short[][] tab) {
			short[][] copy = new short[tab.length][];
			for (int i = 0; i < tab.length; i++) {
				copy[i] = new short[tab[i].length];
				for (int j = 0; j < tab[i].length; j++) {
					copy[i][j] = tab[i][j];
				}
			}
			return copy;
		}

		// nb-09.07.2018
		public double getEffectCoefficient() {
			return effectCoefficient;
		}
		
		public short[][] getNuclearAlleleEffect() {
			return nuclearAlleleEffect;
		}

		public short[][] getMCytoplasmicAlleleEffect() {
			return mCytoplasmicAlleleEffect;
		}

		public short[][] getPCytoplasmicAlleleEffect() {
			return pCytoplasmicAlleleEffect;
		}

		public double getHeritability() {
			return heritability;
		}

		public double getTotalEnvironmentalVariance() {
			return totalEnvironmentalVariance;
		}

		public double getInterEnvironmentalVariance() {
			return interEnvironmentalVariance;
		}

		// nb-09.07.2018
		public void setEffectCoefficient(double effectCoefficient) {
			this.effectCoefficient = effectCoefficient;
		}
		
		private void setHeritability(double h2) {
			heritability = h2;
		}

		private void setTotalEnvironmentalVariance(double ev) {
			totalEnvironmentalVariance = ev;
		}
	}

	/**
	 * Constructor for new AlleleEffect.
	 */
	public AlleleEffect() {
		effect = new Hashtable();
	}

	/**
	 * Clones an AlleleEffect, added with J. Labonne for mutation, see
	 * GenoSpecies.clone (). fc-30.8.2013
	 */
	public Object clone() {
		try {
			AlleleEffect clone = (AlleleEffect) super.clone();

			// Clone the Map
			clone.effect = new Hashtable();

			// Clone the map content
			for (Iterator i = effect.keySet().iterator(); i.hasNext();) {
				String parameterName = (String) i.next();
				ParameterEffect parameterEffect = (ParameterEffect) effect.get(parameterName);

				clone.effect.put(parameterName, parameterEffect.getCopy());
			}

			return clone;

		} catch (Exception e) {
			Log.println(Log.ERROR, "AlleleEffect.clone ()", "Trouble while cloning an AlleleEffect instance", e);
			return null;
		}
	}

	public ParameterEffect getParameterEffect(String parameterName) {
		return (ParameterEffect) effect.get(parameterName);
	}

	/**
	 * Remove an item (key and this value) in an AlleleEffect
	 */
	public void removeParameterEffect(String parameterName) {
		effect.remove(parameterName);
	}

	/**
	 * Return the key Set in a list, i.e. the list of parameter names, for a given
	 * AlleleEffect, always in the same order.
	 */
	public List getParameterNames() {
		// fc+cg-31.5.2018 now returns a list with effects always in the same order
		return new ArrayList(new TreeSet(effect.keySet())); // sort to guaranty order
	}

	/**
	 * This method adds a new item, i.e. the parameter name and his
	 * ParameterEffect, in a AlleleEffect.
	 */
	public void addParameterEffect(String name, short[][] na, short[][] ma, short[][] pa, double h2, double ve,
			double interEV) {
		ParameterEffect pe = new ParameterEffect(na, ma, pa, h2, ve, interEV);
		effect.put(name, pe);
	}

	/**
	 * Test if an AlleleEffect is empty, i.e. contains no keys.
	 */
	public boolean isEmpty() {
		if ((effect.keySet()).isEmpty()) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Return the theoretic heritability of a parameter from a collection of
	 * genotypables and the parameter name. This method is used by
	 * Validate.completeInitialData () when the heritability of a parameter is
	 * not given in the load file.
	 */
	public void getHeritability(Collection gees, String parameterName) {
		ParameterEffect pe = (ParameterEffect) effect.get(parameterName);
		double h2 = pe.getHeritability();
		if (h2 == -1) {
			// calculate heritability if it is not defined in load file
			double ev = pe.getTotalEnvironmentalVariance();
			double gv = GeneticTools.computePanmicticGeneticVariance(gees, parameterName);
			double heritability = (double) (gv / (gv + ev));
			pe.setHeritability(heritability);
		}
	}

	/**
	 * Return total environmental variance of a parameter from a collection of
	 * genotypables and the parameter name. This method is used by
	 * Validate.completeInitialData () when the total environmental variance of
	 * a parameter is not given in the load file.
	 */
	public void getTotalEnvironmentalVariance(Collection gees, String parameterName) {
		ParameterEffect pe = (ParameterEffect) effect.get(parameterName);
		double ev = pe.getTotalEnvironmentalVariance();
		if (ev == -1) {
			double h2 = pe.getHeritability();
			double gv = GeneticTools.computePanmicticGeneticVariance(gees, parameterName);
			double totalEnvironmentalVariance = (1 - h2) * gv / h2;
			pe.setTotalEnvironmentalVariance(totalEnvironmentalVariance);
		}
	}

	/**
	 * Return fixed environmental value of a parameter.
	 */
	public double getFixedEnvironmentalValue(String parameterName) {
		double totalEV = ((ParameterEffect) (effect.get(parameterName))).getTotalEnvironmentalVariance();
		double interEV = ((ParameterEffect) (effect.get(parameterName))).getInterEnvironmentalVariance();
		double intraEV = random.nextGaussian() * Math.sqrt((1 - interEV) * totalEV);
		return intraEV;
	}

	/**
	 * Compute the genetic value of a genotypable with IndividualGenoype or
	 * MultiGenotype.
	 */
	public double getGeneticValue(String parameterName, Genotypable gee) {
		Genotype genotype = gee.getGenotype();
		AlleleDiversity ad = gee.getGenoSpecies().getAlleleDiversity();
		ParameterEffect pe = (ParameterEffect) effect.get(parameterName);
		if (!genotype.equals(Genotype.EMPTY_INDIVIDUAL_GENOTYPE) && !ad.isEmpty()) {
			// calculte the genetic value if genotype of genotypable is an
			// IndividualGenotype.
			if (genotype instanceof IndividualGenotype) {
				IndividualGenotype g = (IndividualGenotype) genotype;
				double geneticValue = 0;

				// sum effect value in nuclearDNA.
				short[][] nuclearDNA = g.getNuclearDNA();
				short[][] potentialAllele = ad.getNuclearAlleleDiversity();
				short[][] effectValue = pe.getNuclearAlleleEffect();
				
				for (int i = 0; i < effectValue.length; i++) {
					int locusNumber = effectValue[i][0];
					for (int j = 0; j < nuclearDNA[locusNumber - 1].length; j++) {
						int allele = nuclearDNA[locusNumber - 1][j];
						int rang = 0;
						while (allele != potentialAllele[locusNumber - 1][rang]) {
							rang = rang + 1;
						}
						// Multiplies the effect value by the effect coefficient to manage real values of 
						// allele effects. nb-09.07.2018.
						geneticValue = geneticValue + effectValue[i][rang + 1] * pe.getEffectCoefficient();
					}
				}

				// sum effect value in MCytoplamicDNA.
				short[] mCytoplasmicDNA = g.getMCytoplasmicDNA();
				short[][] mPotentialAllele = ad.getMCytoplasmicAlleleDiversity();
				short[][] mEffectValue = pe.getMCytoplasmicAlleleEffect();
				for (int i = 0; i < mEffectValue.length; i++) {
					int locusNumber = mEffectValue[i][0];
					int allele = mCytoplasmicDNA[locusNumber - 1];
					int rang = 0;
					while (allele != mPotentialAllele[locusNumber - 1][rang]) {
						rang = rang + 1;
					}
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticValue = geneticValue + mEffectValue[i][rang + 1] * pe.getEffectCoefficient();
				}

				// sum effect value in PCytoplamicDNA.
				short[] pCytoplasmicDNA = g.getPCytoplasmicDNA();
				short[][] pPotentialAllele = ad.getPCytoplasmicAlleleDiversity();
				short[][] pEffectValue = pe.getPCytoplasmicAlleleEffect();
				for (int i = 0; i < pEffectValue.length; i++) {
					int locusNumber = pEffectValue[i][0];
					int allele = pCytoplasmicDNA[locusNumber - 1];
					int rang = 0;
					while (allele != pPotentialAllele[locusNumber - 1][rang]) {
						rang = rang + 1;
					}
					// Multiplies the effect value by the effect coefficient to manage real values of 
					// allele effects. nb-09.07.2018.
					geneticValue = geneticValue + pEffectValue[i][rang + 1] * pe.getEffectCoefficient();
				}
				return geneticValue;

				// Calculate the genetic value if genotype of genotypable is a MultiGenotype.
			} else {
				MultiGenotype g = (MultiGenotype) genotype;
				double geneticValue = 0;

				// sum effect value in nuclearAlleleFrequency.
				int[][] nuclearAlleleFrequency = g.getNuclearAlleleFrequency();
				short[][] effectValue = pe.getNuclearAlleleEffect();
				for (int i = 0; i < effectValue.length; i++) {
					int positionLocus = effectValue[i][0];
					for (int j = 1; j < effectValue[i].length; j++) {
						// Multiplies the effect value by the effect coefficient to manage real values of 
						// allele effects. nb-09.07.2018.
						geneticValue = geneticValue + effectValue[i][j] * pe.getEffectCoefficient()
								* nuclearAlleleFrequency[positionLocus - 1][j - 1] / 2 / gee.getNumber();
					}
				}

				// sum effect value in MCytoplamicAlleleFrequency.
				int[][] mCytoplasmicAlleleFrequency = g.getMCytoplasmicAlleleFrequency();
				short[][] mEffectValue = pe.getMCytoplasmicAlleleEffect();
				for (int i = 0; i < mEffectValue.length; i++) {
					int positionLocus = mEffectValue[i][0];
					for (int j = 1; j < mEffectValue[i].length; j++) {
						// Multiplies the effect value by the effect coefficient to manage real values of 
						// allele effects. nb-09.07.2018.
						geneticValue = geneticValue + mEffectValue[i][j] * pe.getEffectCoefficient()
								* mCytoplasmicAlleleFrequency[positionLocus - 1][j - 1] / gee.getNumber();
					}
				}

				// sum effect value in PCytoplamicAlleleFrequency.
				int[][] pCytoplasmicAlleleFrequency = g.getPCytoplasmicAlleleFrequency();
				short[][] pEffectValue = pe.getPCytoplasmicAlleleEffect();
				for (int i = 0; i < pEffectValue.length; i++) {
					int positionLocus = pEffectValue[i][0];
					for (int j = 1; j < pEffectValue[i].length; j++) {
						// Multiplies the effect value by the effect coefficient to manage real values of 
						// allele effects. nb-09.07.2018.
						geneticValue = geneticValue + pEffectValue[i][j] * pe.getEffectCoefficient()
								* pCytoplasmicAlleleFrequency[positionLocus - 1][j - 1] / gee.getNumber();
					}
				}
				return geneticValue;
			}
		} else {
			Log.println("the genetic value of genotypable " + gee.getId() + " can not be calculated");
			return -1;
		}
	}

	/**
	 * Return phenotypic value of a genotypable
	 */
	public double getPhenoValue(String parameterName, Genotypable gee) {
		ParameterEffect pe = (ParameterEffect) effect.get(parameterName);
		double totalEV = pe.getTotalEnvironmentalVariance();
		double intraEV = gee.getFixedEnvironmentalValue(parameterName);
		double interEV = (double) pe.getInterEnvironmentalVariance();
		double phenoValue;
		phenoValue = getGeneticValue(parameterName, gee) + intraEV
				+ random.nextGaussian() * Math.sqrt(interEV * totalEV);
		return phenoValue;
	}

}
