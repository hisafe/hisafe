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

import capsis.commongui.projectmanager.Current;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;

/**
 * Genotype is genetics data with nuclear, maternal cytoplasmic and paternal
 * cytoplasmic data for a Genotypable. It is subclassed into IndividualGenotype
 * (when for each locus, allelic value is defined) and MultiGenotype (when for
 * each locus, the number of alleles is defined).
 * 
 * @author I. Seynave - July 2002, F. de Coligny - November 2004
 */
public abstract class Genotype implements Serializable {
	
	public static final IndividualGenotype EMPTY_INDIVIDUAL_GENOTYPE = new EmptyIndividualGenotype();
	public static final MultiGenotype EMPTY_MULTI_GENOTYPE = new EmptyMultiGenotype();

	// Each time the current step is changed (e.g. by clicking in the
	// ProjectManager),
	// GeneticTools.lastKnownStep is updated
	// fc-14.10.2010 - to remove a dependency in IntervenerTask
	static {

		Listener l = new Listener() {

			@Override
			public void somethingHappened(ListenedTo l, Object param) {
				GeneticTools.lastKnownStep = Current.getInstance().getStep();
//				System.out.println("Genotype: GeneticTools.lastKnownStep = " + GeneticTools.lastKnownStep); // trace
																											// (to
																											// be
																											// removed)
			}

		};
		Current.getInstance().addListener(l);

	}

	// method getGamete is defined in IndividualGenotype and MultiGenotype
	// classes.
	public abstract IndividualGenotype getGamete(AlleleParameters map);

	/**
	 * Build the IndividualGenotype of a new individal genotypable from the
	 * IndividualGenotype of two gametes.
	 */
	public static IndividualGenotype fuseGametes(IndividualGenotype g1, IndividualGenotype g2) {
		if (g1 != null && g2 != null
		/* && g1.isEmpty () == false && g2.isEmpty () == false */) {
			short[][] femaleGamete = g1.getNuclearDNA();
			short[][] maleGamete = g2.getNuclearDNA();
			int ploidy = femaleGamete[0].length + maleGamete[0].length;

			short[][] newNuclearDNA = new short[femaleGamete.length][ploidy];
			short[] newMCytoplasmicDNA = g1.getMCytoplasmicDNA();
			short[] newPCytoplasmicDNA = g2.getPCytoplasmicDNA();

			for (int i = 0; i < newNuclearDNA.length; i++) {
				for (int j = 0; j < newNuclearDNA[i].length; j++) {
					if (j < femaleGamete[i].length) {
						newNuclearDNA[i][j] = femaleGamete[i][j];
					} else {
						newNuclearDNA[i][j] = maleGamete[i][j - femaleGamete[i].length];
					}
				}
			}
			IndividualGenotype newIndividualGenotype = new IndividualGenotype(newNuclearDNA, newMCytoplasmicDNA,
					newPCytoplasmicDNA);
			return newIndividualGenotype;
		} else {
			return null;
		}
	}

	/**
	 * fc - 6.10.2003 - Equals redefinition Object.equals () return a == b This
	 * is not enough to compare constants after deserialization (bug, PhD) ex:
	 * if (genoptype.equals (EMPTY_INDIVIDUAL_GENOTYPE) This redefinition deals
	 * with constants and relies on super.equals () for the rest
	 */
	public boolean equals(Object b) {
		if (b == null) {
			return false;
		}
		if (this instanceof EmptyIndividualGenotype && b instanceof EmptyIndividualGenotype) {
			return true;
		}
		if (this instanceof EmptyMultiGenotype && b instanceof EmptyMultiGenotype) {
			return true;
		}
		return super.equals(b);
	}

	@Override
	public String toString() {
		if (this.equals(EMPTY_INDIVIDUAL_GENOTYPE))
			return "EMPTY_INDIVIDUAL_GENOTYPE";
		else if (this.equals(EMPTY_MULTI_GENOTYPE))
			return "EMPTY_MULTI_GENOTYPE";
		else if (this instanceof IndividualGenotype)
			return "IndividualGenotype: " + hashCode();
		else if (this instanceof MultiGenotype)
			return "MultiGenotype: " + hashCode();
		else
			return "Unknown Genotype (not individual, not multi) class: " + this.getClass().getName();
	}

}
