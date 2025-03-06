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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

/**
 * Contains several methods which import genetic data from a file with standard
 * format of genetic data.
 * 
 * @author I. Seynave - July 2002, F. de Coligny - November 2004
 */
public class ImportGenotypeData {

	/**
	 * Import collection of double and build array of recombination
	 * probabilities.
	 */
	public static float[] importRecombinationProba(Collection rp) {
		Double essai = new Double(0);
		int rank2 = 0;
		float[] recombinationProbas = new float[rp.size()];
		Iterator ite2 = rp.iterator();
		while (ite2.hasNext()) {
			essai = (Double) ite2.next();
			// System.out.println ("valeur = " + essai);
			recombinationProbas[rank2] = essai.floatValue();
			rank2++;
		}
		return recombinationProbas;
	}

	/**
	 * Import collection of vertexND that contain short. This method is used to
	 * import AlleleDiversity.
	 */
	public static short[][] importMultiAllele(Collection ma) throws Exception {
		short[][] allele = new short[ma.size()][];
		int rank = 0;
		Iterator ite = ma.iterator();
		while (ite.hasNext()) {
			String str = (String) ite.next();
			VertexND vrt = new VertexND(str);
			double[] locus = vrt.getCoordinates();
			int c = locus.length;
			allele[rank] = new short[c];
			int compteur = 0;
			while (compteur < c) {
				allele[rank][compteur] = (short) locus[compteur];
				compteur = compteur + 1;
			}
			rank++;
		}
		return allele;
	}

	/**
	 * Import collection of vertexND that contain int. This method is used to
	 * import MultiGenotype.
	 */
	public static int[][] importMultiGenotype(Collection ma) throws Exception {
		int[][] allele = new int[ma.size()][];
		int rank = 0;
		Iterator ite = ma.iterator();
		while (ite.hasNext()) {
			String str = (String) ite.next();
			VertexND vrt = new VertexND(str);
			double[] locus = vrt.getCoordinates();
			int c = locus.length;
			allele[rank] = new int[c];
			int compteur = 0;
			while (compteur < c) {
				allele[rank][compteur] = (int) locus[compteur];
				compteur = compteur + 1;
			}
			rank++;
		}
		return allele;
	}

	/**
	 * Import collection of vertexND that contain double.
	 */
	public static double[][] importMultiFrequence(Collection mf) throws Exception {
		double[][] frequence = new double[mf.size()][];
		int rank = 0;
		Iterator ite = mf.iterator();
		while (ite.hasNext()) {
			String str = (String) ite.next();
			VertexND vrt = new VertexND(str);
			double[] locus = vrt.getCoordinates();
			int c = locus.length;
			int compteur = 0;
			frequence[rank] = new double[c];
			while (compteur < c) {
				frequence[rank][compteur] = (double) locus[compteur];
				compteur = compteur + 1;
			}
			rank++;
		}
		return frequence;
	}

	/**
	 * Import collection of short. This method is used to import the nuclearDNA
	 * of an IndividualGenotype.
	 */
	public static short[][] importDiploideDNA(Collection dna) {
		Double essai = new Double(0);
		int ploidy = 2;
		int rank = 0;
		int[] alleles = new int[dna.size()];
		Iterator ite = dna.iterator();
		while (ite.hasNext()) {
			essai = (Double) ite.next();
			alleles[rank] = essai.intValue();
			rank++;
		}

		short[][] nuclearDNA = new short[alleles.length / ploidy][ploidy];

		int l = 0;
		for (int j = 0; j < nuclearDNA.length; j++) {
			for (int k = 0; k < nuclearDNA[j].length; k++) {
				nuclearDNA[j][k] = (short) alleles[l];
				l = l + 1;
			}
		}
		return nuclearDNA;
	}

	/**
	 * Import collection of short. This method is used to import the
	 * mCytoplamsicDNA and pCytoplamsicDNA of an IndividualGenotype.
	 */
	public static short[] importHaploideDNA(Collection dna) {
		Double essai = new Double(0);
		int rank = 0;
		short[] cytoplasmicDNA = new short[dna.size()];
		Iterator ite = dna.iterator();
		while (ite.hasNext()) {
			essai = (Double) ite.next();
			cytoplasmicDNA[rank] = essai.shortValue();
			rank++;
		}
		return cytoplasmicDNA;
	}

	// Returns [109,113,117]
	public static String toString(short[] s) {
		StringBuffer b = new StringBuffer("[");
		int n = s.length;
		for (int i = 0; i < n; i++) {
			b.append(s[i]);
			if (i < n - 1) {
				b.append(",");
			}
		}
		b.append("]");
		return b.toString();
	}

	// Returns {[109,113,117]; [109,113,117]}
	public static Collection toCollection(short[][] s) {
		Collection b = new ArrayList();
		int n = s.length;
		for (int i = 0; i < n; i++) {
			b.add(toString(s[i]));
		}
		return b;
	}

	// Returns [109,113,117]
	public static String toString(int[] s) {
		StringBuffer b = new StringBuffer("[");
		int n = s.length;
		for (int i = 0; i < n; i++) {
			b.append(s[i]);
			if (i < n - 1) {
				b.append(",");
			}
		}
		b.append("]");
		return b.toString();
	}

	// Returns {[109,113,117]; [109,113,117]}
	public static Collection toCollection(int[][] s) {
		Collection b = new ArrayList();
		int n = s.length;
		for (int i = 0; i < n; i++) {
			b.add(toString(s[i]));
		}
		return b;
	}

	// ~ // Returns {[109,113,117]; [109,113,117]}
	// ~ public static String toString (short[][] s) {
	// ~ StringBuffer b = new StringBuffer ("{");
	// ~ int n = s.length;
	// ~ for (int i = 0; i < n; i++) {
	// ~ b.append (toString (s[i]));
	// ~ if (i < n-1) {
	// ~ b.append (";");
	// ~ }
	// ~ }
	// ~ b.append ("}");
	// ~ return b.toString ();
	// ~ }

}
