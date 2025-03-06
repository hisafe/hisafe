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
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;
import java.util.TreeSet;

/**
 * Comment needed here - fc
 * 
 * @author I. Seynave - June 2002, F. de Coligny - November 2004
 */
public class AlleleDiversity extends AlleleParameters implements Serializable {
	// fc - 5.11.2004 - replaced GeneticTree / Genotypable, tree / gee, trees /
	// gees

	private short[][] nuclearAlleleDiversity;
	private short[][] mCytoplasmicAlleleDiversity;
	private short[][] pCytoplasmicAlleleDiversity;

	/**
	 * Constructor for new AlleleDiversity.
	 */
	public AlleleDiversity(short[][] na, short[][] ma, short[][] pa) {
		nuclearAlleleDiversity = na;
		mCytoplasmicAlleleDiversity = ma;
		pCytoplasmicAlleleDiversity = pa;
	}

	public short[][] getNuclearAlleleDiversity() {
		return nuclearAlleleDiversity;
	}

	public short[][] getMCytoplasmicAlleleDiversity() {
		return mCytoplasmicAlleleDiversity;
	}

	public short[][] getPCytoplasmicAlleleDiversity() {
		return pCytoplasmicAlleleDiversity;
	}

	/**
	 * Test if all arrays of an AlleleDiversity are without items inside. If so,
	 * this method returns true.
	 */
	public boolean isEmpty() {
		if (nuclearAlleleDiversity.length == 0 && mCytoplasmicAlleleDiversity.length == 0
				&& pCytoplasmicAlleleDiversity.length == 0) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * Compute the nuclearAlleleDiversity array from collection of genotypables
	 * with IndividualGenotype. This method is used by computeAlleleDiversity.
	 */
	public static short[][] computeNuclearAlleleDiversity(Collection gees) {

		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		short nuclearAlleleDiversity[][] = new short[(((IndividualGenotype) gee.getGenotype()).getNuclearDNA()).length][];
		Set[] temp = new Set[(((IndividualGenotype) gee.getGenotype()).getNuclearDNA()).length];
		for (int i = 0; i < (((IndividualGenotype) gee.getGenotype()).getNuclearDNA()).length; i++) {
			temp[i] = new TreeSet();
		}

		// review all genotypables and, for each loci, add the new allele to
		// list of potential allele.
		for (Iterator i = gees.iterator(); i.hasNext();) {

			_t = i.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			gee = (Genotypable) _t; // phd 2003_03_17

			short[][] nuclearDNA = ((IndividualGenotype) gee.getGenotype()).getNuclearDNA();
			for (int j = 0; j < nuclearDNA.length; j++) {
				for (int k = 0; k < 2; k++) {
					if (!temp[j].contains(new Integer(nuclearDNA[j][k]))) {
						temp[j].add(new Integer(nuclearDNA[j][k]));
					}
				}
			}
		}

		// built nuclearAlleleDiversity array.
		for (int j = 0; j < nuclearAlleleDiversity.length; j++) {
			int n = 0;
			nuclearAlleleDiversity[j] = new short[temp[j].size()];
			for (Iterator k = temp[j].iterator(); k.hasNext();) {
				Integer st = (Integer) k.next();
				if (st.intValue() != -1) {
					nuclearAlleleDiversity[j][n] = (short) st.intValue();
					n = n + 1;
				}
				if (st.intValue() == -1) {
					nuclearAlleleDiversity[j][temp[j].size() - 1] = (short) st.intValue();
				}
			}
		}
		return nuclearAlleleDiversity;
	}

	/**
	 * Compute the mCytoplamismicAlleleDiversity or pCytoplasmicAlleleDiversity
	 * array from collection of genotypables with IndividualGenotype and from
	 * origine of DNA. This method is used by computeAlleleDiversity.
	 */
	public static short[][] computeCytoplasmicAlleleDiversity(Collection gees, String parent) {

		Object _t = null;
		Iterator _i = gees.iterator();
		do {
			_t = _i.next();
		} while (!(_t instanceof Genotypable));
		Genotypable gee = (Genotypable) _t; // phd 2003_03_17

		short[][] cytoplasmicAlleleDiversity;
		Set[] temp;
		if ("maternal".equals(parent)) {
			cytoplasmicAlleleDiversity = new short[(((IndividualGenotype) gee.getGenotype()).getMCytoplasmicDNA()).length][];
			temp = new Set[(((IndividualGenotype) gee.getGenotype()).getMCytoplasmicDNA()).length];
		} else {
			cytoplasmicAlleleDiversity = new short[(((IndividualGenotype) gee.getGenotype()).getPCytoplasmicDNA()).length][];
			temp = new Set[(((IndividualGenotype) gee.getGenotype()).getPCytoplasmicDNA()).length];
		}
		for (int i = 0; i < cytoplasmicAlleleDiversity.length; i++) {
			temp[i] = new TreeSet();
		}

		// review all genotypables and, for each loci, add the new allele to
		// list of potential allele.
		for (Iterator i = gees.iterator(); i.hasNext();) {

			_t = i.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			gee = (Genotypable) _t; // phd 2003_03_17

			short[] cytoplasmicDNA;
			if ("maternal".equals(parent)) {
				cytoplasmicDNA = ((IndividualGenotype) gee.getGenotype()).getMCytoplasmicDNA();
			} else {
				cytoplasmicDNA = ((IndividualGenotype) gee.getGenotype()).getPCytoplasmicDNA();
			}
			for (int j = 0; j < cytoplasmicDNA.length; j++) {
				if (!temp[j].contains(new Integer(cytoplasmicDNA[j]))) {
					temp[j].add(new Integer(cytoplasmicDNA[j]));
				}
			}

		}

		// built cytoplasmicAlleleDiversity array.
		for (int j = 0; j < cytoplasmicAlleleDiversity.length; j++) {
			int n = 0;
			cytoplasmicAlleleDiversity[j] = new short[temp[j].size()];
			for (Iterator k = temp[j].iterator(); k.hasNext();) {
				Integer st = (Integer) k.next();
				if (st.intValue() != -1) {
					cytoplasmicAlleleDiversity[j][n] = (short) st.intValue();
					n = n + 1;
				}
				if (st.intValue() == -1) {
					cytoplasmicAlleleDiversity[j][temp[j].size() - 1] = (short) st.intValue();
				}
			}
		}
		return cytoplasmicAlleleDiversity;
	}

	/**
	 * Compute the AlleleDiversity object from collection of genotypables with
	 * IndividualGenotype from computeNuclearAlleleDiversity () and
	 * computeCytoplasmicAlleleDiversity ().
	 */
	public static void computeAlleleDiversity(Collection gees) {
		short[][] nuclearAlleleDiversity = computeNuclearAlleleDiversity(gees);
		short[][] mCytoplasmicAlleleDiversity = computeCytoplasmicAlleleDiversity(gees, "maternal");
		short[][] pCytoplasmicAlleleDiversity = computeCytoplasmicAlleleDiversity(gees, "paternal");
		AlleleDiversity alleleDiversity = new AlleleDiversity(nuclearAlleleDiversity, mCytoplasmicAlleleDiversity,
				pCytoplasmicAlleleDiversity);

		for (Iterator i = gees.iterator(); i.hasNext();) {

			Object _t = i.next();
			if (!(_t instanceof Genotypable)) {
				continue;
			}
			Genotypable gee = (Genotypable) _t; // phd 2003_03_17

			gee.getGenoSpecies().setAlleleDiversity(alleleDiversity);
		}
	}
}
