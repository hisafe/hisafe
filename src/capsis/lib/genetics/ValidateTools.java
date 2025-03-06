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
import java.util.Map;
import java.util.Set;

/**
 * This class contains methods used by Validate.validateInitialData ().
 * 
 * @author I. Seynave - September 2002, F. de Coligny - November 2004
 */
public class ValidateTools {

	/**
	 * Build a Map where genotypables are classified according to species.
	 */
	public static void buildMapSpecies(Map map, GenoSpecies esp, Genotypable t) {
		if (!map.keySet().contains(esp)) {
			Set set = new HashSet();
			map.put(esp, set);
		}
		Set set = (Set) map.get(esp);
		set.add(t);
	}

	/**
	 * Build a Map where genotypables are classified according to species.
	 */
	public static void buildMapSpecies(Map map, String esp, Genotypable t) {
		if (!map.keySet().contains(esp)) {
			Set set = new HashSet();
			map.put(esp, set);
		}
		Set set = (Set) map.get(esp);
		set.add(t);
	}

	/**
	 * Add a column in a line for an array of short.
	 */
	public static void addValueInLineOfArray(short[][] tab, int value, int ligne) {
		short[] newLigne = new short[tab[ligne].length + 1];
		newLigne[tab[ligne].length] = (short) value;
		System.arraycopy(tab[ligne], 0, newLigne, 0, tab[ligne].length);
		tab[ligne] = newLigne;
	}

	/**
	 * Add a column in a line for an array of int.
	 */
	public static void addValueInLineOfArray(int[][] tab, int value, int ligne) {
		int[] newLigne = new int[tab[ligne].length + 1];
		newLigne[tab[ligne].length] = value;
		System.arraycopy(tab[ligne], 0, newLigne, 0, tab[ligne].length);
		tab[ligne] = newLigne;
	}

	/**
	 * Add a column in a line for an array of double.
	 */
	public static void addValueInLineOfArray(double[][] tab, double value, int ligne) {
		double[] newLigne = new double[tab[ligne].length + 1];
		newLigne[tab[ligne].length] = (double) value;
		System.arraycopy(tab[ligne], 0, newLigne, 0, tab[ligne].length);
		tab[ligne] = newLigne;
	}

}
