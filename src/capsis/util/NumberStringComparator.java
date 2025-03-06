/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util;

import java.io.StreamTokenizer;
import java.io.StringReader;
import java.util.Comparator;

/**
 * Comparator for Strings which contain a Number, e.g. "11", "3.45", "12
 * (dbh=3.45)", "index 25", "index_25", "index(25)"...
 * 
 * @author F. de Coligny - March 2004
 */
public class NumberStringComparator implements Comparator {

	public int compare(Object o1, Object o2) {
		
		String n1 = (String) o1;
		String n2 = (String) o2;

		// fc-17.6.2019 Added try catch with a default comparison on string in
		// case of trouble
		try {
			
			// fc-14.9.2021
			double d1 = getFirstNumber(n1);
			double d2 = getFirstNumber(n2);

//			int i1 = n1.indexOf(" ");
//			if (i1 != -1)
//				n1 = n1.substring(0, i1);
//
//			int i2 = n2.indexOf(" ");
//			if (i2 != -1)
//				n2 = n2.substring(0, i2);
//
//			double d1 = new Double(n1).doubleValue();
//			double d2 = new Double(n2).doubleValue();

			// fc-14.9.2021
			return d1 < d2 ? -1 : d1 > d2 ? 1 : 0;

		} catch (Exception e) {
			return ((String) o1).compareTo(((String) o2));
		}
	}

	/**
	 * Explores the given string and returns the first occurrence of number it
	 * contains, or an exception if no number or trouble.
	 */
	static private double getFirstNumber(String s) throws Exception {
		
		// fc-14.9.2021
		
		StreamTokenizer st = new StreamTokenizer(new StringReader(s));

		st.parseNumbers();
		st.ordinaryChar('_'); // Considered as a delimiter like " "

		double number1 = 0;
		while (st.nextToken() != StreamTokenizer.TT_EOF) {
			if (st.ttype == StreamTokenizer.TT_NUMBER)
				return st.nval;
		}
		throw new Exception("Could not find a number in String: " + s);
	}

	// Test method
	public static void main(String[] args) {

		// Testing StreamTokenizer

		try {

			test("12");
			test("12.34");

			test("12 suffix");
			test("12.34 suffix");

			test("12_suffix");
			test("12.34_suffix");

			test("index 25");
			test("index 25.67");

			test("index_25");
			test("index_25.67");

			test("index_25 suffix");
			test("index_25.67 suffix");

			test("index_25_suffix");
			test("index_25.67_suffix");

			test("index(25)suffix");
			test("index,25.67,suffix");
			test("index;25.67;suffix");

			test("big cat");
			test("index");
			test("index; ;suffix");

		} catch (Exception e) {
			e.printStackTrace();
		}

	}

	// Test method (see main ())
	static private void test(String s) {

		try {
			double n = getFirstNumber(s);
			System.out.println(s + " -> " + n);

		} catch (Exception e) {
			System.out.println(s + " -> " + e);
		}

	}

}
