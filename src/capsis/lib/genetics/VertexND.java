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
import java.util.StringTokenizer;

import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;

/**
 * A vertex with nD coordinates. This is a convenient class.
 * 
 * @author I. Seynave - July 2002
 */
public class VertexND implements Serializable, Cloneable {

	public double[] coordinates;

	/**
	 * Usual constructor.
	 */
	public VertexND(double[] t) {
		coordinates = t;
	}

	/**
	 * This constructor tries to build a Vertex with the given String. Failure
	 * causes an exception to be thrown. Waited format is
	 * "[x1, x2, ..., xi, ..]" with xi of type double.
	 */
	public VertexND(String s) throws Exception {
		s = s.trim();
		if (s.charAt(0) != '[' || s.charAt(s.length() - 1) != ']') {
			throw new Exception("could not find [ or ] around this token: "+s);
		}
		s = s.substring(1, s.length() - 1).trim();

		StringTokenizer st = new StringTokenizer(s, ", ");

		coordinates = new double[st.countTokens()];
		int compteur = 0;
		while (st.hasMoreTokens()) {
			coordinates[compteur] = new java.lang.Double(st.nextToken().trim()).doubleValue();
			compteur = compteur + 1;
		}
	}

	public static VertexND convert(Vertex3d v3) {
		double[] coordinates = new double[3];
		coordinates[0] = v3.x;
		coordinates[1] = v3.y;
		coordinates[2] = v3.z;
		return new VertexND(coordinates);
	}

	public static VertexND convert(Vertex2d v2) {
		double[] coordinates = new double[2];
		coordinates[0] = v2.x;
		coordinates[1] = v2.y;
		return new VertexND(coordinates);
	}

	public Object clone() {
		return new VertexND(coordinates);
	}

	public String toString() {
		String str = "[";
		str += coordinates[0];
		for (int i = 1; i < coordinates.length; i++) {
			str += ",";
			str += coordinates[i];
		}
		str += "]";
		return str;
	}

	// for test only
	public static void main(String[] a) {
		double[] test = { 12.12, 12.12 };
		VertexND v1 = new VertexND(test);
		try {
			VertexND v2 = new VertexND("(1.2, 2.3, 4, 5)");
		} catch (Exception e) {
			System.out.println("Exception: " + e);
		}
	}

	// fc - 8.11.2004
	public double[] getCoordinates() {
		return coordinates;
	}

}
