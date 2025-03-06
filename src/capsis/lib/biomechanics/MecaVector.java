/* 
 * Biomechanics library for Capsis4.
 * 
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import java.io.FileWriter;
import java.io.PrintWriter;
import java.text.NumberFormat;

/**
 * MecaVector - Utility class for double precision floating point  vectors.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaVector {
//checked for c4.1.1_08 - fc - 4.2.2003

	private double [] doubleVector;
	private int dimension;


	/**
	 * Constructor for a new vector with all elements (number = dimension provided) to zero.
	 */
	public MecaVector (int dimension) {
		this.dimension = dimension;
		if (dimension != 0) {
   			doubleVector = new double [dimension];
  		}
	}

	/**
	 * Constructor for a new vector with the value of the vector provided.
	 */
	public MecaVector (MecaVector vector) {
      	dimension = vector.dimension;
      	if (dimension != 0) {
   			doubleVector = new double [dimension];
			for (int i=0; i<dimension; i++) {
				doubleVector [i] = vector.doubleVector [i];
			}
		}
 	}

	/**
	 * Return the doubleVector of this vector.
	 */
	public double [] getDoubleVector () {return doubleVector;}

	/**
	 * Return the dimension of this vector.
	 */
	public int getDimension () {return dimension;}

	/**
	 * Set the dimension of this vector to the provided dimension with all new elements to zero.
	 */
	public void setDimension (int dimension) {
		this.dimension = dimension;
		if (dimension != 0) {
   			doubleVector = new double [dimension];
   		} else {
			doubleVector = null;
		}
	}

	/**
	 * Return the specified element of this vector.
	 */
	public double getElement (int row) {return doubleVector [row];}

	/**
	 * Set the specified element of this vector to the provided value.
	 */
	public void setElement (int row, double value) {
		doubleVector [row] = value;
	}

	/**
	 * Set the value of this vector to the value of the provided vector.
	 */
	public void set (MecaVector vector) {
      	setDimension (vector.dimension);
      	if (dimension != 0) {
			for (int i=0; i<dimension; i++) {
				doubleVector [i] = vector.doubleVector [i];
			}
		}
 	}

	/**
	 * Return a copy of this vector.
	 */
	public MecaVector copy () {
		MecaVector resultVector = new MecaVector (this);
		return resultVector;
	}

	/**
	 * Set all elements of this vector to the  provided value.
	 */
	public void setAll (double value) {
		for (int i=0; i<dimension; i++) {
     		doubleVector [i] = value;
		}
	}

	/**
	 * Return the sub vector specified between the rows irow and frow.
	 */
    public MecaVector getSubVector (int irow, int frow) {
		int d = frow - irow + 1;
		MecaVector resultVector = new MecaVector (d);
		if (dimension != 0) {
			for (int i=0; i<d; i++) {
   				resultVector.doubleVector [i] = doubleVector [i+irow];
			}
		}
		return resultVector;
	}

	/**
	 * Set the specified sub vector of this vector to the provided vector.
	 * This sub vector begins at the specified row  in this vector.
	 * Its dimension is equal to the dimension of the provided vector.
	 */
    public void setSubVector (int row, MecaVector vector) {
		int d = vector.dimension;
		if (d !=0 && d <= dimension && row+d <= dimension) {
			for (int i=0; i<d; i++) {
				doubleVector [i+row] = vector.doubleVector [i];
			}
		}
	}

	/**
	 * Return the vector equal to the sum of this vector and the provided vector.
	 * The current vector is not modified.
	 */
	public MecaVector sum (MecaVector vector) {
		MecaVector resultVector = new MecaVector (this);
	   	if (dimension != 0 && vector.dimension == dimension) {
			for (int i=0; i<dimension; i++) {
				resultVector.doubleVector [i] += vector.doubleVector [i];
			}
		}
		return resultVector;
	}

	/**
	 * Return the vector equal to the difference of this vector and the provided vector.
	 * The current vector is not modified.
	 */
	public MecaVector dif (MecaVector vector) {
		MecaVector resultVector = new MecaVector (this);
	   	if (dimension != 0 && vector.dimension == dimension) {
			for (int i=0; i<dimension; i++) {
				resultVector.doubleVector [i] -= vector.doubleVector [i];
			}
		}
		return resultVector;
	}

	/**
	 * Return the vector equal to the product of this vector by the provided scalar.
	 * The current vector is not modified.
	 */
	public MecaVector pro (double scalar) {
		MecaVector resultVector = new MecaVector (dimension);
	   	if (dimension != 0 && scalar != 0d) {
			for (int i=0; i<dimension; i++) {
				resultVector.doubleVector [i] = doubleVector [i] * scalar;
			}
		}
		return resultVector;
	}

	/**
	 * Return the vector equal to the division of this vector by the provided scalar.
	 * The current vector is not modified.
	 */
	public MecaVector div (double scalar) {
		MecaVector resultVector = new MecaVector (dimension);
	   	if (dimension != 0 && scalar != 0d) {
			for (int i=0; i<dimension; i++) {
				resultVector.doubleVector [i] = doubleVector [i] / scalar;
			}
		}
		return resultVector;
	}

	/**
	 * Return the scalar equal to the dot (or scalar) product of this vector by the provided vector.
	 */
	public double dot (MecaVector vector) {
		double scalarProduct = 0d;
	   	if (dimension == vector.dimension) {
			for (int i=0; i<dimension; i++) {
				scalarProduct += doubleVector [i] * vector.doubleVector [i];
			}
		}
		return scalarProduct;
	}

	/**
	 * Return the vector equal to the cross (or vectorial) product of this vector by the provided vector.
	 * The current vector is not modified.
	 * Only when dimension is equal to 3 !
	 */
	public MecaVector cross (MecaVector vector) {
		MecaVector resultVector = new MecaVector (3);
	   	if (dimension == vector.dimension && dimension == 3) {
			resultVector.doubleVector [0] = doubleVector [1] * vector.doubleVector [2] -
					doubleVector [2] * vector.doubleVector [1];
   			resultVector.doubleVector [1] = doubleVector [2] * vector.doubleVector [0] -
					doubleVector [0] * vector.doubleVector [2];
   			resultVector.doubleVector [2] = doubleVector [0] * vector.doubleVector [1] -
					doubleVector [1] * vector.doubleVector [0];
		}
		return resultVector;
	}

	/**
	 * Return the vector equal to the rotation of this vector around
	 * the rotation provided axis and with the provided angle.
	 * The current vector is not modified.
	 * Only when dimension is equal to 3 !
	 */
	public MecaVector rot3D (MecaVector axisR, double angle) {
		MecaVector resultVector = new MecaVector (3);
		if (dimension ==3 && axisR.dimension == 3) {
			double cosa, cosf, sina;
			double ax11, ax12, ax13, ax22, ax23, ax33, ax1, ax2, ax3;
			double e0, e1, e2;
			
			MecaVector axis = new MecaVector (axisR);
			double norm = axis.norm ();
			if (norm != 0d) {
				axis = axis.div (norm);
			}
			
			cosa = Math.cos (angle);
			cosf = 1d - cosa;
			sina = Math.sin (angle);
			
			ax11 = axis.getElement (0) * axis.getElement (0) * cosf;
			ax12 = axis.getElement (0) * axis.getElement (1) * cosf;
			ax13 = axis.getElement (0) * axis.getElement (2) * cosf;
			ax22 = axis.getElement (1) * axis.getElement (1) * cosf;
			ax23 = axis.getElement (1) * axis.getElement (2) * cosf;
			ax33 = axis.getElement (2) * axis.getElement (2) * cosf;
			ax1 = axis.getElement (0) * sina;
			ax2 = axis.getElement (1) * sina;
			ax3 = axis.getElement (2) * sina;
			
			e0 = (ax11 + cosa) * doubleVector [0] + (ax12 - ax3) * doubleVector [1] + (ax13 + ax2) * doubleVector [2];
			e1 = (ax12 + ax3) * doubleVector [0] +(ax22 + cosa) * doubleVector [1] + (ax23 - ax1) * doubleVector [2];
			e2 = (ax13 - ax2) * doubleVector [0] + (ax23 + ax1) * doubleVector [1] + (ax33 + cosa) * doubleVector [2];
			
			resultVector.setElement (0, e0);
			resultVector.setElement (1, e1);
			resultVector.setElement (2, e2);
		}
		return resultVector;
	}

   	/**
  	 * Return the positive scalar equal to the norm of this vector.
	 */
	public double norm () {
		double norm = 0d;
	   	if (dimension != 0) {
			MecaVector vector = new MecaVector (this);
			norm = this.dot (vector);
			norm = Math.sqrt (norm);
		}
		return norm;
	}

	/**
	 * Return the MecaVector String description.
	 */
	public String toString () {
		String str = "MecaVector : dimension = " + dimension;
		return str;
	}

	/**
	 * Return the doubleVector as a simple String.
	 */
	public String bigSimpleString () {
		String str = "";
		NumberFormat nfD = NumberFormat.getInstance ();
		nfD.setMinimumFractionDigits (6);
		nfD.setMaximumFractionDigits (6);
		nfD.setGroupingUsed (false);
		
		for (int i=0; i<dimension; i++) {
			str += nfD.format (doubleVector [i]) + "\t";
		}
		return str;
	}

	/**
	 * Return the doubleVector as a complex String.
	 */
	public String bigComplexString () {
		String str = "\n";
		NumberFormat nfI = NumberFormat.getInstance ();
		nfI.setMinimumIntegerDigits (2);
		NumberFormat nfD = NumberFormat.getInstance ();
		nfD.setMinimumIntegerDigits (5);
		nfD.setMinimumFractionDigits (15);
		nfD.setMaximumFractionDigits (15);
		nfD.setGroupingUsed (false);
		
		for (int i=0; i<dimension; i++) {
			str += "lig. n° " + nfI.format (i+1) + " (i = " + nfI.format (i) + ")   |   " + nfD.format (doubleVector [i]) + "   |\n";
		}
		str += "\n";
		return str;
	}

	/**
	 * Print this vector in the specified file.
	 */
	public void print (String fileName) {
		PrintWriter out = null;
		try {
			out = new PrintWriter (new FileWriter (fileName));
		} catch (Exception e) {
			System.out.println ("File name " + fileName + " causes error : " + e.toString ());
		}
		String str = bigComplexString ();
		out.print (str);
		out.flush ();
		out.close ();
	}

	//------------------------------------------------------------------------------------------------------>

	/**
	 * Test the MecaVector class.
	 */
	public static void main (String [] args) {
		MecaVector v = new MecaVector (6);
		v.setAll (Math.PI);
		v.print ("testvecteur.out");
	}

}