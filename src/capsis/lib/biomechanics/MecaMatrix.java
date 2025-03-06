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
 * MecaMatrix - Utility class for double precision floating point  scare matrices
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaMatrix {
//checked for c4.1.1_08 - fc - 3.2.2003

	private double [][] doubleMatrix;
	private int dimension;

	
	/**
	 * Constructor for a new dimension by dimension matrix with all elements to zero.
	 */
	public MecaMatrix (int dimension) {
		this.dimension = dimension;
		if (dimension != 0) {
   			doubleMatrix = new double [dimension][dimension];
			// default: initialize all elements to zero
   		}
		// default: initialize the reference doubleMatrix to null
	}

	/**
	 * Constructor for a new matrix with the provided values.
	 */
	public MecaMatrix (MecaMatrix matrix) {
      	dimension = matrix.dimension;
      	if (dimension != 0) {
   			doubleMatrix = new double [dimension][dimension];
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					doubleMatrix [i][j] = matrix.doubleMatrix [i][j];
				}
			}
		}
 	}

	/**
	 * Return the doubleMatrix of this matrix.
	 */
	public double [][] getDoubleMatrix () {return doubleMatrix;}

	/**
	 * Return the dimension of this matrix.
	 */
	public int getDimension () {return dimension;}

	/**
	 * Set the dimension of this matrix to the dimension provided with all new elements to zero.
	 */
	public void setDimension (int dimension) {
		this.dimension = dimension;
		if (dimension != 0) {
   			doubleMatrix = new double [dimension][dimension];
   		} else {
			doubleMatrix = null;
		}
	}

	/**
	 * Return the specified element of this matrix.
	 */
	public double getElement (int row, int column) {return doubleMatrix [row][column];}

	/**
	 * Set the specified element of this matrix to the provided value.
	 */
	public void setElement (int row, int column, double value) {
		doubleMatrix [row][column] = value;
	}

	/**
	 * Set the value of this matrix to the value of the provided matrix.
	 */
	public void set (MecaMatrix matrix) {
      	setDimension (matrix.dimension);
      	if (dimension != 0) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					doubleMatrix [i][j] = matrix.doubleMatrix [i][j];
				}
			}
		}
 	}

	/**
	 * Return a copy of this matrix.
	 */
	public MecaMatrix copy () {
		MecaMatrix resultMatrix = new MecaMatrix (this);
		return resultMatrix;
	}

	/**
	 * Set all elements of this matrix to the provided value.
	 */
	public void setAll (double value) {
		for (int i=0; i<dimension; i++) {
   			for (int j=0; j<dimension; j++) {
     			doubleMatrix [i][j] = value;
			}
		}
	}

	/**
	 * Set this matrix to the identity matrix with the actual dimension.
	 */
	public void identity () {
		setAll (0d);
		for (int i=0; i<dimension; i++) {
   			doubleMatrix [i][i] = 1d;
		}
	}

	/**
	 * Return the specified row of this matrix.
	 */
	public MecaVector getRow (int row) {
		MecaVector vector = new MecaVector (dimension);
   		if (dimension != 0) {
   			for (int column=0; column<dimension; column++) {
   				vector.setElement (column, doubleMatrix [row][column]);
			}
      	}
   		return vector;
	}

	/**
	 * Return the specified colunm of this matrix.
	 */
	public MecaVector getColumn (int column) {
		MecaVector vector = new MecaVector (dimension);
   		if (dimension != 0) {
   			for (int row=0; row<dimension; row++) {
   				vector.setElement (row, doubleMatrix [row][column]);
			}
      	}
   		return vector;
	}

	/**
	 * Set the specified row of this matrix to the provided vector.
	 */
	public void setRow (int row, MecaVector vector) {
		if (vector.getDimension () == dimension) {
			for (int column=0; column<dimension; column++) {
				doubleMatrix [row][column] = vector.getElement (column);
			}
		}
	}

	/**
	 * Set the specified column of this matrix to the provided vector.
	 */
	public void setColumn (int column, MecaVector vector) {
		if (vector.getDimension () == dimension) {
			for (int row=0; row<dimension; row++) {
				doubleMatrix [row][column] = vector.getElement (row);
			}
		}
	}

	/**
	 * Set the specified sub matrix of this matrix to the provided matrix.
	 * This sub matrix begins at the specified row and column in this matrix.
	 * Its dimension is equal to the dimension of the provided matrix.
	 */
	public void setSubMatrix (int row,int column, MecaMatrix matrix) {
		int d = matrix.dimension;
		if (d != 0 && d <= dimension) {
			if (row+d <= dimension || column+d <= dimension) {
				for (int i=0; i<d; i++) {
					for (int j=0; j<d; j++) {
						doubleMatrix [i+row][j+column] = matrix.doubleMatrix [i][j];
					}
				}
			}
		}
	}

	/**
	 * Return the matrix equal to the sum of this matrix and the provided matrix.
	 * The current matrix is not modified.
	 */
	public MecaMatrix sum (MecaMatrix matrix) {
		MecaMatrix resultMatrix = new MecaMatrix (this);
	   	if (dimension != 0 && matrix.dimension == dimension) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					resultMatrix.doubleMatrix [i][j] += matrix.doubleMatrix [i][j];
				}
			}
		}
		return resultMatrix;
	}

	/**
	 * Return the matrix equal to the difference of this matrix and the provided matrix.
	 * The current matrix is not modified.
	 */
	public MecaMatrix dif (MecaMatrix matrix) {
		MecaMatrix resultMatrix = new MecaMatrix (this);
	   	if (dimension != 0 && matrix.dimension == dimension) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					resultMatrix.doubleMatrix [i][j] -= matrix.doubleMatrix [i][j];
				}
			}
		}
		return resultMatrix;
	}

	/**
	 * Return the matrix equal to the product of this matrix by the provided matrix.
	 * The current matrix is not modified.
	 */
	public MecaMatrix pro (MecaMatrix matrix) {
		MecaMatrix resultMatrix = new MecaMatrix (dimension);
	   	if (dimension != 0 && matrix.dimension == dimension) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
          			for (int k=0; k<dimension; k++) {
						resultMatrix.doubleMatrix [i][j] +=
								doubleMatrix [i][k] * matrix.doubleMatrix [k][j];
					}
				}
			}
		}
		return resultMatrix;
	}

	/**
	 * Return the vector equal to the product of this matrix by the provided vector (column).
	 */
	public MecaVector pro (MecaVector vector) {
		MecaVector resultVector = new MecaVector (dimension);
		double element;
	   	if (dimension != 0 && vector.getDimension () == dimension) {
			for (int i=0; i<dimension; i++) {
				element = 0d;
				for (int j=0; j<dimension; j++) {
 					element += doubleMatrix [i][j] * vector.getElement (j);
				}
				resultVector.setElement (i, element);
			}
		}
		return resultVector;
	}

	/**
	 * Return the matrix equal to the product of this matrix by the provided scalar.
	 * The current matrix is not modified.
	 */
	public MecaMatrix pro (double scalar) {
		MecaMatrix resultMatrix = new MecaMatrix (dimension);
	   	if (dimension != 0 && scalar != 0d) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					resultMatrix.doubleMatrix [i][j] = doubleMatrix [i][j] * scalar;
				}
			}
		}
		return resultMatrix;
	}

	/**
	 * Return the matrix equal to the division of this matrix by the provided scalar.
	 * The current matrix is not modified.
	 */
	public MecaMatrix div (double scalar) {
		MecaMatrix resultMatrix = new MecaMatrix (dimension);
	   	if (dimension != 0 && scalar != 0d) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					resultMatrix.doubleMatrix [i][j] = doubleMatrix [i][j] / scalar;
				}
			}
		}
		return resultMatrix;
	}

	/**
	 * Return the matrix equal to the transpose of this matrix.
	 * The current matrix is not modified.
	 */
	public MecaMatrix transpose () {
		MecaMatrix resultMatrix = new MecaMatrix (dimension);
	   	if (dimension != 0) {
			for (int i=0; i<dimension; i++) {
				for (int j=0; j<dimension; j++) {
					resultMatrix.doubleMatrix [i][j] = doubleMatrix [j][i];
				}
			}
		}
		return resultMatrix;
	}

	/**
	 * Return the MecaMatrix String description.
	 */
	public String toString () {
		StringBuffer b = new StringBuffer ("MecaMatrix : dimension = ");
		b.append (dimension);
		return b.toString ();
	}

	/**
	 * Return the doubleMatrix as a detailed String.
	 */
	public String bigComplexString () {
		
		// NOTE: it would be faster to use StringBuffer and append () here (see toString ()). - fc - 3.2.2003
		
		String str = "\n";
		
		NumberFormat nfI = NumberFormat.getInstance ();
		nfI.setMinimumIntegerDigits (2);
		NumberFormat nfD = NumberFormat.getInstance ();
		nfD.setMinimumIntegerDigits (5);
		nfD.setMinimumFractionDigits (15);
		nfD.setMaximumFractionDigits (15);
		nfD.setGroupingUsed (false);
		
		for (int k=0; k<dimension; k=k+3) {
			str += "                         ";
			for (int c=k; c<k+3; c++) {
				str += "  col. n° " + nfI.format (c+1) + " (j = "+nfI.format (c) + ")   ";
			}
			str += "\n";
			for (int i=0; i<dimension; i++) {
				str += "lig. n° " + nfI.format (i+1) + " (i = " + nfI.format (i) + ")   |";
				for(int j=k; j<k+3; j++) {
					str += "   " + nfD.format (doubleMatrix [i][j]);
				}
				str += "   |\n";
			}
			str += "\n";
		}
		return str;
	}

	/**
	 * Print this matrix in the specified file.
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

	/**
	 * Test the MecaMatrix class.
	 */
	public static void main (String [] args) {
		MecaMatrix m = new MecaMatrix (3);
		m.setAll (Math.PI);
		m.print ("testmatrice.out");
	}


}
