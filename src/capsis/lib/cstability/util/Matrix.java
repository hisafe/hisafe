package capsis.lib.cstability.util;

import java.io.Serializable;

/**
 * Matrix in the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class Matrix implements Cloneable, Serializable {

	protected double[][] values;
	protected int nLines;
	protected int nColumns;

	/**
	 * Constructor
	 */
	public Matrix(int nLines, int nColumns) {
		// this constructor produced a zero matrix
		this.nLines = nLines;
		this.nColumns = nColumns;
		values = new double[nLines][nColumns];
	}	
	
	/**
	 * Constructor
	 */
	public Matrix(int n) {
		// this constructor produced a zero squared matrix
		nLines = n;
		nColumns = n;
		values = new double[nLines][nColumns];
	}

	/**
	 * clone()
	 */
	@Override
	public Matrix clone() throws CloneNotSupportedException {
		Matrix m = new Matrix(nLines, nColumns);
		for (int i = 0; i < nLines; ++i) {
			for (int j = 0; j < nColumns; ++j) {
				m.values[i][j] = values[i][j];
			}
		}
		return m;
	}

	/**
	 * Standard matrices
	 */

	/**
	 * zeros()
	 */
	public static Matrix zeros(int nLines, int nColumns) {
		return new Matrix(nLines, nColumns);
	}

	/**
	 * zeros()
	 */
	public static Matrix zeros(int n) {
		return new Matrix(n, n);
	}

	/**
	 * zeros()
	 */
	public static Matrix zeros(Matrix m) {
		return zeros(m.nLines, m.nColumns);
	}

	/**
	 * ones()
	 */
	public static Matrix ones(int nLines, int nColumns) {
		Matrix m = new Matrix(nLines, nColumns);
		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				m.values[i][j] = 1d;
			}
		}
		return m;
	}

	/**
	 * ones()
	 */
	public static Matrix ones(Matrix m) {
		return ones(m.nLines, m.nColumns);
	}

	/**
	 * eye()
	 */
	public static Matrix eye(int n) throws Exception {
		return diagonal(lineToArray(ones(1, n)));
	}

	/**
	 * diagonal()
	 */
	public static Matrix diagonal(double[] array) {
		int size = array.length;
		Matrix m = new Matrix(size, size);
		for (int i = 0; i < size; i++) {
			m.set(i, i, array[i]);
		}
		return m;
	}

	/**
	 * Array Matrix convertors
	 */

	/**
	 * arrayToLine()
	 */
	public static Matrix arrayToLine(double[] array) {

		int size = array.length;
		Matrix m = new Matrix(1, size);
		for (int i = 0; i < size; i++) {
			m.set(0, i, array[i]);
		}
		return m;
	}

	/**
	 * arrayToColumn()
	 */
	public static Matrix arrayToColumn(double[] array) {

		int size = array.length;
		Matrix m = new Matrix(size, 1);
		for (int i = 0; i < size; i++) {
			m.set(i, 0, array[i]);
		}
		return m;
	}

	/**
	 * columnToArray()
	 */
	public static double[] columnToArray(Matrix m) throws Exception {

		if (m.nColumns != 1)
			throw new Exception("Matrix.columnToArray(), m must be a column");

		double[] array = new double[m.nLines];
		for (int i = 0; i < m.nLines; i++) {
			array[i] = m.values[i][0];
		}
		return array;
	}

	/**
	 * lineToArray()
	 */
	public static double[] lineToArray(Matrix m) throws Exception {

		if (m.nLines != 1)
			throw new Exception("Matrix.lineToArray(), m must be a line");

		double[] array = new double[m.nColumns];
		for (int i = 0; i < m.nColumns; i++) {
			array[i] = m.values[0][i];
		}
		return array;
	}

	/**
	 * Non-static operations
	 */

	/**
	 * add()
	 */
	public void add(Matrix m) throws Exception {

		if (this.nLines != m.nLines || this.nColumns != m.nColumns)
			throw new Exception("Matrix.add(), dimension mismatch");

		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				this.values[i][j] += m.values[i][j];
			}
		}
	}

	/**
	 * substract()
	 */
	public void substract(Matrix m) throws Exception {

		if (this.nLines != m.nLines || this.nColumns != m.nColumns)
			throw new Exception("Matrix.substract(), dimension mismatch");

		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				this.values[i][j] -= m.values[i][j];
			}
		}
	}

	/**
	 * mult()
	 */
	public void mult(double scalar) {
		for (int i = 0; i < nLines; i++) {
			for (int j = 0; j < nColumns; j++) {
				values[i][j] *= scalar;
			}
		}
	}

	/**
	 * Static operations
	 */

	/**
	 * addition()
	 */
	public static Matrix addition(Matrix m, Matrix n) throws Exception {

		if (m.nLines != n.nLines || m.nColumns != n.nColumns)
			throw new Exception("Matrix.addition(), dimension mismatch");

		Matrix sum = m.clone();
		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				sum.values[i][j] = m.values[i][j] + n.values[i][j];
			}
		}
		return sum;
	}

	/**
	 * substraction()
	 */
	public static Matrix substraction(Matrix m, Matrix n) throws Exception {

		if (m.nLines != n.nLines || m.nColumns != n.nColumns)
			throw new Exception("Matrix.substraction(), dimension mismatch");

		Matrix sum = m.clone();
		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				sum.values[i][j] = m.values[i][j] - n.values[i][j];
			}
		}
		return sum;
	}

	/**
	 * product()
	 */
	public static Matrix product(double scal, Matrix m) throws Exception {

		Matrix product = m.clone();
		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				product.values[i][j] *= scal;
			}
		}
		return product;
	}

	/**
	 * product()
	 */
	public static Matrix product(Matrix m, Matrix n) throws Exception {

		if (m.nColumns != n.nLines)
			throw new Exception("Matrix.product(Matrix m, Matrix n), dimension mismatch");

		Matrix product = new Matrix(m.nLines, n.nColumns);
		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < n.nColumns; j++) {
				for (int k = 0; k < m.nColumns; k++) {
					product.values[i][j] += m.values[i][k] * n.values[k][j];
				}
			}
		}
		return product;
	}

	/**
	 * productByElement()
	 */
	public static Matrix productByElement(Matrix m, Matrix n) throws Exception {

		if (m.nLines != n.nLines || m.nColumns != n.nColumns)
			throw new Exception("Matrix.productByElement(), dimension mismatch");

		Matrix product = zeros(m);
		for (int i = 0; i < m.nLines; i++) {
			for (int j = 0; j < m.nColumns; j++) {
				product.values[i][j] = m.values[i][j] * n.values[i][j];
			}
		}
		return product;
	}

	/**
	 * sum()
	 */
	public double sum() {
		double sum = 0;
		for (int i = 0; i < nLines; i++) {
			for (int j = 0; j < nColumns; j++) {
				sum += values[i][j];
			}
		}
		return sum;
	}

	/**
	 * Accessors
	 */

	/**
	 * set()
	 */
	public void set(int i, int j, double value) {
		values[i][j] = value;
	}

	/**
	 * get()
	 */
	public double get(int i, int j) {
		return values[i][j];
	}

	/**
	 * multiplyColumn()
	 */
	public void multiplyColumn(double scal, int j) throws Exception {
		this.setColum(product(scal, this.getColumn(j)), j);
	}

	/**
	 * setColum()
	 */
	public void setColum(Matrix m, int j) throws Exception {

		if (m.nColumns != 1)
			throw new Exception("Matrix.setColum(), m must be a column");
		if (this.nLines != m.nLines)
			throw new Exception("Matrix.setColum(), m has a wrong number of lines");
		if (this.nColumns <= j)
			throw new Exception("Matrix.setColum(), index is out of bounds");

		for (int i = 0; i < nLines; i++) {
			this.values[i][j] = m.values[i][0];
		}
	}

	/**
	 * getColumn()
	 */
	public Matrix getColumn(int j) {

		Matrix column = new Matrix(nLines, 1);
		for (int i = 0; i < nLines; i++) {
			column.values[i][0] = this.values[i][j];
		}
		return column;
	}

	/**
	 * getNLines()
	 */
	public int getNLines() {
		return nLines;
	}

	/**
	 * getNColumns()
	 */
	public int getNColumns() {
		return nColumns;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		String s = "";
		for (int i = 0; i < nLines; i++) {
			String line = "";
			for (int j = 0; j < nColumns; ++j) {
				line += values[i][j] + " ";
			}
			s += line + "\n";
		}
		return s;

	}
}
