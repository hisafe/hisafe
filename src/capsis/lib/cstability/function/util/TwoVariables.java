package capsis.lib.cstability.function.util;

/**
 * TwoVariables of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
public class TwoVariables extends Variables {

	public double x1;
	public double x2;

	/**
	 * Constructor
	 */
	public TwoVariables(double x1, double x2) {
		this.x1 = x1;
		this.x2 = x2;
	}
}
