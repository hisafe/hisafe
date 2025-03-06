package capsis.lib.cstability.parameter;

import capsis.lib.cstability.util.Format;
import capsis.lib.cstability.util.Interval;

/**
 * Polymerization of a biochemical class.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Polymerization extends Interval<Double> {

	private double step;
	private double[] discretization;

	/**
	 * Constructor
	 */
	public Polymerization(double min, double max, double userStep) throws Exception {
		super(min, max);
		if (this.length() < userStep)
			throw new Exception("Polymerization, wrong Interval discretization, interval length: " + this.length()
					+ " < user step: " + userStep);

		// finalStep
		int nSteps = (int) Math.floor(this.length() / userStep);
		this.step = this.length() / ((double) nSteps);

		discretize(nSteps);
	}

	public Polymerization(Interval<Double> interval, double userStep) throws Exception {
		super(interval);
		if (this.length() < userStep)
			throw new Exception("Polymerization, wrong Interval discretization, interval length: " + this.length()
					+ " < user step: " + userStep);

		// finalStep
		int nSteps = (int) Math.floor(this.length() / userStep);
		this.step = this.length() / ((double) nSteps);

		discretize(nSteps);
	}

	/**
	 * discretize()
	 */
	private void discretize(int nSteps) {
		// discretization
		this.discretization = new double[nSteps + 1];
		for (int i = 0; i < nSteps + 1; ++i) {
			this.discretization[i] = this.getMin() + ((double) i) * this.step;
		}

	}

	/**
	 * getStep()
	 */
	public double getStep() {
		return step;
	}

	/**
	 * ()
	 */
	public double[] getDiscretization() {
		return discretization;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		return "Polymerization, " + super.toString() + ", finalStep: " + Format.toString(step);
	}

}
