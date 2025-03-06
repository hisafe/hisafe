package capsis.lib.cstability.context;

import capsis.lib.cstability.util.Interval;

/**
 * Timeline of a simulation of C-Stability.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
public class Timeline extends Interval<Integer> {

	private String unit;
	private double step;
	private double[] discretization;

	/**
	 * Constructor
	 */
	public Timeline(String unit, int min, int max, double userStep) throws Exception {
		super(min, max);
		this.unit = unit;
		if (userStep > 1)
			throw new Exception("Timeline, wrong discretization, user step " + userStep + " > 1");
		step = 1d / Math.floor(1d / userStep);
		discretize();
	}

	private void discretize() {
		int nSteps = (int) (this.length() / step);
		discretization = new double[nSteps + 1];
		for (int i = 0; i < nSteps + 1; ++i) {
			discretization[i] = this.getMin() + ((double) i) * this.step;
		}
	}

	public String getUnit() {
		return unit;
	}

	public int getInitialDate () {
		return getMin ();
	}

	public int getFinalDate () {
		return getMax ();
	}
	
	public double getStep() {
		return step;
	}

	public double[] getDiscretization() {
		return discretization;
	}

	@Override
	public String toString() {
		return "Timeline, unit: " + unit + ", " + super.toString() + ", step: " + step;
	}

	public int getIndex(double date) {
		int index = -1;
		for (int i = 0; i < discretization.length; ++i) {
			if (date == discretization[i])
				index = i;
		}
		return index;
	}

}
