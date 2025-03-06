package capsis.lib.cstability.function.util;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.interfaces.PrimitiveAvailable;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.Polymerization;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Log;
import capsis.lib.cstability.util.Matrix;

/**
 * Kernel matrix for enzyme in the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class EnzymeKernelMatrix extends Matrix {

	public static final String STANDARD_KERNEL_INTEGRATION = "STANDARD_KERNEL_INTEGRATION";
	public static final String INTEGRAL_KERNEL_INTEGRATION = "INTEGRAL_KERNEL_INTEGRATION";

	public static final Set<String> AVAILABLE_INTEGRATION_METHODS = new HashSet<>();

	static {
		AVAILABLE_INTEGRATION_METHODS.add(INTEGRAL_KERNEL_INTEGRATION);
		AVAILABLE_INTEGRATION_METHODS.add(STANDARD_KERNEL_INTEGRATION);
	}

	private double[] discretization;
	private double discretizationStep;
	private Function kernelFunction;
	private String kernelIntegrationMethod;

	/**
	 * EnzymeKernelMatrix(): the EnzymeKernelMatrix is evaluated with a function
	 */
	public EnzymeKernelMatrix(Polymerization polymerization, Function kernelFunction, String kernelIntegrationMethod,
			Parameters p, Context c, State s) throws Exception {

		super(polymerization.getDiscretization().length);
		discretization = polymerization.getDiscretization();
		discretizationStep = polymerization.getStep();
		this.kernelFunction = kernelFunction;
		this.kernelIntegrationMethod = kernelIntegrationMethod;

		if (kernelIntegrationMethod.equals(STANDARD_KERNEL_INTEGRATION)) {
			buildStandardKernel(p, c, s);
		} else if (kernelIntegrationMethod.equals(INTEGRAL_KERNEL_INTEGRATION)) {
			buildIntegralKernel(p, c, s);
		} else {
			throw new Exception("EnzymeKernelMatrix.constructor(), unknown kernel type" + kernelIntegrationMethod
					+ ", expect: " + STANDARD_KERNEL_INTEGRATION + ", " + INTEGRAL_KERNEL_INTEGRATION);
		}
	}

	/**
	 * buildStandardKernel()
	 */
	public void buildStandardKernel(Parameters p, Context c, State s) throws Exception {
		if (p.getIntegrationMethod().equals(DiscreteDistribution.INTEGRATION_RECTANGLE_LEFT)) {
			for (int i = 0; i < nLines - 1; i++) {
				for (int j = 0; j < nColumns - 1; j++)
					set(i, j,
							kernelFunction.execute(p, c, null, new TwoVariables(discretization[i], discretization[j])));
			}
		} else if (p.getIntegrationMethod().equals(DiscreteDistribution.INTEGRATION_RECTANGLE_RIGHT)) {
			for (int i = 1; i < nLines; i++) {
				for (int j = 1; j < nColumns; j++)
					set(i, j, kernelFunction.execute(p, c, s, new TwoVariables(discretization[i], discretization[j])));
			}
		} else if (p.getIntegrationMethod().equals(DiscreteDistribution.INTEGRATION_TRAPEZE)) {
			for (int i = 0; i < nLines; i++) {
				set(i, 0, kernelFunction.execute(p, c, s, new TwoVariables(discretization[i], discretization[0])) / 2d);
				for (int j = 1; j < nColumns - 1; j++)
					set(i, j, kernelFunction.execute(p, c, s, new TwoVariables(discretization[i], discretization[j])));
				set(i, nColumns - 1, kernelFunction.execute(p, c, s,
						new TwoVariables(discretization[i], discretization[nColumns - 1])) / 2d);
			}
		}
		checkMassConservationAndNormalize(p);
		mult(discretizationStep);
	}

	/**
	 * buildIntegralKernel()
	 */
	private void buildIntegralKernel(Parameters p, Context c, State s) throws Exception {
		if (!(kernelFunction instanceof PrimitiveAvailable))
			throw new Exception("EnzymeKernelMatrix.buildIntegralKernel(), the kernelFunction " + kernelFunction
					+ " has no primitive.");
		PrimitiveAvailable fpa = (PrimitiveAvailable) kernelFunction;

		for (int i = 0; i < nLines; i++) {
			List<Double> Li = new ArrayList<>();
			for (int j = 0; j < nLines - 1; j++) {
				Li.add(fpa.executePrimitive(p, c, s, discretization, new TwoVariables(i, j)));
			}

			set(i, 0, 0.5 * Li.get(0));
			for (int j = 1; j < nLines - 1; j++)
				set(i, j, 0.5 * Li.get(j - 1) + 0.5 * Li.get(j));
			set(i, nLines - 1, 0.5 * Li.get(nLines - 2));
		}
	}

	/**
	 * checkMassConservationAndNormalize()
	 */
	private void checkMassConservationAndNormalize(Parameters p) throws Exception {
		for (int j = 0; j < nColumns; j++) {
			Matrix col = this.getColumn(j);
			double test = col.sum() * p.getUserPolymerizationStep();
			if (test < 0.99 || test > 1.01)
				Log.println("EnzymeKernelMatrix.checkMassConservationAndNormalize()",
						"WARNING, check kernel formulation for mass conservation, expect 1 for norm, get " + test);
			if (test != 0) {
				Matrix normCol = Matrix.product(1 / test, col);
				this.setColum(normCol, j);
			}
		}
	}

	/**
	 * getKernelFunction():
	 */
	public Function getKernelFunction() {
		return kernelFunction;
	}

	/**
	 * getKernelIntegrationMethod()
	 */
	public String getKernelIntegrationMethod() {
		return kernelIntegrationMethod;
	}
}
