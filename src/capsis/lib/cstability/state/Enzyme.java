package capsis.lib.cstability.state;

import java.io.Serializable;
import java.util.Set;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.OneVariable;
import capsis.lib.cstability.function.util.TwoVariables;
import capsis.lib.cstability.parameter.EnzymeTraits;
import capsis.lib.cstability.parameter.Parameters;

/**
 * Enzyme of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Enzyme implements Serializable {

	private EnzymeTraits traits;
	private DiscretePositiveDistribution depolymerizationRate;
	private DiscreteDistribution activityDistribution;
	private double activity;

	/**
	 * Constructor
	 */
	public Enzyme(EnzymeTraits traits) {
		this.traits = traits;
	}

	/**
	 * evaluate()
	 */
	public void evaluate(Parameters p, Context c, State s) throws Exception {

		/**
		 * enzyme production by microbes:
		 *
		 * production = sum_{mic}(prod_function(p,c,mic_mass(t)))
		 */
		Set<String> microbeProducerNames = Microbe.getMicrobeProducing(this.getName());
		double production = 0;
		for (Microbe m : s.getMicrobes(microbeProducerNames)) {
			Function productionFunction = m.getSpecies().getEnzymeProductionMap().get(this.getName());
			production += productionFunction.execute(p, c, s, new OneVariable(m.getMass()));
		}

		/**
		 * depolymerization rate
		 *
		 * depolymerizationRate(pol, production) = \tau
		 */
		double[] discretization = traits.getBiochemicalClass().getPolymerization().getDiscretization();
		Function depolymerizationRateFunction = traits.getDepolymerizationRateFunction();
		String bcName = traits.getBiochemicalClass().getName();

		depolymerizationRate = DiscretePositiveDistribution.getZeroDiscreteDistribution(discretization,
				p.getIntegrationMethod());
		for (int i = 0; i < depolymerizationRate.getLength(); i++) {
			depolymerizationRate.getValuesY()[i] = depolymerizationRateFunction.execute(p, c, s,
					new TwoVariables(discretization[i], production));
		}

		/**
		 * enzymatic activity
		 */
		activityDistribution = DiscreteDistribution.mult(depolymerizationRate,
				s.getSubstrate().getAccessiblePool(bcName));
		activity = activityDistribution.getIntegral(traits.getDepolymerizationDomain());

	}

	/**
	 * getDepolymerizationRate()
	 */
	public DiscretePositiveDistribution getDepolymerizationRate() {
		return depolymerizationRate;
	}

	/**
	 * getTraits()
	 */
	public EnzymeTraits getTraits() {
		return this.traits;
	}

	/**
	 * getName()
	 */
	public String getName() {
		return traits.getName();
	}

	/**
	 * getActivityDistribution()
	 */
	public DiscreteDistribution getActivityDistribution() {
		return activityDistribution;
	}

	/**
	 * getActivity()
	 */
	public double getActivity() {
		return activity;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		return "Enzyme, traits: " + traits;
	}

}
