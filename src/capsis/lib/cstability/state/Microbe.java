package capsis.lib.cstability.state;

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.function.util.OneVariable;
import capsis.lib.cstability.parameter.MicrobeSpecies;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.util.Format;

/**
 * Microbe of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Microbe implements Serializable {

	// this map links each enzyme to a list of the microbes producing it
	static private Map<String, Set<String>> enzyme_microbeNames = new HashMap<>();

	private MicrobeSpecies species;
	private double mass; // g
	private Map<String, DiscreteDistribution> uptakeFluxMap; // key: bcName
	private Map<String, DiscreteDistribution> cUseEfficiencyMap; // key: bcName
	private double respiration;
	private double mortalityFlux; //

	/**
	 * Constructor
	 */
	public Microbe(MicrobeSpecies species, double mass) {
		this.species = species;
		this.mass = mass;

		// update enzyme_microbeNames
		for (String eName : species.getEnzymeProductionMap().keySet()) {
			Set<String> l = enzyme_microbeNames.get(eName);
			if (l == null) {
				l = new HashSet<String>();
				enzyme_microbeNames.put(eName, l);
			}
			if (!l.contains(this.getName()))
				l.add(this.getName());
		}

		this.uptakeFluxMap = new HashMap<>();
		this.cUseEfficiencyMap = new HashMap<>();
	}

	/**
	 * evaluate()
	 */
	public void evaluate(Parameters p, Context c, State s) throws Exception {

		if (!uptakeFluxMap.isEmpty() || !cUseEfficiencyMap.isEmpty())
			throw new Exception("Microbe.evaluate(), cannot be evaluated twice");

		respiration = 0.;
		for (String assimilationBCName : species.getAssimilationBCNames()) {
			DiscretePositiveDistribution accessiblePoolDistribution = s.getSubstrate()
					.getAccessiblePool(assimilationBCName);

			DiscreteDistribution uptakeFlux = DiscreteDistribution.apply(accessiblePoolDistribution, p, c, s,
					species.getUptakeFluxFunction(assimilationBCName));
			DiscreteDistribution cUseEfficiency = DiscreteDistribution.apply(accessiblePoolDistribution, p, c, s,
					species.getCarbonUseEfficiencyFunction(assimilationBCName));

			this.respiration += DiscreteDistribution.mult(cUseEfficiency.getComplementary(), uptakeFlux).getIntegral();

			uptakeFluxMap.put(assimilationBCName, uptakeFlux);
			cUseEfficiencyMap.put(assimilationBCName, cUseEfficiency);
		}

		Function mortalityFunction = species.getMortalityFunction();
		mortalityFlux = mortalityFunction.execute(p, c, s, new OneVariable(mass));

	}

	/**
	 * getSpecies()
	 */
	public MicrobeSpecies getSpecies() {
		return this.species;
	}

	/**
	 * getName()
	 */
	public String getName() {
		return species.getName();
	}

	/**
	 * getMass()
	 */
	public double getMass() {
		return this.mass;
	}

	/**
	 * getUptakeFlux()
	 */
	public DiscreteDistribution getUptakeFlux(String assimilationBCName) {
		return uptakeFluxMap.get(assimilationBCName);
	}

	/**
	 * getUptakeFluxes()
	 */
	public Map<String, DiscreteDistribution> getUptakeFluxes() {
		return uptakeFluxMap;
	}

	/**
	 * getIntegratedUptakeFluxes()
	 */
	public Map<String, Double> getIntegratedUptakeFluxes() {
		Map<String, Double> out = new HashMap<>();
		for (String key : uptakeFluxMap.keySet())
			out.put(key, uptakeFluxMap.get(key).getIntegral());
		return out;
	}

	/**
	 * getCUseEfficiency()
	 */
	public DiscreteDistribution getCUseEfficiency(String assimilationBCName) {
		return cUseEfficiencyMap.get(assimilationBCName);
	}

	/**
	 * getCUseEfficiencies()
	 */
	public Map<String, DiscreteDistribution> getCUseEfficiencies() {
		return cUseEfficiencyMap;
	}

	/**
	 * getRespiration()
	 */
	public double getRespiration() {
		return respiration;
	}

	/**
	 * getMortalityFlux()
	 */
	public double getMortalityFlux() {
		return mortalityFlux;
	}

	/**
	 * getMicrobeProducing()
	 */
	public static Set<String> getMicrobeProducing(String enzymeName) {
		return enzyme_microbeNames.get(enzymeName);
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		return "Microbe, " + species + ", mass: " + Format.toString(mass);
	}

}
