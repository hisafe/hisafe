package capsis.lib.cstability.observer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.lib.cstability.observer.observation.DistributionMapObservation;
import capsis.lib.cstability.observer.observation.DistributionObservation;
import capsis.lib.cstability.observer.observation.DoubleMapObservation;
import capsis.lib.cstability.observer.observation.DoubleObservation;
import capsis.lib.cstability.observer.observation.Observation;
import capsis.lib.cstability.state.Enzyme;
import capsis.lib.cstability.state.Microbe;
import capsis.lib.cstability.state.Pool;
import capsis.lib.cstability.state.PoolTransfer;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.util.Format;

/**
 * A variable of C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class ObservableVariable implements Serializable {

	private static Map<String, ObservableVariable> availableVariables;

	static {

		List<ObservableVariable> ovs = new ArrayList<>();

		ovs.add(new ObservableVariable("respiration", ObservableVariableType.STATE) {
			public Observation getValue(int date, Object state) {
				ObservableVariableType.checkType(this, state, ObservableVariableType.STATE);
				return new DoubleObservation(date, this, ((State) state).getRespiration());
			}
		});

		ovs.add(new ObservableVariable("mass", ObservableVariableType.POOL) {
			public Observation getValue(int date, Object pool) {
				ObservableVariableType.checkType(this, pool, ObservableVariableType.POOL);
				return new DoubleObservation(date, this, ((Pool) pool).getCarbonMass());
			}
		});

		ovs.add(new ObservableVariable("mass_distribution", ObservableVariableType.POOL) {
			public Observation getValue(int date, Object sp) {
				ObservableVariableType.checkType(this, sp, ObservableVariableType.POOL);
				return new DistributionObservation(date, this, (Pool) sp, "polymerization", "mass");
			}
		});

		ovs.add(new ObservableVariable("flux_distribution", ObservableVariableType.POOL_TRANSFER) {
			public Observation getValue(int date, Object pt) {
				ObservableVariableType.checkType(this, pt, ObservableVariableType.POOL_TRANSFER);
				return new DistributionObservation(date, this, (PoolTransfer) pt, "polymerization", "flux");
			}
		});

		ovs.add(new ObservableVariable("mass", ObservableVariableType.MICROBE) {
			public Observation getValue(int date, Object m) {
				ObservableVariableType.checkType(this, m, ObservableVariableType.MICROBE);
				return new DoubleObservation(date, this, ((Microbe) m).getMass());
			}
		});

		ovs.add(new ObservableVariable("uptake_flux_distribution_map", ObservableVariableType.MICROBE) {
			public Observation getValue(int date, Object m) {
				ObservableVariableType.checkType(this, m, ObservableVariableType.MICROBE);
				return new DistributionMapObservation(date, this, ((Microbe) m).getUptakeFluxes(), "biochemical_class",
						"polymerization", "uptake_flux");
			}
		});

		ovs.add(new ObservableVariable("uptake_flux_map", ObservableVariableType.MICROBE) {
			public Observation getValue(int date, Object m) {
				ObservableVariableType.checkType(this, m, ObservableVariableType.MICROBE);
				return new DoubleMapObservation(date, this, ((Microbe) m).getIntegratedUptakeFluxes(),
						"biochemical_class", "polymerization", "uptake_flux");
			}
		});

		ovs.add(new ObservableVariable("carbon_use_efficiency_distribution_map", ObservableVariableType.MICROBE) {
			public Observation getValue(int date, Object m) {
				ObservableVariableType.checkType(this, m, ObservableVariableType.MICROBE);
				return new DistributionMapObservation(date, this, ((Microbe) m).getCUseEfficiencies(),
						"biochemical_class", "polymerization", "carbon_use_efficiency");
			}
		});

		ovs.add(new ObservableVariable("respiration", ObservableVariableType.MICROBE) {
			public Observation getValue(int date, Object m) {
				ObservableVariableType.checkType(this, m, ObservableVariableType.MICROBE);
				return new DoubleObservation(date, this, ((Microbe) m).getRespiration());
			}
		});

		ovs.add(new ObservableVariable("mortality_flux", ObservableVariableType.MICROBE) {
			public Observation getValue(int date, Object m) {
				ObservableVariableType.checkType(this, m, ObservableVariableType.MICROBE);
				return new DoubleObservation(date, this, ((Microbe) m).getMortalityFlux());
			}
		});

		ovs.add(new ObservableVariable("depolymerization_rate_distribution", ObservableVariableType.ENZYME) {
			public Observation getValue(int date, Object e) {
				ObservableVariableType.checkType(this, e, ObservableVariableType.ENZYME);
				return new DistributionObservation(date, this, ((Enzyme) e).getDepolymerizationRate(), "polymerization",
						"depolymerization_rate");
			}
		});

		ovs.add(new ObservableVariable("activity_distribution", ObservableVariableType.ENZYME) {
			public Observation getValue(int date, Object e) {
				ObservableVariableType.checkType(this, e, ObservableVariableType.ENZYME);
				return new DistributionObservation(date, this, ((Enzyme) e).getActivityDistribution(), "polymerization",
						"activity");
			}
		});

		availableVariables = new HashMap<>();
		for (ObservableVariable ov : ovs) {
			availableVariables.put(ov.getType().getName() + "_" + ov.getName(), ov);
		}
	}

	private String name;
	private ObservableVariableType type;

	/**
	 * Constructor
	 */
	private ObservableVariable(String name, ObservableVariableType type) {
		this.name = name;
		this.type = type;
	}

	/**
	 * getValue()
	 */
	public Observation getValue(int date, Object obj) {
		return null;
	}

	/**
	 * getName()
	 */
	public String getName() {
		return name;
	}

	/**
	 * getType()
	 */
	public ObservableVariableType getType() {
		return type;
	}

	/**
	 * getObservableVariable()
	 */
	public static ObservableVariable getObservableVariable(ObservableVariableType type, String name) throws Exception {
		String key = type.getName() + "_" + name;
		if (availableVariables.containsKey(key))
			return availableVariables.get(key);
		else {
			throw new Exception("ObservableVariableType.getVariable(): unknown key " + key + ", should be "
					+ Format.toString(availableVariables.keySet(), ", "));
		}
	}

	/**
	 * getAvailableVariables()
	 */
	public static Map<String, ObservableVariable> getAvailableVariables() {
		return availableVariables;
	}

	/**
	 * getAvailableVariables():
	 */
	public static List<ObservableVariable> getAvailableVariables(ObservableVariableType type) {
		List<ObservableVariable> out = new ArrayList<>();
		for (ObservableVariable ov : availableVariables.values())
			if (ov.getType().equals(type))
				out.add(ov);
		return out;
	}

}
