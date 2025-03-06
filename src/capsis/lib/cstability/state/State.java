package capsis.lib.cstability.state;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.util.Format;

/**
 * State of the model C-STABILITY.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class State implements Serializable {

	private double date;
	private Substrate substrate;
	private Map<String, Microbe> microbeMap;
	private Map<String, Enzyme> enzymeMap;
	private Map<String, PoolTransfer> poolTransferMap;
	private double respiration;

	/**
	 * Constructor
	 */
	public State() {
		poolTransferMap = new HashMap<>();
		microbeMap = new HashMap<>();
		enzymeMap = new HashMap<>();
		respiration = 0;
	}

	/**
	 * Constructor
	 */
	public State(double date) {
		this();
		this.date = date;
	}

	/**
	 * evaluate(): this method evaluates each function of microbe, enzyme and pool
	 * transfer depending on the substrate.
	 */
	public void evaluate(Substrate s, Parameters p, Context c) throws Exception {

		setSubstrate(s);

		for (Microbe m : getMicrobes()) {
			m.evaluate(p, c, this);
			respiration += m.getRespiration();
		}
		for (Enzyme e : getEnzymes()) {
			e.evaluate(p, c, this);
		}
		for (PoolTransfer pt : getPoolTransfers()) {
			pt.evaluate(p, c, this);
		}
	}

	public boolean isDateInteger() {
		return (int) date == date;
	}

	/**
	 * setSubstrate()
	 */
	public void setSubstrate(Substrate s) {
		substrate = s;
	}

	/**
	 * addMicrobe()
	 */
	public void addMicrobe(Microbe microbe) throws Exception {
		if (microbeMap.containsKey(microbe.getSpecies().getName()))
			throw new Exception("State, microbe " + microbe.getSpecies().getName() + " is defined twice.");
		microbeMap.put(microbe.getSpecies().getName(), microbe);
	}

	/**
	 * addEnzyme()
	 */
	public void addEnzyme(Enzyme enzyme) throws Exception {
		if (enzymeMap.containsKey(enzyme.getTraits().getName()))
			throw new Exception("State, enzyme " + enzyme.getTraits().getName() + " is defined twice.");
		enzymeMap.put(enzyme.getTraits().getName(), enzyme);
	}

	/**
	 * addPoolTransfer()
	 */
	public void addPoolTransfer(PoolTransfer pt) throws Exception {
		if (poolTransferMap.containsKey(pt.getTraits().getKey()))
			throw new Exception("State, poolTransfer " + pt.getTraits().getKey() + " is defined twice.");
		poolTransferMap.put(pt.getTraits().getKey(), pt);
	}

	/**
	 * getDate()
	 */
	public double getDate() {
		return date;
	}

	/**
	 * getSubstrate()
	 */
	public Substrate getSubstrate() {
		return this.substrate;
	}

	/**
	 * getMicrobes()
	 */
	public Collection<Microbe> getMicrobes() {
		return microbeMap.values();
	}

	/**
	 * getMicrobes()
	 */
	public List<Microbe> getMicrobes(Collection<String> names) {
		List<Microbe> ml = new ArrayList<>();
		for (String name : names)
			ml.add(microbeMap.get(name));
		return ml;
	}

	/**
	 * getMicrobe()
	 */
	public Microbe getMicrobe(String name) {
		return microbeMap.get(name);
	}

	/**
	 * getEnzymes()
	 */
	public Collection<Enzyme> getEnzymes() {
		return enzymeMap.values();
	}

	/**
	 * getEnzyme()
	 */
	public Enzyme getEnzyme(String name) {
		return enzymeMap.get(name);
	}

	/**
	 * getPoolTransfers()
	 */
	public Collection<PoolTransfer> getPoolTransfers() {
		return poolTransferMap.values();
	}

	/**
	 * getPoolTransfer()
	 */
	public PoolTransfer getPoolTransfer(String name) {
		return poolTransferMap.get(name);
	}

	/**
	 * getRespiration()
	 */
	public double getRespiration() {
		return respiration;
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		final String CR = "\n";
		StringBuffer b = new StringBuffer("--- State, date: " + date);

		b.append(CR);
		b.append("Substrate: " + substrate);
		b.append(CR);
		b.append("microbeMap: " + Format.toString(microbeMap));
		b.append(CR);
		b.append("enzymeMap: " + Format.toString(enzymeMap));

		b.append(CR);
		b.append("--- end-of-State");

		return "" + b;
	}
}
