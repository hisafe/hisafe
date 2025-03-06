package capsis.lib.cstability.state;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.context.poolinput.PoolInput;
import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.SubstrateAccessibility;
import capsis.lib.cstability.util.Format;

/**
 * Substrate contains pools.
 * 
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Substrate implements Serializable {

	private Map<PoolKey, Pool> poolMap;

	/**
	 * Constructor
	 */
	public Substrate() {
		this.poolMap = new HashMap<>();
	}

	/**
	 * Constructor for duplication
	 */
	public Substrate(Substrate substrate) throws Exception {
		this();
		for (PoolKey pk : substrate.poolMap.keySet())
			this.poolMap.put(pk, substrate.poolMap.get(pk).copy());
	}

	/**
	 * manageInputs() : Add substrate inputs
	 */
	public void manageInputs(Parameters p, Context c, State s, double date) throws Exception {

		List<Pool> poolList = new ArrayList<>(poolMap.values());

		for (Pool pool : poolList) {
			String poolKey = pool.getPoolKey().getKey();
			PoolInput pi = c.getSubstrateInputManager().getPoolInput(p, c, s, poolKey, date);
			if (pi != null) {
				// pool = pool + dt*pi
				DiscreteDistribution temp = DiscreteDistribution.mult(c.getTimeline().getStep(), pi);
				pool.add(temp);
			}
		}
	}

	/**
	 * init(): initialize empty pools of substrate according to parameters
	 */
	public void init(Parameters p) throws Exception {
		for (String bcName : p.getSubstrateAccessibilityMap().keySet()) {
			List<SubstrateAccessibility> list = p.getSubstrateAccessibilityMap().get(bcName);
			BiochemicalClass bc = p.getBiochemicalClassMap().get(bcName);
			for (SubstrateAccessibility sa : list) {
				Pool sp = Pool.getEmptySubstratePool(bc, sa, p.getIntegrationMethod());
				this.addPool(bcName, sp);
			}
		}
	}

	/**
	 * addPool()
	 */
	public void addPool(String bcName, Pool pool) throws Exception {
		poolMap.put(pool.getPoolKey(), pool);
	}

	/**
	 * getPool()
	 */
	public Pool getPool(PoolKey poolKey) throws Exception {
		return getPool(poolKey.getBiochemicalClassName(), poolKey.getAccessibilityName());
	}

	/**
	 * getPool()
	 */
	public Pool getPool(String bcName, String accessKey) throws Exception {

		for (PoolKey pk : poolMap.keySet())
			if (pk.getAccessibility().getKey().equals(accessKey) && pk.getBiochemicalClass().getName().equals(bcName))
				return poolMap.get(pk);

		throw new Exception(
				"Substrate.getPool, pool does not exist for bcName " + bcName + " and accessKey " + accessKey);
	}

	/**
	 * getAccessiblePool()
	 */
	public Pool getAccessiblePool(String bcName) throws Exception {

		for (PoolKey pk : poolMap.keySet())
			if (pk.getAccessibility().isAccessible() && pk.getBiochemicalClass().getName().equals(bcName))
				return poolMap.get(pk);

		throw new Exception("Substrate.getAccessiblePool, accessible pool does not exist for: " + bcName);
	}

	/**
	 * getAccessibleBCNames()
	 */
	public Set<String> getAccessibleBCNames() {
		Set<String> set = new HashSet<>();
		for (PoolKey pk : poolMap.keySet())
			if (pk.getAccessibility().isAccessible())
				set.add(pk.getBiochemicalClass().getName());
		return set;
	}

	/**
	 * getInaccessiblePools()
	 */
	public List<Pool> getInaccessiblePools(String bcName) {
		List<Pool> pools = new ArrayList<>();
		for (PoolKey pk : poolMap.keySet())
			if (!pk.getAccessibility().isAccessible() && pk.getBiochemicalClass().getName().equals(bcName))
				pools.add(poolMap.get(pk));

		return pools;
	}

	/**
	 * getPoolKeys()
	 */
	public Set<PoolKey> getPoolKeys() {
		return poolMap.keySet();
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {

		String CR = "\n";
		StringBuffer b = new StringBuffer("--- Substrate");

		b.append(CR);
		b.append("poolMap: " + Format.toString(poolMap));

		b.append(CR);
		b.append("--- end-of-Substrate");

		return b.toString();
	}

}
