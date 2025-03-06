package capsis.lib.cstability.state;

import java.io.Serializable;

import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.SubstrateAccessibility;

/**
 * A pool key of C-STABILITY
 * 
 * @author J. Sainte-Marie - January 2024
 */
@SuppressWarnings("serial")
public class PoolKey implements Serializable {

	public final static String SEP = "_";

	private BiochemicalClass biochemicalClass;
	private SubstrateAccessibility accessibility;
	private String key;

	public PoolKey(BiochemicalClass bc, SubstrateAccessibility a) {
		biochemicalClass = bc;
		accessibility = a;
		key = generateKey(bc.getName(), a.getKey());
	}

	static public String generateKey(String bcName, String aName) {
		return bcName + SEP + aName;
	}

	@Override
	public boolean equals(Object o) {

		if (!(o instanceof PoolKey))
			return false;

		return this.key.equals(((PoolKey) o).key);
	}

	public SubstrateAccessibility getAccessibility() {
		return accessibility;
	}

	public BiochemicalClass getBiochemicalClass() {
		return biochemicalClass;
	}

	public String getAccessibilityName() {
		return accessibility.getKey();
	}

	public String getBiochemicalClassName() {
		return biochemicalClass.getName();
	}

	public String getKey() {
		return key;
	}

	@Override
	public String toString() {
		return key;
	}
}
