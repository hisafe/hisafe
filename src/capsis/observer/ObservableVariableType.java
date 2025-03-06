package capsis.lib.cstability.observer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import capsis.lib.cstability.state.Enzyme;
import capsis.lib.cstability.state.Microbe;
import capsis.lib.cstability.state.Pool;
import capsis.lib.cstability.state.PoolTransfer;
import capsis.lib.cstability.state.State;

/**
 * A type of an observable variable of C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class ObservableVariableType implements Serializable {

	public static final ObservableVariableType STATE = new ObservableVariableType("state");
	public static final ObservableVariableType MICROBE = new ObservableVariableType("microbe");
	public static final ObservableVariableType ENZYME = new ObservableVariableType("enzyme");
	public static final ObservableVariableType POOL = new ObservableVariableType("pool");
	public static final ObservableVariableType POOL_TRANSFER = new ObservableVariableType("pool_transfer");

	private static List<ObservableVariableType> availableObservableVariableType = new ArrayList<>();

	static {
		availableObservableVariableType.add(STATE);
		availableObservableVariableType.add(MICROBE);
		availableObservableVariableType.add(ENZYME);
		availableObservableVariableType.add(POOL);
		availableObservableVariableType.add(POOL_TRANSFER);
	}

	private String name;

	public ObservableVariableType(String name) {
		this.name = name;
	}

	/**
	 * checkType()
	 */
	public static void checkType(ObservableVariable ov, Object o, ObservableVariableType type) throws RuntimeException {

		Class klass = null;
		if (type == STATE)
			klass = State.class;
		else if (type == MICROBE)
			klass = Microbe.class;
		else if (type == ENZYME)
			klass = Enzyme.class;
		else if (type == POOL)
			klass = Pool.class;
		else if (type == POOL_TRANSFER)
			klass = PoolTransfer.class;

		if (!klass.isAssignableFrom(o.getClass()))
			throw new RuntimeException("ObservableVariable.checkType(), in variable name " + ov.getName() + " and type "
					+ ov.getType() + " wrong class: " + o.getClass() + " expected: " + klass);
	}

	public String getName() {
		return name;
	}

	public static List<ObservableVariableType> getAvailableObservableVariableType() {
		return availableObservableVariableType;
	}

}
