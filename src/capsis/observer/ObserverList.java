package capsis.lib.cstability.observer;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.context.Timeline;
import capsis.lib.cstability.observer.key.EnzymeObserverKey;
import capsis.lib.cstability.observer.key.MicrobeObserverKey;
import capsis.lib.cstability.observer.key.ObserverKey;
import capsis.lib.cstability.observer.key.PoolObserverKey;
import capsis.lib.cstability.observer.key.PoolTransferObserverKey;
import capsis.lib.cstability.observer.observation.Observation;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.PoolTransferTraits;
import capsis.lib.cstability.state.Enzyme;
import capsis.lib.cstability.state.Microbe;
import capsis.lib.cstability.state.Pool;
import capsis.lib.cstability.state.PoolTransfer;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.state.Substrate;

/**
 * The observerList which stores observers.
 *
 * @author J. Sainte-Marie, F. de Coligny - April 2021
 */
@SuppressWarnings("serial")
public class ObserverList implements Serializable {

	private List<Observer> observers;

	/**
	 * Constructor
	 */
	public ObserverList() {
		observers = new ArrayList<>();
	}

	/**
	 * observe()
	 */
	public void observe(State s) throws Exception {

		if (!s.isDateInteger())
			return;

		int d = (int) s.getDate();

		for (Observer o : observers) {
			o.observe(s, d);
		}
	}

	/**
	 * setDefaultObserverList()
	 */
	public void setDefaultOberverList(State s, Context c, Parameters p) throws Exception {

		Timeline tl = c.getTimeline();
		List<Integer> datesToObserve = new ArrayList<>();
		for (int i = tl.getMin(); i <= tl.getMax(); ++i) {
			datesToObserve.add(i);
		}

		Map<String, ObservableVariable> availableVariables = ObservableVariable.getAvailableVariables();

		for (String key : availableVariables.keySet()) {
			ObservableVariable av = availableVariables.get(key);
			if (av.getType().equals(ObservableVariableType.STATE)) {
				String variableName = av.getName();
				observers.add(new StateObserver(variableName, datesToObserve));
			}
		}

		for (Microbe m : s.getMicrobes()) {
			for (String key : availableVariables.keySet()) {
				ObservableVariable av = availableVariables.get(key);
				if (av.getType().equals(ObservableVariableType.MICROBE)) {
					String variableName = av.getName();
					observers.add(new MicrobeObserver(m.getSpecies().getName(), variableName, datesToObserve));
				}
			}
		}

		for (Enzyme e : s.getEnzymes()) {
			for (String key : availableVariables.keySet()) {
				ObservableVariable av = availableVariables.get(key);
				if (av.getType().equals(ObservableVariableType.ENZYME)) {
					String variableName = av.getName();
					observers.add(new EnzymeObserver(e.getTraits().getName(), variableName, datesToObserve));
				}
			}
		}

		for (PoolTransfer pt : s.getPoolTransfers()) {
			for (String key : availableVariables.keySet()) {
				ObservableVariable av = availableVariables.get(key);
				if (av.getType().equals(ObservableVariableType.POOL_TRANSFER)) {
					String variableName = av.getName();
					PoolTransferTraits ptt = pt.getTraits();
					observers.add(new PoolTransferObserver(ptt.getBiochemicalClass().getName(), ptt.getOriginName(),
							ptt.getArrivalName(), variableName, datesToObserve, p));
				}
			}
		}

		Substrate sub = s.getSubstrate();
		Set<String> bcNames = sub.getAccessibleBCNames();
		for (String bcName : bcNames) {
			Pool asp = sub.getAccessiblePool(bcName);
			String accessibilityName = asp.getAccessibility().getKey();
			for (String key : availableVariables.keySet()) {
				ObservableVariable av = availableVariables.get(key);
				if (av.getType().equals(ObservableVariableType.POOL)) {
					String variableName = av.getName();
					observers.add(new PoolObserver(bcName, accessibilityName, variableName, datesToObserve));
				}
			}
			List<Pool> ispl = sub.getInaccessiblePools(bcName);
			for (Pool isp : ispl) {
				accessibilityName = isp.getAccessibility().getKey();
				for (String key : availableVariables.keySet()) {
					ObservableVariable av = availableVariables.get(key);
					if (av.getType().equals(ObservableVariableType.POOL)) {
						String variableName = av.getName();
						observers.add(new PoolObserver(bcName, accessibilityName, variableName, datesToObserve));
					}
				}
			}
		}
	}

	/**
	 * isEmpty()
	 */
	public boolean isEmpty() {
		if (observers.size() == 0) {
			return true;
		} else {
			return false;
		}
	}

	/**
	 * addObserver()
	 */
	public void addObserver(Observer o) {
		observers.add(o);
	}

	/**
	 * getObserverKeys()
	 */
	public List<ObserverKey> getObserverKeys() {
		List<ObserverKey> l = new ArrayList<>();
		for (Observer o : observers)
			l.add(o.getKey());
		return l;
	}

	/**
	 * getObservations()
	 */
	public List<Observation> getObservations(ObserverKey observerKey) {
		for (Observer o : observers)
			if (o.getKey().equals(observerKey))
				return o.getObservations();
		return null;
	}

	/**
	 * getObservations()
	 */
	public List<Observation> getObservations(String observableVariableKeyLabel, ObservableVariable ov) {
		return getObservations(getObserverKey(observableVariableKeyLabel, ov));
	}

	/**
	 * getObservableVariableTypes()
	 */
	public List<ObservableVariableType> getObservableVariableTypes() {
		return ObservableVariableType.getAvailableObservableVariableType();
	}

	/**
	 * getObservableVariable()
	 */
	public List<ObservableVariable> getObservableVariable(ObservableVariableType ovt) {
		return ObservableVariable.getAvailableVariables(ovt);
	}

	/**
	 * getObservableVariableKeyLabel()
	 */
	public List<String> getObservableVariableKeyLabel(ObservableVariableType ovt) {
		Set<String> set = new HashSet<>();

		for (Observer o : observers)
			if (o.getObservableVariable().getType().equals(ovt))
				if (ovt.equals(ObservableVariableType.STATE))
					set.add(((StateObserver) o).getKey().getLabel());
				else if (ovt.equals(ObservableVariableType.ENZYME))
					set.add(((EnzymeObserver) o).getKey().getLabel());
				else if (ovt.equals(ObservableVariableType.MICROBE))
					set.add(((MicrobeObserver) o).getKey().getLabel());
				else if (ovt.equals(ObservableVariableType.POOL))
					set.add(((PoolObserver) o).getKey().getLabel());
				else if (ovt.equals(ObservableVariableType.POOL_TRANSFER))
					set.add(((PoolTransferObserver) o).getKey().getLabel());

		List<String> l = new ArrayList<>();
		for (String s : set)
			l.add(s);

		return l;
	}

	/**
	 * getObserverKey()
	 */
	private ObserverKey getObserverKey(String observableVariableKeyLabel, ObservableVariable ov) {
		for (ObserverKey ok : getObserverKeys()) {
			ObservableVariable ov2 = ok.getObservableVariable();
			if (ov2.equals(ov)) {
				if (ov2.getType().equals(ObservableVariableType.STATE)) {
					return ok;
				} else if (ov2.getType().equals(ObservableVariableType.MICROBE)) {
					MicrobeObserverKey mok = (MicrobeObserverKey) ok;
					if (mok.getLabel().equals(observableVariableKeyLabel))
						return ok;
				} else if (ov2.getType().equals(ObservableVariableType.ENZYME)) {
					EnzymeObserverKey eok = (EnzymeObserverKey) ok;
					if (eok.getEnzymeName().equals(observableVariableKeyLabel))
						return ok;
				} else if (ov2.getType().equals(ObservableVariableType.POOL)) {
					PoolObserverKey pok = (PoolObserverKey) ok;
					if (pok.getLabel().equals(observableVariableKeyLabel))
						return ok;
				} else if (ov2.getType().equals(ObservableVariableType.POOL_TRANSFER)) {
					PoolTransferObserverKey ptok = (PoolTransferObserverKey) ok;
					if (ptok.getLabel().equals(observableVariableKeyLabel))
						return ok;
				}
			}
		}
		return null;
	}

	/**
	 * write()
	 */
	public void write(String outputDir, boolean appendMode) throws Exception {
		for (Observer o : observers)
			o.write(outputDir, appendMode);
	}
}
