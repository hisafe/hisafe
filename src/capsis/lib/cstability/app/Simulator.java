package capsis.lib.cstability.app;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.context.Timeline;
import capsis.lib.cstability.distribution.DiscreteDistribution;
import capsis.lib.cstability.distribution.DiscretePositiveDistribution;
import capsis.lib.cstability.function.util.EnzymeKernelMatrix;
import capsis.lib.cstability.observer.ObserverList;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.MicrobeSpecies;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.PoolTransferTraits;
import capsis.lib.cstability.state.Enzyme;
import capsis.lib.cstability.state.Microbe;
import capsis.lib.cstability.state.PoolTransfer;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.state.Substrate;
import capsis.lib.cstability.util.Log;
import capsis.lib.cstability.util.Matrix;
import capsis.lib.cstability.util.StateStorageListener;

/**
 * Simulator of C-STABILITY library.
 *
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class Simulator implements Serializable {

	private ObserverList observerList; // optional i.e. may be null
	private boolean observersEnabled;

	private List<StateStorageListener> stateStorageListeners;

	private Parameters parameters;
	private State currentState;

	/**
	 * Constructor: has to be followed by load
	 */
	public Simulator(Parameters parameters, State state0) {
		this.parameters = parameters;
		this.currentState = state0;

		this.observersEnabled = true; // default
	}

	/**
	 * disableObservers (): optional
	 */
	public void disableObservers() {
		observersEnabled = false;
	}

	/**
	 * execute(): Context c contains the number of steps, ObserverList ol maybe null
	 */
	public void execute(Context c, ObserverList ol, boolean run4R) throws Exception {
		Log.trace("Starting simulation...");

		Timeline tl = c.getTimeline();
		double dt = tl.getStep();

		if (observersEnabled && ol.isEmpty())
			ol.setDefaultOberverList(currentState, c, parameters);

		observerList = ol;
		if (observersEnabled && ol != null)
			ol.observe(currentState);

		List<State> stateList = new ArrayList<>();

		for (int it = 1; it < tl.getDiscretization().length; it++) {

			/**
			 * Creation of new state to update during simulation
			 */
			double date = tl.getDiscretization()[it];
			State newState = new State(date);
			if (date == (int) date)
				Log.trace("	Simulation time " + (int) date + " on " + tl.getMax() + " " + tl.getUnit());

			/**
			 * Creation of newSubstrate which will be stored in newState. Inputs are added
			 * at this stage (constructor) and for each operation occurring on the current
			 * substrate (mic, enz, transfer), the associated variations (delta) will be
			 * computed from currentSubstrate and added to newSubstrate.
			 */
			Substrate newSubstrate = new Substrate(currentState.getSubstrate());
			newSubstrate.manageInputs(parameters, c, currentState, date);

			/**
			 * Microbial biotransformations on substrate are evaluated and new microbes'
			 * states (species and mass) are added to newState
			 */
			for (Microbe currentMicrobe : currentState.getMicrobes()) {

				MicrobeSpecies currentMicrobeSpecies = currentMicrobe.getSpecies();

				/**
				 * Assimilation: for each biochemicalClass where C is taken up by microbe,
				 * assimilationFlux is incremented and C is removed from accessiblePools.
				 */
				double assimilationFlux = 0;
				for (String bcName : currentMicrobeSpecies.getAssimilationBCNames()) {

					DiscreteDistribution cUseEfficiency = currentMicrobe.getCUseEfficiency(bcName);
					DiscreteDistribution uptakeFlux = currentMicrobe.getUptakeFlux(bcName);
					assimilationFlux += DiscreteDistribution.mult(cUseEfficiency, uptakeFlux).getIntegral();

					DiscreteDistribution delta = DiscreteDistribution.mult(dt, uptakeFlux);
					newSubstrate.getAccessiblePool(bcName).substract(delta);
				}

				/**
				 * Biotransformation and mortality: for each biochemicalClass composing the
				 * microbe, C returns to the substrate according to signature
				 */
				double mortalityFlux = currentMicrobe.getMortalityFlux();
				for (String bcName : currentMicrobeSpecies.getSignatureBCNames()) {

					DiscretePositiveDistribution signature = currentMicrobeSpecies.getSignatureDistribution(bcName);

					DiscreteDistribution delta = DiscreteDistribution.mult(dt * mortalityFlux, signature);
					newSubstrate.getAccessiblePool(bcName).add(delta);
				}

				/**
				 * New microbes mass is obtained with Euler's scheme and is stored in newState
				 */
				Microbe newMicrobe = new Microbe(currentMicrobeSpecies,
						currentMicrobe.getMass() + dt * (assimilationFlux - mortalityFlux));
				newState.addMicrobe(newMicrobe);
			}

			/**
			 * PoolsTransfers: changes in local physical and chemical conditions (some
			 * simulations may have no PoolTransfers)
			 */
			if (!parameters.getPoolTransferTraitsMap().isEmpty()) {

				for (PoolTransfer pt : currentState.getPoolTransfers()) {

					PoolTransferTraits ptt = pt.getTraits();
					DiscreteDistribution delta = DiscreteDistribution.mult(dt, pt);

					newSubstrate.getPool(ptt.getBiochemicalClass().getName(), ptt.getOrigin().getKey())
							.substract(delta);
					newSubstrate.getPool(ptt.getBiochemicalClass().getName(), ptt.getArrival().getKey()).add(delta);

					newState.addPoolTransfer(new PoolTransfer(pt));
				}
			}

			/**
			 * Enzymes action on accessible substrate:
			 * 
			 * - The cumulated enzymatic depolymerization action is evaluated and stored in
			 * enzymaticMatricesMap (key: BCNames value: transformation matrix of substrate
			 * - initiated with zeros).
			 * 
			 * - Enzymes' states (traits) are added to newState
			 */
			Map<String, Matrix> enzymaticMatricesMap = new HashMap<>();

			for (Enzyme currentEnzyme : currentState.getEnzymes()) {

				/**
				 * Initialization of enzymaticMatricesMap entry if needed
				 */
				BiochemicalClass bc = currentEnzyme.getTraits().getBiochemicalClass();
				int size = bc.getPolymerization().getDiscretization().length;
				if (!enzymaticMatricesMap.containsKey(bc.getName()))
					enzymaticMatricesMap.put(bc.getName(), Matrix.zeros(size));

				/**
				 * Construction of enzymaticMatrix ( = (-Id + K).tau) and storage of
				 * enzymaticMatrix in enzymaticMatricesMap
				 */
				double[] depolymerizationRate = currentEnzyme.getDepolymerizationRate().getValuesY();
				EnzymeKernelMatrix kernel = currentEnzyme.getTraits().getKernelMatrix();
				Matrix enzymaticMatrix = kernel.clone();
				enzymaticMatrix.substract(Matrix.eye(size));
				for (int j = 0; j < size; ++j)
					enzymaticMatrix.multiplyColumn(depolymerizationRate[j], j);
				enzymaticMatricesMap.get(bc.getName()).add(enzymaticMatrix);

				/**
				 * Creation of new enzyme's state
				 */
				Enzyme newEnzyme = new Enzyme(currentEnzyme.getTraits());
				newState.addEnzyme(newEnzyme);

			}

			/**
			 * Application of the cumulated enzymatic depolymerization to the substrate.
			 */
			for (String bcName : newSubstrate.getAccessibleBCNames()) {

				if (enzymaticMatricesMap.containsKey(bcName)) {
					DiscreteDistribution delta = DiscreteDistribution.mult(dt, DiscreteDistribution.mult(
							enzymaticMatricesMap.get(bcName), currentState.getSubstrate().getAccessiblePool(bcName)));
					newSubstrate.getAccessiblePool(bcName).add(delta);
				}
			}

			/**
			 * Evaluation, observation and storage of the new state
			 */
			newState.evaluate(newSubstrate, parameters, c);
			if (observersEnabled && ol != null)
				ol.observe(newState);

			if (run4R && newState.isDateInteger())
				stateList.add(newState);

			setCurentState(newState);

			/**
			 * TODO ajouter une vérification de la conservation de masse de C
			 */

			Log.trace("Simulation completed");

		}
	}

	/**
	 * writeObservations()
	 */
	public void writeObservations(String outputDir, boolean appendMode) throws Exception {

		if (!observersEnabled)
			return;

		if (observerList == null) {
			Log.println("Simulator.writeObservations()", "observerList is not defined");
		} else {
			Log.trace("Writing observations...");
			observerList.write(outputDir, appendMode);
			Log.trace("Writing completed");
		}
	}

	/**
	 * setState0()
	 */
	public void setState0(State s0) throws Exception {
		currentState = s0;
	}

	/**
	 * setCurentState()
	 */
	public void setCurentState(State s) {
		currentState = s;
		// Tell the listeners if any
		if (stateStorageListeners != null)
			for (StateStorageListener ssl : stateStorageListeners) {
				ssl.stateStored(currentState);
			}
	}

	/**
	 * getParameters()
	 */
	public Parameters getParameters() {
		return parameters;
	}

	/**
	 * getCurrentState()
	 */
	public State getCurrentState() {
		return currentState;
	}

	/**
	 * addStateStorageListener()
	 */
	public void addStateStorageListener(StateStorageListener ssl) {
		if (stateStorageListeners == null)
			stateStorageListeners = new ArrayList<>();
		stateStorageListeners.add(ssl);
	}

	/**
	 * toString()
	 */
	@Override
	public String toString() {
		final String CR = "\n";
		StringBuffer b = new StringBuffer("--- Simulator");

		b.append(CR);
		b.append(parameters);

		b.append(CR);
		b.append("--- end-of-Simulator");

		return "" + b;
	}
}
