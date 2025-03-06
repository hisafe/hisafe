package capsis.lib.cstability.filereader;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import capsis.lib.cstability.context.Context;
import capsis.lib.cstability.context.SubstrateInputManager;
import capsis.lib.cstability.function.Function;
import capsis.lib.cstability.observer.EnzymeObserver;
import capsis.lib.cstability.observer.MicrobeObserver;
import capsis.lib.cstability.observer.Observer;
import capsis.lib.cstability.observer.ObserverList;
import capsis.lib.cstability.observer.PoolObserver;
import capsis.lib.cstability.observer.PoolTransferObserver;
import capsis.lib.cstability.observer.StateObserver;
import capsis.lib.cstability.parameter.BiochemicalClass;
import capsis.lib.cstability.parameter.EnzymeTraits;
import capsis.lib.cstability.parameter.MicrobeSpecies;
import capsis.lib.cstability.parameter.Parameters;
import capsis.lib.cstability.parameter.PoolTransferTraits;
import capsis.lib.cstability.state.Enzyme;
import capsis.lib.cstability.state.PoolTransfer;
import capsis.lib.cstability.state.State;
import capsis.lib.cstability.state.Substrate;

/**
 * A loader for the C-Stability setup file.
 * 
 * @author J. Sainte-Marie, F. de Coligny - March 2021
 */
@SuppressWarnings("serial")
public class SetupFileLoader implements Serializable {

	public static final String NONE = "NONE";

	private String setupFileName;

	private String timeUnit;
	private int initialDate = -1;
	private int finalDate = -1;
	private double userTimeStep = -1;

	private String environmentFileName; // fc+jsm-5.3.2024

	private Parameters parameters;
	private State state0;
	private Context context;

	private ObserverList observerList;

	// Setup file format
	private List<Decodable> decodables;
	private MicrobeDecoder microbeSpeciesDecoder;
	private PoolDecoder poolDecoder;
	private SubstrateInputManager substrateInputManager;

	/**
	 * Constructor
	 */
	public SetupFileLoader(String setupFileName) {
		this.setupFileName = setupFileName;
		initFormat();
	}

	/**
	 * initFormat(): declares the expected line formats in the setup file.
	 */
	private void initFormat() {
		decodables = new ArrayList<>();
		decodables.add(new LabeledNumber()); // Before LabeledString
		decodables.add(new LabeledString());
		decodables.add(new BiochemicalClass());
		decodables.add(new EnzymeTraits());
		decodables.add(new PoolTransferTraits());

		microbeSpeciesDecoder = new MicrobeDecoder();
		decodables.add(microbeSpeciesDecoder);
		poolDecoder = new PoolDecoder();
		decodables.add(poolDecoder);

		decodables.add(new StateObserver());
		decodables.add(new PoolObserver());
		decodables.add(new MicrobeObserver());
		decodables.add(new EnzymeObserver());
		decodables.add(new PoolTransferObserver());

		substrateInputManager = new SubstrateInputManager();
		decodables.add(substrateInputManager);
	}

	/**
	 * load(): load the data in the given simulator
	 */
	public void load() throws Exception {
//	public void load(Simulator sim) throws Exception {

		this.parameters = new Parameters();
		this.context = new Context();
		this.observerList = new ObserverList();

		state0 = new State();

		try {
			BufferedReader in = new BufferedReader(new FileReader(setupFileName));
			String line;
			while ((line = in.readLine()) != null) {
				line = line.trim(); // fc-20.3.2024 fixed a detail
//				line.trim(); // not enough, see just upper
				if (line.startsWith("#") || line.length() == 0)
					continue;
				processLine(state0, line);
			}
			in.close();

		} catch (Exception e) {
			throw new Exception("Could not read setupFile: " + setupFileName, e);
		}

		/**
		 * Store the microbeSpecies in Parameters
		 */
		for (String microbeName : microbeSpeciesDecoder.getMicrobeSpeciesMap().keySet()) {
			MicrobeSpecies ms = microbeSpeciesDecoder.getMicrobeSpeciesMap().get(microbeName);
			ms.checkSignature();
			parameters.addMicrobeSpecies(ms);
		}

		context.buildTimeline(timeUnit, initialDate, finalDate, userTimeStep);

		/**
		 * Loading environment (optional)
		 */
		if (environmentFileName != null && !environmentFileName.equals(NONE))
			context.loadEnvironmentFile(environmentFileName);

		/**
		 * Creation of initial state
		 */

		/**
		 * Creation of substrate pools
		 */
		Substrate substrate = new Substrate();
		substrate.init(parameters);
		Map<String, Function> poolFunctionMap = poolDecoder.getPoolFunctionMap();
		for (String key : poolFunctionMap.keySet()) {
			// initially, substratePool is a ZeroDiscretePositiveDistribution
			// we set here Y values of substratePool from X values according to f
			String bcName = poolDecoder.getBiochemicalClassName(key);
			String accessKey = poolDecoder.getAccessibilityKey(key);
			substrate.getPool(bcName, accessKey).setValuesY(parameters, context, state0, poolFunctionMap.get(key));
		}

		/**
		 * Creation of substrate inputs substrateInputManager requires substrate to be
		 * defined and we add it to context
		 */
		context.setSubstrateInputManager(substrateInputManager);

		/**
		 * Creation of microbes state
		 */
		for (String microbeName : microbeSpeciesDecoder.getMicrobeMap().keySet()) {
			state0.addMicrobe(microbeSpeciesDecoder.getMicrobeMap().get(microbeName));
		}

		/**
		 * Creation poolTransfer state
		 */
		for (String pttName : parameters.getPoolTransferTraitsMap().keySet()) {
			PoolTransfer pt = new PoolTransfer(pttName, parameters);
			state0.addPoolTransfer(pt);
		}

		/**
		 * Creation enzyme state
		 */
		for (String enzymeName : parameters.getEnzymeTraitsMap().keySet()) {
			Enzyme e = new Enzyme(parameters.getEnzymeTraitsMap().get(enzymeName));
			state0.addEnzyme(e);
		}

		/**
		 * Evaluation of initial state and storage in simulation
		 */
		state0.evaluate(substrate, parameters, context);

//		sim.setState0(s0);
	}

	/**
	 * processLine()
	 */
	private void processLine(State s0, String line) throws Exception {

		StringBuffer decoderExceptions = new StringBuffer();

		Decodable decoded = null;
		for (Decodable prototype : decodables) {
			try {
				decoded = prototype.decode(line, parameters, context);
				break;
			} catch (Exception e) {
				// try with next decodable prototype
				decoderExceptions.append("\n" + e.toString());
				if (e.getCause() != null)
					decoderExceptions.append(", caused by: " + e.getCause());
			}
		}

		if (decoded == null)
			throw new Exception("Unexpected line in: " + setupFileName + ": " + line
					+ "\nExceptions returned by the decoders: " + decoderExceptions);

		if (decoded instanceof LabeledNumber) {
			LabeledNumber ln = (LabeledNumber) decoded;
			if (ln.getLabel().equals(CodedFlags.INITIAL_DATE)) {
				this.initialDate = ln.getInt();
				if (initialDate < 0)
					throw new Exception(
							"SetupFileLoader.processLine(): initialDate " + initialDate + " must be non negative");

			} else if (ln.getLabel().equals(CodedFlags.FINAL_DATE)) {
				this.finalDate = ln.getInt();
				if (finalDate < initialDate)
					throw new Exception("SetupFileLoader.processLine(): finalDate " + finalDate
							+ " must be strictly superior to initialDate" + initialDate);

			} else if (ln.getLabel().equals(CodedFlags.USER_TIME_STEP)) {
				this.userTimeStep = ln.getDouble();
				if (userTimeStep <= 0)
					throw new Exception(
							"SetupFileLoader.processLine(): userTimeStep " + userTimeStep + " must be positive");

			} else if (ln.getLabel().equals(CodedFlags.USER_POLYMERIZATION_STEP)) {
				parameters.setUserPolymerizationStep(ln.getDouble());
			} else {
				throw new Exception("Unknown labeledNumber: " + ln.getLabel());
			}

		} else if (decoded instanceof LabeledString) {
			LabeledString ls = (LabeledString) decoded;
			if (ls.getLabel().equals(CodedFlags.TIME_UNIT)) {
				this.timeUnit = ls.getString();
			} else if (ls.getLabel().equals(CodedFlags.INTEGRATION_METHOD)) {
				parameters.setIntegrationMethod(ls.getString());
			} else if (ls.getLabel().equals(CodedFlags.ENVIRONMENT_FILE_NAME)) {
				this.environmentFileName = ls.getString();
			} else {
				throw new Exception("Unknown labeledString " + ls.getLabel());
			}

		} else if (decoded instanceof BiochemicalClass) {
			parameters.addBiochemicalClass((BiochemicalClass) decoded);

		} else if (decoded instanceof EnzymeTraits) {
			parameters.addEnzymeTraits((EnzymeTraits) decoded);

		} else if (decoded instanceof PoolTransferTraits) {
			parameters.addPoolTransferTraits((PoolTransferTraits) decoded);

		} else if (decoded instanceof Observer) {
			observerList.addObserver((Observer) decoded);

		}
	}

	/**
	 * getParameters()
	 */
	public Parameters getParameters() {
		return parameters;
	}

	/**
	 * getState0()
	 */
	public State getState0() {
		return state0;
	}

	/**
	 * getContext()
	 */
	public Context getContext() {
		return context;
	}

	/**
	 * getObserverList()
	 */
	public ObserverList getObserverList() {
		return observerList;
	}
}
