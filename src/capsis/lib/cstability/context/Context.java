package capsis.lib.cstability.context;

import java.io.Serializable;

/**
 * Context of the model C-STABILITY
 *
 * @author J. Sainte-Marie, F. de Coligny - February 2021
 */
@SuppressWarnings("serial")
public class Context implements Serializable {

//	private String timeUnit;
//	private int initialDate = -1;
//	private int finalDate = -1;
//	private double userTimeStep = -1;

	private Timeline timeline;

	private SubstrateInputManager substrateInputManager;

	// fc+jsm-5.3.2024
	private EnvironmentFileLoader environmentFileLoader;

	/**
	 * Constructor
	 */
	public Context() {
	}

	/**
	 * buildTimeline()
	 */
	public void buildTimeline(String timeUnit, int initialDate, int finalDate, double userTimeStep) throws Exception {
		this.timeline = new Timeline(timeUnit, initialDate, finalDate, userTimeStep);
	}

	/**
	 * changeTimeline(): replaces the current timeline keeping timeUnit and
	 * userTimeStep. In order to run a new simulation with same environnement but
	 * for a new date range.
	 */
	public void changeTimeline(int initialDate, int finalDate) throws Exception {
		String timeUnit = timeline.getUnit();
		double userTimeStep = timeline.getStep();
		this.timeline = new Timeline(timeUnit, initialDate, finalDate, userTimeStep);

	}

	/**
	 * loadEnvironmentFile()
	 */
	public void loadEnvironmentFile(String environmentFileName) throws Exception {

		environmentFileLoader = new EnvironmentFileLoader(environmentFileName, timeline);
		environmentFileLoader.load();

	}

	/**
	 * setTimeUnit()
	 */
//	public void setTimeUnit(String timeUnit) throws Exception {
//		this.timeUnit = timeUnit;
//	}

	/**
	 * setInitialDate()
	 */
//	public void setInitialDate(int initialDate) throws Exception {
//		if (initialDate < 0)
//			throw new Exception("Context.setInitialDate(): initialDate " + initialDate + " must be non negative");
//		this.initialDate = initialDate;
//	}

	/**
	 * setFinalDate()
	 */
//	public void setFinalDate(int finalDate) throws Exception {
//		if (finalDate < initialDate)
//			throw new Exception("Context.setFinalDate(): finalDate " + finalDate
//					+ " must be strictly superior to initialDate" + initialDate);
//		this.finalDate = finalDate;
//	}

	/**
	 * setUserTimeStep()
	 */
//	public void setUserTimeStep(double userTimeStep) throws Exception {
//		if (userTimeStep <= 0)
//			throw new Exception("Context.setUserTimeStep(): userTimeStep " + userTimeStep + " must be positive");
//		this.userTimeStep = userTimeStep;
//	}

	/**
	 * setSubstrateInputManager()
	 */
	public void setSubstrateInputManager(SubstrateInputManager substrateInputManager) {
		this.substrateInputManager = substrateInputManager;
	}

	/**
	 * getTimeUnit()
	 */
//	public String getTimeUnit() {
//		return timeUnit;
//	}

	/**
	 * getInitialDate()
	 */
//	public int getInitialDate() {
//		return initialDate;
//	}

	/**
	 * getFinalDate()
	 */
//	public int getFinalDate() {
//		return finalDate;
//	}

	/**
	 * getUserTimeStepv
	 */
//	public double getUserTimeStep() {
//		return userTimeStep;
//	}

	/**
	 * getTimeline()
	 */
	public Timeline getTimeline() {
		return timeline;
	}

	/**
	 * getSubstrateInputManager()
	 */
	public SubstrateInputManager getSubstrateInputManager() {
		return substrateInputManager;
	}

	public EnvironmentFileLoader getEnvironmentFileLoader() throws Exception {
		if (environmentFileLoader == null)
			throw new Exception("Context, environmentFileLoader is not available, please check setup file");
		return environmentFileLoader;
	}

	@Override
	/**
	 * toString()
	 */
	public String toString() {
		String CR = "\n";
		StringBuffer b = new StringBuffer("--- Context");

//		b.append(CR);
//		b.append("timeUnit: " + timeUnit);
//
//		b.append(CR);
//		b.append("initialDate: " + initialDate);
//
//		b.append(CR);
//		b.append("finalDate: " + finalDate);
//
//		b.append(CR);
//		b.append("userTimeStep: " + userTimeStep);

		b.append(CR);
		b.append("timeline: " + timeline);

		b.append(CR);
		b.append("--- end-of-Context");

		return "" + b;
	}

}
