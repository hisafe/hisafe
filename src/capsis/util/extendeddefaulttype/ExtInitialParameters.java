/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */
package capsis.util.extendeddefaulttype;

import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.gui.MainFrame;
import capsis.kernel.AbstractSettings;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.util.EnumProperty;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.Log;
import repicea.app.AbstractGenericTask;
import repicea.app.UseModeProvider;
import repicea.gui.genericwindows.REpiceaProgressBarDialog;
import repicea.lang.MemoryWatchDog;
import repicea.simulation.ApplicationScaleProvider;
import repicea.simulation.REpiceaPredictor;
import repicea.simulation.climate.REpiceaClimateGenerator.ClimateChangeScenario;
import repicea.simulation.climate.REpiceaClimateGenerator.ClimateChangeScenarioProvider;
import repicea.simulation.climate.REpiceaClimateGenerator.RepresentativeConcentrationPathway;
import repicea.simulation.covariateproviders.plotlevel.ManagementTypeProvider;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * The AbstractInitialParameters class handles all the settings of a simulation
 * with many plots.
 * 
 * @author Mathieu Fortin - October 2013
 *
 * @param <C> a CompositeTreeList type
 * @param <S> an AbstractSimulationSettings type
 * @param <CP> an AbstractConstantInitialParameters type
 */
@SuppressWarnings({"rawtypes" })
public abstract class ExtInitialParameters<C extends ExtCompositeStand, S extends ExtSimulationSettings, CP extends ExtConstantInitialParameters>
							extends AbstractSettings implements InitialParameters, 
							ApplicationScaleProvider, 
							ManagementTypeProvider,
							UseModeProvider,
							ClimateChangeScenarioProvider {
	
	final static class Backup {
		final ExtRecordReader reader;
		final String filename;
		final ExtCompositeStand initScene;
		final int initialDateYr;
		final ExtInitialParameters parms;

		Backup(ExtInitialParameters parms) {
			this.parms = parms;
			reader = parms.getRecordReader();
			filename = parms.getFilename();
			initScene = parms.getInitScene();
			initialDateYr = parms.getInitialDateYr();
			parms.backupPredictors = parms.getInitScene().getPredictors();
		}

		void restoreBackup() {
			parms.setRecordReader(reader);
			parms.setFilename(filename);
			parms.initStand = initScene;
			parms.setInitialDateYear(initialDateYr);
			parms.backupPredictors = null;
		}

	}

	public final class ExtInitializationTask extends AbstractGenericTask {

		private ExtInitializationTask() {
		}

		@Override
		protected void doThisJob() throws Exception {

			try {
				int stratumId = getRecordReader().getSelectedGroupId();
				displayThisMessage(MessageID.ReadingFile.toString());
				loadInitCompositeStand(stratumId);
				if (!isCancelled()) {
					createRealizations(this);
				}
				if (!isCancelled()) {
					initializeModel(this);
				}
				if (isCancelled()) {
					initStand = null;
				}
			} catch (Exception exc) {
				Log.println(Log.ERROR, "AbstractInitialParameters ()", "Error during inventory load", exc);
				initStand = null; // to free the memory from this stand
				throw exc;
			}
		}

		private void displayThisMessage(String message) {
			firePropertyChange(REpiceaProgressBarDialog.LABEL, "", message);
		}

		private void setToThisValue(int value) {
			firePropertyChange(REpiceaProgressBarDialog.PROGRESS, Double.NaN, value);
		}

	}

	public static enum MessageID implements TextableEnum {
		Initialization("Initialization", "Initialisation"), CreatingRealization("Creating realization",
				"Cr\u00E9ation de la r\u00E9alisation"), ReadingFile("Reading inventory file",
						"Lecture du fichier d'inventaire"), UpdatingStandVariables("Updating realization",
								"Mise \u00E0 jour de la r\u00E9alisation");

		MessageID(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}

		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}
	}
	
	
	public static class PossibleValuesList<M> extends ArrayList<M> {
		
		final M defaultValue;
		
		public PossibleValuesList(M defaultValue, List<M> possibleValues) {
			this.defaultValue = defaultValue;
			addAll(possibleValues);
		}
	}


	private String projectName = "Unnamed"; // default value
	private UseMode useMode;
	private String filename;
	private int iInitialSimulationDateYr;
	private int initialAgeYr;
	private int iNumberOfIterations;
	private ExtTicketDispenser treeIdDispenser;

	private Map<String, REpiceaPredictor> backupPredictors = null;
	
	protected final Map<Class, PossibleValuesList> possibleValuesMap;
	
	private transient ExtRecordReader recordReader;
	private S simulationSettings;
	private CP constantInitialParameters;

	protected transient C initStand;
	protected boolean bForceResidualStand;

	private ApplicationScale scale; // default value
	private ManagementType managementType; // default value

	private boolean isStochastic = false; // default value
	
	private ClimateChangeScenario climateChangeScenario;


	/**
	 * General constructor. 
	 */
	protected ExtInitialParameters() {
		treeIdDispenser = new ExtTicketDispenser();
		possibleValuesMap = new HashMap<Class, PossibleValuesList>();
		setPossibleValuesMap();
		if (possibleValuesMap.containsKey(ApplicationScale.class)) {
			scale = (ApplicationScale) possibleValuesMap.get(ApplicationScale.class).defaultValue;
		}
		if (possibleValuesMap.containsKey(ManagementType.class)) {
			managementType = (ManagementType) possibleValuesMap.get(ManagementType.class).defaultValue;
		}
		if (possibleValuesMap.containsKey(RepresentativeConcentrationPathway.class)) {
			climateChangeScenario = (RepresentativeConcentrationPathway) possibleValuesMap.get(RepresentativeConcentrationPathway.class).defaultValue;
		}
		
		setSimulationSettings(instantiateSimulationSettings());
		setRecordReader(instantiateRecordReader());
		setConstantInitialParameters(instantiateConstantInitialParameters());
	}

	protected abstract S instantiateSimulationSettings();
	
	protected abstract ExtRecordReader instantiateRecordReader();
	
	protected abstract CP instantiateConstantInitialParameters();
	
	/**
	 * This method is used to set the possible values in terms
	 * of ApplicationScale and ManagementType
	 */
	protected abstract void setPossibleValuesMap();


	/**
	 * Set the climate scenario for this simulation.
	 * @param a ClimateChangeScenario instance
	 */
	public void setClimateChangeScenario(ClimateChangeScenario climateChangeScenario) {
		if (!getPossibleValues(climateChangeScenario.getClass()).contains(climateChangeScenario)) {
			throw new InvalidParameterException("This climate change scenario is not accepted for this model! " + climateChangeScenario.toString());
		}
		this.climateChangeScenario = climateChangeScenario;
	}

	/**
	 * Provide the climate scenario for this simulation.
	 * @return a ClimateChangeScenario instance
	 */
	@Override
	public ClimateChangeScenario getClimateChangeScenario() {return climateChangeScenario;}
	
	public List getPossibleValues(Class clazz) {
		List possibleValues = new ArrayList();
		if (possibleValuesMap.containsKey(clazz)) {
			possibleValues.addAll(possibleValuesMap.get(clazz));
		}
		return possibleValues;
	}
	
	protected final void setBasicFeaturesOfInitialStand() {
		initStand.setSourceName(getFilename());
		initStand.setDate(getInitialDateYr());
		initStand.setApplicationScale(getApplicationScale());
		initStand.setManagementType(getManagementType());
		initStand.setClimateChangeScenario(getClimateChangeScenario());
	}
	
	/**
	 * This method returns the project name as specified in the initial
	 * parameter dialog.
	 * 
	 * @return a String
	 */
	public String getProjectName() {
		return projectName;
	}

	protected void setProjectName(String projectName) {
		this.projectName = projectName;
	}

	protected void recordPossibleValues(Object defaultValue, Object... valueList) {
		List possibleValues = new ArrayList();
		for (Object o : valueList) {
			possibleValues.add(o);
		}
		PossibleValuesList pv = new PossibleValuesList(defaultValue, possibleValues);
		possibleValuesMap.put(defaultValue.getClass(), pv);
	}
	
	
	
	/**
	 * This method sets the application scale. By default the scale is set to
	 * the stand
	 * 
	 * @param scale
	 *            an ApplicationScale enum
	 */
	public void setApplicationScale(ApplicationScale scale) {
		if (!getPossibleValues(ApplicationScale.class).contains(scale)) {
			throw new InvalidParameterException("This application scale is not accepted for this model!");
		}
		this.scale = scale;
	}

	/**
	 * This method returns the scale at which the model applies.
	 * 
	 * @return an ApplicationScale enum
	 */
	@Override
	public ApplicationScale getApplicationScale() {return scale;}

	public void setManagementType(ManagementType managementType) {
		if (!getPossibleValues(ManagementType.class).contains(managementType)) {
			throw new InvalidParameterException("This management type is not accepted for this model!");
		}
		this.managementType = managementType;
	}

	@Override
	public ManagementType getManagementType() {return managementType;}

	/**
	 * This method returns the initial composite stand for any simulation.
	 * 
	 * @return a CompositeTreeList-derived instance
	 */
	public C getInitScene() {
		return initStand;
	}

	/**
	 * This method forces the initial stand to be an intervention results. The
	 * implementation depends on the model.
	 * 
	 * @param b
	 *            a boolean
	 */
	public void setForceInterventionResultEnabled(boolean b) {
		bForceResidualStand = b;
	}

	public ExtTicketDispenser getTreeIdDispenser() {
		return treeIdDispenser;
	}

	public S getSimulationSettings() {
		return simulationSettings;
	}

	public void setSimulationSettings(S generalSettings) {
		this.simulationSettings = generalSettings;
	}

	public void setFilename(String filename) {
		this.filename = filename;
	}

	public String getFilename() {
		return this.filename;
	}

	
	protected void createRealizations(ExtInitializationTask task) {
		for (int i = 1; i < getNumberOfRealizations(); i++) {
			task.displayThisMessage(ExtInitialParameters.MessageID.CreatingRealization.toString() + " " + i);
			task.setToThisValue((i + 1) * 100 / getNumberOfRealizations());
			for (ExtPlot plot : initStand.getRealization(i - 1).getPlots()) {
				if (task.isCancelled()) {
					return;
				}
				ExtPlot oNewStand = plot.getHeavyCloneForInitialization();

				oNewStand.setInitialScene(true); // override the getHeavyClone
													// method to make sure the
													// stand is correctly
													// identified as an
													// initialScene
				oNewStand.setInterventionResult(plot.isInterventionResult()); // override the getHeavyClone method to make sure 
																			  // the stand is identified as an intervention result
				initStand.getRealization(i).addPlot(plot.getId(), oNewStand);
				MemoryWatchDog.checkAvailableMemory();
			}
		}
	}

	protected Backup getBackupForAdditionalData() {
		return new Backup(this);
	}

	/**
	 * This method loads the initial stratum (or stand) that may have one or
	 * many plots.
	 * 
	 * @param stratumId
	 *            the id of the stratum (or stand) that is to be loaded
	 * @throws Exception
	 */
	protected abstract void loadInitCompositeStand(int stratumId) throws Exception;

	protected abstract void setPredictors();

	protected void setGroups() {
		GrouperManager.getInstance().buildGroupers(getConstantInitialParameters().groupTagSpecimen, TreeList.GROUP_ALIVE_TREE);
	}

	/**
	 * This method initializes some parameters before making the first step
	 * visible.
	 * 
	 * @throws Exception
	 */
	protected void initializeModel(ExtInitializationTask task) throws Exception {
		ExtCompositeStand initCompositeStand = getInitScene();
		if (initCompositeStand == null) {
			throw new NullPointerException();
		}

		initCompositeStand.setSpeciesGroupTag(getConstantInitialParameters().groupTagMap);

		setMonteCarloSettings();
		if (backupPredictors != null) {
			initCompositeStand.setPredictors(backupPredictors);
		} else {
			setPredictors();
		}

		if (isGuiEnabled()) {
			if (getInitScene().getStratumName() != null) {
				Log.println("Processing stratum : " + getInitScene().getStratumName());
			}
		}

		for (int i = 0; i < getNumberOfRealizations(); i++) {
			task.displayThisMessage(ExtInitialParameters.MessageID.UpdatingStandVariables.toString() + " " + i);
			task.setToThisValue((i + 1) * 100 / getNumberOfRealizations());
			getInitScene().getRealization(i).initialize(this);
			if (task.isCancelled()) {
				return;
			}

			MemoryWatchDog.checkAvailableMemory();
		}

	}

	protected void setMonteCarloSettings() {
		if (!isStochastic()) {
			getSimulationSettings().setDefaultMonteCarloSettings(false); // if
																			// deterministic
																			// no
																			// variability
																			// source
																			// enabled
		}
	}

	public void setInitialDateYear(int i) {iInitialSimulationDateYr = i;}

	public int getInitialDateYr() {return iInitialSimulationDateYr;}

	public int getInitialAgeYr() {return initialAgeYr;}
	
	public void setInitialAgeYr(int i) {initialAgeYr = i;}
	
	
	/**
	 * This method returns the constant initial parameters for this simulation.
	 * 
	 * @return a ConstantInitialParameters-derived instance
	 */
	public CP getConstantInitialParameters() {
		return constantInitialParameters;
	}

	/**
	 * This method sets the constant initial parameters of the simulation. 
	 * It also sets the groups for CAPSIS GroupBuilder instance. NOTE: this 
	 * method should be called in the constructor of derived classes.
	 * 
	 * @param constantInitialParameters
	 *            an ExtConstantInitialParameters-derived instance
	 */
	protected void setConstantInitialParameters(CP constantInitialParameters) {
		this.constantInitialParameters = constantInitialParameters;
		setGroups();
	}

	// public boolean isGuiEnabled() {return (useMode == UseMode.GUI_MODE);}

	/**
	 * This method set the number of Monte Carlo realizations.
	 * 
	 * @param i
	 *            an Integer
	 */
	public void setNumberOfRealizations(int i) {
		iNumberOfIterations = i;
	}

	/**
	 * This method returns the number of Monte Carlo realizations.
	 * 
	 * @return an Integer
	 */
	public int getNumberOfRealizations() {
		return iNumberOfIterations;
	}

	/**
	 * This method returns true if the simulation mode is set to stochastic or
	 * false if the default deterministic mode is enabled.
	 * 
	 * @return a boolean
	 */
	public boolean isStochastic() {
		return isStochastic;
	}

	/**
	 * This method sets the simulation mode.
	 * 
	 * @param isStochastic
	 *            true to use the stochastic simulation mode or false to use the
	 *            deterministic mode (default)
	 */
	public void setStochastic(boolean isStochastic) {
		this.isStochastic = isStochastic;
	}

	public String getInventoryPath() {
		return getSimulationSettings().getInventoryPath();
	}

	/**
	 * This method requests the initialization of the RecordInstantiator. NOTE:
	 * To be called in GUI mode only.
	 */
	public void initImport() throws Exception {
		getRecordReader().initGUIMode(MainFrame.getInstance(), useMode, getFilename());
	}

	/**
	 * This method returns a RecordReader instance that can read the
	 * user-specified file.
	 * 
	 * @return a RecordReader instance
	 */
	public ExtRecordReader getRecordReader() {
		return recordReader;
	}

	protected void setRecordReader(ExtRecordReader<?> recordReader) {
		this.recordReader = recordReader;
	}

	public EnumProperty getSpeciesGroupTagSpecimen() {
		return getConstantInitialParameters().groupTagSpecimen;
	}

	public EnumProperty getSpeciesGroupTag(int speciesID) {
		return getConstantInitialParameters().groupTagMap.get(speciesID);
	}

	/**
	 * This method returns a vector of simulation dates.
	 * 
	 * @return a Vector of integer instances that represent the simulation step
	 *         dates
	 */
	@Deprecated
	protected List<Integer> getSimulationDates() {
		List<Integer> outputVec = new ArrayList<Integer>();
		int maxYear = getInitialDateYr()
				+ getSimulationSettings().getMaxNumberOfSteps() * getSimulationSettings().getTimeStep();
		for (int year = getInitialDateYr(); year <= maxYear; year += getSimulationSettings().getTimeStep()) {
			outputVec.add(year);
		}
		return outputVec;
	}

	@Override
	public UseMode getUseMode() {
		return useMode;
	}

	@Override
	public void setUseMode(UseMode useMode) {
		this.useMode = useMode;
	}

	/**
	 * This method loads initial scene using a RecordReader instance
	 * 
	 */
	@SuppressWarnings("deprecation")
	@Override
	public void buildInitScene(GModel model) throws Exception {

		ExtInitializationTask initializationTask = new ExtInitializationTask();

		if (isGuiEnabled()) {
			new REpiceaProgressBarDialog(MainFrame.getInstance(), MessageID.Initialization.toString(),
					MessageID.Initialization.toString(), initializationTask, false);
		} else {
			initializationTask.run();
		}

		if (!initializationTask.isCorrectlyTerminated()) {
			throw initializationTask.getFailureReason();
		}

	}

	protected void clear() {
		initStand = null;
	}
	
}
