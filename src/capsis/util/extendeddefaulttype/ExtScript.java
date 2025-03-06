/*
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 *
 * Authors: M. Fortin and J-F Lavoie - Canadian Forest Service 
 * Copyright (C) 2020-21 Her Majesty the Queen in right of Canada
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.util.extendeddefaulttype;

import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.security.InvalidParameterException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

import capsis.app.C4Script;
import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.ExtEvolutionDialog.BreakVariable;
import capsis.util.extendeddefaulttype.disturbances.DisturbanceParameters;
import repicea.app.JSONConfigurationGlobal;
import repicea.app.REpiceaJSONConfiguration;
import repicea.app.UseModeProvider.UseMode;
import repicea.io.tools.ImportFieldElement.ImportFieldElementIDCard;
import repicea.io.tools.ImportFieldManager;
import repicea.io.tools.StreamImportFieldManager;
import repicea.lang.REpiceaSystem;
import repicea.simulation.ApplicationScaleProvider.ApplicationScale;
import repicea.simulation.climate.REpiceaClimateGenerator.ClimateChangeScenario;
import repicea.simulation.scriptapi.CapsisWebAPICompatibleScript;
import repicea.simulation.scriptapi.Request;
import repicea.simulation.scriptapi.ScriptResult;
import repicea.stats.data.DataSet;

/**
 * An abstract class for script to be launched from the Capsis server.<p>
 * Typical usage of this class is
 * <ol>
 * <li> Instantiate using constructor
 * <li> call registerOutputRequest() for all the outputs requested
 * <li> use setInitialParameters()
 * <li> use setEvolutionParameters() and give disturbances if desired
 * <li> call runSimulation() and get results
 * </ol>
 * @author Mathieu Fortin - November 2020
 */
public abstract class ExtScript extends C4Script implements PropertyChangeListener, CapsisWebAPICompatibleScript {
	
	public static boolean Verbose = false;
//	private final static double EstimatedMemoryLoadByStepRecordAndRealizationMb = 0.001222;		
	private final static double EstimatedMemoryLoadByStepRecordAndRealizationMb = 0.005; // was increased to prevent situations where young plots have very few trees but stem density increases quickly and results in a heap space issue	MF20240516	
	
	private int forceThisNumberOfRuns = -1;
	
	public final double[] elapsedTimes = new double[2];
	
	protected final ImportFieldManager importFieldManager; 
	protected final ExtInitialParameters<?,?,?> initParms;
	protected ExtEvolutionParametersList<?> evolutionParameters;
	protected final Map<Request, ExtScriptAbstractOutputRequest> outputRequests;
	
	protected int nbRuns;
	protected int runId;
	protected int nbStepsByRun;
	protected double simulationProgress;
	
	/**
	 * General constructor. If no ImportFieldManager instance is provided, then
	 * the script will rely on a StreamImportFieldManager instance instead.
	 * @param modelName
	 * @param ife an ImportFieldManager instance
	 * @throws Exception
	 */
	protected ExtScript(String modelName, ImportFieldManager ife) throws Exception {
		super(modelName);
		outputRequests = new HashMap<Request, ExtScriptAbstractOutputRequest>();
		initParms = getInitialParameters();
		initParms.setUseMode(UseMode.PURE_SCRIPT_MODE);
		ExtRecordReader<?> recordReader = initParms.getRecordReader();
		if (ife != null) {
			importFieldManager = ife;
		} else {
	     	importFieldManager = new StreamImportFieldManager(recordReader);
		}
		recordReader.initInScriptMode(importFieldManager);
	}
	
	/**
	 * Constructor for streamed data.
	 * @param modelName
	 * @throws Exception
	 */
	protected ExtScript(String modelName) throws Exception {
		this(modelName, null);
	}
	
	private ExtMethodProvider getMethodProvider() {return (ExtMethodProvider) getModel().getMethodProvider();}
	
	@Override
	public ExtModel<?> getModel() {return (ExtModel<?>) super.getModel();}
	
	public ExtInitialParameters<?,?,?> getInitialParameters() {
		return (ExtInitialParameters<?,?,?>) getModel().getSettings();
	}
	
	/**
	 * Set the initial parameteres of the simulation.
	 * @param initialDateYr the date (yr) of the initial stand or inventory
	 * @param isStochastic a boolean
	 * @param nbRealizations the number of realizations to be used in a stochastic simulation
	 * @param scale an ApplicationScale enum
	 */
	public void setInitialParameters(int initialDateYr, 
			boolean isStochastic, 
			int nbRealizations, 
			ApplicationScale scale) {
		if (nbRealizations < 0) {
			throw new InvalidParameterException("The number of realizations should be equal to or greater than 1!");
		}
		initParms.setInitialDateYear(initialDateYr);
		initParms.setStochastic(isStochastic); 
		if (isStochastic) {
			initParms.setNumberOfRealizations(nbRealizations);
		} else {
			initParms.setNumberOfRealizations(1);
		}
		initParms.setApplicationScale(scale);		
	}
	
	
	/**
	 * Set the evolution parameters.
	 * @param finalDateYr the final date of the simulation
	 * @param disturbances a List of DisturbanceParameters instances
	 */
	@SuppressWarnings({ "unchecked", "rawtypes" })
	public void setEvolutionParameters(int finalDateYr, List<DisturbanceParameters> disturbances) {
     	List<DisturbanceParameters> listDistParms = new ArrayList<DisturbanceParameters>();	// unnecessary at this point
     	if (disturbances != null) {
     		listDistParms.addAll(disturbances);
     	}
     	int timeStep = getInitialParameters().getSimulationSettings().getTimeStep();
     	ExtEvolutionBreakParameters breakParms = new ExtEvolutionBreakParameters(initParms.getInitialDateYr(), 
     			BreakVariable.Date, 
     			finalDateYr, 
     			timeStep, 
     			false);
     	ExtEvolutionParameters evolParmForThisSegment = new ExtEvolutionParameters(breakParms, listDistParms.toArray(new DisturbanceParameters[]{}));
     	evolutionParameters = new ExtEvolutionParametersList(evolParmForThisSegment);
	}

	@Override
	public final void setEvolutionParameters(int finalDateYr) {
		setEvolutionParameters(finalDateYr, null);
	}
	
	private void checkOutputRequests() {
		if (outputRequests.isEmpty()) {
			LinkedHashMap<String, List<String>> defaultSpeciesGrouping = retrieveDefaultSpeciesGroupsMap();
			ExtScriptSpecificOutputRequest requestForAlive = new ExtScriptSpecificOutputRequest(Request.AliveVolume, defaultSpeciesGrouping);
			outputRequests.put(Request.AliveVolume, requestForAlive);
		}
	}

	/**
	 * Register a request for a particular output of the simulation. 
	 * <br>
	 * If no request is registered, a default entry will be added when calling the runSimulation
	 * method: all living trees are grouped into a single group called "ALL", regardless of their 
	 * species. 
	 * 
	 * @param request a Request enum
	 * @param a LinkedHashMap for species grouping (this argument can be null for some requests)
	 */
	@Override
	public void registerOutputRequest(Request request, LinkedHashMap<String, List<String>> aggregationPatterns) {
		if (outputRequests.containsKey(request)) {
			throw new InvalidParameterException("A request of type : " + request.name() + " has already been registered !");
		}
		if (request.getStatusClass() == null || request.getVariableForEstimation() == null) {
			outputRequests.put(request, new ExtScriptSimpleOutputRequest(request));
		} else {
			outputRequests.put(request, new ExtScriptSpecificOutputRequest(request, aggregationPatterns));
		}
	}

	@Override
	public ScriptResult runSimulation() throws Exception {
		long initialRunSimulationTime = System.currentTimeMillis();
		checkOutputRequests();
		int numberRealizations = getInitialParameters().getNumberOfRealizations();
		int nbRecords = importFieldManager.getNumberOfRecords();
		nbStepsByRun = evolutionParameters.getNbSteps();
		int nbSteps = evolutionParameters.getNbSteps() + 1; // + 1 for the initial stand
		double estimatedMemoryLoadMb = nbSteps * nbRecords * numberRealizations * ExtScript.EstimatedMemoryLoadByStepRecordAndRealizationMb;
		
		double MaxMemoryThresholdMb = ((Number) JSONConfigurationGlobal.getInstance().get(REpiceaJSONConfiguration.processingMaxMemoryLimitMB, 2048)).doubleValue();

		nbRuns = 1;
		if (forceThisNumberOfRuns == -1) {
			if (estimatedMemoryLoadMb > MaxMemoryThresholdMb) {
				nbRuns = (int) Math.ceil(estimatedMemoryLoadMb / MaxMemoryThresholdMb); 
			}
		} else {
			nbRuns = forceThisNumberOfRuns;
		}
		
		int realizationsPerRun = (int) Math.ceil((double) numberRealizations / nbRuns);
		
		if (Verbose) {
			System.out.println("Number of realizations per run set to " + realizationsPerRun);
		}
		int nbRealizationsSoFar = 0;

		Map<Request, ExtScriptAbstractOutputRequest> outputRequestCopy = ExtScriptAbstractOutputRequest.copyOutputRequestMap(outputRequests);
		runId = 0;
		while (runId < nbRuns && nbRealizationsSoFar < numberRealizations) {
			int nbRealizationsInThisRun = numberRealizations - nbRealizationsSoFar;
			if (nbRealizationsInThisRun > realizationsPerRun) {
				nbRealizationsInThisRun = realizationsPerRun;
			}
			initParms.setNumberOfRealizations(nbRealizationsInThisRun);
			long initialTime = System.currentTimeMillis();
			if (Verbose) {
				int nbRecordsToBeRead = importFieldManager.getNumberOfRecords();
				System.out.println("Records to be read: " + nbRecordsToBeRead);
			}
			
			importFieldManager.getFormatReader().reset();
			if (model.getProject() != null) {
				closeProject(model.getProject());
			}

			init(initParms);
			postInitialization(initParms);
			elapsedTimes[0] += System.currentTimeMillis() - initialTime;

			initialTime = System.currentTimeMillis();
			currentStep = evolve(getRoot(), evolutionParameters);
			elapsedTimes[1] += System.currentTimeMillis() - initialTime;

			List<ExtCompositeStand> compositeStands = new ArrayList<ExtCompositeStand>();
			for (Step s : currentStep.getProject().getStepsFromRoot(currentStep)) {
				compositeStands.add((ExtCompositeStand) s.getScene());
			}
			
			for (ExtScriptAbstractOutputRequest request : outputRequestCopy.values()) {
				for (ExtCompositeStand stand : compositeStands) {
					if (request.isMeantForInitialStands() || !stand.isInitialScene()) {
						request.addEstimateForThisStand(stand, compositeStands, getMethodProvider());
					}
				}
			}
			
			if (Verbose) {
				NumberFormat nf = NumberFormat.getNumberInstance();
				nf.setMaximumFractionDigits(1);
				System.out.println("In-Simulation Memory used " + nf.format(REpiceaSystem.getCurrentMemoryLoadMb()) + " Mb");
			}
			
			nbRealizationsSoFar += nbRealizationsInThisRun;
						
			if (Verbose) {
				NumberFormat nf = NumberFormat.getNumberInstance();
				nf.setMaximumFractionDigits(1);
				System.out.println("Realizations " + nbRealizationsSoFar + " / " + numberRealizations);
				System.out.println("Memory used " + nf.format(REpiceaSystem.getCurrentMemoryLoadMb()) + " Mb");
			}
			runId++;
		}
		initParms.setNumberOfRealizations(numberRealizations); // reset the number of realizations to its original value
		ScriptResult res = getScriptResult(numberRealizations, 
				initParms.getInitScene().getExtPlotSample().getPlotIds().size(),
				initParms.getClimateChangeScenario(),
				model.getIdCard().getModelName(),
				outputRequestCopy);
		if (Verbose) {
			double elapsedTimeSec = (System.currentTimeMillis() - initialRunSimulationTime) * 0.001;
			NumberFormat nf = NumberFormat.getNumberInstance();
			nf.setMaximumFractionDigits(1);
			System.out.println("Total time to run simulation = " + nf.format(elapsedTimeSec) + " sec.");
		}

		// TODO MF20210923 should not even exist: the project should always be closed because the results are stored in the ScriptOutputResults. It is left here just because of Junit test purposes.
		if (nbRuns > 1) {		// in case of multiple run we must clean the project. Otherwise, the current stand and the initial stand stand for the last x realizations and this could lead to inconsistent results MF20210922
			closeProject(model.getProject());
		}
		
		clearRecords();
		
		return res;
	}


	/**
	 * Return the volume per hectare for each growth step.
	 * @return a Map
	 */
	@Deprecated
	public Map<Integer, Double> getVolumeMap() {
		if (initParms.getNumberOfRealizations() > 1) {
			throw new InvalidParameterException("The method cannot be used when the number of realizations is larger than 1!");
		}
		double areaFactor = -1;
		Map<Integer,Double> outputMap = new TreeMap<Integer, Double>();
		for (Step s : currentStep.getProject().getStepsFromRoot(currentStep)) {
			ExtCompositeStand compStand = (ExtCompositeStand) s.getScene();
			if (areaFactor == -1) {
				areaFactor = 1d / compStand.getAreaHa();
			}
			double volumeHa = getMethodProvider().getV(compStand, compStand.getTrees()) * areaFactor;
			outputMap.put(compStand.getDateYr(), volumeHa);
		}
		return outputMap;
	}

	private ScriptResult getScriptResult(int nbRealizations, int nbPlots, ClimateChangeScenario climateChangeScenario, String growthModel, Map<Request, ExtScriptAbstractOutputRequest> outputRequests) {
		DataSet outputDataSet = ScriptResult.createEmptyDataSet();
		for (ExtScriptAbstractOutputRequest filter : outputRequests.values()) {
			filter.storeEstimateIntoDataSet(outputDataSet, getInitialParameters().getInitialDateYr());
		}
		outputDataSet.indexFieldType();
		ScriptResult result = new ScriptResult(nbRealizations, nbPlots, climateChangeScenario, growthModel, outputDataSet);
		return result;
	}


	/**
	 * Create a default grouping: all species are grouped into a single unnamed group.
	 * @return a LinkedHashMap instance.
	 */
	protected LinkedHashMap<String, List<String>> retrieveDefaultSpeciesGroupsMap() {
		LinkedHashMap<String, List<String>> defaultSpeciesGroupMap = new LinkedHashMap<String, List<String>>();
		return defaultSpeciesGroupMap;
	};

	@Override
	public List<ImportFieldElementIDCard> getFieldDescriptions() {return importFieldManager.getFieldDescriptions();}
	
	/**
	 * Add a record to be asynchronously read by the recordReader if the importFieldManager member is
	 * of the StreamImportFieldManager class. Otherwise, it does nothing.
	 *
	 * @param record an array of Object instances
	 */
	@Override
	public void addRecord(Object[] record) {
		if (importFieldManager instanceof StreamImportFieldManager) {
			((StreamImportFieldManager) importFieldManager).getFormatReader().addRecord(record);
		}
	}

	/**
	 * Clear the records from the StreamImportFieldManager.
	 */
	public void clearRecords() {
		if (importFieldManager instanceof StreamImportFieldManager) {
			((StreamImportFieldManager) importFieldManager).getFormatReader().clearRecords();
		}
	}

	@Override
	public boolean setFieldMatches(int[] indices) {
		if (importFieldManager instanceof StreamImportFieldManager) {
			return ((StreamImportFieldManager) importFieldManager).setFieldMatches(indices);
		} else {
			return false;
		}
	}
	
	protected Step getCurrentStep() {return currentStep;}
	
	
	@Override
	public void closeProject(Project p)  {
		resetCurrentStep();
		model.setProject(null);
		super.closeProject(p);
	}	

	@Override
	public final String getCapsisVersion() {
		return Engine.getVersionAndRevision();
	}
	
	protected void postInitialization(ExtInitialParameters initParms) throws Exception {}
	
	protected void setForceThisNumberOfRuns(int i) {
		forceThisNumberOfRuns = i;
	}
	
	@Override
	public void propertyChange(PropertyChangeEvent arg0) {
		if (arg0.getPropertyName().equals(ExtModel.STEP_PROGRESS_PROPERTY))	{
			simulationProgress = ((int)arg0.getNewValue() + (runId * nbStepsByRun)) / (double) (nbStepsByRun * nbRuns);
			System.out.println("ArtScript : simulation progress set to " + simulationProgress);
		}
	}

	@Override
	public List<Request> getPossibleRequests() {
		return Arrays.asList(Request.values());
	}

	@Override
	public final double getSimulationProgress() {return simulationProgress;}
}
