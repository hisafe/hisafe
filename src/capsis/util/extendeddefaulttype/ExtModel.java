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

import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.logging.Level;

import javax.swing.JOptionPane;

import org.apache.commons.math.random.JDKRandomGenerator;
import org.apache.commons.math.random.RandomGenerator;

import capsis.defaulttype.Tree;
import capsis.extension.OverridableDataExtractorParameter;
import capsis.extension.dataextractor.configuration.DESettings;
import capsis.gui.MainFrame;
import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.ExtCompositeStand.PredictorID;
import capsis.util.extendeddefaulttype.disturbances.ThinningDisturbanceOccurrences;
import jeeb.lib.util.Log;
import repicea.app.JSONConfigurationGlobal;
import repicea.app.REpiceaJSONConfiguration;
import repicea.gui.genericwindows.REpiceaProgressBarDialog;
import repicea.gui.genericwindows.REpiceaProgressBarDialog.REpiceaProgressBarDialogParameters;
import repicea.simulation.REpiceaBinaryEventPredictor;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.Language;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * The extended model class handles multiple plots and stochastic simulations. <br>
 * <br>
 * 
 * WARNING : the ExtModel class should not contain any non static member because the 
 * assisted script model implements multi threading.

 * @author Mathieu Fortin - 2015
 *
 * @param <P> a class that extends ExtInitialParameters
 */
public abstract class ExtModel<P extends ExtInitialParameters> extends GModel implements OverridableDataExtractorParameter, ExtMultiThreadingStochasticModel {

	public static enum MessageID implements TextableEnum {
		Evolution("Evolution", "Evolution"),
		StartingEvolution("Starting evolution", "Amorce de l'\u00E9volution"),
		HarvestingStand("Harvesting scene", "R\u00E9colte"),
		EvolutionOf("Growing scene", "Evolution"),
		EvolutionIsOver("The evolution is done", "L'\u00E9volution est termin\u00E9e"),
		EvolutionCancelled("The evolution task has been cancelled!", "La t\u00E2che d'\u00E9volution a \u00E9t\u00E9 annul\u00E9e!");
		
		
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

	public static RandomGenerator RANDOM = new JDKRandomGenerator();

	public static enum InfoType{EXPORT, EVOLUTION_DIALOG}

	private boolean verbose;
	
	private final CopyOnWriteArrayList<PropertyChangeListener> listeners; 
	
	public static final String STEP_PROGRESS_PROPERTY = "stepProgress";
	

	protected ExtModel() {
		listeners = new CopyOnWriteArrayList<PropertyChangeListener>();
		Locale locale = Locale.getDefault();
		String languageString = locale.getLanguage();
		if (languageString == "fr") {
			REpiceaTranslator.setCurrentLanguage(Language.French);
		} else {
			REpiceaTranslator.setCurrentLanguage(Language.English);
		}
	}

	/**
	 * This method returns the initial parameters of the simulation.
	 * @return a QuebecMRNFInitialParameters-derived instance
	 */
	@Override
	public P getSettings() {
		return (P) super.getSettings();
	}

	/**
	 * This method sets the initial parameters of the simulation.
	 * @param settings a QuebecMRNFInitialParameters derived instance
	 */
	public void setSettings(P settings) {
		this.settings = settings;
	}

	/*
	 * Makes sure that the "per hectare" option is always enabled for ExtendedModel instances
	 */
	@Override
	public void setDefaultProperties(DESettings settings) {
		// fc-27.8.2021 HECTARE and perHa were removed and replaced by a standard "perHectare" 
		// boolean property everywhere, commented the 4 lines below
//		if (settings.booleanProperties.containsKey(AbstractDataExtractor.HECTARE)
//				|| settings.configProperties.contains(AbstractDataExtractor.HECTARE)) {
//			settings.perHa = true;
//		}
	}
	
	/**
	 * This method is the core of the evolution. 
	 * @param s the Step instance from which the evolution is made
	 * @param evolutionElement a ExtendedEvolutionParameters instance that encompasses all the parameters for the evolution
	 * @param numberOfGrowthStepsSoFar an Integer that must be add to the ProgressDispatcher for consistent values on the progress bar
	 * @param task the task is sent in this method to allow for message display
	 * @return the last Step instance of the evolution
	 * @throws Exception
	 */
	protected Step processEvolution(Step s, ExtEvolutionParameters evolutionElement, int numberOfGrowthStepsSoFar, ExtEvolutionTask task) throws Exception {
		int numberIterationsMC = ((ExtCompositeStand) s.getScene()).getNumberRealizations();
		
		double startTime = System.currentTimeMillis();
		double time;
		int iNumberOfThreadsToStart;
				
		int maxProcessingThreads = ((Number) JSONConfigurationGlobal.getInstance().get(REpiceaJSONConfiguration.processingMaxThreads, 2)).intValue();		
		
		if (numberIterationsMC == 1) {
			iNumberOfThreadsToStart = 1;
		} else {
			iNumberOfThreadsToStart = (int)((long)maxProcessingThreads);
		}
		
		ExtMonteCarloWorkerThread[] aoWorkerThread = new ExtMonteCarloWorkerThread[iNumberOfThreadsToStart];
		ExtCompositeStand newCompositeStand = null;
		
		int iNumberOfMCIterationsPerThread = numberIterationsMC / iNumberOfThreadsToStart;
		
		int nbStepsDone = 0;
		while (evolutionElement.shouldKeepRunning(s.getScene())) {
			
			task.displayThisMessage(ExtModel.MessageID.EvolutionOf.toString() + " " + s.getCaption());
			
			// for each stand into the composite stand
			ExtCompositeStand currentCompositeStand = (ExtCompositeStand) s.getScene();
			newCompositeStand = (ExtCompositeStand) s.getScene().getEvolutionBase();
			newCompositeStand.getEvolutionTracking().add(evolutionElement);
			newCompositeStand.setDate(currentCompositeStand.getDate() + evolutionElement.getStepDuration(currentCompositeStand.getDateYr(), this));

			// start processing threads
			
			int iStartMC = 0;
			int iEndMC = 0;
			
			for (int j = 0; j < iNumberOfThreadsToStart; j++) {
				int iNumberOfIterationsToLaunch = iNumberOfMCIterationsPerThread;
				
				if (j == iNumberOfThreadsToStart - 1) {
					iNumberOfIterationsToLaunch = iNumberOfMCIterationsPerThread + numberIterationsMC % iNumberOfThreadsToStart;
				}
				
				iEndMC = iStartMC + iNumberOfIterationsToLaunch;

				aoWorkerThread[j] = new ExtMonteCarloWorkerThread(this, currentCompositeStand, newCompositeStand, task, iStartMC, iEndMC);
				iStartMC = iEndMC;
			}
			
			boolean anyAlive;
			do {
				anyAlive = false;
				for (int threadId = 0; threadId < aoWorkerThread.length; threadId++) {
					if (aoWorkerThread[threadId].isAlive()) 
						anyAlive = true;
				}
			} while (anyAlive);
			
			for (ExtMonteCarloWorkerThread thread : aoWorkerThread) {
				if (thread.getResult() != null) {
					throw thread.getResult();
				}
			}
			
//			// now log the thread timing 
//			for (int j = 0; j < iNumberOfThreadsToStart; j++) {
//				Log.println("ExtendedType", Log.INFO, "ExtModel.processEvolution()",
//						"Thread " + j + " : Processed " + aoWorkerThread[j].getNumberOfIterations() + " iteration(s) in " + aoWorkerThread[j].getRunTimeMs()+ " ms.");
//			}
			
			String reason = "Evolution to date " + newCompositeStand.getDate(); // last

//			// an ArtEvolutionParameters object is created and save into the Evolution tracking
//			newCompositeStand.getEvolutionTracking().add(evolutionElement.deriveCopyForASingleStep());
			
			Step newStp = s.getProject().processNewStep(s, newCompositeStand, reason);	
			s = newStp;
			
			nbStepsDone++;
			task.firePropertyChange(STEP_PROGRESS_PROPERTY, null, numberOfGrowthStepsSoFar + nbStepsDone);
			
			evolutionElement = evolutionElement.mute();

			if (isVerbose()) {
				@SuppressWarnings("unused")
				double memoryUsed;
				memoryUsed = Math.round((Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory())*ExtSimulationSettings.MEMORY_MEG_FACTOR);
				time = (System.currentTimeMillis() - startTime) * 0.001;
				System.out.println("Time : " + time + " Memory used BEFORE " + s.getName() + " : " + memoryUsed + " Mg");

				System.gc();
				memoryUsed = Math.round((Runtime.getRuntime().totalMemory()-Runtime.getRuntime().freeMemory())*ExtSimulationSettings.MEMORY_MEG_FACTOR);
				time = (System.currentTimeMillis() - startTime) * 0.001;
				System.out.println("Time : " + time + " Memory used AFTER " + s.getName() + " : " + memoryUsed + " Mg");
			}			
		}
		time = (System.currentTimeMillis() - startTime) * 0.001;
		
		return s;
	}

	@Override
	public Step processEvolution(Step stp, EvolutionParameters evolParms) throws Exception {
		ExtEvolutionTask evolutionTask = new ExtEvolutionTask(this, stp, evolParms);
		for (PropertyChangeListener l : listeners) {
			evolutionTask.addPropertyChangeListener(l);
		}

		boolean guiEnabled = getSettings().isGuiEnabled();
		try {
			if (guiEnabled) {
				List<REpiceaProgressBarDialogParameters> parms = new ArrayList<REpiceaProgressBarDialogParameters>();
				parms.add(new REpiceaProgressBarDialogParameters(MessageID.Evolution.toString(), 
						evolutionTask, 
						true));
				new REpiceaProgressBarDialog(MainFrame.getInstance(), 
						MessageID.Evolution.toString(), 
						parms);
			} else {
				evolutionTask.execute();
				evolutionTask.get();
			}
			
			if (!evolutionTask.isCorrectlyTerminated()) {
				throw evolutionTask.getFailureReason();
			}
		} catch (CancellationException e) { // the task has been cancelled 
			if (guiEnabled) {
				JOptionPane.showMessageDialog(MainFrame.getInstance(), 
						MessageID.EvolutionCancelled.toString(), 
						"Information", 
						JOptionPane.INFORMATION_MESSAGE);
			}
			Log.println(Level.INFO, "processEvolution", "The evolution task has been cancelled!");
		} 
		return evolutionTask.newStp != null ? evolutionTask.newStp : stp; // FP TODO MF20230323 Maybe we should delete the extra step here and just return the initial step.
	}

	protected abstract Step processCutting (Step stp, ExtEvolutionParameters stepEvolution) throws Exception;

	
	protected boolean processTreeThinning(ExtPlot newStand, ExtPlot currentStand, ThinningDisturbanceOccurrences thinningOcc) {
		Collection<Tree> collCopy = new ArrayList<Tree>();
		collCopy.addAll(currentStand.getTrees());//	to avoid ConcurrentModificationException
		
		for (Tree tree : collCopy) {
			Object result;
			if (thinningOcc != null && thinningOcc.containsTree(tree)) {
				result = thinningOcc.getTreeResult(tree);
				double ratioOfTimeStep = (double) (thinningOcc.getLastOccurrenceDateYr() - currentStand.getDateYr())/ (newStand.getDateYr() - currentStand.getDateYr());
				((ExtTree) tree).processStatusChange(result, newStand, StatusClass.cut, ratioOfTimeStep, currentStand);
			} else {
				result = false;
				((ExtTree) tree).processStatusChange(result, newStand, StatusClass.cut);
			}
		}

 		boolean hasBeenHarvested = !newStand.getTrees(StatusClass.cut.name()).isEmpty();
		newStand.setInterventionResult(hasBeenHarvested);
		if (hasBeenHarvested) {
			newStand.setLastThinningOccurrence(thinningOcc);
		}
		currentStand.setGoingToBeHarvested(hasBeenHarvested);
		return hasBeenHarvested;
	}

	protected final void processTreeMortality(ExtPlot newStand, ExtPlot currentStand) {
		processTreeMortality(newStand, currentStand, null);
	}

	/**
	 * Process the mortality of each individual tree. The trees must have been stored in the newStand instance 
	 * prior to this method call. 
	 * @param newPlot an ExtPlot instance
	 * @param currentPlot an ExtPlot instance
	 * @param parms some optional parameters for the mortality model
	 */
	protected void processTreeMortality(ExtPlot newPlot, ExtPlot currentPlot, Map<String, Object> parms) {
		REpiceaBinaryEventPredictor mortalityPredictor = (REpiceaBinaryEventPredictor) currentPlot.getStratum().getPredictor (PredictorID.MORTALITY);
		Collection<Tree> collCopy = new ArrayList<Tree>();
		collCopy.addAll(newPlot.getTrees());	//	to avoid ConcurrentModificationException
		int timeStepLengthYr = newPlot.getDateYr() - currentPlot.getDateYr();
		for (Tree tree : collCopy) {
			ExtTree t = (ExtTree) tree;
			Object deathEvent = mortalityPredictor.predictEvent(currentPlot, t, parms);
			if (deathEvent instanceof StatusClass) {	// the mortality module can eventually return a status different than dead. For instance Mathilde may return windfall
				StatusClass newStatus = (StatusClass) deathEvent;
				if (newStatus == StatusClass.alive) {
					t.processStatusChange(false, newPlot, StatusClass.dead);
				} else {
					int dateOfStatusChangeYr = currentPlot.getDateYr() + (int) Math.ceil(RANDOM.nextDouble() * timeStepLengthYr);
					double ratioOfTimeStep = (double) (dateOfStatusChangeYr - currentPlot.getDateYr()) / timeStepLengthYr;
					t.processStatusChange(true, newPlot, newStatus, ratioOfTimeStep, currentPlot);
				}
			} else { // we are dealing with competition-induced mortality
				if (deathEvent instanceof Boolean) { 	// stochastic simulation
					int dateOfStatusChangeYr = currentPlot.getDateYr() + (int) Math.ceil(RANDOM.nextDouble() * timeStepLengthYr);
					double ratioOfTimeStep = (double) (dateOfStatusChangeYr - currentPlot.getDateYr()) / timeStepLengthYr;
					t.processStatusChange(deathEvent, newPlot, StatusClass.dead, ratioOfTimeStep, currentPlot);
				} else { 								// deterministic simulation we do not update the diameter
					t.processStatusChange(deathEvent, newPlot, StatusClass.dead);  
				}
			}
		}
	}

	
	@Override
	public void setProject(Project p) {
		super.setProject(p);
		if (p != null) {
			p.setName(getSettings().getProjectName());
		}
	}
	
	@Override
	public void clear() {getSettings().clear();}
	
	/**
	 * This method sets the verbose to true or false. By default, the verbose
	 * is DISABLED.
	 * @param enabled a boolean
	 */
	public void setVerbose(boolean enabled) {
		verbose = enabled;
	}
	
	/**
	 * This method returns true if the verbose is enabled. By default, the verbose
	 * is DISABLED.
	 * @return a boolean
	 */
	public boolean isVerbose() {return verbose;}

	/**
	 * This method initiates a validation log. It can be further defined in derived class.
	 */
	protected void initLogFilesDataValidation() {}
	
	@Override
	public ExtMethodProvider getMethodProvider() {return (ExtMethodProvider) super.getMethodProvider();}
	
	public void addPropertyChangeListener(PropertyChangeListener l) {
		if (!listeners.contains(l))
			listeners.add(l);
	}
	
	public boolean removePropertyChangeListener(PropertyChangeListener l) {
		return listeners.remove(l);
	}
}
