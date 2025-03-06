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

import java.io.Serializable;
import java.security.InvalidParameterException;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.extension.modeltool.carbonstorageinra.CATStandConnector;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.EnumProperty;
import capsis.util.extendeddefaulttype.disturbances.DisturbanceParameters.DisturbanceMode;
import repicea.lang.MemoryWatchDog;
import repicea.simulation.REpiceaBinaryEventPredictor;
import repicea.simulation.REpiceaPredictor;
import repicea.simulation.allometrycalculator.AllometryCalculableTree;
import repicea.simulation.climate.REpiceaClimateGenerator.ClimateChangeScenario;
import repicea.simulation.climate.REpiceaClimateGenerator.ClimateChangeScenarioProvider;
import repicea.simulation.climate.REpiceaClimateGenerator.RepresentativeConcentrationPathway;
import repicea.simulation.covariateproviders.plotlevel.StochasticInformationProvider;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * The CompositeTreeList class handles several TreeList instance in the same
 * simulation. It is also designed for stochastic simulation.
 * 
 * @author Mathieu Fortin - October 2013
 */
@SuppressWarnings({ "serial", "rawtypes", "unchecked" })
public abstract class ExtCompositeStand extends TreeList implements StochasticInformationProvider<ExtPlotSample>, 
																	CATStandConnector,
																	ClimateChangeScenarioProvider {
	 
	public static class PredictorID implements Serializable {

		public static final PredictorID DIAMETER_GROWTH = new PredictorID("DIAMETER_GROWTH");
		public static final PredictorID MORTALITY = new PredictorID("MORTALITY");
		public static final PredictorID HD_RELATIONSHIP = new PredictorID("HD_RELATIONSHIP");
		public static final PredictorID COMMERCIAL_VOLUME = new PredictorID("COMMERCIAL_VOLUME");
		public static final PredictorID TOTAL_VOLUME = new PredictorID("TOTAL_VOLUME");
		public static final PredictorID TREE_HARVESTING = new PredictorID("TREEHARVESTING");
		public static final PredictorID STAND_HARVESTING = new PredictorID("STAND_HARVESTING");
		public static final PredictorID CLIMATE = new PredictorID("CLIMATE");

		private final String predictorName;

		protected PredictorID(String name) {
			predictorName = name;
		}

		@Override
		public int hashCode() {
			return predictorName.hashCode();
		}

		@Override
		public String toString() {
			return "PredictorID - " + predictorName;
		}
	}

	protected static enum MessageID implements TextableEnum {
		StemDensity("stems/ha", "tiges/ha"), 
		BasalArea("m2/ha", "m2/ha"), 
		SampleSize("Sample size", "Taille d'\u00E9chantillon"), 
		StochasticMode("Stochastic mode", "Mode stochastique"), 
		DeterministicMode("Deterministic mode", "Mode d\u00E9terministe"), 
		Realization("realization", "r\u00E9alisation"),
		Scale("Scale", "Echelle"),
		ClimateChangeScenario("Climate change scenario", "Sc\u00E9nario de changements climatiques");

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

	public class Immutable extends TreeList.Immutable {
		private String stratumName = null;
		private boolean isStochastic;
		private int numberIterationMC;
		private int initialDateYr;
		private int initialAgeYr;
		private Map<Integer, EnumProperty> speciesGroupTags;
		private Map<String, REpiceaPredictor> predictors;
		private Map<DisturbanceType, Map<DisturbanceMode, REpiceaBinaryEventPredictor>> naturalDisturbanceOccurrencePredictors;
		private Map<DisturbanceType, Map<DisturbanceMode, REpiceaBinaryEventPredictor>> naturalDisturbanceConditionalVulnerabilityPredictors;
		private double area;
		private ApplicationScale scale;
		private ManagementType managementType;
		private ClimateChangeScenario climateChangeScenario = RepresentativeConcentrationPathway.NO_CHANGE;
	}

	private List<ExtEvolutionParameters> evolutionVector;
	
	/**
	 * A Map with the Monte Carlo id as key and ExtPlotSample instances as values
	 */
	private Map<Integer, ExtPlotSample> plotSampleArray;

	
	
	
	/**
	 * Constructor. In case of deterministic simulations the number of Monte
	 * Carlo iteration is automatically set to 1.
	 * 
	 * @param isStochastic true to enable stochastic simulation (if implemented)
	 * @param numberIterationMC the number of Monte Carlo iterations
	 * @param initialSimulationDateYr the date at the creation of the project
	 */
	protected ExtCompositeStand(boolean isStochastic, int numberIterationMC, int initialSimulationDateYr, int initialAgeYr) {
		super();
		if (numberIterationMC < 1) {
			throw new InvalidParameterException(
					"The number of Monte Carlo iteration must be larger than or equal to 1!");
		}
		evolutionVector = new ArrayList<ExtEvolutionParameters>();
		getImmutable().isStochastic = isStochastic;
		getImmutable().numberIterationMC = numberIterationMC;
		getImmutable().predictors = new HashMap<String, REpiceaPredictor>();
		getImmutable().naturalDisturbanceOccurrencePredictors = new HashMap<DisturbanceType, Map<DisturbanceMode, REpiceaBinaryEventPredictor>>();
		getImmutable().naturalDisturbanceConditionalVulnerabilityPredictors = new HashMap<DisturbanceType, Map<DisturbanceMode, REpiceaBinaryEventPredictor>>();
		getImmutable().initialDateYr = initialSimulationDateYr;
		getImmutable().initialAgeYr = initialAgeYr;
		setApplicationScale(ApplicationScale.FMU); // default value
		init();
	}

	/**
	 * Constructor for deterministic simulations and initial age.
	 */
	protected ExtCompositeStand(int initialSimulationYear, int initialAgeYr) {
		this(false, 1, initialSimulationYear, initialAgeYr);
	}

	/**
	 * Constructor for deterministic simulations and no initial age.
	 */
	protected ExtCompositeStand(int initialSimulationYear) {
		this(false, 1, initialSimulationYear, -99);
	}

	/**
	 * Constructor for stochastic simulation and no initial age
	 * @param isStochastic
	 * @param numberIterationMC
	 * @param initialSimulationDateYr
	 */
	protected ExtCompositeStand(boolean isStochastic, int numberIterationMC, int initialSimulationDateYr) {
		this(isStochastic, numberIterationMC, initialSimulationDateYr, -99);
	}

	
	/**
	 * Return a particular submodel of the simulator.
	 * @param predictorID an instance that stands for the submodel
	 * @return an REpiceaPredictor instance or null if there is no submodel matching the predictorID instance 
	 */
	public REpiceaPredictor getPredictor(PredictorID predictorID) {
		return getPredictors().get(predictorID.predictorName);
	}
	
	/**
	 * Provide an REpiceaBinaryEventPredictor instance that predicts the occurrence
	 * of large-scale natural disturbance. <p>
	 * These instances must have been specified in the model initialization beforehand. 
	 * @param type a DisturbanceType enum
	 * @param mode a DisturbanceMode enum
	 * @return an REpiceaBinaryEventPredictor instance
	 */
	public REpiceaBinaryEventPredictor getNaturalDisturbanceOccurrencePredictor(DisturbanceType type, DisturbanceMode mode) {
		if (getImmutable().naturalDisturbanceOccurrencePredictors.containsKey(type)) {
			return getImmutable().naturalDisturbanceOccurrencePredictors.get(type).get(mode);
		}
		return null;
	}
	
	public void addNaturalDisturanceOccurrencePredictor(DisturbanceType type, DisturbanceMode mode, REpiceaBinaryEventPredictor predictor) {
		if (type == null || mode == null) {
			throw new InvalidParameterException("The type or the mode cannot be null!");
		}
		if (mode != DisturbanceMode.Random && mode != DisturbanceMode.FullModelBased) {
			throw new InvalidParameterException("The mode must be either Random or FullModelBased!");
		}
		if (!getImmutable().naturalDisturbanceOccurrencePredictors.containsKey(type)) {
			getImmutable().naturalDisturbanceOccurrencePredictors.put(type, new HashMap<DisturbanceMode, REpiceaBinaryEventPredictor>());
		}
		getImmutable().naturalDisturbanceOccurrencePredictors.get(type).put(mode, predictor);
	}
	
	public void addNaturalDisturbanceConditionalVulnerabilityPredictor(DisturbanceType type, DisturbanceMode mode, REpiceaBinaryEventPredictor predictor) {
		if (type == null || mode == null) {
			throw new InvalidParameterException("The type or the mode cannot be null!");
		}
		if (!getImmutable().naturalDisturbanceConditionalVulnerabilityPredictors.containsKey(type)) {
			getImmutable().naturalDisturbanceConditionalVulnerabilityPredictors.put(type, new HashMap<DisturbanceMode, REpiceaBinaryEventPredictor>());
		}
		getImmutable().naturalDisturbanceConditionalVulnerabilityPredictors.get(type).put(mode, predictor);
	}
	
	/**
	 * Provide an REpiceaBinaryEventPredictor instance that predicts the vulnerability of a plot
	 * conditional on the occurrence of large-scale natural disturbance. <p>
	 * These instances must have been specified in the model initialization beforehand. 
	 * @param type a DisturbanceType enum
	 * @param mode a DisturbanceMode enum
	 * @return an REpiceaBinaryEventPredictor instance
	 */
	public REpiceaBinaryEventPredictor getNaturalDisturbanceConditionalVulnerability(DisturbanceType type, DisturbanceMode mode) {
		if (getImmutable().naturalDisturbanceConditionalVulnerabilityPredictors.containsKey(type)) {
			return getImmutable().naturalDisturbanceConditionalVulnerabilityPredictors.get(type).get(mode);
		}
		return null;
	}

	
	/**
	 * This method returns the scale at which the model applies.
	 * @return an ApplicationScale enum
	 */
	@Override
	public final ApplicationScale getApplicationScale() {return getImmutable().scale;}

	/**
	 * Set the application scale of the simulation. <p>
	 * By default, the application scale is at the stand level.
	 * @param scale an ApplicationScale enum
	 */
	public void setApplicationScale(ApplicationScale scale) {getImmutable().scale = scale;} 

	
	public void setManagementType(ManagementType managementType) {getImmutable().managementType = managementType;}

	@Override
	public final ManagementType getManagementType() {return getImmutable().managementType;}

	
	/**
	 * Add a predictor to a map of predictors.
	 * 
	 * @param predictorID a PredictorID enum variable
	 * @param predictor an REpiceaPredictor instance
	 */
	public void addPredictor(PredictorID predictorID, REpiceaPredictor predictor) {
		getPredictors().put(predictorID.predictorName, predictor);
	}

	@Override
	protected Immutable getImmutable() {
		return (Immutable) immutable;
	}

	@Override
	protected void createImmutable() {
		immutable = new Immutable();
	}

	@Override
	public void init() {
		super.init();
		createMapArray();
	}

	private void createMapArray() {
		List<Integer> monteCarloId = new ArrayList<Integer>();
		if (plotSampleArray != null) {
			for (ExtPlotSample plotSample : getPlotSamples()) {
				monteCarloId.add(plotSample.getMonteCarloRealizationId());
			}
		}
		if (monteCarloId.isEmpty()) {
			for (int i = 0; i < getImmutable().numberIterationMC; i++) {
				monteCarloId.add(i);
			}
		}
		plotSampleArray = new TreeMap<Integer, ExtPlotSample>();
		for (Integer i : monteCarloId) {
			plotSampleArray.put(i, createPlotSample(i));
		}
	}

	public Collection<ExtPlotSample> getPlotSamples() {
		if (plotSampleArray != null) {
			return plotSampleArray.values();
		} else {
			return new ArrayList<ExtPlotSample>();
		}
	}
	
	protected final void addPlotSample(ExtPlotSample plotSample) {
		plotSampleArray.put(plotSample.getMonteCarloRealizationId(), plotSample);
	}
	
	protected final void removePlotSample(int monteCarloId) {
		plotSampleArray.remove(monteCarloId);
	}
	
	/**
	 * This method can be overriden in derived class if the PlotSample instance
	 * has to be extended.
	 * 
	 * @param i the Monte Carlo realization id
	 * @return an ExtPlotSample instance
	 */
	protected abstract ExtPlotSample createPlotSample(int i);

	@Override
	public void setInterventionResult(boolean b) {
		super.setInterventionResult(b);

		if (plotSampleArray != null) {
			for (ExtPlotSample plotList : getPlotSamples()) {
				plotList.setInterventionResult(b);
			}
		}
	}

	@Override
	public void clearTrees() {
		for (ExtPlotSample plotSample : getPlotSamples()) {
			plotSample.clearTrees();
		}
	}

	@Override
	public Collection<? extends Tree> getTrees() {
		Collection trees = new ArrayList();
		for (ExtPlotSample plotList : getPlotSamples()) {
			trees.addAll(plotList.getTrees());
		}
		return trees;
	}

	@Override
	public Collection<Tree> getTrees(String status) {
		Collection<Tree> coll = new ArrayList<Tree>();

		if (status == null) {
			return coll;
		}

		for (ExtPlotSample plotList : getPlotSamples()) {
			coll.addAll(plotList.getTrees(status));
		}
		return coll;
	}

	@Override
	public Set<String> getStatusKeys() {
		Set<String> oStatusCollection = new HashSet<String>();

		for (ExtPlotSample plotList : getPlotSamples()) {
			oStatusCollection.addAll(plotList.getStatusKeys());
		}
		return oStatusCollection;
	}

	/*
	 * Needed redefinition for getTree (id). Chose the option to return always
	 * the tree with the given id in the standList(0) (could be changed).
	 */
	@Override
	// fc - 27.4.2009 mf2009-10-13
	public Tree getTree(int treeId) {
		Tree t = null;
		for (TreeList stand : getExtPlotSample().getPlots()) {
			if (stand.getTree(treeId) != null) {
				t = stand.getTree(treeId);
			}
		}
		return t;
	}

	@Override	// mf2009-10-13
	public void removeTree(Tree tree) {
		for (ExtPlotSample plotSample : getPlotSamples()) {
			for (TreeList stand : plotSample.getPlots()) {
				if (stand.getTrees().contains(tree)) {
					stand.removeTree(tree);
				}
			}
		}
	}

	/**
	 * This method overrides the super method to make sure the evolution
	 * tracking is independent from step to step
	 */
	@Override
	public GScene getLightClone() {

		MemoryWatchDog.checkAvailableMemory();

		ExtCompositeStand lightClone = (ExtCompositeStand) super.getLightClone();

		// A copy of the evolution tracking is made
		List<ExtEvolutionParameters> oVec = new ArrayList<ExtEvolutionParameters>();
		for (ExtEvolutionParameters param : evolutionVector) {
			oVec.add(param.clone());
		}
		// this copy is put into the light clone
		lightClone.evolutionVector = oVec;

		return lightClone;
	}

	/**
	 * getHeavyClose method is redefined because the ArtCompositeStand object
	 * may contain many ArtStand objects
	 * 
	 * @see capsis.defaulttype.TreeList#getHeavyClone()
	 */
	@Override
	public GScene getHeavyClone() {

		ExtCompositeStand heavyCompositeStand = (ExtCompositeStand) getLightClone();

		for (ExtPlotSample plotSample : getPlotSamples()) {
			for (ExtPlot stand : plotSample.getPlots()) {
				ExtPlot heavyStand = stand.getHeavyClone();
				try {
					heavyCompositeStand.getRealization(plotSample.getMonteCarloRealizationId()).addPlot(heavyStand.getSourceName(), heavyStand);
				} catch (NullPointerException e) {
					heavyCompositeStand.getRealization(plotSample.getMonteCarloRealizationId()).addPlot(heavyStand.getSourceName(), heavyStand);
				}
			}
		}
		return heavyCompositeStand;
	}

	@Override
	public String toString() {
		return "CompositeTreeList_" + getCaption();
	}

	/**
	 * This method sets a map of EnumProperty instances which represent the
	 * species.
	 * 
	 * @param speciesGroupTags
	 *            a Map of EnumProperty instances
	 */
	public void setSpeciesGroupTag(Map<Integer, EnumProperty> speciesGroupTags) {
		getImmutable().speciesGroupTags = speciesGroupTags;
	}

	/**
	 * This method returns the map of group tags for the species.
	 * 
	 * @return a Map of integers and EnumProperty instances
	 */
	public Map<Integer, EnumProperty> getSpeciesGroupTags() {
		return getImmutable().speciesGroupTags;
	}

	/**
	 * This method returns the TreeList that matches the following parameters
	 * 
	 * @param iter
	 *            the Monte Carlo realization
	 * @param strID
	 *            the TreeList id
	 * @return a TreeList instance
	 */
	public ExtPlot getExtPlot(int iter, String strID) {
		return getRealization(iter).getPlot(strID);
	}


	/**
	 * This method returns all the TreeList instances of a particular Monte
	 * Carlo realization.
	 * 
	 * @param iter
	 *            the MonteCarlo realization
	 * @return a Map of Strings and TreeList instances
	 */
	@Override
	public ExtPlotSample getRealization(int iter) {
		return plotSampleArray.get(iter);
	}

	/**
	 * This method returns the TreeList instance of the first realization, which
	 * is the default in case of deterministic simulation.
	 * 
	 * @return a Map of Strings and TreeList instances
	 */
	public ExtPlotSample getExtPlotSample() {
		Collection<ExtPlotSample> coll = plotSampleArray.values();
		if (coll != null) {
			return coll.iterator().next();
		} else {
			return null;
		}
	}
	
	/**
	 * Returns the ExtPlotSample instance that matches the Monte
	 * Carlo realization i.
	 * @param i the Monte Carlo realization id
	 * @return an ExtPlotSample instance
	 */
	public ExtPlotSample getExtPlotSample(int i) {
		return getRealization(i);
	}


	@Override
	public boolean isStochastic() {
		return getImmutable().isStochastic;
	}

	/**
	 * This method sets the stratum name.
	 * 
	 * @param stratumName
	 *            a String
	 */
	public void setStratumName(String stratumName) {
		getImmutable().stratumName = stratumName;
	}

	/**
	 * This method returns the stratum name.
	 * 
	 * @return a String
	 */
	public String getStratumName() {
		return getImmutable().stratumName;
	}

	@Override
	public double getArea() {
		if (getImmutable().area != getExtPlotSample().getArea()) {
			getImmutable().area = getExtPlotSample().getArea();
		}
		return getImmutable().area;
	}

	/**
	 * This method returns the evolution tracking of the composite tree list.
	 * 
	 * @return a List of ExtendedEvolutionParameters
	 */
	public List<ExtEvolutionParameters> getEvolutionTracking() {
		return evolutionVector;
	}


	@Override
	public String getToolTip() {
		NumberFormat formatter = NumberFormat.getInstance();
		formatter.setMaximumFractionDigits(1);
		ExtMethodProvider mp = new ExtMethodProvider();
		double areaFactor = 1d / getAreaHa();
		String mode = MessageID.DeterministicMode.toString() + ", ";
		boolean plural = getNumberRealizations() > 1;
		if (isStochastic()) {
			mode = MessageID.StochasticMode.toString() + " (" + getNumberRealizations() + " "
					+ MessageID.Realization.toString();
			if (plural) {
				mode = mode.concat("s");
			}
			mode = mode.concat(")");
		}
		String scale = MessageID.Scale.toString() + ": " + getApplicationScale().toString();
		String sampleSize = MessageID.SampleSize.toString() + ": " + getExtPlotSample().size();
		String climateChange = MessageID.ClimateChangeScenario.toString() + ": " + getClimateChangeScenario(); 
		return "<html>" + climateChange + "<br>" + scale + "<br>" + mode + "<br>" + sampleSize + "<br>" + "N = "
				+ formatter.format(mp.getN(this, getTrees()) * areaFactor) + " " + MessageID.StemDensity.toString()
				+ "<br>" + "G = " + formatter.format(mp.getG(this, getTrees()) * areaFactor) + " "
				+ MessageID.BasalArea.toString();
	}

	/**
	 * Provide collections of living tree by species.
	 * 
	 * @return a Map instance with species names as keys and collections as values
	 * @see ExtCompositeStand#getCollectionsBySpecies(StatusClass)
	 */
	public Map<String, Collection<AllometryCalculableTree>> getCollectionsBySpecies() {
		return getCollectionsBySpecies(StatusClass.alive);
	} 
	
	/**
	 * Provide collections of tree by species for a particular status class.
	 * 
	 * @param statusClass a StatusClass enum (StatusClass.alive, StatusClass.dead, StatusClass.cut, StatusClass.windfall)
	 * @return a Map instance with species names as keys and collections as values
	 * @see ExtCompositeStand#getCollectionsBySpecies(Collection, Collection)
	 */
	public Map<String, Collection<AllometryCalculableTree>> getCollectionsBySpecies(StatusClass statusClass) {
		Collection<EnumProperty> index = getSpeciesGroupTags().values();
		return getCollectionsBySpecies(index, getTrees(statusClass));
	}

	/**
	 * Sort the Tree instance and produce a series of species-specific collections. <p>
	 * 
	 * The output map is populated with a Collection instance for each species plus one extra for all species.
	 * 
	 * @param index a Vector of integers that represent the species groups
	 * @param trees a Collection of trees
	 * @return a Map instance with species names as keys and collections as values
	 */
	public Map<String, Collection<AllometryCalculableTree>> getCollectionsBySpecies(Collection<EnumProperty> index, Collection trees) {
		Map<String, Collection<AllometryCalculableTree>> speciesCollection = new TreeMap<String, Collection<AllometryCalculableTree>>();
		for (EnumProperty speciesGroupTag : index) {
			Collection<AllometryCalculableTree> oMap = new ArrayList<AllometryCalculableTree>();
			speciesCollection.put(speciesGroupTag.getName(), oMap);
		}

		for (Object tree : trees) {
			speciesCollection.get(((ExtTree) tree).getSpeciesName()).add((AllometryCalculableTree) tree);
		}

		speciesCollection.put(ExtSimulationSettings.ALL_SPECIES, (Collection<AllometryCalculableTree>) trees); // add the whole collection of living trees in the all-species collection
		return speciesCollection;
	}

	@Override
	public List<Integer> getRealizationIds() {
		List<Integer> ids = new ArrayList<Integer>();
		if (plotSampleArray != null) {
			ids.addAll(plotSampleArray.keySet());
		}
		return ids;
	}

//	@Override
//	public double getAreaHa() {
//		return getArea() * .0001;
//	}
//
	@Override
	public final String getStandIdentification() {
		Step step = ((GScene) this).getStep();
		return step.getProject().getName() + " - " + step.getName();
	}
//
//	@Override
//	public int getDateYr() {return getDate();}

	/**
	 * This method returns the initial date of the 
	 * simulation.
	 * @return an integer
	 */
	public int getInitialCreationDateYr() {
		return getImmutable().initialDateYr;
	}

	@Override
	public void storeStatus(Numberable tree, String status, double numberUnderThisStatus) {
		ExtPlot plot = (ExtPlot) ((ExtTree) tree).getScene();
		plot.storeStatus(tree, status, numberUnderThisStatus);
	}
	
	public void storeStatus(Numberable tree, StatusClass statusClass, double numberUnderThisStatus) {
		storeStatus(tree, statusClass.name(), numberUnderThisStatus);
	}

	@Override
	public void storeStatus(Tree tree, String status) {
		ExtPlot plot = (ExtPlot) ((ExtTree) tree).getScene();
		plot.storeStatus(tree, status);
	}
	
	public void storeStatus(Tree tree, StatusClass statusClass) {
		storeStatus(tree, statusClass.name());
	}
	
	@Override
	public CATStandConnector getHarvestedStand() {
		ExtCompositeStand newScene = (ExtCompositeStand) getInterventionBase();
		Collection<ExtTree> trees = (Collection<ExtTree>) getTrees();
		for (ExtTree tree : trees) {
			tree.setStatusClass(StatusClass.cut); 
			newScene.storeStatus((Numberable) tree, StatusClass.cut, ((Numberable) tree).getNumber());
		}
		newScene.clearTrees();
		try {
			// fc-7.12.2016 the line below was before the try {
			step.getProject().getModel().processPostIntervention(this, newScene);
			step.getProject().processNewStep(step, newScene, "Final cut");
		} catch (Exception e) { // fc-7.12.2016 was InterruptedException
			e.printStackTrace();
		}
		newScene.setInterventionResult(true);
		return (CATStandConnector) newScene;
	}

	/*
	 * This method is involved in the backup of the predictors when passing the predictors to another ExtCompositeStand instance
	 */
	protected Map<String, REpiceaPredictor> getPredictors() {
		return getImmutable().predictors;
	}

	/*
	 * This method is involved in the backup of the predictors when passing the predictors to another ExtCompositeStand instance
	 */
	void setPredictors(Map<String, REpiceaPredictor> oMap) {
		getImmutable().predictors = oMap;
	}
	
	void merge(ExtCompositeStand compositeStand) {
		if (getClass().equals(compositeStand.getClass())) {
			if (getNumberRealizations() == compositeStand.getNumberRealizations()) {
				for (ExtPlotSample plotSample : getPlotSamples()) {
					ExtPlotSample newPlotSample = compositeStand.getRealization(plotSample.getMonteCarloRealizationId());
					if (newPlotSample == null) {
						throw new InvalidParameterException("The realization " + plotSample.getMonteCarloRealizationId() + " does not exist in the new composite stand!");
					}
					Collection<String> ids = newPlotSample.getPlotIds();
					for (String key : ids) {
						ExtPlot plot = newPlotSample.getPlot(key);
						if (plotSample.getPlotIds().contains(key)) {
							throw new InvalidParameterException("The former composite stand already contains a plot with id " + key);
						} else {
							plotSample.addPlot(key, plot);
						}
					}
					plotSample.setArea(); // update the area of the sample
				}
			}
		}
	}

	@Override
	public int getAgeYr() {
		if (getImmutable().initialAgeYr != -99) {
			return getDateYr() - getImmutable().initialDateYr + getImmutable().initialAgeYr;
		} else {
			return getImmutable().initialAgeYr;
		}
	}
	
	/**
	 * Set the climate change scenario.
	 * @param climateChangeScenario a ClimateChangeScenario instance
	 */
	public void setClimateChangeScenario(ClimateChangeScenario climateChangeScenario) {
		getImmutable().climateChangeScenario = climateChangeScenario;
	}

	@Override
	public ClimateChangeScenario getClimateChangeScenario() {return getImmutable().climateChangeScenario;}
	

	
}
