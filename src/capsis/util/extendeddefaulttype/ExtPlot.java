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
import java.util.Map;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.util.EnumProperty;
import capsis.util.extendeddefaulttype.ExtCompositeStand.PredictorID;
import capsis.util.extendeddefaulttype.disturbances.ThinningDisturbanceOccurrences;
import repicea.simulation.HierarchicalLevel;
import repicea.simulation.MonteCarloSimulationCompliantObject;
import repicea.simulation.REpiceaPredictor;
import repicea.simulation.allometrycalculator.AllometryCalculableTree;
import repicea.simulation.allometrycalculator.AllometryCalculator;
import repicea.simulation.covariateproviders.plotlevel.AreaHaProvider;
import repicea.simulation.covariateproviders.plotlevel.BasalAreaM2HaProvider;
import repicea.simulation.covariateproviders.plotlevel.DateYrProvider;
import repicea.simulation.covariateproviders.plotlevel.GeographicalCoordinatesProvider;
import repicea.simulation.covariateproviders.plotlevel.InterventionPlannedProvider;
import repicea.simulation.covariateproviders.plotlevel.PlotWeightProvider;
import repicea.simulation.covariateproviders.plotlevel.TreeStatusCollectionsProvider;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.simulation.disturbances.DisturbanceOccurrences;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;
import repicea.simulation.hdrelationships.HeightPredictor;
import repicea.simulation.thinners.REpiceaTreatmentDefinition;

/**
 * The AbstractExtendedTreeList class implements the MonteCarlo technique for a TreeList instance.
 * @author Mathieu Fortin - October 2013
 */
public abstract class ExtPlot extends TreeList implements MonteCarloSimulationCompliantObject, 
																			BasalAreaM2HaProvider,
																			AreaHaProvider,
																			TreeStatusCollectionsProvider,
																			GeographicalCoordinatesProvider,
																			InterventionPlannedProvider,
																			PlotWeightProvider,
																			DateYrProvider {

	private static AllometryCalculator allometryCalculator;
	
	public static class Immutable extends TreeList.Immutable {

		public String ID;
		public double longitude;
		public double latitude;
		public double altitude;	
		public double weight = 1d;
	}

	private ExtPlotSample sample;
	private boolean isGoingToBeHarvested;
	private ExtHistoricalRecordList history;
	private ThinningDisturbanceOccurrences lastThinning;

	/**
	 * Constructor.
	 * @param ID a String
	 * @param monteCarloID a MonteCarlo realization
	 */
	protected ExtPlot(String ID) {
		super();
		getImmutable().ID = ID;
	}
	

	private synchronized void createAllometryCalculator() {
		if (allometryCalculator == null) {
			allometryCalculator = new AllometryCalculator();
		}
	}

	/**
	 * This method returns an AllometryCalculator which can be used to calculate
	 * simple allometry features.
	 * @return a allometry calculator (AllometryCalculator)
	 */
	public AllometryCalculator getAllometryCalculator() {
		if (allometryCalculator == null) {
			createAllometryCalculator();
		}
		return allometryCalculator;
	}

	protected void setLastThinningOccurrence(ThinningDisturbanceOccurrences occ) {lastThinning = occ;}
	protected ThinningDisturbanceOccurrences getLastThinningOccurrence() {return lastThinning;}
	
	@Override
	protected Immutable getImmutable() {return (Immutable) immutable;}
	
	@Override
	protected void createImmutable () { immutable = new Immutable();}

	@Override
	public ExtPlot getLightClone() {
		ExtPlot plot = (ExtPlot) super.getLightClone();
		if (history != null) {
			plot.history = history.clone();
		}
		return plot;
	}
	
	/*
	 * Extended visibility (non-Javadoc)
	 * @see capsis.defaulttype.TreeList#getHeavyClone()
	 */
	@Override
	public ExtPlot getHeavyClone () {
		ExtPlot copy = (ExtPlot) super.getHeavyClone();
		return copy;
	}
	
	/**
	 * This method differs from ExtPlot.getHeavyClone as it also includes the non alive trees in the clone.
	 */
	protected final ExtPlot getHeavyCloneForInitialization() {
		ExtPlot clonePlot = getHeavyClone();
		Collection<? extends Tree> otherTreesIfAny = getTreesNotAlive();
		for (Tree t : otherTreesIfAny) {
			ExtTree cloneTree = (ExtTree) t.clone();
			if (cloneTree instanceof Numberable) {
				cloneTree.setScene(clonePlot);
				clonePlot.storeStatus(cloneTree, cloneTree.getStatusClass());
			}
		}
		return clonePlot;
	}
	
	
	@Override
	public int getDateYr () {return getDate();}

	public String getId() {return getImmutable().ID;}

	public void setLongitudeDeg(double fValue) {getImmutable().longitude = fValue;}

	@Override
	public double getLongitudeDeg() {
		return getImmutable().longitude;
	}
	
	public void setLatitudeDeg(double fValue) {getImmutable().latitude = fValue;}
	
	@Override
	public double getLatitudeDeg() {
		return getImmutable().latitude;
	}
	
	public void setElevation(double fValue) {getImmutable().altitude = fValue;}
	@Override
	public double getElevationM() {return getImmutable().altitude;}

	@Override
	public boolean isGoingToBeHarvested() {return isGoingToBeHarvested;}
	
	public void setGoingToBeHarvested(boolean bool) {isGoingToBeHarvested = bool;}

	@Override
	public HierarchicalLevel getHierarchicalLevel() {return HierarchicalLevel.PLOT;}


	@Override
	public String getSubjectId() {
		return getId();
	}
	
	/**
	 * This method returns the stratum this plot belongs to.
	 * @return a ExtCompositeStand instance
	 */
	public ExtCompositeStand getStratum() {return sample.getCompositeStand();}

	/**
	 * Returns the sample the plot belongs to.
	 * @return an ExtPlotSample instance
	 */
	public ExtPlotSample getPlotSample() {return sample;}
	
	@Override
	public int getMonteCarloRealizationId() {return getPlotSample().getMonteCarloRealizationId();}
	

	@Override
	public Collection<Tree> getTrees(StatusClass statusClass) {
		return getTrees(statusClass.name());
	}
	

	protected void setPlotSample(ExtPlotSample sample) {
		this.sample = sample;
	}
	

	@Override
	public void setWeight(double weight) {getImmutable().weight = weight;}

	@Override
	public double getWeight() {return getImmutable().weight;}

	
	/**
	 * Provide collections of trees by species for a particular status class.
	 * 
	 * @param statusClass a StatusClass enum (StatusClass.alive, StatusClass.dead, StatusClass.cut, StatusClass.windfall)
	 * @return a Map instance with species names as keys and collections as values
	 */
	public Map<String, Collection<AllometryCalculableTree>> getCollectionsBySpecies(StatusClass statusClass) {
		Collection<EnumProperty> index = getStratum().getSpeciesGroupTags().values();
		return getStratum().getCollectionsBySpecies(index, getTrees(statusClass));
	}

	/**
	 * Provide collections of living trees by species.
	 * 
	 * @return a Map instance with species names as keys and collections as values
	 * @see ExtPlot#getCollectionsBySpecies(StatusClass)
	 */
	public Map<String, Collection<AllometryCalculableTree>> getCollectionsBySpecies() {
		return this.getCollectionsBySpecies(StatusClass.alive);
	}

	
	@Override
	public double getAreaHa() {return getArea() * .0001;}
	
	/**
	 * Store Numberable instance under a particular status. <p>
	 * 
	 * This method is a proxy for the method in the TreeList class.  
	 * 
	 * @param tree a Numberable instance
	 * @param statusClass a StatusClass enum
	 * @param numberUnderThisStatus the number of trees stored under this status
	 * @see TreeList#storeStatus(Numberable, String, double)
	 */
	public void storeStatus(Numberable tree, StatusClass statusClass, double numberUnderThisStatus) {
		storeStatus(tree, statusClass.name(), numberUnderThisStatus);
	}

	/**
	 * Store tree instance under a particular status. <p>
	 * 
	 * This method has been modified from its original code in the TreeList class. It requires 
	 * Numberable trees or it will throw an exception. It then processes the status changes using
	 * the ExtPlot#storeStatus(Numberable, String, double) method and set the numberUnderThisStatus 
	 * as the number of trees represented by the Numberable instance. In other words, all the trees 
	 * represented by the Numberable instance change status.
	 * 
	 * @param tree a Tree instance (must be Numberable)
	 * @param statusClass a StatusClass enum
	 * @see ExtPlot#storeStatus(Numberable, String, double)
	 * @throws InvalidParameterException if the tree argument is not a Numberable instance
	 */
	public void storeStatus(Tree tree, StatusClass statusClass) {
		if (!(tree instanceof Numberable)) {
			throw new InvalidParameterException("The method ExtPlot.storeStatus is meant for Numberable tree instances!");
		}
		storeStatus((Numberable) tree, statusClass.name(), ((Numberable) tree).getNumber());
	}

	protected boolean isThisPlotGoingToBeAffectedByThisNaturalDisturbance(DisturbanceType type) {
		DisturbanceOccurrences occurrences = getPlotSample().getDisturbanceOccurrences(type);
		return occurrences != null && occurrences.isThisPlotAffected(this);
	}

	
	/**
	 * Check if the stand can be harvested. <br>
	 * <br>
	 * A stand cannot be harvested if it has
	 * been harvested and the delay before re-entry is not over. 
	 * @return a boolean
	 */
	public boolean canBeHarvested(ThinningDisturbanceOccurrences newOccurrence) {
		return canBeHarvested(newOccurrence.getLastOccurrenceDateYr());
	}

	
	/**
	 * Check if the stand could be harvested within this time step. <br>
	 * <br>
	 * A stand cannot be harvested if it has
	 * been harvested and the delay before re-entry is not over. 
	 * @return a boolean
	 */
	public boolean canBeHarvested(int endDateYr) {
		if (getLastThinningOccurrence() == null) {
			return true;
		} else {
			int dateYrPreviousOccurrence = getLastThinningOccurrence().getLastOccurrenceDateYr();
			REpiceaTreatmentDefinition def = (REpiceaTreatmentDefinition) getLastThinningOccurrence().getTreatmentDefinition();
			if (def != null) {
				int delayBeforeNextTreatment = def.getDelayBeforeReentryYrs();
				return endDateYr - dateYrPreviousOccurrence >= delayBeforeNextTreatment;  
			} else {	// if there is no definition, then it is assumed that there is no delay before re-entry
				return true;
			}
		}
	}

	
	
	
	/**
	 * Retrieves the information such as the climate variability from previous plot.
	 * @param formerPlot an ExtPlot instance
	 */
	public abstract void retrievePlotHistoryFromFormerPlot(ExtPlot formerPlot); 
	
	protected void recordHistory(ExtHistoricalRecord rec) {
		if (history == null) {
			history = new ExtHistoricalRecordList();
		}
		getHistory().add(rec);
	}
	
	protected ExtHistoricalRecordList getHistory() {
		return history;
	}
	
	/**
	 * Returns the history of this plot. The ExtHistoricalRecordList is cloned to avoid any change to
	 * the original copy. However, this may slow down the processing. For a direct access, use the 
	 * protected method getHistory
	 * @return an ExtHistoricalRecordList instance
	 */
	public final ExtHistoricalRecordList getPlotHistory() {
		if (getHistory() != null) {
			return getHistory().clone();
		}
		return null;
	}

	/**
	 * Provide a collection of the trees that are not alive, that is dead, cut or windfall
	 * @return a Collection of Tree instances
	 */
	private Collection<? extends Tree> getTreesNotAlive() {
		Collection<? extends Tree> trees = new ArrayList<ExtTree>();
		for (StatusClass s : StatusClass.values()) {
			if (s != StatusClass.alive)
				trees.addAll((Collection) getTrees(s));
		}
		return trees;
	}
	
	/**
	 * Provide a collection with trees of all statuses (alive, dead, cut, windfall).
	 * @return a Collection of Tree instances
	 */
	protected final Collection<? extends Tree> getAllTrees() {
		Collection<? extends Tree> trees = new ArrayList<ExtTree>();
		trees.addAll((Collection) getTreesNotAlive());
		trees.addAll((Collection) getTrees());
//		for (StatusClass s : StatusClass.values()) {
//			trees.addAll((Collection) getTrees(s));
//		}
		return trees;
	}
	
	
	/**
	 * Set the heights of the trees in this plot.
	 */
	public void setTreeHeights() {
		HeightPredictor heightModel = (HeightPredictor) getStratum().getPredictor(PredictorID.HD_RELATIONSHIP);
		Collection<? extends Tree> treesToUpdate = this.isInitialScene() ? 
				getAllTrees() : // all trees must be updated in the initial scene
					getTrees();	// just alive for subsequent growth steps
		for (Object t : treesToUpdate) {
			ExtTree tree = (ExtTree) t;
			tree.updateHeightM(heightModel, this);
		}
	}

	/**
	 * Set the volume of the trees in this plot.
	 */
	public final void setTreeVolumes() {
		REpiceaPredictor volumeModel = getStratum().getPredictor(PredictorID.COMMERCIAL_VOLUME);
		Collection<? extends Tree> treesToUpdate = this.isInitialScene() ? 
				getAllTrees() : // all trees must be updated in the initial scene
					getTrees();	// just alive for subsequent growth steps
		for (Object t : treesToUpdate) {
			ExtTree tree = (ExtTree) t;
			tree.updateCommercialVolumeM3(volumeModel, this);
		}
	}


	
}
