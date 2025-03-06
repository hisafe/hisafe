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

import java.util.ArrayList;
import java.util.Collection;

import capsis.defaulttype.NumberableTree;
import capsis.defaulttype.Tree;
import capsis.kernel.GScene;
import capsis.util.extendeddefaulttype.ExtCompositeStand.PredictorID;
import capsis.util.methodprovider.TreeBasicFeatures;
import capsis.util.methodprovider.TreeStatus;
import jeeb.lib.util.Log;
import repicea.simulation.HierarchicalLevel;
import repicea.simulation.MonteCarloSimulationCompliantObject;
import repicea.simulation.REpiceaPredictor;
import repicea.simulation.allometrycalculator.AllometryCalculableTree;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider;
import repicea.simulation.hdrelationships.HDRelationshipStand;
import repicea.simulation.hdrelationships.HDRelationshipTree;
import repicea.simulation.hdrelationships.HeightPredictor;

public abstract class ExtTree extends NumberableTree implements TreeBasicFeatures, 
																Cloneable, 
																TreeStatusProvider,
																AllometryCalculableTree, 
																MonteCarloSimulationCompliantObject {

	/**
	 * This class contains immutable instance variables for a logical ArtTree.
	 * 
	 * @see Tree
	 */
	public static class Immutable extends NumberableTree.Immutable {
		private String initialSpeciesName; // name of the species in the input file
		public ExtSpeciesGroup species;
	}

	private StatusClass statusClass;
	private double height;
	protected double dbh;
	
	private double commercialVolumeM3;
	

	/**
	 * General constructor.
	 * 
	 * @param id
	 * @param stand
	 * @param age
	 * @param height
	 * @param dbh
	 * @param number
	 */
	public ExtTree(int id, GScene stand, int age, double height, double dbh, double number, ExtSpeciesGroup species) {
		super(id, stand, age, height, dbh, false, number, -1);
		this.statusClass = StatusClass.alive;
		getImmutable().species = species;
	}

	/**
	 * Constructor for survivor tree.
	 * 
	 * @param modelTree
	 *            a QuebecMRNFTree instance
	 * @param stand
	 *            the QuebecMRNFStand instance whose tree is
	 */
	@Deprecated
	protected ExtTree(ExtTree modelTree, GScene stand) {
		super(modelTree, stand, modelTree.age + 1, modelTree.getHeight(), modelTree.getDbh(), false, modelTree.getNumber(), -1); // age
																												// =
																												// 0,
																												// marked
																												// =
																												// false,
																												// numberOfDead
																												// =
																												// -1,
																												// height
																												// =
																												// -1.0
		this.statusClass = StatusClass.alive;
		getImmutable().species = modelTree.getSpecies();
	}

	
	/**
	 * This method is called in the processMortality from the model.
	 * @param scene an ExtendedPlot instance
	 * @return an ExtTree instance
	 */
	protected ExtTree createSurvivorTree(ExtPlot stand) {
		ExtTree newTree = clone();
		newTree.scene = stand;
		newTree.age += 1;
		return newTree;
	}

	/**
	 * Create an Immutable object whose class is declared at one level of the
	 * hierarchy. This is called only in constructor for new logical object in
	 * superclass. If an Immutable is declared in subclass, subclass must
	 * redefine this method (same body) to create an Immutable defined in
	 * subclass.
	 */
	@Override
	protected void createImmutable() {
		immutable = new Immutable();
	}

	protected Immutable getImmutable() {
		return (Immutable) immutable;
	}

	@Override
	public void setHeight(double h) {height = h;}

	@Override
	public void setDbh(double d) {dbh = d;}

	
	/**
	 * This method provides a wrapper for the status class in the different data
	 * extractor.
	 * 
	 * @return a TreeStatus which extends EnumProperty
	 */
	public TreeStatus getTreeStatus() {
		return TreeStatus.getStatus(getStatusClass());
	}

	@Override
	public StatusClass getStatusClass() {
		return statusClass;
	}

	@Override
	public void setStatusClass(StatusClass statusClass) {
		this.statusClass = statusClass;
	}

	@Override
	public double getDbhCm() {
		return getDbh();
	}

	@Override
	public double getSquaredDbhCm() {
		return getDbh2();
	}

	@Override
	public double getStemBasalAreaM2() {
		double dbh2 = getDbh2();
		return Math.PI * dbh2 * 0.000025;
	}

	@Override
	public double getPlotWeight() {
		if (getScene() == null) {
			int u = 0;
		}
		return ((ExtPlot) getScene()).getWeight();
	}

	@Override
	public double getHeightM() {
		return getHeight();
	}

	@Override
	public double getHeight() {return height;}

	@Override
	public double getDbh() {return dbh;}


	@Override
	public abstract double getTotalVolumeM3();

	/**
	 * Return the square dbh.
	 */
	protected double getDbh2() {return dbh * dbh;}

	/**
	 * This method returns the name of the species group to which belong that
	 * tree.
	 * 
	 * @return a String
	 */
	public String getSpeciesName() {
		return getSpecies().getName();
	}

	/**
	 * This method returns the species features of this tree.
	 * 
	 * @return a QuebecMRNFSpecies instance
	 */
	public ExtSpeciesGroup getSpecies() {
		return getImmutable().species;
	}

	@Override
	public String getSubjectId() {
		return ((Integer) getId()).toString();
	}

	@Override
	public HierarchicalLevel getHierarchicalLevel() {
		return HierarchicalLevel.TREE;
	}

	@Override
	public int getMonteCarloRealizationId() {
		return ((ExtPlot) getScene()).getMonteCarloRealizationId();
	}

	/**
	 * This method returns the initial species name or an empty string if this
	 * field has not been set
	 * 
	 * @return a String
	 */
	public String getInitialSpeciesName() {
		if (getImmutable().initialSpeciesName != null) {
			return getImmutable().initialSpeciesName;
		} else {
			return "";
		}
	}

	protected void setInitialSpeciesName(String speciesName) {
		getImmutable().initialSpeciesName = speciesName;
	}
	

	/**
	 * This method handles a change in the status such as in the mortality or
	 * the thinning process. The hostPlot parameter can be either the current plot
	 * or the new plot. If the ExtTree instance is already contained in the hostPlot 
	 * parameter, it is removed before changing the status so that it avoids duplicating
	 * the tree.
	 * 
	 * @param event a boolean (in case of stochastic simulation) or a double (in case of
	 * deterministic simulation)
	 * @param newPlot the plot that will contained the ExtTree instance
	 * @param newStatus a StatusClass instance
	 * @param ratioOfTimeStep a double ranging from 0 to 1 which stands for the proportion of the growth step until the status change
	 * @param currentPlot the ExtPlot instance that is the current plot and NOT the new plot
	 */
	public final void processStatusChange(Object event, ExtPlot newPlot, StatusClass newStatus, Double ratioOfTimeStep, ExtPlot currentPlot) {
		Collection<ExtTree> newTrees = new ArrayList<ExtTree>();

		boolean isAlreadyPartOfThisScene = newPlot.getTrees().contains(this);

		ExtTree survivorTree;

		if (isAlreadyPartOfThisScene) {
			survivorTree = this;
		} else {
			survivorTree = createSurvivorTree(newPlot);
		}

		ExtTree treeWithNewStatus;
		if (event instanceof Boolean) {
			boolean changingStatus = (Boolean) event;
			if (changingStatus) {
				treeWithNewStatus = clone();
				treeWithNewStatus.setScene(newPlot);
				treeWithNewStatus.setStatusClass(newStatus);
				if (currentPlot != null) {
					treeWithNewStatus.processGrowth(currentPlot.getStratum().getPredictor(PredictorID.DIAMETER_GROWTH),
							currentPlot, 
							ratioOfTimeStep);		// update the diameter
					treeWithNewStatus.updateHeightM((HeightPredictor) currentPlot.getStratum().getPredictor(PredictorID.HD_RELATIONSHIP), currentPlot); // update height
					treeWithNewStatus.updateCommercialVolumeM3(currentPlot.getStratum().getPredictor(PredictorID.COMMERCIAL_VOLUME), currentPlot);  // update commercial volume
				}
				newTrees.add(treeWithNewStatus);
				if (isAlreadyPartOfThisScene) {
					newPlot.removeTree(this);
				}
			} else if (!isAlreadyPartOfThisScene) {
				newTrees.add(survivorTree);
			}
		} else if (event instanceof Double) {
			double eventProbability = (Double) event;
			double numberBeforeProcessing = survivorTree.number;
			survivorTree.number *= (1.0 - eventProbability);
			if (survivorTree.number > ExtSimulationSettings.VERY_SMALL) {
				if (!isAlreadyPartOfThisScene) {
					newTrees.add(survivorTree);
				}
			} else {
				if (isAlreadyPartOfThisScene) {
					newPlot.removeTree(this);
				}
			}
			if ((numberBeforeProcessing - survivorTree.number) > ExtSimulationSettings.VERY_SMALL) { 
				treeWithNewStatus = clone();
				treeWithNewStatus.setScene(newPlot);
				treeWithNewStatus.number = numberBeforeProcessing - survivorTree.number;
				treeWithNewStatus.setStatusClass(newStatus);
				newTrees.add(treeWithNewStatus);
			}
		}

		if (!newTrees.isEmpty()) {
			for (ExtTree tree : newTrees) {
				newPlot.storeStatus(tree, tree.getStatusClass(), tree.getNumber());
			}
		}

	}
	
	/**
	 * This method handles a change in the status such as in the mortality or
	 * the thinning process. The hostPlot parameter can be either the current plot
	 * or the new plot. If the ExtTree instance is already contained in the hostPlot 
	 * parameter, it is removed before changing the status so that it avoids duplicating
	 * the tree.
	 * 
	 * @param event a boolean (in case of stochastic simulation) or a double (in case of
	 * deterministic simulation)
	 * @param newPlot the plot that will contained the ExtTree instance
	 * @param newStatus a StatusClass instance
	 * @param dateOfChangeYr an integer that stands for the date at which occurred the status change 
	 * @param currentPlot the ExtPlot instance that is the current plot and NOT the new plot
	 */
	public void processStatusChange(Object event, ExtPlot newPlot, StatusClass newStatus) {
		processStatusChange(event, newPlot, newStatus, 0d, null);
	}
	
	/**
	 * Increment the diameter of the height.
	 * @param growthPredictor a REpiceaPredictor instance 
	 * @param currentPlot the ExtPlot instance to be used with the predictors
	 * @param ratioOfTimeStep a double that ranges from 0 to 1. 
	 */
	protected abstract void processGrowth(REpiceaPredictor growthPredictor, ExtPlot currentPlot, double ratioOfTimeStep);

	/**
	 * This method handles the mortality at the tree level. The method takes in
	 * charge the update of the tree list in the new stand.
	 * 
	 * @param dDeathProbability
	 *            a double between 0 and 1 that represents the probability of
	 *            mortality
	 * @param newStand
	 *            a QuebecMRNFStand instance that contains the tree
	 * @param bMonteCarlo
	 *            a boolean (true if the model operates in stochastic mode or
	 *            false if it is deterministic)
	 * @deprecated use {@link ExtTree#processStatusChange(Object, ExtPlot, repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass, Double, ExtPlot)} instead
	 */
	@Deprecated
	public void processMortality(double dDeathProbability, ExtPlot newStand, boolean bMonteCarlo) {
		Collection<ExtTree> newTrees = new ArrayList<ExtTree>();

		ExtTree survivorTree = createSurvivorTree(newStand);
		ExtTree deadTree;

		if (bMonteCarlo) { // stochastic
			double residualError = ExtModel.RANDOM.nextDouble();
			if (residualError < dDeathProbability) {
				deadTree = clone();
				deadTree.setScene(newStand);
				deadTree.setStatusClass(StatusClass.dead);
				newTrees.add(deadTree);
			} else {
				newTrees.add(survivorTree);
			}
		} else { // deterministic
			survivorTree.number *= (1.0 - dDeathProbability);
			if (survivorTree.number > ExtSimulationSettings.VERY_SMALL) {
				newTrees.add(survivorTree);
			}
			if ((this.number - survivorTree.number) > ExtSimulationSettings.VERY_SMALL) { 
				deadTree = clone();
				deadTree.setScene(newStand);
				deadTree.number *= dDeathProbability;
				deadTree.setStatusClass(StatusClass.dead);
				newTrees.add(deadTree);
			}
		}

		if (!newTrees.isEmpty()) {
			for (ExtTree tree : newTrees) {
				newStand.storeStatus(tree, tree.getStatusClass().name(), tree.getNumber());
			}
		}
	}

	@Override
	public ExtTree clone() {
		try {
			ExtTree t = (ExtTree) super.clone();
			return t;
		} catch (Exception exc) {
			Log.println(Log.ERROR, "ExtendedTree.clone ()", "Error while cloning tree." + " Source tree=" + toString()
					+ " " + exc.toString(), exc);
			return null;
		}
	}


	/**
	 * Set the height of the tree, given the current plot and a model of HD relationships 
	 * @param heightPredictor a HeightPredictor instance
	 * @param currentPlot an ExtPlot instance
	 */
	public void updateHeightM(HeightPredictor heightPredictor, ExtPlot currentPlot) {
		double height = heightPredictor.predictHeightM((HDRelationshipStand) currentPlot, (HDRelationshipTree) this);
		if (height != -1) {
			setHeight(height);
		}
	}
	
	/**
	 * Set the commercial volume of the tree, given the current plot and a model of volume
	 * @param volumePredictor an REpiceaPredictor instance
	 * @param currentPlot an ExtPlot instance
	 */
	public abstract void updateCommercialVolumeM3(REpiceaPredictor volumePredictor, ExtPlot currentPlot);
	
	@Override
	public double getCommercialVolumeM3() {return commercialVolumeM3;}

	/**
	 * This method sets the merchantable volume in m3
	 * @param volume the volume in m3
	 */
	protected void setCommercialVolumeM3(double volume) {commercialVolumeM3 = volume;}

}
