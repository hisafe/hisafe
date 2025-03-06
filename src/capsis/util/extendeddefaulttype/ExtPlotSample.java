/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin 
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
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.extension.modeltool.carbonstorageinra.CATStandConnector;
import repicea.simulation.HierarchicalLevel;
import repicea.simulation.MonteCarloSimulationCompliantObject;
import repicea.simulation.covariateproviders.plotlevel.AreaHaProvider;
import repicea.simulation.covariateproviders.plotlevel.NaturalDisturbanceInformationProvider;
import repicea.simulation.covariateproviders.plotlevel.TreeStatusCollectionsProvider;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.simulation.disturbances.DisturbanceOccurrences;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;

/**
 * The PlotSample class contains a sample of plots from a stand or a larger
 * population.
 * 
 * @author Mathieu Fortin - July 2015
 */
public abstract class ExtPlotSample extends TreeList implements TreeStatusCollectionsProvider, 
														AreaHaProvider,		
														MonteCarloSimulationCompliantObject, 
														CATStandConnector,
														NaturalDisturbanceInformationProvider {


	private final Map<String, ExtPlot> internalPlotList;
	protected final ExtCompositeStand compositeTreeList;
	private int monteCarloRealizationID = -1;
	private double areaM2;
	private Map<DisturbanceType, DisturbanceOccurrences> disturbanceOccurrencesMap;
	private Map<DisturbanceType, Integer> lastOccurrenceMap;
	
	protected ExtPlotSample(ExtCompositeStand compositeTreeList, int i) {
		super();
		this.compositeTreeList = compositeTreeList;
		internalPlotList = new TreeMap<String, ExtPlot>();
		createImmutable();
		setMonteCarloRealizationId(i);
	}

	protected void recordDisturbanceOccurrences(DisturbanceType type, DisturbanceOccurrences occurrences) {
		if (disturbanceOccurrencesMap == null) {
			disturbanceOccurrencesMap = new HashMap<DisturbanceType, DisturbanceOccurrences>();
		}
		disturbanceOccurrencesMap.put(type, occurrences);
	}
	
	protected DisturbanceOccurrences getDisturbanceOccurrences(DisturbanceType type) {
		if (disturbanceOccurrencesMap != null) {
			return disturbanceOccurrencesMap.get(type);
		} else {
			return null;
		}
	}

	private Map<String, ExtPlot> getPlotMap() {
		return internalPlotList;
	}
	

	/**
	 * Provide the collection of ExtPlot instances. It is equivalent to 
	 * getPlots(false).
	 * @return the collection of ExtPlot instance contained in the sample
	 */
	public Collection<ExtPlot> getPlots() {
		return getPlots(false);
	}

	/**
	 * Provide a list of the plots. If the shuffled parameter is set to
	 * true, the order in which the ExtPlot instances are returned is random. Otherwise,
	 * the TreeMap instance behind this list ensures a consistent order.
	 * @param shuffled a boolean
	 * @return a List of ExtPlot instance contained in the sample
	 */
	public List<ExtPlot> getPlots(boolean shuffled) {
		List<ExtPlot> plots = new ArrayList<ExtPlot>();
		plots.addAll(getPlotMap().values());	// to avoid changes in the collection at some point that would be reflected in the map
		if (shuffled) {
			Collections.shuffle(plots);
		} 		
		return plots;
	}

	
	public Collection<String> getPlotIds() {
		return getPlotMap().keySet();
	}
	
	public void addPlot(String id, ExtPlot plot) {
		getPlotMap().put(id, plot);
		plot.setPlotSample(this);
	}
	
	public ExtPlot getPlot(String id) {
		return getPlotMap().get(id);
	}
	
	
	@Override
	public Collection<? extends Tree> getTrees() {
		Collection coll = new ArrayList();
		for (ExtPlot treeList : internalPlotList.values()) {
			coll.addAll(treeList.getTrees());
		}
		return coll;
	}

	/**
	 * This method returns the trees that match a particular StatusClass. To be
	 * preferred over getTrees(String status).
	 * @param statusClass a StatusClass enum variable
	 * @return a Collection of Tree instances
	 */
	@Override
	public Collection<Tree> getTrees(StatusClass statusClass) {
		return getTrees(statusClass.name());
	}

	@Override
	public Collection<Tree> getTrees(String status) {
		Collection<Tree> coll = new ArrayList<Tree>();

		if (status == null) {
			return coll;
		}

		for (ExtPlot plot : internalPlotList.values()) {
			coll.addAll(plot.getTrees(status));
		}
		return coll;
	}

	@Override
	public Set<String> getStatusKeys() {
		Set<String> oStatusCollection = new HashSet<String>();

		for (ExtPlot treeList : internalPlotList.values()) {
			oStatusCollection.addAll(treeList.getStatusKeys());
		}
		return oStatusCollection;
	}

	@Override
	public double getAreaHa() {
		return getArea() * .0001;
	}

	@Override
	public double getArea() {
		if (areaM2 == 0d) {
			setArea();
		}
		return areaM2;
	}

	void setArea() {
		double area = 0d;
		for (ExtPlot treeList : internalPlotList.values()) {
			area += treeList.getArea();
		}
		areaM2 = area;
	}
	
	
	
	@Override
	public void setInterventionResult(boolean b) {
		super.setInterventionResult(b);

		if (getPlotMap() == null) {
			return;
		}

		for (TreeList stand : getPlotMap().values()) {
			stand.setInterventionResult(b);
		}
	}

	@Override
	public void clearTrees() {
		for (TreeCollection stand : getPlotMap().values()) {
			stand.clearTrees();
		}
	}

	@Override
	public int size() {
		if (getPlotMap() != null) {
			return getPlotMap().size();
		} else {
			return 0;
		}
	}

	@Override
	public String toString() {
		return "PlotSample_" + monteCarloRealizationID;
	}

	@Override
	public String getSubjectId() {
		return "0";
	}

	@Override
	public HierarchicalLevel getHierarchicalLevel() { // Should be called only
														// if the scale of
														// application is set to
														// Stand
		return HierarchicalLevel.PLOT; // TODO FP this part is unclear and
										// should obey to a spatial model
	}

	@Override
	public int getMonteCarloRealizationId() {return monteCarloRealizationID;}

	@Override
	public String getStandIdentification() {
		return getCompositeStand().getStandIdentification();
	}

	@Override
	public int getDateYr() {
		return getCompositeStand().getDateYr();
	}

	/**
	 * This method updates the stand variables such as the basal area and so on.
	 */
	public abstract void updatePlotVariables();

	protected void initialize(ExtInitialParameters parms) {
		updatePlotVariables();
	}
	
	
	@Override
	public ManagementType getManagementType() {return getCompositeStand().getManagementType();}

	@Override
	public ApplicationScale getApplicationScale() {return getCompositeStand().getApplicationScale();}

	@Override
	public int getAgeYr() {return getCompositeStand().getAgeYr();}
	
	
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
		ExtPlotSample newScene = (ExtPlotSample) getInterventionBase();
		Collection<ExtTree> trees = (Collection<ExtTree>) newScene.getTrees();
		for (ExtTree tree : trees) {
			tree.setStatusClass(StatusClass.cut); 
			newScene.storeStatus((Numberable) tree, StatusClass.cut, ((Numberable) tree).getNumber());
		}
		newScene.clearTrees();
//		try {		// NO NEED FOR SAVING THIS IN THE PROJECT
//			// fc-7.12.2016 the line below was before the try {
//			step.getProject().getModel().processPostIntervention(this, newScene);
//			step.getProject().processNewStep(step, newScene, "Final cut");
//		} catch (Exception e) { // fc-7.12.2016 was InterruptedException
//			e.printStackTrace();
//		}
		newScene.setInterventionResult(true);
		return (CATStandConnector) newScene;
	}

	/**
	 * Returns the ExtCompositeStand the sample belongs to.
	 * @return an ExtCompositeStand instance
	 */
	public ExtCompositeStand getCompositeStand() {
		return compositeTreeList;
	}

	@Override
	public int getTimeSinceFirstKnownDateYrs(int currentDateYr) {
		return currentDateYr - compositeTreeList.getInitialCreationDateYr();
	}

	
	public void setLastOccurrences(ExtPlotSample formerPlotSample) {
		for (DisturbanceType type : DisturbanceType.values()) {
			if (type != DisturbanceType.Harvest) {
				int lastOccurrence = getLastOccurrenceFromFormerSample(type, formerPlotSample);
				if (lastOccurrence != -1) {
					recordLastOccurrence(type, lastOccurrence);
				}
			}
		}
	}
	
	private int getLastOccurrenceFromFormerSample(DisturbanceType type, ExtPlotSample formerPlotSample) {
		if (formerPlotSample.disturbanceOccurrencesMap != null && formerPlotSample.disturbanceOccurrencesMap.containsKey(type)) {
			DisturbanceOccurrences occurrences = formerPlotSample.disturbanceOccurrencesMap.get(type);
			int lastOccurrence = occurrences.getLastOccurrenceDateYrToDate(getDateYr());
			if (lastOccurrence != -1) {
				return lastOccurrence;
			}
		} 
		return formerPlotSample.getLastOccurrence(type);
	}
	
	private void recordLastOccurrence(DisturbanceType type, int dateYr) {
		if (lastOccurrenceMap == null) {
			lastOccurrenceMap = new HashMap<DisturbanceType, Integer>();
		}
		lastOccurrenceMap.put(type, dateYr);
	}
	
	private int getLastOccurrence(DisturbanceType type) {
		if (lastOccurrenceMap != null) {
			Integer lastOccurrence = lastOccurrenceMap.get(type);
			if (lastOccurrence == null) {
				return -1;
			} else {
				return lastOccurrence;
			}
		} else {
			return -1;
		}
	}
	
	
	@Override
	public Integer getTimeSinceLastDisturbanceYrs(DisturbanceType type, int currentDateYr) {
		int lastOccurrence = getLastOccurrence(type);
		if (lastOccurrence != -1) {
			System.out.println("Time since last disturbance = " + lastOccurrence);
			return currentDateYr - lastOccurrence;
		} else {
			return null;
		}
	}


	public void setMonteCarloRealizationId(int monteCarloRealizationId) {
		if (this.monteCarloRealizationID != -1 && this.monteCarloRealizationID != monteCarloRealizationId) {
			getCompositeStand().removePlotSample(this.monteCarloRealizationID);	// remove the key-value for the former monte carlo id
		}
		this.monteCarloRealizationID = monteCarloRealizationId;
		getCompositeStand().addPlotSample(this);
	}
	
}
