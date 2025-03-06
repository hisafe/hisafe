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

import java.awt.Container;
import java.awt.Window;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import capsis.kernel.Step;
import capsis.util.extendeddefaulttype.methodprovider.PeriodicAnnualIncrementEstimatesProvider.GrowthComponent;
import repicea.io.GExportFieldDetails;
import repicea.io.GExportRecord;
import repicea.io.REpiceaRecordSet;
import repicea.io.tools.REpiceaExportTool;
import repicea.io.tools.REpiceaExportToolDialog;
import repicea.lang.MemoryWatchDog;
import repicea.math.Matrix;
import repicea.math.SymmetricMatrix;
import repicea.simulation.allometrycalculator.AllometryCalculableTree;
import repicea.simulation.allometrycalculator.AllometryCalculator;
import repicea.simulation.covariateproviders.MethodProviderEnum.VariableForEstimation;
import repicea.simulation.covariateproviders.treelevel.TreeStatusProvider.StatusClass;
import repicea.stats.estimates.Estimate;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * This class implements the REpiceaExportTool in Capsis.
 * @author Mathieu Fortin - May 2016
 */
@SuppressWarnings({"rawtypes", "unchecked"})
public abstract class ExtExportTool extends REpiceaExportTool {


	/**
	 * This enum variable represents the different export formats available for 
	 * the QuebecMRNF modules.
	 * @author Mathieu Fortin - April 2011
	 */
	public static enum ExportType implements TextableEnum {
		/**
		 * Basic tree level information.
		 */
		TREE("Tree level", "\u00C9chelle de l'arbre"),
		/**
		 * Tree-level export with biomass by compartment.
		 */
		TREEBIO("Tree-level biomass", "\u00C9chelle de l'arbre avec biomasse"),
		/**
		 * Stand-level export with species breakdown.
		 */
		STAND_SPECIES("Plot level with species breakdown", "\u00C9chelle de la placette ventil\u00E9 par esp\u00E8ce"), 
		/**
		 * Stand-level export with status breakdown.
		 */
		STAND_SPECIES_BIO("Plot-level biomass with species breakdown", "\u00C9chelle de la placette ventil\u00E9 par esp\u00E8ce avec biomasse"), 
		/**
		 * Stand-level export with status breakdown.
		 */
		STAND_STATUS("Plot level with status breakdown", "\u00C9chelle de la placette ventil\u00E9 par \u00E9tat"), 
		
		COMPOSITE_STAND("Stand level", "\u00C9chelle de la strate"), 
		COMPOSITE_STAND_WOODSTOCK("Stand level in column format", "\u00C9chelle de la strate (format en colonnes)"),
		TREE_QUALITY("Tree level with quality", "\u00C9chelle de l'arbre avec qualit\u00e9 "),
		TREELOG("Tree log level", "\u00C9chelle du billon"),
		STANDLOG ("Stand log level", "\u00C9chelle du billon au niveau de la placette"),		
		STAND_DHP_CATEGORY("Stand dhp category", "\u00C9chelle de la placette par cat\u00e9gorie de diam\u00e8tre"),				
		SCENARIO("Scenario", "Sc\u00E9nario");
		
		ExportType(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}

	};

	protected abstract static class InternalSwingWorkerForRecordSet<P extends ExtExportTool> extends REpiceaExportTool.InternalSwingWorkerForRecordSet {
		
		protected final P caller;
		
		protected InternalSwingWorkerForRecordSet(P caller, Enum exportOption, REpiceaRecordSet recordSet) {
			super(exportOption, recordSet);
			this.caller = caller;
		}
		
		/**
		 * Export scenario
		 */
		protected void createScenario() throws Exception {
			GExportRecord r;
			List<ExtEvolutionParameters> oVec = ((ExtCompositeStand) caller.getStepVector().lastElement().getScene()).getEvolutionTracking();
			double progressFactor = 100d / oVec.size();
			int l = 0;
			
			if (!oVec.isEmpty()) {
				outerLoop:
				for (int i = 0; i < oVec.size(); i++) {
					
					if (isCancelled()) {
						break outerLoop;
					}
					MemoryWatchDog.checkAvailableMemory();

					ExtEvolutionParameters evolParams = (ExtEvolutionParameters) oVec.get(i);
					r = evolParams.getRecord();	
					addRecord(r);
					setProgress((int) ((++l) * progressFactor));
				}
			} else {
				throw new Exception();
			}
		}
		
		/**
		 * Create a stand record set with species breakdown. Only for living trees.
		 * @throws Exception
		 */
		protected void createStandRecordSetBySpecies() throws Exception {
			double progressFactor = 100d / caller.getStepVector().size();
			GExportRecord r;

			int i = 0;
		
			// 1 - first iterate on the steps
			outerLoop:
			for (Step step : caller.getStepVector()) {
				
				REpiceaRecordSet recordSetFromCurrentStep = new REpiceaRecordSet();

				ExtCompositeStand compositeStand = (ExtCompositeStand) step.getScene ();
				// Consider restriction to one particular group if needed

				// 2 - second iterate on the Monte Carlo iterations within the steps
				for (ExtPlotSample plotSample : compositeStand.getPlotSamples()) {
//					Map<String, ExtPlot> standMap = compositeStand.getRealization(iterMC).getPlotMap();

					// 3 - third iterate on the stand in the stand list
					for (ExtPlot plot : plotSample.getPlots()) {
						Map<String, Collection<AllometryCalculableTree>> oSpeciesColl = plot.getCollectionsBySpecies();
						AllometryCalculator allometryCalculator = plot.getAllometryCalculator();
						double areaFactor = 10000d / plot.getArea();
						// 4 - fourth iterate on the species within the steps
						for (String speciesGroupName : oSpeciesColl.keySet()) {
							
							if (isCancelled()) {
								break outerLoop;
							}
							MemoryWatchDog.checkAvailableMemory();

							Collection<AllometryCalculableTree> oColl = oSpeciesColl.get(speciesGroupName);

								r = new GExportRecord();
								if (caller.stratumID != null) {
									r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StratumID), caller.stratumID));
								}
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Year), compositeStand.getDate ()));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.MonteCarloIteration), plot.getMonteCarloRealizationId()));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotID), plot.getId()));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotWeight), plot.getWeight()));
								if (plot.isInterventionResult()) {
									r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), (Integer) 1));
								} else {
									r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), (Integer) 0));
								}
								
								double stemDensityHA = allometryCalculator.getNumberOfTrees(oColl) * areaFactor;
								double basalAreaHA = allometryCalculator.getBasalAreaM2(oColl) * areaFactor;
								double meanQuadDiam = allometryCalculator.getMeanQuadraticDiameterCm(oColl);
								double merchantableVolumeHA = allometryCalculator.getCommercialVolumeM3(oColl) * areaFactor; 
								
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.SpeciesGroup), speciesGroupName));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandDensity), stemDensityHA));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandBasalArea), basalAreaHA));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandMQD), meanQuadDiam));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandVolume), merchantableVolumeHA));
								recordSetFromCurrentStep.add(r);
						}
					}
				}
				addRecordSet(recordSetFromCurrentStep);
				setProgress((int) ((++i) * progressFactor));
			}
		}

		/**
		 * Create a stand record set with species breakdown. Only for living trees.
		 * @throws Exception
		 */
		protected final void createStandRecordSetByStatus() throws Exception {
			double progressFactor = 100d / caller.getStepVector().size();
			GExportRecord r;

			int i = 0;
		
			// 1 - first iterate on the steps
			outerLoop:
			for (Step step : caller.getStepVector()) {
				
				REpiceaRecordSet recordSetFromCurrentStep = new REpiceaRecordSet();

				ExtCompositeStand compositeStand = (ExtCompositeStand) step.getScene ();
				// Consider restriction to one particular group if needed

				// 2 - second iterate on the Monte Carlo iterations within the steps
				for (ExtPlotSample plotSample : compositeStand.getPlotSamples()) {

					// 3 - third iterate on the stand in the stand list
					for (ExtPlot plot : plotSample.getPlots()) {
						AllometryCalculator allometryCalculator = plot.getAllometryCalculator();
						double areaFactor = 10000d / plot.getArea();
						// 4 - fourth iterate on the species within the steps
						for (StatusClass statusClass : StatusClass.values()) {
							
							if (isCancelled()) {
								break outerLoop;
							}
							MemoryWatchDog.checkAvailableMemory();

							Collection<AllometryCalculableTree> oColl = (Collection) plot.getTrees(statusClass);

								r = new GExportRecord();
								if (caller.stratumID != null) {
									r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StratumID), caller.stratumID));
								}
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Year), compositeStand.getDate ()));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.MonteCarloIteration), plot.getMonteCarloRealizationId()));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotID), plot.getId()));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotWeight), plot.getWeight()));
								if (plot.isInterventionResult()) {
									r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), (Integer) 1));
								} else {
									r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), (Integer) 0));
								}
								
								double stemDensityHA = allometryCalculator.getNumberOfTrees(oColl) * areaFactor;
								double basalAreaHA = allometryCalculator.getBasalAreaM2(oColl) * areaFactor;
								double meanQuadDiam = allometryCalculator.getMeanQuadraticDiameterCm(oColl);
								double merchantableVolumeHA = allometryCalculator.getCommercialVolumeM3(oColl) * areaFactor; 
								
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Status), statusClass));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandDensity), stemDensityHA));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandBasalArea), basalAreaHA));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandMQD), meanQuadDiam));
								r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandVolume), merchantableVolumeHA));
								recordSetFromCurrentStep.add(r);
						}
					}
				}
				addRecordSet(recordSetFromCurrentStep);
				setProgress((int) ((++i) * progressFactor));
			}
		}
		
		/**
		 * Export: CompositeStand Level -> RecordSet - Implementation here.
		 * (RecordSet -> File in superclass)
		 */
		protected void createCompositeStandRecordSet() throws Exception {
			double progressFactor = 100d / caller.getStepVector().size();

			GExportRecord r;

			int i = 0;
			
			// 1 - first iterate on the steps
			outerLoop:
			for (Step step : caller.getStepVector()) {

				REpiceaRecordSet recordSetFromCurrentStep = new REpiceaRecordSet();
				
				boolean interventionStep = step.getScene ().isInterventionResult ();
				ExtCompositeStand extCompositeStand = (ExtCompositeStand) step.getScene();
				Map<String, Collection<AllometryCalculableTree>> oSpeciesColl = extCompositeStand.getCollectionsBySpecies();
				
				double paiGrowth = caller.getMethodProvider().getPAIMeanEstimate(extCompositeStand, GrowthComponent.Growth, VariableForEstimation.G);
				double paiMortality = caller.getMethodProvider().getPAIMeanEstimate(extCompositeStand, GrowthComponent.Mortality, VariableForEstimation.G);
				double paiRecruitment = caller.getMethodProvider().getPAIMeanEstimate(extCompositeStand, GrowthComponent.Recruitment, VariableForEstimation.G);
				double paiHarvesting = caller.getMethodProvider().getPAIMeanEstimate(extCompositeStand, GrowthComponent.Harvesting, VariableForEstimation.G);
				Estimate<Matrix, SymmetricMatrix, ?> averageProductionEstimate = caller.getMethodProvider().getAverageProductionPerHa(extCompositeStand);
				double averageProductionVPerHa;
				if (averageProductionEstimate != null) {
					averageProductionVPerHa = averageProductionEstimate.getMean().getValueAt(0, 0);
				} else {
					averageProductionVPerHa = 0d;
				}
				// 2 - second iterate on the species within the steps
				for (String speciesGroupName : oSpeciesColl.keySet()) {
					
					if (isCancelled()) {
						break outerLoop;
					}
					MemoryWatchDog.checkAvailableMemory();

					Collection<AllometryCalculableTree> oColl = oSpeciesColl.get(speciesGroupName);

					if (!oColl.isEmpty() || speciesGroupName.equals(ExtSimulationSettings.ALL_SPECIES)) {			// even if the collection is empty, it must be process if the speciesID is ALL_SPECIES. Otherwise empty stands disappear 
						r = new GExportRecord();
						if (caller.stratumID != null) {
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StratumID), caller.stratumID));
						}
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Year), extCompositeStand.getDateYr()));
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.NumberOfPlots), extCompositeStand.getExtPlotSample().size()));
						if (interventionStep) {
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), (Integer) 1));
						} else {
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), (Integer) 0));
						}
						double basalAreaHa = caller.getMethodProvider().getGPerHaEstimate(extCompositeStand, oColl).getMean().getValueAt(0, 0);
						double stemDensityHa = caller.getMethodProvider().getNPerHaEstimate(extCompositeStand, oColl).getMean().getValueAt(0, 0);
						double meanQuadDiam = Math.sqrt((basalAreaHa * 40000) / (stemDensityHa * 3.14159));
						double standVolumeHa = caller.getMethodProvider().getVolumePerHaEstimate(extCompositeStand, oColl).getMean().getValueAt(0, 0);
						double dominantHeightM = caller.getMethodProvider().getHdomEstimate(extCompositeStand, oColl).getMean().getValueAt(0, 0);
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.SpeciesGroup), speciesGroupName));
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandDensity), stemDensityHa));
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandBasalArea), basalAreaHa));
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandMQD), meanQuadDiam));
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StandVolume), standVolumeHa));
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.DominantHeight), dominantHeightM)); 	// no need for the coefHA multiplicator because HDOM is already expressed at the HA scale
						if (speciesGroupName == ExtSimulationSettings.ALL_SPECIES) {
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIGrowth), paiGrowth, 5)); 
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIMortality), paiMortality, 5)); 
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIRecruitment), paiRecruitment, 5));
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIHarvest), paiHarvesting, 5));
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.AverageProduction), averageProductionVPerHa));
						} else {
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIGrowth), 0d, 5)); 
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIMortality), 0d, 5)); 
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIRecruitment), 0d, 5));
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PAIHarvest), 0d, 5));
							r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.AverageProduction), 0d));
						}
						recordSetFromCurrentStep.add(r);
					}
				}
				addRecordSet(recordSetFromCurrentStep);
				setProgress((int) ((++i) * progressFactor));
			}
		}
		
		
		/**
		 * Export: Tree Level -> RecordSet - Implementation here.
		 * (RecordSet -> File in superclass)
		 */
		protected void createTreeRecordSet() throws Exception {
			double progressFactor = 100d / caller.getStepVector().size();
			GExportRecord r;
			ExtTree oTree;
			
			
			int i = 0;
			outerLoop:
			for (Step step : caller.getStepVector()) {
				
				REpiceaRecordSet recordSetFromCurrentStep = new REpiceaRecordSet();

				// Consider restriction to one particular group if needed
				ExtCompositeStand s = (ExtCompositeStand) step.getScene ();
				Collection trees = new ArrayList();	// fc - 24.3.2004
				
				try {
					for (StatusClass status : StatusClass.values()) {
						trees.addAll(((ExtCompositeStand) s).getTrees(status));
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
				
				ExtPlot plot;
				// Stand variables ------------------------------------------------------------------
				for (Object t : trees) {
					if (isCancelled()) {
						break outerLoop;
					}
					MemoryWatchDog.checkAvailableMemory();
					
					oTree = (ExtTree) t;
					plot = (ExtPlot) oTree.getScene();
					r = new GExportRecord();
					if (caller.stratumID != null) {
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.StratumID), caller.stratumID));
					}
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Year),s.getDateYr()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.MonteCarloIteration), plot.getMonteCarloRealizationId()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotID), plot.getId()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotAreaHa), plot.getAreaHa()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.PlotWeight), plot.getWeight()));
					if (plot.isInterventionResult()) {
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual),(Integer) 1));
					} else {
						r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual),(Integer) 0));
					}
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.TreeID), oTree.getId()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Species), oTree.getInitialSpeciesName()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.SpeciesGroup), oTree.getSpecies().getName()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Status), oTree.getStatusClass().toString()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Number), oTree.getNumber()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.TreeDBH), oTree.getDbh()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.TreeHeight), oTree.getHeight()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.TreeBasalArea), oTree.getStemBasalAreaM2()));
					r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.TreeVolume), oTree.getCommercialVolumeM3() * 1000));
					recordSetFromCurrentStep.add(r);
				}
				addRecordSet(recordSetFromCurrentStep);
				setProgress((int) ((++i) * progressFactor));
			}
			
		}

	}

	
	private Vector<Step> steps;
	private ExtModel model;
	protected String stratumID;


	/**
	 * General constructor with multiple selection enabled 
	 * @throws Exception
	 */
	protected ExtExportTool() {
		super(true);
	}


	@Override
	public REpiceaExportToolDialog getUI(Container parent) {
		if (guiInterface == null) {
			guiInterface = new ExtExportToolDialog(this, (Window) parent);
		}
		return guiInterface;
	}

	
	/**
	 * This method is a short initializer for the export tool. It implies there is no additional parameters inherited
	 * from a parent.
	 * @param model a QuebecMRNFModel instance
	 * @param step a Step object
	 * @throws Exception
	 */
	public void init(ExtModel model, Step step) {
		init(model, step, null);
	}
	
	/**
	 * This method initializes the export tool.
	 * @param model a QuebecMRNFModel instance
	 * @param step a Step object
	 * @param parentExportTool a ExtExportTool object from which additional parameters can be inherited
	 */
	public void init(ExtModel model, Step step, ExtExportTool parentExportTool) {
		this.model = model;
		if (step != null) {
			steps = step.getProject().getStepsFromRoot(step);
			ExtCompositeStand stand = (ExtCompositeStand) step.getScene();
			stratumID = stand.getStratumName();
		}
	}

	protected boolean isMultipleSelectionEnabled() {return multipleSelection;}
	protected ExtModel getModel() {return model;}
	protected ExtMethodProvider getMethodProvider() {return (ExtMethodProvider) getModel().getMethodProvider();} 
	protected Vector<Step> getStepVector() {return steps;}
	
	
	protected Map<Enum, REpiceaRecordSet> justGetRecordSetsForSelectedExportOptions() {
		Map<Enum, REpiceaRecordSet> outputMap = new HashMap<Enum, REpiceaRecordSet>();
		for (Enum option : getSelectedExportFormats()) {
			REpiceaRecordSet recordSet = new REpiceaRecordSet();
			REpiceaExportTool.InternalSwingWorkerForRecordSet worker = this.instantiateInternalSwingWorkerForRecordSet(option, recordSet);
			worker.run();
			outputMap.put(option, recordSet);
		}
		return outputMap;
	}
	
	/*
	 * Just to make it public (non-Javadoc)
	 * @see quebecmrnfutility.ioformat.exportdbf.ExportDBFTool#setRecordSet(java.lang.Enum)
	 */
	public List<Enum> getSelectedExportFormats() {return super.getSelectedExportOptions();}
	
	@Override
	protected Vector<Enum> defineAvailableExportOptions() {
		Vector<Enum> exportOptions = new Vector<Enum>();
		for (ExportType exportType : ExportType.values()) {
			exportOptions.add(exportType);
		}
		exportOptions.remove(ExportType.STAND_STATUS); // removed by default for all models
		return exportOptions;
	}

	@Override
	protected List<Enum> getAvailableExportOptions() {return super.getAvailableExportOptions();}

	@Override
	protected String getFilename() {return super.getFilename();}
	
	
}

