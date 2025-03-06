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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.kernel.EvolutionParameters;
import capsis.kernel.GScene;
import capsis.util.extendeddefaulttype.disturbances.DisturbanceParameters;
import capsis.util.extendeddefaulttype.disturbances.NaturalDisturbanceParameters;
import capsis.util.extendeddefaulttype.disturbances.ThinningDisturbanceParameters;
import repicea.io.GExportFieldDetails;
import repicea.io.GExportRecord;
import repicea.io.tools.REpiceaExportTool.FieldName;
import repicea.simulation.disturbances.DisturbanceOccurrences;
import repicea.simulation.disturbances.DisturbanceParameter;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;
import repicea.util.REpiceaTranslator;


public class ExtEvolutionParameters implements EvolutionParameters, Cloneable, Serializable {
	
	/*
	 * Members of this class
	 */
	protected ExtEvolutionBreakParameters breakParameters;
	protected Map<DisturbanceType, DisturbanceParameters> disturbanceMap;

	@Deprecated
	protected int m_iYear;
	@Deprecated
	protected int m_iNbSteps;
	protected boolean isInterventionResult;
	protected Object treatment;


	/**
	 * Default constructor for normal evolution step.
	 * @param dateYr the date expressed in years
	 * @param numberSteps the number of growth steps for this evolution
	 */
	@Deprecated
	public ExtEvolutionParameters(int dateYr, int numberSteps) {
		m_iYear = dateYr;
		m_iNbSteps = numberSteps;
		isInterventionResult = false;
		treatment = null;
	}

	/**
	 * Constructor for post thinning.
	 * @param dateYr the date expressed in years
	 * @param treatment an Object that represents the treatment
	 */
	@Deprecated
	public ExtEvolutionParameters(int dateYr, Object treatment) {
		this(dateYr, 0);
		isInterventionResult = true;
		this.treatment = treatment;
	}

	/**
	 * Preferred constructor.
	 * @param breakParameters a BreakParameters instance which defines when the simulation stops.
	 * @param disturbances an array of disturbances that may affect the simulation
	 */
	public ExtEvolutionParameters(ExtEvolutionBreakParameters breakParameters, DisturbanceParameters... disturbances) {
		this.breakParameters = breakParameters;
		disturbanceMap = new HashMap<DisturbanceType, DisturbanceParameters>();
		if (disturbances != null) {
			for (DisturbanceParameters disturbance : disturbances) {
				if (disturbance != null) {
					disturbanceMap.put(disturbance.getDisturbanceType(), disturbance);
				}
			}
		}
		isInterventionResult = false;
		treatment = null;
	}
	
	/**
	 * For cloning.
	 */
	protected ExtEvolutionParameters() {}

	/*
	 * List of getters
	 */
	public int getInitialDate() {
		if (breakParameters != null) {
			return breakParameters.getInitialDate();
		} else {
			return m_iYear;
		}
	}

	public boolean shouldKeepRunning(GScene scene) {
		return breakParameters.shouldTheSimulationKeepRunning(scene);
	}
	
	public int getNbSteps() {
		if (breakParameters != null) {
			if (breakParameters.getStepDurations() != null) {
				return breakParameters.getStepDurations().length;
			} else {
				return 1;
			}
		} else {
			return m_iNbSteps;
		}
	}

	public boolean isGoingToBeInterventionResult() {return isInterventionResult;}
	public Object getTreatment() {return treatment;}

	/**
	 * Method called during the Model.getLightClone() method
	 */
	@Override
	public ExtEvolutionParameters clone() {			
		try {
			return (ExtEvolutionParameters) super.clone();
		} catch (CloneNotSupportedException e) {
			return null;
		}
	}
	
	
	public int getStepDuration(int currentDate, ExtModel model) {
		return breakParameters.getStepDuration(currentDate);
	}
	
	/**
	 * This method returns a copy of the parameters but for a single step.
	 * @return an ExtendedEvolutionParameters instance
	 */
	public ExtEvolutionParameters deriveCopyForASingleStep() {
		ExtEvolutionParameters output = clone();
		output.m_iNbSteps = 1;
		return output;
	}

	/**
	 * This method returns a GExportRecord that is available for export.
	 * @return a GExportRecord
	 */
	public GExportRecord getRecord() {
		int bool;
//		DisturbanceParameters param;
//		for (DisturbanceType type : DisturbanceType.values()) {
//			param = parameterMap.get(type);
//			if (param !=null) {
//				List<GExportFieldDetails> disturbanceParametersFields = param.getRecords();
//				for (GExportFieldDetails fld : disturbanceParametersFields) {
//					r.addField(fld);
//				}
//			}
//		}

		GExportRecord r = new GExportRecord();
		r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Year), getInitialDate()));
		r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.NumberOfDecades), getNbSteps()));
		if (isGoingToBeInterventionResult()) {
			bool = 1;
		} else {
			bool = 0;
		}
		r.addField(new GExportFieldDetails(REpiceaTranslator.getString(FieldName.Residual), bool));
		String treatmentType = "";
		if (getTreatment() != null) {
			treatmentType = getTreatment().toString();
		}
		GExportFieldDetails field = new GExportFieldDetails(REpiceaTranslator.getString(FieldName.TreatmentType), treatmentType);
		field.setLength(250);
		r.addField(field);
		return r;
	}
	
	
	/**
	 * This method returns the parameters for a given type. It returns null if there is no parameter of this type.
	 * @param type a DisturbanceType enum
	 * @return a DisturbanceParameters instance
	 */
	private DisturbanceParameters getDisturbanceParameters(DisturbanceType type) {
		if (disturbanceMap != null) {
			return disturbanceMap.get(type);
		} else {
			return null;
		}
	}

	
	/**
	 * This method returns the NaturalDisturbanceParameters instance from the disturbanceMap member.
	 * @return a NaturalDisturbanceParameters object or null if it does not exist
	 */
	public NaturalDisturbanceParameters getNaturalDisturbanceParameters(DisturbanceType type) {
		if (type == DisturbanceType.Harvest) {
			throw new InvalidParameterException("Harvest cannot be treated as a natural disturbance!");
		}
		return (NaturalDisturbanceParameters) getDisturbanceParameters(type);
	}

	/**
	 * This method returns the ThinningDisturbanceParameters instance from the disturbanceMap member.
	 * @return a ThinningDisturbanceParameters object or null if it does not exist
	 */
	public ThinningDisturbanceParameters getThinningDisturbanceParameters() {
		return (ThinningDisturbanceParameters) getDisturbanceParameters(DisturbanceType.Harvest);
	}
	
	/**
	 * This method returns information about the occurrence of a particular disturbance. If
	 * there is no disturbance parameters of this type in the disturbanceMap member, it considers
	 * there is no occurrence. 
	 * @param type a DisturbanceType enum
	 * @param compositeStand a ExtCompositeStand instance
	 * @param plotSample a TreeList instance
	 * @param stepLengthYrs the step length in years
	 * @param parms additional parameters if needed
	 * @return a DisturbanceOccurrence object if it occurs or null otherwise
	 */
	public void isThereANaturalDisturbanceOfThisType(DisturbanceType type, 
			ExtCompositeStand compositeStand, 
			ExtPlotSample plotSample, 
			int stepLengthYrs) {
		NaturalDisturbanceParameters distParms = getNaturalDisturbanceParameters(type);
		if (distParms != null) {
			Map<String, Object> innerParms = new HashMap<String, Object>();
			innerParms.put(DisturbanceParameter.ParmCurrentDateYr, compositeStand.getDateYr());
			DisturbanceOccurrences occurrences = distParms.isThereADisturbance(compositeStand, plotSample, stepLengthYrs, innerParms);
			if (occurrences != null) {
				plotSample.recordDisturbanceOccurrences(type, occurrences);
			}
		}
	}
	
	
	public Collection<DisturbanceParameters> getDisturbances() {
		if (disturbanceMap != null) {
			return disturbanceMap.values();
		} else {
			return new ArrayList<DisturbanceParameters>();
		}
	}
	
	public ExtEvolutionParameters mute() {
		List<DisturbanceParameters> disturbances = new ArrayList<DisturbanceParameters>();
		for (DisturbanceParameters disturbance : disturbanceMap.values()) {
			disturbances.add(disturbance.mute());
		}
		return new ExtEvolutionParameters(breakParameters, disturbances.toArray(new DisturbanceParameters[]{}));
	}
	
}
