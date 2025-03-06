/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Authors: M. Fortin, Canadian Forest Service 
 * Copyright (C) 2019 Her Majesty the Queen in right of Canada 
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
package capsis.util.extendeddefaulttype.disturbances;

import java.io.Serializable;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import capsis.defaulttype.TreeList;
import capsis.util.extendeddefaulttype.ExtCompositeStand;
import repicea.io.GExportFieldDetails;
import repicea.simulation.MonteCarloSimulationCompliantObject;
import repicea.simulation.disturbances.DisturbanceAffectedProvider;
import repicea.simulation.disturbances.DisturbanceOccurrences;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

/**
 * The DisturbanceParameters class is the abstract class for natural and human-made disturbances.
 * @author Mathieu Fortin - March 2019
 */
@SuppressWarnings("serial")
public abstract class DisturbanceParameters implements DisturbanceAffectedProvider,
														Serializable, 
														Cloneable {
	
	protected static final Random RANDOM = new Random();
	
	public static enum DisturbanceFieldName implements TextableEnum {
		Disturbance("Dist","Pert"),
		Recurrence("Recurrence","Recurrence");
		
		DisturbanceFieldName(String englishText, String frenchText) {
			setText(englishText, frenchText);
		}
		
		
		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}
		
		@Override
		public String toString() {return REpiceaTranslator.getString(this);}
	}

	
	public static enum DisturbanceMode {
		None, 
		NextStep, 
		Random,
		FullModelBased,
		RuleBased;
	}

	private final DisturbanceType type;
	private DisturbanceMode mode;
	protected final double recurrenceYrs;

	/**
	 * Constructor.
	 * @param type a DisturbanceType enum
	 * @param mode a DisturbanceMode enum
	 * @param recurrenceYrs
	 */
	public DisturbanceParameters(DisturbanceType type, DisturbanceMode mode, double recurrenceYrs) {
		this.type = type;
		this.mode = mode;
		if (recurrenceYrs <= 0 && mode == DisturbanceMode.Random) {
			throw new InvalidParameterException("The mode cannot be random with a null or negative recurrence!");
		}
		this.recurrenceYrs = recurrenceYrs;
	}

	public List<GExportFieldDetails> getRecords() {
		List<GExportFieldDetails> fields = new ArrayList<GExportFieldDetails>();
		fields.add(new GExportFieldDetails(DisturbanceFieldName.Disturbance.toString().concat(type.toString()), mode.name()));
		fields.add(new GExportFieldDetails(DisturbanceFieldName.Recurrence.toString().concat(type.toString()), recurrenceYrs));
		return fields;
	}

	@Override
	public DisturbanceType getDisturbanceType() {return type;}
	
	public DisturbanceMode getMode() {return mode;}

	/**
	 * This method returns an object that defines whether or not there is a disturbance. In deterministic mode, that object is a double while it is a boolean 
	 * in most stochastic implementations.
	 * @param yrs the length of the growth interval
	 * @param parms eventual parameters
	 * @return a DisturbanceOccurrence instance if it occurs or null if it does not
	 */
	public DisturbanceOccurrences isThereADisturbance(ExtCompositeStand compositeStand, TreeList stand, int stepLengthYrs, Map<String, Object> parms) {
		if (mode == DisturbanceMode.NextStep) {
			DisturbanceOccurrences occurrences = new DisturbanceOccurrences(this, getInternalOccurrenceDateYr(compositeStand, stepLengthYrs));
			return occurrences;
		} else {
			return null;
		}
	}
	
	/**
	 * Provide a year for the occurrence of the disturbance within the growth step.
	 * @param compositeStand an ExtCompositeStand instance
	 * @param stepLengthYrs the length of the step (yrs)
	 * @return
	 */
	protected final int getInternalOccurrenceDateYr(ExtCompositeStand compositeStand, int stepLengthYrs) {
		int occurrenceDateYr;
		if (compositeStand.isStochastic()) {
			occurrenceDateYr = compositeStand.getDateYr() + (int) Math.ceil(RANDOM.nextDouble() * stepLengthYrs);
		} else {
			occurrenceDateYr = (int) Math.round(compositeStand.getDateYr() + 0.5 * stepLengthYrs);				
		}
		return occurrenceDateYr;
	}
	
	
	/**
	 * Create a new DisturbanceParameter instance. If the mode was set to NextStep, it
	 * is automatically switch to None.
	 * @return a DisturbanceParameters instance
	 */
	public DisturbanceParameters mute() {
		DisturbanceParameters mutant = clone();
		if (mutant.getMode() == DisturbanceMode.NextStep) {
			mutant.mode = DisturbanceMode.None;
		}
		return mutant;
	}
	
	@Override
	protected DisturbanceParameters clone() {
		try {
			return (DisturbanceParameters) super.clone();
		} catch (CloneNotSupportedException e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public boolean isThisPlotAffected(MonteCarloSimulationCompliantObject plot) {
		return true;
	}
	
}
