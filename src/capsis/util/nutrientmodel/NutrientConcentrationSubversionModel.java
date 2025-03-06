/*
 * This file is part of the lerfob-forestools library.
 *
 * Copyright (C) 2010-2013 Mathieu Fortin for LERFOB INRA/AgroParisTech, 
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed with the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * Please see the license at http://www.gnu.org/copyleft/lesser.html.
 */
package capsis.util.nutrientmodel;

import repicea.math.Matrix;
import repicea.simulation.REpiceaPredictor;
import capsis.util.biomassmodel.BiomassPredictionModel.BiomassCompartment;

/**
 * This abstract class defines a sub model of the NutrientConcentration simulator.
 * @author Mathieu Fortin - March 2013
 */
abstract class NutrientConcentrationSubversionModel extends REpiceaPredictor {

	private final NutrientConcentrationPredictionModel owner;
	
	protected NutrientConcentrationSubversionModel(NutrientConcentrationPredictionModel owner, boolean isParametersVariabilityEnabled, boolean isResidualVariabilityEnabled) {
		super(isParametersVariabilityEnabled, false, isResidualVariabilityEnabled);
		this.owner = owner;
		init();
	}

	/**
	 * This method initializes the parameters
	 */
	protected abstract void init();

	/**
	 * This method provides a 3x1 Matrix that contains the nutrient concentration for the wood, the bark and the wood plus the bark.
	 * @param compartment a BiomassCompartment instance
	 * @param tree a NutrientCompatibleTree instance
	 * @return a Matrix instance
	 */
	protected Matrix predictNutrientConcentration(BiomassCompartment compartment, NutrientCompatibleTree tree) {
		double midDiameterCm = owner.getMedianDiameterCm(tree, compartment);
		double barkRatio = 1. - owner.getWoodDryBiomassRatio(tree, compartment);
		return getConcentrations(midDiameterCm, barkRatio);
	}

	/**
	 * This method provides a 3x1 Matrix that contains the nutrient concentration for the wood, the bark and the wood plus the bark 
	 * between two diameters. IMPORTANT: this method assumes that the section is not a branch!
	 * @param compartment a BiomassCompartment instance
	 * @param tree a NutrientCompatibleTree instance
	 * @return a Matrix instance
	 */
	protected Matrix predictNutrientConcentrationBetweenTheseBounds(NutrientCompatibleTree tree, double largeEnd, double smallEnd) {
		double midDiameterCm = (largeEnd + smallEnd) * .5;
		double barkRatio = 1. - owner.getWoodDryBiomassRatio(tree, midDiameterCm, false);
		return getConcentrations(midDiameterCm, barkRatio);
	}

	protected abstract Matrix getConcentrations(double midDiameterCm, double barkRatio);

}
