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
import repicea.simulation.ModelParameterEstimates;

/**
 * The NutrientSubversionN is the concentration model for nitrogen. 
 * 
 * This class is modified on september 2015 : previous parameters were adjusted without site effect. 
 * Replacement parameters are those published in Forest ecology and Management (Wernsd�rfer et al. 2014).
 * These parameters are adjusted with an additive site effect that is to consider in the predictions 
 * 
 * @author Mathieu Fortin - March 2013
 * @author Nicolas Bilot - September 2015 (refaction setp 2015)
 */
class NutrientSubversionN extends NutrientConcentrationSubversionModel {

	protected NutrientSubversionN(NutrientConcentrationPredictionModel owner, boolean isParametersVariabilityEnabled, boolean isResidualVariabilityEnabled) {
		super(owner, isParametersVariabilityEnabled, isResidualVariabilityEnabled);
		init();
	}

	@Override
	protected void init() {
		Matrix betaReference = new Matrix(6,1);
//		Parameters without site effect
//		betaReference.m_afData[0][0] = 0.908271;
//		betaReference.m_afData[1][0] = 1.590231;
//		betaReference.m_afData[2][0] = -0.20064;
//		betaReference.m_afData[3][0] = 9.651501;
//		betaReference.m_afData[4][0] = 0.021955;
//		betaReference.m_afData[5][0] = -0.40811;

//		Parameters with site effect
		betaReference.setValueAt(0, 0, 0.748); // a_wood
		betaReference.setValueAt(1, 0, 2.275); // b_wood
		betaReference.setValueAt(2, 0, -0.240); // c_wood
		betaReference.setValueAt(3, 0, 10.459); // b_bark
		betaReference.setValueAt(4, 0, 0.023); // c_bark
		betaReference.setValueAt(5, 0, -0.456); // d_bark
		
		setParameterEstimates(new ModelParameterEstimates(betaReference, null));
		
		// TODO implement the residual errors
				
	}

	@Override
	protected Matrix getConcentrations(double midDiameterCm, double barkRatio) {
		Matrix y = new Matrix(3,1);

		Matrix beta = getParameterEstimates().getMean();

//		Wood concentration
		y.setValueAt(0, 0, beta.getValueAt(0, 0) + beta.getValueAt(1, 0) * Math.exp(beta.getValueAt(2, 0) * midDiameterCm));
//		Bark concentration
		y.setValueAt(1, 0, beta.getValueAt(3, 0) * Math.exp(beta.getValueAt(4, 0) * midDiameterCm) + beta.getValueAt(5, 0) * midDiameterCm);
//		Compartment concentration
		y.setValueAt(2, 0, (1 - barkRatio) * y.getValueAt(0, 0) + barkRatio * y.getValueAt(1, 0));

		if (isResidualVariabilityEnabled) {
			Matrix errors = getResidualError();
			y = y.add(errors);
		}
		
		return y;
	}

}
