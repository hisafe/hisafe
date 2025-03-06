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
 * The NutrientSubversionS is the concentration model for sulfur.
 * 
 * This class is modified on september 2015 : previous parameters were adjusted without site effect. 
 * Replacement parameters are those published in Forest ecology and Management (Wernsd�rfer et al. 2014).
 * These parameters are adjusted with an additive site effect that is to consider in the predictions 
 * 
 * @author Mathieu Fortin - March 2013
 * @author Nicolas Bilot - September 2015 (refaction setp 2015)
 */
class NutrientSubversionS extends NutrientConcentrationSubversionModel {

	protected NutrientSubversionS(NutrientConcentrationPredictionModel owner, boolean isParametersVariabilityEnabled, boolean isResidualVariabilityEnabled) {
		super(owner, isParametersVariabilityEnabled, isResidualVariabilityEnabled);
		init();
	}

	@Override
	protected void init() {
		Matrix betaReference = new Matrix(6,1);
//		Parameters without site effect
//		betaReference.m_afData[0][0] = 0.114688;
//		betaReference.m_afData[1][0] = 0.147543;
//		betaReference.m_afData[2][0] = -0.31139;
//		betaReference.m_afData[3][0] = 0.549526;
//		betaReference.m_afData[4][0] = -0.00731;
		
//		Parameters with site effect
		betaReference.setValueAt(0, 0, 0.122); // a_wood
		betaReference.setValueAt(1, 0, 0.164); // b_wood
		betaReference.setValueAt(2, 0, -0.354); // c_wood
		betaReference.setValueAt(3, 0, 0.559); // b_bark
		betaReference.setValueAt(4, 0, -0.007); // c_bark
		
		
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
		y.setValueAt(1, 0, beta.getValueAt(3, 0) * Math.exp(beta.getValueAt(4, 0) * midDiameterCm));
//		Compartment concentration
		y.setValueAt(2, 0, (1 - barkRatio) * y.getValueAt(0, 0) + barkRatio * y.getValueAt(1, 0));

		if (isResidualVariabilityEnabled) {
			Matrix errors = getResidualError();
			y = y.add(errors);
		}
		
		return y;
	}
	
	
}
