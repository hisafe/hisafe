/*
 * This file is part of the lerfob-forestools library.
 *
 * Copyright (C) 2010-2012 Mathieu Fortin for LERFOB INRA/AgroParisTech, 
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
package capsis.util.biomassmodel;

import capsis.util.biomassmodel.BiomassPredictionModel.BiomassCompartment;

/**
 * This interface ensures the wood piece can provide its compartment.
 * @author Mathieu Fortin - March 2013
 */
public interface BiomassCompartmentProvider {

	/**
	 * This method returns the biomass compartment of this instance. Is to be used with wood pieces for example.
	 * @return a BiomassCompartment enum variable
	 */
	public BiomassCompartment getBiomassCompartmentOfThisObject();
}
