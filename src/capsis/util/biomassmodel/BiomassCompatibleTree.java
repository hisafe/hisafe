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

import lerfob.fagacees.FagaceesSpeciesProvider;
import repicea.simulation.covariateproviders.treelevel.AgeYrProvider;
import repicea.simulation.covariateproviders.treelevel.DbhCmProvider;
import repicea.simulation.covariateproviders.treelevel.HeightMProvider;

/**
 * This interface ensures the tree is compatible with the biomass model.
 * @author Mathieu Fortin - December 2012
 */
public interface BiomassCompatibleTree extends HeightMProvider, 
												DbhCmProvider, 
												AgeYrProvider, 
												FagaceesSpeciesProvider	{

}
