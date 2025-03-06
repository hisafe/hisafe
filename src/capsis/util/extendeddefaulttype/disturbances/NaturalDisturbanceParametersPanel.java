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

import capsis.util.extendeddefaulttype.ExtModel;
import repicea.simulation.disturbances.DisturbanceTypeProvider.DisturbanceType;

/**
 * A panel for the parameters of a natural disturbance
 * @author Mathieu Fortin - March 2019
 */
public class NaturalDisturbanceParametersPanel extends DisturbanceParametersPanel {

	public NaturalDisturbanceParametersPanel(DisturbanceType type, ExtModel model) {
		super(type, model);
	}

	public NaturalDisturbanceParametersPanel(DisturbanceType type, ExtModel model, boolean showModelOption) {
		super(type, model, showModelOption);
	}

	
}
