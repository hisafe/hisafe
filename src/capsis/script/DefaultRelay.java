/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
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
package capsis.script;

import capsis.kernel.EvolutionParameters;
import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;
import capsis.kernel.Relay;
import capsis.kernel.Step;


/**
 * Default relay for script
 * @author sdufour
 *
 */
public class DefaultRelay extends Relay {

	public DefaultRelay(GModel m)  {
		super(m);	
	}

	@Override
	public EvolutionParameters getEvolutionParameters(Step stp) {
		return null;
	}

	@Override
	public InitialParameters getInitialParameters() {
		return null;
	}

}
