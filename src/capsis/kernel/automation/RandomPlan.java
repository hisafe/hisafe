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
package capsis.kernel.automation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Random;

import jeeb.lib.util.DeepCopy;

/** choose randomly a  parameter in the variations 
 * 
 * @author sdufour
 *
 */
public class RandomPlan extends AutomationPlan {



	@Override
	public List<Automation> getVariations(Automation a, AutomationVariation v) {

		List<Automation> ret = new ArrayList<Automation>();
		
		Automation.Event evt = null;
		Random r = new Random();

		Automation newobj = (Automation) DeepCopy.copy(a);; 

		// for each event id
		for(Integer evId : v.data.keySet()) {

			evt = v.getEvent(newobj, evId);

			// for each key
			Map<String, List<Object>> keyMap = v.data.get(evId);
			for(String key : keyMap.keySet()) {

				// get variations
				List<Object> l = keyMap.get(key);
				if(l.size() > 0) {

					// choose a value randomly if there are variations
					int index = r.nextInt(l.size());
					Object value = l.get(index);
					evt.parameters.put(key, value);
				}

			}
		}

		updateName(newobj, ret.size());
		ret.add(newobj);
		return ret;

	}
}