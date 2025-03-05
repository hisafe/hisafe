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

import jeeb.lib.util.DeepCopy;



public class OrderedPlan extends AutomationPlan {

	@Override
	public List<Automation> getVariations(Automation a, AutomationVariation v) {

		List<Automation> ret = new ArrayList<Automation>();
		Automation a0 = (Automation) DeepCopy.copy(a);
		updateName(a0, 0);
		ret.add(a0);

		int index = 0;
		boolean next = true;

		while(next) {

			Automation newobj = null; 
			Automation.Event evt = null;
			next = false;

			// for each event id
			for(Integer evId : v.data.keySet()) {

				// for each key
				Map<String, List<Object>> keyMap = v.data.get(evId);
				for(String key : keyMap.keySet()) {

					// test if there are other parameters
					List<Object> l = keyMap.get(key);

					if(index < l.size()) {

						// create object if necessary
						if(newobj == null) {
							newobj = (Automation) DeepCopy.copy(a);
						}
						evt = v.getEvent(newobj, evId);
						// set value
						Object value = l.get(index);
						evt.parameters.put(key, value);
					}

					// test if a next iteration is necessary
					if(index < (l.size() - 1)) { next = true; }
				}
			}

			if(newobj != null) {
				// add suffix in name
				updateName(newobj, ret.size());
				ret.add(newobj);
			}

			index ++;
		}

		return ret;

	}

}



