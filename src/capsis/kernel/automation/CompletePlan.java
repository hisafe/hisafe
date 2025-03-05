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

import jeeb.lib.util.DeepCopy;

public class CompletePlan extends AutomationPlan {

	@Override
	public List<Automation> getVariations(Automation a, AutomationVariation v) {
		List<Automation> ret = new ArrayList<Automation>();
		
		Automation a0 = (Automation) DeepCopy.copy(a);
		updateName(a0, 0);
		ret.add(a0);
		
		List<Integer> ids = new ArrayList<Integer>(v.data.keySet());
		
		if(ids.size() == 0) {
			ret.add((Automation) DeepCopy.copy(a));
			return ret;
		}
		
		List<String> params = new ArrayList<String>( v.data.get( ids.get(0) ).keySet() );
		multipleCombineVariations(0, 0, ids, params, (Automation) DeepCopy.copy(a), v, ret);
		
		return ret;
	}

	
	
	/**
	 * create a list of new automations by combining param variation
	 * @param idIndex : current id
	 * @param paramIndex : current param
	 * @param ids :
	 * @param params
	 * @param a : original automation ,
	 * @param output list of automation
	 */
	protected void multipleCombineVariations(int idIndex, int paramIndex, 
			List<Integer> ids, List<String> params, 
			Automation a, AutomationVariation v,  List<Automation> output) {
		
		// if we have used all variation, we keep the current state
		if(idIndex >= ids.size()) {
			Automation na = (Automation) DeepCopy.copy(a);
			updateName(na, output.size());
			output.add(na);
			return;
		}
		
		int nextParamIndex = paramIndex + 1;
		int nextIdIndex = idIndex;
		List<String> nextParams = params;
		
		if(nextParamIndex  >= params.size()) {
			nextParamIndex = 0;
			nextIdIndex += 1;
		}
			
		if(nextIdIndex < ids.size() ) {
			nextParams = new ArrayList<String> ( v.data.get( ids.get(nextIdIndex) ).keySet() );
		}
		
		Integer id = ids.get(idIndex);
		String param = params.get(paramIndex);
		
		Automation.Event evt = v.getEvent(a, id);
		// for each element in the current list
		for(Object o : v.data.get(id).get( param )) {
		
			evt.parameters.put(param, o);
			multipleCombineVariations(nextIdIndex, nextParamIndex, ids, nextParams, a, v, output);
		}
		
	}

}
