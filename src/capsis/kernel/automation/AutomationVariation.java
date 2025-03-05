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

import java.io.Serializable;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jeeb.lib.util.ClassUtils;
import capsis.kernel.automation.Automation.Event;

/**
 * Automation variation is able to build a set of automation based 
 * on parameter lists
 * 
 * @author sdufour
 *
 */
public class AutomationVariation implements Serializable {

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	protected Map<Integer, Map<String, List<Object>>> data;
	public String plan = OrderedPlan.class.getName();
	
	
	// Map between an event id and its index in the event list
	transient Map<Integer, Integer> evtIndexMap;
		
	
	/**
	 * Constructor
	 */
	public AutomationVariation() {
		
		data = new HashMap<Integer, Map<String, List<Object>>>();
	}
	
	public Set<String> getParamNames(Integer id) {
		
		if(data.containsKey(id)) {
			return data.get(id).keySet();
		}
		
		return new HashSet<String>();
 		
	}
	
	
	/**
	 * Add a parameter variation 
	 * @param eventId
	 * @param key
	 * @param values
	 */
	public void set(Integer eventId, String key, List<Object> values){
		
		
		// Build data structure if necessary
		if(values != null && !data.containsKey(eventId)) {
			data.put(eventId, new HashMap<String, List<Object>>());
		}
		
		Map<String, List<Object>> keyMap = data.get(eventId);
		if(keyMap == null) { return; }
		
		// replace list or destroy previous one
		if(values != null) {
			keyMap.put(key, values);
			
		} else {
			keyMap.remove(key);
		}
		
	}
	
	/** Get values for an event and a key */
	public List<Object> get(Integer id, String key) {
		
		if(data.containsKey(id) && data.get(id).containsKey(key)) {
			return data.get(id).get(key);
		} else  {
			return null;
		}
		
		
	}
	
	/**
	 * Get all variation. First element is parameter
	 * @param a
	 */
	public List<Automation> getAutomations(Automation a) {
		
		AutomationPlan p;
		
		try {
			Class<?> c = getClass().getClassLoader().loadClass(plan);
			p = (AutomationPlan) ClassUtils.instantiateClass(c);
			
		} catch (Exception e) {
			p = new OrderedPlan();
		}
		
		System.out.println("Use " + p);
		return p.getVariations(a, this);
		
	}
	
	
	/** Return the correct event object for an id */
	protected Automation.Event getEvent(Automation a, Integer id) {
		
		if(evtIndexMap == null) {
			// Create a map between event index and event id
			evtIndexMap = new HashMap<Integer, Integer>();
			int index = 0;
			for(Automation.Event ev : a.events) {
				evtIndexMap.put(ev.id, index++);
			}
		}
		
		// get event
		Event ev;
		if(id < 0) ev = a.initEvent; // initial parameters
		else ev = a.events.get( evtIndexMap.get(id) );
		
		return ev;
	}
	
	
	
	

	
	

	
	
}
