/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2019 CFS-CWFC 
 * 
 * Authors: M. Fortin, 
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
package capsis.util.extendeddefaulttype;

import java.util.HashMap;

/**
 * The ExtHistoricalRecord class is a Map that contains the information about different
 * event for a given interval length which is defined by a start and an end date.
 * @author Mathieu Fortin - July 2019
 *
 */
public class ExtHistoricalRecord extends HashMap<String, Double> implements Cloneable {

	private final int startDateYr;
	private final int endDateYr;
	
	public ExtHistoricalRecord(int startDateYr, int endDateYr, String key, Double value) {
		this(startDateYr,endDateYr);
		put(key, value);
	}
	
	private ExtHistoricalRecord(int startDateYr, int endDateYr) {
		super();
		this.startDateYr = startDateYr;
		this.endDateYr = endDateYr;
	}

	@Override
	public ExtHistoricalRecord clone() {
		ExtHistoricalRecord clone = new ExtHistoricalRecord(startDateYr, endDateYr);
		for (String key : this.keySet()) {
			clone.put(key, get(key));
		}
		return clone;
	}
	
}
