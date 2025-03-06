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

import java.util.ArrayList;

/**
 * The ExtHistoricalRecordList class is just a list of historical record.
 * @author Mathieu Fortin - July 2019
 */
public class ExtHistoricalRecordList extends ArrayList<ExtHistoricalRecord> implements Cloneable {

	@Override
	public ExtHistoricalRecordList clone() {
		ExtHistoricalRecordList clone = new ExtHistoricalRecordList();
		for (ExtHistoricalRecord rec : this) {
			clone.add(rec.clone());
		}
		return clone;
	}
	
}
