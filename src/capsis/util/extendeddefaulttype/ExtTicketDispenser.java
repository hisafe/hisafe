/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) Her Majesty the Queen in right of Canada
 * Author: M. Fortin, Canadian Forest Service
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
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

import java.security.InvalidParameterException;

import jeeb.lib.util.TicketDispenser;

/**
 * A synchronized version of the original TicketDispenser class. The setCurrentValue method is no
 * longer allowed and it will throw an InvalidParameterException if called.
 * @author Mathieu Fortin - March 2021
 */
public class ExtTicketDispenser extends TicketDispenser {
	
	@Override
	public synchronized int getNext() {
		return super.getNext();
	}

	@Override
	public void setCurrentValue(int v) {
		throw new InvalidParameterException("This method is no longer available in the ExtTicketDispenser class!");
	}

	
	
}
