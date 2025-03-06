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
package capsis.util.group;

import java.util.ArrayList;
import java.util.Collection;

/**
 * A collection which is returned by a Grouper apply method. It is possible to
 * check if a collection c of individuals is a group by testing if c instanceof
 * GroupCollection.
 * 
 * @author F. de Coligny - September 2005
 */
public class GroupCollection extends ArrayList {

	public GroupCollection() {
		super();
	}

	public GroupCollection(Collection c) {
		super(c);
	}

}
