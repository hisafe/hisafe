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

package capsis.util;

import java.util.Collection;
import capsis.util.GFish;
import capsis.util.group.GroupableElement;

/**
 * A marker interface for reaches (for grouping).
 *
 * @author F. de Coligny - september 2004
 */
public interface GReach extends GroupableElement {

	public int getId ();
//	public String getAdress ();
	public int getOrder();
	public short getLength ();
	public float getMeanWidth();
	public float getSlope ();
	//public byte getBank ();
	//public float[] getSubstratum();
	//public float getDrainageArea ();
	//public float[] getDrainageOccupancy ();
	public Collection getFishes();
	public Collection getHatches();
	public FishStand getStand();
	public Collection getDownstream (int unit) ;
	public Collection getUpstream (int unit) ;
	public Collection getBrothers () ;


}