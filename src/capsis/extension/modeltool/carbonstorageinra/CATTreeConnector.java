/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2021 Mathieu Fortin (Canadian Wood Fibre Centre)
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.extension.modeltool.carbonstorageinra;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import lerfob.carbonbalancetool.CATCompatibleTree;

/**
* The CATStandConnector interface adds default methods adapted to CAPSIS for the connection with
 * the Carbon Accounting Tool (CAT). 
 * @author Mathieu Fortin - June 2021
 *
 */
public interface CATTreeConnector extends CATCompatibleTree {

	@Override
	public default StatusClass getStatusClass() {
		Tree t = (Tree) this;
		return StatusClass.valueOf(t.getStatusInScene());
	}

	
	@Override
	public default void setStatusClass(StatusClass statusClass) {
		Tree myTree = (Tree) (this);
		String currentStatus = myTree.getStatusInScene();
		String desiredStatus = statusClass.name();
		if (currentStatus.equals(desiredStatus)) {
			return;		// nothing to do here
		} else {
			TreeList s = (TreeList) myTree.getScene();
			if (currentStatus.equals("alive")) {	// removing the tree from its current collection status
				s.removeTree(myTree);  // alive is a bit special because the idmap must be updated as well
			} else {
				s.getTrees(currentStatus).remove(myTree);
			}
			if (myTree instanceof Numberable) {		// storing the tree under the other status
				s.storeStatus((Numberable) myTree, "cut", ((Numberable) myTree).getNumber ()); // "cut all this tree"
			} else {
				s.storeStatus(myTree, desiredStatus);
			}
		}
	}

}
