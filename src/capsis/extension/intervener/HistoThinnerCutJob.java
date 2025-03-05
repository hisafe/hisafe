/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

package capsis.extension.intervener;

import java.util.Collection;
import java.util.Iterator;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;

/**
 * Some of the trees must be cut to reach the target tree number.
 * The trees must be in their stand. They may be GMaddTrees (individual trees) 
 * or GMaidTrees (with a number of represented trees). GMaidTrees implement 
 * the Numberable interface.
 * 
 * @author F. de Coligny - april 2003
 */
public class HistoThinnerCutJob {
	
	// Number of trees (GMaddTree number or sum of the GMaidTree number propperties)
	private int totalNumber; 
	
	// The trees below must be "cut" to reach this number
	private int targetNumber; 
	
	// The concerned trees to be "cut"
	// If GMaddtrees -> remove some of the stand
	// If GMaidTrees -> reduce their number properties for their sum to reach the targetNumber
	private Collection trees;
	
	public HistoThinnerCutJob (int totalNumber, int targetNumber, Collection trees) {
		this.totalNumber = totalNumber;
		this.targetNumber = targetNumber;
		this.trees = trees;
	}
	
	public int getTotalNumber () {return totalNumber;}
	
	public int getTargetNumber () {return targetNumber;}
	
	public void setTrees (Collection trees) {this.trees = trees;}	// fc - 7.9.2005
	public Collection getTrees () {return trees;}
	
	
	/**
	 * Returns the sum of trees to be cut in the given cut jobs.
	 */
	static public int sumOfTreesToBeCut (HistoThinnerCutJob[] cutJobs) {
		if (cutJobs == null) return 0;
		int sum = 0;
		for (int i = 0; i < cutJobs.length; i++) {
			if (cutJobs[i] == null) continue;
			sum += cutJobs[i].totalNumber - cutJobs[i].targetNumber;
		}
		return sum;
	}
	
	public String toString () {
		String tString = "";
		for (Iterator i = trees.iterator (); i.hasNext ();) {
			Tree t = (Tree) i.next ();
			tString += "t_"+t.getId ();
			if (t instanceof Numberable) {tString += "("+((Numberable) t).getNumber ()+")";}
			tString += " ";
		}
		return "totalNumber="+totalNumber+" targetNumber="+targetNumber+" trees="+tString;
	}
	
}
