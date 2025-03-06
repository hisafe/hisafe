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

import java.util.ArrayList;
import java.util.List;

/** */
public class CombineList extends ArrayList<ArrayList<Double>>{

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;

	public CombineList(ArrayList<ArrayList<Double>> l) {
		
		this.addAll(generate(l));
	}

	/** internal class function to generate all combinaison with lists of l */
	private ArrayList<ArrayList<Double>>  generate( ArrayList<ArrayList<Double>> l) {
		
		if(l.size() == 0 ) return null;
		
		List<Double> first = l.get(0);
		l.remove(0);
		
		ArrayList<ArrayList<Double>> tmp = generate(l);
		
		ArrayList<ArrayList<Double>> ret = new ArrayList<ArrayList<Double>>();
		
		for(Double d : first) {
			
			if(tmp == null) {
				ArrayList<Double> sublist = new ArrayList<Double>();
				sublist.add(d);
				ret.add(sublist);
			} else  {
		
				for(ArrayList<Double> l2 : tmp ) {

					ArrayList<Double> sublist = new ArrayList<Double>();
					sublist.add(d);

					for(Double d2 : l2) { sublist.add(d2); }
					ret.add(sublist);
				}
			}

		}
		
		return ret; 
		
	}
	

}
