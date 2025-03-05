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
package capsis.extension.datarenderer.barchart;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;

/**	A Bar in a BarChart renderer.
*
*	@author F. de Coligny - december 2004
*/
public class Bar {
	
	public int line;
	
	public int col;
	
	public String label;
	
	public Collection<Double> values;	// Maybe several values if cumulated histogram
	
	public Collection<String> notes;	// If null or blank, note may be = to value
	
	public Color color;
	
	
	/**	Constructor.
	*/
	public Bar (int line, int col, String label, Color color) {
		this.line = line;
		this.col = col;
		this.label = label;
		this.values = new ArrayList<Double> ();
		this.notes = new ArrayList<String> ();
		this.color = color;
	}
	
	/**	Add a value and a note in the bar.
	*/
	public void add (double value, String note) {
		values.add (value);	// autoboxing double -> Double
		notes.add (note);
	}
	
	/**	Return total value.
	*/
	public double getValue () {
		double total = 0;
		for (double v: values) {
			if (Double.isNaN (v)) {continue;}
			total += v;
		}
		return total;
	}
	
	/**	Set line and column for this bar : it will be drawn in a 
	*	multi line / multi column pattern.
	*/
	public void setLineAndCol (int line, int col) {
		this.line = line;
		this.col = col;
	}
	
	// Test templates, foreach, autoboxing
	//
	public static void main (String[] a) {
		Collection<Double> c = new ArrayList<Double> ();
		for (double i = 0; i < 20; i+=0.5) {
			c.add (i);
		}
		
		for (double i: c) {
			System.out.print (i+" ");
		}
		System.out.println ();
		
	}
	
}
