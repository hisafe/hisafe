/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  Francois de Coligny, Samuel Dufour
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
package capsis.commongui.projectmanager;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Point;
import java.awt.Stroke;


/**	ProjectLine is a line composed of three segments : two Points + two intermediates Points.
*	A ProjectLine is drawn between tho StepButtons in a ScenarioPanel.
*	@author F. de Coligny - august 2009
*/
public class ProjectLine {
	private Point p1;
	private Point p2;
	private Color color;
	private Stroke stroke;

	
	/**	Constructor 1.
	*/
	public ProjectLine () {
		this (new Point (0, 0), new Point (0, 0));
	}
	
	/**	Constructor 2.
	*/
	public ProjectLine (Point p1, Point p2) {
		this.p1 = p1;
		this.p2 = p2;
		color = Color.BLACK;			// use setColor () to change this default
		stroke = new BasicStroke (1f);	// use setStoke () to change this default
	}
	
	/**	Returns the 1st inflection point between P1 and P2.
	*/
	public Point getFirstInflectionPoint () {
		Point p = new Point ();
		p.x = p1.x + (p2.x - p1.x) / 2;
		p.y = p1.y;
		return p;
	}
	
	/**	Returns the 2nd inflection point between P1 and P2.
	*/
	public Point getSecondInflectionPoint () {
		Point p = new Point ();
		p.x = p1.x + (p2.x - p1.x) / 2;
		p.y = p2.y;
		return p;
	}
	
	public Point getP1 () {return p1;}
	
	public Point getP2 () {return p2;}

	public Color getColor () {return color;}

	public Stroke getStroke () {return stroke;}
	
	public void setP1 (Point p) {p1 = p;}
	
	public void setP2 (Point p) {p2 = p;}
	
	public void setColor (Color c) {color = c;}

	public void setStroke (Stroke v) {stroke = v;}


}



