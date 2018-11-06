/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
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
package safe.extension.standviewer.safe;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.geom.Line2D;

import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTable;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.util.Drawer;
import capsis.util.Panel2D;
/**
 * Compass - A component to see North and bottom of slope directions.
 *
 * @author B. Courbaud - January 2003
 * Updated by I Lecomte - April 2003
 */
public class Compass extends JPanel implements Drawer {
// review - fc - 11.3.2004

	protected Panel2D panel2D;
	protected double angle;	// degrees
	private Font tableFont = new JTable ().getFont ();


	/**	Constructor.
	*/
	public Compass (double angle) {
		super (new BorderLayout ());
		this.angle = angle;

		Rectangle.Double r2 = new Rectangle.Double (-1, -1, 2, 2);

		panel2D = new Panel2D (this,
				r2,
				0, 	// x margin
				0);	// y margin
		panel2D.setPreferredSize (new Dimension (40, 40));
		panel2D.setZoomEnabled (false);
		panel2D.setSelectionEnabled (false);
		panel2D.setMoveEnabled (false);
		panel2D.setInfoIconEnabled (false);	// fc - 19.4.2007
		LinePanel l1 = new LinePanel ();

		//setLayout (new BoxLayout (this, BoxLayout.X_AXIS));

		l1.add (new JWidthLabel ("", 5));
		l1.add (panel2D);

		JLabel n = new JLabel (Translator.swap ("SafeCompass.North"));
		n.setFont (tableFont);
		n.setForeground (Color.RED);
	//	JLabel b = new JLabel (Translator.swap ("SafeCompass.TreeLine"));
	//	b.setFont (tableFont);
	//	b.setForeground (Color.BLACK);
		ColumnPanel l = new ColumnPanel (2, 0);
		l.add (n);
//		l.add (b);
		l.setBackground (Color.WHITE);

		l.addGlue ();

		// fc - 8.3.2004
		int width = l.getPreferredSize ().width;
		int height= 40;
		l.setMaximumSize (new Dimension (width, height));

		l1.add (l);
		l1.addGlue ();
		l1.setBackground (Color.WHITE);

		add (l1, BorderLayout.NORTH);

	}


	/**	From Drawer interface.
	*	This method draws in the Panel2D each time this one must be
	*	repainted.
	*/
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Tree Line
		double alpha = 3d*Math.PI/2d;
		double x = 0;
		double y = -1;

		double beta = Math.toRadians (30);
		double l = 0.3d;

		double x1 = x - l*Math.cos (alpha+beta);
		double y1 = y - l*Math.sin (alpha+beta);

		double x2 = x - l*Math.cos (beta-alpha);
		double y2 = y + l*Math.sin (beta-alpha);

		g2.setColor (Color.black);
		Shape bottom = new Line2D.Double (0, 0, x, y);
		Shape bBranch1 = new Line2D.Double (x, y, x1, y1);
		Shape bBranch2 = new Line2D.Double (x, y, x2, y2);
/*		g2.draw (bottom);
		g2.draw (bBranch1);
		g2.draw (bBranch2);*/

		// North arrow
		double red = 0.75d;
		//red = 1d;
		alpha = 3d*Math.PI/2d + Math.toRadians (angle);
		x = red*Math.cos (alpha);
		y = red*Math.sin (alpha);

		beta = Math.toRadians (30);
		l = 0.3d;

		x1 = x - l*Math.cos (alpha+beta);
		y1 = y - l*Math.sin (alpha+beta);

		x2 = x - l*Math.cos (beta-alpha);
		y2 = y + l*Math.sin (beta-alpha);

		g2.setColor (Color.red);
		Shape north = new Line2D.Double (0, 0, x, y);
		Shape nBranch1 = new Line2D.Double (x, y, x1, y1);
		Shape nBranch2 = new Line2D.Double (x, y, x2, y2);
		g2.draw (north);
		g2.draw (nBranch1);
		g2.draw (nBranch2);

	}


	/**	From Drawer interface.
	*	We may receive (from Panel2D) a selection rectangle (in user space i.e. meters)
	*	and return a JPanel containing information about the objects (trees) inside
	*	the rectangle.
	*	If no objects are found in the rectangle, return null.
	*/
	public JPanel select (Rectangle.Double r, boolean ctrlIsDown) {
		return null;	// no selection allowed here
	}

}



