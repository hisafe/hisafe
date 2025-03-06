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

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Iterator;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JPanel;

/**
 * Test de déplacements et de zooms sur Graphics2D.
 * 
 * @author F. de Coligny - february 2001
 */
public class ZoomG2d extends JFrame implements ActionListener {

	static class View extends JPanel {

		private Vector data;
		private Rectangle.Double userBounds;
		private double xOffset = 0;
		private double yOffset = 0;

		public View (Vector data) {
			super ();

			this.data = data;

			setBackground (Color.red);
			System.out.println ("View (init) bounds="+getBounds ());

			resetUserBounds ();
		}

		public void resetUserBounds () {
			xOffset = 0;
			yOffset = 0;
			userBounds = getInitialUserBounds (data);
			repaint ();
		}

		private Rectangle.Double getInitialUserBounds (Vector data) {
			// compute initial userBounds () : the whole plot
			double x0 = Double.MAX_VALUE;
			double y0 = Double.MAX_VALUE;
			double x1 = Double.MIN_VALUE;
			double y1 = Double.MIN_VALUE;

			for (Iterator i = data.iterator (); i.hasNext ();) {
				Rectangle.Double r = (Rectangle.Double) i.next ();
				if (r.x < x0) {x0 = r.x;}
				if (r.y < y0) {y0 = r.y;}
				if (r.x > x1) {x1 = r.x;}
				if (r.y > y1) {y1 = r.y;}
			}
			double w = x1 - x0;
			double h = y1 - y0;
			System.out.println ("InitialUserBounds="+new Rectangle.Double (x0, y0, w, h));
			return new Rectangle.Double (x0, y0, w, h);
		}

		private double getScale (Rectangle.Double userBounds, Rectangle bounds) {
			double w = bounds.getWidth () / userBounds.getWidth ();
			double h = bounds.getHeight () / userBounds.getHeight ();
			System.out.println ("scaleFactor="+Math.min (w, h));
			return Math.min (w, h);
		}

		private void paintData (Graphics2D g2) {
			for (Iterator i = data.iterator (); i.hasNext ();) {
				Rectangle.Double r = (Rectangle.Double) i.next ();
				int x = (int) r.x;
				int y = (int) r.y;
				int w = (int) r.getWidth ();
				g2.drawLine (x, y, x, y);
				g2.drawOval (x-w/2, y-w/2, w, w);
			}
		}

		public void setUserBounds (Rectangle.Double r) {
			userBounds = r;
			System.out.println ("setUserBounds ():"+r);
			repaint ();
		}

		public void zoomIn () {
			Rectangle.Double z = userBounds;
			double zoomFactor = (double) Math.max (z.width, z.height)/10;

			xOffset+=zoomFactor/2;
			yOffset+=zoomFactor/2;
			setUserBounds (new Rectangle.Double (z.x+zoomFactor,
												z.y+zoomFactor,
												z.width-zoomFactor,
												z.height-zoomFactor));
		}

		public void zoomOut () {
			Rectangle.Double z = userBounds;
			double zoomFactor = (double) Math.max (z.width, z.height)/10;

			xOffset-=zoomFactor/2;
			yOffset-=zoomFactor/2;
			setUserBounds (new Rectangle.Double (z.x-zoomFactor,
												z.y-zoomFactor,
												z.width+zoomFactor,
												z.height+zoomFactor));
		}

		public void move (Point shift) {
			xOffset += shift.x;
			yOffset += shift.y;
			repaint ();
		}

		public void paintComponent (Graphics g) {
			super.paintComponent (g);

			Rectangle bounds = getBounds ();
			System.out.println ("View bounds="+bounds);

			Graphics2D g2 = (Graphics2D) g;

			g.setClip (bounds);		// SEEMS IMPORTANT

			// origin is bottom left and y grows upward
			g2.translate (bounds.width/2, bounds.height/2);
			g2.scale (1, -1);

			// apply scale factor to represent the chosen user zone on
			// the available device zone
			double scaleFactor = getScale (userBounds, bounds);

			g2.scale (scaleFactor, scaleFactor);
			g2.translate (-(userBounds.width/2+xOffset), -(userBounds.height/2+yOffset));

			paintData (g2);
		}
	}









	private View view;
	private JButton b1;
	private JButton b2;
	private JButton b3;
	private JButton north;
	private JButton east;
	private JButton south;
	private JButton west;

	public ZoomG2d () {
		super ("ZoomG2d");

		Vector data = createData ();

		setBounds (10, 10, 200, 200);
			System.out.println ("ZoomG2d bounds="+getBounds ());
		view = new View (data);
		setContentPane (view);

		JDialog d = new JDialog (this, "Control", false);

		b1 = new JButton ("Zoom +");
		b1.addActionListener (this);
		d.getContentPane ().add (b1, BorderLayout.NORTH);
		b2 = new JButton ("Zoom -");
		b2.addActionListener (this);
		d.getContentPane ().add (b2, BorderLayout.SOUTH);
		b3 = new JButton ("Zoom 0");
		b3.addActionListener (this);
		d.getContentPane ().add (b3, BorderLayout.CENTER);

		JPanel arrows = new JPanel (new BorderLayout ());
		north = new JButton ("^");
		north.addActionListener (this);
		arrows.add (north, BorderLayout.NORTH);
		east = new JButton (">");
		east.addActionListener (this);
		arrows.add (east, BorderLayout.EAST);
		south = new JButton ("v");
		south.addActionListener (this);
		arrows.add (south, BorderLayout.SOUTH);
		west = new JButton ("<");
		west.addActionListener (this);
		arrows.add (west, BorderLayout.WEST);
		d.getContentPane ().add (arrows, BorderLayout.EAST);

		d.pack ();

		d.setVisible (true);




		setVisible (true);
	}

	public void actionPerformed (ActionEvent e) {
		if (e.getSource ().equals (b1)) {
			view.zoomIn ();
		} else if (e.getSource ().equals (b2)) {
			view.zoomOut ();
		} else if (e.getSource ().equals (b3)) {
			view.resetUserBounds ();
		} else if (e.getSource ().equals (north)) {
			view.move (new Point (0, -1));
		} else if (e.getSource ().equals (east)) {
			view.move (new Point (-1, 0));
		} else if (e.getSource ().equals (south)) {
			view.move (new Point (0, 1));
		} else if (e.getSource ().equals (west)) {
			view.move (new Point (1, 0));
		}
	}

	private Vector createData () {
		Vector data = new Vector ();
		data.add (new Rectangle.Double (0, 0, 10, 1));
		data.add (new Rectangle.Double (0, 40, 10, 2));
		data.add (new Rectangle.Double (40, 40, 10, 3));
		data.add (new Rectangle.Double (40, 0, 10, 4));
		data.add (new Rectangle.Double (10, 10, 5, 5));
		return data;
	}

	public static void main (String[] args) {
		new ZoomG2d ();
	}


}
