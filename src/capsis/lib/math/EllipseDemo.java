/*
 * mathutil library for Capsis4.
 *
 * Copyright (C) 2004 Francois de Coligny.
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

package capsis.lib.math;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.geom.Point2D;
import java.util.Random;
import java.util.Vector;

import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

/**
 * EllipseDemo
 *
 * @author A. Piboule - october 2004
 */
public class EllipseDemo {

	public static DemoWindow myFrame;
	public static MyPanel drawing;
	public static JLabel nbOfIntersectionsLabel;

	public static Random rnd;

	public static long currentTime, delay;

	public static Ellipse e1;
	public static Ellipse e2;
	public static Vector intersections;
	public static String nbOfIntersections;


	static class DemoWindow extends JFrame implements ActionListener, KeyListener {


		DemoWindow (String s) {
			super (s);
			addKeyListener (this);

			addWindowListener (new WindowAdapter () {
								public void windowClosing (WindowEvent e) {
									System.exit (0);
								}});
			setBounds (300,50,500,500);

			rnd = new Random ();


			JPanel globalPanel = new JPanel ();
			drawing = new MyPanel ();


			JPanel panel = new JPanel (new GridLayout (6,1));
			panel.add (new JLabel ("Clic on white zone, or press a key, to create random ellipses"));
			panel.add (new JLabel (""));

			nbOfIntersectionsLabel = new JLabel ();
			panel.add (nbOfIntersectionsLabel);

			globalPanel.setLayout (new BorderLayout ());


			// insertion position setting
			drawing.addMouseListener (new MouseAdapter () {
				public void mouseClicked (MouseEvent e) {
					updateEllipse ();
			}});

			drawing.setMaximumSize (new Dimension (400,400));


			globalPanel.add (new JLabel ("Démo :"), BorderLayout.NORTH);
			globalPanel.add (drawing, BorderLayout.CENTER);
			globalPanel.add (panel, BorderLayout.SOUTH);

			updateEllipse ();
			intersections = Ellipse.ellipsesIntersection (e1, e2);

			getContentPane ().add (globalPanel);
			setVisible (true);
		}


		DemoWindow (String s, Ellipse e1t, Ellipse e2t) {
			this (s);
			e1=e1t;
			e2=e2t;


			intersections = Ellipse.ellipsesIntersection (e1, e2);
			nbOfIntersections = (new Integer (intersections.size ())).toString ();
			nbOfIntersectionsLabel.setText ("Number of intersection points : "+nbOfIntersections);

			System.out.println (intersections.size ()+" Intersections");
			for (int i=0 ; i<intersections.size ();i++) {
				Point2D.Double pt = (Point2D.Double) intersections.get (i);
				System.out.println ("x="+pt.x+" y="+pt.y);
			}
			System.out.println ("");
			drawing.repaint ();
		}



		public void updateEllipse () {
				e1 = new Ellipse (rnd.nextDouble ()*300d+100d, rnd.nextDouble ()*200d+100d, rnd.nextDouble ()*90d+10d, rnd.nextDouble ()*90d+10d, rnd.nextDouble ()*Math.PI*2d);
				e2 = new Ellipse (rnd.nextDouble ()*300d+100d, rnd.nextDouble ()*200d+100d, rnd.nextDouble ()*90d+10d, rnd.nextDouble ()*90d+10d, rnd.nextDouble ()*Math.PI*2d);
				drawing.repaint ();

				intersections = Ellipse.ellipsesIntersection (e1, e2);
				nbOfIntersections = (new Integer (intersections.size ())).toString ();
				nbOfIntersectionsLabel.setText ("Number of intersection points : "+nbOfIntersections);

				System.out.println (intersections.size ()+" Intersections");
				for (int i=0 ; i<intersections.size ();i++) {
					Point2D.Double pt = (Point2D.Double) intersections.get (i);
					System.out.println ("x="+pt.x+" y="+pt.y);
				}
				System.out.println ("");
		}


		public void keyPressed(KeyEvent e) {
			updateEllipse ();
		}

		public void keyTyped(KeyEvent e) {
		}

		public void keyReleased(KeyEvent e) {
		}


		public void actionPerformed (ActionEvent e) {

			if (e.getSource ().equals (drawing)) {
				drawing.repaint ();

			} else if (e.getSource ().equals (drawing)) {
			}
		}



	}


	static class MyPanel extends JPanel {

		public void paint (Graphics g) {

			Graphics2D g2 = (Graphics2D) g;

			g2.setColor (new Color (255,255,255));

			g2.fillRect (0, 0, 1000, 1000);

			g2.setColor (new Color (0,0,0));
			g2.draw (e1.getShape ());
			g2.setColor (new Color (255,0,0));
			g2.draw (e2.getShape ());

			if (intersections !=null) {
				for (int i=0;i<intersections.size ();i++) {
					Point2D.Double pt = (Point2D.Double) intersections.get (i);
					g2.setColor (new Color (0,0,255));
					g2.drawLine ((int) Math.round (pt.x)-5, (int) Math.round (pt.y), (int) Math.round (pt.x)+5, (int) Math.round (pt.y));
					g2.drawLine ((int) Math.round (pt.x), (int) Math.round (pt.y)-5, (int) Math.round (pt.x), (int) Math.round (pt.y+5));

				}
			}

		}
	}

	public static void main (String[] args) {

		myFrame = new DemoWindow ("Ellipse intersection demonstration");


	}

}