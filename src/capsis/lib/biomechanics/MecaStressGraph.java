/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.lib.biomechanics;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import capsis.util.Drawer;
import capsis.util.Panel2D;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;


/**
 * MecaStressGraph - Interface for view compression stresses along trees stem
 * according to abscissa SxxC and ordinate H.
 *
 * @author Ph. Ancelin - february 2002
 */
public class MecaStressGraph extends AmapDialog implements ActionListener, Drawer {
//checked for c4.1.1_08 - fc - 4.2.2003

	private MecaPanel2D panel2D;
	protected JPanel mainPanel;
	protected JScrollPane scrollPane;

	private JTextField xStepField;
	private JTextField yStepField;
	private JButton export;
	private JButton close;

	private MecaTree mecaTree;
	private double xMin;
	private double yMin;
	private double xMax;
	private double yMax;
	private double axisRate;
	private double xStep;
	private double yStep;
	private int nbXMark;
	private int nbYMark;
	private double xFirstMark;
	private double xLastMark;
	private double yFirstMark;
	private double yLastMark;
	private String type;
	private String xTitle;
	private String yTitle;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaStressGraph");
	}


	/**
	 * Constructors.
	 */
	public MecaStressGraph (JDialog d, MecaTree mecaTree) {
		super (d);

		this.mecaTree = mecaTree;
		continueConstruction ();
	}

	public MecaStressGraph (JFrame f, MecaTree mecaTree) {
		super (f);

		this.mecaTree = mecaTree;
		continueConstruction ();
	}

	public MecaStressGraph (MecaTree mecaTree) {
		super ();

		this.mecaTree = mecaTree;
		continueConstruction ();
	}

	public void continueConstruction () {
		setTitle (Translator.swap ("MecaStressGraph"));

		double x, y, width, height;
		Rectangle.Double r2 = null;

		xTitle = "SxxC (MPa)";
		yTitle = "Height (m)";
		xMin = 0d;
		yMin = 0d;
		xMax = Double.MIN_VALUE;
		yMax = Double.MIN_VALUE;
		xStep = 5d;
		yStep = 5d;

		y = 0d;
		Collection mecaGUs = mecaTree.getMecaGUs ();
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			/*x = currentGU.getSxxCB ();
			if (x >= xMax) {
				xMax = x;
			}
			if (y >= yMax) {
				yMax = y;
			}*/
			y += (currentGU.getHeight () / 2);
			x = currentGU.getSxxCM ();
			if (x >= xMax) {
				xMax = x;
			}
			if (y >= yMax) {
				yMax = y;
			}
			y += (currentGU.getHeight () / 2);
			/*x = currentGU.getSxxCT ();
			if (x >= xMax) {
				xMax = x;
			}
			if (y >= yMax) {
				yMax = y;
			}*/
		}

		xFirstMark = ((int) (xMin / xStep)) * xStep;
		xLastMark = xFirstMark;
		nbXMark = 1;
		while (xLastMark < xMax) {
			nbXMark ++;
			xLastMark += xStep;
		}
		width = xLastMark - xFirstMark;

		yFirstMark = ((int) (yMin / yStep)) * yStep;
		yLastMark = yFirstMark;
		nbYMark = 1;
		while (yLastMark < yMax) {
			nbYMark ++;
			yLastMark += yStep;
		}
		height = yLastMark - yFirstMark;
		axisRate = width / height;
		height *= axisRate;
		x = xFirstMark - width / 10d;
		y = yFirstMark - height / 10d;
		width +=  width / 5d;
		height +=  height / 5d;
		r2 = new Rectangle.Double (x, y, width, height);
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
				"graphHvsSxx", axisRate, yMin);

		//~ setBounds(20, 235, 450, 500);	// fc - 31.1.2003
		createUI ();
		
		setModal (false);
		pack ();
		show ();
	}






	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (xStepField)) {
			if (Check.isEmpty (xStepField.getText ()) || !Check.isDouble (xStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaStressGraph.stepIsNotANumber"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			xStep = Check.doubleValue (xStepField.getText ());

			double x, y, width, height;
			Rectangle.Double r2 = null;

			xFirstMark = ((int) (xMin / xStep)) * xStep;
			xLastMark = xFirstMark;
			nbXMark = 1;
			while (xLastMark < xMax) {
				nbXMark ++;
				xLastMark += xStep;
			}
			width = xLastMark - xFirstMark;

			height = yLastMark - yFirstMark;
			axisRate = width / height;
			panel2D.setAxisRate (axisRate);
			height *= axisRate;
			x = xFirstMark - width / 10d;
			y = yFirstMark - height / 10d;
			width +=  width / 5d;
			height +=  height / 5d;
			r2 = new Rectangle.Double (x, y, width, height);

			r2 = panel2D.addMarginToUserBounds (r2, panel2D.getBounds ());
			panel2D.setUserBounds (r2);
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (yStepField)) {
			if (Check.isEmpty (yStepField.getText ()) || !Check.isDouble (yStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaStressGraph.stepIsNotANumber"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			yStep = Check.doubleValue (yStepField.getText ());

			double x, y, width, height;
			Rectangle.Double r2 = null;

			width = xLastMark - xFirstMark;

			yFirstMark = ((int) (yMin / yStep)) * yStep;
			yLastMark = yFirstMark;
			nbYMark = 1;
			while (yLastMark < yMax) {
				nbYMark ++;
				yLastMark += yStep;
			}
			height = yLastMark - yFirstMark;
			axisRate = width / height;
			panel2D.setAxisRate (axisRate);
			height *= axisRate;
			x = xFirstMark - width / 10d;
			y = yFirstMark - height / 10d;
			width +=  width / 5d;
			height +=  height / 5d;
			r2 = new Rectangle.Double (x, y, width, height);

			r2 = panel2D.addMarginToUserBounds (r2, panel2D.getBounds ());
			panel2D.setUserBounds (r2);
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (export)) {
			exportGraph ();

		} else if (evt.getSource ().equals (close)) {
			dispose ();

		}
	}

	/**
	 * Export to Log.
	 */
	public void exportGraph () {

		// NOTE : should use StringBuffer and append () instead of String + String + String

		String expstr = "\n\n<<*****************************************************************************>>\n";
		expstr += "\nExport de donn�es pour " + mecaTree.getMecaProcess ().getStep ().getCaption ();
		double h, hb;
		if (mecaTree.getMecaProcess ().getConstraints ().standHeight.equals ("mean")) {
			h = mecaTree.getMecaProcess ().getMeanHeight ();
			hb = mecaTree.getMecaProcess ().getMeanCrownBaseHeight ();
		} else {
			h = mecaTree.getMecaProcess ().getDominantHeight ();
			hb = mecaTree.getMecaProcess ().getDominantCrownBaseHeight ();
		}

		String windLevel;
		if (mecaTree.getMecaProcess ().getConstraints ().windAt10m) {
			windLevel = "At 10 m above the ground";
		} else {
			windLevel = "At h (m) above the ground";
		}

		expstr += "\nCalculs faits pour location =\t" + mecaTree.getMecaProcess ().getConstraints ().location;
		expstr += "\nHauteurs (m) prises en compte : \th =\t" + h + "\thb =\t" + hb;
		expstr += "\nWind Level at stand edge =\t" + windLevel;
		expstr += "\nWindSpeedEdgeAt10m (m/s) =\t" + mecaTree.getMecaProcess ().getWindSpeedEdgeAt10m ();
		expstr += "\nWindSpeedEdgeAtH (m/s) =\t" + mecaTree.getMecaProcess ().getWindSpeedEdgeAtH ();
		expstr += "\nWindSpeedStandAtH (m/s) =\t" + mecaTree.getMecaProcess ().getWindSpeedStandAtH ();
		expstr += "\nWindSpeedStandAtHb (m/s) =\t" + mecaTree.getMecaProcess ().getWindSpeedStandAtHb ();
		expstr += "\nGraphe contrainte - hauteur pour arbre n� " + mecaTree.getId ();
		expstr += "\n\nSxx\tHeight";

		double x = 0d;
		double y = 0d;
		Collection mecaGUs = mecaTree.getMecaGUs ();
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			y += (currentGU.getHeight () / 2);
			x = currentGU.getSxxCM ();
			expstr += "\n"+ x + "\t" + y;
			y += (currentGU.getHeight () / 2);
		}

		expstr += "\n\n<<*****************************************************************************>>\n\n";
		Log.println (expstr);
	}

	/**
	 * Dispose the box.
	 */
	public void dispose () {
		super.dispose ();
		if (panel2D != null) {
			panel2D.dispose ();
			panel2D = null;
		}
		mainPanel = null;
		scrollPane = null;
	}

	// Manage tool tip.
	//
	private void updateToolTipText () {
		String toolTipText = Translator.swap ("MecaStressGraph.information");
		panel2D.setToolTipText (toolTipText);
	}

	public int getPanel2DXMargin () {return Panel2D.X_MARGIN_IN_PIXELS;}

	public int getPanel2DYMargin () {return Panel2D.Y_MARGIN_IN_PIXELS;}

	/**
	 * Draw points.
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Graph axes.
		double height = yLastMark - yFirstMark;
		height *= axisRate;
		double width = xLastMark - xFirstMark;
		double xmin = xFirstMark - width / 10d;
		double ymin = yFirstMark - height / 10d;
		double xx = xmin + width + width / 5d;
		double yx = ymin;
		double xy = xmin;
		double yy = ymin + height + height / 5d;
		Shape sh1 = new Line2D.Double (xmin, ymin, xx, yx);
		Shape sh2 = new Line2D.Double (xmin, ymin, xy, yy);
		g2.setColor (Color.black);
		g2.draw (sh1);
		g2.drawString (xTitle, (float) xx, (float) (yx - panel2D.getUserHeight (15)));
		g2.draw (sh2);
		g2.drawString (yTitle, (float) (xy - panel2D.getUserWidth (30)), (float) yy);

		// Axes marks.
		// Dbh axis : one mark every xStep cm
		// H axis : one mark every yStep m
		g2.setColor (Color.black);
		double xMark = xFirstMark;
		double yMark = ymin - panel2D.getUserHeight (5);
		for (int i=0; i<nbXMark; i++) {
			sh2 = new Line2D.Double (xMark, ymin, xMark, yMark);
			g2.draw (sh2);
			g2.drawString (String.valueOf (xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			xMark += xStep;
		}

		xMark = xmin - panel2D.getUserHeight (5);
		yMark = yFirstMark;
		for (int i=0; i<nbYMark; i++) {
			double doubleYMark = yMark;
			height = doubleYMark - yMin;
			height *= axisRate;
			doubleYMark = yMin + height;
			sh1 = new Line2D.Double (xmin, doubleYMark, xMark, doubleYMark);
			g2.draw (sh1);
			g2.drawString (String.valueOf (yMark), (float) (xmin - panel2D.getUserWidth (30)), (float) (doubleYMark - panel2D.getUserHeight (10)));
			yMark += yStep;
		}

		double x = 0d;
		double y = 0d;
		double yPoint;
		double xd = 0d;
		double yd = 0d;
		width = panel2D.getUserWidth (6);
		Collection mecaGUs = mecaTree.getMecaGUs ();
		Rectangle2D bBox;

		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();

			/*x = currentGU.getSxxCB ();
			height = y - yMin;
			height *= axisRate;
			yPoint = yMin + height;
			sh1 = new Ellipse2D.Double (x-width/2, yPoint-width/2, width, width);
			g2.setColor (Color.blue);
			bBox = sh1.getBounds2D ();
			if (r.intersects (bBox)) {
				g2.draw (sh1);
				g2.fill (sh1);
			}
			sh1 = new Line2D.Double (xd, yd, x, yPoint);
			g2.setColor (Color.blue);
			bBox = sh1.getBounds2D ();
			g2.draw (sh1);
			xd = x;
			yd = yPoint;*/

			y += (currentGU.getHeight () / 2);
			x = currentGU.getSxxCM ();
			height = y - yMin;
			height *= axisRate;
			yPoint = yMin + height;
			sh1 = new Ellipse2D.Double (x-width/2, yPoint-width/2, width, width);
			g2.setColor (Color.blue);
			bBox = sh1.getBounds2D ();
			if (r.intersects (bBox)) {
				g2.draw (sh1);
				g2.fill (sh1);
			}
			sh1 = new Line2D.Double (xd, yd, x, yPoint);
			bBox = sh1.getBounds2D ();
			if (r.intersects (bBox)) {
				g2.draw (sh1);
			}
			xd = x;
			yd = yPoint;

			y += (currentGU.getHeight () / 2);
			/*x = currentGU.getSxxCT ();
			height = y - yMin;
			height *= axisRate;
			yPoint = yMin + height;
			sh1 = new Ellipse2D.Double (x-width/2, yPoint-width/2, width, width);
			g2.setColor (Color.blue);
			bBox = sh1.getBounds2D ();
			if (r.intersects (bBox)) {
				g2.draw (sh1);
				g2.fill (sh1);
			}
			sh1 = new Line2D.Double (xd, yd, x, yPoint);
			g2.setColor (Color.blue);
			bBox = sh1.getBounds2D ();
			if (r.intersects (bBox)) {
				g2.draw (sh1);
			}
			xd = x;
			yd = yPoint;*/
		}
		updateToolTipText ();
	}

	/**
	 * Select mecaTrees.
	 */
	public JPanel select (Rectangle.Double r, boolean more) {
		JPanel infoPanel = null;
		return infoPanel;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		mainPanel = new JPanel ();
		mainPanel.setLayout (new BorderLayout ());

		// 1. Options panel.
		Box part1 = Box.createVerticalBox ();

		JPanel pOptions1 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		pOptions1.add (new JWidthLabel (Translator.swap ("MecaStressGraph.graduatingStep")+"    ", 10));
		xStepField = new JTextField (2);
		xStepField.addActionListener (this);
		pOptions1.add (new JWidthLabel (xTitle+" :", 10));
		pOptions1.add (xStepField);
		xStepField.setText (""+ xStep);
		yStepField = new JTextField (2);
		yStepField.addActionListener (this);
		pOptions1.add (new JWidthLabel ("    "+yTitle+" :", 10));
		pOptions1.add (yStepField);
		yStepField.setText (""+yStep);
		part1.add (pOptions1);

		// 1. Viewer panel2D.
		scrollPane = new JScrollPane (panel2D);

		scrollPane.setPreferredSize (new Dimension (400, 400));

		scrollPane.getViewport().putClientProperty
              ("EnableWindowBlit", Boolean.TRUE);	// faster
		mainPanel.add (scrollPane, BorderLayout.CENTER);

		// 2. Control panel.
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		export = new JButton (Translator.swap ("MecaStressGraph.export"));
		export.addActionListener (this);
		pControl.add (export);
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		pControl.add (close);

		// Set close as default (see AmapDialog).
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// Layout parts.
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}


//======================================================================>>>

	/*public MecaStressGraph (MecaTree mecaTree) {
		super ();

		this.mecaTree = mecaTree;
		setTitle (Translator.swap ("MecaStressGraph"));

		double x, y, width, height;
		Rectangle.Double r2 = null;

		xTitle = "SxxC (MPa)";
		yTitle = "Height (m)";
		xMin = 0d;
		yMin = 0d;
		xMax = Double.MIN_VALUE;
		yMax = Double.MIN_VALUE;
		xStep = 5d;
		yStep = 5d;

		y = 0d;
		Collection mecaGUs = mecaTree.getMecaGUs ();
		for (Iterator i = mecaGUs.iterator (); i.hasNext ();) {
			MecaGU currentGU = (MecaGU) i.next ();
			y += (currentGU.getHeight () / 2);
			x = currentGU.getSxxCM ();
			if (x >= xMax) {
				xMax = x;
			}
			if (y >= yMax) {
				yMax = y;
			}
			y += (currentGU.getHeight () / 2);
		}

		xFirstMark = ((int) (xMin / xStep)) * xStep;
		xLastMark = xFirstMark;
		nbXMark = 1;
		while (xLastMark < xMax) {
			nbXMark ++;
			xLastMark += xStep;
		}
		width = xLastMark - xFirstMark;

		yFirstMark = ((int) (yMin / yStep)) * yStep;
		yLastMark = yFirstMark;
		nbYMark = 1;
		while (yLastMark < yMax) {
			nbYMark ++;
			yLastMark += yStep;
		}
		height = yLastMark - yFirstMark;
		axisRate = width / height;
		height *= axisRate;
		x = xFirstMark - width / 10d;
		y = yFirstMark - height / 10d;
		width +=  width / 5d;
		height +=  height / 5d;
		r2 = new Rectangle.Double (x, y, width, height);
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
				"graphHvsSxx", axisRate, yMin);

		//~ setBounds(20, 235, 450, 500);	// fc - 31.1.2003
		createUI ();
		
		setModal (false);
		pack ();
		show ();
	}

*/


}




