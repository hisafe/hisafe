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
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComponent;
import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JSlider;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import capsis.commongui.util.Helper;
import capsis.commongui.util.Tools;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import capsis.util.SmartFlowLayout;
import jeeb.lib.util.Check;
import jeeb.lib.util.Translator;

//import capsis.lib.rubberband.*;

/**
 * MecaPTrunkShape - Panel for view trees trunk shape.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaPTrunkShape extends JPanel implements ChangeListener, ActionListener, Drawer/*, MouseMotionListener*/ {
//checked for c4.1.1_08 - fc - 4.2.2003

	private MecaPanel2D panel2D;
	private JPanel mainPanel;
	private JScrollPane scrollPane;

	private JSlider angleSlider;
	private JTextField angleText;
	private double angle;

	private JButton help;

	private JButton stress;
	private JButton moment;

	private MecaTree mecaTree;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaPTrunkShape");
	}


	/**
	 * Constructor.
	 */
	public MecaPTrunkShape (MecaTree mecaTree, Rectangle.Double r2) {
		super ();
		this.setLayout (new BorderLayout ());

		this.mecaTree = mecaTree;
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (), "tracer");
		angle = 0d;

		createUI ();
		setVisible (true);
		//setSize(400, 400);
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (angleText)) {
			if (Check.isEmpty (angleText.getText ()) || !Check.isInt (angleText.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaPTrunkShape.angleIsNotAnInteger"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			angleSlider.setValue (Check.intValue (angleText.getText ()));
		}
		if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);	// fc - 3.2.2003
			//~ MecaDHelp helpDialog = new MecaDHelp (Translator.swap ("MecaPTrunkShape.helpTrunkForm"),
					//~ Translator.swap ("MecaPTrunkShape.helpDialog"),
					//~ 560, 500);
		}
		if (evt.getSource ().equals (stress)) {
			Window w = Tools.getWindow (this);
			if (w instanceof JDialog) {
				new MecaStressGraph ((JDialog) w, mecaTree);
			} else if (w instanceof JFrame) {
				new MecaStressGraph ((JFrame) w, mecaTree);
			} else {
				new MecaStressGraph (mecaTree);
			}
			//new MecaStressGraph (mecaTree);
		}
		if (evt.getSource ().equals (moment)) {
			Window w = Tools.getWindow (this);
			if (w instanceof JDialog) {
				new MecaMomentHisto ((JDialog) w, mecaTree);
			} else if (w instanceof JFrame) {
				new MecaMomentHisto ((JFrame) w, mecaTree);
			} else {
				new MecaMomentHisto (mecaTree);
			}
			//new MecaMomentHisto (mecaTree);
		}
	}

	/**
	 * Answers to the slider changing panel state.
	 */
	public void stateChanged (ChangeEvent evt) {
		if (evt.getSource ().equals (angleSlider)) {
			angleText.setText (""+angleSlider.getValue ());
			angle = (double) angleSlider.getValue ();
			// if we want the new view while slider is changing...
			//this.updateUI ();
			panel2D.reset ();	// fc - 31.1.2003 - important : else, the image is not recalculated
			panel2D.repaint ();

			// if we want the slider is adjusting !
				/*if (!angleSlider.getValueIsAdjusting()) {
					angle = (double) angleSlider.getValue ();
					//this.updateUI ();
					panel2D.repaint ();
				}*/
		}
	}

	/**
	 * Creates a tool tip text on panel2D to inform user if some filtering is
	 * currently active.
	 */
	private void updateToolTipText (/*double x, double z*/) {
		/*String toolTipText="";
		toolTipText+="x' = " + x + " ; z = " + z + " (m)";
		panel2D.setToolTipText (toolTipText);*/

		String toolTipText = Translator.swap ("MecaPTrunkShape.coordinates");
		panel2D.setToolTipText (toolTipText);
	}

	/*// MouseMotionListener stuff -----------------------------------------------
	public void mouseDragged (MouseEvent evt) {}
	public void mouseMoved (MouseEvent evt) {
		// mouse moves when Shift is pressed
		if (evt.isShiftDown ()) {
			int shift = 1;
			Rubberband rubberBand = new RubberbandRectangle (panel2D);
			Point center = evt.getPoint ();
			rubberBand.anchor (new Point (center.x, center.y));
			rubberBand.end (new Point (center.x+shift, center.y+shift));

			Rectangle rect = rubberBand.lastBounds ();
			Rectangle.Double r = panel2D.getUserRectangle (rect);

			double x = r.getX ();
			double z = r.getY ();

			String toolTipText="";
			toolTipText+="x' = " + x + " ; z = " + z + " (m)";
			panel2D.setToolTipText (toolTipText);
			//this.updateUI ();
			panel2D.repaint ();
		}
	}*/

	public int getPanel2DXMargin () {return Panel2D.X_MARGIN_IN_PIXELS;}

	public int getPanel2DYMargin () {return Panel2D.Y_MARGIN_IN_PIXELS;}

	/**
	 * Draw a mecaGU.
	 */
	public void drawGU (Graphics2D g2, Rectangle.Double r, MecaGU gu) {
		int nPoints = 4;
		double [] abscissaPoints = new double [nPoints];
		double [] ordinatePoints = new double [nPoints];
		double xmt, ymt, zmt, xp, yp, zp, xc, yc, zc;
		xmt = mecaTree.getX ();
		ymt = mecaTree.getY ();
		zmt = mecaTree.getZ ();
		if (gu.previousGU () != null) {
			xp = gu.previousGU ().getXTop () - xmt;
			yp = gu.previousGU ().getYTop () - ymt;
			zp = gu.previousGU ().getZTop () - zmt;
		} else {
			xp = 0d;
			yp = 0d;
			zp = 0d;
		}
		xc = gu.getXTop () - xmt;
		yc = gu.getYTop () - ymt;
		zc = gu.getZTop () - zmt;

		double angleRadian = -angle * Math.PI / 180;
		MecaVector nrx = new MecaVector (2);
		nrx.setElement (0, Math.cos (angleRadian));
		nrx.setElement (1, Math.sin (angleRadian));

		MecaVector nry = new MecaVector (2);
		nry.setElement (0, -nrx.getElement (1));
		nry.setElement (1, nrx.getElement (0));

		MecaMatrix R = new MecaMatrix (2);
		R.setColumn (0, nrx);
		R.setColumn (1, nry);
		R = R.transpose();

		MecaVector coordinates = new MecaVector (2);
		coordinates.setElement (0, xp);
		coordinates.setElement (1, yp);
		coordinates = R.pro (coordinates);
		xp = coordinates.getElement (0);
		yp = coordinates.getElement (1);

		coordinates.setElement (0, xc);
		coordinates.setElement (1, yc);
		coordinates = R.pro (coordinates);
		xc = coordinates.getElement (0);
		yc = coordinates.getElement (1);

		MecaVector vbs = new MecaVector (2);
		double zoomRadius = 4d;
		vbs.setElement (0, zp - zc);
		vbs.setElement (1, xc - xp);
		double norm = vbs.norm ();
		if (norm != 0d) {
			vbs = vbs.div (norm);
		}
		vbs = vbs.pro (gu.getRadius () * zoomRadius);
		abscissaPoints [0] = xc + vbs.getElement (0);
		ordinatePoints [0] = zc + vbs.getElement (1);
		abscissaPoints [1] = xc - vbs.getElement (0);
		ordinatePoints [1] = zc - vbs.getElement (1);

		/*double abscissaBase = xp;
		double ordinateBase = zp;
		double abscissaTop = xc;
		double ordinateTop = zc;
		Shape sh = new Line2D.Double (abscissaBase, ordinateBase, abscissaTop, ordinateTop);*/

		if (gu == gu.getMecaTree ().firstGU ()) {
			abscissaPoints [2] = gu.getRadius () * zoomRadius;
			ordinatePoints [2] = 0d;
			abscissaPoints [3] = - gu.getRadius () * zoomRadius;
			ordinatePoints [3] = 0d;
		} else if (gu == gu.getMecaTree ().firstGU ().nextGU ()) {
			vbs.setElement (0, - zp);
			vbs.setElement (1, xp);
			norm = vbs.norm ();
			if (norm != 0d) {
				vbs = vbs.div (norm);
			}
			vbs = vbs.pro (gu.getRadius () * zoomRadius);
			abscissaPoints [2] = xp - vbs.getElement (0);
			ordinatePoints [2] = zp - vbs.getElement (1);
			abscissaPoints [3] = xp + vbs.getElement (0);
			ordinatePoints [3] = zp + vbs.getElement (1);
		} else {
			xc = xp;
			zc = zp;
			xp = gu.previousGU ().previousGU ().getXTop () - xmt;
			yp = gu.previousGU ().previousGU ().getYTop () - ymt;
			zp = gu.previousGU ().previousGU ().getZTop () - zmt;
			coordinates.setElement (0, xp);
			coordinates.setElement (1, yp);
			coordinates = R.pro (coordinates);
			xp = coordinates.getElement (0);
			yp = coordinates.getElement (1);
			vbs.setElement (0, zp - zc);
			vbs.setElement (1, xc - xp);
			norm = vbs.norm ();
			if (norm != 0d) {
				vbs = vbs.div (norm);
			}
			vbs = vbs.pro (gu.getRadius () * zoomRadius);

			abscissaPoints [2] = xc - vbs.getElement (0);
			ordinatePoints [2] = zc - vbs.getElement (1);
			abscissaPoints [3] = xc + vbs.getElement (0);
			ordinatePoints [3] = zc + vbs.getElement (1);
		}

		GeneralPath p2 = Tools.getPolygon2D (abscissaPoints, ordinatePoints, nPoints);

		Rectangle2D bBox = p2.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.setColor (new Color (150, 75, 0));
			g2.fill (p2);
			g2.setColor (new Color (220, 110, 0));
			//g2.draw (sh);
		}

	}

	/**
	 * Draw the crown part of a mecaGU.
	 */
	public void drawCrownGU (Graphics2D g2, Rectangle.Double r, MecaGU gu) {
		int nPoints = 4;
		double [] abscissaPoints = new double [nPoints];
		double [] ordinatePoints = new double [nPoints];
		double xmt, ymt, zmt, xp, yp, zp, xc, yc, zc;
		xmt = mecaTree.getX ();
		ymt = mecaTree.getY ();
		zmt = mecaTree.getZ ();
		if (gu.previousGU () != null) {
			xp = gu.previousGU ().getXTop () - xmt;
			yp = gu.previousGU ().getYTop () - ymt;
			zp = gu.previousGU ().getZTop () - zmt;
		} else {
			xp = 0d;
			yp = 0d;
			zp = 0d;
		}
		xc = gu.getXTop () - xmt;
		yc = gu.getYTop () - ymt;
		zc = gu.getZTop () - zmt;

		double angleRadian = -angle * Math.PI / 180;
		MecaVector nrx = new MecaVector (2);
		nrx.setElement (0, Math.cos (angleRadian));
		nrx.setElement (1, Math.sin (angleRadian));

		MecaVector nry = new MecaVector (2);
		nry.setElement (0, -nrx.getElement (1));
		nry.setElement (1, nrx.getElement (0));

		MecaMatrix R = new MecaMatrix (2);
		R.setColumn (0, nrx);
		R.setColumn (1, nry);
		R = R.transpose();

		MecaVector coordinates = new MecaVector (2);
		coordinates.setElement (0, xp);
		coordinates.setElement (1, yp);
		coordinates = R.pro (coordinates);
		xp = coordinates.getElement (0);
		yp = coordinates.getElement (1);

		coordinates.setElement (0, xc);
		coordinates.setElement (1, yc);
		coordinates = R.pro (coordinates);
		xc = coordinates.getElement (0);
		yc = coordinates.getElement (1);

		int ngu = gu.getId () - gu.getMecaTree ().getId () * 10000;
		ngu = ngu / 100 - 1;
		double crownRadius = gu.getMecaTree ().CrownRadiusAt (ngu);

		MecaVector vbs = new MecaVector (2);
		double zoomRadius = 1d;
		vbs.setElement (0, zp - zc);
		vbs.setElement (1, xc - xp);
		double norm = vbs.norm ();
		if (norm != 0d) {
			vbs = vbs.div (norm);
		}
		vbs = vbs.pro (crownRadius * zoomRadius);
		abscissaPoints [0] = xc + vbs.getElement (0);
		ordinatePoints [0] = zc + vbs.getElement (1);
		abscissaPoints [1] = xc - vbs.getElement (0);
		ordinatePoints [1] = zc - vbs.getElement (1);

		/*double abscissaBase = xp;
		double ordinateBase = zp;
		double abscissaTop = xc;
		double ordinateTop = zc;
		Shape sh = new Line2D.Double (abscissaBase, ordinateBase, abscissaTop, ordinateTop);*/

		if (gu == gu.getMecaTree ().firstGU ()) {
			abscissaPoints [2] = crownRadius * zoomRadius;
			ordinatePoints [2] = 0d;
			abscissaPoints [3] = - crownRadius * zoomRadius;
			ordinatePoints [3] = 0d;
		} else if (gu == gu.getMecaTree ().firstGU ().nextGU ()) {
			vbs.setElement (0, - zp);
			vbs.setElement (1, xp);
			norm = vbs.norm ();
			if (norm != 0d) {
				vbs = vbs.div (norm);
			}
			vbs = vbs.pro (crownRadius * zoomRadius);
			abscissaPoints [2] = xp - vbs.getElement (0);
			ordinatePoints [2] = zp - vbs.getElement (1);
			abscissaPoints [3] = xp + vbs.getElement (0);
			ordinatePoints [3] = zp + vbs.getElement (1);
		} else {
			xc = xp;
			zc = zp;
			xp = gu.previousGU ().previousGU ().getXTop () - xmt;
			yp = gu.previousGU ().previousGU ().getYTop () - ymt;
			zp = gu.previousGU ().previousGU ().getZTop () - zmt;
			coordinates.setElement (0, xp);
			coordinates.setElement (1, yp);
			coordinates = R.pro (coordinates);
			xp = coordinates.getElement (0);
			yp = coordinates.getElement (1);
			vbs.setElement (0, zp - zc);
			vbs.setElement (1, xc - xp);
			norm = vbs.norm ();
			if (norm != 0d) {
				vbs = vbs.div (norm);
			}
			vbs = vbs.pro (crownRadius * zoomRadius);

			abscissaPoints [2] = xc - vbs.getElement (0);
			ordinatePoints [2] = zc - vbs.getElement (1);
			abscissaPoints [3] = xc + vbs.getElement (0);
			ordinatePoints [3] = zc + vbs.getElement (1);
		}

		GeneralPath p2 = Tools.getPolygon2D (abscissaPoints, ordinatePoints, nPoints);

		Rectangle2D bBox = p2.getBounds2D ();
		if (r.intersects (bBox)) {
			//g2.setColor (new Color (150, 75, 0));
			g2.setColor (Color.green);
			g2.fill (p2);
			g2.setColor (new Color (220, 110, 0));
			//g2.draw (sh);
		}

	}

	/**
	 * Draw mecaGUs of a tree.
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Draw different axes :

		double angleRadian = -angle * Math.PI / 180;
		MecaVector nrx = new MecaVector (2);
		nrx.setElement (0, Math.cos (angleRadian));
		nrx.setElement (1, Math.sin (angleRadian));

		MecaVector nry = new MecaVector (2);
		nry.setElement (0, -nrx.getElement (1));
		nry.setElement (1, nrx.getElement (0));

		MecaMatrix R = new MecaMatrix (2);
		R.setColumn (0, nrx);
		R.setColumn (1, nry);
		R = R.transpose();

		// Tree base...
		double width = panel2D.getUserWidth (10);
		double xR = - (r.getWidth () / 2) + (r.getWidth () / 6);
		double yR = r.getHeight () - panel2D.getUserHeight (110);	// 0d;
		Shape sh0 = new Ellipse2D.Double (xR-width/2, yR-width/2, width, width);
		g2.setColor (Color.green);
		g2.fill (sh0);

		// x and y axes
		double height = panel2D.getUserHeight (50);
		width = panel2D.getUserWidth (50);
		double xx = xR + width;
		double yx = yR;
		double xy = xR;
		double yy = yR + height;
		Shape sh1 = new Line2D.Double (xR, yR, xx, yx);
		Shape sh2 = new Line2D.Double (xR, yR, xy, yy);
		g2.setColor (Color.black);
		g2.draw (sh1);
		g2.drawString ("x", (float) xx, (float) (yx - panel2D.getUserHeight (15)));
		g2.draw (sh2);
		g2.drawString ("y", (float) (xy - panel2D.getUserWidth (10)), (float) yy);

		// x' axis...
		xx = xx - panel2D.getUserWidth (15);
		xy = xR - (xx - xR);
		yy = yx;
		MecaVector coordinates = new MecaVector (2);
		coordinates.setElement (0, xx - xR);
		coordinates.setElement (1, yx - yR);
		coordinates = R.pro (coordinates);
		xx = coordinates.getElement (0) + xR;
		yx = coordinates.getElement (1) + yR;
		sh1 = new Line2D.Double (xR, yR, xx, yx);
		coordinates.setElement (0, xy - xR);
		coordinates.setElement (1, yy - yR);
		coordinates = R.pro (coordinates);
		xy = coordinates.getElement (0) + xR;
		yy = coordinates.getElement (1) + yR;
		sh2 = new Line2D.Double (xR, yR, xy, yy);
		g2.setColor (Color.red);
		g2.draw (sh1);
		g2.drawString ("x'", (float) xx, (float) yx);
		g2.draw (sh2);

		// User point of view...
		int nPoints = 3;
		double [] abscissaPoints = new double [nPoints];
		double [] ordinatePoints = new double [nPoints];
		abscissaPoints [0] = xR;
		ordinatePoints [0] = yR - panel2D.getUserHeight (10);
		coordinates.setElement (0, abscissaPoints [0] - xR);
		coordinates.setElement (1, ordinatePoints [0] - yR);
		coordinates = R.pro (coordinates);
		abscissaPoints [0] = coordinates.getElement (0) + xR;
		ordinatePoints [0] = coordinates.getElement (1) + yR;

		abscissaPoints [1] = xR + panel2D.getUserWidth (5);
		ordinatePoints [1] = yR - panel2D.getUserHeight (30);
		coordinates.setElement (0, abscissaPoints [1] - xR);
		coordinates.setElement (1, ordinatePoints [1] - yR);
		coordinates = R.pro (coordinates);
		abscissaPoints [1] = coordinates.getElement (0) + xR;
		ordinatePoints [1] = coordinates.getElement (1) + yR;

		abscissaPoints [2] = xR - panel2D.getUserWidth (5);
		ordinatePoints [2] = yR - panel2D.getUserHeight (30);
		coordinates.setElement (0, abscissaPoints [2] - xR);
		coordinates.setElement (1, ordinatePoints [2] - yR);
		coordinates = R.pro (coordinates);
		abscissaPoints [2] = coordinates.getElement (0) + xR;
		ordinatePoints [2] = coordinates.getElement (1) + yR;

		GeneralPath p2 = Tools.getPolygon2D (abscissaPoints, ordinatePoints, nPoints);
		g2.fill (p2);

		// x' and z axes...
		xR = 0d;
		yR = 0d;
		height = r.getHeight () - (r.getHeight () / 5);
		width = (r.getWidth () / 2) - (r.getWidth () / 10);
		xx = xR + width;
		yx = yR;
		xy = xR;
		yy = yR + height;
		sh1 = new Line2D.Double (xR, yR, xx, yx);
		sh2 = new Line2D.Double (xR, yR, xy, yy);
		g2.setColor (Color.black);
		g2.drawString ("O", (float) (xR - panel2D.getUserWidth (3)), (float) (yR - panel2D.getUserHeight (15)));
		g2.draw (sh2);
		g2.drawString ("z", (float) (xy - panel2D.getUserWidth (10)), (float) yy);

		if (mecaTree.isWindThrow ()) {
			g2.setColor (Color.red);
			g2.drawString (Translator.swap ("MecaPTrunkShape.windThrow"), (float) (xy - panel2D.getUserWidth (20)), (float) (yy + panel2D.getUserHeight (30)));
		} else	if (mecaTree.isStemBreakage ()) {
			g2.setColor (Color.blue);
			g2.drawString (Translator.swap ("MecaPTrunkShape.stemBreakage"), (float) (xy - panel2D.getUserWidth (20)), (float) (yy + panel2D.getUserHeight (30)));
		} else {
			g2.setColor (Color.green);
			g2.drawString (Translator.swap ("MecaPTrunkShape.noDamage"), (float) (xy - panel2D.getUserWidth (20)), (float) (yy + panel2D.getUserHeight (30)));
		}

		g2.setColor (Color.red);
		g2.draw (sh1);
		g2.drawString ("x'", (float) xx, (float) (yx - panel2D.getUserHeight (15)));
		xx = xR - width;
		yx = yR;
		sh1 = new Line2D.Double (xR, yR, xx, yx);
		g2.draw (sh1);

		// for the GU...
		double hTopGU = 0.0;
		for (Iterator i = mecaTree.getMecaGUs ().iterator (); i.hasNext ();) {
			MecaGU gu = (MecaGU) i.next ();
			hTopGU += gu.getHeight ();
			if (hTopGU > mecaTree.getCrownBaseHeight ()) {
				drawCrownGU (g2, r, gu);
			}
			drawGU (g2, r, gu);
		}

		g2.setColor (Color.black);
		xx = - panel2D.getUserWidth (6);
		yx = mecaTree.getHeight ();
		xy = 0d;
		yy = yx;
		sh1 = new Line2D.Double (xx, yx, xy, yy);
		g2.draw (sh1);
		g2.drawString ("H = " + String.valueOf ( (int) yx) + " m", (float) (- panel2D.getUserWidth (60)), (float) (yx - panel2D.getUserWidth (2)));

		updateToolTipText ();
	}

	/**
	 * Select mecaGUs.
	 */
	public JPanel select (Rectangle.Double r, boolean more) {
		JPanel infoPanel = null;
		Map panels = new Hashtable ();
		for (Iterator i = mecaTree.getMecaGUs ().iterator (); i.hasNext ();) {
			MecaGU gu = (MecaGU) i.next ();
			double xg = gu.getCg ().getElement (0) - mecaTree.getX ();
			double yg = gu.getCg ().getElement (1) - mecaTree.getY ();
			double zg = gu.getCg ().getElement (2) - mecaTree.getZ ();

			double angleRadian = -angle * Math.PI / 180;
			MecaVector nrx = new MecaVector (2);
			nrx.setElement (0, Math.cos (angleRadian));
			nrx.setElement (1, Math.sin (angleRadian));

			MecaVector nry = new MecaVector (2);
			nry.setElement (0, -nrx.getElement (1));
			nry.setElement (1, nrx.getElement (0));

			MecaMatrix R = new MecaMatrix (2);
			R.setColumn (0, nrx);
			R.setColumn (1, nry);
			R = R.transpose();

			MecaVector coordinates = new MecaVector (2);
			coordinates.setElement (0, xg);
			coordinates.setElement (1, yg);
			coordinates = R.pro (coordinates);
			xg = coordinates.getElement (0);
			yg = coordinates.getElement (1);

			Point.Double p = new Point.Double (xg, zg);
			if (r.contains (p)) {
					panels.put (""+gu.getId (),
							Tools.getIntrospectionPanel (gu));
							//Tools.getIntrospectionPanel (mecaTree.getGU (gu.getId ())));
			}
		}

		if (!panels.isEmpty ()) {
			infoPanel = new JPanel (new BorderLayout ());
			JTabbedPane tabPane = new JTabbedPane ();

			Iterator keys = panels.keySet ().iterator ();
			Iterator values = panels.values ().iterator ();

			while (keys.hasNext () && values.hasNext ()) {
				String key = (String) keys.next ();
				JComponent value = (JComponent) values.next ();
				tabPane.addTab (key, value);
			}

			infoPanel.add (tabPane, BorderLayout.CENTER);
		}
		return infoPanel;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		//0. Buttons for stress and moment representation
		JPanel mecaPart = new JPanel (new SmartFlowLayout (FlowLayout.CENTER));
		stress = new JButton (Translator.swap ("MecaPTrunkShape.stressDistribution"));
		stress.addActionListener (this);
		moment = new JButton (Translator.swap ("MecaPTrunkShape.momentContribution"));
		moment.addActionListener (this);
		mecaPart.add (stress);
		mecaPart.add (new JLabel ("            "));
		mecaPart.add (moment);

		this.add (mecaPart, BorderLayout.NORTH);

		// 1. Viewer panel2D
		scrollPane = new JScrollPane (panel2D);

		scrollPane.setPreferredSize (new Dimension (400, 400));	// fc - 31.1.2003

		scrollPane.getViewport ().putClientProperty
              ("EnableWindowBlit", Boolean.TRUE);	// faster

		this.add (scrollPane, BorderLayout.CENTER);

		// 2. Slider to choose plane view angle of trunk shape
		JPanel sliderPart = new JPanel ();
		sliderPart.setLayout (new BoxLayout (sliderPart, BoxLayout.Y_AXIS));

		JPanel sliderText = new JPanel (new SmartFlowLayout (FlowLayout.CENTER));
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
        JLabel sliderLabel = new JLabel ("      " + Translator.swap ("MecaPTrunkShape.sliderLabel")+" : ", JLabel.CENTER);
		angleText = new JTextField (3);
		angleText.addActionListener (this);
		sliderText.add (help);
		sliderText.add (sliderLabel);
		sliderText.add (angleText);
        sliderText.setBorder (BorderFactory.createEmptyBorder(5,0,0,0));

		angleSlider  = new JSlider (JSlider.HORIZONTAL, -180, 180, 0);
		angleText.setText (""+angleSlider.getValue ());
		angleSlider.addChangeListener (this);
        angleSlider.setMajorTickSpacing (45);
        angleSlider.setMinorTickSpacing (15);
        angleSlider.setPaintTicks (true);
        angleSlider.setPaintLabels (true);
        angleSlider.setBorder (BorderFactory.createEmptyBorder(5,10,5,10));

		/*JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (help);*/

		sliderPart.add (sliderText);
		sliderPart.add (angleSlider);
		//sliderPart.add (pControl);
		this.add (sliderPart, BorderLayout.SOUTH);

	}

	//------------------------------------------>
	// Drafts

	public void drawGUOld (Graphics2D g2, Rectangle.Double r, MecaGU gu) {
		int nPoints = 4;
		double [] abscissaPoints = new double [nPoints];
		double [] ordinatePoints = new double [nPoints];
		double zoomRadius = 4d;
		double xmt, ymt, zmt, xp, yp, zp, xc, yc, zc;
		xmt = mecaTree.getX ();
		ymt = mecaTree.getY ();
		zmt = mecaTree.getZ ();
		if (gu.previousGU () != null) {
			xp = gu.previousGU ().getXTop () - xmt;
			yp = gu.previousGU ().getYTop () - ymt;
			zp = gu.previousGU ().getZTop () - zmt;
		} else {
			xp = 0d;
			yp = 0d;
			zp = 0d;
		}
		xc = gu.getXTop () - xmt;
		yc = gu.getYTop () - ymt;
		zc = gu.getZTop () - zmt;

		abscissaPoints [0] = xc - (gu.getRadius () * zoomRadius);
		ordinatePoints [0] = zc;
		abscissaPoints [1] = xc + (gu.getRadius () * zoomRadius);
		ordinatePoints [1] = zc;
		abscissaPoints [2] = xp + (gu.getRadius () * zoomRadius);
		ordinatePoints [2] = zp;
		abscissaPoints [3] = xp - (gu.getRadius () * zoomRadius);
		ordinatePoints [3] = zp;

		GeneralPath p2 = Tools.getPolygon2D (abscissaPoints, ordinatePoints, nPoints);
		double abscissaBase = xp;
		double ordinateBase = zp;
		double abscissaTop = xc;
		double ordinateTop = zc;
		Shape sh = new Line2D.Double (abscissaBase, ordinateBase, abscissaTop, ordinateTop);

		Rectangle2D bBox = p2.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.setColor (new Color (150, 75, 0));
			g2.fill (p2);
			g2.setColor (new Color (220, 110, 0));
			g2.draw (sh);
		}

	}

	//------------------------------------------>
	// end of class

}




