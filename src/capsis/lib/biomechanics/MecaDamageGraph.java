/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.JTextField;

import capsis.commongui.util.Tools;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * MecaDamageGraph - Interface for view trees damages according to abscissa Dbh and ordinate H.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDamageGraph extends AmapDialog implements ActionListener, Drawer {
//checked for c4.1.1_08 - fc - 3.2.2003

	private MecaPanel2D panel2D;
	protected JPanel mainPanel;
	protected JScrollPane scrollPane;

	private JTextField xStepField;
	private JTextField yStepField;
	private JCheckBox idButton;
	private JButton export;
	private JButton close;

	private MecaProcess mecaProcess;
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
	private String [] listG;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDamageGraph");
	}


	/**
	 * Constructor.
	 */
	public MecaDamageGraph (AmapDialog parent, MecaProcess mecaProcess, String typeX, String typeY) {
		super (parent);

		this.mecaProcess = mecaProcess;
		xTitle = typeX;
		yTitle = typeY;
		type = yTitle + " = f(" + xTitle + ")";
		setTitle (Translator.swap ("MecaDamageGraph") + " : " + type);

		double x, y, width, height;
		Rectangle.Double r2 = null;

		xMin = Double.MAX_VALUE;
		yMin = Double.MAX_VALUE;
		xMax = Double.MIN_VALUE;
		yMax = Double.MIN_VALUE;

		//"CArea/SArea", "SxxMax", "BBM", "P_SB", "P_WT",

		listG = new String [14];
		listG [0] = "Dbh (cm)";
		listG [1] = "Height (m)";
		listG [2] = "CrownLength (m)";
		listG [3] = "Slenderness";
		listG [4] = "CrownRatio (%)";
		listG [5] = "CArea / SArea";
		listG [6] = "SxxMax (MPa)";
		listG [7] = "BBM (kN.m)";
		listG [8] = "P_SB";
		listG [9] = "P_WT";
		listG [10] = "HsmRatio (%)";
		listG [11] = "Dbh.CL (cm x m)";
		listG [12] = "H� (m x m)";
		listG [13] = "CrDens (kg/m3)";


		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);

			if (xTitle.equals (listG [0])) {
				x = mt.getDbh ();
				xStep = 5d;
			} else if (xTitle.equals (listG [1])) {
				x = mt.getHeight ();
				xStep = 2d;
			} else if (xTitle.equals (listG [2])) {
				x = mt.getHeight () - mt.getCrownBaseHeight ();
				xStep = 2d;
			} else if (xTitle.equals (listG [3])) {
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [4])) {
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				x = 100 * (den - num) / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [5])) {
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				x = num / den;
				xStep = 200d;
			} else if (xTitle.equals (listG [6])) {
				x = mt.getStressMax ();
				xStep = 5d;
			} else if (xTitle.equals (listG [7])) {
				x = mt.getEncastreMoment () / 1000d;
				xStep = 5d;
			} else if (xTitle.equals (listG [8])) {
				boolean isb = mt.isStemBreakage ();
				x = 0d;
				if (isb) {
					x = 1d;
				}
				xStep = 1d;
			} else if (xTitle.equals (listG [9])) {
				boolean iwt = mt.isWindThrow ();
				x = 0d;
				if (iwt) {
					x = 1d;
				}
				xStep = 1d;
			} else if (xTitle.equals (listG [10])) {
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [11])) {
				x = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
				xStep = 100d;
			} else if (xTitle.equals (listG [12])) {
				x = mt.getHeight () * mt.getHeight ();
				xStep = 100d;
			} else if (xTitle.equals (listG [13])) {
				x = mt.getCrownDensity ();
				xStep = 0.5d;
			} else {
				x = 0d;
				xStep = 1d;
			}

			if (yTitle.equals (listG [0])) {
				y = mt.getDbh ();
				yStep = 5d;
			} else if (yTitle.equals (listG [1])) {
				y = mt.getHeight ();
				yStep = 2d;
			} else if (yTitle.equals (listG [2])) {
				y = mt.getHeight () - mt.getCrownBaseHeight ();
				yStep = 2d;
			} else if (yTitle.equals (listG [3])) {
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				y = 100d * num / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [4])) {
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				y = 100 * (den - num) / den;
				yStep = 5d;

			} else if (yTitle.equals (listG [5])) {
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				y = num / den;
				yStep = 200d;
			} else if (yTitle.equals (listG [6])) {
				y = mt.getStressMax ();
				yStep = 5d;
			} else if (yTitle.equals (listG [7])) {
				y = mt.getEncastreMoment () / 1000d;
				yStep = 5d;
			} else if (yTitle.equals (listG [8])) {
				boolean isb = mt.isStemBreakage ();
				y = 0d;
				if (isb) {
					y = 1d;
				}
				yStep = 1d;
			} else if (yTitle.equals (listG [9])) {
				boolean iwt = mt.isWindThrow ();
				y = 0d;
				if (iwt) {
					y = 1d;
				}
				yStep = 1d;

			} else if (yTitle.equals (listG [10])) {
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				y = 100d * num / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [11])) {
				y = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
				yStep = 100d;
			} else if (yTitle.equals (listG [12])) {
				y = mt.getHeight () * mt.getHeight ();
				yStep = 100d;
			} else if (yTitle.equals (listG [13])) {
				y = mt.getCrownDensity ();
				yStep = 0.5d;
			} else {
				y = 0d;
				yStep = 1d;
			}

			if (x <= xMin) {
				xMin = x;
			}
			if (x >= xMax) {
				xMax = x;
			}
			if (y <= yMin) {
				yMin = y;
			}
			if (y >= yMax) {
				yMax = y;
			}
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

		if (type.equals ("Height (m) = f(Dbh (cm))")) {
			panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
					"graphHvsD", axisRate, yMin);
		} else if (type.equals ("CrownLength (m) = f(Height (m))")) {
			panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
					"graphCLvsH", axisRate, yMin);
		} else if (type.equals ("H� (m x m) = f(Dbh.CL (cm x m))")) {
			panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
					"graphH2vsDCL", axisRate, yMin);
		} else {
			panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
					"otherGraph", axisRate, yMin);
		}

		//~ setBounds(20, 235, 420, 500);	// fc - 31.1.2003
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
			/*if (Check.isEmpty (xStepField.getText ()) || !Check.isInt (xStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageGraph.stepIsNotAnInteger"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			xStep = (double) Check.intValue (xStepField.getText ());*/

			if (!xTitle.equals (listG [1]) && !xTitle.equals (listG [2])) {
				if (Check.isEmpty (xStepField.getText ()) || !Check.isInt (xStepField.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageGraph.stepIsNotAnInteger"),
							Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				xStep = (double) Check.intValue (xStepField.getText ());
			} else {
				if (Check.isEmpty (xStepField.getText ()) || !Check.isDouble (xStepField.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageGraph.stepIsNotANumber"),
							Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				xStep = Check.doubleValue (xStepField.getText ());
			}

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
			/*if (Check.isEmpty (yStepField.getText ()) || !Check.isInt (yStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageGraph.stepIsNotAnInteger"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			yStep = (double) Check.intValue (yStepField.getText ());*/

			if (!yTitle.equals (listG [1]) && !yTitle.equals (listG [2])) {
				if (Check.isEmpty (yStepField.getText ()) || !Check.isInt (yStepField.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageGraph.stepIsNotAnInteger"),
							Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				yStep = (double) Check.intValue (yStepField.getText ());
			} else {
				if (Check.isEmpty (yStepField.getText ()) || !Check.isDouble (yStepField.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageGraph.stepIsNotANumber"),
							Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				yStep = Check.doubleValue (yStepField.getText ());
			}

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

		} if (evt.getSource ().equals (idButton)) {
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} if (evt.getSource ().equals (export)) {
			exportGraph ();

		} if (evt.getSource ().equals (close)) {
			dispose ();
		}

	}

	// Write graph to Log.
	//
	private void exportGraph () {
		String expstr = "\n\n<<*****************************************************************************>>\n";
		expstr += "\nExport de donn�es pour " + mecaProcess.getStep ().getCaption ();
		double h, hb;
		if (mecaProcess.getConstraints ().standHeight.equals ("mean")) {
			h = mecaProcess.getMeanHeight ();
			hb = mecaProcess.getMeanCrownBaseHeight ();
		} else {
			h = mecaProcess.getDominantHeight ();
			hb = mecaProcess.getDominantCrownBaseHeight ();
		}

		String windLevel;
		if (mecaProcess.getConstraints ().windAt10m) {
			windLevel = "At 10 m above the ground";
		} else {
			windLevel = "At h (m) above the ground";
		}

		expstr += "\nCalculs faits pour location =\t" + mecaProcess.getConstraints ().location;
		expstr += "\nHauteurs (m) prises en compte : \th =\t" + h + "\thb =\t" + hb;
		expstr += "\nWind Level at stand edge =\t" + windLevel;
		expstr += "\nWindSpeedEdgeAt10m (m/s) =\t" + mecaProcess.getWindSpeedEdgeAt10m ();
		expstr += "\nWindSpeedEdgeAtH (m/s) =\t" + mecaProcess.getWindSpeedEdgeAtH ();
		expstr += "\nWindSpeedStandAtH (m/s) =\t" + mecaProcess.getWindSpeedStandAtH ();
		expstr += "\nWindSpeedStandAtHb (m/s) =\t" + mecaProcess.getWindSpeedStandAtHb ();
		expstr += "\nNuage de points pour analyse des d�gats : " + type;
		expstr += "\n\nType\t" + xTitle + "\t" + yTitle;
		//expstr += "\n\nType\tId\t" + xTitle + "\t" + yTitle+ "\tD\tH";

		double x, y;
		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);

			if (xTitle.equals (listG [0])) {
				x = mt.getDbh ();
				xStep = 5d;
			} else if (xTitle.equals (listG [1])) {
				x = mt.getHeight ();
				xStep = 2d;
			} else if (xTitle.equals (listG [2])) {
				x = mt.getHeight () - mt.getCrownBaseHeight ();
				xStep = 2d;
			} else if (xTitle.equals (listG [3])) {
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [4])) {
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				x = 100 * (den - num) / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [5])) {
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				x = num / den;
				xStep = 200d;
			} else if (xTitle.equals (listG [6])) {
				x = mt.getStressMax ();
				xStep = 5d;
			} else if (xTitle.equals (listG [7])) {
				x = mt.getEncastreMoment () / 1000d;
				xStep = 5d;
			} else if (xTitle.equals (listG [8])) {
				boolean isb = mt.isStemBreakage ();
				x = 0d;
				if (isb) {
					x = 1d;
				}
				xStep = 1d;
			} else if (xTitle.equals (listG [9])) {
				boolean iwt = mt.isWindThrow ();
				x = 0d;
				if (iwt) {
					x = 1d;
				}
				xStep = 1d;
			} else if (xTitle.equals (listG [10])) {
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [11])) {
				x = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
				xStep = 100d;
			} else if (xTitle.equals (listG [12])) {
				x = mt.getHeight () * mt.getHeight ();
				xStep = 100d;
			} else if (xTitle.equals (listG [13])) {
				x = mt.getCrownDensity ();
				xStep = 0.5d;
			} else {
				x = 0d;
				xStep = 1d;
			}

			if (yTitle.equals (listG [0])) {
				y = mt.getDbh ();
				yStep = 5d;
			} else if (yTitle.equals (listG [1])) {
				y = mt.getHeight ();
				yStep = 2d;
			} else if (yTitle.equals (listG [2])) {
				y = mt.getHeight () - mt.getCrownBaseHeight ();
				yStep = 2d;
			} else if (yTitle.equals (listG [3])) {
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				y = 100d * num / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [4])) {
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				y = 100 * (den - num) / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [5])) {
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				y = num / den;
				yStep = 200d;
			} else if (yTitle.equals (listG [6])) {
				y = mt.getStressMax ();
				yStep = 5d;
			} else if (yTitle.equals (listG [7])) {
				y = mt.getEncastreMoment () / 1000d;
				yStep = 5d;
			} else if (yTitle.equals (listG [8])) {
				boolean isb = mt.isStemBreakage ();
				y = 0d;
				if (isb) {
					y = 1d;
				}
				yStep = 1d;
			} else if (yTitle.equals (listG [9])) {
				boolean iwt = mt.isWindThrow ();
				y = 0d;
				if (iwt) {
					y = 1d;
				}
				yStep = 1d;
			} else if (yTitle.equals (listG [10])) {
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				y = 100d * num / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [11])) {
				y = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
				yStep = 100d;
			} else if (yTitle.equals (listG [12])) {
				y = mt.getHeight () * mt.getHeight ();
				yStep = 100d;
			} else if (yTitle.equals (listG [13])) {
				y = mt.getCrownDensity ();
				yStep = 0.5d;
			} else {
				y = 0d;
				yStep = 1d;
			}

			if (mt.isWindThrow ()) {
				expstr += "\nWT\t";
			} else if (mt.isStemBreakage ()) {
				expstr += "\nSB\t";
			} else {
				expstr += "\nND\t";
			}

			expstr += x + "\t" + y;

			/*if (y > 65d) {
				expstr += "\nN3\t";
			} else if (y < 30d) {
				expstr += "\nN1\t";
			} else {
				expstr += "\nN2\t";
			}

			expstr += mt.getId ()+ "\t" + x + "\t" + y + "\t" + mt.getDbh () + "\t" + mt.getHeight ();*/
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
		String toolTipText = Translator.swap ("MecaDamageGraph.information");
		panel2D.setToolTipText (toolTipText);
	}

	public int getPanel2DXMargin () {return Panel2D.X_MARGIN_IN_PIXELS;}

	public int getPanel2DYMargin () {return Panel2D.Y_MARGIN_IN_PIXELS;}

	/**
	 * Draw a mecaTree.
	 */
	public void drawTree (Graphics2D g2, Rectangle.Double r, MecaTree mt) {

		double width = panel2D.getUserWidth (4);
		double x, y;
		if (xTitle.equals (listG [0])) {
			x = mt.getDbh ();
			xStep = 5d;
		} else if (xTitle.equals (listG [1])) {
			x = mt.getHeight ();
			xStep = 2d;
		} else if (xTitle.equals (listG [2])) {
			x = mt.getHeight () - mt.getCrownBaseHeight ();
			xStep = 2d;
		} else if (xTitle.equals (listG [3])) {
			double num = mt.getHeight ();
			double den = mt.getDbh ();
			x = 100d * num / den;
			xStep = 5d;
		} else if (xTitle.equals (listG [4])) {
			double num = mt.getCrownBaseHeight ();
			double den = mt.getHeight ();
			x = 100 * (den - num) / den;
			xStep = 5d;
		} else if (xTitle.equals (listG [5])) {
			double num = mt.getCrownArea ();
			double r130 = (mt.getDbh () / 200d);
			double den = Math.PI * Math.pow (r130, 2);
			x = num / den;
			xStep = 200d;
		} else if (xTitle.equals (listG [6])) {
			x = mt.getStressMax ();
			xStep = 5d;
		} else if (xTitle.equals (listG [7])) {
			x = mt.getEncastreMoment () / 1000d;
			xStep = 5d;
		} else if (xTitle.equals (listG [8])) {
			boolean isb = mt.isStemBreakage ();
			x = 0d;
			if (isb) {
				x = 1d;
			}
			xStep = 1d;
		} else if (xTitle.equals (listG [9])) {
			boolean iwt = mt.isWindThrow ();
			x = 0d;
			if (iwt) {
				x = 1d;
			}
			xStep = 1d;
		} else if (xTitle.equals (listG [10])) {
			double num = mt.getHeightBreakage ();
			double den = mt.getHeight ();
			x = 100d * num / den;
			xStep = 5d;
		} else if (xTitle.equals (listG [11])) {
			x = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
			xStep = 100d;
		} else if (xTitle.equals (listG [12])) {
			x = mt.getHeight () * mt.getHeight ();
			xStep = 100d;
		} else if (xTitle.equals (listG [13])) {
			x = mt.getCrownDensity ();
			xStep = 0.5d;
		} else {
			x = 0d;
			xStep = 1d;
		}

		if (yTitle.equals (listG [0])) {
			y = mt.getDbh ();
			yStep = 5d;
		} else if (yTitle.equals (listG [1])) {
			y = mt.getHeight ();
			yStep = 2d;
		} else if (yTitle.equals (listG [2])) {
			y = mt.getHeight () - mt.getCrownBaseHeight ();
			yStep = 2d;
		} else if (yTitle.equals (listG [3])) {
			double num = mt.getHeight ();
			double den = mt.getDbh ();
			y = 100d * num / den;
			yStep = 5d;
		} else if (yTitle.equals (listG [4])) {
			double num = mt.getCrownBaseHeight ();
			double den = mt.getHeight ();
			y = 100 * (den - num) / den;
			yStep = 5d;
		} else if (yTitle.equals (listG [5])) {
			double num = mt.getCrownArea ();
			double r130 = (mt.getDbh () / 200d);
			double den = Math.PI * Math.pow (r130, 2);
			y = num / den;
			yStep = 200d;
		} else if (yTitle.equals (listG [6])) {
			y = mt.getStressMax ();
			yStep = 5d;
		} else if (yTitle.equals (listG [7])) {
			y = mt.getEncastreMoment () / 1000d;
			yStep = 5d;
		} else if (yTitle.equals (listG [8])) {
			boolean isb = mt.isStemBreakage ();
			y = 0d;
			if (isb) {
				y = 1d;
			}
			yStep = 1d;
		} else if (yTitle.equals (listG [9])) {
			boolean iwt = mt.isWindThrow ();
			y = 0d;
			if (iwt) {
				y = 1d;
			}
			yStep = 1d;
		} else if (yTitle.equals (listG [10])) {
			double num = mt.getHeightBreakage ();
			double den = mt.getHeight ();
			y = 100d * num / den;
			yStep = 5d;
		} else if (yTitle.equals (listG [11])) {
			y = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
			yStep = 100d;
		} else if (yTitle.equals (listG [12])) {
			y = mt.getHeight () * mt.getHeight ();
			yStep = 100d;
		} else if (yTitle.equals (listG [13])) {
			y = mt.getCrownDensity ();
			yStep = 0.5d;
		} else {
			y = 0d;
			yStep = 1d;
		}

		double height = y - yMin;
		height *= axisRate;
		y = yMin + height;

		Shape sh1 = new Ellipse2D.Double (x-width/2, y-width/2, width, width);
		g2.setColor (Color.green);
		if (mt.isWindThrow ()) {
			g2.setColor (Color.red);
		} else if (mt.isStemBreakage ()) {
			g2.setColor (Color.blue);
		}

		Rectangle2D bBox = sh1.getBounds2D ();
		if (r.intersects (bBox)) {
			g2.draw (sh1);
			g2.fill (sh1);
		}

		if (idButton.isSelected ()) {
			g2.setColor (Color.black);
			if (r.contains (new Point.Double (x, y))) {
				g2.drawString (String.valueOf (mt.getId ()), (float) x, (float) y);
			}
		}
	}

	/**
	 * Draw mecaTrees.
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;
		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);
			drawTree (g2, r, mt);
		}

		// Graph axes
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

		// axes marks
		// Dbh axis : one mark every xStep cm
		// H axis : one mark every yStep m
		g2.setColor (Color.black);
		double xMark = xFirstMark;
		double yMark = ymin - panel2D.getUserHeight (5);
		for (int i=0; i<nbXMark; i++) {
			sh2 = new Line2D.Double (xMark, ymin, xMark, yMark);
			g2.draw (sh2);

			//g2.drawString (String.valueOf ( (int) xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));

			if (!xTitle.equals (listG [1]) && !xTitle.equals (listG [2])) {
				g2.drawString (String.valueOf ( (int) xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			} else {
				g2.drawString (String.valueOf (xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			}

			/*if (type.equals ("Height = f(Dbh)") || type.equals ("H� = f(Dbh.CL)") || type.equals ("HsmRatio = f(Slenderness)")) {
				g2.drawString (String.valueOf ( (int) xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			} else if (type.equals ("CrownLength = f(Height)")) {
				g2.drawString (String.valueOf (xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			}*/
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

			//g2.drawString (String.valueOf ( (int) yMark), (float) (xmin - panel2D.getUserWidth (30)), (float) (doubleYMark - panel2D.getUserHeight (10)));

			if (!yTitle.equals (listG [1]) && !yTitle.equals (listG [2])) {
				g2.drawString (String.valueOf ( (int) yMark), (float) (xmin - panel2D.getUserWidth (30)), (float) (doubleYMark - panel2D.getUserHeight (10)));
			} else {
				g2.drawString (String.valueOf (yMark), (float) (xmin - panel2D.getUserWidth (30)), (float) (doubleYMark - panel2D.getUserHeight (10)));
			}

			//g2.drawString (String.valueOf (yMark), (float) (xmin - panel2D.getUserWidth (30)), (float) (doubleYMark - panel2D.getUserHeight (10)));
			yMark += yStep;
		}

		// Graph legend
		double xmiddle = (xx + xmin) / 2d;
		g2.setColor (Color.green);
		g2.drawString (Translator.swap ("MecaDamageGraph.noDamage"), (float) (xmiddle - panel2D.getUserWidth (20)), (float) (yy + panel2D.getUserHeight (20)));
		g2.setColor (Color.red);
		g2.drawString (Translator.swap ("MecaDamageGraph.windthrow"), (float) (xmiddle - panel2D.getUserWidth (70)), (float) (yy));
		g2.setColor (Color.blue);
		g2.drawString (Translator.swap ("MecaDamageGraph.breakage"), (float) (xmiddle + panel2D.getUserWidth (30)), (float) (yy));

		updateToolTipText ();
	}

	/**
	 * Select mecaTrees.
	 */
	public JPanel select (Rectangle.Double r, boolean ctrlIsDown) {
		JPanel infoPanel = null;
		Map panels = new Hashtable ();

		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);

			double x, y;
			if (xTitle.equals (listG [0])) {
				x = mt.getDbh ();
				xStep = 5d;
			} else if (xTitle.equals (listG [1])) {
				x = mt.getHeight ();
				xStep = 2d;
			} else if (xTitle.equals (listG [2])) {
				x = mt.getHeight () - mt.getCrownBaseHeight ();
				xStep = 2d;
			} else if (xTitle.equals (listG [3])) {
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [4])) {
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				x = 100 * (den - num) / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [5])) {
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				x = num / den;
				xStep = 200d;
			} else if (xTitle.equals (listG [6])) {
				x = mt.getStressMax ();
				xStep = 5d;
			} else if (xTitle.equals (listG [7])) {
				x = mt.getEncastreMoment () / 1000d;
				xStep = 5d;
			} else if (xTitle.equals (listG [8])) {
				boolean isb = mt.isStemBreakage ();
				x = 0d;
				if (isb) {
					x = 1d;
				}
				xStep = 1d;
			} else if (xTitle.equals (listG [9])) {
				boolean iwt = mt.isWindThrow ();
				x = 0d;
				if (iwt) {
					x = 1d;
				}
				xStep = 1d;
			} else if (xTitle.equals (listG [10])) {
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (xTitle.equals (listG [11])) {
				x = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
				xStep = 100d;
			} else if (xTitle.equals (listG [12])) {
				x = mt.getHeight () * mt.getHeight ();
				xStep = 100d;
			} else if (xTitle.equals (listG [13])) {
				x = mt.getCrownDensity ();
				xStep = 0.5d;
			} else {
				x = 0d;
				xStep = 1d;
			}

			if (yTitle.equals (listG [0])) {
				y = mt.getDbh ();
				yStep = 5d;
			} else if (yTitle.equals (listG [1])) {
				y = mt.getHeight ();
				yStep = 2d;
			} else if (yTitle.equals (listG [2])) {
				y = mt.getHeight () - mt.getCrownBaseHeight ();
				yStep = 2d;
			} else if (yTitle.equals (listG [3])) {
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				y = 100d * num / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [4])) {
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				y = 100 * (den - num) / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [5])) {
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				y = num / den;
				yStep = 200d;
			} else if (yTitle.equals (listG [6])) {
				y = mt.getStressMax ();
				yStep = 5d;
			} else if (yTitle.equals (listG [7])) {
				y = mt.getEncastreMoment () / 1000d;
				yStep = 5d;
			} else if (yTitle.equals (listG [8])) {
				boolean isb = mt.isStemBreakage ();
				y = 0d;
				if (isb) {
					y = 1d;
				}
				yStep = 1d;
			} else if (yTitle.equals (listG [9])) {
				boolean iwt = mt.isWindThrow ();
				y = 0d;
				if (iwt) {
					y = 1d;
				}
				yStep = 1d;
			} else if (yTitle.equals (listG [10])) {
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				y = 100d * num / den;
				yStep = 5d;
			} else if (yTitle.equals (listG [11])) {
				y = mt.getDbh () * (mt.getHeight () - mt.getCrownBaseHeight ());
				yStep = 100d;
			} else if (yTitle.equals (listG [12])) {
				y = mt.getHeight () * mt.getHeight ();
				yStep = 100d;
			} else if (yTitle.equals (listG [13])) {
				y = mt.getCrownDensity ();
				yStep = 0.5d;
			} else {
				y = 0d;
				yStep = 1d;
			}

			double height = y - yMin;
			height *= axisRate;
			y = yMin + height;
			Point.Double p = new Point.Double (x, y);
			if (r.contains (p)) {
				panels.put (""+mt.getId (),
						Tools.getIntrospectionPanel (mt));
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
			tabPane.setPreferredSize (new Dimension (200, 300));	// fc - 31.3.2001
			infoPanel.add (tabPane, BorderLayout.CENTER);
		}

		return infoPanel;
	}

	/**
	 * User interface definition.
	 */
	private void createUI () {
		mainPanel = new JPanel ();
		mainPanel.setLayout (new BorderLayout ());

		// 1. Options panel
		Box part1 = Box.createVerticalBox ();

		JPanel pOptions1 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		pOptions1.add (new JWidthLabel (Translator.swap ("MecaDamageGraph.graduatingStep")+"    ", 10));
		xStepField = new JTextField (2);
		xStepField.addActionListener (this);
		pOptions1.add (new JWidthLabel (xTitle+" :", 10));
		pOptions1.add (xStepField);

		//xStepField.setText (""+ (int) xStep);

		if (!xTitle.equals (listG [1]) && !xTitle.equals (listG [2])) {
			xStepField.setText (""+ (int) xStep);
		} else {
			xStepField.setText (""+ xStep);
		}

		/*if (type.equals ("Height = f(Dbh)") || type.equals ("H� = f(Dbh.CL)") || type.equals ("HsmRatio = f(Slenderness)")) {
			xStepField.setText (""+ (int) xStep);
		} else if (type.equals ("CrownLength = f(Height)")) {
			xStepField.setText (""+ xStep);
		}*/

		yStepField = new JTextField (2);
		yStepField.addActionListener (this);
		pOptions1.add (new JWidthLabel ("    "+yTitle+" :", 10));
		pOptions1.add (yStepField);

		//yStepField.setText (""+ (int) yStep);

		if (!yTitle.equals (listG [1]) && !yTitle.equals (listG [2])) {
			yStepField.setText (""+ (int) yStep);
		} else {
			yStepField.setText (""+ yStep);
		}

		//yStepField.setText (""+yStep);

		part1.add (pOptions1);

		JPanel pOptions2 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		idButton = new JCheckBox(Translator.swap ("MecaDamageGraph.MarkTreeIdentity"), false);
		idButton.addActionListener (this);
		pOptions2.add (idButton);
		part1.add (pOptions2);

		// 1. Viewer panel2D
		scrollPane = new JScrollPane (panel2D);

		scrollPane.setPreferredSize (new Dimension (400, 400));	// fc - 31.1.2003

		scrollPane.getViewport().putClientProperty
              ("EnableWindowBlit", Boolean.TRUE);	// faster
		mainPanel.add (scrollPane, BorderLayout.CENTER);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		export = new JButton (Translator.swap ("MecaDamageGraph.export"));
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

}




