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
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.Iterator;

import javax.swing.Box;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
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
 * MecaDamageHisto - Interface for view trees damages
 * according to abscissa H/Dbh class and ordinate Number in H/Dbh class.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaDamageHisto extends AmapDialog implements ActionListener, Drawer {
//checked for c4.1.1_08 - fc - 3.2.2003

	private MecaPanel2D panel2D;
	protected JPanel mainPanel;
	protected JScrollPane scrollPane;

	private JTextField xStepField;
	private JTextField yStepField;
	private JRadioButton rdHorizontal;
	private JRadioButton rdVertical;
	private ButtonGroup rdGroup;
	private JButton export;
	private JButton close;

	private MecaProcess mecaProcess;
	private double xMin;
	private int yMin;
	private double xMax;
	private int yMax;
	private double axisRate;
	private double xStep;
	private int yStep;
	private double [] xTab;
	private double [] xTabNoDamage;
	private double [] xTabWindthrow;
	private double [] xTabBreakage;
	private int nbXClass;
	private double xFirstMark;
	private double xLastMark;
	private int [] xClassEff;
	private int [] xClassEffNoDamage;
	private int [] xClassEffWindthrow;
	private int [] xClassEffBreakage;
	private boolean horizontalMode;
	private String type;
	private String xTitle;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaDamageHisto");
	}


	/**
	 * Constructor.
	 */
	public MecaDamageHisto (AmapDialog parent, MecaProcess mecaProcess, String type) {
		super (parent);

		this.mecaProcess = mecaProcess;
		this.type = type;
		setTitle (Translator.swap ("MecaDamageHisto") + " : " + type);

		double x, y, width, height;
		Rectangle.Double r2 = null;

		xMin = Double.MAX_VALUE;
		xMax = Double.MIN_VALUE;
		int nbTrees = mecaProcess.getTreeIds ().size ();
		xTab = new double [nbTrees];
		xTabNoDamage = new double [nbTrees];
		xTabWindthrow = new double [nbTrees];
		xTabBreakage = new double [nbTrees];
		int nt = 0;
		for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
			Integer integerId = (Integer) i.next ();
			MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);

			x = 0d;
			if (type.equals ("N = f(Slenderness)")) {
				xTitle = "Slenderness";
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (type.equals ("N = f(CrownRatio)")) {
				xTitle = "CrownRatio (%)";
				double num = mt.getCrownBaseHeight ();
				double den = mt.getHeight ();
				x = 100 * (den - num) / den;
				xStep = 5d;
			} else if (type.equals ("N = f(CrDensity)")) {
				xTitle = "CrDens (kg/m3)";
				x = mt.getCrownDensity ();
				xStep = 0.5d;
			} else if (type.equals ("N = f(CArea/SArea)")) {
				xTitle = "CArea / SArea";
				double num = mt.getCrownArea ();
				double r130 = (mt.getDbh () / 200d);
				double den = Math.PI * Math.pow (r130, 2);
				x = num / den;
				xStep = 200d;
			} else if (type.equals ("N = f(SL/CR)")) {
				xTitle = "SL / CR";
				double num = mt.getHeight ();
				double den = mt.getDbh ();
				double sl = 100d * num / den;
				num = mt.getCrownBaseHeight ();
				den = mt.getHeight ();
				double cr = (den - num) / den;
				x = sl / cr;
				xStep = 15d;
			} else if (type.equals ("N = f(HsmRatio)")) {
				xTitle = "HsmRatio (%)";
				double num = mt.getHeightBreakage ();
				double den = mt.getHeight ();
				x = 100d * num / den;
				xStep = 5d;
			} else if (type.equals ("N = f(Dbh)")) {
				xTitle = "Dbh (cm)";
				x = mt.getDbh ();
				xStep = 5d;
			} else if (type.equals ("N = f(Height)")) {
				xTitle = "Height (m)";
				x = mt.getHeight ();
				xStep = 2d;
			} else if (type.equals ("N = f(Age)")) {
				xTitle = "Age (year)";
				x = mt.getAge ();
				xStep = 5d;
			}

			if (x <= xMin) {
				xMin = x;
			}
			if (x >= xMax) {
				xMax = x;
			}
			xTab [nt] = x;
			xTabWindthrow [nt] = Double.MAX_VALUE;
			xTabBreakage [nt] = Double.MAX_VALUE;
			xTabNoDamage [nt] = Double.MAX_VALUE;
			nt++;
		}

		xFirstMark = ((int) (xMin / xStep)) * xStep;
		xLastMark = xFirstMark;
		nbXClass = 0;
		while (xLastMark < xMax) {
			nbXClass ++;
			xLastMark += xStep;
		}
		width = xLastMark - xFirstMark;
		xClassEff = new int [nbXClass];
		double xMark = xFirstMark;
		double xNextMark;
		yStep = 10;
		yMin = 0;
		yMax = Integer.MIN_VALUE;
		for (int xClass=0; xClass<nbXClass; xClass++) {
			xNextMark = xMark + xStep;
			for (nt=0; nt<nbTrees; nt++) {
				if (xTab [nt] >= xMark && xTab [nt] < xNextMark) {
					xClassEff [xClass] ++;
				}
			}
			if (xClassEff [xClass] >= yMax) {
				yMax = xClassEff [xClass];
			}
			xMark = xNextMark;
		}

		height = (double) (yMax - yMin);
		axisRate = width / height;
		height *= axisRate;
		x = xFirstMark - width / 10d;
		y = yMin;
		width +=  width / 5d;
		height +=  height / 5d;
		r2 = new Rectangle.Double (x, y, width, height);
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
				"histo", axisRate, yMin);

		//~ setBounds(470, 235, 420, 500);	// fc - 31.1.2003
		createUI ();
		
		setModal (false);
		pack ();
		show ();
	}

	// Synchronisation
	//
	private void rdGroupAction () {
		horizontalMode = rdGroup.getSelection ().equals (rdHorizontal.getModel ());
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (xStepField)) {
			if (type.equals ("N = f(Dbh)") || type.equals ("N = f(Slenderness)") || type.equals ("N = f(Age)")) {
				if (Check.isEmpty (xStepField.getText ()) || !Check.isInt (xStepField.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageHisto.stepIsNotAnInteger"),
							Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				xStep = (double) Check.intValue (xStepField.getText ());
			} else {//else if (type.equals ("N = f(Height)") || type.equals ("N = f(CrownRatio)") || type.equals ("N = f(CrDensity)")) {
				if (Check.isEmpty (xStepField.getText ()) || !Check.isDouble (xStepField.getText ())) {
					JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageHisto.stepIsNotANumber"),
							Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
					return;
				}
				xStep = Check.doubleValue (xStepField.getText ());
			}

			/*if (Check.isEmpty (xStepField.getText ()) || !Check.isInt (xStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageHisto.stepIsNotAnInteger"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			xStep = (double) Check.intValue (xStepField.getText ());*/

			double x, y, width, height;
			Rectangle.Double r2 = null;

			xFirstMark = ((int) (xMin / xStep)) * xStep;
			xLastMark = xFirstMark;
			nbXClass = 0;
			while (xLastMark < xMax) {
				nbXClass ++;
				xLastMark += xStep;
			}
			width = xLastMark - xFirstMark;
			xClassEff = new int [nbXClass];
			double xMark = xFirstMark;
			double xNextMark;
			yStep = 10;
			yMin = 0;
			yMax = Integer.MIN_VALUE;
			for (int xClass=0; xClass<nbXClass; xClass++) {
				xNextMark = xMark + xStep;
				for (int nt=0; nt<xTab.length; nt++) {
					if (xTab [nt] >= xMark && xTab [nt] < xNextMark) {
						xClassEff [xClass] ++;
					}
				}
				if (xClassEff [xClass] >= yMax) {
					yMax = xClassEff [xClass];
				}
				xMark = xNextMark;
			}

			height = (double) (yMax - yMin);
			axisRate = width / height;
			height *= axisRate;
			x = xFirstMark - width / 10d;
			y = yMin;
			width +=  width / 5d;
			height += height / 5d;
			r2 = new Rectangle.Double (x, y, width, height);

			r2 = panel2D.addMarginToUserBounds (r2, panel2D.getBounds ());
			panel2D.setUserBounds (r2);
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (yStepField)) {
			if (Check.isEmpty (yStepField.getText ()) || !Check.isInt (yStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaDamageHisto.stepIsNotAnInteger"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			yStep = Check.intValue (yStepField.getText ());
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (rdHorizontal) || evt.getSource ().equals (rdVertical)) {
			rdGroupAction ();
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (export)) {
				exportHisto ();

		} else if (evt.getSource ().equals (close)) {
				dispose ();
		}

	}

	// Export Histo to Log.
	//
	private void exportHisto () {
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
		expstr += "\nHistogramme pour analyse des d�gats : " + type;
		expstr += "\n\nValMin\tClasse\tNb WT\tNb SB\tNb ND";

		double xMark = xFirstMark;
		for (int xClass=0; xClass<nbXClass; xClass++) {
			expstr += "\n"+ xMark + "\t" + xMark + "-" + (xMark+xStep) + "\t";
			expstr += xClassEffWindthrow [xClass] + "\t";
			expstr += xClassEffBreakage [xClass] + "\t";
			expstr += xClassEffNoDamage [xClass] + "\t";

			xMark += xStep;
		}
		expstr += "\n"+ xMark;
		expstr += "\n\n<<*****************************************************************************>>\n\n";
		Log.println (expstr);
	}

	// Dispose box.
	//
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
		String toolTipText = Translator.swap ("MecaDamageHisto.information");
		panel2D.setToolTipText (toolTipText);
	}

	public int getPanel2DXMargin () {return Panel2D.X_MARGIN_IN_PIXELS;}

	public int getPanel2DYMargin () {return Panel2D.Y_MARGIN_IN_PIXELS;}

	/**
	 * Draw mecaTrees.
	 */
	public void draw (Graphics g, Rectangle.Double r) {
		Graphics2D g2 = (Graphics2D) g;

		// Graph axes
		double height = (double) (yMax - yMin);
		height *= axisRate;
		double width = xLastMark - xFirstMark;
		double xmin = xFirstMark - width / 10d;
		double ymin = yMin;
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
		g2.drawString (Translator.swap ("MecaDamageHisto.yStep"), (float) (xy - panel2D.getUserWidth (20)), (float) yy);

		// axes marks
		// H/Dbh axis : one mark every xStep cm
		// Numbers axis : one mark every yStep m
		g2.setColor (Color.black);
		double xMark = xFirstMark;
		double doubleYMark = ymin - panel2D.getUserHeight (5);
		for (int xClass=0; xClass<nbXClass+1; xClass++) {
			sh2 = new Line2D.Double (xMark, ymin, xMark, doubleYMark);
			g2.draw (sh2);
			if (type.equals ("N = f(Dbh)") || type.equals ("N = f(Slenderness)") || type.equals ("N = f(Age)")) {
				g2.drawString (String.valueOf ( (int) xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			} else {//if (type.equals ("N = f(Height)") || type.equals ("N = f(CrownRatio)")) {
				g2.drawString (String.valueOf (xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			}

			//g2.drawString (String.valueOf ( (int) xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			xMark += xStep;
		}

		int yMark = yMin;
		xMark = xmin - panel2D.getUserHeight (5);
		int yLastMark = yMax;
		while (yMark <= yLastMark) {
			doubleYMark = (double) yMark;
			height = doubleYMark - yMin;
			height *= axisRate;
			doubleYMark = yMin + height;
			sh1 = new Line2D.Double (xmin, doubleYMark, xMark, doubleYMark);
			g2.draw (sh1);
			g2.drawString (String.valueOf (yMark), (float) (xmin - panel2D.getUserWidth (25)), (float) (doubleYMark - panel2D.getUserHeight (10)));
			yMark += yStep;
		}

		// Graph legend
		double xmiddle = (xx + xmin) / 2d;
		g2.setColor (Color.green);
		g2.drawString (Translator.swap ("MecaDamageHisto.noDamage"), (float) (xmiddle - panel2D.getUserWidth (30)), (float) (yy + panel2D.getUserHeight (20)));
		g2.setColor (Color.red);
		g2.drawString (Translator.swap ("MecaDamageHisto.windthrow"), (float) (xmiddle - panel2D.getUserWidth (70)), (float) (yy));
		g2.setColor (Color.blue);
		g2.drawString (Translator.swap ("MecaDamageHisto.breakage"), (float) (xmiddle + panel2D.getUserWidth (30)), (float) (yy));

		// Histo bars
		if (mecaProcess.isAnalized ()) {
			int ntnd = 0;
			int ntwt = 0;
			int ntbk = 0;
			for (Iterator i = mecaProcess.getTreeIds ().iterator (); i.hasNext ();) {
				Integer integerId = (Integer) i.next ();
				MecaTree mt = (MecaTree) mecaProcess.getTreeId_MecaTree ().get (integerId);

				double x = 0d;
				if (type.equals ("N = f(Slenderness)")) {
					double num = mt.getHeight ();
					double den = mt.getDbh ();
					x = 100d * num / den;
				} else if (type.equals ("N = f(CrownRatio)")) {
					double num = mt.getCrownBaseHeight ();
					double den = mt.getHeight ();
					x = 100 * (den - num) / den;
				} else if (type.equals ("N = f(CrDensity)")) {
					x = mt.getCrownDensity ();
				} else if (type.equals ("N = f(CArea/SArea)")) {
					double num = mt.getCrownArea ();
					double r130 = (mt.getDbh () / 200d);
					double den = Math.PI * Math.pow (r130, 2);
					x = num / den;
				} else if (type.equals ("N = f(SL/CR)")) {
					double num = mt.getHeight ();
					double den = mt.getDbh ();
					double sl = 100d * num / den;
					num = mt.getCrownBaseHeight ();
					den = mt.getHeight ();
					double cr = (den - num) / den;
					x = sl / cr;
				} else if (type.equals ("N = f(HsmRatio)")) {
					double num = mt.getHeightBreakage ();
					double den = mt.getHeight ();
					x = 100d * num / den;
				} else if (type.equals ("N = f(Dbh)")) {
					x = mt.getDbh ();
				} else if (type.equals ("N = f(Height)")) {
					x = mt.getHeight ();
				} else if (type.equals ("N = f(Age)")) {
					x = mt.getAge ();
				}

				if (mt.isWindThrow ()) {
					xTabWindthrow [ntwt] = x;
					ntwt++;
				} else if (mt.isStemBreakage ()) {
					xTabBreakage [ntbk] = x;
					ntbk++;
				} else {
					xTabNoDamage [ntnd] = x;
					ntnd++;
				}
			}

			xClassEffNoDamage = new int [nbXClass];
			xClassEffWindthrow = new int [nbXClass];
			xClassEffBreakage = new int [nbXClass];
			xMark = xFirstMark;
			double xNextMark;
			for (int xClass=0; xClass<nbXClass; xClass++) {
				xNextMark = xMark + xStep;
				for (int nt=0; nt<xTab.length; nt++) {
					if (xTabWindthrow [nt] >= xMark && xTabWindthrow [nt] < xMark + xStep) {
						xClassEffWindthrow [xClass] ++;
					}
					if (xTabBreakage [nt] >= xMark && xTabBreakage [nt] < xMark + xStep) {
						xClassEffBreakage [xClass] ++;
					}
					if (xTabNoDamage [nt] >= xMark && xTabNoDamage [nt] < xMark + xStep) {
						xClassEffNoDamage [xClass] ++;
					}
				}
				xMark = xNextMark;
			}
		}

		xMark = xFirstMark;
		for (int xClass=0; xClass<nbXClass; xClass++) {
			// total bar
			double x = xMark;
			double y = (double) yMin;
			width = xStep;
			height = (double) xClassEff [xClass];
			height *= axisRate;
			sh1 = new Rectangle2D.Double (x, y, width, height);
			if (!mecaProcess.isAnalized ()) {
				g2.setColor (Color.green);
				g2.fill (sh1);
			} else {
				if (horizontalMode) {
					width =  xStep / 3d;
					// windthrow bar
					height = (double) xClassEffWindthrow [xClass];
					height *= axisRate;
					sh2 = new Rectangle2D.Double (x, y, width, height);
					g2.setColor (Color.red);
					g2.fill (sh2);

					// breakage bar
					x += width;
					height = (double) xClassEffBreakage [xClass];
					height *= axisRate;
					sh2 = new Rectangle2D.Double (x, y, width, height);
					g2.setColor (Color.blue);
					g2.fill (sh2);

					// no damage bar
					x += width;
					height = (double) xClassEffNoDamage [xClass];
					height *= axisRate;
					sh2 = new Rectangle2D.Double (x, y, width, height);
					g2.setColor (Color.green);
					g2.fill (sh2);
				} else {
					// windthrow bar
					height = (double) xClassEffWindthrow [xClass];
					height *= axisRate;
					sh2 = new Rectangle2D.Double (x, y, width, height);
					g2.setColor (Color.red);
					g2.fill (sh2);

					// breakage bar
					y += height;
					height = (double) xClassEffBreakage [xClass];
					height *= axisRate;
					sh2 = new Rectangle2D.Double (x, y, width, height);
					g2.setColor (Color.blue);
					g2.fill (sh2);

					// no damage bar
					y += height;
					height = (double) xClassEffNoDamage [xClass];
					height *= axisRate;
					sh2 = new Rectangle2D.Double (x, y, width, height);
					g2.setColor (Color.green);
					g2.fill (sh2);
				}
			}

			// total bar
			g2.setColor (Color.black);
			g2.draw (sh1);

			xMark += xStep;
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

		// 1. Options panel
		Box part1 = Box.createVerticalBox ();

		JPanel pOptions1 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		pOptions1.add (new JWidthLabel (Translator.swap ("MecaDamageHisto.graduatingStep")+"    ", 10));
		xStepField = new JTextField (2);
		xStepField.addActionListener (this);
		pOptions1.add (new JWidthLabel (xTitle+" :", 10));
		pOptions1.add (xStepField);
		if (type.equals ("N = f(Dbh)") || type.equals ("N = f(Slenderness)") ||
			type.equals ("N = f(CArea/SArea)") || type.equals ("N = f(SL/CR)") ||
			type.equals ("N = f(HsmRatio)") || type.equals ("N = f(Age)")) {
			xStepField.setText (""+ (int) xStep);
		} else if (type.equals ("N = f(Height)") || type.equals ("N = f(CrownRatio)") || type.equals ("N = f(CrDensity)")) {
			xStepField.setText (""+ xStep);
		}
		yStepField = new JTextField (2);
		yStepField.addActionListener (this);
		pOptions1.add (new JWidthLabel ("    " + Translator.swap ("MecaDamageHisto.yStep")+" :", 10));
		pOptions1.add (yStepField);
		yStepField.setText (""+ yStep);
		part1.add (pOptions1);

		JPanel pOptions2 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		rdGroup = new ButtonGroup ();
		pOptions2.add (new JWidthLabel (Translator.swap ("MecaDamageHisto.comparisonMode")+"    ", 10));
		rdHorizontal = new JRadioButton (Translator.swap ("MecaDamageHisto.horizontal"));
		rdHorizontal.addActionListener (this);
		rdGroup.add (rdHorizontal);
		pOptions2.add (rdHorizontal);
		pOptions2.add (new JWidthLabel ("    ", 10));
		rdVertical = new JRadioButton (Translator.swap ("MecaDamageHisto.vertical"));
		rdVertical.addActionListener (this);
		rdGroup.add (rdVertical);
		pOptions2.add (rdVertical);
		part1.add (pOptions2);
		rdGroup.setSelected (rdVertical.getModel (), true);
		rdGroupAction ();

		// 1. Viewer panel2D
		scrollPane = new JScrollPane (panel2D);

		scrollPane.setPreferredSize (new Dimension (500, 400));

		scrollPane.getViewport().putClientProperty
              ("EnableWindowBlit", Boolean.TRUE);	// faster
		mainPanel.add (scrollPane, BorderLayout.CENTER);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		export = new JButton (Translator.swap ("MecaDamageHisto.export"));
		export.addActionListener (this);
		pControl.add (export);
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		pControl.add (close);

		// set close as default (see AmapDialog)
		close.setDefaultCapable (true);
		getRootPane ().setDefaultButton (close);

		// layout parts
		getContentPane ().add (part1, BorderLayout.NORTH);
		getContentPane ().add (mainPanel, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

}




