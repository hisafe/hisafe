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
import java.text.NumberFormat;

import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JCheckBox;
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
 * MecaMomentHisto - Interface to view trees damages
 * according to abscissa H/Dbh class and ordinate Number in H/Dbh class.
 *
 * @author Ph. Ancelin - october 2001
 */
public class MecaMomentHisto extends AmapDialog implements ActionListener, Drawer {
//checked for c4.1.1_08 - fc - 3.2.2003

	private MecaPanel2D panel2D;
	protected JPanel mainPanel;
	protected JScrollPane scrollPane;

	private JTextField yStepField;
	private JCheckBox windForces;
	private JButton export;
	private JButton close;

	private MecaTree mecaTree;
	private int xMin;
	private double yMin;
	private int xMax;
	private double yMax;
	private double axisRate;
	private int xStep;
	private double yStep;
	private double xFirstMark;
	private double xLastMark;
	private String xTitle;
	private String yTitle;
	private double [][] contributionToMencastr;
	private double SCWindContribution;
	private double SWeightContribution;
	private double CWeightContribution;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaMomentHisto");
	}


	/**
	 * Constructors.
	 */
	public MecaMomentHisto (JDialog d, MecaTree mecaTree) {
		super (d);

		this.mecaTree = mecaTree;
		continueConstruction ();
	}

	public MecaMomentHisto (JFrame f, MecaTree mecaTree) {
		super (f);

		this.mecaTree = mecaTree;
		continueConstruction ();
	}

	public MecaMomentHisto (MecaTree mecaTree) {
		super ();

		this.mecaTree = mecaTree;
		continueConstruction ();
	}


	public void continueConstruction () {
		setTitle (Translator.swap ("MecaMomentHisto"));

		double x, y, width, height;
		Rectangle.Double r2 = null;

		xTitle = "Element";
		yTitle = "% Mencastr";
		xMin = 0;
		yMin = 0d;
		yMax = Double.MIN_VALUE;
		xStep = 1;
		yStep = 2d;

		int nbGus = mecaTree.getAge ();
		xMax = nbGus;
		contributionToMencastr = mecaTree.getContributionToMencastr ();
		double yCumul = 0d;
		SCWindContribution = 0d;
		SWeightContribution = 0d;
		CWeightContribution = 0d;

		for (int ngu=0; ngu<nbGus; ngu++) {
			yCumul = contributionToMencastr [ngu][1] + contributionToMencastr [ngu][2]
					+ contributionToMencastr [ngu][3] + contributionToMencastr [ngu][4];
			SCWindContribution += contributionToMencastr [ngu][2] + contributionToMencastr [ngu][4];
			SWeightContribution += contributionToMencastr [ngu][1];
			CWeightContribution += contributionToMencastr [ngu][3];

			if (yCumul >= yMax) {
				yMax = yCumul;
			}
		}

		xFirstMark = (double) (xMin);
		xLastMark = (double) (xMax);
		width = xLastMark - xFirstMark;
		height = yMax - yMin;
		axisRate = width / height;
		height *= axisRate;
		x = xFirstMark - width / 10d;
		y = yMin;
		width +=  width / 5d;
		height +=  height / 5d;
		r2 = new Rectangle.Double (x, y, width, height);
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
				"histo", axisRate, yMin);

		//~ setBounds(500, 235, 450, 500);	// fc - 31.1.2003
		createUI ();
		
		setModal (false);
		pack ();
		show ();
	}

	/**
	 * Manage gui events.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (yStepField)) {
			if (Check.isEmpty (yStepField.getText ()) || !Check.isDouble (yStepField.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("MecaMomentHisto.stepIsNotANumber"),
						Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			yStep = Check.doubleValue (yStepField.getText ());
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (windForces)) {
			panel2D.reset ();	// fc - 31.1.2003
			panel2D.repaint ();

		} else if (evt.getSource ().equals (export)) {
			exportHisto ();

		} else if (evt.getSource ().equals (close)) {
			dispose ();
		}
	}

	// Export histo in Log.
	//
	private void exportHisto () {
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
		expstr += "\nHistogramme des contributions � Mencastr par �l�ment pour arbre n� " + mecaTree.getId ();
		expstr += "\n\nElement\tS+C Wind\tS Weight\tC Weight";

		int nbGus = mecaTree.getAge ();
		for (int ngu=0; ngu<nbGus; ngu++) {
			expstr += "\n"+ (ngu+1) + "\t";
			expstr += (contributionToMencastr [ngu][2] + contributionToMencastr [ngu][4]) + "\t";
			expstr += contributionToMencastr [ngu][1] + "\t";
			expstr += contributionToMencastr [ngu][3] + "\t";
		}
		expstr += "\n\n<<*****************************************************************************>>\n\n";
		Log.println (expstr);
	}

	/**
	 * Dispose box.
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
		String toolTipText = Translator.swap ("MecaMomentHisto.information");
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
		double height = yMax - yMin;
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
		g2.drawString (yTitle, (float) (xy - panel2D.getUserWidth (20)), (float) yy);

		// axes marks
		// H/Dbh axis : one mark every xStep cm
		// Numbers axis : one mark every yStep m
		g2.setColor (Color.black);
		double xMark = xFirstMark;
		double doubleYMark = ymin - panel2D.getUserHeight (5);
		int nbGus = mecaTree.getAge ();
		for (int ngu=0; ngu<nbGus; ngu++) {
			sh2 = new Line2D.Double (xMark, ymin, xMark, doubleYMark);
			g2.draw (sh2);
			//g2.drawString (String.valueOf ( (int) xMark), (float) (((xMark+xStep)/2) - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			if ((ngu % 5) == 0) {
				g2.drawString (String.valueOf ( (int) xMark), (float) (xMark - panel2D.getUserWidth (5)), (float) (ymin- panel2D.getUserHeight (25)));
			}
			xMark += xStep;
		}
		sh2 = new Line2D.Double (xMark, ymin, xMark, doubleYMark);
		g2.draw (sh2);

		double yMark = yMin;
		xMark = xmin - panel2D.getUserHeight (5);
		double yLastMark = yMax;
		while (yMark <= yLastMark) {
			doubleYMark = yMark;
			height = yMark - yMin;
			height *= axisRate;
			doubleYMark = yMin + height;
			sh1 = new Line2D.Double (xmin, doubleYMark, xMark, doubleYMark);
			g2.draw (sh1);
			g2.drawString (String.valueOf (yMark), (float) (xmin - panel2D.getUserWidth (25)), (float) (doubleYMark - panel2D.getUserHeight (10)));
			yMark += yStep;
		}

		// Graph legend
		double xmiddle = (xx + xmin) / 2d;
		NumberFormat nf1 = NumberFormat.getInstance ();
		nf1.setMinimumFractionDigits (1);
		nf1.setMaximumFractionDigits (1);
		nf1.setGroupingUsed (false);

		g2.setColor (Color.blue);
		g2.drawString (Translator.swap ("MecaMomentHisto.treeWind") + " (" + nf1.format (SCWindContribution) + "%)",
				(float) (xmiddle - panel2D.getUserWidth (45)), (float) (yy + panel2D.getUserHeight (20)));
		g2.setColor (new Color (150, 75, 0));
		g2.drawString (Translator.swap ("MecaMomentHisto.stemWeight") + " (" + nf1.format (SWeightContribution) + "%)",
				(float) (xmiddle - panel2D.getUserWidth (90)), (float) (yy));
		g2.setColor (Color.green);
		g2.drawString (Translator.swap ("MecaMomentHisto.crownWeight") + " (" + nf1.format (CWeightContribution) + "%)",
				(float) (xmiddle + panel2D.getUserWidth (50)), (float) (yy));
		if (windForces.isSelected ()) {
			g2.setColor (Color.red);
			g2.drawString (Translator.swap ("MecaMomentHisto.windForces"),
					(float) (xmiddle - panel2D.getUserWidth (45)), (float) (yy - panel2D.getUserHeight (20)));
		}

		// Histo bars
		xMark = xFirstMark;
		Shape sh3;
		double xp = 0d;
		double yp = 0d;
		double xd = xMark + xStep / 2.0;
		double yd = (contributionToMencastr [0][2] + contributionToMencastr [0][4]) * (3*nbGus/4);
		yd *= axisRate;
		double widthP = panel2D.getUserWidth (5);

		for (int ngu=0; ngu<nbGus; ngu++) {
			// total bar
			double x = xMark;
			double y = yMin;
			width = (double) (xStep);
			height = contributionToMencastr [ngu][1] + contributionToMencastr [ngu][2]
					+ contributionToMencastr [ngu][3] + contributionToMencastr [ngu][4];
			height *= axisRate;
			sh1 = new Rectangle2D.Double (x, y, width, height);
			if (mecaTree.getMecaProcess ().isAnalized ()) {
				// treeWind bar
				height = contributionToMencastr [ngu][2] + contributionToMencastr [ngu][4];
				height *= axisRate;
				sh2 = new Rectangle2D.Double (x, y, width, height);
				g2.setColor (Color.blue);
				g2.fill (sh2);

				xp = x + width / 2.0;
				yp = ((height/axisRate) / (ngu+1)) * (3*nbGus/4);
				yp *= axisRate;

				// stemWeight bar
				y += height;
				height = contributionToMencastr [ngu][1];
				height *= axisRate;
				sh2 = new Rectangle2D.Double (x, y, width, height);
				g2.setColor (new Color (150, 75, 0));
				g2.fill (sh2);

				// crownWeight bar
				y += height;
				height = contributionToMencastr [ngu][3];
				height *= axisRate;
				sh2 = new Rectangle2D.Double (x, y, width, height);
				g2.setColor (Color.green);
				g2.fill (sh2);

				// force curve
				if (windForces.isSelected ()) {
					sh3 = new Ellipse2D.Double (xp-widthP/2, yp-widthP/2, widthP, widthP);
					g2.setColor (Color.red);
					g2.draw (sh3);
					g2.fill (sh3);
					sh3 = new Line2D.Double (xd, yd, xp, yp);
					g2.draw (sh3);
				}
				xd = xp;
				yd = yp;
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
		pOptions1.add (new JWidthLabel (Translator.swap ("MecaMomentHisto.graduatingStep")+" :", 10));
		yStepField = new JTextField (2);
		yStepField.addActionListener (this);
		pOptions1.add (yStepField);
		yStepField.setText (""+ yStep);
		part1.add (pOptions1);

		JPanel pOptions2 = new JPanel (new FlowLayout (FlowLayout.CENTER));
		windForces = new JCheckBox(Translator.swap ("MecaMomentHisto.showWindForces"), false);
		windForces.addActionListener (this);
		pOptions2.add (windForces);
		part1.add (pOptions2);

		// 1. Viewer panel2D
		scrollPane = new JScrollPane (panel2D);

		scrollPane.setPreferredSize (new Dimension (400, 400));	// fc - 31.1.2003

		scrollPane.getViewport().putClientProperty
              ("EnableWindowBlit", Boolean.TRUE);	// faster
		mainPanel.add (scrollPane, BorderLayout.CENTER);

		// 2. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.CENTER));
		export = new JButton (Translator.swap ("MecaMomentHisto.export"));
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


	/**
	 * Constructor.
	 */
/*	public MecaMomentHisto (MecaTree mecaTree) {
		super ();

		this.mecaTree = mecaTree;
		setTitle (Translator.swap ("MecaMomentHisto"));

		double x, y, width, height;
		Rectangle.Double r2 = null;

		xTitle = "Element";
		yTitle = "% Mencastr";
		xMin = 0;
		yMin = 0d;
		yMax = Double.MIN_VALUE;
		xStep = 1;
		yStep = 2d;

		int nbGus = mecaTree.getAge ();
		xMax = nbGus;
		contributionToMencastr = mecaTree.getContributionToMencastr ();
		double yCumul = 0d;
		SCWindContribution = 0d;
		SWeightContribution = 0d;
		CWeightContribution = 0d;

		for (int ngu=0; ngu<nbGus; ngu++) {
			yCumul = contributionToMencastr [ngu][1] + contributionToMencastr [ngu][2]
					+ contributionToMencastr [ngu][3] + contributionToMencastr [ngu][4];
			SCWindContribution += contributionToMencastr [ngu][2] + contributionToMencastr [ngu][4];
			SWeightContribution += contributionToMencastr [ngu][1];
			CWeightContribution += contributionToMencastr [ngu][3];

			if (yCumul >= yMax) {
				yMax = yCumul;
			}
		}

		xFirstMark = (double) (xMin);
		xLastMark = (double) (xMax);
		width = xLastMark - xFirstMark;
		height = yMax - yMin;
		axisRate = width / height;
		height *= axisRate;
		x = xFirstMark - width / 10d;
		y = yMin;
		width +=  width / 5d;
		height +=  height / 5d;
		r2 = new Rectangle.Double (x, y, width, height);
		panel2D = new MecaPanel2D (this, r2, getPanel2DXMargin (), getPanel2DYMargin (),
				"histo", axisRate, yMin);

		//~ setBounds(500, 235, 450, 500);	// fc - 31.1.2003
		createUI ();
		
		setModal (true);
		pack ();
		show ();
	}
*/



}




