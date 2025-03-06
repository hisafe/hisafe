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

import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.text.NumberFormat;

import javax.swing.JPanel;

import capsis.lib.rubberband.Rubberband;
import capsis.lib.rubberband.RubberbandRectangle;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import jeeb.lib.util.Translator;

/**
 * MecaPanel2D - To show coordinates in a Panel2D.
 *
 * @author Ph. Ancelin - november 2001
 */
public class MecaPanel2D extends Panel2D {
//checked for c4.1.1_08 - fc - 3.2.2003

	private boolean viewer;
	private boolean tracer;
	private boolean graphHvsD;
	private boolean graphCLvsH;
	private boolean graphH2vsDCL;
	private boolean graphHvsSxx;
	private boolean graphPercvsTime;
	private boolean histo;
	private double axisRate;
	private double ymin;

	static {
		Translator.addBundle("capsis.lib.biomechanics.MecaPanel2D");
	}


	/**
	 * Constructor.
	 */
	public MecaPanel2D (Drawer drawer,
					Rectangle.Double initialUserBounds,
					String type) {
		this (drawer, initialUserBounds, X_MARGIN_IN_PIXELS, Y_MARGIN_IN_PIXELS, type);
	}

	/**
	 * Constructor.
	 */
	public MecaPanel2D (Drawer drawer,
					Rectangle.Double initialUserBounds,
					int xMarginInPixels,
					int yMarginInPixels,
					String type) {
		this (drawer, initialUserBounds, xMarginInPixels, yMarginInPixels, false, type);
	}

	/**
	 * Constructor.
	 */
	public MecaPanel2D (Drawer drawer,
					Rectangle.Double initialUserBounds,
					int xMarginInPixels,
					int yMarginInPixels,
					String type,
					double axisRate,
					double ymin) {
		this (drawer, initialUserBounds, xMarginInPixels, yMarginInPixels, false, type);
		this.axisRate = axisRate;
		this.ymin = ymin;
	}

	/**
	 * Constructor.
	 */
	public MecaPanel2D (Drawer drawer,
					Rectangle.Double initialUserBounds,
					int xMarginInPixels,
					int yMarginInPixels,
					boolean wrapPanel,
					String type) {
		super (drawer, initialUserBounds, xMarginInPixels, yMarginInPixels, wrapPanel);
		viewer = false;
		tracer = false;
		graphHvsD = false;
		graphCLvsH = false;
		graphH2vsDCL = false;
		graphHvsSxx = false;
		graphPercvsTime = false;
		histo = false;
		if (type.equals ("viewer")) {
			viewer = true;
		}
		if (type.equals ("tracer")) {
			tracer = true;
		}
		if (type.equals ("graphHvsD")) {
			graphHvsD = true;
		}
		if (type.equals ("graphCLvsH")) {
			graphCLvsH = true;
		}
		if (type.equals ("graphH2vsDCL")) {
			graphH2vsDCL = true;
		}
		if (type.equals ("graphHvsSxx")) {
			graphHvsSxx = true;
		}
		if (type.equals ("graphPercvsTime")) {
			graphPercvsTime = true;
		}
		if (type.equals ("histo")) {
			histo = true;
		}
		axisRate = 1d;
		ymin = 0d;
	}

	public void setAxisRate (double ar) {axisRate = ar;}

	public boolean isTracer () {return tracer;}


	/**
	 * Show selection result component.
	 */
	public void show (JPanel pan) {
		super.show (pan);
		// fc - 31.1.2003 - spoils general dialog layout
		//~ if (viewer) {
			//~ infoDialog.setLocation(20, 98);
		//~ } else {
			//~ infoDialog.setBounds(540, 50, 450, 500);
		//~ }
	}

	/**
	 * Return info title.
	 */
	public String getInfoTitle () {
		//String title =
		if (viewer) {
			return Translator.swap ("MecaPanel2D.infoTitleTracer");
		} else {
		//if (tracer || graphHvsD || graphCLvsH || graphH2vsDCL || graphHsmvsSl || graphHvsSxx) {
			return Translator.swap ("MecaPanel2D.infoTitleInspector");
		}
		//return "Info";
	}

	/**
	 * MouseMotionListener method.
	 */
	public void mouseMoved (MouseEvent evt) {
		// mouse moves when Shift is pressed
		if (evt.isShiftDown ()) {
			int shift = 1;
			Rubberband rubberBand = new RubberbandRectangle (this);
			Point center = evt.getPoint ();
			rubberBand.anchor (new Point (center.x, center.y));
			rubberBand.end (new Point (center.x+shift, center.y+shift));

			Rectangle rect = rubberBand.lastBounds ();
			Rectangle.Double r = getUserRectangle (rect);
			double x = r.getX ();
			double y = r.getY ();
			double yext = y - ymin;
			yext /= axisRate;
			double yt = ymin + yext;
			NumberFormat nf2 = NumberFormat.getInstance ();
			nf2.setMinimumFractionDigits (2);
			nf2.setMaximumFractionDigits (2);
			nf2.setGroupingUsed (false);

			String toolTipText = "";
			if (tracer || viewer) {
				toolTipText += Translator.swap ("MecaPanel2D.coordinates") + " (m) : ( ";
				NumberFormat nf3 = NumberFormat.getInstance ();
				nf3.setMinimumFractionDigits (3);
				nf3.setMaximumFractionDigits (3);
				nf3.setGroupingUsed (false);
				toolTipText+= nf3.format (x) + " ; " + nf3.format (y) + " )";
			} else if (graphHvsD) {
				double SL = 100d * yt / x;
				toolTipText += "SL = " + nf2.format (SL) + " (D = " + nf2.format (x) + " cm ; H = " + nf2.format (yt) + " m)";
			} else if (graphCLvsH) {
				double CR = 100d * yt / x;
				toolTipText += "CR = " + nf2.format (CR) + " (H = " + nf2.format (x) + " m ; CL = " + nf2.format (yt) + " m)";
			} else if (graphH2vsDCL) {
				double SLCR = 100 * yt / x;
				toolTipText += "SL/CR = " + nf2.format (SLCR) + " (D.CL = " + nf2.format (x) + " cm.m ; H� = " + nf2.format (yt) + " m.m)";
			} else if (graphHvsSxx) {
				toolTipText += "SxxC = " + nf2.format (x) + " MPa ; H = " + nf2.format (yt) + " m";
			} else if (graphPercvsTime) {
				toolTipText += "t = " + nf2.format (x) + " ; % = " + nf2.format (yt);
			} else {
				toolTipText += "X = " + nf2.format (x) + " ; Y = " + nf2.format (yt);
			}
			setToolTipText (toolTipText);
			// reset (); // seems to be not needed - fc - 3.2.2003
			this.repaint ();
		}
	}

}







