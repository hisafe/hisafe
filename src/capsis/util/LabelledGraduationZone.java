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

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.util.Iterator;
import java.util.Vector;

/**
 * A zone in a Graphics2D where you can draw axis' graduations.
 * This graduations are read in a label list (ex: "0-10", 10-20", ...).
 * This component can have 4 optional margins to allow graduation
 * whole appearance. HORIZONTAL axes only.
 * <PRE>
 *       m2
 *    |------|
 * m1 |      | m3
 *    |------|
 *       m4
 * </PRE>
 * 
 * @author F. de Coligny - september 2000
 */
public class LabelledGraduationZone extends GraduationZone {
	public static final int MAX_FRACTION_DIGITS = 2;
	public static final int INTER_GRAD = 3;
	private Vector xAnchors;
	private Vector xLabels;
	
	public LabelledGraduationZone (int w, int h, int m1, int m2, int m3, int m4) {
		super (w, h, m1, m2, m3, m4);
	}

	/**
	 * Redefines AxisZone.setAxis (). A labbelled graduation zone is always hoorizontal 
	 * (only for x axes). It will draw each label anchored to the matching x anchor.
	 */
	public void setAxis (double l, Interval interval, double step, AffineTransform axisTransform, 
			Vector xAnchors, Vector xLabels)  {
		super.setAxis (AxisZone.HORIZONTAL, l, interval, step, axisTransform);
		this.xAnchors = xAnchors;
		this.xLabels = xLabels;
	}

	public void paintComponent (Graphics g) {
		superPaintComponent (g);
		
		Graphics2D g2 = (Graphics2D) g;
		FontMetrics fm = g2.getFontMetrics (g2.getFont ());
		int fontAscent = fm.getAscent ();
		int fontHeight = fm.getHeight ();
		
		// graduation direction for horizontal axes
		
		// graduation max width
		int wMax = 0;
		for (Iterator i = xLabels.iterator (); i.hasNext ();) {
			String label = (String) i.next ();
			int w = fm.stringWidth (label);
			if (w > wMax) {
				wMax = w;
			}
		}
		
		// max space available => graduation direction (ver/hor)
		boolean apla = true;
		int realWidth = getWidth ()-m1-m3;
		int spaceNeeded = xLabels.size () * (wMax + INTER_GRAD);
		if (spaceNeeded > realWidth) {
			apla = false;
		}
		
	    g2.setStroke(new BasicStroke(0));	// important: no width (to avoid scale interaction for pen)
		g2.setPaint (Color.black);
		
		g2.translate (m1-fontAscent/2, 0d);	// fc - 16.5.2003
		
		g2.transform (axisTransform);
		
		// Font transform (shifts, rotations...)
		AffineTransform at = new AffineTransform ();
		at.setToIdentity ();
		
		double xScale = axisTransform.getScaleX ();
		at.scale (1/xScale, 1);
		
		if (apla) {
			//~ at.translate (-(double) (fontAscent/2-1)+m1, fontAscent);		// fc - 16.5.2003
			at.translate (-(double) (fontAscent/2-1), fontAscent);	// fc - 16.5.2003
		} else {
			//~ at.translate (m1, 0d);	// fc - 16.5.2003
			at.rotate (Math.PI/2);
		}
		
		Font font = g2.getFont ();		
		Font drawFont = font.deriveFont (Font.PLAIN, at);
		g2.setFont (drawFont);
		
		Iterator anchors = xAnchors.iterator ();
		Iterator labels = xLabels.iterator ();
		
		while (anchors.hasNext () && labels.hasNext ()) {
			float x = (float) ((Integer) anchors.next ()).intValue ();
			//float x = (float) ((Double) anchors.next ()).doubleValue ();
			String label = (String) labels.next ();
			
			if (gAspect == LEFT) {
				g2.drawString (label, x, 0f);
			} else {
				g2.drawString (label, x, (float) getHeight ());
			}
		}
		
	}

	public String toString () {
 		return super.toString ();
	}

}

