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
import java.text.NumberFormat;

/**
 * A zone in a Graphics2D where you can draw axis' graduations
 * This component can have 4 optional margins to allow graduation
 * whole appearance.
 * <PRE>
 *       m2
 *    |------|
 * m1 |      | m3
 *    |------|
 *       m4
 * </PRE>
 * 
 * @author F. de Coligny
 */
public class GraduationZone extends AxisZone {
	public static final int MAX_FRACTION_DIGITS = 2;
	public static final int INTER_GRAD = 3;
	protected int graduationShift;
	protected AffineTransform axisInverseTransform;
	
	protected int m1, m2, m3, m4;
	
	public GraduationZone (int w, int h, int m1, int m2, int m3, int m4) {
		super ();
		graduationShift = 0;
		axisInverseTransform = new AffineTransform ();
		this.m1 = m1;
		this.m2 = m2;
		this.m3 = m3;
		this.m4 = m4;
		setSize (w+m1+m3, h+m2+m4);	// fc - 31.3.203
		
	}

	public void setGraduationShift (int shift) {
		graduationShift = shift;
	}

	public int getWidth () {
		return superGetWidth ();
	}

	public int getHeight () {
		return superGetHeight ();
	}

	public void setCorner1 (int x, int y) {
		setBounds (x-m1, y-m2, getWidth (), getHeight ());
	}
	public void setCorner2 (int x, int y) {
		setBounds (x-(getWidth ()-m3), y-m2, getWidth (), getHeight ());
	}
	public void setCorner3 (int x, int y) {
		setBounds (x-m1, y-(getHeight ()-m4), getWidth (), getHeight ());
	}
	public void setCorner4 (int x, int y) {
		setBounds (x-(getWidth ()-m3), y-(getHeight ()-m4), getWidth (), getHeight ());
	}

	public void paintComponent (Graphics g) {
		superPaintComponent (g);	// i.e. super.super.paintComponent ()
		
		// fc - 31.3.2003 - test
		//~ setOpaque (true);
		//~ setBackground (Color.blue);
		
		Graphics2D g2 = (Graphics2D) g;
		FontMetrics fm = g2.getFontMetrics (g2.getFont ());
		int fontAscent = fm.getAscent ();
		int fontHeight = fm.getHeight ();
		
		// Graduation direction for horizontal axes
		// If too small , grads are rotated by 90 degrees
		//
		boolean apla = true;
		if (direction == HORIZONTAL) {
			// graduation max width
			int wMax = 0;
			for (double i = interval.a; i < interval.b+step; i += step) {
				String gradLabel = "";
				if (i == (double) ((int) i)) {	// "1" instead of "1.0"
					gradLabel = "" + (int) i;
				} else {
					NumberFormat nf = NumberFormat.getInstance ();
					nf.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
					gradLabel = nf.format ((double) i);
				}
				int w = fm.stringWidth (gradLabel);
				if (w > wMax) {
					wMax = w;
				}
			}
			// max space available
			double xScale = axisTransform.getScaleX ();
			double deScaledStep = step * xScale;
			
			double spaceMax = deScaledStep - (double) INTER_GRAD;
			if ((double) wMax > spaceMax) {
				apla = false;
			}
		}
		
		
		// Important: no width (to avoid scale interaction for pen)
		//
	    g2.setStroke(new BasicStroke(0));	
		g2.setPaint (Color.black);
		
		//~ g2.translate (m1, m2);
		//~ g2.transform (axisTransform);
		
		
		// Font transform (descaling, translations, rotations...)
		//
		AffineTransform at = new AffineTransform ();
		at.setToIdentity ();
		// VERTICAL axis
		if (direction == VERTICAL) {
			double yScale = axisTransform.getScaleY ();
			at.translate (0d, (double) (fontAscent/2));
			at.scale (1, 1/yScale);
			
			// fc NEW
			g2.translate (0d, m2);
			
		// HORIZONTAL axis
		} else {	
			
			// This works quite fine, to be refined a little...
			double xScale = axisTransform.getScaleX ();
			at.scale (1/xScale, 1);
			if (apla) {
				at.translate (0d, fontAscent);
			} else {
				at.rotate (Math.PI/2);
			}
			
			// fc NEW
			g2.translate (m1-fontAscent/2, 0d);
			
		}
		Font font = g2.getFont ();		
		Font drawFont = font.deriveFont (Font.PLAIN, at);
		g2.setFont (drawFont);
		
		// fc NEW
		g2.transform (axisTransform);
		
		
		// Draw graduations
		//
		// VERTICAL axis
		if (direction == VERTICAL) {
			for (double i = interval.a; i < interval.b+step; i += step) {
				String gradLabel = null;
				if (i == (double) ((int) i)) {	// "1" instead of "1.0"
					gradLabel = "" + (int) i;
				} else {
					NumberFormat nf = NumberFormat.getInstance ();
					nf.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
					gradLabel = nf.format (i);
					gradLabel.trim ();
				}
				if (gAspect == LEFT) {
					g2.drawString (gradLabel, 0f, (float) i);
				} else {
					int w = fm.stringWidth (gradLabel);
					g2.drawString (gradLabel, (float) (getWidth ()-w), (float) i);
				}
			}
			
		// HORIZONTAL axis
		} else {	
			for (double i = interval.a; i < interval.b+step; i += step) {
				String gradLabel = "";
				if (i == (double) ((int) i)) {	// "1" instead of "1.0"
					gradLabel = "" + (int) i;
				} else {
					NumberFormat nf = NumberFormat.getInstance ();
					nf.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
					gradLabel = nf.format ((double) i);
				}
				if (gAspect == LEFT) {
					g2.drawString (gradLabel, (float) i, 0f);
				} else {
					g2.drawString (gradLabel, (float) i, (float) getHeight ());
				}
			}
		}
		
	}


	public String toString () {
 		return super.toString ();
	}

}

