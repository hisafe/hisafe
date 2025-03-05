/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.datarenderer.drcurves;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.text.NumberFormat;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Vertex2d;
import capsis.commongui.util.Tools;
import capsis.util.Interval;


public class Graduator {
	public static final int INTERVAL_MIN_VARIATION = 2;

	public static final int VERTICAL = 0;
	public static final int HORIZONTAL = 1;
	public static final int LEFT = 0;
	public static final int RIGHT = 1;

	/**	Computes step for axis graduation (length pixels) from the interval.
	 *	This interval may be resized (enlarged) for "nice" min and max values
	 *	(multiple of step).
	 *	Note: xLabelledGraduation and xAnchors are used only if isXAxis.
	 */
	static public double graduate (Interval interval, boolean gradIsInteger, int gradSize,
			int size, boolean isXAxis, boolean xLabelledGraduation, Collection xAnchors,
			boolean yRightLabelled, double uRightLabelSize) {	// fc - 21.3.2008 - added uRightLabelSize

		// 1. If null interval, enlarge it to a default size
		//
		if (interval.isNull ()) {interval.enlarge (INTERVAL_MIN_VARIATION);}

		// 2. Compute graduations number according to total available size in pixels
		// and graduation "size" in pixels (length if x axis, height otherwise)
		//
		int minInterGrad = 3;	// min number of pixels between two graduation labels
		int nbGrad = size / (gradSize + minInterGrad);

		// 3. Compute accurate step (not "nice" (ex: 1.1235660001))
		//
		double accurateStep = interval.getVariation () / nbGrad;

		// 4. Round the accurate step to a "nice" (greater) value
		// this step MUST NOT be zero
		//
		int maxFractionDigits = 2;
		double step = GraduationStepAdjuster.adjustStep (accurateStep,
				gradIsInteger, maxFractionDigits);

		// 5. Round x interval if labelled graduation (i.e. labels are given, with anchors)
		//
		if (isXAxis && xLabelledGraduation) {
			Iterator anchors = xAnchors.iterator ();
			double x1 = 0d;
			double x2 = 0d;
			double classWidth = 0d;
			double margin = 0d;
			if (anchors.hasNext ()) {
				x1 = (double) ((Integer) anchors.next ()).intValue ();
			}
			if (anchors.hasNext ()) {
				x2 = (double) ((Integer) anchors.next ()).intValue ();
			}
			if (x2 != 0d) {
				classWidth = x2 - x1;
				margin = classWidth / 2;
			}
			interval.a = interval.a - margin;
			interval.b = interval.b + margin;

		} else {
			// 6. Round interval min value to a lower multiple of the step

			// fc - 4.10.2002
			// bug correction (detected by bc) - Some points with negative Y are not drawn - fixed
			//
			double a = interval.a;
			a = (double) ((Math.floor (a / step)) * step);	// fc - 4.10.2002

			interval.a = a;

			// 7. Compute new interval max value (greater or equal than the old one)
			double b = interval.b;
			if (!Tools.isAMultipleOfB (b, step)) {
				b = (double) ((Math.floor (b / step)) * step) + step;	// fc - 4.10.2002
				interval.b = b;
			}
		}

		// fc - 21.3.2008
		if (yRightLabelled) {
			
			if (isXAxis) {
				interval.b += uRightLabelSize;	// some place to write labels at the end (x)
			} else {
				interval.b += step;	// some place to write labels at the end (y)
			}
		}

		return step;
	}

	/**	Draw an axis in the given Graphics, with its dashes and graduations.
	*	Axis origin is in p0, ending in p1 (points with pixel coordinates).
	*	Interval & step are mandatory, Anchors & labels are optional :
	*	If anchors != null, given anchors and labels are directly used,
	*	else grad values are calculated with interval [a,b] and step.
	*	Direction can be VERTICAL or HORIZONTAL.
	*	Graduations can be located on the left or on the right (following the axis direction).
	*	Dash size is given in pixels.
	*	Panel height is needed for vertical axes only.
	*	Rq: no axis title managed here.
	*/
	static public void drawGrads (
			Graphics g,
			Point p0, Point p1,
			Interval interval, double step,
			Collection anchors, Collection labels,
			int direction, int gradsPosition,
			int dashSize, int panelHeight) {
		initSpaces (p0, p1, interval, direction, panelHeight);

		Graphics2D g2 = (Graphics2D) g;
		Font f = g2.getFont ();
		FontMetrics fm = g2.getFontMetrics (f);
		int fontAscent = fm.getAscent ();
		int fontDescent = fm.getDescent ();
		int fontHeight = fontAscent+fontDescent;
		NumberFormat nf = NumberFormat.getInstance ();
		nf.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
		nf.setGroupingUsed (false);

		int xShiftForRotatedLabels = 3;	// 3 pixels to the left - fc - 11.10.2004

		int inter = 3;
		int shift = (gradsPosition == LEFT) ? -1 : 1;	// 1 pixel shift left or right

		if (direction == VERTICAL) {

			// 1. Draw VERTICAL axis and dashes
			//
			g2.draw (new Line2D.Double (p0.x+shift, u2p(interval.a), p0.x+shift, u2p(interval.b)));
			for (double i = interval.a; i < interval.b + step/2; i += step) {
				if (gradsPosition == LEFT) {
					g2.draw (new Line2D.Double (p0.x+shift, u2p(i), p0.x+shift - dashSize, u2p(i)));
				} else {
					g2.draw (new Line2D.Double (p0.x+shift, u2p(i), p0.x+shift + dashSize, u2p(i)));
				}
			}

			// 2. Draw VERTICAL graduations
			//
			for (double i = interval.a; i <= interval.b+step/2; i+=step) {
				String grad = nf.format ((double) i);
				int gradSize = fm.stringWidth (grad);
				int x = 0;
				if (gradsPosition == LEFT) {
					x = p0.x+shift - dashSize - INTER_GRAD - gradSize;
				} else {
					x = p0.x+shift + dashSize + INTER_GRAD;
				}
				int y = u2p (i) + fontAscent/2 -1;
				g2.drawString (grad, (float) x, (float) y);
			}

		} else {	// HORIZONTAL

			// Draw axis, then dashes and graduations
			//
			g2.draw (new Line2D.Double (u2p(interval.a), p0.y+shift, u2p(interval.b), p0.y+shift));

			if (anchors != null) {	// Use directly anchors and labels

				// Grads rotation needed ?
				// 1.  graduation max width
				int wMax = 0;
				for (Iterator i = labels.iterator (); i.hasNext ();) {
					int w = fm.stringWidth ((String) i.next ());
					if (w > wMax) {wMax = w;}
				}

				// 2. max space available => graduation direction (ver/hor)
				boolean rotateGrads = false;
				int spaceNeeded = labels.size () * (wMax + INTER_GRAD);
				if (spaceNeeded > pd) {rotateGrads = true;}

				Font memoFont = g2.getFont ();
				if (rotateGrads) {	// Font transform for rotations

					AffineTransform at = new AffineTransform ();
					at.setToIdentity ();
					at.rotate (Math.PI/2);
					Font drawFont = memoFont.deriveFont (Font.PLAIN, at);
					g2.setFont (drawFont);
				}

				// Iterate and draw grads
				Iterator anc = anchors.iterator ();
				Iterator lab = labels.iterator ();
				while (anc.hasNext () && lab.hasNext ()) {
					double a = (double) ((Integer) anc.next ()).intValue ();
					String grad = (String) lab.next ();
					int gradSize = fm.stringWidth (grad);

					int x = 0;
					if (rotateGrads) {
						//~ x = u2p (a) - fontHeight/2 + 1;
						x = u2p (a) - xShiftForRotatedLabels;
//~ System.out.println ("DRCurves: rotateGrads");
					} else {
						x = u2p (a) - gradSize/2 + 1;
					}
					
					int y = 0;
					int rotationShift = 0;

					if (gradsPosition == LEFT) {
						if (rotateGrads) {rotationShift = -gradSize;}
						g2.draw (new Line2D.Double (
								u2p(a), p0.y+shift, u2p(a), p0.y+shift - dashSize));	// dash
						y = p0.y+shift - dashSize - INTER_GRAD + rotationShift;

					} else {
						if (rotateGrads) {rotationShift = -fontHeight;}
						g2.draw (new Line2D.Double (
								u2p(a), p0.y+shift, u2p(a), p0.y+shift + dashSize));	// dash
						y = p0.y+shift + dashSize + INTER_GRAD + fontHeight + rotationShift;
					}

					g2.drawString (grad, (float) x, (float) y);	// grad

				}

				// Restore font in case it was rotated
				g2.setFont (memoFont);

			} else {	// Use Interval and step

				// Grads rotation needed ?
				// 1.  graduation max width
				int wMax = 0;
				int nbGrads = 0;
				for (double i = interval.a; i <= interval.b+step/2; i+=step) {
					String grad = nf.format ((double) i);
					nbGrads++;
					int w = fm.stringWidth ((String) grad);
					if (w > wMax) {wMax = w;}
				}

				// 2. max space available => graduation direction (ver/hor)
				boolean rotateGrads = false;
				int spaceNeeded = nbGrads * (wMax + INTER_GRAD);
				if (spaceNeeded > pd) {rotateGrads = true;}

				Font memoFont = g2.getFont ();
				if (rotateGrads) {	// Font transform for rotations
					AffineTransform at = new AffineTransform ();
					at.setToIdentity ();
					at.rotate (Math.PI/2);
					Font drawFont = memoFont.deriveFont (Font.PLAIN, at);
					g2.setFont (drawFont);
				}

				// Loop and draw grads
				for (double i = interval.a; i <= interval.b+step/2; i+=step) {
					String grad = nf.format ((double) i);
					int gradSize = fm.stringWidth (grad);

					//~ int x = u2p (i) - gradSize/2 + 1;
					int x = 0;
					if (rotateGrads) {
						//~ x = u2p (i) - fontHeight/2 + 1;
						x = u2p (i) - xShiftForRotatedLabels;
					} else {
						x = u2p (i) - gradSize/2 + 1;
					}
					
					if (x > p1.x) continue; // fc-27.3.2012 (sometimes a grad in the right margin)

					int y = 0;
					int rotationShift = 0;
					if (gradsPosition == LEFT) {
						if (rotateGrads) {rotationShift = -gradSize;}

						g2.draw (new Line2D.Double (
								u2p(i), p0.y+shift, u2p(i), p0.y+shift - dashSize));	// dash
						y = p0.y+shift - dashSize - INTER_GRAD + rotationShift;
					} else {
						if (rotateGrads) {rotationShift = -fontHeight;}

						g2.draw (new Line2D.Double (
								u2p(i), p0.y+shift, u2p(i), p0.y+shift + dashSize));	// dash
						y = p0.y+shift + dashSize + INTER_GRAD + fontHeight + rotationShift;
					}
					g2.drawString (grad, (float) x, (float) y);	// grad
				}

				// Restore font in case it was rotated
				g2.setFont (memoFont);

			}
		}

	}

	public static final int MAX_FRACTION_DIGITS = 2;
	public static final int INTER_GRAD = 3;


	// Space conversion variables
	//
	static private int direction;
	static private int panelHeight;

	// User space U (double precision) : 2 points, distance
	//
	static private Vertex2d u0;
	static private Vertex2d u1;
	static private double ud;

	// Pixel space P (int precision) : 2 points, distance
	//
	static private Point p0;
	static private Point p1;
	static private int pd;

	// Inits some variables for u2p () conversions method
	//
	static private void initSpaces (Point p0, Point p1, Interval interval,
			int direction, int panelHeight) {
		Graduator.direction = direction;
		Graduator.panelHeight = panelHeight;
		Graduator.p0 = p0;
		Graduator.p1 = p1;
		if (direction == VERTICAL) {
			Graduator.u0 = new Vertex2d (0, interval.a);
			Graduator.u1 = new Vertex2d (0, interval.b);
			Graduator.pd = Math.abs (p1.y-p0.y);
			Graduator.ud = Math.abs (u1.y-u0.y);
		} else {
			Graduator.u0 = new Vertex2d (interval.a, 0);
			Graduator.u1 = new Vertex2d (interval.b, 0);
			Graduator.pd = Math.abs (p1.x-p0.x);
			Graduator.ud = Math.abs (u1.x-u0.x);
		}
	}

	// Conversion U-space -> P-space (user -> pixel)
	// Occurs only in one dimension (axis direction)
	// initSpace must have been called before
	//
	static private int u2p (double u) {
		if (direction == VERTICAL) {
			return panelHeight - (int) ((u-u0.y) / ud * pd + (panelHeight-p0.y));
		} else {
			return (int) ((u-u0.x) / ud * pd + p0.x);
		}
	}

}


