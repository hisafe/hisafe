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

package capsis.util.diagram2d;

import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Vertex2d;

/**
 * Information concerning graduations for an axis (X or Y) in a Diagram2D.
 * With a given GraduationContext, you can prepare an axis by using the adjustXAxis () or
 * adjustYAxis (), specifying the FontMetrics and the available size for the future axis drawing.
 * Adjust process creates an other GraduationContext, with adjusted == true.
 * Once adjusted, you can use the matching draw...Axis () to really draw the axis.
 */
public class GraduationContext {
	public static final int MAX_FRACTION_DIGITS = 2;
	
	static public final int FILLER_1 = 3;	// pixels - between border and graduation
	static public final int FILLER_2 = 2;	// pixels - between graduation ans dash
	static public final int DASH_SIZE = 3;	// pixels - size of the dash

	// True if the GraduationContext was built by one of the adjust... methods.
	public boolean adjusted;
	
	// If not null, should be writen near the axis
	public String axisName;
	
	// After adjust, contains all info for graduating (Grad instances)
	public Collection grads;
	
	// If true, grads must all be integers
	public boolean integersOnly;

	// If true, grads must be rotated 90 degrees
	public boolean rotateGrads;

	// Axis should begin here
	public double begin;
	
	// Axis should end here
	public double end;
	
	// Optional: we can set a grad label which must be writable on the axis
	// this is to allow alignment of two vertical axes of two Diagram2D 
	public String longestGrad;
	
	// computed by adjust...Axis () methods
	public int margin;		// in pixels

	protected NumberFormat formater;	// to control number of decimals

	
	/**	Default constructor.
	*/
	public GraduationContext () {
		// Used to format decimal part with 2 digits only
		formater = NumberFormat.getInstance ();
		formater.setMaximumFractionDigits (MAX_FRACTION_DIGITS);
	}

	/**	Prepares graduations (grads + begin + end + margin) from unajusted
	*	graduations (grads + begin + end OR just begin + end AND maybe integersOnly)
	*	and some metrics information in order to build an X axis.
	*/
	public GraduationContext adjustXAxis (FontMetrics fm, int approxWidth) {	// approxWidth in pixels
		
		int neededWidth = 0;				// in pixels
		int gradHeight = fm.getHeight ();	// in pixels
		int maxGradWidth = 1;			
		
		GraduationContext gc = new GraduationContext ();
		
		gc.axisName = axisName;
		
		gc.begin = begin;
		gc.end = end;

		int gradNumber = 0;
		
		// Case 1. Grads exist, set margin and rotate
		// Note : according to available size, some labels may be discarded
		if (grads != null) {
			double minAnchor = Double.MAX_VALUE;
			double maxAnchor = Double.MIN_VALUE;
			for (Iterator i = grads.iterator (); i.hasNext ();) {
				Grad g = (Grad) i.next ();
				if (g == null || g.label == null) {break;}	// security (related to forcedXMax)
				maxGradWidth = Math.max (maxGradWidth, fm.stringWidth (g.label));	// in pixels
				minAnchor = Math.min (minAnchor, g.anchor);
				maxAnchor = Math.max (maxAnchor, g.anchor);
			}
			gradNumber = approxWidth / maxGradWidth;
			
			// Case 1.1
			if (gradNumber >= grads.size ()) {
				gc.margin = FILLER_1 + gradHeight + FILLER_2 + DASH_SIZE;
				gc.grads = grads;
			} else {	// Rotate grad labels by 90 degrees and maybe discard some of them
				gc.rotateGrads = true;
				gc.margin = FILLER_1 + maxGradWidth + FILLER_2 + DASH_SIZE;
				
				gc.grads = grads;
			}
			
		// Case 2. compute step and grads from begin, end and integersOnly.
		} else {
			gc.grads = new ArrayList ();
			
			double accurateStep = (end - begin) / gradNumber;
			
			// Replace this by a nice step computation (enjoy decimal cases)
			double niceStep = accurateStep;
			if (integersOnly) {
				niceStep = (double) (int) accurateStep;
				if ((end - begin) % niceStep != 0) {end += niceStep;}
			}
			if (niceStep == 0) {	// must be different than zero
				niceStep = 1;
				end = begin + niceStep;
			}
			
			for (double i = begin; i <= end; i+= niceStep) {
				String label = "" + i;
				if (integersOnly) {
					label = ""+ (int) i;	// avoid ".0"
				}
				neededWidth = Math.max (neededWidth, fm.stringWidth (label));	// max grad length
				Grad grad = new Grad (i, label);
				gc.grads.add (grad);
			}
		}
		
		if (!"".equals (axisName)) {	// leave some space for X axis name
			gc.margin += gradHeight;
		}
		
		gc.adjusted = true;
		
		return gc;
	}

	/**	Draws an horizontal X axis in a Diagram2D from an adjusted Graduation context.
	*/
	public void drawXAxis (Graphics2D g2, double y0, FontMetrics fm, Diagram2D d2) {
		if (!adjusted) {return;}
		
		Line2D.Double axis = new Line2D.Double (begin, y0, end, y0);
		g2.draw (axis);
		
		// Let's remind original Font in case we would rotate...
		Font memoFont = new Font (g2.getFont ().getName (), 
				g2.getFont ().getStyle (), g2.getFont ().getSize ());	
		AffineTransform memoTransform = g2.getFont ().getTransform ();
		memoFont = memoFont.deriveFont (memoTransform);
		
		double dashUserSize = d2.getUserHeight (DASH_SIZE);
		int gradHeight = fm.getHeight ();	// in pixels
		int gradAscent = fm.getAscent ();	// in pixels
		double userAscent = d2.getUserHeight (gradAscent);
		double maxLabelUserShift = 0d;
		
		
		// Grad labels frequency problem
		int realWidth = d2.getPixelWidth (end - begin);
		int neededWidth = grads.size () * gradAscent;	// labels are vertical
		int frequency = 1;
		if (realWidth != 0) {frequency = (int) ((neededWidth / realWidth) + 1);}	// ok
				
		// Rotate font to draw grad labels if needed
		if (rotateGrads) {
			// Font rotation
			AffineTransform at = new AffineTransform ();
			at.setToIdentity ();
			
			Vertex2d scaleFactor = d2.getScale ();
			at.scale (-1/scaleFactor.x, 1/scaleFactor.y);
			
			at.rotate (-Math.PI/2);
			
			Font font = g2.getFont ();		
			Font rotateFont = font.deriveFont (Font.PLAIN, at);
			g2.setFont (rotateFont);
		}
		
		// Draw the graduation labels
		float x = 0f;
		float y = 0f;
		int k = 0;
		for (Iterator i = grads.iterator (); i.hasNext (); k++) {
			Grad grad = (Grad) i.next ();
			if (grad == null || grad.label == null) {break;}	// security (related to forcedXMax)
			// Draw dash
			Line2D.Double dash = new Line2D.Double (grad.anchor, y0 - dashUserSize, grad.anchor, y0);
			g2.draw (dash);
			
			// Draw label with rotation
			if (rotateGrads) {
				int labelPixelHeight = fm.stringWidth (grad.label);
				double labelUserHeight = d2.getUserHeight (labelPixelHeight);	// label is vertical
				double labelUserShift = d2.getUserHeight (FILLER_2 + DASH_SIZE);
				maxLabelUserShift = Math.max (maxLabelUserShift, labelUserShift + labelUserHeight);
				
				double labelUserWidth = d2.getUserWidth (gradAscent);	// fc - 5.1.2005
				
				x = (float) (grad.anchor - labelUserWidth/2);	// fc - 5.1.2005
				y = (float) (y0 - labelUserShift);
				
			// Draw normal label
			} else {
				int labelPixelWidth = fm.stringWidth (grad.label);
				double labelUserWidth = d2.getUserWidth (labelPixelWidth);	// fc - 5.1.2005
				double labelUserShift = userAscent + d2.getUserHeight (FILLER_2 + DASH_SIZE);
				maxLabelUserShift = Math.max (maxLabelUserShift, labelUserShift);
				
				x = (float) (grad.anchor - labelUserWidth/2);
				y = (float) (y0 - labelUserShift);
			}
			
			// If rotate, take frequency into account
			if ((!rotateGrads) || (k % frequency == 0)) {
				g2.drawString (grad.label, x, y);
			}
				
		}
		
		g2.setFont (memoFont);
		
		// Draw axis name if needed
		if (!"".equals (axisName)) {
			double axisNameUserShift = d2.getUserWidth (fm.stringWidth (axisName));
			g2.drawString (axisName, (float) (end - axisNameUserShift), 
					(float) (y0 - maxLabelUserShift - userAscent));
		}
		
	}
	


	/**	Prepares graduations (grads + begin + end + margin) from unajusted
	*	graduations (grads + begin + end OR just begin + end AND maybe integersOnly)
	*	and some metrics information in order to build an Y axis.
	*/
	public GraduationContext adjustYAxis (FontMetrics fm, int approxHeight) {	// approxHeight in pixels
		GraduationContext gc = new GraduationContext ();
		gc.axisName = axisName;
		gc.integersOnly = integersOnly;
		
		int neededWidth = fm.stringWidth (longestGrad);		// in pixels
		int gradHeight = fm.getHeight ();	// in pixels

		gc.begin = begin;
		gc.end = end;
		
		int gradNumber = 0;
		try {gradNumber = approxHeight / gradHeight;} catch (Exception e) {}
		if (gradNumber == 0) {gradNumber = 4;}

		// Case 1. Ensure all prepared grads can be drawn
		if (grads != null) {
			// later - calculate a  and reduce if needed the 
			// grads number to match available size
		
		// Case 2. compute step and grads from begin, end and integersOnly.
		} else {
			gc.grads = new ArrayList ();
			
			double accurateStep = (end - begin) / gradNumber;
			
			// Replace this by a nice step computation (enjoy decimal cases)
			double niceStep = accurateStep;
			if (integersOnly) {
				niceStep = (double) (int) accurateStep;
			}
			if (niceStep == 0) {	// must be different than zero
				niceStep = 1;
			}
			
			for (double i = begin; i <= end; i+= niceStep) {
				String label = "" + i;
				if (integersOnly) {
					label = ""+ (int) i;	// avoid ".0"
				} else {
					label = ""+ formater.format (i);
				}
				neededWidth = Math.max (neededWidth, fm.stringWidth (label));	// max grad length
				Grad grad = new Grad (i, label);
				gc.grads.add (grad);
			}
		}
		
		neededWidth = FILLER_1 + neededWidth + FILLER_2 + DASH_SIZE;
		gc.margin = neededWidth;
		gc.adjusted = true;
		
		return gc;
	}
	
	/**	Draws a vertical Y axis in a Diagram2D from an adjusted Graduation context.
	*/
	public void drawYAxis (Graphics2D g2, double x0, FontMetrics fm, Diagram2D d2) {
		if (!adjusted) {return;}
	
		Line2D.Double axis = new Line2D.Double (x0, begin, x0, end);
		g2.draw (axis);
	
		double dashUserSize = d2.getUserWidth (DASH_SIZE);
		double labelUserHeight = d2.getUserHeight (fm.getAscent ());
		double maxLabelUserShift = 0d;

		for (Iterator i = grads.iterator (); i.hasNext ();) {
			Grad grad = (Grad) i.next ();
			// Draw dash
			Line2D.Double dash = new Line2D.Double (x0, grad.anchor, x0 - dashUserSize, grad.anchor);
			g2.draw (dash);
			
			// Draw label
			int labelPixelWidth = fm.stringWidth (grad.label);
			double labelUserShift = d2.getUserWidth (labelPixelWidth + FILLER_2 + DASH_SIZE);
			maxLabelUserShift = Math.max (maxLabelUserShift, labelUserShift);
			
			g2.drawString (grad.label, (float) (x0 - labelUserShift), 
					(float) (grad.anchor - labelUserHeight/2));	// <<<<<<<<<<<<<<<<<<<<<<<<
		}
		
		if (!"".equals (axisName)) {
			double axisNameUserShift = Math.min (d2.getUserWidth (fm.stringWidth (axisName)), maxLabelUserShift);
			g2.drawString (axisName, (float) (x0 - axisNameUserShift), 
					(float) (end + labelUserHeight / 2));	// <<<<<<<<<<<<<<<<<<<<<<<<
		}
	}
	
	public String toString () {
		String s = "GraduationContext:";
		s+= " axisName="+axisName;
		s+= " adjusted="+adjusted;
		s+= " begin="+begin;
		s+= " end="+end;
		s+= " grads="+grads;
		return s;
	}
	
	
}

