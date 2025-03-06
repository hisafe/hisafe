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
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.util.Iterator;
import java.util.Vector;

/**
 * A zone in a Graphics2D where you can draw an axis with graduations.
 * Graduations anchor are read in a given list.
 * Used only for horizontal axes.
 * 
 * @author F. de Coligny - october 2000
 */
public class LabelledAxisZone extends AxisZone {
	private Vector xAnchors;

	public LabelledAxisZone () {
		super ();	// identity transform
	}

	public void setAxis (double l, Interval interval, Vector xAnchors, AffineTransform axisTransform)  {
		super.setAxis (AxisZone.HORIZONTAL, l, interval, 0d, axisTransform);
		//~ validateAnchors (xAnchors);
		this.xAnchors = xAnchors;
		step = 1;
	}

	public void paintComponent (Graphics g) {
		superPaintComponent (g);
		//System.out.println ("LabelledAxisZone.paintComponent ()");
		//System.out.println ("   "+toString ());
		
		Graphics2D g2 = (Graphics2D) g;
	    g2.setStroke(new BasicStroke(0));	// important: no width (to avoid scale interaction for pen)
		g2.setPaint (Color.black);
		
		g2.transform (axisTransform);
		
		int axisShift = 0;	// to shift the axis for graduations on its right / left / center
		if (gAspect == LEFT) {
			axisShift = gSize;
		} else if (gAspect == CENTER) {
			axisShift = gSize/2;
		}
			
		g2.draw (new Line2D.Double (interval.a, y+axisShift, interval.b, y+axisShift));

		for (Iterator anchors = xAnchors.iterator (); anchors.hasNext ();) {
			double i = (double) ((Integer) anchors.next ()).intValue ();
			g2.draw (new Line2D.Double (i, y, i, y+gSize));
		}

	}
	

	public String toString () {
		StringBuffer b = new StringBuffer ();
		b.append ("x=");
		b.append (x);
		b.append (", y=");
		b.append (y);
		b.append (", direction=");
		b.append (direction);
		b.append (", gAspect=");
		b.append (gAspect);
		b.append (", gSize=");
		b.append (gSize);
		b.append (", axisTransform=");
		b.append (axisTransform);
		b.append (", interval=");
		b.append (interval);
		b.append (", step=");
		b.append (step);
		
 		return b.toString ();
	}

}

