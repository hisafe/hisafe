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

/**
 * A zone in a Graphics2D where you can draw an axis with graduations
 * 
 * @author F. de Coligny
 */
public class AxisZone extends DrawingZone {
	public static final int VERTICAL = 0;
	public static final int HORIZONTAL = 1;
	public static final int CENTER = 0;
	public static final int RIGHT = 1;
	public static final int LEFT= 2;
	public static final int DEFAULT_GRADUATION_SIZE = 6;
	
	protected double x;
	protected double y;
	
	protected int direction;
	protected double l;
	protected Interval interval;
	protected double step;
	protected AffineTransform axisTransform;
	protected int gAspect;
	protected int gSize;

	public AxisZone () {
		super (new AffineTransform ());	// identity transform
		x = 0;
		y = 0;
		
		direction = HORIZONTAL;
		l = 0;
		interval = null;
		step = 1;	// never 0 to avoid infinite loops
		axisTransform = new AffineTransform ();
		gAspect = CENTER;
		gSize = DEFAULT_GRADUATION_SIZE;
		
	}

	public void setAxis (int direction, double l, Interval interval, double step, AffineTransform axisTransform)  {
		this.direction = direction;
		this.l = l;
		this.interval = interval;
		this.step = step;
		this.axisTransform = axisTransform;

	}

	public void setGraduationAspect (int aspect) {
		gAspect = aspect;
	}

	public void setGraduationSize (int size) {
		gSize = size;
	}

	public int getWidth () {
		if (direction == VERTICAL) {
			return gSize+1;
		} else {
			return (int) l;
		}			
	}

	public int getHeight () {
		if (direction == VERTICAL) {
			return (int) l;
		} else {
			return gSize+1;
		}			
	}

	protected int superGetWidth () {
		return super.getWidth ();
	}
	protected int superGetHeight () {
		return super.getHeight ();
	}
	// for subclasses which want to redefine paintComponent ()
	protected void superPaintComponent (Graphics g) {
		super.paintComponent (g);
	}

	public void paintComponent (Graphics g) {
		super.paintComponent (g);
		
		// fc - 31.3.2003
		//~ setOpaque (true);
		//~ setBackground (Color.red);
		
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
			
		if (direction == VERTICAL) {
			g2.draw (new Line2D.Double (x+axisShift, interval.a, x+axisShift, interval.b));
			for (double i = interval.a; i < interval.b+step; i += step) {
				g2.draw (new Line2D.Double (x, i, x+gSize, i));
			}
		} else {
			g2.draw (new Line2D.Double (interval.a, y+axisShift, interval.b, y+axisShift));
			for (double i = interval.a; i < interval.b+step; i += step) {
				g2.draw (new Line2D.Double (i, y, i, y+gSize));
			}
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

