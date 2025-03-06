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

import java.awt.Point;

import jeeb.lib.util.Range;

/**
 * This class allows to translate a coordinate. Use an xTracer for x
 * and an yTracer for y which may be reversed. Logical coordinates (ex: (0, 0),
 * (1, 1) etc...) can thus be translated in others to draw in a Graphics.
 * 
 * @author F. de coligny
 */
public class Tracer {
	/** Origin point logical (human referential) coordinates. */
	private Point origin;

	/** This is the maximum range of values that will be drawn on x and y. */
	private Range xRange;
	private Range yRange;

	/** Length in pixels used to draw the ranges on x and y. */
	private int xRealLength;
	private int yRealLength;

	/** Graphics have their origin Point at the upper left corner. Used to reverse y. */
	private int reverseHeight;


	public Tracer (Point org, Range xRang, int xRealL, Range yRang, int yRealL, int reverseH) {
		////System.out.println ("Tracer: org="+org+", xRang="+xRang+", xRealL="+xRealL
		//						+",yRang="+yRang+", yRealL="+yRealL+", reverseH="+reverseH);
		origin = org;
		xRange = xRang;
		yRange = yRang;
		xRealLength = xRealL;
		yRealLength = yRealL;
		reverseHeight = reverseH;
	}

	/**
	 * Scales an horizontal length to be drawn on the Graphics.
	 */
	public int scaleX (double x) {
		double scale = 1;
		if (xRange.getVariation () != 0) {
			scale = (double) xRealLength / xRange.getVariation ();
		}
		x = x * scale;
		return (int) x;
	}

	/**
	 * Scales a vertical length to be drawn on the Graphics.
	 */
	public int scaleY (double y) {
		double scale = 1;
		if (yRange.getVariation () != 0) {
			scale = (double) yRealLength / yRange.getVariation ();
		}
		y = y * scale;
		return (int) y;
	}

	/**
	 * Translation of x coordinate (no reversion).
	 */
	public int drwX (double x) {
		x = x - xRange.a;
		double scale = 1;
		if (xRange.getVariation () != 0) {
			scale = (double) xRealLength / xRange.getVariation ();
		}
		x = x * scale;
		x += origin.x;
		return (int) x;
	}

	/**
	 * Translation of y coordinate (reversion required).
	 */
	public int drwY (double y) {
		/*//System.out.println ("Tracer.drwY (): input y="+y);
		//System.out.println ("                yRange.a="+yRange.a);
		//System.out.println ("                yRealLength="+yRealLength);
		//System.out.println ("                yRange.getVariation ()="+yRange.getVariation ());
		//System.out.println ("                origin.y="+origin.y);
		//System.out.println ("                reverseHeight="+reverseHeight);*/
		y = y - yRange.a;
		double scale = 1;
		if (yRange.getVariation () != 0) {
			scale = (double) yRealLength / yRange.getVariation ();
		}
		y = y * scale;
		y += origin.y;
		y = reverse ((int) y);
		/*//System.out.println ("               output y="+(int) y);*/
		return (int) y;
	}

	public int reverse (int y) {
		return (reverseHeight + origin.y) - y;
	}

	public Point getOrigin () {
		return origin;
	}

}



