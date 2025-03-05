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

import java.awt.Graphics2D;
import java.awt.geom.Line2D;

import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extensiontype.DataExtractor;

/**
 * ScatterPlot drawer. Version 2.
 * 
 * @author F. de Coligny - september 2003
 */
public class DRScatterPlot extends DRCurves {

	static {
		Translator.addBundle("capsis.extension.datarenderer.drcurves.DRCurves");
	} 

	// nb-06.08.2018
	//static final public String NAME = Translator.swap ("DRScatterPlot"); 
	//static final public String VERSION = "2.1";
	//static final public String AUTHOR = "F. de Coligny";
	//static final public String DESCRIPTION = Translator.swap ("DRScatterPlot.description");	
	
	static public boolean matchWith (Object target) {
		boolean b = false;
		if (target instanceof DataExtractor && target instanceof DFCurves) {b = true;}
		return b;
	}

	@Override
	public String getName() {
		return Translator.swap("DRScatterPlot.name");
	}
	
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}
	
	@Override
	public String getDescription() {
		return Translator.swap("DRScatterPlot.description");
	}
	
	@Override
	public String getVersion() {
		return "2.1";
	}
	
	/**	This method plots a little cross.
	*/
	// fc - 18.3.2005
	protected void plot (Graphics2D g2, double x, double y) {
		
		if (Double.isNaN (x) || Double.isNaN (y)) {return;}
		
		// Draws a little cross "3" pixels width centered on the plot
		int precision = 1;
		if (enlargedMode) {precision = 3;}	// fc - 21.3.2005
			
		
		g2.draw (new Line2D.Double (x-precision, y, x+precision, y));	// fc - 18.3.2005
		g2.draw (new Line2D.Double (x, y-precision, x, y+precision));	// fc - 18.3.2005
			
		// Draws a single pixel
		//~ Rectangle2D.Double p = new Rectangle2D.Double ();	// fc - 11.10.2004
		//~ p.setRect (x, y, 1d, 1d);
		//~ g2.draw (p);
	}

	/**
	 * This method draws the point. It may be redefined in subclasses
	 * to draw differently.
	 * DRScatterPlot draws only points.
	 * We are working on the i th curve among n.
	 */
	@Override
	protected double draw (Graphics2D g2, int i, int n, double ux, double uy, double px, double py, 
			int curveId, int numberOfCurves, int pw, int ph) {
		plot (g2, px, py);
		return px;	// no change here
	}

}
