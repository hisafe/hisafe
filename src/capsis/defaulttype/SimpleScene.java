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

package capsis.defaulttype;

import java.io.Serializable;

import jeeb.lib.util.Log;
import jeeb.lib.util.Vertex3d;
import capsis.kernel.GScene;
import capsis.kernel.Plot;
import capsis.kernel.PlotSubdivision;
import capsis.kernel.Step;

/**
 * Simple stand description : without individualized trees.
 *
 * @author F. de Coligny - august 2002
 */
public class SimpleScene implements GScene, Serializable, Cloneable {

	// WARNING: if references to objects (not primitive types) are added here,
	// implement a "public Object clone ()" method (see RectangularPlot.clone () for template)

	// nb-27.08.2018
	//private static final long serialVersionUID = 1L;
	
	/**
	 * This class contains immutable instance variables for a logical SimpleScene.
	 * @see Tree
	 */
	public static class Immutable implements Cloneable, Serializable {

		// nb-27.08.2018
		//private static final long serialVersionUID = 1L;

		protected String sourceName;
	}

	// these values won't change along time
	protected Immutable immutable;
	
	// these value may change along time
	protected boolean initialScene;
	protected boolean interventionResult;
	protected Plot<? extends PlotSubdivision> plot;	
	protected int date;
	protected Step step;
	
	
	
	/**	Constructor
	 */
	public SimpleScene () {
		createImmutable ();
		setPlot (new DefaultPlot ());
		plot.setScene(this);
		
		setSourceName ("");
		setOrigin (new Vertex3d (0d, 0d, 0d));
		setXSize (0d);
		setYSize (0d);
		setArea (0d);
		
		setInitialScene (false);
		setInterventionResult (false);
		
		setDate (0);
		setStep (null);
	}

	/**
	 * Create an Immutable object whose class is declared at one level of the hierarchy.
	 * This is called only in constructor for new logical object in superclass. 
	 * If an Immutable is declared in subclass, subclass must redefine this method
	 * (same body) to create an Immutable defined in subclass.
	 */
	protected void createImmutable () {immutable = new Immutable ();}
	
	// From this point : methods required by GScene interface
	@Override
	public Object clone () {
		try {
			return super.clone ();
		} catch (Exception e) {
			Log.println (Log.ERROR, "SimpleScene.clone ()", "Clone error due to exception", e);
			return null;
		}
	}
	
	@Override
	public GScene getEvolutionBase () {return null;}

	@Override
	public void setInitialScene (boolean b) {initialScene = b;}

	@Override
	public boolean isInitialScene () {return initialScene;}
	
	@Override
	public GScene getInterventionBase () {return null;}
	
	@Override
	public void setInterventionResult (boolean b) {interventionResult = b;}

	@Override
	public boolean isInterventionResult () {return interventionResult;}
	
	@Override
	public void setPlot (Plot p) {plot = p;}	// optional
	
	// fc+sg-25.10.2017 added plot subclasses
	@Override
	public <T extends Plot> T getPlot() {return (T) plot;}
//	public Plot getPlot () {return plot;}	// optional
	
	@Override
//	public boolean hasPlot () {return (plot != null) && !(plot instanceof DefaultPlot);}	// optional
	public boolean hasPlot () {return plot != null;} // fc-9.12.2011 Warning in ModisPinaster, the plots should always be cloned (see TreeList.clone ())

	@Override
	public int getDate () {return date;}
	
	public double getCorrectedDate () {return getDate ();}	// fc - 20.1.2003 - temporary
	
	@Override
	public String getCaption () {
		if (isInterventionResult ()) {
			return "*"+getDate ();
		} else {
			return ""+getDate ();
		}
	}
	
	@Override
	public String getToolTip () {return "Date "+date+((initialScene)?", initial stand":"");}

	@Override
	public String getSourceName () {return immutable.sourceName;}

	@Override
	public Vertex3d getOrigin () { return plot.getOrigin(); }

	@Override
	public double getXSize () { return plot.getXSize(); }

	@Override
	public double getYSize () { return plot.getYSize(); }

	@Override
	public double getArea () {return plot.getArea();}

	@Override
	public Step getStep () {return step;}
	
	@Override
	public void setDate (int d) {date = d;}

	@Override
	public void setStep (Step stp) {step = stp;}
	
	@Override
	public void setSourceName (String str) {immutable.sourceName = str;}
	
	@Override
	public void setOrigin (Vertex3d o) { plot.setOrigin(o); }
	
	@Override
	public void setXSize (double v) { plot.setXSize(v); }
	
	@Override
	public void setYSize (double v) { plot.setYSize(v); }
	
	@Override
	public void setArea (double a) { plot.setArea(a); }

	@Override
	public String toString () {return "SimpleScene_"+date;}
	
	@Override
	public String bigString () {return toString ();}
		
}

