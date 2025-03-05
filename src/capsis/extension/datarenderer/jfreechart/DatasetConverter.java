/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2011  F. de Coligny, S. Dufour-Kowalski
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
package capsis.extension.datarenderer.jfreechart;

import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;

import capsis.extension.dataextractor.format.DFCurves;

/**	A superclass for Dataset converters.
 *	@author F. de Coligny - february 2011
 */
public abstract class DatasetConverter {
	
	protected List<? extends DFCurves> listOfDfcurves;
	
	protected DFCurves dfcurves;  // the first one (for evaluation)
	
	// Are there labels for ALL xs
	protected boolean xsWithLabels;
	// Are there lines annotations (a label at the end: Dg, Ddom...)
	protected boolean annotatedCurves;
	// Are there several x with same value
	protected boolean xsContainDuplicates;

	protected int nbCurves;		
	
	protected List<String> xLabels;
	protected List<String> yAnnotations;
	protected List<Color> yColors;
//	protected List<Integer> nbPoints;  // of the 'ith' curve

	protected String title;
	protected String xAxisName;
	protected String yAxisName;

	
	
	/**	Constructor.
	 * 	All instances in the given list must have the same type (same class).
	 */
	public DatasetConverter (List<? extends DFCurves> listOfDfcurves) {
		this.listOfDfcurves = listOfDfcurves;
		
		// Not empty
		if (listOfDfcurves == null || listOfDfcurves.size () == 0) {
			Log.println (Log.WARNING, "XYDatasetConverter.c ()", 
					"Wrong listOfDfcurves (null or empty): "+listOfDfcurves+", error");
			return;
		}
		
		// All same class
		Collection reps = AmapTools.getRepresentatives(listOfDfcurves);
		if (reps == null || reps.size () != 1) {
			Log.println (Log.WARNING, "XYDatasetConverter.c ()", 
					"Wrong listOfDfcurves: should contain instances of same class: "+listOfDfcurves+", error");
			return;
			
		}
		
		// Evaluate first DFCurves instance
		evaluate (listOfDfcurves.get (0));
		
		memoCurvesInfo (listOfDfcurves);
	
	}

	
	/**	Evaluate.
	 * 	Runs an evaluation on the first DFCurves of the list, picks up 
	 * 	information valid for the whole dataset.
	 */
	public void evaluate (DFCurves dfcurves) {
		this.dfcurves = dfcurves;

		// Sizes (number of series / length)
		int n1 = dfcurves.getNY ();
		
		// Check getNY (): must be curves.size () -1
		List<List<? extends Number>> curves = dfcurves.getCurves ();
		nbCurves = curves.size () -1;		
		
		if (nbCurves != dfcurves.getNY ()) {
			Log.println (Log.WARNING, "XYDatasetConverter.evaluate ()", 
					"DFCurves inconsistency: nbCurves="+nbCurves+" getNY()="
					+dfcurves.getNY ()+", passed: "+dfcurves.getClass ().getName ());
		}
		
		int nbPoints1 = curves.get (0).size ();
		
		// Evaluate labels
		List<List<String>> labels = dfcurves.getLabels ();
		
		evaluateXLabels (labels, nbCurves, nbPoints1);
		evaluateYLabels (labels, nbCurves, nbPoints1);
		
		evaluateXs (curves.get (0));
		
		String title = dfcurves.getName();
		
		List<String> axesNames = dfcurves.getAxesNames ();
		xAxisName = axesNames.get (0);
		yAxisName = axesNames.get (1);
		
	}

	/**	Evaluate the X labels
	 */
	private void evaluateXLabels (List<List<String>> labels, int nbCurves, int nbPoints1) {
		if (labels != null && labels.size () != 0) {
			
			// x labels evaluation
			xLabels = labels.get (0);
	
			xsWithLabels = false;
			
			if (xLabels != null && xLabels.size () != 0) {
				if (xLabels.size () == nbPoints1) {
					xsWithLabels = true;
				} else {
					Log.println (Log.WARNING, "XYDatasetConverter.evaluateXLabels ()", 
							"DFCurves inconsistency: nbPoints1="+nbPoints1+" xLabels.size ()="
							+xLabels.size ()+"(should be 0 or nbPoints1), considered 0, passed: "+dfcurves.getClass ().getName ());
					
				}
			}
		}		
	}

	/**	Evaluate the Y labels
	 */
	private void evaluateYLabels (List<List<String>> labels, int nbCurves, int nbPoints1) {
		if (labels != null && labels.size () != 0) {

			// ys labels evaluation
			annotatedCurves = false;
			yAnnotations = new ArrayList<String> ();
			
			if (labels.size () > 1) {
				try {
					// There should be an yLabels list for each curve
					// With ONE label only inside (for curves annotation)
					for (int i = 1; i <= nbCurves; i++) {
						List<String> yLabels = labels.get (i);
						if (yLabels == null) 
							throw new Exception ("yLabels should not be null here (looking for curves annotations)");
						if (yLabels.size () == 0) 
							throw new Exception ("yLabels should not be empty here (looking for curves annotations)");
						if (yLabels.size () > 1) 
							throw new Exception ("yLabels size should be 1 here (found "+yLabels.size ()
									+", looking for curves annotations)");
						yAnnotations.add (yLabels.get (0));
					}
					annotatedCurves = true;
				} catch (Exception e) {
					Log.println (Log.WARNING, "XYDatasetConverter.evaluateYLabels ()", 
							"DFCurves inconsistency: incorrect curves annotations, ignored annotations, passed: "
							+dfcurves.getClass ().getName (), e);
					
				}
				
			}
		
		}
	}

	/**	Evaluate the X values
	 */
	private void evaluateXs (List<? extends Number> xs) {
		if (xs == null || xs.size () == 0) {
			Log.println (Log.WARNING, "XYDatasetConverter.evaluateXs ()", 
					"DFCurves inconsistency: could not find any values for xs, passed: "+dfcurves.getClass ().getName ());
		}
		Set<Number> bag = new HashSet<Number> ();
		for (Number n : xs) {
			double v = n.doubleValue ();
			if (bag.contains (v)) {
				xsContainDuplicates = true;
				break;
			}
			bag.add (v);
		}

	}

	
	/**	Memo for each curve its color and number of points.
	 */
	private void memoCurvesInfo (List<? extends DFCurves> listOfDfcurves) {
//		nbPoints = new ArrayList<Integer> ();
		yColors = new ArrayList<Color> ();
		for (DFCurves curves : listOfDfcurves) {
			yColors.add (curves.getColor ());
//			nbPoints.add (curves.getCurves().get (0).size());
		}
	}

	// Accessors
	public String getTitle() {return title;}
	public String getXAxisName() {return xAxisName;}
	public String getYAxisName() {return yAxisName;}
	public List<Color> getYColors() {return yColors;}
	public boolean isXsWithLabels () {return xsWithLabels;}
	public boolean isXsContainDuplicates () {return xsContainDuplicates;}

	
}
