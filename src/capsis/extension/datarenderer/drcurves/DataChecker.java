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

import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import capsis.extension.dataextractor.format.DFColoredCurves;
import capsis.extension.dataextractor.format.DFCurves;

public class DataChecker {

	/**
	 * Check data : only for extractors, no special extractors here.
	 */
	static public boolean dataAreCorrect(Collection extractors) {

		long time = System.currentTimeMillis(); // TIME ELAPSED STUFF

		try {

			if (extractors == null) {
				Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", "Reason: extractors == null");
				return false;
			}
			if (extractors.isEmpty()) {
				Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", "Reason: extractors isEmpty ()");
				return false;
			}
			for (Iterator i = extractors.iterator(); i.hasNext();) {
				DFCurves extr = (DFCurves) i.next();

				// // fc-30.10.2014 If the extractor says it is not available,
				// this is correct, the renderer will manage it
				// if (!extr.isAvailable()) {
				// continue;
				// }

				// System.out.println (traceData (extr)); // fc-20.1.2014

				
				// fc-21.9.2015
				String extractorName = extr.getClass().getName();
				String errorPrefix = "Error in DataChecker for "+extractorName+": ";
				
				// lacking info
				if (extr.getCurves() == null) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: extr.getCurves () == null");
					return false;
				}
				if (extr.getCurves().isEmpty()) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: extr.getCurves ().isEmpty ()");
					return false;
				}
				if (extr.getAxesNames() == null) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: extr.getAxesNames () == null");
					return false;
				}
				if (extr.getAxesNames().isEmpty()) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: extr.getAxesNames ().isEmpty ()");
					return false;
				}

				if (extr instanceof DFColoredCurves) { // fc - 9.2.2006
					DFColoredCurves cc = (DFColoredCurves) extr;
					if (cc.getColors() == null) {
						Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()",
								errorPrefix+"Reason: DFColoredCurves and extr.getColors () == null");
						return false;
					}
				}

				// wrong axes number
				int axesN = extr.getAxesNames().size();
				if (axesN < 2 || axesN > 3) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()",
							errorPrefix+"Reason: axesN < 2 || axesN > 3, (axenN=extr.getAxesNames ().size ()=" + axesN + ")");
					return false;
				}

				// wrong data lists number
				int nD = extr.getCurves().size();
				if (nD < axesN) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()",
							errorPrefix+"Reason: nD = extr.getCurves ().size () = " + nD
									+ " < axesN = extr.getAxesNames ().size () = " + axesN + ")");
					return false;
				}

				// wrong y curves number
				int nY = extr.getNY();
				if (nY < 1) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: extr.getNY () ("+nY+") < 1");
					return false;
				}

				if (extr instanceof DFColoredCurves) { // fc - 9.2.2006 - check
														// number of curve
														// colors
					DFColoredCurves cc = (DFColoredCurves) extr;
					if (cc.getColors().size() != nD - 1) {
						Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()",
								errorPrefix+"Reason: DFColoredCurves and wrong number of colors, expected: " + (nD - 1)
										+ ", found: " + cc.getColors().size());
						return false;
					}
				}

				if (axesN == 2 && nY != nD - 1) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: axesN == 2 && nY ("+nY+") != nD-1 ("+(nD - 1)+")");
					return false;
				}
				if (axesN == 3 && nY > nD - 2) {
					Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", errorPrefix+"Reason: axesN == 3 && nY ("+nY+") > nD-2 ("+(nD - 2)+")");
					return false;
				}
			}
		} catch (Exception e) {
			Log.println(Log.WARNING, "DataChecker.dataAreCorrect ()", "Could not check extractors due to an exception ", e);
			return false;
		}

		time = System.currentTimeMillis() - time; // TIME ELAPSED STUFF
		// System.out.println
		// ("TIME ELAPSED in DRCurves.dataAreCorrect () : "+time+" ms"); // TIME
		// ELAPSED STUFF

		return true;
	}

	static private String traceData(DFCurves c) {
		StringBuffer b = new StringBuffer("DataChecker, trace...");

		b.append("\nName: " + c.getName());
		b.append("\nCaption: " + AmapTools.cutIfTooLong(c.getCaption(), 50));
		b.append("\nNumber of Y axes: " + c.getNY());
		b.append("\nAxes names");
		for (String an : c.getAxesNames()) {
			b.append("\n" + an);
		}

		b.append("\nDefault data renderer: " + c.getDefaultDataRendererClassName());

		b.append("\nLabels...");
		for (List<String> list : c.getLabels()) {
			b.append("\n");
			for (String lab : list) {
				b.append(" | " + lab);
			}
		}

		b.append("\nValues...");
		for (List<? extends Number> list : c.getCurves()) {
			b.append("\n");
			for (Number n : list) {
				b.append(" | " + n);
			}
		}

		return b.toString();

	}

}