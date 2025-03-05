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

package capsis.script;

import java.awt.Color;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import jeeb.lib.util.Log;
import au.com.bytecode.opencsv.CSVWriter;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extension.datarenderer.drcurves.DataChecker;
import capsis.extensiontype.DataExtractor;

/**	
 * Write a CSV from a data extractor
 * Xname, Yname
 * X, Y1, Y2, Y3, ...
 * None, curveName, curveName
 * xa, ya1, ya2, ...
 * xb, yb1, yb2, ... 
 */
public class DEExporter  {

	private DataExtractor dE;

	public DEExporter(DataExtractor de) {

		dE = de;
	}


	public void export (String filename) throws IOException {

		CSVWriter writer = new CSVWriter(new FileWriter(filename), '\t', CSVWriter.NO_QUOTE_CHARACTER);

		// number of curves
		int nbCurves = 0;
		int nbPoints = 0;

		DFCurves extr = (DFCurves) dE;				
		List<List<? extends Number>> curves = extr.getCurves ();
		nbCurves = curves.size ();		// + n 1D coordinates vectors (x, y1, y2 ...yn-1)
		nbPoints = curves.get(0).size();

		// Axe name
		List<String> axesNames = extr.getAxesNames ();
		String xName = (String) axesNames.get (0);
		String yName = (String) axesNames.get (1);
		writer.writeNext(new String[]{xName, yName});
		
		// Header
		String[] header = new String[nbCurves];
		header[0] = "X";
		for(int i=1; i<nbCurves; i++) {
			header[i] = "Y" + i;
		}
		writer.writeNext(header);
		
		
		// labels
		List<List<String>> labels = extr.getLabels ();
		String[] strlabels = new String[labels.size()];
		for(int i=0; i<labels.size(); i++) {
			String s = labels.get(i).toString();
			strlabels[i] = s;
		}
		
		writer.writeNext(strlabels);

		//curves
		for(int i=0; i<nbPoints; i++) {
			String[] line = new String[nbCurves];
			for(int j=0; j<nbCurves; j++) {
				Number n = curves.get(j).get(i);
				line[j] = "" + n;
			}
			writer.writeNext(line);
		}
		
		writer.close();
	}




}


