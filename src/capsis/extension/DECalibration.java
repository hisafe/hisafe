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

package capsis.extension;

import java.awt.Color;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.Vector;

import jeeb.lib.util.Alert;
import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extensiontype.DataBlock;

/**
 * Calibration extractor : search calibration data in conventional files 
 * and draw the calibration data around the other extractors.
 * 
 * @author F. de Coligny - October 2004
 */
public class DECalibration extends PaleoDataExtractor implements DFCurves {

	static {
		Translator.addBundle("capsis.extension.DECalibration");
	}

	protected List<List<? extends Number>> curves;

	private DataBlock dataBlock;
	private String modelName;
	private String extractorName;
		
	private String xName;
	private String yName;
 
	
	/**
	 */
	public DECalibration (DataBlock dataBlock, String modelName, String extractorName) {
		curves = new ArrayList<List<? extends Number>> ();
		this.dataBlock = dataBlock;
		this.modelName = modelName;
		this.extractorName = extractorName;
		xName = "";
		yName = "";
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	// Problem, this Extension is not compatible
	public boolean matchWith (Object referent) {
		try {
//			if(!(referent instanceof GModel)) { return false; } // ERROR fc-4.9.2012
			
			Object[] modelAndExtractorNames = (Object[]) referent;
			String modelName = (String) modelAndExtractorNames[0];
			String extractorName = (String) modelAndExtractorNames[1];
			
			File f = searchCalibrationFile (modelName, extractorName);
			if (f == null || !f.exists ()) {return false;}
			
			return true;
			
		} catch (Exception e) {
			//Log.println (Log.ERROR, "DECalibration.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	/**
	 * DataFormat interface.
	 * Extension interface.
	 */
	@Override
	public String getName () {
		return Translator.swap ("DECalibration.name");
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getAuthor () {
		return "F. de Coligny";
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getDescription () {
		return Translator.swap ("DECalibration.description");
	}

	/**
	 * Extension interface.
	 */
	@Override
	public String getVersion () {
		return "1.0";
	}
	
	// nb-09.08.2018
	public static final String VERSION = "";

	public String getModelName () {return modelName;}
	public String getExtractorName () {return extractorName;}
	
	//	
	//
	private File searchCalibrationFile (String modelName, String extractorName) {
		
		StringBuffer fileName = new StringBuffer (PathManager.getDir("class")); // fc-4.9.2012 was bin/ (wrong since 2010)
		fileName.append (File.separator);
		fileName.append (modelName);
		fileName.append (File.separator);
		fileName.append ("calibration");
		fileName.append (File.separator);
		fileName.append (extractorName);

		File f = new File (fileName.toString ());
		return f;
	}

	/**	Computes the data series. This is the real output building.
	*	Return false if trouble while extracting.
	*/
	public boolean doExtraction () {
		File f = searchCalibrationFile (modelName, extractorName);
		
		boolean xInteger = xIsInteger ();
		
		// 1. Build a Collection of Maps (x -> y)
		Collection series = new ArrayList ();
		
		try {
			ListMap serie = new ListMap ();	// one key -> a Collection of values - fc - 19.10.2006 [S. Perret]
			FileInputStream fis= new FileInputStream (f);
			BufferedReader in = new BufferedReader (new InputStreamReader (fis));
			String line = null;
			while ((line = in.readLine ()) != null) {
				
				// Comment line
				if (line.startsWith ("#")) {continue;}
				
				// Name for the x column (i.e. first column)
				if (line.toLowerCase ().startsWith ("xname")) {
					int pos = line.indexOf ("=");
					xName = line.substring (pos+1).trim ();
					continue;
				}
					
				// Name for the y column
				if (line.toLowerCase ().startsWith ("yname")) {
					int pos = line.indexOf ("=");
					yName = line.substring (pos+1).trim ();
					continue;
				}
					
				// One blank line between different curves
				if (line.length () == 0) {
					// blank line : memo serie and create a new one
					if (!serie.isEmpty ()) {
						series.add (serie);
						serie = new ListMap ();	// fc - 19.10.2006
					}
					continue;
				}
				
				try {
					Number[] n = new Number[2];	// (Integer, Double) or (Double, Double)
					StringTokenizer st = new StringTokenizer (line, "\t");
					String x = st.nextToken ();
					Number X = null;
					if (xInteger) {
						double d = new Double (x);
						if ((int) d != d) {
							// trouble, we are expecting an integer x and we find a double, tell the user
							Alert.print (Translator.swap ("DECalibration.warningInCalibrationFile")
									+"\n"
									+f
									+"\n"
									+Translator.swap ("DECalibration.expectingAnIntegerForXAndFoundADouble")
									+": "
									+x
									+"\n"
									+Translator.swap ("DECalibration.ignored"));
						}
						X = new Integer (x);
					} else {
						X = new Double (x);
					}
					
					String y = st.nextToken ();
					Number Y = new Double (y);
					
					//~ serie.put (X, Y);	// fc - 19.10.2006
					serie.addObject(X, Y);
					
				} catch (Exception e) {
					Log.println (Log.ERROR, "DECalibration.doExtraction ()", 
							"", e);
				}
			}
			
			// Memo last serie
			if (!serie.isEmpty ()) {
				series.add (serie);
			}

//System.out.println ();
//System.out.println ("DECalibration...");			
//for (Iterator i = series.iterator (); i.hasNext ();) {
//	ListMap sm = (ListMap) i.next ();
//	String[][] array = sm.toArray ();
//	for (int i1 = 0; i1 < array.length; i1++) {
//		for (int j1 = 0; j1 < array[i1].length; j1++) {
//			System.out.print (" "+array[i1][j1]);
//		}
//		System.out.println ();
//	}
//	System.out.println ();
//}
			
		} catch (IOException e) {
			Log.println (Log.ERROR, "DECalibration.doExtraction ()", 
					"error while reading file "+ f.getName (), e);
		}
		
		// 2. Build the ordered list of possible xs
		Set xs = new TreeSet ();
		int arrayLineNumber = 0;		// fc - 19.10.2006
		Map xTimes = new HashMap ();	// fc - 19.10.2006
		
		for (Iterator i = series.iterator (); i.hasNext ();) {
			// a ListMap is also a Map - fc - 19.10.2006
			ListMap serie = (ListMap) i.next ();
			for (Iterator k = serie.keySet ().iterator (); k.hasNext ();) {
				Number x = (Number) k.next ();
				xs.add (x);		// if same x found several times, added only once (Set) : ok
				
				Collection v = serie.getObjects(x);	// fc - 19.10.2006
				arrayLineNumber += v.size ();		// fc - 19.10.2006
				
				int size = v.size ();
				if (xTimes.containsKey (x)) {
					int prev = (Integer) xTimes.get (x);
					size = Math.max (prev, size);
				}
				
				xTimes.put (x, size);			// fc - 19.10.2006
			}
		}
		
		// make one vector of xs and 
		// one vector of ys per ListMap
		// add them in curves
		curves.clear ();
		Vector vx = new Vector ();
		curves.add (vx);
		for (Iterator i = xs.iterator (); i.hasNext ();) {
			Number x = (Number) i.next ();
			int n = (Integer) xTimes.get (x);
			for (int k = 0; k < n; k++) {
				vx.add (x);
			}
		}
		
		for (Iterator i = series.iterator (); i.hasNext ();) {
			ListMap serie = (ListMap) i.next ();
			Vector vy = new Vector ();
			curves.add (vy);
			
			for (Iterator j = xs.iterator (); j.hasNext ();) {
				Number x = (Number) j.next ();
				int times = 0;
				int max = (Integer) xTimes.get (x);	// how many lines max required for this x
				
				Collection c = (Collection) serie.get (x);	// some ys or null
				if (c != null) {
					for (Iterator k = c.iterator (); k.hasNext ();) {
						Number y = (Number) k.next ();
						vy.add (y);
						times++;	// one more y added
					}
				}
				
				for (int l = times; l < max; l++) {
					vy.add (Double.NaN);	// complete lines if needed with NaNs
				}
			}
			
		}
		
		return true;

	}

	//	Try to know if the x values are integers.
	//	Note: y values are always doubles.
	private boolean xIsInteger () {
		try {
			Collection extractors = dataBlock.getDataExtractors ();
			DFCurves representative = (DFCurves) extractors.iterator ().next ();
			
			List<List<? extends Number>> curves = representative.getCurves ();
			List<? extends Number> xLine = curves.get (0);
			// Integer values in x ?
			if (xLine.get (0) instanceof Integer) {return true;}
		} catch (Exception e) {
			Log.println (Log.ERROR, "DECalibration.xIsInteger ()", 
					"trying to know if x values are integers in first extractor of data block", e);
		}
		return false;
	}
	
	/**
	 * DataFormat interface.
	 */
	public String getDefaultDataRendererClassName () {return "";}
	
	/**
	 * DataFormat interface.
	 */
	public Color getColor () {
		return Color.GRAY;
	}
	
	/**
	 * DataFormat interface.
	 */
	public String getCaption () {
		String caption = "Calibration "+modelName;
		return caption;
	}

	
	/**
	 * DFCurves interface.
	 */
	public List<List<? extends Number>> getCurves () {
		return curves;
	}

	/**
	 * DFCurves interface.
	 */
	public List<List<String>> getLabels () {return null;}

	/**
	 * DFCurves interface.
	 */
	public List<String> getAxesNames () {
		Vector v = new Vector ();
		v.add (xName);
		v.add (yName);
		return v;
	}

	/**
	 * DFCurves interface.
	 */
	public int getNY () {
		return curves.size () - 1;
	}

	public void setConfigProperties () {}

}	
