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
package capsis.extension.datarenderer.barchart;

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ScrollPaneConstants;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.annotation.Param;
import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.extension.DataFormat;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Renders multiple bar charts (horizontal histograms).
 *
 * @author F. de Coligny - december 2004
 */
public class BarChart extends PanelDataRenderer implements Configurable {
	
	// fc-14.12.2016 reviewed lines below
	static {
		Translator.addBundle("capsis.extension.datarenderer.barchart.BarChart");
	}
	
	// nb-06.08.2018
	//static final public String NAME = Translator.swap ("BarChart"); 
	//static final public String VERSION = "1.0";
	//static final public String AUTHOR = "F. de Coligny";
	//static final public String DESCRIPTION = Translator.swap ("BarChart.description"); 
	
	
	@Param
	protected BarChartSettings settings;
	private Collection extractors;


		
	@Override
	public void init(DataBlock db) {
		super.init (db);
		retrieveSettings ();
		createUI ();
	}
	
	/**	Tell if the renderer can show an extractor's production.
	*	True if the extractor is an instance of the renderer's compatible data formats
	*	Note: DataExtractor must implement a data format in order to
	*	be recognized by DataRenderers.
	*/
	static public boolean matchWith (Object target) {
		if (target instanceof DataExtractor && target instanceof DFCurves) {
			return true;
		}
		return false;
	}

	@Override
	public String getName() {
		return Translator.swap("BarChart.name");
	}
	
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("BarChart.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**	Update strategy for subclasses of BarChart. 
	*	This method is used to refresh browser after configuration.
	*/
	public void update () {
		super.update ();	// fc - 2.4.2003
		removeAll ();

		
		// fc-30.10.2014 extractors may be unavailable on the step
		Collection extractors = dataBlock.getDataExtractors ();
		Collection specialExtractors = dataBlock.getSpecialExtractors ();

		System.out.println("BarChart update...");
		
		// fc-30.10.2014 if one of the extractors is not avaialble, send a message
		List specialAndNormalExtractors = new ArrayList(extractors);
		if (specialExtractors != null) { // fc - 14.12.2007
			specialAndNormalExtractors.addAll(specialExtractors);
		}
		for (Object e : specialAndNormalExtractors) {
			if (e instanceof DataFormat) {
				DataFormat df = (DataFormat) e;
				if (!df.isAvailable()) {
					addMessage(Translator.swap ("Shared.notAvailableOnThisStep"));
					return;
				}
			}
		}
		// fc-30.10.2014 extractors may be unavailable on the step


		
		add (createView (), BorderLayout.CENTER);
		
		revalidate ();
	}

	// Prints a warning message ("see configuration")
	//
	private void addMessage (String message) {
		LinePanel l1 = new LinePanel ();
		l1.add (new JLabel (message));
		l1.addGlue ();
		l1.setBackground (Color.WHITE);
		add (l1, BorderLayout.NORTH);
		revalidate ();
		repaint ();
		
		System.out.println("JFCBarChart wrote message: "+message);
	}

	//	Create the complex table component to be displayed
	//
	private JComponent createView () {
		try {

			extractors = dataBlock.getDataExtractors ();
			JComponent chart = createChart (extractors);
			return chart;	// one single chart with maybe several horizontal histograms inside
			
		} catch (Exception e) {
			Log.println (Log.ERROR, "BarChart.createView ()", "Exception ", e);
			return new JPanel ();
		}
	}
	
	//	Charts creation. 
	//	The chart will refresh with its paintComponent () method.
	//
	private JComponent createChart (Collection extractors) {
			
		if (extractors.size () <= 0) {
			return seeConfigurationPanel ();
			
		} else {	// one or more extractors - fc - 6.12.2004
			Set anchors = new TreeSet ();	// anchors are sorted in ascending order
			Map labelsMap = new HashMap ();	// anchor -> label
			
			String xAxisName = null;
			String yAxisName = null;
			
			int i = 0;
			Map[] mapex = new HashMap[extractors.size ()]; // tmp map : anchor -> bar
			
			// Consider each extractor
			//
			for (Iterator exs = extractors.iterator (); exs.hasNext ();) {
				DataExtractor e = (DataExtractor) exs.next ();
				DFCurves f = (DFCurves) e;
				
				// Axes names
				if (xAxisName == null) {	// all extractors in the block have same axes names
					xAxisName = (String) f.getAxesNames ().get (0);
					yAxisName = (String) f.getAxesNames ().get (1);
				}
				
				mapex[i] = new HashMap (); // tmp map : anchor -> bar
				
				List<List<? extends Number>> curves = f.getCurves ();
				List<List<String>> labels = f.getLabels ();	// may be null, see DFCurves
				
				// Controls: if trouble, stop now
				if (curves == null) {return seeConfigurationPanel ();}
				if (curves.size () < 2) {return seeConfigurationPanel ();}
				int curvesNumber = curves.size () - 1;
				
				// 1 extractor and 2 or more curves : general case
				//
				Iterator<List<? extends Number>> cs = curves.iterator ();
				
				// ixs : x values
				List<? extends Number> xs = cs.next ();	// curves
				Iterator<? extends Number> ixs = xs.iterator ();	// iterators on curves and labels
				
				// ils : x axis labels
				// ilys : y curves labels
				Iterator<String> ils = null;
				Iterator<String>[] ilys = new Iterator[curvesNumber];
				if (labels != null) {
					
					if (labels.size () >= 0){
						Iterator<List<String>> ilabs = labels.iterator ();
						List<String> l1 = ilabs.next ();	// first line of labels
						ils = l1.iterator ();
						int lysNumber = labels.size () - 1;
						for (int m = 0; m < lysNumber; m++) {
							ilys[m] = ilabs.next ().iterator ();	// other lines of labels (maybe none)
						}
					}
				}
				
				// iys : y curves valyes
				Iterator<? extends Number>[] iys = new Iterator[curvesNumber];
				int k = 0;
				while (cs.hasNext ()) {
					iys[k++] = cs.next ().iterator ();
				}
				
				// For each x, create an histogram bar
				//
				while (ixs.hasNext ()) {	// bars creation
					Object X = ixs.next ();
					double x = ((Number) X).doubleValue ();
					
					String label = null;
					if (ils != null && ils.hasNext ()) {label = (String) ils.next ();}
					if (label == null) {label = ""+x;}
					
					Bar b = new Bar (-1, -1, label, e.getColor ());	// -1 : see below
					
					// Y coordinates series
					double prevY = 0d;
					for (int l = 0; l < curvesNumber; l++) {
						Object Y = iys[l].next ();
						double y = ((Number) Y).doubleValue ();
						
						double v = y - prevY;	// see DFCurves cumulated histograms
						
						String note = ""+v;	// default note
						try {
							note = ilys[l].next ();
						} catch (Exception exc) {}	// if trouble, default note is used
						
						b.add (v, note);	// value, note
						prevY = y;
					}
					
					anchors.add (X);	// sorted, no dupplicates
					mapex[i].put (X, b);
					labelsMap.put (X, label);
				}
				
				i++;	// next extractor
			}
			
			// Process the bars, calculate their line & col
			Collection bars = new ArrayList ();	// bars under construction
			int line = 0;
			for (Object x: anchors) {
				for (int col = 0; col < extractors.size (); col++) {
					Bar b = (Bar) mapex[col].get (x);
					if (b != null) {
						b.setLineAndCol (line, col);
						bars.add (b);	// this bar is complete
					}
				}
				line++;
			}
			
			// Create the chart with all the bars
			Chart chart = new Chart (bars, xAxisName, yAxisName, extractors.size ());
			
			// Add one caption per extractor
			for (Iterator j = extractors.iterator (); j.hasNext ();) {
				DataExtractor e = (DataExtractor) j.next ();
				chart.addCaption (AmapTools.cutIfTooLong(e.getCaption (), 50), e.getColor ());
			}
			
			// Put the chart in a jscrollpane and add datarenderer contextual menu
			JScrollPane p = new JScrollPane (chart, 
					ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
					ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
			p.addMouseListener (this);	// for datarenderer ctx menu
			
			return p;
		}
	}
	
	/**	User interface creation.
	*/
	protected void createUI () {
		add (createView (), BorderLayout.CENTER);
	}

	/**	Create a message for the user : see configuration.
	*/
	private JPanel seeConfigurationPanel () {
		JPanel p1 = new JPanel (new BorderLayout ());
		p1.setBackground (Color.WHITE);
		LinePanel aux = new LinePanel ();
		aux.add (new JLabel (Translator.swap ("Shared.seeConfiguration")));
		aux.addGlue ();
		aux.setBackground (Color.WHITE);
		p1.add (aux, BorderLayout.NORTH);
		return p1;
	}

	// Check data
	//
	//~ private boolean dataAreCorrect (Collection extractors) {
		// see drcurves.dataAreCorrect ()...
	//~ }

	
	/**	From Configurable interface.
	*	Configurable interface allows to pass a parameter.
	*/
	public ConfigurationPanel getConfigPanel (Object parameter) {
		BarChartConfigurationPanel panel = new BarChartConfigurationPanel (this);
		return panel;
	}
	
	/**	From Configurable interface.
	*/
	public void applyConfig (ConfigurationPanel panel) {
		super.applyConfig (panel);	// DataRenderer configuration
		
		BarChartConfigurationPanel p = (BarChartConfigurationPanel) panel;
		settings.columnWidth = p.getColumnWidth ();
	}
	
	/**	From Configurable interface.
	*/
	public void postConfig () {
		ExtensionManager.recordSettings (this);
	}
	
	/**	Ask the extension manager for last version of settings for this extension type.
	*	redefinable by subclasses to get settings subtypes.
	*/
	protected void retrieveSettings () {
		settings = new BarChartSettings ();
		
	}

	public BarChartSettings getSettings () {return settings;}


	
	

}


/*	// fc - 6.12.2004 : svg before adaptation for several extractors.
	//	Charts creation
	//
	protected JComponent createChart (Collection extractors) {
		if (extractors.size () <= 0) {
			return new JLabel ("see Configuration");
			
		} else if (extractors.size () == 1) {
			DataExtractor e = (DataExtractor) extractors.iterator ().next ();
			DFCurves f = (DFCurves) e;
			
			Vector curves = f.getCurves ();
			Vector labels = f.getLabels ();	// may be null, see DFCurves
			
			if (curves == null) {return new JLabel ("error, curves == null");}
			if (curves.size () < 2) {return new JLabel ("error, curves.size () < 2");}
			
			// 1 extractor and 2 curves : simple case
			//
			if (curves.size () == 2) {
				Iterator cs = curves.iterator ();
				Vector xs = (Vector) cs.next ();	// curves
				Vector ys = (Vector) cs.next ();
				
				String xAxisName = (String) f.getAxesNames ().get (0);
				String yAxisName = (String) f.getAxesNames ().get (1);
				
				Iterator ixs = xs.iterator ();	// iterators on curves and labels
				Iterator iys = ys.iterator ();
				Iterator ils = null;
				if (labels != null) {
					Collection c = (Collection) labels;
					if (c.size () >= 0){
						Vector l1 = (Vector) c.iterator ().next ();	// first line of labels
						ils = l1.iterator ();
					}
				}
				
				Collection bars = new ArrayList ();
				int line = 0;
				while (ixs.hasNext () && iys.hasNext ()) {	// bars creation
					
					Object X = ixs.next ();
					double x = 0;
					if (X instanceof Integer) {
						x = ((Integer) X).doubleValue ();
					} else {
						x = ((Double) X).doubleValue ();
					}
					
					Object Y = iys.next ();
					double y = 0;
					if (Y instanceof Integer) {
						y = ((Integer) Y).doubleValue ();
					} else {
						y = ((Double) Y).doubleValue ();
					}
					
					String label = null;
					if (ils != null && ils.hasNext ()) {label = (String) ils.next ();}
					if (label == null) {label = ""+x;}
					
					Bar b = new Bar (line++, label, y, ""+y, e.getColor ());
					bars.add (b);
				}
				final Chart chart = new Chart (bars, xAxisName, yAxisName);
				
				JScrollPane p = new JScrollPane (chart, 
						ScrollPaneConstants.VERTICAL_SCROLLBAR_AS_NEEDED, 
						ScrollPaneConstants.HORIZONTAL_SCROLLBAR_NEVER);
				p.addMouseListener (this);	// for datarenderer ctx menu
				
				return p;
				
			// 1 extractor and several curves
			//
			} else {
				JTabbedPane charts = new JTabbedPane ();
				//return new JLabel ("error, curves.size () > 2");
				//~ Vector xs = (Vector) curves.iterator ().next ();	// curves
				
				//~ Vector ys = (Vector) curves.iterator ().next ();
				
				//~ int nCharts = xs.size ();	// several charts
				//~ for (int i = 0; i < nCharts; i++) {
					
					
					
					
					//~ Chart chart = new Chart (bars);
					//~ charts.addTab (title, chart);
				//~ }
				
				
				
				return charts;
			}
			
			
			
			
			
			
		// several extractors
		//
		} else {
			return new JLabel ("number of extractors : "+extractors.size ());
			
		}
	}

*/

