/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */

package capsis.extension.datarenderer.drcurves;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Event;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.MouseEvent;
import java.awt.geom.AffineTransform;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.awt.image.ImageObserver;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;

import javax.swing.JPanel;

import capsis.extension.DataFormat;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.format.DFColoredCurves;
import capsis.extension.dataextractor.format.DFCurves;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.util.Converter;
import capsis.util.Drawer;
import capsis.util.Interval;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColoredIcon;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A data renderer for data extractors implementing DFCurves : draws curves.
 * Version 2.
 * 
 * @author F. de Coligny - september 2003, reviewed may 2011
 */
public class DRCurves extends PanelDataRenderer implements Configurable, Drawer {

	// nb-06.08.2018
	//static final public String NAME = Translator.swap("DRCurves");
	//static final public String VERSION = "2.2";
	//static final public String AUTHOR = "F. de Coligny";
	//static final public String DESCRIPTION = Translator.swap("DRCurves.description");

	public static final int DASH_SIZE = 4;
	public static final int VERTICAL_INTER_ZONE = 5;
	public static final int HORIZONTAL_INTER_ZONE = 5;

	// fc-21.11.2014 Moved these properties here from DRSettings (removed class)
	public boolean enlargedMode;
	public boolean linesAsked;

	// fc-21.11.2014 In some graphs, the labels may be listed on the right and
	// the colors are not contrasted enough to see what curve / points series
	// they match. This option moves the right labels near their data
	public boolean alignRightLabelsOnData;

	// ___ new stuff _ do it without transforms
	// fc - 1.10.2003
	//

	// We make match a pixel space (rectangle) to the user space
	// (u2p / p2u methods)
	// Axes and captions are drawn around P space

	// User space U (double precision) : 4 points, width, height
	// u1 u2
	// uh
	// u0 uw u3
	//
	protected Vertex2d u0;
	protected Vertex2d u1;
	protected Vertex2d u2;
	protected Vertex2d u3;
	protected double uw;
	protected double uh;

	// Pixel space P (int precision) : 4 points, width, height
	// p1 p2
	// ph
	// p0 pw p3
	//
	protected Point p0;
	protected Point p1;
	protected Point p2;
	protected Point p3;
	protected int pw;
	protected int ph;

	// ___
	protected int panelWidth; // in pixels
	protected int panelHeight; // in pixels

	protected Map captionBounds; // fc - 12.5.2003 - caption rectangle ->
									// extractor (for mouse
									// selection)

	// @Param
	// REMOVED replaced by Settings.getProperty () fc-21.11.2014
	// protected DRCurvesSettings settings;

	protected double xMemo, yMemo;
	protected double lastKnownX; // coordinates of last valid point drawn
	protected double lastKnownY;

	protected double xPixel = 1;
	protected double yPixel = 1;

	protected Font userFont;

	private boolean xLabelledGraduation; // labels instead of numbers on x axis
											// (ex: "0-10",
											// "10-20"...)
	private boolean yRightLabelled; // labels at the right of the curves (ex:
									// "Hg", "Hdom"...)
	// fc - 21.3.2008
	private int rightLabelSize; // size in pixels of the largest right label in
								// relevant font (with
								// FontMetrics)

	private boolean yFullLabelled; // labels for each point (ex: "1994",
									// "1995"...)
	private ArrayList<String> xLabels;
	private ArrayList<Integer> xAnchors;

	// fc - 21.3.2005 - unused, removed
	// ~ protected int drawingWidth;
	// ~ protected int drawingHeight;
	// ~ protected int upperMargin;
	// ~ protected int lowerMargin;
	// ~ protected int rightMargin;
	// ~ protected int leftMargin;

	protected String xAxisLabel;
	protected String yAxisLabel;
	protected String zAxisLabel;
	protected boolean zAxisIsHere = false;

	protected double xStep = 1d; // never zero !
	protected double yStep = 1d; // never zero !
	protected double zStep = 1d; // never zero !

	protected Interval defaultXInterval;
	protected Interval defaultYInterval;
	protected Interval defaultZInterval;
	protected Interval xInterval;
	protected Interval yInterval;
	protected Interval zInterval;

	protected int minVariation;
	protected boolean xIsInteger;
	protected boolean yIsInteger;
	protected boolean zIsInteger;

	private ContentPanel contentPanel;
	private Collection extractors;
	private Collection specialExtractors; // fc - 12.10.2004
	private Collection specialAndNormalExtractors; // fc - 12.10.2004

	boolean forcedEnabled = false;
	double forcedXMin = Double.MIN_VALUE;
	double forcedXMax = Double.MAX_VALUE;
	double forcedYMin = Double.MIN_VALUE;
	double forcedYMax = Double.MAX_VALUE;

	private String extractorType;

	protected boolean someExtractorsAreColored; // fc - 16.6.2009

	private boolean forcedEdgesOverflow; // fc - 10.10.2006

	protected boolean displayCheckConfigurationMessage = false;

	static {
		Translator.addBundle("capsis.extension.datarenderer.drcurves.DRCurves");
	}

	@Override
	public void init(DataBlock db) {
		try {
			super.init(db);

			extractorType = getDataBlock().getExtractorType();

			updateProperties();

			// REMOVED replaced by Settings.getProperty () fc-21.11.2014
			// retrieveSettings();

			initLine();

			// default x and y ranges (for axis ranges)
			// may be set to some values by subclass
			defaultXInterval = null;
			defaultYInterval = null;
			minVariation = 4;

			createUI();

		} catch (Exception e) {// fc-20.1.2014
			Log.println(Log.ERROR, "DRCurves.init ()", "Error in init (), wrote in Log and passed", e);
		}

	}

	/**
	 * Tells if the renderer can show an extractor's production. True if the
	 * extractor is an instance of the renderer's compatible data formats Note:
	 * DataExtractor must implement a data format in order to be recognized by
	 * DataRenderers.
	 */
	static public boolean matchWith(Object target) {

		return target instanceof DataExtractor && target instanceof DFCurves;

	}
	
	@Override
	public String getName() {
		return Translator.swap("DRCurves.name");
	}
	
	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DRCurves.description");
	}

	@Override
	public String getVersion() {
		return "2.2";
	}
	
	/** Update curve properties */
	protected void updateProperties() {

		enlargedMode = Settings.getProperty(extractorType + ".enlargedMode", true); // fc-21.11.2014
		linesAsked = Settings.getProperty(extractorType + ".linesAsked", true); // fc-21.11.2014
		alignRightLabelsOnData = Settings.getProperty(extractorType + ".alignRightLabelsOnData", false); // fc-21.11.2014

		forcedEnabled = Settings.getProperty(extractorType + ".forcedEnabled", false);
		forcedXMin = Settings.getProperty(extractorType + ".forcedXMin", Double.MIN_VALUE);
		forcedXMax = Settings.getProperty(extractorType + ".forcedXMax", Double.MAX_VALUE);
		forcedYMin = Settings.getProperty(extractorType + ".forcedYMin", Double.MIN_VALUE);
		forcedYMax = Settings.getProperty(extractorType + ".forcedYMax", Double.MAX_VALUE);
	}

	/**
	 * Asks the extension manager for last version of settings for this
	 * extension type. redefinable by subclasses to get settings subtypes.
	 */
	// REMOVED replaced by Settings.getProperty () fc-21.11.2014
	// protected void retrieveSettings() {
	// settings = new DRCurvesSettings();
	//
	// }

	/**
	 * UI creation. Redefines super.createUI () : here we use a special
	 * ContentPanel.
	 */
	protected void createUI() {
		setBackground(Color.WHITE);
		setOpaque(true);
		setForeground(defaultColor);

		contentPanel = new ContentPanel(this); // drawing will be inside,
												// repaint () will trigger
												// refresh
		box = contentPanel;
		box.addMouseListener(this); // fc - 1.4.2003 - for click and ctrl-click
									// detection

		add(box, BorderLayout.CENTER);
	}

	/**
	 * New update strategy - fc - 27.3.2003
	 */
	public void update() {
		super.update(); // fc - 2.4.2003

		int w = getSize().width;
		int h = getSize().height;

		this.setPreferredSize(new Dimension(w, h));
		this.setSize(new Dimension(w, h));
		displayCheckConfigurationMessage = false;

		repaint();
	}

	/**
	 * Possible change in a subclass. Ex: histograms like their y axis to begin
	 * at least at zero. At least means they can also have negative bars.
	 */
	protected boolean yIsAtLeastZero() {
		return false;
	}

	/**
	 * Special case : each point has a label. If true, label of each point is
	 * drawn vertically
	 */
	protected boolean isYFullLabelledVertically() {
		return false;
	}

	/**
	 * MousePressed redefinition. First calls super.mousePressed (), then checks
	 * if evt was consumed. Here, we manage 2-clicks on extractor captions to
	 * trigger config dialog box. fc - 12.5.2003
	 */
	public void mousePressed(MouseEvent evt) {
		super.mousePressed(evt);

		// Evaluate evt if not consumed by superclass
		//
		if (!evt.isConsumed()) {
			if (evt.getClickCount() == 2) {
				Rectangle.Double selection = new Rectangle.Double(evt.getX(), evt.getY(), 1, 1);
				boolean ctrlDown = (evt.getModifiers() & Event.CTRL_MASK) != 0;
				select(selection, ctrlDown);
			}
		}

	}

	// Build intervals for x, ys and zs.
	// Process normal and special (calibration..) extractors
	//
	private Collection<DataExtractor> prepareData(Collection extractors) throws Exception {

		// fc - 6.6.2008
		// Make a list od "correct extractors", excluding those with no data
		// inside, etc.
		// One of thes correct extractors will be asked for axes names, x axis
		// labels, etc.
		Collection<DataExtractor> correctExtractors = new ArrayList<DataExtractor>();

		if (extractors.isEmpty()) {
			return correctExtractors;
		} // nothing to prepare, nothing
			// correct

		xIsInteger = false;
		yIsInteger = false;
		zIsInteger = false;

		double xMin = Double.MAX_VALUE;
		double yMin = Double.MAX_VALUE;
		double zMin = Double.MAX_VALUE;
		double xMax = -Double.MAX_VALUE;
		double yMax = -Double.MAX_VALUE;
		double zMax = -Double.MAX_VALUE;

		// 1. Min/max...
		// for each extractor
		for (Iterator it1 = extractors.iterator(); it1.hasNext();) {
			DFCurves data = (DFCurves) it1.next();

			// fc - 16.6.2009
			if (data instanceof DFColoredCurves) {
				someExtractorsAreColored = true;
			}

			List<List<? extends Number>> curves = data.getCurves();
			int curvesCount = curves.size() - 1;
			int yLinesCount = data.getNY();
			int zLinesCount = curvesCount - yLinesCount;

			// A. xLine (needed, only one line)
			//
			if (curves.isEmpty()) {
				continue;
			}

			List<? extends Number> xLine = curves.get(0);
			if (xLine.isEmpty()) { // fc - 19.10.2001
				// fc - 6.6.2008 - try differently (groups in Mustard)
				// move the extractor at the end of the collection...
				continue; // this extractor will not be considered "correct" -
							// fc - 6.6.2008
				// ~ throw new Exception
				// ("DRCurves.prepareData (): x vector is empty, can not draw, extractor="
				// ~ +data);
				// fc - 6.6.2008
			}

			// Integer values in x ?
			if (xLine.get(0) instanceof Integer) {
				xIsInteger = true;
			}

			// Looking for min/max value in x
			for (Iterator ite = xLine.iterator(); ite.hasNext();) {
				double d = 0d;
				if (xIsInteger) {
					Integer value = (Integer) ite.next();
					d = value.doubleValue();
				} else {
					Double value = (Double) ite.next();
					d = value.doubleValue();
				}

				if (d < xMin) {
					xMin = d;
				}
				if (d > xMax) {
					xMax = d;
				}
			}

			// ~ String extractorType = getDataBlock ().getExtractorType ();
			updateProperties();

			if (forcedEnabled && forcedXMin != Double.MIN_VALUE) {
				xMin = forcedXMin;
			}
			if (forcedEnabled && forcedXMax != Double.MAX_VALUE) {
				xMax = forcedXMax;
			}

			xInterval = new Interval(xMin - 0.5, xMax + 0.5); // trying to
																// enlarge to
																// have a better
																// rendering
																// later
			// xInterval = new Interval (xMin, xMax);

			// B. Then, some yLines (maybe several)
			//
			for (int i = 1; i <= yLinesCount; i++) {
				int ind = 1 + (i - 1); // jumps xLine
				List<? extends Number> yLine = curves.get(ind);
				// Integer values in y ?
				if (i == 1 && yLine.get(0) instanceof Integer) {
					yIsInteger = true;
				}
				// Looking for min/max value in y
				// Preparing points vector at the same time
				int k = 0;
				for (Iterator ite = yLine.iterator(); ite.hasNext();) {
					double d = 0d;
					if (yIsInteger) {
						Integer value = (Integer) ite.next();
						d = value.doubleValue();
					} else {
						Double value = (Double) ite.next();
						d = value.doubleValue();
					}
					if (d < yMin) {
						yMin = d;
					}
					if (d > yMax) {
						yMax = d;
					}
				}

			}

			// if a curve is full of Double.NaN, choose a default interval for
			// y: [0,10]
			if (yMin == Double.MAX_VALUE || yMax == -Double.MAX_VALUE) {
				yMin = 0; // fc-27.3.2012
				yMax = 10; // fc-27.3.2012
				displayCheckConfigurationMessage = true;
			}

			// System.out.println("DRCurves yMin: "+yMin+" yMax: "+yMax);

			if (forcedEnabled && forcedYMin != Double.MIN_VALUE) {
				yMin = forcedYMin;
			}
			if (forcedEnabled && forcedYMax != Double.MAX_VALUE) {
				yMax = forcedYMax;
			}
			if (yIsAtLeastZero()) {
				yMin = Math.min(yMin, 0d);
			}

			// Do not enlarge yMin and yMax to yMin - 0.5 and yMax + 0.5
			// (some curves and histograms would look strange)
			yInterval = new Interval(yMin, yMax);

			// C. Then, some zLines (maybe none : optional)
			//
			for (int i = 1; i <= zLinesCount; i++) {
				int ind = 1 + yLinesCount + (i - 1); // jumps xLine + yLines
				List<? extends Number> zLine = curves.get(ind);
				// Integer values in z ?
				if (i == 1 && zLine.get(0) instanceof Integer) {
					zIsInteger = true;
				}
				// Looking for min/max value in z
				int k = 0;
				for (Iterator ite = zLine.iterator(); ite.hasNext();) {
					double d = 0d;
					if (zIsInteger) {
						Integer value = (Integer) ite.next();
						d = value.doubleValue();
					} else {
						Double value = (Double) ite.next();
						d = value.doubleValue();
					}
					if (d < zMin) {
						zMin = d;
					}
					if (d > zMax) {
						zMax = d;
					}
				}
			}
			zInterval = new Interval(zMin, zMax);

			DataExtractor ex = (DataExtractor) data;
			correctExtractors.add(ex);
		}

		// fc - 6.6.2008
		return correctExtractors;
	}

	/**
	 * From Drawer interface.
	 */
	public JPanel select(Rectangle.Double selection, boolean ctrlDown) {
		try {
			for (Iterator i = captionBounds.keySet().iterator(); i.hasNext();) {
				Rectangle r = (Rectangle) i.next();
				if (selection.intersects(r)) {
					// ~ DataExtractor ex = (DataExtractor) captionBounds.get
					// (r);
					// ~ System.out.println
					// ("mouse selection for caption="+ex.getCaption ());

					openConfigure(INDIVIDUAL_FIRST);
					return null;
				}
			}

			openConfigure(LAST_OPENED_FIRST); // fc - 13.3.2006

			return null;
		} catch (Exception e) {
		}
		return null;
	}

	/**
	 * From Drawer interface. Prepares the data is necessary, then draw the
	 * curves. Called by the paintComponent () method of the drawing DrawerZone.
	 */
	public void draw(Graphics g, Rectangle.Double r) {
		this.panelWidth = getWidth();
		this.panelHeight = getHeight();

		// 1. if asked, draw gray lines in drawing according to y axis
		if (linesAsked) {
			Graphics2D g2 = (Graphics2D) g;
			Color originalColor = g2.getColor();
			g2.setColor(Color.lightGray);

			g2.setStroke(new BasicStroke(1f));
			g2.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);

			// Security
			if (yInterval == null) {
				Log.println(Log.ERROR, "DRCurves.draw ()", "yInterval = null");
				return;
			}

			for (double i = yInterval.a; i <= yInterval.b + yStep / 2; i += yStep) {
				g2.draw(new Line2D.Double(u2px(xInterval.a), u2py(i), u2px(xInterval.b), u2py(i)));
			}

			if (xLabelledGraduation) {
				for (Iterator i = xAnchors.iterator(); i.hasNext();) {
					double x = (double) ((Integer) i.next()).intValue();
					g2.draw(new Line2D.Double(u2px(x), u2py(yInterval.a), u2px(x), u2py(yInterval.b)));
				}
				g2.draw(new Line2D.Double(u2px(xInterval.b), u2py(yInterval.a), u2px(xInterval.b), u2py(yInterval.b)));
			} else {
				for (double i = xInterval.a; i <= xInterval.b + xStep / 2; i += xStep) {
					g2.draw(new Line2D.Double(u2px(i), u2py(yInterval.a), u2px(i), u2py(yInterval.b)));
				}
			}

			g2.setColor(originalColor);
		}

		// 2. draw curves
		// ~ drawCurves (g, extractors);
		drawCurves(g, specialAndNormalExtractors);

		// fc-12.10.2015 REMOVED
//		// 3. draw extra data series if found
//		// fc-21.9.2015
//		drawDataSeries(g, r, specialAndNormalExtractors);

	}

	// fc-12.10.2015 REMOVED
//	// fc-21.9.2015
//	public void drawDataSeries(Graphics g, Rectangle.Double r, Collection allExtractors) {
//		Graphics2D g2 = (Graphics2D) g;
//		Color memoColor = g2.getColor();
//
//		// System.out.println("DRCurves drawDataSeries...");
//
//		for (Object o : allExtractors) {
//			try {
//				AbstractDataExtractor de = (AbstractDataExtractor) o;
//				// System.out.println("DRCurves drawDataSeries extractor: "+de);
//
//				List<XYSeries> listOfDataSeries = de.getListOfDataSeries();
//				if (listOfDataSeries != null) {
//					// System.out.println("DRCurves drawDataSeries #series: "+listOfDataSeries.size
//					// ());
//
//					for (XYSeries series : listOfDataSeries) {
//
//						// System.out.println(series.toString ());
//
//						Color seriesColor = series.getColor();
//						g2.setPaint(seriesColor);
//
//						initLine();
//						for (Vertex2d v : series.getPoints()) {
//							line(g2, u2px(v.x), u2py(v.y));
//
//						}
//
//						// Series name on the right
//						g2.drawString(series.getName(), (float) xMemo, (float) yMemo);
//
//					}
//				}
//			} catch (Exception e) {
//				// ignore
//			}
//		}
//
//		g2.setPaint(memoColor);
//
//	}
//
//	// fc-21.9.2015

	/**
	 * Draws all the curves for all the extractors in the browser's
	 * contentPanel's drawing's Graphics. (i.e. drawing size).
	 */
	public void drawCurves(Graphics g, Collection extractors) {

		// fc - 21.3.2005 - pw and ph must have been set before calling this
		// method :
		// refreshing the DRCurves goes through repaint () ->
		// ContentPanel.paintComponent () ->
		// ContentPanel.update (Graphics, int, int) which calculates pixel Space
		// -> DRCurves.draw ()
		// ->
		// DRCurves.drawCurves ()
		Graphics2D g2 = (Graphics2D) g;

		// Font transform (shifts, rotations...)
		Font font = g2.getFont();

		// ~ AffineTransform at = font.getTransform ();
		// 111111111111111111111111111111111111111111111111111111111111111111111111111111111
		AffineTransform at = new AffineTransform();
		at.setToIdentity();

		if (yRightLabelled) {
			at.translate(2d, 2d);
		}
		if (yFullLabelled) {
			if (isYFullLabelledVertically()) {
				at.translate(2d, 2d);
				at.rotate(Math.PI / 2);
			} else { // else, horizontal label
				FontMetrics fm = getFontMetrics(font);
				int fontAscent = fm.getAscent();

				at.translate(0d, fontAscent - 2); // ...
			}
		}

		// 222222222222222222222222222222222222222222222222222222222222222222222222222222222
		Font drawFont = font.deriveFont(Font.PLAIN, at);
		g2.setFont(drawFont);

		// Pen width for y axis
		g2.setStroke(new BasicStroke(1f)); // width for lines in y

		int extrRank = 0;
		int extrCount = extractors.size();

		Iterator pLabels = null; // if yFullLabels, one label for each point in
									// pLabels

		// fc - 21.3.2005 - Calculate numberOfBars
		int numberOfBars = 0; // may stay zero in case of trouble
		for (Iterator allTheExtractors = extractors.iterator(); allTheExtractors.hasNext();) {
			DFCurves e = (DFCurves) allTheExtractors.next();
			try {
				Collection c = (Collection) e.getCurves().iterator().next(); // x
																				// coordinates
				numberOfBars = Math.max(numberOfBars, c.size());
			} catch (Exception exc) {
			}
		}
		// fc - 21.3.2005 - Calculate numberOfBars

		Color currentColor = null;

		for (Iterator allTheExtractors = extractors.iterator(); allTheExtractors.hasNext();) {
			DFCurves extr = (DFCurves) allTheExtractors.next();
			extrRank++; // i th extractor

			// We need the number of curves in the extrator to change histogram
			// appearance
			// in case of several curves (for cumulated histograms) in
			// DRHistogram subclass
			// fc - 23.7.2002
			//
			int curveCount = extr.getNY(); // number of Y coordinates vectors (Z
											// not processed
											// here)

			// Extractor color
			currentColor = ((DataExtractor) extr).getColor();
			g2.setPaint(currentColor);

			// Colored curves - [JacquesLabonne] - fc - 9.2.2006
			Iterator colors = null;
			if (extr instanceof DFColoredCurves) {
				colors = ((DFColoredCurves) extr).getColors().iterator();
			}

			Iterator<List<? extends Number>> curves = extr.getCurves().iterator();
			Iterator<List<String>> labels = null;
			if (yRightLabelled || yFullLabelled) {
				labels = extr.getLabels().iterator();
				List<String> xLabelVector = labels.next(); // jump over unused x
															// labels
			}

			List<? extends Number> vxs = curves.next();
			int curveRank = 0; // i th curve

			boolean zTransformDone = false;

			forcedEdgesOverflow = false; // fc - 10.10.2006
			while (curves.hasNext()) {
				curveRank++;
				// secutity: z curves found where they should not be found ->
				// ignore
				if (!zAxisIsHere && curveRank > extr.getNY()) {
					break;
				}

				List<? extends Number> vys = curves.next(); // might be zs

				// Colored curves - [JacquesLabonne] - fc - 9.2.2006
				if (extr instanceof DFColoredCurves) {
					currentColor = (Color) colors.next();

					// REMOVED the line below, disturbed P. Vallet in Melies,
					// finally not very
					// convenient fc-1.2.2012
					// Calculate a color from the extractor color hue and the
					// coloredCurve
					// saturation and brightness
					// currentColor = DefaultColorProvider.getGradientColor
					// (((DataExtractor)
					// extr).getColor (), currentColor);

					g2.setPaint(currentColor);
				}

				if (yRightLabelled || yFullLabelled) {
					pLabels = ((Collection) labels.next()).iterator(); // one
																		// line
																		// of
																		// labels
																		// for
																		// each
																		// point
				}

				if (curveRank > extr.getNY() && !zTransformDone) {
					// We leave y axis to attack z axis curves (new transform &
					// pen size)
					zTransformDone = true;

					Color col = g2.getColor().brighter();
					g2.setColor(col);
					g2.setStroke(new BasicStroke(1f)); // width for lines in z
				}

				Iterator xs = vxs.iterator();
				Iterator ys = vys.iterator();

				initLine();
				while (xs.hasNext() && ys.hasNext()) {
					double x = Converter.create(xs.next()).doubleValue();
					double y = Converter.create(ys.next()).doubleValue();

					String label = "";
					if (yFullLabelled) {
						label = (String) pLabels.next();
					}

					if (!Double.isNaN(x) && !Double.isNaN(y)) {

						updateProperties();
						if (forcedEnabled) {
							if (forcedXMin != Double.MIN_VALUE && x < forcedXMin) {
								forcedEdgesOverflow = true;
							}
							if (forcedXMax != Double.MAX_VALUE && x > forcedXMax) {
								forcedEdgesOverflow = true;
							}
							if (forcedYMin != Double.MIN_VALUE && y < forcedYMin) {
								forcedEdgesOverflow = true;
							}
							if (forcedYMax != Double.MAX_VALUE && y > forcedYMax) {
								forcedEdgesOverflow = true;
							}
						}

						// Draw one point (line / bar)
						double newX = draw(g2, extrRank, extrCount, x, y, u2px(x), u2py(y), curveRank, curveCount, pw,
								numberOfBars);

						lastKnownX = x;
						lastKnownY = y;

						if (yFullLabelled) {
							Color g2Color = g2.getColor();
							if (extr instanceof DFColoredCurves) { // fc -
																	// 9.2.2006
								g2.setColor(((DataExtractor) extr).getColor()); // write
																				// the
																				// label
																				// with
																				// the
																				// extractor
																				// color
							} else {
								g2.setColor(Color.BLACK); // write the label in
															// black
							}
							g2.drawString(label, (float) newX, (float) u2py(y));
							g2.setColor(g2Color);
						}
					}
				}

				if (yRightLabelled && (lastKnownX != Double.MIN_VALUE) && (lastKnownY != Double.MIN_VALUE)) {

					String label = (String) pLabels.next();

					writeRightLabel(g2, extr, extrRank, extrCount, curveRank, curveCount, lastKnownX, lastKnownY,
							label, currentColor);

					// Color g2Color = g2.getColor ();
					// if (extr instanceof DFColoredCurves) { // fc - 9.2.2006
					// g2.setColor (((DataExtractor) extr).getColor ()); //
					// write the label with
					// // the extractor color
					// } else {
					// g2.setColor (Color.BLACK); // write the label in black
					// }
					//
					// g2.drawString (label, (float) u2px (lastKnownX), (float)
					// u2py (lastKnownY));
					// g2.setColor (g2Color);

				}
			}
		}
	}

	/**
	 * Writes the right label right to the (x,y) representation or in some other
	 * place (called only if there is a right label).
	 */
	protected void writeRightLabel(Graphics2D g2, DFCurves extr, int extrRank, int extrCount, int curveRank,
			int curveCount, double lastKnownX, double lastKnownY, String label, Color currentColor) {

		// fc-21.11.2014 Added an option to force right labels alignment on the
		// curve / point series, in case the colors are not contrasted enough to
		// see clearly what matches what: settings.alignRightLabelsOnData (PhD
		// question)
		if (extr instanceof DFColoredCurves && !alignRightLabelsOnData) {

			if (extrRank > 1)
				return;

			// For colored curves, write the label somewhere in the graph
			// Only for the first extractor, take care of the colors matching

			g2.setColor(currentColor);

			double yAnchor = p2.y + curveRank * 15;

			g2.drawString(label, (float) u2px(lastKnownX), (float) yAnchor);

		} else {

			g2.setColor(Color.BLACK);

			g2.drawString(label, (float) u2px(lastKnownX), (float) u2py(lastKnownY));

			g2.setColor(currentColor);

		}

	}

	// Inits variables later used by line ().
	//
	private void initLine() {
		xMemo = Double.MIN_VALUE;
		yMemo = Double.MIN_VALUE;
		lastKnownX = Double.MIN_VALUE;
		lastKnownY = Double.MIN_VALUE;
	}

	/**
	 * This method plots.
	 */
	protected void plot(Graphics2D g2, double x, double y) {
		if (Double.isNaN(x) || Double.isNaN(y)) {
			return;
		}

		// Draws a little square "3" pixels width centered on the plot
		if (enlargedMode) {
			// - begin - classical method
			Rectangle2D r = new Rectangle2D.Double();
			r.setRect(x - xPixel, y - yPixel, xPixel + xPixel, yPixel + yPixel);
			g2.fill(r);
			g2.draw(r);
			// - end - classical method

			// Draws a single pixel
		} else {

			// fc-21.11.2014 If not enlargedMode, do not draw the point

			// Rectangle2D.Double p = new Rectangle2D.Double(); // fc -
			// 11.10.2004
			// p.setRect(x, y, 1d, 1d);
			// g2.draw(p);
		}
	}

	// Draws a line from last plot to this coordinates.
	//
	private void line(Graphics2D g2, double x, double y) {
		if (Double.isNaN(x) || Double.isNaN(y)) {
			return;
		}

		if (xMemo == Double.MIN_VALUE && yMemo == Double.MIN_VALUE) {
			plot(g2, x, y);
		} else {
			plot(g2, x, y);
			g2.draw(new Line2D.Double(xMemo, yMemo, x, y));
		}
		xMemo = x;
		yMemo = y;
	}

	/**
	 * This method draws the point. It may be redefined in subclasses to draw
	 * differently. DRCurves draws lines between points to represent curves. We
	 * are working on the extrRank th extractor among extrCount.
	 */
	protected double draw(Graphics2D g2, int extrRank, int extrCount, double ux, double uy, double px, double py,
			int curveRank, int curveCount, int pw, int numberOfBars) {

		if (Double.isNaN(px) || Double.isNaN(py)) {
			return px;
		}

		line(g2, px, py);
		return px; // unchanged here
	}

	/**
	 * From Configurable interface. Configurable interface allows to pass a
	 * parameter.
	 */
	@Override
	public ConfigurationPanel getConfigPanel(Object parameter) {
		DRCurvesConfigurationPanel panel = new DRCurvesConfigurationPanel(this, extractorType);
		return panel;
	}

	/**
	 * From Configurable interface.
	 */
	@Override
	public void applyConfig(ConfigurationPanel panel) {
		super.applyConfig(panel); // DataRenderer configuration

		DRCurvesConfigurationPanel p = (DRCurvesConfigurationPanel) panel;

		enlargedMode = p.isEnlargedMode();
		linesAsked = p.isLinesAsked();
		alignRightLabelsOnData = p.isAlignRightLabelsOnData();

		String extractorType = getDataBlock().getExtractorType();

		// fc-21.11.2014
		Settings.setProperty(extractorType + ".enlargedMode", enlargedMode);
		Settings.setProperty(extractorType + ".linesAsked", linesAsked);
		Settings.setProperty(extractorType + ".alignRightLabelsOnData", alignRightLabelsOnData);

		Settings.setProperty(extractorType + ".forcedEnabled", "" + p.isForcedEnabled());
		Settings.setProperty(extractorType + ".forcedXMin", "" + p.getForcedXMin());
		Settings.setProperty(extractorType + ".forcedXMax", "" + p.getForcedXMax());
		Settings.setProperty(extractorType + ".forcedYMin", "" + p.getForcedYMin());
		Settings.setProperty(extractorType + ".forcedYMax", "" + p.getForcedYMax());

	}

	/**
	 * From Configurable interface.
	 */
	@Override
	public void postConfig() {
		ExtensionManager.recordSettings(this);
	}

	protected int u2px(double ux) {
		return (int) ((ux - u0.x) / uw * pw + p0.x);
	}

	protected int u2py(double uy) {
		return panelHeight - (int) ((uy - u0.y) / uh * ph + (panelHeight - p0.y));
	}

	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ///////////////////////////////////// Content panel below
	// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
	// ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

	/**
	 * Panel where data are drawn.
	 */
	public class ContentPanel extends JPanel {

		private Drawer drawer;

		/**
		 * Constructor
		 */
		public ContentPanel(Drawer drw) {
			super(true); // doubleBuffered = true
			drawer = drw;

			setLayout(null);
			setOpaque(true);
			// setBackground (Color.YELLOW);
			setBackground(Color.WHITE);

			setDoubleBuffered(true); // try to get faster
		}

		/**
		 * Entry point for viewer refresh. Contains recomputation of extractors
		 * if needed, scales, transforms preparation and creation of components
		 * for drawing, axes and graduations. Adding these components in a
		 * realized one (ContentPanel) results in calling their paintComponent
		 * () method which draw themselves with available place.
		 */
		public void paintComponent(Graphics g) {
			super.paintComponent(g);

			Font gFont = g.getFont();
			userFont = new Font(gFont.getName(), gFont.getStyle(), gFont.getSize() - 2); // main
																							// font
																							// used
																							// to
																							// write
																							// in
																							// the
																							// panels

			g.setFont(userFont); // fc - NEW

			Container parent = this.getParent();
			update(g, parent.getWidth(), parent.getHeight()); // triggers
																// refresh

		}

		// Main refresh work (axes, grads, drawing) is grouped in this method.
		private void update(Graphics g, int panelWidth, int panelHeight) {

			try { // fc-20.1.2014

				// System.out.println ("DRCurves.update ()...");

				Graphics2D g2 = (Graphics2D) g; // here, we draw directly
												// caption, axes with their
												// graduations in it

				this.removeAll(); // removes all components before redrawing
				setLayout(null);

				extractors = dataBlock.getDataExtractors();
				specialExtractors = dataBlock.getSpecialExtractors(); // fc -
																		// 12.10.2004

				specialAndNormalExtractors = new ArrayList(extractors);
				if (specialExtractors != null) { // fc - 14.12.2007
					specialAndNormalExtractors.addAll(specialExtractors);
				}

				// fc-30.10.2014 if one of the extractors is not avaialble, send
				// a message
				for (Object e : specialAndNormalExtractors) {
					if (e instanceof DataFormat) {
						DataFormat df = (DataFormat) e;
						if (!df.isAvailable()) {
							addMessage(g, Translator.swap("Shared.notAvailableOnThisStep"));
							return;
						}
					}
				}

				// // fc-6.10.2015
				// Object representative = specialAndNormalExtractors.iterator
				// ().next ();

				// 0. Check data
				// if (representative instanceof DFCurves) {
				if (!DataChecker.dataAreCorrect(extractors)) {
					Log.println(Log.ERROR, "DRCurves.update ()",
							"Error while checking data to be represented with DataChecker");
					addWarningMessage(g);
					return;
				}
				// }

				// 1. Prepare data : type double/int and min/max values in x, y,
				// z (intervals)
				Collection<DataExtractor> correctExtractors = null;
				try {
					correctExtractors = prepareData(specialAndNormalExtractors);
				} catch (Exception exc) {
					Log.println(Log.ERROR, "DRCurves.update ()",
							"Error while getting axes types and extreme values (prepareData ())", exc);
					addWarningMessage(g);
					return;
				}
				if (extractors == null) {
					addWarningMessage(g);
					return;
				} // no extractors

				if (xInterval == null || yInterval == null || (zAxisIsHere && yInterval == null)) {
					return;
				}

				// 2. General updates (axes names...)

				// fc - 6.6.2008 - choose a representative among the
				// "correct extractors"
				if (correctExtractors == null || correctExtractors.isEmpty()) {
					correctExtractors = extractors;
				}
				// ~ DFCurves representative = (DFCurves) extractors.iterator
				// ().next (); // Note:
				// special extractors are aside
				DFCurves representative = (DFCurves) correctExtractors.iterator().next(); // Note:
																							// special
																							// extractors
																							// are
																							// aside

				// Ask common data to the representative
				if (representative == null) {
					Log.println(Log.ERROR, "DRCurves.update ()", "DataBlock representative = null (" + representative
							+ ").");
					addWarningMessage(g);
					return;
				}
				List<String> axesNames = representative.getAxesNames();

				if (axesNames == null) {
					Log.println(Log.ERROR, "DRCurves.update ()", "Wrong value in DFCurves DataExtractor ("
							+ representative + "). axesNames = null");
					addWarningMessage(g);
					return;
				}
				if (axesNames.size() >= 1) {
					xAxisLabel = (String) axesNames.get(0);
				}
				if (axesNames.size() >= 2) {
					yAxisLabel = (String) axesNames.get(1);
				}
				if (axesNames.size() >= 3) {
					zAxisLabel = (String) axesNames.get(2);
					zAxisIsHere = true;
				}

				// fc - 13.3.2006 - forcedEnabled: limits on x and y axis (from
				// x0 to x1...)
				// -> adapt axes names
				boolean f0 = false;
				boolean f1 = false;
				boolean f2 = false;
				boolean f3 = false;

				// System.out.println
				// ("DRCurves.update () updating properties...");

				updateProperties();

				// System.out.println ("DRCurves.update () updated");

				if (forcedEnabled && forcedXMin != Double.MIN_VALUE) {
					f0 = true;
				}
				if (forcedEnabled && forcedXMax != Double.MAX_VALUE) {
					f1 = true;
				}
				if (forcedEnabled && forcedYMin != Double.MIN_VALUE) {
					f2 = true;
				}
				if (forcedEnabled && forcedYMax != Double.MAX_VALUE) {
					f3 = true;
				}
				NumberFormat nf = NumberFormat.getInstance(Locale.ENGLISH); // fc
																			// -
																			// 13.3.2006
				nf.setMaximumFractionDigits(2);
				nf.setMinimumFractionDigits(0);
				nf.setGroupingUsed(false);

				if (f0 && f1) {
					xAxisLabel += " (" + Translator.swap("DRCurves.fromInFromTo") + " " + nf.format(forcedXMin);
					xAxisLabel += " " + Translator.swap("DRCurves.toInFromTo") + " " + nf.format(forcedXMax) + ")";
				} else if (f0) {
					xAxisLabel += " (" + Translator.swap("DRCurves.from") + " " + nf.format(forcedXMin) + ")";
				} else if (f1) {
					xAxisLabel += " (" + Translator.swap("DRCurves.to") + " " + nf.format(forcedXMax) + ")";
				}
				if (f2 && f3) {
					yAxisLabel += " (" + Translator.swap("DRCurves.fromInFromTo") + " " + nf.format(forcedYMin);
					yAxisLabel += " " + Translator.swap("DRCurves.toInFromTo") + " " + nf.format(forcedYMax) + ")";
				} else if (f2) {
					yAxisLabel += " (" + Translator.swap("DRCurves.from") + " " + nf.format(forcedYMin) + ")";
				} else if (f3) {
					yAxisLabel += " (" + Translator.swap("DRCurves.to") + " " + nf.format(forcedYMax) + ")";
				}

				// System.out.println ("DRCurves.update () labels...");

				// Labels in x
				xLabelledGraduation = false;

				// Labels in y (& z: it's the same)
				FontMetrics fm = getFontMetrics(userFont); // MOVED here - fc -
															// 21.3.2008
				// ~ System.out.println
				// ("DRCurves:944 TEST fm.stringWidth (tagada)="+fm.stringWidth
				// ("tagada"));
				yRightLabelled = false;
				yFullLabelled = false;
				if (representative.getLabels() != null && !representative.getLabels().isEmpty()) {

					List<List<String>> labelVectors = representative.getLabels();
					Iterator<List<String>> i = labelVectors.iterator();
					if (i.hasNext()) { // xLabels
						List<String> xLabels = i.next();
						if (xLabels != null && !xLabels.isEmpty()) {
							xLabelledGraduation = true;
						}
					}
					if (i.hasNext()) { // yLabels
						// fc - 21.3.2008 - rightLabelSize
						rightLabelSize = -1;
						while (i.hasNext()) {
							List<String> yLabels = i.next();
							// Are we in rightLabel mode ?
							if (!yRightLabelled && yLabels.size() == 1) { // first
																			// yLabel
																			// vector
								yRightLabelled = true;
							}
							// What is the largest rightLabel ?
							if (yRightLabelled) {
								String rightLabel = (String) yLabels.iterator().next();
								rightLabelSize = Math.max(rightLabelSize, fm.stringWidth(rightLabel));
								// ~ System.out.println
								// ("DRCurves:968 rightLabel="+rightLabel+" size="+fm.stringWidth
								// (rightLabel)+" max="+rightLabelSize);
							}
						}
						// fc - 21.3.2008 - rightLabelSize
						yFullLabelled = !yRightLabelled;
					}
				}

				// System.out.println ("DRCurves.update () labels done");

				// ~ System.out.println
				// ("DRCurves:976 xLabelledGraduation="+xLabelledGraduation+" yRightLabelled="+yRightLabelled+" rightLabelSize="+rightLabelSize);

				// System.out.println ("DRCurves.update () sizes...");

				// 3. Compute size of the main drawing.
				// ~ FontMetrics fm = getFontMetrics (userFont); // MOVED upper
				// - fc - 21.3.2008
				int userFontAscent = fm.getAscent();
				int userFontDescent = fm.getDescent();
				int userFontHeight = userFontAscent + userFontDescent;

				int vInter = VERTICAL_INTER_ZONE; // to "space away" components
													// vertically
				int hInter = HORIZONTAL_INTER_ZONE; // to "space away"
													// components horizontally

				int hCaption = specialAndNormalExtractors.size() * userFontHeight; // pre-copmputed

				if (!getDataBlock().isCaptionRequired()) { // fc - 29.4.2003 -
															// caption becomes
															// optionnal
					hCaption = 0;
				}

				int wYName = fm.stringWidth(yAxisLabel);
				// ~ int hYName = userFontHeight+TextZone.MARGIN; // font height
				int hYName = userFontHeight; // font height
				int wYAxis = DASH_SIZE + 1; // graduation size + 1 for axis line

				int wYGrad = 1;
				if (yInterval != null) {
					wYGrad = GraduationLengthApproximer.approximateGradLength(yInterval, fm, yIsInteger); // heuristic
				}
				if (yIsInteger) {
					wYGrad += fm.stringWidth("5");
				}

				int hXAxis = DASH_SIZE + 1; // graduation size + 1 for axis line

				// hXGrad: if labels in representative.getLabels () first
				// vector, they are
				// x graduation text : consider the longest one
				int hXGrad = 1;
				if (xInterval != null) {
					hXGrad = GraduationLengthApproximer.approximateGradLength(xInterval, fm, xIsInteger); // heuristic
				}

				if (xLabelledGraduation) {
					SortedMap<Integer, String> map = new TreeMap<Integer, String>();

					for (Iterator i = extractors.iterator(); i.hasNext();) {
						DFCurves extr = (DFCurves) i.next();
						List<? extends Number> firstAnchorsList = extr.getCurves().iterator().next();
						List<String> firstLabelsList = extr.getLabels().iterator().next();

						Iterator<? extends Number> as = firstAnchorsList.iterator();
						Iterator<String> ls = firstLabelsList.iterator();
						while (as.hasNext() && ls.hasNext()) {
							Integer a = (Integer) as.next();
							String l = ls.next();
							map.put(a, l);
						}

						xAnchors = new ArrayList<Integer>(map.keySet());
						xLabels = new ArrayList<String>(map.values());
					}

					int wMax = 0;
					for (Iterator i = xLabels.iterator(); i.hasNext();) {
						String label = (String) i.next();
						int w = fm.stringWidth(label);
						if (w > wMax) {
							wMax = w;
						}
					}
					hXGrad = wMax;
				}

				int wXName = fm.stringWidth(xAxisLabel);
				int hXName = userFontHeight; // font height

				int wZAxis = 0;
				int wZGrad = 0;
				int wZTotalStuff = hXGrad;

				if (zAxisIsHere) {
					wZAxis = DASH_SIZE + 1; // graduation size + 1 for axis line
					wZGrad = GraduationLengthApproximer.approximateGradLength(zInterval, fm, zIsInteger); // heuristic
					// wZName = fm.stringWidth (zAxisLabel);
					// hZName = userFontHeight; // font height
					wZTotalStuff = wZAxis + hInter + wZGrad;
				}

				int hDrawing = panelHeight - vInter - hCaption - vInter - hYName - 2 * vInter - hXAxis - vInter
						- hXGrad - hXName;
				int wDrawing = panelWidth - wYGrad - hInter - wYAxis - wZTotalStuff;

				// fc - 21.3.2008
				// ~ NOPE if (yRightLabelled) {wDrawing -= rightLabelSize;}

				int x0 = wYGrad + hInter + wYAxis;
				int y0 = vInter + hCaption + vInter + hYName + 2 * vInter + hDrawing;

				// 4. For each axis, resize interval, compute graduation step,
				// min grad, max grad

				// fc - 21.3.2008 - calculate the width of the largest right
				// label in uSpace

				// fc-30.11.2011 trying to restrict the label size if huge
				rightLabelSize = Math.min(rightLabelSize, (int) (wDrawing / 3d));

				double uRightLabelSize = 0;
				if (yRightLabelled) {
					int tmp_pw = yRightLabelled ? wDrawing - rightLabelSize : wDrawing;
					double tmp_uw = xInterval.getVariation();
					int gradSize = userFontHeight;
					int roundedRLSize = (int) Math.round(((double) rightLabelSize) / gradSize) * gradSize;
					uRightLabelSize = ((double) roundedRLSize) / tmp_pw * tmp_uw * 1.2;
				}

				xStep = Graduator.graduate(xInterval, xIsInteger, userFontHeight, wDrawing, true, xLabelledGraduation,
						xAnchors, yRightLabelled, uRightLabelSize);

				yStep = Graduator.graduate(yInterval, yIsInteger, userFontHeight, hDrawing, false, false, null,
						yRightLabelled, uRightLabelSize);

				if (zAxisIsHere) {
					zStep = Graduator.graduate(zInterval, zIsInteger, userFontHeight, hDrawing, false, false, null,
							yRightLabelled, uRightLabelSize);
				}

				// fc - 11.10.2004 - not enough space in y ->
				// "please enlarge panel"
				if (hDrawing <= 0 || yInterval.isNull()) {
					addEnlargeMessage(g);
					return;
				}

				// System.out.println ("DRCurves.update () sizes done");

				// _________________
				// _________________ P-space definition
				// _________________
				p0 = new Point(x0, y0);
				p1 = new Point(x0, y0 - hDrawing);
				p2 = new Point(x0 + wDrawing, y0 - hDrawing);
				p3 = new Point(x0 + wDrawing, y0);
				pw = wDrawing;
				ph = hDrawing;
				// ~ System.out.println
				// ("P-space: p0="+p0+" p1="+p1+" p2="+p2+" p3="+p3+" pw="+pw+" ph="+ph);

				// _________________
				// _________________ U-space definition
				// _________________
				u0 = new Vertex2d(xInterval.a, yInterval.a);
				u1 = new Vertex2d(xInterval.a, yInterval.b);
				u2 = new Vertex2d(xInterval.b, yInterval.b);
				u3 = new Vertex2d(xInterval.b, yInterval.a);
				uw = xInterval.getVariation();
				uh = yInterval.getVariation();
				// ~ System.out.println
				// ("U-space: u0="+u0+" u1="+u1+" u2="+u2+" u3="+u3+" uw="+uw+" uh="+uh);

				if (zAxisIsHere) {
				}

				// System.out.println ("DRCurves.update () drawer.draw ()...");

				/*
				 * fc - 10.10.2006 - [SPerret] trying to manage forced limits
				 * overflow clearly
				 */
				// Call draw of the drawer - fc - 1.10.2003
				//
				drawer.draw(g, new Rectangle.Double()); // null : rectangle -
														// unused ere
				/* [SPerret] the previous line was moved from below */

				// System.out.println
				// ("DRCurves.update () back from drawer.draw ()");

				// fc - 10.10.2006
				// fill WHITE rectangles arround the main drawing to
				// remove the parts of curves / bars... exceeding the
				// forced limits
				Color foreground = g2.getColor();
				g2.setColor(Color.WHITE);
				if (!forcedEdgesOverflow) { // nothing outside the main drawing
					Rectangle top = new Rectangle(0, 0, panelWidth, p1.y);
					Rectangle bottom = new Rectangle(0, p0.y + 1, panelWidth, panelHeight - p0.y);
					Rectangle left = new Rectangle(0, 0, p0.x, panelHeight);
					Rectangle right = new Rectangle(p2.x + 1, 0, panelWidth - p2.y, panelHeight);
					g2.fill(top);
					g2.fill(bottom);
					g2.fill(left);
					g2.fill(right);
				} else { // small parts of curves / histo are visible outside
							// the main drawing
					Rectangle top0 = new Rectangle(0, 0, panelWidth, p1.y - 3);
					Rectangle top1 = new Rectangle(0, p1.y - 1, panelWidth, 1);
					Rectangle bottom0 = new Rectangle(0, p0.y + 1, panelWidth, 2);
					Rectangle bottom1 = new Rectangle(0, p0.y + 5, panelWidth, panelHeight - p0.y - 2);
					Rectangle left0 = new Rectangle(0, 0, p0.x - 5, panelHeight);
					Rectangle left1 = new Rectangle(p0.x - 3, 0, 2, panelHeight);
					Rectangle right0 = new Rectangle(p2.x + 1, 0, 1, panelHeight);
					Rectangle right1 = new Rectangle(p2.x + 4, 0, panelWidth - p2.y - 2, panelHeight);
					g2.fill(top0);
					g2.fill(top1);
					g2.fill(bottom0);
					g2.fill(bottom1);
					g2.fill(left0);
					g2.fill(left1);
					g2.fill(right0);
					g2.fill(right1);
					g2.setColor(Color.BLACK);
					int x = 0;
					int y = userFontHeight + vInter / 2;
					g2.drawString(Translator.swap("DRCurves.forcedEdgesOverflow"), (float) x, (float) y);
				}
				g2.setColor(foreground);
				// fc - 10.10.2006

				g2.setFont(userFont);

				// System.out.println ("DRCurves.update () caption...");

				// Caption
				//
				captionBounds = new HashMap(); // KEEP this for mouse action on
												// captions

				if (getDataBlock().isCaptionRequired()) { // fc - 29.4.2003 -
															// caption becomes
															// optionnal

					int k9 = hCaption - userFontHeight;

					int x = 0;
					int y = userFontHeight + vInter / 2;

					for (Iterator ite = specialAndNormalExtractors.iterator(); ite.hasNext();) {
						DataFormat extr = (DataFormat) ite.next();

						// fc-8.11.2011 replaced '___' by a nicer colored icon
						ColoredIcon icon = new ColoredIcon(extr.getColor());
						int iconWidth = icon.getIconWidth();
						int iconHeight = icon.getIconHeight();

						// String c = "   ___ " + extr.getCaption ();
						String c = AmapTools.cutIfTooLong(extr.getCaption(), 50);

						int textWidth = fm.stringWidth(c);

						int captionWidth = iconWidth + textWidth;

						x = panelWidth - captionWidth - hInter;
						ImageObserver observer = null;
						g2.drawImage(icon.getImage(), x, y - iconHeight, observer);

						x = panelWidth - textWidth - hInter;
						g2.setColor(extr.getColor());
						g2.drawString(c, (float) x, (float) y);

						// fc-8.11.2011 replaced '___' by a nicer colored icon
						Rectangle r = new Rectangle(x, y - userFontHeight, captionWidth, userFontHeight); // fc
																											// -
																											// 12.5.2003
						k9 -= userFontHeight;
						captionBounds.put(r, extr); // caption for extr is in
													// rectangle r (used for
													// mouse selection)

						y += userFontHeight; // NEW
					}
				}

				g2.setColor(defaultColor); // defined in DataRenderer

				// System.out.println ("DRCurves.update () caption done");

				// yName
				//
				int x = Math.max(x0, wYName) - wYName;
				int y = vInter / 2 + hCaption + vInter + hYName;
				g2.drawString(yAxisLabel, (float) x, (float) y);

				// System.out.println ("DRCurves.update () graduators...");

				// yAxis & yGrad
				//
				Graduator.drawGrads(g2, p0, p1, yInterval, yStep, null, null, Graduator.VERTICAL, Graduator.LEFT,
						DASH_SIZE, panelHeight); // anchors
													// and
													// labels
													// are
													// null

				// xAxis
				if (xLabelledGraduation) {

					// Deal with potential "holes" in the graduation
					validateAnchors(xAnchors, xLabels);

					Graduator.drawGrads(g2, p0, p3, xInterval, xStep, xAnchors, xLabels, Graduator.HORIZONTAL,
							Graduator.RIGHT, DASH_SIZE, panelWidth); // anchors
																		// and
																		// labels
																		// are
																		// given

				} else {

					Graduator.drawGrads(g2, p0, p3, xInterval, xStep, null, null, Graduator.HORIZONTAL,
							Graduator.RIGHT, DASH_SIZE, panelWidth); // anchors
																		// and
																		// labels
																		// are
																		// null

				}

				// xName
				//
				x = x0 + wDrawing - wXName;
				y = y0 + hXAxis + vInter + hXGrad + hXName - userFontDescent;
				g2.drawString(xAxisLabel, (float) x, (float) y);

				/*
				 * if (zAxisIsHere) { // zAxis zAxis = new AxisZone ();
				 * zAxis.setBackground (Color.white); zAxis.setAxis
				 * (AxisZone.VERTICAL, hDrawing, zInterval, zStep, zTransform);
				 * zAxis.setGraduationAspect (AxisZone.RIGHT);
				 * zAxis.setGraduationSize (DASH_SIZE); zAxis.setCorner3
				 * (x0+xAxis.getWidth (), y0); zAxis.addMouseListener
				 * (DRCurves.this); // fc - 1.4.2003 - for click and ctrl-click
				 * detection add (zAxis);
				 * 
				 * // zGrad zGrad = new GraduationZone (wZGrad, hDrawing, 0, 8,
				 * 0, 8); zGrad.setBackground (Color.white); // works only if
				 * opaque is true zGrad.setUserFont (userFont); zGrad.setAxis
				 * (AxisZone.VERTICAL, hDrawing, zInterval, zStep, zTransform);
				 * zGrad.setGraduationAspect (AxisZone.LEFT); zGrad.setCorner3
				 * (zAxis.getX ()+zAxis.getWidth ()+hInter, y0); zGrad.setOpaque
				 * (false); zGrad.addMouseListener (DRCurves.this); // fc -
				 * 1.4.2003 - for click and ctrl-click detection add (zGrad);
				 * 
				 * // zName zName = new TextZone (wZName, hZName,
				 * TextZone.COLUMN); zName.setAlignment (TextZone.LEFT);
				 * zName.setBackground (Color.white); zName.setUserFont
				 * (userFont); zName.add (zAxisLabel); zName.setCorner3
				 * (Math.min (zAxis.getX (), getWidth ()-wZName),
				 * y0-zAxis.getHeight ()-2*vInter); zName.addMouseListener
				 * (DRCurves.this); // fc - 1.4.2003 - for click and ctrl-click
				 * detection add (zName); }
				 */

				// Call draw of the drawer - fc - 1.10.2003
				//
				// fc - 10.10.2006 - moved upper: drawer.draw (g, new
				// Rectangle.Double ()); // null
				// :
				// rectangle - unused ere

				// fc-27.3.2012 add a warning message if the configuration
				// should be checked
				if (displayCheckConfigurationMessage)
					contentPanel.addWarningMessage(g);

				// System.out.println
				// ("DRCurves.update () end-of-update, normal");

			} catch (Exception e) {// fc-20.1.2014
				Log.println(Log.ERROR, "PanelDataRenderer.update ()", "Error in update (), wrote in Log and passed", e);
			}

		}

		// Prints a given message in the graphics ("see configuration")
		//
		public void addMessage(Graphics g, String message) { // fc-30.10.2014
			Graphics2D g2 = (Graphics2D) g;
			Font f = g2.getFont();
			FontMetrics fm = g2.getFontMetrics(f);
			int fontAscent = fm.getAscent();
			int fontDescent = fm.getDescent();
			int fontHeight = fontAscent + fontDescent;
			g2.drawString(message, 0f, (float) fontHeight);

		}

		// Prints a warning message in the graphics ("see configuration")
		//
		public void addWarningMessage(Graphics g) {
			Graphics2D g2 = (Graphics2D) g;
			Font f = g2.getFont();
			FontMetrics fm = g2.getFontMetrics(f);
			int fontAscent = fm.getAscent();
			int fontDescent = fm.getDescent();
			int fontHeight = fontAscent + fontDescent;
			g2.drawString(Translator.swap("Shared.seeConfiguration"), 0f, (float) fontHeight);

		}

		// Prints a message in the graphics ("enlarge the panel")
		//
		private void addEnlargeMessage(Graphics g) {
			Graphics2D g2 = (Graphics2D) g;
			Font f = g2.getFont();
			FontMetrics fm = g2.getFontMetrics(f);
			int fontAscent = fm.getAscent();
			int fontDescent = fm.getDescent();
			int fontHeight = fontAscent + fontDescent;
			g2.drawString(Translator.swap("Shared.enlargePanel"), 0f, (float) fontHeight);

		}

		// There may be "holes" in the anchors, ex: 2, 4, 6, 12, 14 (interval =
		// 2).
		// This method should turn this in 2, 4, 6, 8, 10, 12, 14.
		// white labels are inserted along with new anchors.
		// Correction of bug (7.8.2003 - pv)
		//
		private void validateAnchors(List<Integer> anchors, List<String> labels) {
			if (anchors.size() < 3) {
				return;
			}

			// Calculate interval between 2 grads
			int interval = Integer.MAX_VALUE;
			int a1 = 0;
			int a2 = 0;
			for (int i = 1; i < anchors.size(); i++) { // we jump element 0
				a1 = ((Integer) anchors.get(i - 1)).intValue();
				a2 = ((Integer) anchors.get(i)).intValue();
				interval = Math.min(interval, a2 - a1);
			}

			// Rebuild anchors/labels without holes

			List<Integer> clonedAnchors = new ArrayList<Integer>(anchors);
			List<String> clonedLabels = new ArrayList<String>(labels);
			anchors.clear();
			labels.clear();
			Iterator<String> labs = clonedLabels.iterator();
			for (int i = 1; i < clonedAnchors.size(); i++) { // we jump element
																// 0
				a1 = ((Integer) clonedAnchors.get(i - 1)).intValue();
				anchors.add(new Integer(a1));
				labels.add(labs.next());
				a2 = ((Integer) clonedAnchors.get(i)).intValue();

				// add anchors/white labels between a1 and a2 ?
				for (int k = a1 + interval; k < a2; k += interval) {
					anchors.add(new Integer(k));
					labels.add("");
				}
			}
			// last element
			anchors.add(new Integer(a2));
			labels.add(labs.next());
			// ~ System.out.println
			// ("after operation, anchors="+anchors+" labels="+labels);

		}

	} // end-of-ContentPanel

} // end-of-DRCurves

