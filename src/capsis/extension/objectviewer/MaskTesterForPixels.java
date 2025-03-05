/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.extension.objectviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Ellipse2D;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;

import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JScrollPane;
import javax.swing.JTextField;

import jeeb.lib.util.Alert;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColoredButton;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.SpatializedList;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;
import capsis.commongui.util.Tools;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.defaulttype.plotofpixels.Parcel;
import capsis.defaulttype.plotofpixels.Pixel;
import capsis.defaulttype.plotofpixels.PixelMap;
import capsis.defaulttype.plotofpixels.PixelRoundMask;
import capsis.defaulttype.plotofpixels.PlotOfPixels;
import capsis.extension.AbstractObjectViewer;
import capsis.kernel.GScene;
import capsis.util.Drawer;
import capsis.util.Panel2D;

/**
 * A viewer for testing the Capsis pixel based masks, to get trees or pixels
 * around a point / pixel / tree.
 *
 * @author F. de Coligny - November 2017
 */
public class MaskTesterForPixels extends AbstractObjectViewer implements Drawer, ActionListener, Disposable {

	static {
		Translator.addBundle("capsis.extension.objectviewer.MaskTesterForPixels");
	}
	
	// nb-07.08.2018
	//public static final String NAME = Translator.swap("MaskTesterForPixels");
	//public static final String DESCRIPTION = Translator.swap("MaskTesterForPixels.description");
	//static public final String AUTHOR = "F. de Coligny";
	//static public final String VERSION = "1.0";

	public static final int X_MARGIN_IN_PIXELS = 10;
	public static final int Y_MARGIN_IN_PIXELS = 10;

	public static final Color SELECTION_CIRCLE_COLOR = Color.BLACK;
	public static final Color CANDIDATE_PIXEL_COLOR = new Color(153, 255, 153); // ligh
																				// green
	public static final Color SELECTED_PIXEL_COLOR = Color.ORANGE;
	public static final Color SELECTED_ELEMENT_COLOR = Color.BLUE;
	public static final Color NEUTRAL_COLOR = Color.GRAY;

	private JButton help;

	private GScene scene;
	private Collection pixels;

	private Pixel selectedPixel; // the pixel the user clicked on
	private Collection inMask; // pixels or trees
	private List<Pixel> candidatePixels; // pixels matching all the mask
											// sites

	private Panel2D panel2D;
	private JScrollPane scroll;

	private Container normalPanel;
	private Container securityPanel;

	private Vertex2d selectedVertex;
	private JTextField maskRadius; // m
	// private JCheckBox torusEnabled;
	private JRadioButton pixelMask;
	private JRadioButton partilallyIncluded;
	private JRadioButton centerIncluded;
	private JRadioButton completelyIncluded;
	private JRadioButton treeMask;
	private ButtonGroup group;
	private ButtonGroup group2;
	private JTextField numberInMask;

	/**
	 * Constructor.
	 */
	public MaskTesterForPixels() {
	}

	@Override
	public void init(Collection s) throws Exception {

		try {
			selectedPixel = null;
			inMask = new HashSet();
			candidatePixels = new ArrayList<Pixel>();

			// System.out.println("MaskTesterForPixels init () given collection:
			// " + AmapTools.toString(s));

			extractPixels(s);

			createUI();

			calculatePanel2D();

		} catch (Exception e) {
			Log.println(Log.ERROR, "MaskTesterForPixels.c ()", e.toString(), e);
			throw e; // object viewers may throw exception
		}
	}

	/**
	 * Extension dynamic compatibility mechanism. This method checks if the
	 * extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {

			// System.out.println("MaskTesterForPixels matchWith (), referent: "
			// + AmapTools.toString(((Collection) referent)));

			// Referent is a Collection
			Collection c = (Collection) referent;
			if (c.isEmpty())
				return false;

			// Find representative objects (ie with different classes)
			Collection reps = Tools.getRepresentatives(c);
			for (Iterator i = reps.iterator(); i.hasNext();) {
				Object candidate = i.next();
				if (candidate instanceof Pixel)
					// if at least one pixel, ok
					return true;

			}
			return false;
		} catch (Exception e) {
			Log.println(Log.ERROR, "MaskTesterForPixels.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
	}

	@Override
	public String getName() {
		return Translator.swap("MaskTesterForPixels.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MaskTesterForPixels.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	// call extractPixels before this method
	private void calculatePanel2D() {
		Vertex3d origin = scene.getOrigin();
		double userWidth = scene.getXSize();
		double userHeight = scene.getYSize();

		Rectangle.Double r2 = new Rectangle.Double(origin.x, origin.y, userWidth, userHeight);

		// When panel2D repaint is needed, panel2D will call
		// this.draw ()
		panel2D = new Panel2D(this, r2, X_MARGIN_IN_PIXELS, Y_MARGIN_IN_PIXELS, false);

		scroll.getViewport().setView(panel2D);
	}

	// ObjectViewers manage a Collection
	private Collection extractPixels(Collection subjects) throws Exception {
		pixels = new ArrayList();
		if (subjects == null || subjects.isEmpty()) {

			// System.out.println(("MaskTesterForPixels extractPixels () empty
			// or null subject, found #pixels: " + pixels
			// .size()));

			return pixels;
		}

		//
		// System.out.println("MaskTesterForPixels extractPixels ()...");
		for (Object o : subjects) {
			// System.out.println("MaskTesterForPixels extractPixels () subject:
			// " + o);
			if (o instanceof Pixel) {
				Pixel pix = (Pixel) o;

				Collection pixelMaps = null;

				// fc+ed-14.3.2018 use pixelMap and pixels without PlotOfPixels
				// nor Parcel
				PlotOfPixels plot = pix.getPixelMap().getPlot();
				if (plot != null) {
					pixelMaps = plot.getPixelMaps();
					scene = plot.getScene();
				} else {
					pixelMaps = new ArrayList();
					pixelMaps.add(pix.getPixelMap());
					
//					System.out.println("MaskTesterForPixels extractPixels pixelMap: "+pix.getPixelMap());
//					System.out.println("MaskTesterForPixels extractPixels pixels: "+pix.getPixelMap().getPixels());
					
					scene = pix.getPixelMap().getScene();
				}

				for (Object o2 : pixelMaps) {
					PixelMap pm = (PixelMap) o2;
					pixels.addAll(pm.getPixels());
				}
				break;
			}
		}

//		 System.out.println(("MaskTesterForPixels extractPixels () found #pixels: " + pixels.size()));

		return pixels;
	}

	private void update() {

		if (panel2D != null) {

			updateMask(); // fc-13.11.2017

			panel2D.reset();
			panel2D.repaint();
		}
	}

	/**
	 * Disposable
	 */
	public void dispose() {
	}

	/**
	 * In case of trouble, show a security panel
	 */
	public void security() {
		securityPanel = new JPanel();
		securityPanel.add(new JLabel(Translator.swap("MaskTesterForPixels.canNotUpdate")));

		removeAll();
		add(securityPanel);

	}

	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed(ActionEvent evt) {
		synchro();

		update();

	}

	/**
	 * User selected a new location or changed configuration -> the mask needs
	 * to be reset.
	 */
	private void updateMask() {

		Vertex2d v2 = selectedVertex;

		if (v2 == null)
			return; // fc+ed-14.3.2018 if no selection yet
		
		selectedPixel = null;
		inMask.clear();

		// Search the pixel covering the location selected by user
		for (Object o : pixels) {
			Pixel pix = (Pixel) o;

			if (pix.getShape().contains(v2.x, v2.y)) {
				selectedPixel = pix; // pixel covering selectedVertex
				break;
			}
		}

		if (selectedPixel == null)
			return; // Selection outside a pixel -> no mask possible

		// Create a neighbourhood mask on the selected pixel's pixelMap
		double maskRadius = Check.doubleValue(this.maskRadius.getText().trim());
		PixelRoundMask mask = new PixelRoundMask(selectedPixel.getPixelMap(), maskRadius);
		
		try {
			
			Settings.setProperty("MaskTesterForPixels.maskRadius", Check.doubleValue(this.maskRadius.getText().trim())); // fc-19.3.2018
			Settings.setProperty("MaskTesterForPixels.treeMask", treeMask.isSelected ()); // fc-19.3.2018
			
			// No restriction to pixels containing trees
			boolean onlyPixelsWithElementsInside = false;

			// 1. pixels matching the mask sites (green)
			candidatePixels = mask.getNeighbours(selectedPixel, onlyPixelsWithElementsInside);

			if (pixelMask.isSelected()) {

				// 2. pixels strictly selected by the mask (orange)
				byte criterion = PixelRoundMask.PARTIALLY_INCLUDED; // default
				if (centerIncluded.isSelected()) {
					criterion = PixelRoundMask.CENTER_INCLUDED;
				} else if (completelyIncluded.isSelected()) {
					criterion = PixelRoundMask.COMPLETELY_INCLUDED;
				}

				inMask = mask.getPixelsNear(selectedPixel, criterion);
				
			} else { // treeMask

				// 3. trees in the mask (blue)
				boolean includingXYElement = true;
				inMask = mask.getElementsNear(v2.x, v2.y, includingXYElement);

			}

		} catch (Exception e) {
			Log.println(Log.WARNING, "MaskTesterForFpixels.updateMask ()", "Can not update", e);
			Alert.print(Translator.swap("MaskTesterForPixels.canNotUpdate"), e);
		}

	}

	/**
	 * From Drawer interface. This method draws in the Panel2D each time this
	 * one must be repainted. The given Rectangle is the sub-part of the object
	 * to draw (zoom) in user coordinates (i.e. meters...). It can be used in
	 * preprocesses to avoid drawing invisible parts.
	 */
	public void draw(Graphics g, Rectangle.Double r) {

		Graphics2D g2 = (Graphics2D) g;
		
		// Draw the pixels
		for (Iterator i = pixels.iterator(); i.hasNext();) {
			Pixel pixel = (Pixel) i.next();

			Shape sh = pixel.getShape();
			Rectangle2D bBox = sh.getBounds2D();

			if (r.intersects(bBox)) {
				

				if (inMask.contains(pixel)) {
					g2.setColor(SELECTED_PIXEL_COLOR);
					g2.fill(sh);

				} else if (candidatePixels.contains(pixel)) {
					g2.setColor(CANDIDATE_PIXEL_COLOR); // light green
					g2.fill(pixel.getShape());
				}

				g2.setColor(NEUTRAL_COLOR);
				g2.draw(sh);

			}

		}

		// If trees, draw them
		List<Spatialized> trees = new ArrayList();

		if (scene instanceof TreeList) {
			for (Object o2 : ((TreeList) scene).getTrees()) {
				if (!(o2 instanceof Spatialized))
					continue;

				trees.add((Spatialized) o2);
			}
		}

		if (scene instanceof SpatializedList) {
			for (Spatialized o3 : ((SpatializedList) scene).getSpatializeds()) {
				trees.add(o3);
			}
		}

		for (Spatialized t : trees) {			
			drawTree(g2, t, r);
		}

		if (scene.getPlot() instanceof PlotOfPixels) {
			PlotOfPixels plotp = scene.getPlot();
			for (Object o : plotp.getParcels()) {
				Parcel p = (Parcel) o;
				Shape sh = p.getShape();

				g2.setColor(Color.BLACK);
				g2.draw(sh);
			}
		}

		// More drawing
		if (selectedVertex != null) {

			// Draw the selection circle

			double x = selectedVertex.x;
			double y = selectedVertex.y;

			if (pixelMask.isSelected() && selectedPixel != null) {
				x = selectedPixel.getCenter().x;
				y = selectedPixel.getCenter().y;
			}

			double maskRadius = Check.doubleValue(this.maskRadius.getText().trim());
			Ellipse2D.Double sh = new Ellipse2D.Double(x - maskRadius, y - maskRadius, 2 * maskRadius, 2 * maskRadius);
			Rectangle2D bBox = sh.getBounds2D();
			if (r.intersects(bBox)) {
				g2.setColor(SELECTION_CIRCLE_COLOR);
				g2.draw(sh);
			}

			// Draw the center of circle
			Vertex2d v2 = selectedVertex;
			double t = 1; // m.
			Line2D.Double sh3 = new Line2D.Double(v2.x - t, v2.y - t, v2.x + t, v2.y + t);
			Line2D.Double sh4 = new Line2D.Double(v2.x - t, v2.y + t, v2.x + t, v2.y - t);
			g2.setColor(SELECTION_CIRCLE_COLOR);
			g2.draw(sh3);
			g2.draw(sh4);
		}

		numberInMask.setText("" + inMask.size());

		
		
	}

	/**
	 * Method to draw a Spatialized Tree within this viewer.
	 */
	public void drawTree(Graphics2D g2, Spatialized spa, Rectangle.Double r) {

		double dbh = 20; // cm
		if (spa instanceof Tree) {
			Tree t = (Tree) spa;
			dbh = t.getDbh();
			// at least 20 cm for drawing
			if (dbh < 20)
				dbh = 20;

		}

		double radius = dbh / 100 / 2;
		Ellipse2D.Double sh = new Ellipse2D.Double(spa.getX() - radius, spa.getY() - radius, 2 * radius, 2 * radius);
		Rectangle2D bBox = sh.getBounds2D();

		if (r.intersects(bBox)) {
			if (inMask.contains(spa)) {
				g2.setColor(SELECTED_ELEMENT_COLOR);
				g2.fill(sh);

				double r2 = radius * 2;
				Ellipse2D.Double sh2 = new Ellipse2D.Double(spa.getX() - r2, spa.getY() - r2, 2 * r2, 2 * r2);
				g2.setColor(SELECTED_ELEMENT_COLOR);
				g2.draw(sh2);

			} else {
				g2.setColor(NEUTRAL_COLOR);
				g2.draw(sh);
			}
		}

	}

	private boolean check() {
		if (!Check.isDouble(maskRadius.getText().trim())) {
			MessageDialog.print(this, Translator.swap("MaskTesterForPixels.maskRadiusMustBeADouble"));
			return false;
		}

		return true;
	}

	/**
	 * From Drawer interface. We may receive (from Panel2D) a selection
	 * rectangle (in user space i.e. meters) and return a JPanel containing
	 * information about the objects (trees) inside the rectangle. If no objects
	 * are found in the rectangle, return null.
	 */
	public JPanel select(Rectangle.Double r, boolean ctrlIsDown) {
		if (!check()) {
			return null;
		}

		selectedVertex = new Vertex2d(r.getCenterX(), r.getCenterY());

		updateMask(); // fc-13.11.2017

		panel2D.reset();
		panel2D.repaint();

		return null;
	}

	@Override
	public Collection show(Collection candidateSelection, boolean thisIsAReselection) {
		try {
			realSelection = extractPixels(candidateSelection);

			calculatePanel2D();
			update();
			return realSelection;
		} catch (Exception e) {
			Log.println(Log.ERROR, "MaskTesterForPixels.show ()", "Could not show candidate selection", e);
			return Collections.EMPTY_LIST;
		}
	}

	/**
	 * Synchronize gui components according to radio buttons selections.
	 */
	private void synchro() {
		partilallyIncluded.setEnabled(pixelMask.isSelected());
		centerIncluded.setEnabled(pixelMask.isSelected());
		completelyIncluded.setEnabled(pixelMask.isSelected());
	}

	/**
	 * User interface definition.
	 */
	private void createUI() {
		this.setLayout(new BorderLayout());

		JPanel part1 = new JPanel(new BorderLayout());
		scroll = new JScrollPane();

		// Do not set sizes explicitly inside object viewers
		// ~ scroll.setPreferredSize (new Dimension (500, 400));
		scroll.setMinimumSize(new Dimension(100, 100));

		part1.add(scroll, BorderLayout.CENTER);

		ColumnPanel lateral = new ColumnPanel(0, 0);

		LinePanel l5 = new LinePanel();
		l5.add(new JLabel(Translator.swap("MaskTesterForPixels.selectInTheSceneWithTheMouse")));
		l5.addGlue();
		lateral.add(l5);

		LinePanel l1 = new LinePanel();
		l1.add(new JWidthLabel(Translator.swap("MaskTesterForPixels.maskRadius") + " : ", 80));
		maskRadius = new JTextField();
		maskRadius.setText(""+Settings.getProperty("MaskTesterForPixels.maskRadius", 3d)); // fc-19.3.2018
		maskRadius.addActionListener(this);
		l1.add(maskRadius);
		l1.addStrut0();
		lateral.add(l1);

		// LinePanel l2 = new LinePanel();
		// torusEnabled = new
		// JCheckBox(Translator.swap("MaskTesterForPixels.torusEnabled"), true);
		// torusEnabled.addActionListener(this);
		// l2.add(torusEnabled);
		// l2.addGlue();
		// lateral.add(l2);

		LinePanel l3 = new LinePanel();
		l3.add(new JWidthLabel(Translator.swap("MaskTesterForPixels.select") + " ", 110));
		treeMask = new JRadioButton(Translator.swap("MaskTesterForPixels.treeMask"));
		treeMask.addActionListener(this);
		l3.add(treeMask);
		l3.addGlue();
		lateral.add(l3);

		LinePanel l4 = new LinePanel();
		l4.add(new JWidthLabel("", 110));
		pixelMask = new JRadioButton(Translator.swap("MaskTesterForPixels.pixelMask"));
		pixelMask.addActionListener(this);
		l4.add(pixelMask);
		l4.addGlue();
		lateral.add(l4);

		group = new ButtonGroup();
		group.add(pixelMask);
		group.add(treeMask);
//		pixelMask.setSelected(true); // fc-19.3.2018

		treeMask.setSelected(Settings.getProperty("MaskTesterForPixels.treeMask", true)); // fc-19.3.2018
		pixelMask.setSelected(!treeMask.isSelected ()); // fc-19.3.2018
	
		LinePanel l7 = new LinePanel();
		l7.add(new JWidthLabel("", 50));
		partilallyIncluded = new JRadioButton(Translator.swap("MaskTesterForPixels.partilallyIncluded"));
		partilallyIncluded.addActionListener(this);
		l7.add(partilallyIncluded);
		l7.addGlue();
		lateral.add(l7);

		LinePanel l8 = new LinePanel();
		l8.add(new JWidthLabel("", 50));
		centerIncluded = new JRadioButton(Translator.swap("MaskTesterForPixels.centerIncluded"));
		centerIncluded.addActionListener(this);
		l8.add(centerIncluded);
		l8.addGlue();
		lateral.add(l8);

		LinePanel l9 = new LinePanel();
		l9.add(new JWidthLabel("", 50));
		completelyIncluded = new JRadioButton(Translator.swap("MaskTesterForPixels.completelyIncluded"));
		completelyIncluded.addActionListener(this);
		l9.add(completelyIncluded);
		l9.addGlue();
		lateral.add(l9);

		group2 = new ButtonGroup();
		group2.add(partilallyIncluded);
		group2.add(centerIncluded);
		group2.add(completelyIncluded);
		partilallyIncluded.setSelected(true);

		LinePanel l10 = new LinePanel();
		JButton selectionCircle = new ColoredButton(SELECTION_CIRCLE_COLOR);
		l10.add(selectionCircle);
		l10.add(new JLabel(Translator.swap("MaskTesterForPixels.selectionCircleColor")));
		l10.addGlue();
		lateral.add(l10);

		LinePanel l11 = new LinePanel();
		JButton cellButton1 = new ColoredButton(CANDIDATE_PIXEL_COLOR);
		l11.add(cellButton1);
		l11.add(new JLabel(Translator.swap("MaskTesterForPixels.candidateCellColor")));
		l11.addGlue();
		lateral.add(l11);

		LinePanel l12 = new LinePanel();
		JButton cellButton2 = new ColoredButton(SELECTED_PIXEL_COLOR);
		l12.add(cellButton2);
		l12.add(new JLabel(Translator.swap("MaskTesterForPixels.selectedCellColor")));
		l12.addGlue();
		lateral.add(l12);

		LinePanel l13 = new LinePanel();
		JButton treeButton = new ColoredButton(SELECTED_ELEMENT_COLOR);
		l13.add(treeButton);
		l13.add(new JLabel(Translator.swap("MaskTesterForPixels.selectedTreeColor")));
		l13.addGlue();
		lateral.add(l13);

		LinePanel l14 = new LinePanel();
		l14.add(new JLabel(Translator.swap("MaskTesterForPixels.numberInMask") + " : "));
		numberInMask = new JTextField(5);
		numberInMask.setEditable(false); // fc-19.3.2018
		l14.add(numberInMask);
		l14.addStrut0();
		lateral.add(l14);

		JPanel aux = new JPanel(new BorderLayout());
		aux.add(lateral, BorderLayout.NORTH);

		// 2. Control panel
		JPanel pControl = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		help = new JButton(Translator.swap("MaskTesterForPixels.help"));
		help.addActionListener(this);
		pControl.add(help);

		// Layout parts
		normalPanel = new JPanel(new BorderLayout());
		normalPanel.add(part1, BorderLayout.CENTER);
		normalPanel.add(aux, BorderLayout.EAST);
		normalPanel.add(pControl, BorderLayout.SOUTH);

		setLayout(new GridLayout(1, 1));
		add(normalPanel); // fc - 11.2.2008

		synchro();

	}

}
