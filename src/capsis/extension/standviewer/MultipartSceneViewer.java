/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2001-2018 Francois de Coligny
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
package capsis.extension.standviewer;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.swing.JCheckBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import capsis.app.CapsisExtensionManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.defaulttype.MultipartScene;
import capsis.defaulttype.ScenePart;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import jeeb.lib.util.geometry.Geometry;
import capsis.util.Drawer;
import capsis.util.Panel2D;
import jeeb.lib.maps.geom.Polygon2;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.OVDialog;
import jeeb.lib.util.OVSelector;
import jeeb.lib.util.OVSelectorSource;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * MultipartSceneViewer is a cartography viewer MultipartScenes.
 * 
 * @author F. de Coligny - January 2011
 */
public class MultipartSceneViewer extends AbstractStandViewer implements ActionListener, OVSelectorSource {

	// fc-4.7.2018 completely reviewed for better selection handling

	static {
		Translator.addBundle("capsis.extension.standviewer.MultipartSceneViewer");
	}

	// nb-07.08.2018
	// public static final String NAME = "MultipartSceneViewer";
	// public static final String VERSION = "2.0";
	// public static final String AUTHOR = "F. de Coligny";
	// public static final String DESCRIPTION =
	// "MultipartSceneViewer.description";

	private MultipartScene scene;

	// We will manage OVChooser reselection on the visible InnerPanel (they
	// are in tabs)
	private Map<String, InnerPanel> panelMap; // key is panel name

	private JTabbedPane tabs; // contains the inner panel(s)

	// If checked, the parts will be splited in different innerPanels else the
	// whole scene will be in the same innerPanel, acts on separateViews
	private JCheckBox separate;
	// Linked to separate
	protected boolean separateViews;

	// An ObjectViewer selector: user chooses an Object viewer and all user
	// selections are sent to this ov
	protected OVSelector ovSelector;

	// The tab an objectViewer is opened on, set at selection time
	private int objectViewerTab = -1;

	// Optional legend
	private JScrollPane legendScrollPane;

	private JLabel statusBar;

	/**
	 * Init method. Initialises the extension on the given model and step.
	 */
	@Override
	public void init(GModel model, Step stp, StepButton but) throws Exception {
		super.init(model, stp, but);

		separateViews = Settings.getProperty("MultipartSceneViewer.separateViews", false);

		createOVSelector();

		createUI();

		createPanelMap();

		update(stp);
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchWith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();

			if (!(s instanceof MultipartScene)) {
				return false;
			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "MultipartSceneViewer.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("MultipartSceneViewer.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("MultipartSceneViewer.description");
	}

	@Override
	public String getVersion() {
		return "2.0";
	}

	/**
	 * Defines what objects are candidate for selection by OVSelector
	 */
	protected void createOVSelector() {
		try {

			scene = (MultipartScene) step.getScene();
			List<ScenePart> parts = scene.getParts();

			// See which objects will be sent to the selected objectViewer

			// Default: the sceneParts
			Collection candidateObjects = new ArrayList(parts);

			// fc-25.6.2015 Added the trees
			for (ScenePart part : parts) {
				candidateObjects.addAll(part.getObjectsInPart());
			}

			JScrollPane targetScrollPane = null; // not used
			GModel modelForVetoes = null; // not used

			// Look for objectViewers compatible with the candidateObjects,
			// build an objectViewerSelector
			ExtensionManager extMan = CapsisExtensionManager.getInstance();

			OVSelectorSource ovSelectorSource = this;

			ovSelector = new OVSelector(extMan, ovSelectorSource, candidateObjects, targetScrollPane, false, false,
					modelForVetoes);

		} catch (Exception e) {
			Log.println(Log.ERROR, "MultipartSceneViewer.InnerPanel.createOVSelector ()",
					"Exception during OVSelector construction, wrote this error and passed", e);
		}
	}

	/**
	 * Create the panel map, at first time, then when user clicks on separate
	 * (one single view / all parts in separate panels)
	 */
	private void createPanelMap() {

		scene = (MultipartScene) step.getScene();
		MultipartScene mps = (MultipartScene) step.getScene();

		// Put the parts in a list map: all in one single entry or all in its
		// own entry depending on separate
		ListMap<String, String> partMap = new ListMap<>();

		for (ScenePart part : mps.getParts()) {
			if (!separateViews)
				// all parts in same list (same key)
				partMap.addObject(Translator.swap("MultipartSceneViewer.wholeScene"), part.getName());
			else
				// all parts separated
				partMap.addObject(part.getName(), part.getName()); // split
																	// parts
		}

		// Create the panels, store them in the panelMap
		panelMap = new HashMap<>();

		// and add them in the tabbedPane
		tabs.removeAll();

		for (String name : partMap.getKeys()) {
			List<String> partNames = partMap.getObjects(name);

			// Give each inner panel the name(s) of the part(s) it will have to
			// draw
			InnerPanel panel = new InnerPanel(this, name,
					partNames, /* , mps, r, */ ovSelector);

			panelMap.put(panel.getName(), panel);

			tabs.addTab(panel.getName(), panel);
		}

		// panelMap is ready, needs to be redone if user clicks on separate

	}

	/**
	 * A stand viewer may be updated to synchronize itself with a given step
	 * button. In subclasses, redefine this method (beginning by super.update
	 * (sb);) to update your representation of the step.
	 */
	public void update(StepButton sb) {

		try {
			super.update(sb);

			updateViewer();

			// Send a reselection signal to the current innerPanel in case user
			// selected something, it can be updated on current step if step
			// changed, or shown differently if a configuration changed
			reselect();

		} catch (Exception e) {
			Log.println(Log.ERROR, "MultiPartSceneViewer.update (StepButton)", "Could not update on sb: " + sb, e);
			MessageDialog.print(this, Translator.swap("MultipartSceneViewer.errorWhileUpdatingMultipartSceneViewer"),
					e);
		}

	}

	/**
	 * Updates the whole viewer on the current Step / StepButton
	 */
	private void updateViewer() throws Exception {

		scene = (MultipartScene) step.getScene();

		// Update all panels
		for (InnerPanel panel : panelMap.values()) {
			panel.udpatePanel(step);
		}

		// Optional legend
		JPanel legend = scene.getLegend();
		if (legend != null)
			legendScrollPane.setViewportView(legend);

		this.revalidate();
		this.repaint();

	}

	@Override
	public void actionPerformed(ActionEvent e) {

		if (e.getSource().equals(separate)) {

			separateViews = !separateViews;
			Settings.setProperty("MultipartSceneViewer.separateViews", separateViews);

			try {
				createPanelMap();

				updateViewer();

				// fc-6.7.2018 Close any opened object viewer
				// cancelSelection();
				objectViewerTab = -1;
				ovSelector.dispose();

			} catch (Exception ex) {
				Log.println(Log.ERROR, "MultipartSceneViewer.actionPerformed ()", "Could not update viewer", ex);
				MessageDialog.print(this, Translator.swap("MultipartSceneViewer.couldNotUpdateTheViewerSeeLog"), ex);
			}
		}

	}

	/**
	 * Called when there is a user selection in an InnerPanel, sends the section
	 * to the shared ovSelector
	 */
	public Collection select(Collection candidateSelection, boolean thisIsAReselection) {

		// Keep this to manage objectViewer showing / hiding on tabs change
		objectViewerTab = tabs.getSelectedIndex();

		// ovSelector.select() will pass the selection to the selected
		// objectViewer, which will choose among the candidate selection which
		// objects it will indeed show (e.g. Inspector will show all,
		// TreeInspector would only show trees) and it will return the list of
		// what it effectively selected, we return this list to the caller (an
		// InnerPanel), it could decide to show selection marks around the
		// effective selection objects
		Collection effectiveSelection = ovSelector.select(candidateSelection, thisIsAReselection);

		return effectiveSelection;

	}

	/**
	 * Create the user interface.
	 */
	private void createUI() {

		// Create the user interface, the tabbed pane is created but empty at
		// this time

		// The ObjectViewer selector and the separate checkbox
		LinePanel l1 = new LinePanel();

		l1.add(ovSelector);

		separate = new JCheckBox(Translator.swap("MultipartSceneViewer.separateViews"), separateViews);
		separate.addActionListener(this);
		l1.add(separate);
		l1.addGlue();

		// The main JTabbedPane, empty at this time
		tabs = new JTabbedPane();
		tabs.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

		// ovSelector works on current tab. If tabs changed, cancel selection
		// (close objectViewer if any)
		tabs.addChangeListener(new ChangeListener() {
			public void stateChanged(ChangeEvent e) {

				try {
					// If user changed tabs and there was an OVDialog opened,
					// hide it, if user comes back, show it again
					boolean thatsTheGoodTab = tabs.getSelectedIndex() == objectViewerTab;

					ovSelector.getOvDialog().setVisible(thatsTheGoodTab);

					if (thatsTheGoodTab)
						// Send a reselection signal to the current innerPanel
						// in case user changed steps or shown differently if a
						// configuration changed
						reselect();

				} catch (Exception ex) {
					// ignore trouble here
				}
			}
		});

		// If a legend is found, it will go there
		legendScrollPane = new JScrollPane();

		// A status bar at the bottom
		statusBar = new JLabel();
		statusBar.setText(Translator.swap("Shared.ready"));

		getContentPane().setLayout(new BorderLayout());

		getContentPane().add(l1, BorderLayout.NORTH);
		getContentPane().add(tabs, BorderLayout.CENTER);
		getContentPane().add(legendScrollPane, BorderLayout.EAST);
		getContentPane().add(statusBar, BorderLayout.SOUTH);

	}

	@Override
	public void reselect() {
		System.out.println("MultipartSceneViewer.reselect ()... tab: " + objectViewerTab);

		// Viewer construction is not yet completed
		if (tabs == null || tabs.getTabCount() == 0)
			return;

		// No effect...
//		// fc-12.4.2019 bug: reselect () was reopening a closes OVDialog
//		OVDialog dlg = ovSelector.getOvDialog();
//		if (dlg != null && !dlg.isVisible())
//			return;

		// Get the inner panel on the selected tab
		int index = tabs.getSelectedIndex();

		// Is there an object viewer opened on this tab ?
		if (index != objectViewerTab)
			return;

		String tabName = tabs.getTitleAt(index);

		InnerPanel panel = panelMap.get(tabName);

		// We need to paint immediately to update the innerPanel on its new Step
		// (in case step changed, which is a common reason for reselection) and
		// update its drawnMap which will be used during effective reselection
		// just below
		panel.paintImmediately(panel.getBounds());

		// To trigger reselection in the innerPanel, we pass a null selection
		// rectangle
		boolean more = false; // this is not a selection extending
		panel.select(null, more);

	}

	@Override
	public void cancelSelection() {
		System.out.println("MultipartSceneViewer.cancelSelection ()... tab: " + objectViewerTab);

		ovSelector.select(null, false);

	}

	// ////////////////////////////////////////////////////////

	/**
	 * A panel to draw a list of scene parts (one or several).
	 */
	private static class InnerPanel extends JPanel
			implements Drawer, /* OVSelectorSource, */ Comparable {

		private MultipartSceneViewer caller;

		// Name of the panel
		private String name;

		// The panel draws the parts with these names
		private List<String> partNames;

		// fc-12.4.2019 a closed ov reopens on step button changing
		private OVSelector ovSelector;
		
		// The panel draws parts of the scene under this step
		private Step step;

		// The panel draws parts of this scene
		private MultipartScene scene;

		// The parts of the scene under step, with the names in partNames (maybe
		// one single or several)
		private List<ScenePart> parts;

		// Contains the main drawing
		private Panel2D panel2D;
		private Rectangle.Double userBounds; // bounds drawn in the panel2D

		// Contains the panel2D with the main drawing
		private JScrollPane scrollPane;

		protected boolean thisIsAReselection;
		protected Rectangle.Double memoSelectionRectangle;
		// protected Collection<Object> memoSelection;
		// protected Collection<Object> effectiveSelection;

		// Memo all objects drawn with their shape as a key to help handle
		// selection
		private Map<Shape, Object> drawnMap;

		// private Map<Shape, Object> memoDrawnMap; // to optimize draw () below

		/**
		 * Constructor, parts contains all the parts or one single part.
		 */
		public InnerPanel(MultipartSceneViewer caller, String name, List<String> partNames, 
				OVSelector ovSelector) {

			this.caller = caller;
			this.name = name;
			this.partNames = partNames;
			this.ovSelector = ovSelector;
			
			this.parts = new ArrayList<>();

			drawnMap = new HashMap<>();

			// First display will be a new selection
			thisIsAReselection = true;

			createUI();
		}

		/**
		 * Called at first update(), when a step is given. The panel2D can be
		 * created
		 */
		private void init() {

			// Calculate the bounds of the drawing in user coordinates
			double xMin = Double.MAX_VALUE;
			double xMax = -Double.MAX_VALUE;
			double yMin = Double.MAX_VALUE;
			double yMax = -Double.MAX_VALUE;

			for (ScenePart part : parts) {
				List<Polygon2> polygons = part.getPolygons();
				for (Polygon2 p : polygons) {
					xMin = Math.min(xMin, p.getXmin());
					xMax = Math.max(xMax, p.getXmax());
					yMin = Math.min(yMin, p.getYmin());
					yMax = Math.max(yMax, p.getYmax());
				}
			}
			userBounds = new Rectangle.Double(xMin, yMin, xMax - xMin, yMax - yMin);

			// Create the panel2D to draw within these bounds
			panel2D = new Panel2D(this, userBounds, Panel2D.X_MARGIN_IN_PIXELS, Panel2D.Y_MARGIN_IN_PIXELS);

		}

		/**
		 * Synchronizes the Panel on the given step
		 */
		public void udpatePanel(Step step) {

			// We change steps, user did not change the selection
			thisIsAReselection = false;

			this.step = step;
			scene = (MultipartScene) step.getScene();

			// Get the parts to be drawn
			parts.clear();
			for (ScenePart part : scene.getParts())
				if (partNames.contains(part.getName()))
					parts.add(part);

			// First call
			if (panel2D == null)
				init();

			// panel2D might change size
			scrollPane.setViewportView(panel2D);

			// To disable buffered image and force redrawing
			panel2D.reset();

			// Will call draw () below
			panel2D.repaint();

			this.validate();
			this.repaint();
		}

		/**
		 * Drawer interface
		 */
		@Override
		public void draw(Graphics g, Rectangle2D.Double r) {

			Graphics2D g2 = (Graphics2D) g;

			Color textColor = Color.BLACK;

			drawnMap.clear();

			// fc-3.7.2019 defer part names drawing to them on the top
			Map<Vertex2d,String> nameMap = new HashMap<> ();
			
			// For each part
			for (ScenePart part : parts) {

				List<Polygon2> polygons = part.getPolygons();

				double xMin = Double.MAX_VALUE;
				double xMax = -Double.MAX_VALUE;
				double yMin = Double.MAX_VALUE;
				double yMax = -Double.MAX_VALUE;

				// For each polygon of the part
				Shape sh = null;
				for (Polygon2 polygon : polygons) {

					xMin = Math.min(xMin, polygon.getXmin());
					xMax = Math.max(xMax, polygon.getXmax());
					yMin = Math.min(yMin, polygon.getYmin());
					yMax = Math.max(yMax, polygon.getYmax());

					sh = Geometry.getShape(polygon);

					// Color - fc-22.5.2012
					int[] rgb = part.getRGB();
					Color fillColor = new Color(rgb[0], rgb[1], rgb[2]);
					Color edgeColor = fillColor.darker();

					g2.setColor(fillColor);
					g2.fill(sh);
					g2.setColor(edgeColor);
					g2.draw(sh);

					drawnMap.put(sh, part);

				}

				// fc-25.6.2015 Optional further drawing
				// The drawers store the shapes they draw in drawnMap to ease
				// selection management
				scene.draw(g2, r, part, drawnMap);

				// Draw the the label of this part at the center of its bounding
				// box
				g2.setColor(textColor);
				Rectangle2D bbox = sh.getBounds2D();

				// fc-3.7.2019 deferred part names drawing, see lower
				nameMap.put(new Vertex2d(bbox.getCenterX(), bbox.getCenterY()), part.getName());
//				g2.drawString(part.getName(), (int) bbox.getCenterX(), (int) bbox.getCenterY());

			}
			
			// fc-3.7.2019 deferred part names drawing
			for (Vertex2d v : nameMap.keySet()) {
				String name = nameMap.get(v);
				g2.drawString(name, (int) v.x, (int) v.y);
			}
			
			// fc-7.1.2019 In case something should be drawn after (in top of)
			// the scene parts
			scene.drawMore(g2, r, drawnMap);

		}

		/**
		 * Drawer interface
		 */
		@Override
		public JPanel select(Rectangle2D.Double r, boolean more) {

			// In case of a reselection (if viewer is synchronized on another
			// step), r is passed null, we use the last time memorized selection
			thisIsAReselection = false;
			if (r == null) {
				r = memoSelectionRectangle;
				thisIsAReselection = true;
			}

			// fc-12.4.2019
			OVDialog dlg = ovSelector.getOvDialog();
			boolean dlgIsClosed = (dlg == null) || (dlg != null && !dlg.isVisible ());
			if (thisIsAReselection && dlgIsClosed)
				return null;
			
			// In case r is still null (early call), return
			if (r == null)
				return null;

			// Memo the selection rectangle in case of later reselection
			memoSelectionRectangle = r; // for reselect

			Collection memoSelection = searchInRectangle(r);

			// Selection is delegated to MultipartSceneViewer and its ovSelector
			Collection effectiveSelection = caller.select(memoSelection, thisIsAReselection);

			// fc-5.7.2018 WE DO NOT manage effectiveSelection showing here

			// rearm selection for next time (detail: in reselection mode,
			// if the OVDialog is unvisible, it is not set visible)
			// thisIsAReselection = false;
			//
			// // Force panel repainting for effective selection enlighting
			// panel2D.reset();
			// panel2D.repaint();

			return null; // OVSelector framework: the OVSelector takes care of
							// showing the ov in a separate dialog, we can
							// return null to the panel2D in which user selected
		}

		/**
		 * This methods returns the object selected by the given rectangle.
		 */
		protected Collection<Object> searchInRectangle(Rectangle.Double r) {
			Collection<Object> inRectangle = new ArrayList<Object>();

			// null rectangle: nothing found
			if (r == null)
				return inRectangle;

			// What drawn objects intersect the user selection rectangle ?
			for (Shape shape : drawnMap.keySet()) {

				if (shape.intersects(r)) {

					Object o = drawnMap.get(shape);
					inRectangle.add(o);

				}

			}

			return inRectangle;
		}

		// public Rectangle.Double getMemoSelectionRectangle() {
		// return memoSelectionRectangle;
		// }

		public String getName() {
			return name;
		}

		/**
		 * Create the user interface.
		 */
		private void createUI() {

			setLayout(new BorderLayout());

			// scrollPane will contain the Panel2D with the drawing inside of
			// the Panels' parts (one single part or all parts depending on the
			// separate checkbox in MultiPartSceneViewer)
			scrollPane = new JScrollPane(new JPanel());

			add(scrollPane, BorderLayout.CENTER);

		}

		/**
		 * Needed to sort Inner panels.
		 */
		@Override
		public int compareTo(Object o) {
			InnerPanel p1 = this;
			InnerPanel p2 = (InnerPanel) o;
			return p1.name.compareTo(p2.name);
		}

	}

}
