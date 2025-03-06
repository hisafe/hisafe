/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski,
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */
package capsis.gui;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Random;
import java.util.Set;
import java.util.TreeSet;
import java.util.WeakHashMap;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JSplitPane;
import javax.swing.event.InternalFrameAdapter;
import javax.swing.event.InternalFrameEvent;

import capsis.commongui.projectmanager.ProjectManager;
import capsis.extension.AbstractDiagram;
import capsis.extension.AbstractStandViewer;
import capsis.extension.DiagramFrame;
import capsis.extension.PanelDataRenderer;
import capsis.extension.datarenderer.AbstractPanelDataRenderer;
import capsis.extensiontype.MuteDataBlock;
import capsis.util.PrintContext;
import capsis.util.RandomIntGenerator;
import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Disposable;
import jeeb.lib.util.Settings;
import jeeb.lib.util.WxHString;

/**
 * Sets the position for components in Capsis MainFrame in a docked manner.
 * 
 * @author F. de Coligny - April 2003
 */
public class DockedPositioner extends Positioner implements Printable {

	// public static final int SCRAMBLE = 0;
	// public static final int CASCADE = 1;
	// public static final int SAME_PLACE = 2;

	private JSplitPane horizontal;
	private JSplitPane vertical;

	protected int defaultVerticalDividerLocation;
	protected int defaultHorizontalDividerLocation;

	protected JLabel empty3;

	protected JDesktopPane desktop; // fc-22.4.2003

	protected Dimension minimumSize = new Dimension(250, 250);

	protected Random random;

	protected int defaultToolWidth;
	protected int defaultToolHeight;

	// Weak reference set : opened components
	protected Set<Component> components = Collections.newSetFromMap(new WeakHashMap<Component, Boolean>());

	// Internal frame index (to be able to order them)
	private WeakHashMap<DiagramFrame, Integer> framesIndex = new WeakHashMap<DiagramFrame, Integer>();
	private int frameIndex = 0;

	private Map<Object, DiagramFrame> internalFrames = new HashMap<Object, DiagramFrame>(); // fc-25.11.2011

	/**
	 * Constructor 1
	 */
	public DockedPositioner(MainFrame mainFrame) {
		this(mainFrame, null);
	}

	/**
	 * Constructor 2
	 */
	public DockedPositioner(MainFrame mainFrame, Positioner previousPositioner) {
		super(mainFrame);

		random = new Random();
		defaultToolWidth = 250;
		defaultToolHeight = 250;

		Settings.setProperty("capsis.positioner", getClass().getName());

		// Previous positioner, ask him to clean out
		Collection<Component> comps = null;
		if (previousPositioner != null) {
			comps = new ArrayList<Component>(previousPositioner.getAllComponents());

			if (previousPositioner instanceof DockedPositioner) {
				DockedPositioner p = (DockedPositioner) previousPositioner;
				frameIndex = p.frameIndex;
				framesIndex = p.framesIndex;
			}

			previousPositioner.clear();
		}

		// Layout split panes
		horizontal = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT);
		horizontal.setResizeWeight(0.1);
		vertical = new JSplitPane(JSplitPane.VERTICAL_SPLIT);
		vertical.setResizeWeight(0.1);

		JComponent space = createDesktops();

		// fc-11.1.2019 Memo vertical locations to restore them
		defaultHorizontalDividerLocation = Settings.getProperty("DockedPositioner.defaultHorizontalDividerLocation",
				300);
		defaultVerticalDividerLocation = Settings.getProperty("DockedPositioner.defaultVerticalDividerLocation", 110);

		empty3 = new JLabel();
		empty3.setBackground(Color.WHITE);
		empty3.setOpaque(true);
		empty3.setMinimumSize(minimumSize);

		horizontal.setLeftComponent(empty3);
		horizontal.setRightComponent(vertical);
		horizontal.setBorder(BorderFactory.createEmptyBorder(5, 5, 5, 5));
		horizontal.setDividerLocation(defaultHorizontalDividerLocation);
		horizontal.setOneTouchExpandable(true);
		
		vertical.setTopComponent(ProjectManager.getInstance());
		vertical.setBottomComponent(space);
		vertical.setBorder(null);
		vertical.setDividerLocation(defaultVerticalDividerLocation);
		vertical.setOneTouchExpandable(true);

		// fc-11.1.2019 Memo vertical locations to restore them
		horizontal.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent pce) {
				Settings.setProperty("DockedPositioner.defaultHorizontalDividerLocation",
						horizontal.getDividerLocation());
			}
		});
		vertical.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, new PropertyChangeListener() {
			@Override
			public void propertyChange(PropertyChangeEvent pce) {
				Settings.setProperty("DockedPositioner.defaultVerticalDividerLocation",
						vertical.getDividerLocation());
			}
		});


	mainFrame.getContentPane().add(horizontal,BorderLayout.CENTER);

	// Position toolbar and statusbar
	JComponent toolBar = mainFrame.getToolBar();
	JComponent statusBar = mainFrame
			.getStatusBar();mainFrame.getContentPane().add(toolBar,BorderLayout.NORTH);mainFrame.getContentPane().add(statusBar,BorderLayout.SOUTH);

	// Need to do this before first layOut (desktop must have a size)
	mainFrame.getContentPane().validate();

	// Previous positioner, re-lay out its components
	if(previousPositioner!=null&&comps!=null)
	{
		for (Component c : comps) {

			// System.out.println("DockedPositioner, restoring component: "
			// + c.getClass().getName());

			DiagramFrame ifr = null;
			if (c instanceof AbstractDiagram) {
				ifr = getInternalFrame((AbstractDiagram) c);
			}

			if (ifr != null) {
				desktop.add(ifr);
				components.add(c);
			} else {
				layOut(c);
			}
		}
		autoLayoutDesktop();

	}

	horizontal.setDividerLocation(defaultHorizontalDividerLocation);vertical.setDividerLocation(defaultVerticalDividerLocation);

	}

	protected JComponent createDesktops() {
		// Create desktop pane
		desktop = new JDesktopPane(); // to add InternalFrames inside
		desktop.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);
		desktop.setBackground(Color.WHITE); // fc-14.9.2004 - nicer

		// fc-9.11.2005 - refresh layout (mosaic...)
		desktop.addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent e) {
				autoLayoutDesktop();
			}
		});

		return desktop;
	}

	/**
	 * Called when changing positioners.
	 */
	@Override
	public void clear() {
		mainFrame.getContentPane().removeAll();
	}

	protected Set<DiagramFrame> getDiagramFrames() {
		JInternalFrame[] currentPageFrames = desktop.getAllFrames();

		Set<DiagramFrame> frames = new TreeSet<>();
		for (JInternalFrame ifr : currentPageFrames) {
			frames.add((DiagramFrame) ifr);
		}
		return frames;
	}

	/**
	 * Return the complete list of managed components.
	 */
	public Collection<Component> getAllComponents() {

		return components; // fc-7.12.2015

		// // fc-7.12.2015
		// // Order the frames according to their index
		// Collection<Component> orderedComponents = new ArrayList<>();
		//
		// // Retrieve all internal frames
		// Set<DiagramFrame> frames = getDiagramFrames();
		//
		// // Set<DiagramFrame> f = new TreeSet<DiagramFrame>(new
		// // FrameComparator(framesIndex));
		// for (DiagramFrame frame : frames) {
		// if (frame.isVisible() && !frame.isIcon()) {
		// orderedComponents.add(frame.getContentPane());
		// }
		// }
		//
		// return orderedComponents;
		// // fc-7.12.2015

	}

	/**
	 * Remove a component from memo list.
	 */
	@Override
	public void remove(Component comp) {

		// E.g. comp may be a StandViewer or a DataRenderer

		if (comp instanceof AbstractDiagram) {
			// DiagramFrame is an internalFrame containing the component
			DiagramFrame df = getInternalFrame((AbstractDiagram) comp);

			// Try to dispose the internal frame
			if (df != null) {
				df.dispose();

				// fc-14.10.2018 Release df references (GC pbs)
				releaseDiagramFrame(df);
			}

		}

		components.remove(comp);

		autoLayoutDesktop();

	}

	/**
	 * When a diagramFrame is no longer to be used, release all references to
	 * it.
	 */
	private void releaseDiagramFrame(DiagramFrame df) {

		// System.out.println();
		// System.out.println("DockedPositioner.releaseDiagramFrame ()...");
		// System.out.println("BEFORE releasing");
		// System.out.println("framesIndex: " +
		// AmapTools.toString(framesIndex));
		// System.out.println("internalFrames: " +
		// AmapTools.toString(internalFrames));

		// fc-14.10.2018 Release df in the maps too (GC pbs)
		framesIndex.remove(df);

		// fc-14.10.2018 Remove the entries referencing the closed
		// internalFrame (GC pbs)
		for (Object key : new HashSet(internalFrames.keySet()))
			if (internalFrames.get(key).equals(df))
				internalFrames.remove(key);

		// fc-14.10.2018 Was missing
		
//		System.out.println("DockedPositioner LocationSizeManager.unregisterListener(df) df: " + df + "...");
		
		LocationSizeManager.unregisterListener(df);

		// System.out.println();
		// System.out.println("AFTER releasing");
		// System.out.println("framesIndex: " +
		// AmapTools.toString(framesIndex));
		// System.out.println("internalFrames: " +
		// AmapTools.toString(internalFrames));
		// System.out.println();

	}

	/**
	 * Remove all internal frames
	 */
	@Override
	public void removeInternalFrames() {

		for (DiagramFrame df : framesIndex.keySet()) {
			df.dispose();

			// fc-14.10.2018 Release df references (GC pbs)
			releaseDiagramFrame(df);

			// // fc-14.10.2018 Was missing
			// System.out.println("DockedPositioner
			// LocationSizeManager.unregisterListener(df) df: "+df+"...");
			// LocationSizeManager.unregisterListener(df);
		}

		// framesIndex.clear(); // fc-14.10.2018 Was missing
		// internalFrames.clear(); // fc-25.11.2011

		components.clear(); // fc-14.10.2018 Was missing

		autoLayoutDesktop();
	}

	/**
	 * Manage component visibility.
	 */
	public void setVisible(Component comp, boolean v) {

		// ScenarioManager
		if (comp instanceof ProjectManager) {
			if (v) {
				if (vertical.getDividerLocation() == 0) {
					vertical.setDividerLocation(defaultVerticalDividerLocation);
				}
			} else {
				vertical.setDividerLocation(0);
			}

			// Selector
		} else if (comp instanceof Selector) {
			if (v) {
				horizontal.setDividerLocation(defaultHorizontalDividerLocation);
			} else {
				horizontal.setDividerLocation(0);
			}

		}

	}

	/**
	 * Manage component location and size.
	 */
	@Override
	public void layoutComponent(ProjectManager pm) {

		components.add(pm);

		pm.setMinimumSize(minimumSize);

		try {
			((Disposable) vertical.getTopComponent()).dispose();
		} catch (Exception e) {
		}
		vertical.remove(vertical.getTopComponent());
		vertical.setTopComponent(pm);

	}

	@Override
	public void layoutComponent(Selector selector) {

		components.add(selector);

		selector.setMinimumSize(new Dimension(150, 100));
		try {
			((Disposable) horizontal.getLeftComponent()).dispose();
		} catch (Exception e) {
		}
		horizontal.remove(horizontal.getLeftComponent());
		horizontal.setLeftComponent(selector);

	}

	@Override
	public void layoutComponent(AbstractStandViewer viewer) {

		components.add(viewer);

		viewer.setMinimumSize(minimumSize);

		// Replace the contents of the internalFrames when possible
		DiagramFrame df = embedInFrame(viewer, viewer);

		if (df == null)
			return; // the internalFrame already existed

		String className = viewer.getClass().getName();

		// Register listener for size and position
		// System.out.println("DockedPositioner
		// LocationSizeManager.registerListener(df) df: " + df + "...");
		LocationSizeManager.registerListener(df, className);

		Color color = viewer.getStepButton().getColor();
		AbstractStandViewer.colorize(df, color);

		df.setVisible(true);
		viewer.update(); // fc-22.4.2003 - ex: if size changes, reset /
							// repaint maps

		Dimension size = LocationSizeManager.restoreSize(className);
		if (size.height > 0 || size.width > 0) {
			df.setSize(size);
		}

		Point p = LocationSizeManager.restoreLocation(className);
		if (p.x > 0 || p.y > 0) {
			df.setLocation(p);
		}

		autoLayoutDesktop(); // fc-3.11.2005

	}

	@Override
	public void layoutComponent(AbstractPanelDataRenderer renderer) {

		// fc-15.10.2003 MuteDataBlocks : use an extractor with a data
		// renderer embedded in some component (e.g. objectrenderer containing
		// curves - related with S. Chalon work for PhD)
		if (renderer.getDataBlock() instanceof MuteDataBlock)
			return;

		components.add(renderer);

		renderer.setMinimumSize(minimumSize);

		String className = renderer.getDataBlock().getExtractorType();

		// fc-4.2.2011 JFCLineChart does not have a name (?)
		if (renderer.getName() == null || renderer.getName().equals("")) {
			renderer.setName(renderer.getTitle());
		}

		// Replace the contents of the internalFrames when possible
		DiagramFrame df = embedInFrame(renderer.getDataBlock(), renderer);

		if (df == null)
			return; // the internalFrame already existed

		// System.out.println("DockedPositioner
		// LocationSizeManager.registerListener(df) df: " + df + "...");
		LocationSizeManager.registerListener(df, className);

		df.setVisible(true);

		Dimension size = LocationSizeManager.restoreSize(className);
		if (size.height > 0 || size.width > 0) {
			df.setSize(size);
		}

		Point p = LocationSizeManager.restoreLocation(className);
		if (p.x > 0 || p.y > 0) {
			df.setLocation(p);
		}

		autoLayoutDesktop(); // fc-3.11.2005

	}

	@Override
	public void layoutComponent(AmapDialog comp) {
		RandomIntGenerator randomX = new RandomIntGenerator(-10, 40);
		RandomIntGenerator randomY = new RandomIntGenerator(-10, 30);
		int x = new WxHString(Settings.getProperty("capsis.generic.tool.location", "")).getW();
		int y = new WxHString(Settings.getProperty("capsis.generic.tool.location", "")).getH();
		comp.setLocation(x + randomX.draw(), y + randomY.draw());
	}

	/**
	 * We want to embed in a DiagramFrame the given panel showing the given
	 * dataModel. If there was already a DiagramFrame showing this dataModel,
	 * replace its contents by the given panel and return null (no new
	 * internalFrame was created). Otherwise, create a new DiagramFrame and
	 * return it at the end.
	 */
	protected DiagramFrame embedInFrame(Object dataModel, AbstractDiagram diagram) {

		// Set diagram index if not set yet (for frames sorting below)
		setIndexIfMissing(diagram);

		DiagramFrame prevF = internalFrames.get(dataModel);
		if (prevF != null) {
			// Note: if the previous frame was maximized, it stays maximized
			prevF.setContentPane(diagram);
			return null;
		}

		DiagramFrame df = getInternalFrame(diagram);

		// Create internal frame
		if (df == null) {

			df = new DiagramFrame(diagram, true, true, true, true);

			// Set the new diagram in the internalFrame
			setDiagram(df, diagram);

			int index = frameIndex;

			// Several diagrams may be added at the same time -> an order may
			// have been proposed
			if (upcomingDiagramsMap != null) { // fc-7.12.2015
				String className = diagram.getClass().getName();
				Integer i = upcomingDiagramsMap.get(className);
				if (i != null) {
					index = i;
					frameIndex = i;
				}
			}

			framesIndex.put(df, index);

			frameIndex++;

			// The diagram is inside the diagramFrame (internalFrame)
			final AbstractDiagram fDiagram = diagram;
			final Disposable fDisposable = (Disposable) diagram;
			final DiagramFrame fdf = df;

			// Good for standviewer & datarenderer, but not for SM or Selector

			df.addInternalFrameListener(new InternalFrameAdapter() {

				public void internalFrameClosed(InternalFrameEvent evt) {
					try { // fc-13.10.2013 Added try, catch: a bug may prevent
							// the internalFrames to close, do some stuff to
							// remove
							// all reference to the internal frame and avoid
							// memory leak
						MainFrame.getInstance().requestFocus();
						MainFrame.getInstance().getSelector().requestFocus();
						fDisposable.dispose();
						desktop.remove(fdf);
						desktop.selectFrame(true);

						// fc-14.8.2018 release all references in
						// DockedPositioner
						// fc-14.10.2018 Release df references (GC pbs)
						releaseDiagramFrame(fdf);

					} catch (Exception e) {
						// Do nothing
					}

				}
			});

			desktop.add(df);

			df.setSize(minimumSize);

			int offset = framesIndex.size() * 50;
			df.setLocation(offset, offset);

			// Store the new internalFrame
			internalFrames.put(dataModel, df);

		}

		return df;
	}

	protected int getX0() {
		return mainFrame.getContentPane().getLocationOnScreen().x;
	}

	protected int getY0() {
		return mainFrame.getContentPane().getLocationOnScreen().y;
	}

	protected int getWidth() {
		return desktop.getWidth();
	}

	protected int getHeight() {
		return desktop.getHeight();
	}

	/**
	 * Return a default location for the component
	 */
	protected String getDefaultAnchor(Component comp) {

		int wMax = getWidth();
		int hMax = getHeight();

		if (comp instanceof AbstractStandViewer || comp instanceof PanelDataRenderer) {
			int x = (int) (wMax / 2 * random.nextDouble());
			int y = (int) (hMax / 2 * random.nextDouble());
			return x + "x" + y;
		}
		return "10x10";
	}

	/**
	 * Return a default size for the component.
	 */
	protected String getDefaultSize(Component comp) {

		if (comp instanceof AbstractStandViewer || comp instanceof PanelDataRenderer) {
			return defaultToolWidth + "x" + defaultToolHeight;
		}
		return "300x300";
	}

	public void componentShown(ComponentEvent evt) {
		autoLayoutDesktop();
	}

	public void componentHidden(ComponentEvent evt) {
		autoLayoutDesktop();
	}

	/**
	 * Manage default tool size.
	 */
	public void setDefaultToolWidth(int v) {
		defaultToolWidth = v;
	}

	public void setDefaultToolHeight(int v) {
		defaultToolHeight = v;
	}

	/**
	 * Print method. Called by a printJob. See capsis.gui.command.PrintPreview
	 * and capsis.gui.command.Print.
	 */
	public int print(Graphics g, PageFormat pageformat, int i) throws PrinterException {

		if (i >= 1) {
			return NO_SUCH_PAGE;
		}

		Graphics2D graphics2d = (Graphics2D) g;
		graphics2d.translate(pageformat.getImageableX(), pageformat.getImageableY());

		if (PrintContext.isFitToSize()) { // default
			double d = pageformat.getImageableWidth() / (double) getWidth();
			double d1 = pageformat.getImageableHeight() / (double) getHeight();
			double d2 = Math.min(d, d1);
			graphics2d.scale(d2, d2);
		} else {
			java.awt.geom.Rectangle2D.Double double1 = new java.awt.geom.Rectangle2D.Double();
			double1.setRect(0.0D, 0.0D, pageformat.getImageableWidth(), pageformat.getImageableHeight());
			graphics2d.clip(double1);
		}

		Color rescue = desktop.getBackground();
		desktop.setBackground(Color.WHITE); // fc-24.9.2003 (ok)
		desktop.print(graphics2d);
		desktop.setBackground(rescue);

		return PAGE_EXISTS;
	}

	/**
	 * Arrange the internal frames in zone 2
	 */
	public void mosaic() {

		// System.out.println("DockedPositioner mosaic was called...");

		// If still upcoming diagram, wait for running mosaic...
		if (upcomingDiagramsMap != null && !upcomingDiagramsMap.isEmpty())
			return;

		// Retrieve all internal frames
		Set<DiagramFrame> frames = getDiagramFrames();
		if (frames.size() == 0)
			return; // No frames

		// Count visible frames
		Set<DiagramFrame> visibleFrames = new TreeSet<>();
		for (DiagramFrame frame : frames) {
			if (frame.isVisible() && !frame.isIcon()) {
				visibleFrames.add(frame);
			}
		}
		if (visibleFrames.size() == 0)
			return; // No visible frames

		// System.out.println("DockedPositioner *** run mosaic () ***");

		// Calculate layout

		// desktop.getSize () is not ok under Linux / GTK+ L&F, there is a bar
		// at the bottom (kind of BorderLayout.SOUTH region) and the frames are
		// too high
		// NOT FOUND yet a nice workaround
		int n = visibleFrames.size();
		Dimension size = desktop.getSize(); // if we can not have better
		int width = size.width;
		int height = size.height;

		int nCol = (int) Math.ceil(Math.sqrt(n));
		int nLin = (int) Math.ceil(((double) n) / nCol);

		int w = width / nCol;
		int h = height / nLin;

		int cpt = 0;
		for (DiagramFrame frame : visibleFrames) {

			int l = cpt % nCol;
			int c = cpt / nCol;

			int x = l * w;
			int y = c * h;
			frame.setLocation(x, y);
			frame.setSize(w, h);
			cpt++;
		}
	}

	public void cascade() {
	} // fc-3.11.2005

	public void autoLayoutDesktop() {
		if (Settings.getProperty("auto.mode.mosaic", false)) {
			mosaic();
		}
		// else if (Settings.getProperty ("auto.mode.cascade", false)) {cascade
		// ();}
		// mosaic();
	}

}
