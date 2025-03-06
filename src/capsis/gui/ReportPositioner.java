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

package capsis.gui;

import java.awt.Color;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.Set;
import java.util.TreeSet;

import javax.swing.JComponent;
import javax.swing.JDesktopPane;
import javax.swing.JInternalFrame;
import javax.swing.JTabbedPane;
import javax.swing.SwingConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import capsis.extension.DiagramFrame;
import capsis.util.Page;

/**
 * Sets the position for components in Capsis MainFrame in a docked manner,
 * adding tabs.
 * 
 * @author F. de Coligny - September 2003
 */
public class ReportPositioner extends DockedPositioner implements Page, ChangeListener {
	// fc-12.1.2018 see getNbDesktops ()
	public final static int DEFAULT_NB_DESKTOPS = 10;
	
	public final static int DESKTOPS_WIDTH = 800;
	public final static int DESKTOPS_HEIGHT = 1200;

	public static final int BROWSER_WIDTH = 280;
	public static final int BROWSER_HEIGHT = 550;

	public static final int FUZZINESS = 20;

	private JTabbedPane tabs;
	private JDesktopPane[] desktops;

	private int currentPage; // first page is 0

	/**
	 * Constructor 1
	 */
	public ReportPositioner(MainFrame mainFrame) {
		this(mainFrame, null);
	}

	/**
	 * Constructor 2
	 */
	public ReportPositioner(MainFrame mainFrame, Positioner previousPositioner) {
		super(mainFrame, previousPositioner);
	}

	/**
	 * Returns number of pages
	 */
	public int getPageNumber() {
		return desktops.length;
	}
	
	/**
	 * Number of desktops can be changed by changing the related property.
	 */
	static public int getNbDesktops () {
		// fc-12.1.2018 If the property is set, use it, else use default value
		return Settings.getProperty("ReportPositioner.preferred.nb.desktops", DEFAULT_NB_DESKTOPS);
	}
	
	/**
	 * Create desktop panes
	 */
	@Override
	protected JComponent createDesktops() {
		tabs = new JTabbedPane(SwingConstants.TOP);
		tabs.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);
		tabs.setBorder(null);
		tabs.addChangeListener(this);

		desktops = new JDesktopPane[getNbDesktops ()];

		for (int i = 0; i < getNbDesktops (); i++) {
			JDesktopPane d = new JDesktopPane(); // to add InternalFrames inside
			d.setDragMode(JDesktopPane.OUTLINE_DRAG_MODE);
			d.setBackground(Color.WHITE); // fc-24.9.2003
			d.setBorder(null);

			desktops[i] = d;

			tabs.addTab(Translator.swap("Shared.page") + " " + (i + 1), desktops[i]);
		}
		currentPage = 0;
		desktop = desktops[currentPage]; // current desktop

		// fc-9.11.2005 Refresh layout (mosaic...)
		desktop.addComponentListener(new ComponentAdapter() {
			public void componentResized(ComponentEvent e) {
				autoLayoutDesktops();
			}
		});

		return tabs;
	}

	/**
	 * Listen to tabs change, set current tab
	 */
	public void stateChanged(ChangeEvent evt) {
		if (evt.getSource().equals(tabs)) {
			int index = tabs.getSelectedIndex();
			currentPage = index;
			desktop = desktops[index];
		}
	}

	protected Set<DiagramFrame> getDiagramFrames() {
		JInternalFrame[] currentPageFrames = desktops[currentPage].getAllFrames();

		Set<DiagramFrame> frames = new TreeSet<>();
		for (JInternalFrame ifr : currentPageFrames) {
			frames.add((DiagramFrame) ifr);
		}
		return frames;
	}

	/**
	 * Refresh layout (cascade, mosaic...) for all the desktops
	 */
	public void autoLayoutDesktops() {
		int memo = currentPage;
		for (int i = 0; i < getNbDesktops (); i++) {
			currentPage = i;
			autoLayoutDesktop();
		}
		currentPage = memo;
	}

}
