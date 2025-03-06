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

import java.awt.Component;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.WeakHashMap;

import jeeb.lib.util.AmapDialog;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.extension.AbstractDiagram;
import capsis.extension.AbstractStandViewer;
import capsis.extension.DiagramFrame;
import capsis.extension.datarenderer.AbstractPanelDataRenderer;
import capsis.gui.selectordiagramlist.DiagramLine;
import capsis.util.InfoDialog;

/**
 * Superclass of component positioners. Provides methods to compute location and
 * size of graphical interface components inside the mainFrame.
 * 
 * @author F. de Coligny (May 2000), S. Dufour-Kowalski (2010), F. de Coligny
 *         (December 2015)
 */
abstract public class Positioner {

	// Main frame
	protected MainFrame mainFrame;

	// Which internal frame contains which diagram (i.e. viewer or dataRenderer)
	// InternalFrames are hold by weak references. We rely on this single
	// (rather small) map to manage the bi-directional association.
	// fc-8.12.2015
	private WeakHashMap<DiagramFrame, AbstractDiagram> internalFrame_diagram;

	// A list (optional) of Diagrams (className) to be laid out, contains info
	// on their index (OR locations OR sizes)
	protected Map<String, Integer> upcomingDiagramsMap; // may be null

	// Diagram unique index management
	static private int diagramCounter = 0;

	/**
	 * Constructor
	 */
	public Positioner(MainFrame mainFrame) {

		// The top level frame we are working in
		this.mainFrame = mainFrame;

		// Create the internalFrame_diagram map
		internalFrame_diagram = new WeakHashMap<DiagramFrame, AbstractDiagram>();

	}

	/**
	 * Management of internalFrame_diagram.
	 * 
	 * Returns the internal frame containing the given diagram.
	 */
	// fc-8.12.2015
	public DiagramFrame getInternalFrame(AbstractDiagram diagram) {
		for (DiagramFrame f : internalFrame_diagram.keySet()) {
			AbstractDiagram d = internalFrame_diagram.get(f);
			if (d.equals(diagram))
				return f;
		}
		return null;
	}

	/**
	 * Returns all the DiagramFrames in a set.
	 */
	public Set<DiagramFrame> getAllDiagramFrames() {
		return internalFrame_diagram.keySet();
	}

	/**
	 * Management of internalFrame_diagram.
	 * 
	 * Returns the diagram in the given internal frame.
	 */
	// fc-8.12.2015
	public AbstractDiagram getDiagram(DiagramFrame internalFrame) {
		AbstractDiagram d = internalFrame_diagram.get(internalFrame);
		return d;
	}

	/**
	 * Management of internalFrame_diagram.
	 * 
	 * Set the diagram in the internalFrame and store the association.
	 */
	// fc-8.12.2015
	public void setDiagram(DiagramFrame f, AbstractDiagram diagram) {
		// Set the new diagram in the internalFrame
		f.setContentPane(diagram);

		// Register the new diagram's frame
		internalFrame_diagram.put(f, diagram);

	}

	/**
	 * Management of internalFrame_diagram.
	 * 
	 * Removes the internalFrame for the given diagram.
	 */
	// fc-8.12.2015
	public void removeDiagram(AbstractDiagram diagram) {
		DiagramFrame foundFrame = null;
		for (DiagramFrame f : internalFrame_diagram.keySet()) {
			AbstractDiagram d = internalFrame_diagram.get(f);
			if (d.equals(diagram)) {
				foundFrame = f;
				break;
			}
		}
		if (foundFrame != null)
			internalFrame_diagram.remove(foundFrame);
	}

	/**
	 * Management of internalFrame_diagram.
	 * 
	 * Returns the number of diagrams / internalFrames.
	 */
	// fc-8.12.2015
	// public int diagramCount() {
	// return internalFrame_diagram.size();
	// }

	// public int getMaxDiagramIndex () {
	// for (AbstractDiagram d : internalFrame_diagram.values()) {
	//
	// }
	// }

	/**
	 * When several diagrams are to be added at the same time, calling this
	 * method may help to choose their order / locations / sizes.
	 */
	public void prepareUpcomingDiagrams(Set<DiagramLine> diagrams) {
		if (upcomingDiagramsMap == null)
			upcomingDiagramsMap = new HashMap<>(); // fc-7.12.2015

		// Tell the positioner about the opening order (threaded opening)
		synchronized (this) {

//			System.out.println("Positioner upcoming, diagramCounter was: " + diagramCounter);

			int anchor = ++diagramCounter;

//			System.out.println("Positioner upcoming, anchor: " + anchor);

			// Shift the indices if there are already diagrams
			int max = 0;
			int i = 1;
			for (DiagramLine d : diagrams) {

				int newIndex = anchor + i;
				max = Math.max(max, newIndex);

				upcomingDiagramsMap.put(d.getClassName(), newIndex);

//				System.out.println("Positioner upcoming, diagram: " + d.getClassName() + "rank: " + d.getRank()
//						+ " originalIndex: " + i + " newIndex: " + newIndex);
				
				i++;
			}

			diagramCounter = max;

//			System.out.println("Positioner upcoming, reset diagramCounter to: " + diagramCounter);
		}

	}

	/**
	 * Index strategy.
	 */
	public void setIndexIfMissing(AbstractDiagram diagram) {

		// Set diagram index if not set

		// Already set
		if (diagram.getIndex() > 0)
			return;

		// An index was proposed for this upcoming diagram
		String dcn = diagram.getDiagramClassName();
		if (upcomingDiagramsMap != null && upcomingDiagramsMap.keySet().contains(dcn)) {
			diagram.setIndex(upcomingDiagramsMap.get(dcn));
			upcomingDiagramsMap.remove(dcn); // done
			return;
		}

		// For diagrams opened one by one
		synchronized (this) {
			int newIndex = ++diagramCounter;
			diagram.setIndex(newIndex);
		}

	}

	/**
	 * Replaces the old diagram by the new diagram. The new diagram is added in
	 * the internalFrame of the old one, which is removed.
	 */
	public void replaceDiagram(AbstractDiagram oldDiagram, AbstractDiagram newDiagram) {

		// Get the internelFrame containing the old diagram
		DiagramFrame iframe = getInternalFrame(oldDiagram);
		// RootPaneContainer f = getRootContainer(oldDiagram);

		// Tell the old diagram does not own this internalFrame any more
		removeDiagram(oldDiagram);
		// map frames.remove(oldDiagram);

		// Same index for new diagram
		newDiagram.setIndex(oldDiagram.getIndex());

		// Set the new diagram in the internalFrame
		setDiagram(iframe, newDiagram);

	}

	abstract public void remove(Component comp);

	abstract public void removeInternalFrames();

	abstract public void setVisible(Component comp, boolean v);

	abstract public Collection<Component> getAllComponents();

	abstract public void clear();

	abstract public void mosaic();

	abstract public void cascade();

	abstract public void autoLayoutDesktop();

	abstract public void layoutComponent(ProjectManager projectManager);

	abstract public void layoutComponent(Selector selector);

	abstract public void layoutComponent(AbstractStandViewer diagram);

	abstract public void layoutComponent(AbstractPanelDataRenderer diagram);

	abstract public void layoutComponent(AmapDialog dlg);

	/** Layout */
	public void layOut(Component comp) {

		// InfoDialog
		if (comp instanceof InfoDialog)
			return;

		// Repositionable
		if (comp instanceof Repositionable)
			((Repositionable) comp).setLayout(this);

		// Other cases are ignored

	}

}
