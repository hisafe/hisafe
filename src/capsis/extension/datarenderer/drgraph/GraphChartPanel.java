/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2015 INRA 
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
package capsis.extension.datarenderer.drgraph;

import java.awt.event.ActionEvent;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;

import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.extension.datarenderer.AbstractPanelDataRenderer;
import capsis.extension.datarenderer.DataRendererPopup;
import capsis.extensiontype.DataBlock;
import capsis.kernel.Step;
import jeeb.lib.util.ListenedTo;
import jeeb.lib.util.Listener;
import jeeb.lib.util.Translator;

/**
 * A specific JFreeChart ChartPanel for Capsis data renderers.
 * 
 * @author F. de Coligny - October 2015 (based on S. Dufour-Kowalski, 2010)
 */
public class GraphChartPanel extends ChartPanel implements Listener {

	protected DataBlock dataBlock;

	private JMenuItem resetGraph;

	/**
	 * Constructor
	 */
	public GraphChartPanel(JFreeChart chart, DataBlock dataBlock) {
		super(chart, true, true, false, true, true);

		setMouseZoomable(true);
		setMouseWheelEnabled(true);
		setDomainZoomable(true);
		setRangeZoomable(true);

		this.dataBlock = dataBlock;

		// Create the popup menu
		updatePopupMenu();

		Current.getInstance().addListener(this);
	}

	/**
	 * Called by Current
	 */
	@Override
	public void somethingHappened(ListenedTo l, Object param) {
		if (l instanceof Current) { // fc-9.3.2016
			try {
				// Current step has changed, update the popup menu
				updatePopupMenu();
			} catch (Exception e) {
				// The graph may have been closed: ignore
				// Current.getInstance().removeListener(this); //
				// ConcurrentModification
				dispose();
			}
		}

	}

	// fc-9.3.2016
	public void dispose() {
		// Deferred removeListener ()
		Current.getInstance().pleaseForgetMe(this);
	}

	/**
	 * For GraphChartPanel, the popup menu must be set by ChartPanel.setPopupMenu().
	 * But this popup must also be aware of the Current Step (for its add
	 * (CurrentStep) option. This object listens to Current and updates the popup
	 * each time user changes Current step by clicking in the ProjectManager.
	 */
	private void updatePopupMenu() { // fc-9.3.2016

		// We add a specific option for JFreeChart graphs (in which user can
		// zoom)
		List<JMenuItem> extraMenuItems = new ArrayList<>();

		resetGraph = new JMenuItem(Translator.swap("GraphChartPanel.resetGraph"));
		resetGraph.setEnabled(true);
		resetGraph.addActionListener(this);

		extraMenuItems.add(resetGraph);

		// Create the JPopupMenu with correct Current step and extra features
		boolean jpgExportFeature = true;
		JPopupMenu pm = new DataRendererPopup(dataBlock, jpgExportFeature, extraMenuItems);

		setPopupMenu(pm);

	}

	@Override
	public void actionPerformed(ActionEvent evt) {
		super.actionPerformed(evt);

		if (evt.getSource().equals(resetGraph)) {
			restoreAutoBounds();
		}
	}

	// fc-3.9.2021 Warning: In PanelDataRenderer, for renderers before JFreeChart
	// was used, the mouse events are checked in mousePressed and mouseReleased (Unix
	// and Windows need booth). In this class, they are checked in *mouseClicked*
	// and mouseReleased. Doing it in mousePressed results in loosing the resetGraph
	// additional option in the popupMenu -> we kept mouseClicked and added the check
	// for double click here
	@Override
	public void mouseClicked(MouseEvent evt) {

		// fc-3.9.2021 Added double click to open renderer and extractors configuration
		if (evt.getClickCount() == 2 && !evt.isConsumed()) {
			evt.consume();
			
			((AbstractPanelDataRenderer) dataBlock.getRenderer()).openConfigure();
			
		}

		// Ctrl-click : add an extractor to the data block for the current step
		if ((evt.getModifiers() & Tools.getCtrlMask()) != 0) {
			Step s = Current.getInstance().getStep();
			StepButton sb = ProjectManager.getInstance().getStepButton(s);

			if (!sb.isColored())
				ButtonColorer.getInstance().newColor(sb);

			dataBlock.addExtractor(sb.getStep());

			evt.consume(); // fc-12.5.2003
		}

		if (evt.isPopupTrigger()) {
			DataRendererPopup popup = new DataRendererPopup(dataBlock, true);
			popup.show(evt.getComponent(), evt.getX(), evt.getY());

			evt.consume(); // fc-12.5.2003
		}

		// Let superclass manage its mouse gestures (zoom...)
		if (!evt.isConsumed())
			super.mouseClicked(evt);

	}

	/**
	 * MouseListener interface.
	 */
	@Override
	public void mouseReleased(MouseEvent evt) {

		if (evt.isPopupTrigger()) {
			DataRendererPopup popup = new DataRendererPopup(dataBlock, true);
			popup.show(evt.getComponent(), evt.getX(), evt.getY());

			evt.consume(); // fc-12.5.2003
		}

		// Let superclass manage its mouse gestures (zoom...)
		if (!evt.isConsumed())
			super.mouseReleased(evt);

	}

}
