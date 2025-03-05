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

package capsis.extension;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.JInternalFrame;
import javax.swing.JPanel;

import jeeb.lib.util.Log;
import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.extension.dataextractor.configuration.DRConfigurationPanel;
import capsis.extension.datarenderer.AbstractPanelDataRenderer;
import capsis.extension.datarenderer.DataRendererPopup;
import capsis.extensiontype.DataBlock;
import capsis.extensiontype.DataExtractor;
import capsis.gui.Pilot;
import capsis.gui.Positioner;
import capsis.kernel.Step;
import capsis.util.CheckableItem;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Superclass for all Capsis data renderers. A DataRenderer is connected to one
 * DataBlock. It renders the DataExtractors connected to the block. Compatible
 * DataRenderers can be swapped to render the data block differently (curves,
 * histograms...).
 * 
 * @see DataBlock and DataExtractor.
 * @author F. de Coligny - july 2001 / march 2003
 */
abstract public class PanelDataRenderer extends AbstractPanelDataRenderer implements MouseListener {

	protected JPanel box; // the box where data block is drawn

	protected Color defaultColor;

	/**
	 * Constructor.
	 */
	public PanelDataRenderer() {
		super();

	}

	@Override
	public void init(DataBlock db) {
		try {
			setLayout(new BorderLayout());
			dataBlock = db;
			defaultColor = Color.BLACK;

			addMouseListener(this);
			reposition();
			setTransferHandler(StepButton.transfertHandler);

		} catch (Exception e) {// fc-20.1.2014
			Log.println(Log.ERROR, "PanelDataRenderer.init ()", "Error in init (), wrote in Log and passed", e);
		}
	}

	@Override
	public Component getPanel() {
		return this;
	}

	/** Change renderer status to "Updating..." */
	public void setUpdating() {
	}

	/**
	 * Change renderer status to "Updated"
	 */
	public void setUpdated() {
	}

	/**
	 * From Embedded interface. This component can be embedded in a container by the
	 * current positionner (ex: a JInternelFrame).
	 */
	public String getTitle() {
		return dataBlock.getName();
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	/**
	 * This method must be called by redefiners : super.createUI ().
	 */
	protected void createUI() {
		setBackground(Color.WHITE);
		setOpaque(true);
		setForeground(defaultColor);

		box = new JPanel();
		box.setLayout(new BorderLayout());
		box.setBackground(Color.WHITE);
		box.setOpaque(true);

		add(box, BorderLayout.CENTER);
	}

	/**
	 * Try to get faster.
	 */
	public boolean isDoubleBuffered() {
		return true;
	}

	/**
	 * MouseListener interface. Catches mouse events.
	 */
	// fc-3.9.2021 Added double click to open renderer and extractors configuration
	@Override
	public void mouseClicked(MouseEvent evt) {

		if (evt.getClickCount() == 2 && !evt.isConsumed()) {
			evt.consume();

			openConfigure();

		}

	}

	/**
	 * MouseListener interface. Catch right clicks.
	 */
	@Override
	public void mousePressed(MouseEvent evt) {

		// Ctrl-click : add an extractor to the data block for the current step
		if ((evt.getModifiers() & Tools.getCtrlMask()) != 0) {
			Step s = Current.getInstance().getStep();
			StepButton sb = ProjectManager.getInstance().getStepButton(s);

			if (!sb.isColored())
				ButtonColorer.getInstance().newColor(sb);

			dataBlock.addExtractor(sb.getStep());

			evt.consume(); // fc-12.5.2003
			return;
		}

		if (evt.isPopupTrigger()) {
			DataRendererPopup popup = new DataRendererPopup(dataBlock, true);
			popup.show(evt.getComponent(), evt.getX(), evt.getY());

			evt.consume(); // fc-12.5.2003
		}
	}

	/**
	 * MouseListener interface. Catch right clicks.
	 */
	@Override
	public void mouseReleased(MouseEvent evt) {
		
		if (evt.isPopupTrigger()) {
			DataRendererPopup popup = new DataRendererPopup(dataBlock, true);
			popup.show(evt.getComponent(), evt.getX(), evt.getY());

			evt.consume(); // fc-12.5.2003
		}
		
	}

	/**
	 * MouseListener interface. Unused.
	 */
	@Override
	public void mouseEntered(MouseEvent evt) {
	}

	/**
	 * MouseListener interface. Unused.
	 */
	@Override
	public void mouseExited(MouseEvent evt) {
	}

	/**
	 * Return the data block to which this renderer is connected.
	 */
	@Override
	public DataBlock getDataBlock() {
		return dataBlock;
	}

	/**
	 * From Configurable interface. Configurable interface allows to pass a
	 * parameter.
	 */
	public ConfigurationPanel getConfigPanel(Object parameter) {
		DRConfigurationPanel panel = new DRConfigurationPanel(this);
		return panel;
	}

	/**
	 * From Configurable interface. This method is called after renderer
	 * configuration. It updates the properties which are DataRenderer level.
	 */
	public void applyConfig(ConfigurationPanel panel) {
		DRConfigurationPanel p = (DRConfigurationPanel) panel;

		// Remove the checked extractors
		Collection items = p.getCheckableItems();
		for (Iterator i = items.iterator(); i.hasNext();) {
			CheckableItem item = (CheckableItem) i.next();
			if (item.isSelected()) {
				DataExtractor ex = item.getExtractor();
				getDataBlock().removeExtractor(ex);

				// fc-4.1.2017 release stepButtons colors
				ex.dispose();

			}
		}

	}

	/**
	 * From Configurable interface.
	 */
	public void postConfig() {
	}

	/**
	 * Renderer update. Subclass should redefine this method to render the
	 * extractors, beginning by "super.update ();".
	 */
	public void update() {
		try {
			// Embedder may be null depending on Current Positioner
			JInternalFrame ifr = Pilot.getInstance().getPositioner().getInternalFrame(this);
			if (ifr != null) {
				ifr.setTitle(getTitle());
			}
			setUpdated(); // fc - 21.9.2005

		} catch (Exception e) { // fc-20.1.2014
			Log.println(Log.ERROR, "PanelDataRenderer.update ()", "Error in update (), wrote in Log and passed", e);
		}
	}

}
