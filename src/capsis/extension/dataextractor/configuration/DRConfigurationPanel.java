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

package capsis.extension.dataextractor.configuration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import javax.swing.JColorChooser;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.Translator;
import capsis.extension.PanelDataRenderer;
import capsis.extension.dataextractor.superclass.AbstractDataExtractor;
import capsis.util.CheckListRenderer;
import capsis.util.CheckableItem;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;

/**
 * Configuration Panel for DataRenderers.
 * 
 * @author F. de Coligny - december 2000
 */
public class DRConfigurationPanel extends ConfigurationPanel {

	public Map ind_ex;
	public JList list;
	private Vector checkableItems;

	protected ColumnPanel mainContent; // fc-25.11.2014

	/**
	 * Constructor.
	 */
	public DRConfigurationPanel(Configurable obj) {
		super(obj);

		mainContent = new ColumnPanel(0, 0); // fc-25.11.2014
		setLayout(new BorderLayout());
		add(mainContent, BorderLayout.NORTH); // fc-25.11.2014

		ind_ex = new HashMap();

		// 1. DataExtractor list
		ColumnPanel c1 = new ColumnPanel();

		checkableItems = new Vector();
		boolean updateRequested = false;
		boolean visibleOnly = false;

		Collection v = ((PanelDataRenderer) obj).getDataBlock().getDataExtractors();
		for (Iterator i = v.iterator(); i.hasNext();) {
			AbstractDataExtractor ex = (AbstractDataExtractor) i.next();
			CheckableItem item = new CheckableItem(AmapTools.cutIfTooLong(ex.getCaption(), 50), false, ex.getColor(),
					ex);
			checkableItems.add(item);
			ind_ex.put(new Integer(checkableItems.indexOf(item)), ex);
		}

		list = new JList(checkableItems);
		list.setCellRenderer(new CheckListRenderer());
		list.setVisibleRowCount(4);
		list.setOpaque(true);
		list.setBackground(Color.WHITE);

		list.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		list.addMouseListener(new MouseAdapter() {
			public void mouseClicked(MouseEvent e) {
				if (e.isControlDown()) {
					
					// fc-4.1.2017 REMOVED: color is managed better now with StepPopup / ChangeStepColor
					
//					int index = list.locationToIndex(e.getPoint());
//					CheckableItem item = (CheckableItem) list.getModel().getElementAt(index);
//					AbstractDataExtractor ex = item.getExtractor();
//					Color initialColor = ex.getColor();
//
//					Color resultColor = JColorChooser.showDialog(DRConfigurationPanel.this,
//							Translator.swap("DataRenderer.chooseAColor"), initialColor);
//					if (resultColor != null) {
//						ex.setColor(resultColor);
//						item.setColor(resultColor);
//					}
//
//					Rectangle rect = list.getCellBounds(index, index);
//					list.repaint(rect);
					
					// fc-4.1.2017 REMOVED: color is managed better now with StepPopup / ChangeStepColor

				} else {
					int index = list.locationToIndex(e.getPoint());
					CheckableItem item = (CheckableItem) list.getModel().getElementAt(index);
					item.setSelected(!item.isSelected());
					Rectangle rect = list.getCellBounds(index, index);
					list.repaint(rect);
				}
			}
		});
		JScrollPane scrollPane = new JScrollPane(list);
		// ~ scrollPane.setPreferredSize (new Dimension (150, 100));

		JLabel label = new JLabel(Translator.swap("DataRenderer.extractors") + " :");
		LinePanel aux = new LinePanel();
		aux.add(label);
		aux.addGlue();

		JLabel label2 = new JLabel(Translator.swap("DataRenderer.checkToDelete"));
		LinePanel aux2 = new LinePanel();
		aux2.add(label2);
		aux2.addGlue();

		c1.add(aux);
		c1.add(scrollPane);
		c1.add(aux2);

		mainContent.add(c1);
		// NOTE: subclasses may add other lines in mainContent fc-25.11.2014


		MemoPanel userMemo = new MemoPanel (Translator.swap("DRConfigurationPanel.rendererConfigurationExplanation"));
		add (userMemo, BorderLayout.SOUTH);

		
	}

	public boolean checksAreOk() {
		return true;
	}

	public Collection getCheckableItems() {
		return checkableItems;
	}

}
