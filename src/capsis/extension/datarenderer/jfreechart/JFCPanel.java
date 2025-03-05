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

package capsis.extension.datarenderer.jfreechart;

import java.awt.event.MouseEvent;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.MenuElement;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;

import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.Current;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.extension.datarenderer.DataRendererPopup;
import capsis.extensiontype.DataBlock;
import capsis.kernel.Step;

/**
 * author : SDK
 * improved Jfreechart panel for capsis
 */
public class JFCPanel extends ChartPanel {
	
	protected DataBlock db;

	
	public JFCPanel(JFreeChart chart, DataBlock d) {
		super(chart, true, true, false, true, true );
		setMouseZoomable(true);
		setMouseWheelEnabled(true);
		setDomainZoomable(true);
		setRangeZoomable(true);
		
		db = d;
		
		// complete popupmenu
		JPopupMenu m = getPopupMenu();
		if(db != null) {
			JPopupMenu m2 = new DataRendererPopup(db, false);
			m2.addSeparator();
			for(MenuElement me : m.getSubElements()) { 
				JMenuItem mi = (JMenuItem) me;
				m2.add(mi);
			}
			setPopupMenu(m2);
		}
		
	}
	
	@Override
	public void mouseClicked(MouseEvent event) {
		
		// Ctrl-click : add an extractor to the data block for the current step
		if ((event.getModifiers () & Tools.getCtrlMask ()) != 0) {	
			Step s = Current.getInstance ().getStep ();
			StepButton sb = ProjectManager.getInstance ().getStepButton (s);
			
			ButtonColorer.getInstance ().newColor (sb);

			
			db.addExtractor (sb.getStep ());
			
			event.consume ();	// fc - 12.5.2003
			return;
		}
		
		
		super.mouseClicked(event);
	}
	
	

	
}
