/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  Francois de Coligny, Samuel Dufour
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied 
 * warranty of MERCHANTABILITY or FITNESS FOR A 
 * PARTICULAR PURPOSE. See the GNU Lesser General Public 
 * License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.commongui.projectmanager;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

import jeeb.lib.util.Translator;

/**
 * ExpandPopup is popup menu which can be opend on a StepButton in a
 * ProjectManager to tune finely the visibility between 2 step buttons.
 * 
 * @author F. de Coligny - December 2009
 */
public class ExpandPopup extends JPopupMenu implements ActionListener {

	private ProjectManager projectManager;

	private JMenuItem expand;
	private JMenuItem collapse;

	private StepButton sb0;
	private StepButton sb1;

	/**
	 * Constructor
	 */
	public ExpandPopup(ProjectManager projectManager, String range, StepButton sb0, StepButton sb1) {
		super();

		this.projectManager = projectManager;
		this.sb0 = sb0;
		this.sb1 = sb1;

		expand = new JMenuItem(Translator.swap("ExpandPopup.expand") + range);
		expand.addActionListener(this);
		add(expand);

		collapse = new JMenuItem(Translator.swap("ExpandPopup.collapse") + range);
		collapse.addActionListener(this);
		add(collapse);

	}

	/**
	 * A JMenuItem was selected
	 */
	@Override
	public void actionPerformed(ActionEvent e) {

		Object source = e.getSource();
		if (source != null && source == expand) {
			projectManager.updateVisibilityRange(sb0, sb1, true);

		} else if (source != null && source == collapse) {
			projectManager.updateVisibilityRange(sb0, sb1, false);
		}

	}

}
