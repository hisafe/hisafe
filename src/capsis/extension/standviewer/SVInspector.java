/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2001-2017  Francois de Coligny
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

package capsis.extension.standviewer;

import java.awt.GridLayout;

import javax.swing.JComponent;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.StepButton;
import capsis.commongui.util.Tools;
import capsis.extension.AbstractStandViewer;
import capsis.kernel.GModel;
import capsis.kernel.Step;

/**
 * SVInspector is a viewer for scene dynamic introspection: shows the variables
 * and their values in a table.
 * 
 * @author F. de Coligny - June 2001
 */
public class SVInspector extends AbstractStandViewer {
	// fc-11.1.2017 code review
	
	static {
		Translator.addBundle("capsis.extension.standviewer.SVInspector");
	}

	// nb-07.08.2018
	//static public String NAME = Translator.swap("SVInspector");
	//static public String DESCRIPTION = Translator.swap("SVInspector.description");
	//static public String AUTHOR = "F. de Coligny";
	//static public String VERSION = "1.0";

	/**
	 * Opens and inits the tool on the given stepButton
	 */
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		super.init(model, s, but);
		setLayout(new GridLayout(1, 1)); // important for viewer appearance

		update(stepButton);
	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			return referent instanceof GModel;
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "SVInspector.matchWith ()",
					"Error in matchWith () (returned false)", e);
			return false;
		}

	}

	@Override
	public String getName() {
		return Translator.swap("SVInspector.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SVInspector.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**
	 * Synchronizes the tool on the given stepButton
	 */
	public void update(StepButton sb) {
		super.update(sb);

		JComponent comp = Tools.getIntrospectionPanel(stepButton.getStep()
				.getScene());

		removeAll();
		add(comp);

		revalidate();
		repaint();

	}

}
