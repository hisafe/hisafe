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
package capsis.gui.command;

import java.awt.Window;

import jeeb.lib.util.Command;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.projectmanager.Current;
import capsis.extension.generictool.assistant.AutomationEditor;
import capsis.kernel.Project;
import capsis.kernel.automation.Automation;

/**
 * @author sdufour
 * 
 */
public class EditAutomation implements Command {

	private boolean showOnly = false;
	private Window window;

	/**
	 * Constructor
	 */
	public EditAutomation (boolean showOnly, Window window) {

		this.showOnly = showOnly;
		this.window = window;

	}

	@Override
	public int execute () throws Exception {

		if (showOnly) {
			Project s = Current.getInstance ().getProject ();
			if (s == null) { return 0; }
			
			Automation exp = s.getAutomation ();

			if (exp == null || !exp.isValid ()) { // fc-28.11.2012 added exp == null, automation is now optional, see Project and GModel
				StatusDispatcher.print (Translator.swap ("Shared.commandFailed"));
				MessageDialog.print (this, Translator.swap ("Automation.ProjectNotAutomatable"));
				return 1;
			}

			new AutomationEditor (window, exp);
		} else {

			new AutomationEditor (window);
		}
		return 0;
	}

}
