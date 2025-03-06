/* 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2015 LERFoB AgroParisTech/INRA 
 * 
 * Authors: M. Fortin, 
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
package capsis.util.extendeddefaulttype;

import java.awt.Window;
import java.util.ArrayList;
import java.util.List;

import capsis.commongui.util.Helper;
import jeeb.lib.util.Settings;
import repicea.gui.AutomatedHelper;
import repicea.gui.UIControlManager;
import repicea.io.tools.REpiceaExportTool;
import repicea.io.tools.REpiceaExportToolDialog;

@SuppressWarnings({ "rawtypes", "serial" })
public class ExtExportToolDialog extends REpiceaExportToolDialog {

	public ExtExportToolDialog(REpiceaExportTool caller, Window owner) {
		super(caller, owner);
	}

	@Override
	protected void init() {
		setAutomatedHelper("artemis.gui.ArtExportDialog");
		String modelName = getCaller().getModel().getIdCard().getModelName();
		getProperties(getClass().getSimpleName() + "." + modelName);
		super.init();
	}
	
	
	/**
	 * This method sets the web page reference for the help button.
	 * @param webPageReference a String
	 */
	protected void setAutomatedHelper(String webPageReference) {
		Object[] arguments = new Object[1];
		arguments[0] = webPageReference;
		AutomatedHelper helper;
		try {
			helper = new AutomatedHelper(Helper.class.getMethod("helpFor", String.class), arguments);
			UIControlManager.setHelpMethod(this.getClass(), helper);
		} catch (Exception e) {
			System.out.println("Error while setting the helper for the import dialog!");
		}
	}

	protected void getProperties(String prefix) {
		String filename = Settings.getProperty(prefix.trim() + ".filename", "");
		
		if (!filename.isEmpty()) {
			getCaller().setFilename(filename);
		}
		
		String enumName = Settings.getProperty(prefix.trim() + ".SelectedOption", "");
		if (!enumName.isEmpty()) {
			for (Enum enumVar : getCaller().getAvailableExportOptions()) {
				if (enumVar.toString().trim().toLowerCase().equals(enumName.trim().toLowerCase())) {
					try {
						List<Enum> selectedOptions = new ArrayList<Enum>();
						selectedOptions.add(enumVar);
						getCaller().setSelectedOptions(selectedOptions);
					} catch (Exception e) {}
					break;
				}
			}
		}
	}

	@Override
	protected boolean checkFileValidity() throws Exception {
		boolean valid = super.checkFileValidity();
		if (!valid) {
			return false;
		}
		String modelName = getCaller().getModel().getIdCard().getModelName();
		setProperties(getClass().getSimpleName() + "." + modelName);
		return true;
	}

	protected void setProperties(String prefix) {
		Settings.setProperty(prefix.trim() + ".filename", getCaller().getFilename());
		if (getCaller().getSelectedExportFormats().size() == 1) {
			Enum selectedFormat = getCaller().getSelectedExportFormats().iterator().next();
			Settings.setProperty(prefix.trim() + ".SelectedOption", selectedFormat.toString());
		}
	}
	
	
	@Override
	protected ExtExportTool getCaller() {
		return (ExtExportTool) super.getCaller();
	}
	
}

