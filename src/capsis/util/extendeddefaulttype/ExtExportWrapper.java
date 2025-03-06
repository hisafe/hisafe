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

import jeeb.lib.util.CancellationException;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.OFormat;


public abstract class ExtExportWrapper implements OFormat {


	/**
	 * Generic constructor for this class.
	 */
	protected ExtExportWrapper() {}

	
	/**
	 * This method initialize the export tool and show the dialog. To be called in GUI mode. 
	 */
	public void initExport(GModel model, Step s) throws Exception {
		GScene stand = s.getScene();
		if (stand != null) {
			ExtExportTool exportTool = createExportTool((ExtModel) model, s);
			exportTool.showUI(MainFrame.getInstance());		// okAction method is redefined in the interface to make sure the save is not done yet
			throw new CancellationException();
		} else {
			throw new Exception ("ExtendedExportTool: Exporting empty stand.");
		}
	}

	/**
	 * This method create an ExportTool. It is to be overriden in derived classes.
	 * @param model a ExtendedModel instance
	 * @param s a Step instance
	 * @return an ExtendedExportTool instance
	 */
	protected abstract ExtExportTool createExportTool(ExtModel model, Step s) throws Exception;
	
	/**
	 * Import: RecordSet -> Stand - Implementation here.
	 * (File -> RecordSet in superclass).
	 */
	public GScene load (GModel model) throws Exception {return null;}

	// fc-7.12.2020 Deprecated
//	public boolean isImport () {return false;}
//	public boolean isExport () {return true;}

	// fc-7.12.2020 Deprecated
//	public void activate() {}

	@Override
	public void save(String fileName) throws Exception {}
	
}

