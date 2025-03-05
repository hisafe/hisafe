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

package capsis.kernel.extensiontype;

import capsis.kernel.GModel;
import capsis.kernel.Step;
import jeeb.lib.defaulttype.Extension;

/**
 * Interface for output formats, i.e. tools writing data to files.
 *
 * @author F. de Coligny - March 2001
 */
public interface OFormat extends Extension {

	/**
	 * Inits the output format: the data to be exported is in the GScene linked to
	 * the given Step. The model class is given in case other information would be
	 * needed in it or in the linked InitialParameters object. After initExport (),
	 * call save (fileName) to write the file.
	 */
	public void initExport(GModel model, Step step) throws Exception;

	/**
	 * Save a export format into an export file.
	 */
	public void save(String fileName) throws Exception;

}
