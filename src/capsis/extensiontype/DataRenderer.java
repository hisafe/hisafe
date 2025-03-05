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
package capsis.extensiontype;

import java.awt.Component;

import jeeb.lib.defaulttype.Extension;
import jeeb.lib.util.Disposable;

/**
 * The super interface for all data renderers.
 * 
 * @author F. de Coligny - December 1999
 */
public interface DataRenderer extends Extension, Disposable {

	public void init(DataBlock db);

	public void update();

	public void setUpdating();

	public void setUpdated();

	public void close();

	@Override
	public void dispose();

	public Component getPanel();

	public String getTitle();

	public DataBlock getDataBlock();

	// Something failed, write a message on the renderer
	// fc-20.1.2014
	public void security();

}
