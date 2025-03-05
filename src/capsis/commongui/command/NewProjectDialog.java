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
package capsis.commongui.command;

import capsis.kernel.GModel;
import capsis.kernel.InitialParameters;

/**
 * An interface for the dialogs managed by the NewProject command
 * 
 * @author F. de Coligny - May 2010
 */
public interface NewProjectDialog {

	public boolean isValidDialog();

	public void dispose();

	public String getProjectName();

	public GModel getModel();

	public InitialParameters getInitialParameters();

	public void finalize() throws Throwable;

}
