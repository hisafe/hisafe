/**
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA
 * 
 * Authors: F. de Coligny
 * 
 * This file is part of Capsis Capsis is free software: you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by the Free Software
 * Foundation, either version 2.1 of the License, or (at your option) any later version.
 * 
 * Capsis is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even
 * the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU lesser General Public License along with Capsis. If
 * not, see <http://www.gnu.org/licenses/>.
 * 
 */
package capsis.commongui;

import java.awt.Container;

import capsis.kernel.InitialParameters;

/**
 * An interface for the initial dialogs in Capsis.
 * @author F. de Coligny - December 2011
 */
public interface InitialDialogInterface {
	
	public InitialParameters getInitialParameters();
	
	public void setInitialParameters(InitialParameters ip);
	
	public Container getContentPane ();
	
	public void setValidDialog (boolean v);

}
