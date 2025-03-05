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

package capsis.kernel;

/**
 * AbstractPilot is a superclass for all Capsis pilots. A pilot deals with a
 * context of use of Capsis. The 'gui' pilot opens a main window with menus and
 * a graphical project manager, it can be used to run simulations interactively.
 * The 'script' pilot opens no windows and executes the script given in
 * parameter, most of the tasks that can be run with 'gui' can also be run with
 * 'script'. Other pilots can be written to run simulations in other ways. The
 * pilot is constructed by the Engine from its name: 'gui' ->
 * applicationPackageName.gui.Pilot (convention). The capsis.script.Pilot can be
 * used in other applications to run scripts.
 * 
 * @author F. de Coligny - September 2001, September 2010
 */
abstract public class AbstractPilot {

	/**
	 * Starts the pilot. The pilots generally have a constructor and then the start
	 * () method must be called.
	 */
	abstract public void start() throws Exception;

	/**
	 * Each pilot returns its default relay on demand. This relay will be used to
	 * load modules without a specific Relay class. e.g. for capsis.gui ->
	 * capsis.commongui.DefaultRelay
	 */
	abstract public Relay getDefaultRelay(GModel model);

}
