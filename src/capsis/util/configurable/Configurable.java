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

package capsis.util.configurable;

/**
 * Interface implemented by graphical configurable objects (e.g.
 * dataExtractors). They can propose a ConfigurationPanel to user to get
 * configuration instructions. ConfigurationPanel.execute () reconfigures them.
 *
 * @author F. de Coligny - November 1999
 */
public interface Configurable {

	// fc-25.7.2021 Reviewed comments, renamed methods

	/**
	 * Returns a label for this specific configurable object, i.e. a different label
	 * for each considered configurable object. (must be the result of a call to the
	 * Translator, in french or english).
	 */
	public String getConfigLabel();

	/**
	 * Returns a panel with the configuration options to be proposed to user for
	 * this particular configurable, see the ConfigurationPanel class.
	 */
	public ConfigurationPanel getConfigPanel(Object param);

	/**
	 * Applies the configuration options found in the panel.
	 */
	public void applyConfig(ConfigurationPanel panel);

	/**
	 * Called after configuration (can be used to save current configuration...)
	 */
	public void postConfig();

}
