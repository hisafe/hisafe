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
 * dataExtractors). Used to configure several objects with the same
 * configuration panel (generally several instances of the same extractor,
 * synchronised on several simulation steps). Retrieve the configurationPanel
 * from the first object, then add other configurable objects to the
 * configurationPanel. ConfigurationPanel.execute () reconfigures everybody.
 * E.g. of common configuration: 'Per hectare': must be applied to all the
 * curves in a graph.
 *
 * @author F. de Coligny - September 2000
 */
public interface SharedConfigurable {

	// fc-25.7.2021 Reviewed comments, renamed methods

	/**
	 * Returns a label for this shared configuration, e.g. a 'Common configuration'
	 * (must be the result of a call to the Translator, in french or english).
	 */
	public String getSharedConfigLabel();

	/**
	 * Returns a panel with the shared configuration options to be proposed to user,
	 * see the ConfigurationPanel class.
	 */
	public ConfigurationPanel getSharedConfigPanel(Object param);

	/**
	 * Apply the configuration options found in the panel.
	 */
	public void applySharedConfig(ConfigurationPanel p);

	/**
	 * Called after configuration (can be used to save current configuration...)
	 */
	public void postConfig();

}
