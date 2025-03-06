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

import java.awt.Component;
import java.awt.Container;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import javax.swing.JPanel;

import capsis.extension.dataextractor.superclass.AbstractDataExtractor;
import capsis.extensiontype.DataBlock;
import jeeb.lib.util.Log;

/**
 * ConfigurationPanel contains configuration information for one Configurable
 * object or several SharedConfigurable objects. The object must implement the
 * Configurable interface. If several, they must implement the
 * SharedConfigurable interface. The object (or one of them) gives a
 * configuration panel (JPanel). panel.execute () reconfigures the object(s) and
 * all its (their) superclasses (if needed).
 * 
 * @author F. de Coligny - September 2000
 */
abstract public class ConfigurationPanel extends JPanel {

	// fc-25.7.2021 Reviewed comments, renamed methods

	protected Set configurables;

	private boolean isSharedConfPanel;

	/**
	 * Constructor
	 */
	public ConfigurationPanel() {
		super();
		configurables = new HashSet(); // no duplicates
		isSharedConfPanel = false;

		// in subclasses : set layout and add configuration components
	}

	/**
	 * Constructor 2
	 */
	public ConfigurationPanel(Configurable obj) {
		this();
		configurables.add(obj);
	}

	/**
	 * Constructor 3
	 */
	public ConfigurationPanel(SharedConfigurable obj) {
		this();
		configurables.add(obj);
		isSharedConfPanel = true;
	}

	/**
	 * Return true if ConfigurationPanel is empty.
	 */
	public boolean isEmpty() {
		return getComponents().length == 0;
	}

	/**
	 * This method is called by containing dialog before calling execute (). If
	 * trouble, the method is responsible for user information (e.g. "Integer
	 * needed..."), then it is supposed to return false.
	 */
	abstract public boolean checksAreOk();

	/**
	 * Command to trigger the configuration, once panel modified. See checksAreOk
	 * ().
	 */
	public void execute() {
		if (configurables.isEmpty()) {

			Log.println(Log.ERROR, "ConfigurationPanel.execute ()", "No configurable found for panel: " + this);

		} else if (!isSharedConfPanel()) { // one single Configurable

			getConfigurable().applyConfig(this);
			getConfigurable().postConfig();

		} else { // several SharedConfigurable with the same panel

			for (Iterator i = configurables.iterator(); i.hasNext();) {
				SharedConfigurable c = (SharedConfigurable) i.next();

				c.applySharedConfig(this);
				c.postConfig();
			}

		}
	}

	/**
	 * This method is called by containing dialog after calling execute (). If
	 * trouble, the method is responsible for user information (e.g. "classWidth must be
	 * greater than 1"), then it is supposed to return false.
	 */
	public boolean additionalChecks() {
		// fc-8.9.2021 Can be used to check properties values in expected ranges
		
		return true; // Default: all is correct
	}

	/**
	 * Return true if the panel concerns several configurables (i.e.
	 * shared-configuration).
	 */
	public boolean isSharedConfPanel() {
		return isSharedConfPanel;
	}

	/**
	 * Return the configurable if single mode.
	 */
	public Configurable getConfigurable() {
		return (Configurable) configurables.iterator().next(); // first element
	}

	/**
	 * Return the shared configurable if shared mode.
	 */
	public SharedConfigurable getSharedConfigurable() {
		return (SharedConfigurable) configurables.iterator().next(); // first
																		// element
	}

	/**
	 * Add a SharedConfigurable to the list. They will all be reconfigured with the
	 * panel.
	 */
	public void addSharedConfigurable(SharedConfigurable c) {
		configurables.add(c);
	}

	/**
	 * Return the configurables Set. Contains one Configurable in single mode or
	 * several SharedConfigurable in shared mode.
	 */
	public Set getConfigurables() {
		return configurables;
	}

	/**
	 * To add things in a ConfigurationPanel, use getContentPane ().add (...).
	 */
	public JPanel getContentPane() {
		return this;
	}

	/**
	 * Consider recursively all the panel's sub component and set them enabled
	 * (false).
	 */
	public void disablePanel() {
		disablePanel(this);
	}

	//
	// Recursive, triggered by disablePanel ().
	//
	private void disablePanel(Component c) {
		if (c instanceof Container) {
			Component[] components = ((Container) c).getComponents();
			for (int i = 0; i < components.length; i++) {
				disablePanel(components[i]);
			}
		}
		c.setEnabled(false);
	}

	public String toString() {
		StringBuffer b = new StringBuffer();
		b.append("ConfigurationPanel for : ");
		b.append(configurables.toString());
		return b.toString();
	}

}
