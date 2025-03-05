/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2010-2012 Mathieu Fortin
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */
package capsis.extension.modeltool.woodqualityworkshop;

import java.util.Collection;
import java.util.Vector;

import jeeb.lib.util.extensionmanager.ExtensionManager;
import repicea.simulation.treelogger.TreeLogger;
import repicea.simulation.treelogger.TreeLoggerDescription;
import repicea.simulation.treelogger.TreeLoggerParameters;
import capsis.app.CapsisExtensionManager;
import capsis.kernel.GModel;


/**
 * The CapsisTreeLoggerDescription class derives from TreeLoggerDescription. The instantiation of 
 * the TreeLogger instance relies on the ExtensionManager instead of the standard newInstance() method.
 * @author Mathieu Fortin - November 2012
 */
public class CapsisTreeLoggerDescription extends TreeLoggerDescription {
	
	
	/**
	 * Constructor.
	 * @param address a String that represents the address of the tree logger.
	 */
	public CapsisTreeLoggerDescription(String address) {
		super(address);
	}
	

	@Override
	public TreeLogger instantiateTreeLogger(boolean scriptMode) {
		TreeLogger treeLogger = (TreeLogger) CapsisExtensionManager.getInstance().instantiate(getClass().getName());
		if (scriptMode) {
			TreeLoggerParameters params = treeLogger.createDefaultTreeLoggerParameters();
			treeLogger.setTreeLoggerParameters(params);
		}
		return treeLogger;
	}
	
	
	/**
	 * This static method returns a vector of TreeLoggerDescription instances.
	 * @param model a GModel object
	 * @return a Vector of TreeLoggerDescription instances
	 */ 
	public static Vector<TreeLoggerDescription> getMatchingTreeLoggers(GModel model) {
		ExtensionManager extMan = CapsisExtensionManager.getInstance();
		Collection<String> compatibleLoggerAddresses = extMan.getExtensionClassNames(CapsisExtensionManager.TREELOGGER, model);
		Vector<TreeLoggerDescription> treeLoggerDescriptions = new Vector<TreeLoggerDescription>();
		
		for (String loggerAddress : compatibleLoggerAddresses) {
			treeLoggerDescriptions.add(new TreeLoggerDescription(loggerAddress));
		}
			
		return treeLoggerDescriptions;
	}

	
}
