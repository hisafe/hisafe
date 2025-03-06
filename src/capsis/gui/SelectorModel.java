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
package capsis.gui;

import java.lang.reflect.Modifier;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.TreeMap;

import jeeb.lib.util.extensionmanager.ExtensionManager;
import capsis.app.CapsisExtensionManager;
import capsis.kernel.GModel;

/**
 * Selector knows which extensions are compatible with a given project. Ex:
 * stand viewers and extractors. A linked user interface opens tools when user
 * clicks on their name.
 * 
 * @author F. de Coligny - march 2003 - reviewed on 9.1.2004
 */
public class SelectorModel {

	private Collection<String> consideredExtensionTypes;

	// Memory is multi scale map :
	// ModuleName -> ExtensionType -> Name -> ClassName
	private Map<String, Map> memory;
	private ExtensionManager extMan;

	/**
	 * Constructor.
	 */
	public SelectorModel() {
		memory = Collections.synchronizedMap(new HashMap<String, Map>()); // fc
																			// -
																			// 9.1.2004

		extMan = CapsisExtensionManager.getInstance();

		consideredExtensionTypes = new HashSet<String>();
		consideredExtensionTypes.add(CapsisExtensionManager.STAND_VIEWER);
		consideredExtensionTypes.add(CapsisExtensionManager.DATA_EXTRACTOR);
		consideredExtensionTypes.add(CapsisExtensionManager.EXTRACTOR_GROUP); // fc-23.9.2013
	}

	/**
	 * Return considered extension types. Ex: Extension.STAND_VIEWER,
	 * Extension.DATA_EXTRACTOR...
	 */
	public Collection<String> getConsideredExtensionTypes() {
		return consideredExtensionTypes;
	}

	/**
	 * Return Map of tools <Name, ClassName> of the given type for the given
	 * model.
	 */
	public Map<String, String> getTools(GModel model, String extensionType) {

		String moduleName = model.getIdCard().getModelPackageName();

		// Ensure scenario is known
		if (!memory.keySet().contains(moduleName)) {
			add(model);
		}

		// moduleTools : key = extensionType & values = Collection of tool names
		Map<String, Map> moduleTools = memory.get(moduleName);
		
		return (Map<String, String>) moduleTools.get(extensionType); 
		// Map: name -> className
		
	}

	/** 
	 * Build tool map in memory for the given model if not yet built. 
	 */
	private void add(GModel model) {

		// Memory is multi scale map :
		// ModuleName -> ExtensionType -> Name -> ClassName
		String moduleName = model.getIdCard().getModelPackageName();
		if (memory.containsKey(moduleName)) {
			return;
		} // no more update required for this module (known)

		Map<String, Map> moduleTools = new HashMap<String, Map>();
		for (String extensionType : consideredExtensionTypes) {

			Collection<String> extensionClassNames = extMan.getExtensionClassNames(extensionType, model);

			Map<String, String> extensions = new TreeMap<String, String>();

			for (String className : extensionClassNames) {

				String name = ExtensionManager.getName(className);
				extensions.put(name, className); // name is translated (french /
													// english...)
			}
			moduleTools.put(extensionType, extensions);
		}
		memory.put(moduleName, moduleTools);

		return;
	}

	/**
	 * Clear memory for the given module (next getTools() will add it again :
	 * update).
	 */
	public void remove(String modelPackageName) {
		memory.remove(modelPackageName);
	}

	/**
	 * Clear all memory (next getTools() will update it correctly).
	 */
	public void clearMemory() {
		memory.clear();
	}

}
