/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2003  Francois de Coligny
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

package capsis.extension.generictool;

import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import javax.swing.ButtonGroup;
import javax.swing.JMenu;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JRadioButtonMenuItem;
import javax.swing.JTable;

import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.CompatibilityManager;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * GraphicalExtensionManagerTablePopup is a JPopupMenu for the
 * GraphicalExtensionManager tool.
 * 
 * @author F. de Coligny - January 2004
 */
public class GraphicalExtensionManagerTablePopup extends JPopupMenu {

	static public enum MenuCommand {
		ADD_VETO_OPTION, REMOVE_VETO_OPTION, ADD_IN_LIST_OPTION, REMOVE_FROM_LIST_OPTION, MANAGE_COMPATIBILITY, EXTENSION_NATIVE, EXTENSION_VETO, EXTENSION_LIST, SELECTED_EXTENSIONS_WARNINGS, ALL_EXTENSIONS_WARNINGS
	};

	/**
	 * Constructor
	 */
	public GraphicalExtensionManagerTablePopup(JTable table, ActionListener listener, Collection<MenuCommand> options,
			String modelName, String referentName, ExtensionManager extensionManager) {

		// fc-23.10.2007 added modelName and modulePackageName
		// fc-7.8.2018 added extensionManager to remove dependency to Capsis

		super();

		if (options == null) {
			options = new ArrayList();
		} // security -> empty popup

		for (Iterator i = options.iterator(); i.hasNext();) {
			MenuCommand option = (MenuCommand) i.next();
			JMenuItem menuItem = null;

			if (option == MenuCommand.SELECTED_EXTENSIONS_WARNINGS) {
				menuItem = new JMenuItem(Translator.swap("GraphicalExtensionManager.selectedExtensionsWarnings"),
						MenuCommand.SELECTED_EXTENSIONS_WARNINGS.ordinal());

			} else if (option == MenuCommand.ALL_EXTENSIONS_WARNINGS) {
				menuItem = new JMenuItem(Translator.swap("GraphicalExtensionManager.allExtensionsWarnings"),
						MenuCommand.ALL_EXTENSIONS_WARNINGS.ordinal());

			} else if (option == MenuCommand.ADD_VETO_OPTION) {
				menuItem = new JMenuItem(Translator.swap("GraphicalExtensionManager.addVeto"),
						MenuCommand.ADD_VETO_OPTION.ordinal());

			} else if (option == MenuCommand.REMOVE_VETO_OPTION) {
				menuItem = new JMenuItem(Translator.swap("GraphicalExtensionManager.removeVeto"),
						MenuCommand.REMOVE_VETO_OPTION.ordinal());

			} else if (option == (MenuCommand.ADD_IN_LIST_OPTION)) {
				menuItem = new JMenuItem(Translator.swap("GraphicalExtensionManager.addInList"),
						MenuCommand.ADD_IN_LIST_OPTION.ordinal());

			} else if (option == (MenuCommand.REMOVE_FROM_LIST_OPTION)) {
				menuItem = new JMenuItem(Translator.swap("GraphicalExtensionManager.removeFromList"),
						MenuCommand.REMOVE_FROM_LIST_OPTION.ordinal());

			} else if (option == (MenuCommand.MANAGE_COMPATIBILITY)) {
				menuItem = new JMenu( // JMenu extends JMenuItem
						Translator.swap("GraphicalExtensionManager.manageCompatibilityFor") + " " + modelName);

				// sub menu
				// 1. extension native
				JRadioButtonMenuItem extensionNative = new JRadioButtonMenuItem(
						Translator.swap("GraphicalExtensionManager.extensionNative"));
				extensionNative.setMnemonic(MenuCommand.EXTENSION_NATIVE.ordinal());
				extensionNative.setActionCommand(referentName); // the listener
																// will know
																// modulePackageName
				extensionNative.addActionListener(listener);
				menuItem.add(extensionNative);

				// 2. extension veto
				JRadioButtonMenuItem extensionVeto = new JRadioButtonMenuItem(
						Translator.swap("GraphicalExtensionManager.use") + " " + modelName + File.separator
								+ "extension.veto");
				extensionVeto.setMnemonic(MenuCommand.EXTENSION_VETO.ordinal());
				extensionVeto.setActionCommand(referentName); // the listener
																// will know
																// modulePackageName
				extensionVeto.addActionListener(listener);
				menuItem.add(extensionVeto);

				// 3. extension list
				JRadioButtonMenuItem extensionList = new JRadioButtonMenuItem(
						Translator.swap("GraphicalExtensionManager.use") + " " + modelName + File.separator
								+ "extension.list");
				extensionList.setMnemonic(MenuCommand.EXTENSION_LIST.ordinal());
				extensionList.setActionCommand(referentName); // the listener
																// will know
																// modulePackageName
				extensionList.addActionListener(listener);
				menuItem.add(extensionList);

				ButtonGroup group1 = new ButtonGroup();
				group1.add(extensionVeto);
				group1.add(extensionList);
				group1.add(extensionNative);

				CompatibilityManager man = extensionManager.getCompatibilityManager();
				boolean list = man.usesExtensionList(referentName);
				boolean veto = man.usesExtensionVeto(referentName);
				// fc - 25.8.2008
				if (list) {
					extensionList.setSelected(true);
				} else if (veto) {
					extensionVeto.setSelected(true);
				} else {
					extensionNative.setSelected(true);
				}

			}
			menuItem.addActionListener(listener);
			add(menuItem);
		}
	}

}
