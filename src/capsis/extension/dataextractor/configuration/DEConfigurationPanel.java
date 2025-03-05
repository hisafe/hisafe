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

package capsis.extension.dataextractor.configuration;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;
import java.util.TreeSet;
import java.util.Vector;

import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JTextField;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.extension.dataextractor.superclass.AbstractDataExtractor;
import capsis.extensiontype.DataBlock;
import capsis.gui.DListSelector;
import capsis.gui.GrouperChooser;
import capsis.gui.StatusChooser;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.group.GroupableType;
import capsis.util.group.GrouperManager;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Translator;

/**
 * Configuration panel for DataExtractors, single extractor configuration mode.
 * 
 * @see DEMultiConfPanel
 * 
 * @author F. de Coligny - march 2003
 */
public class DEConfigurationPanel extends ConfigurationPanel implements ActionListener {

	// The data block of the extractor given at
	// construction time
	private DataBlock dataBlock;

	private AbstractDataExtractor extractor;

	private JTextField treeIds;
	private JButton chooseTreeIds;

	public GrouperChooser grouperChooser;

	// Status
	public StatusChooser statusChooser;

	/**
	 * Constructor, build user interface.
	 */
	public DEConfigurationPanel(Configurable c) {
		super(c);

		extractor = (AbstractDataExtractor) c;

		dataBlock = extractor.getDataBlock(); // fc-4.9.2019

		if (dataBlock == null)
			System.out.println("[DEConfigurationPanel.c()] *** dataBlock is null, extractor: "
					+ extractor.getClass().getName() + "@" + extractor.hashCode());

		ColumnPanel master = new ColumnPanel();

		// Tree ids selection
		//
		// 1.1 for TREE_IDS property
		// check if model manages individual trees
		// disable TREE_IDS property if not
		// [C. Meredieu, T. Labbé] lemoine model is stand level, no trees
		// fc-12.10.2006
		//
		if (extractor.hasConfigProperty(AbstractDataExtractor.TREE_IDS)
				&& extractor.isUserActionEnabled(AbstractDataExtractor.TREE_IDS)) {
			// fc-10.1.2019 added isUserActionEnabled ()

			Step step = extractor.getStep();
			GScene stand = step.getScene();
			boolean modelWithTrees = stand instanceof TreeCollection;
			extractor.setPropertyEnabled(AbstractDataExtractor.TREE_IDS, modelWithTrees);
		}

		// 1.2 create a treeIds chooser
		//
		if (extractor.hasConfigProperty(AbstractDataExtractor.TREE_IDS)
				&& extractor.isUserActionEnabled(AbstractDataExtractor.TREE_IDS)) {
			// fc-10.1.2019 added isUserActionEnabled ()

			LinePanel l0 = new LinePanel();
			l0.add(new JLabel(Translator.swap("DEConfigurationPanel.concernedIndividuals") + " :"));
			String ids = "";
			for (Iterator i = extractor.getTreeIds().iterator(); i.hasNext();) {
				String id = (String) i.next();
				ids += id;
				if (i.hasNext()) {
					ids += ", ";
				}
			}
			treeIds = new JTextField(ids, 2);
			treeIds.addActionListener(this); // fc-4.9.2019
			l0.add(treeIds);

			chooseTreeIds = new JButton(Translator.swap("DEConfigurationPanel.choose"));
			chooseTreeIds.addActionListener(this);

			if (!extractor.isPropertyEnabled(AbstractDataExtractor.TREE_IDS)) { // fc
																				// -
				// 6.2.2004
				treeIds.setEnabled(false);
				chooseTreeIds.setEnabled(false);
			}

			l0.add(chooseTreeIds);
			l0.addStrut0();

			master.add(l0);

		}

		// Individual tree / cell grouper : for this extractor only
		// fc-6.5.2003
		//
		if (extractor.getIGrouperType() != null
				&& extractor.isUserActionEnabled(extractor.getIGrouperType().getKey())) {
			// fc-10.1.2019 added isUserActionEnabled ()

			GroupableType type = extractor.getIGrouperType();

			boolean checked = !extractor.isCommonGrouper() && extractor.isGrouperMode();
			GrouperManager gm = GrouperManager.getInstance();
			String selectedGrouperName = gm.removeNot(extractor.getGrouperName());

			// fc-3.6.2020 Restricted to type
			grouperChooser = new GrouperChooser(extractor.getStep().getScene(), type, Arrays.asList(type),
					selectedGrouperName, extractor.isGrouperNot(), true, checked);

			LinePanel l8 = new LinePanel();
			l8.add(grouperChooser);
			l8.addStrut0();
			master.add(l8);
		}

		// Choose trees status if available (Ex: alive / cut / dead...)
		// fc-22.3.2004 / 22.4.2004
		//
		if (extractor.hasConfigProperty(AbstractDataExtractor.STATUS)
				&& extractor.isUserActionEnabled(AbstractDataExtractor.STATUS)) {
			// fc-10.1.2019 added isUserActionEnabled ()

			if (extractor.getStep().getScene() instanceof TreeList) {
				TreeList stand = (TreeList) extractor.getStep().getScene();
				statusChooser = new StatusChooser(stand.getStatusKeys(), extractor.getStatusSelection());
				master.add(statusChooser);
			}
		}

		// fc-3.11.2020 Specific properties at the individual level (for each extractor
		// in the dataBlock)
		layoutSpecificProperties(master, extractor);

		// General layout
		//
		if (master.getComponents().length != 0) {
			setLayout(new BorderLayout());
			add(master, BorderLayout.NORTH);
		}

	}

	/**
	 * Specific properties: add a button to open the property's configuration
	 * dialog. DEConfigurationPanel deals with the individual level properties
	 */
	private void layoutSpecificProperties(ColumnPanel master, AbstractDataExtractor ex) {

//		String hc = ex.getClass().getSimpleName()+"@"+ex.hashCode();
//		JLabel lab = new JLabel ("DEConfigurationPanel, debug, extractor: "+hc);
//		master.add(lab);
		
		// Common property -> first key is null (see AbstractDataExtractor)
		Set<String> propertyNames = new TreeSet<String>(ex.getSettings().specificProperties.keySet2(ex));
//		Set<String> propertyNames = new TreeSet<>(ex.getSettings().specificProperties.keySet());

		for (String propertyName : propertyNames) {

			if (!ex.isUserActionEnabled(propertyName))
				continue;

			DESpecificProperty property = ex.getSettings().specificProperties.get(ex, propertyName);

			boolean buttonEnabled = ex.isPropertyEnabled(propertyName);
			JPanel l1 = property.getConfigurationLine(this, propertyName, buttonEnabled);

			master.add(l1);

		}

	}

	public GrouperChooser getGrouperChooser() {
		return grouperChooser;
	}

	public StatusChooser getStatusChooser() {
		return statusChooser;
	}

	/**
	 * Actions management.
	 */
	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(chooseTreeIds)) {

			// Change treeIds by the chooseIds button
			chooseTreeIdsAction();

		}
	}

	/**
	 * Dialog to choose some tree ids.
	 */
	private void chooseTreeIdsAction() {
		Vector<String> ids = new Vector<String>();
		Collection<? extends Tree> v = ((TreeCollection) extractor.getStep().getScene()).getTrees();
		for (Tree t : v) {

			String id = "" + t.getId(); // String

			// fc - 19.9.2005 - if Numberable, show number in list
			if (t instanceof Numberable) {
				double number = ((Numberable) t).getNumber();
				// if (number <= 0) {continue;} // fc - correction for
				// C.Meredieu : wants these trees to be selectable here
				// (20.9.2005)
				id += " (number=" + number + ")";
			}

			ids.add(id);
		}

		// fc - 28.9.2006 - added singleSelection device
		boolean singleSelection = extractor.isSingleIndividual();

		DListSelector dlg = new DListSelector(Translator.swap("DEConfigurationPanel.individualsSelection"),
				Translator.swap("DEConfigurationPanel.individualsSelectionText"), ids, singleSelection);

		if (dlg.isValidDialog()) {
			String txt = "";
			for (Iterator i = dlg.getSelectedItems().iterator(); i.hasNext();) {
				String id = (String) i.next();

				// fc - 19.9.2005 - discard "(number=12)" in case of id of
				// Numberable
				int space = id.indexOf(" ");
				if (space != -1) {
					id = id.substring(0, space);
				}
				txt += id;

				if (i.hasNext()) {
					txt += ", ";
				}
			}
			treeIds.setText(txt);
		}
	}

	public boolean checksAreOk() {

		// fc-4.9.2019 Set default value, may be changed below
		dataBlock.setElementRestrictionAtIndividualLevel(false, "DEConfigurationPanel");

		// fc - 22.4.2004 - at least one check needed
		//
		if (statusChooser != null) {
			if (!statusChooser.isChooserValid()) {
				MessageDialog.print(this, Translator.swap("Shared.chooseAtLeastOneStatus"));
				return false;
			}
		}

		// fc-23.3.2004
		if (grouperChooser != null && grouperChooser.isGrouperAvailable()) {

			if (grouperChooser.getGrouperName() == null || grouperChooser.getGrouperName().equals("")) {
				MessageDialog.print(this, Translator.swap("DEMultiConfPanel.wrongGrouperName"));
				return false;
			}

			// fc-4.9.2019 Check individual / common consistency
			if (dataBlock.isElementRestrictionAtCommonLevel()) {

				MessageDialog.print(this,
						Translator.swap("Shared.errorDuringElementsRestrictionConsistencyManagement"));

				return false;
			} else {
				dataBlock.setElementRestrictionAtIndividualLevel(true, "DETimeConfigPanel");
			}
		}

		// fc-4.9.2019
		if (treeIds != null) {
			List<String> ids = getTreeIds();

			if (ids.size() > 0) { // user specified ids
				// fc-4.9.2019 Check individual / common consistency
				if (dataBlock.isElementRestrictionAtCommonLevel()) {
					MessageDialog.print(this,
							Translator.swap("Shared.errorDuringElementsRestrictionConsistencyManagement"));
					return false;
				} else {
					dataBlock.setElementRestrictionAtIndividualLevel(true, "DETimeConfigPanel");
				}

			}

		}

		return true;
	}

	// fc-4.9.2019 return a list of String instead of the JTextField
	public List<String> getTreeIds() {

		List<String> ids = new ArrayList<>();

		if (treeIds == null)
			return ids;

		StringTokenizer st = new StringTokenizer(treeIds.getText().trim(), ", ");

		while (st.hasMoreTokens())
			ids.add(st.nextToken());

		return ids;
	}

	// public JTextField getTreeIdsTF() {
	// return treeIds;
	// }

}
