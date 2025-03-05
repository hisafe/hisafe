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

package capsis.extension.intervener.filterthinner;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import capsis.defaulttype.ContainsVirtualTrees;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.gui.DialogWithOkCancel;
import capsis.gui.FilterManager;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.AbstractGroupableIntervener;
import capsis.kernel.extensiontype.Intervener;

import capsis.util.group.Filtrer;
import capsis.util.group.GroupableIntervener;
import capsis.util.group.GroupableType;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Create an FilterThinner. For interactive mode, use constructor with
 * ExtensionStarter (containing stand to thin and mode CUT/MARK trees). A dialog
 * box is showed to get user choices. For console mode, use the other
 * constructor with specific paramater FilterThinnerStarter.
 * 
 * @author F. de Coligny - November 2000
 */
public class FilterThinner extends AbstractGroupableIntervener implements Intervener, GroupableIntervener {
	
	// fc-8.6.2020 Added AbstractGroupableIntervener for getSceneArea (scene)

	static {
		Translator.addBundle("capsis.extension.intervener.filterthinner.FilterThinner");
	}

	// nb-06.08.2018
	// public static final String NAME = "FilterThinner";
	// public static final String VERSION = "1.1";
	// public static final String AUTHOR = "F. de Coligny";
	// public static final String DESCRIPTION = "FilterThinner.description";

	// nb-29.08.2018
	// static public String SUBTYPE = "SelectiveThinner";

	private boolean constructionCompleted = false; // if cancel in interactive
													// mode, false

	// fc - 22.9.2004
	private Collection concernedTrees; // Intervener will be ran on this trees
										// only (maybe all, maybe a group)

	private int mode; // CUT or MARK
	private GScene stand; // Stand: will be altered by apply (), no step yet
	private Step refStep; // Step on which the intervener was opened

	private Collection filters; // These parametered filters will be applied on
								// reference stand
	private GModel model; // Economics2 implementation by Gauthier Ligot
							// 23/01/2019

	/**
	 * Default constructor.
	 */
	public FilterThinner() {
	}

	/**
	 * Build an individual thinner in console mode from an FilterThinnerStarter.
	 * It can then be executed by apply ().
	 */
	public FilterThinner(Collection filters) {

		this.filters = filters;

		constructionCompleted = true;

	}

	@Override
	public void init(GModel m, Step refStep, GScene scene, Collection c) {

		this.stand = scene;
		this.refStep = refStep;
		this.model = m; // Economoics2 implementation - gl - 23/01/2019

		if (c == null) {
			if (stand instanceof TreeCollection)
				concernedTrees = ((TreeCollection) stand).getTrees();
		} else {
			concernedTrees = c;
		}

		// Define mode : ask model
		if (m.isMarkModel()) {
			mode = MARK;
		} else {
			mode = CUT;
		}

	}

	@Override
	public boolean initGUI() throws Exception {
		// 2. Create dialog box for user sublist selection
		final FilterManager filterManager = new FilterManager(stand, concernedTrees, TreeList.GROUP_ALIVE_TREE,
				new ArrayList()); // fc
									// -
									// 22.9.2004
									// -
									// concernedTrees
		constructionCompleted = false;

		// build title bar name
		// step was passed in EXtensionStarter only to put its caption in title
		// bar nb-06.08.2018
		String titleBarName = getName(); // fc-31.8.2018
		// String titleBarName = Translator.swap(NAME);
		// String titleBarName = Translator.swap(getName());
		try {
			titleBarName += " - " + stand.getStep().getCaption();
		} catch (Exception e) {
		} // do not stop if trouble here

		DialogWithOkCancel dlg = new DFilterThinner(filterManager, titleBarName);

		filters = null;
		if (dlg.isValidDialog()) {
			filters = filterManager.getFilters();
			constructionCompleted = true;
		}
		filterManager.dispose();
		dlg.dispose();
		return constructionCompleted;
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	/**
	 * Extension dynamic compatibility mechanism. This matchwith method checks
	 * if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {
				return false;
			}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject().getRoot()).getScene();
			if (!(s instanceof TreeCollection)) {
				return false;
			}
			// if (!(s instanceof TreeList)) {return false;} // fc - 19.3.2004

		} catch (Exception e) {
			Log.println(Log.ERROR, "FilterThinner.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("FilterThinner.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("FilterThinner.description");
	}

	@Override
	public String getVersion() {
		return "1.1";
	}

	@Override
	public String getSubType() {
		return Translator.swap("FilterThinner.subType");
	}

	/**
	 * GroupableIntervener interface. This intervener acts on trees, tree groups
	 * can be processed.
	 */
	public GroupableType getGrouperType() {
		return TreeList.GROUP_ALIVE_TREE;
	} // fc - 22.9.2004

	// These assertions must be checked before apply.
	//
	private boolean assertionsAreOk() {
		if (mode != CUT && mode != MARK) {
			Log.println(Log.ERROR, "FilterThinner.assertionsAreOk ()", "Wrong mode=" + mode + ", should be " + CUT
					+ " (CUT) or " + MARK + " (MARK). FilterThinner is not appliable.");
			return false;
		}
		if (stand == null) {
			Log.println(Log.ERROR, "FilterThinner.assertionsAreOk ()",
					"stand is null. FilterThinner is not appliable.");
			return false;
		}
		if (filters == null) {
			Log.println(Log.ERROR, "FilterThinner.assertionsAreOk ()",
					"filters is null. FilterThinner is not appliable.");
			return false;
		}
		return true;
	}

	/**
	 * From Intervener. Control input parameters.
	 */
	public boolean isReadyToApply() {
		// Cancel on dialog in interactive mode -> constuctionCompleted = false
		if (constructionCompleted && assertionsAreOk()) {
			return true;
		}
		return false;
	}

	/**
	 * From Intervener. Makes the action : thinning.
	 */
	public Object apply() throws Exception {
		

		return stand;
	}

	/**
	 * Normalized toString () method : should allow to rebuild a filter with
	 * same parameters.
	 */
	public String toString() {
		// nb-06.08.2018
		// return "class=" + getClass().getName() + " name=\"" + NAME + "\"" + "
		// filters=" + filters;
		return "class=" + getClass().getName() + " name=\"" + getName() + "\"" + " filters=" + filters;
	}

}
