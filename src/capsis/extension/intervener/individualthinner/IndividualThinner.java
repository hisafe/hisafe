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

package capsis.extension.intervener.individualthinner;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;

import capsis.defaulttype.ContainsVirtualTrees;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.gui.DListSelector;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.AbstractGroupableIntervener;
import capsis.kernel.extensiontype.Intervener;

import capsis.util.group.GroupableIntervener;
import capsis.util.group.GroupableType;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Create an IndividualThinner.
 * For interactive mode, use constructor with ExtensionStarter (containing stand to
 * thin and mode CUT/MARK trees). A dialog box is showed to get user choices.
 * For console mode, use the other constructor with specific paramater
 * IndividualThinnerStarter.
 *
 * @author F. de Coligny - May 2002, modified M. Fortin - September 2009
 */
public class IndividualThinner extends AbstractGroupableIntervener implements Intervener, GroupableIntervener {
//public class IndividualThinner  implements Intervener, GroupableIntervener {
	
	// fc-8.6.2020 Adapted AbstractGroupableIntervener for getSceneArea (scene)
	
	static {
		Translator.addBundle ("capsis.extension.intervener.individualthinner.IndividualThinner");
	}

	// nb-06.08.2018
	//public static final String NAME = "IndividualThinner";
	//public static final String VERSION = "1.2";
	//public static final String AUTHOR =  "F. de Coligny";
	//public static final String DESCRIPTION = "IndividualThinner.description";

	// nb-29.08.2018
	//static public String SUBTYPE = "SelectiveThinner";


	protected int mode;				// CUT or MARK
	protected Collection concernedTrees;	// Intervener will be ran on this trees only (maybe all, maybe a group)
	protected GScene scene;			// Reference stand: will be altered by apply ()
	protected GModel model;			// Associated model
	protected Collection treeIdsToCut;	// Cut (or mark) these trees (contains Integers)


	private boolean ok = false;

	/**
	 * Default constructor.
	 */
	public IndividualThinner () {

	}

	// M. Fortin 2010-09-13 : this constructor does not serve anymore
	// NOTE : use the setTreeIdsToCut method in script mode
	
//	/**	A constructor for script mode, give the ids of the trees to be cut, 
//	 * 	Than call init () and apply ().
//	 * @param treeIdsToCut: the ids of the trees to be cut.
//	 */
//	public IndividualThinner (Collection<Integer> treeIdsToCut) {
//
//		this.treeIdsToCut = treeIdsToCut;
//		this.ok = true;
//		
//	}


	/**
	 * This method serves to set the trees to be cut in script mode. This Intervener should be 
	 * used as follows 
	 * <PRE>
	 * IndividualThinner thinner = new IndividualThinner();
	 * thinner.init(gModel, step, gScene, (Collection) concernedTrees);
	 * thinner.setTreeIdsToCut((Collection) treesToCut);
	 * GScene harvestedStand = thinner.apply()
	 * </PRE>
	 */
	public void setTreeIdsToCut(Collection treeIdsToCut) {
		this.treeIdsToCut = treeIdsToCut;
		this.ok = true;
	}
	
	/**	Inits the intervener. It will perform its intervention on the given
	 * 	step / scene. The model class must be provided. 
	 * 	If concernedTrees is null, the intervener will be applied on all the alive 
	 * 	trees of the scene, otherwise it will be applied only on the concerned trees.
	 */
	@Override
	public void init(GModel m, Step s, GScene sc, Collection concernedTrees) {

		model = m;
		scene = sc;
		treeIdsToCut = new ArrayList ();

		// Define mode : ask model
		if (model.isMarkModel ()) {
			mode = MARK;
		} else {
			mode = CUT;
		}

		if (concernedTrees == null) {
			this.concernedTrees = ((TreeList) scene).getTrees ();
		} else {
			this.concernedTrees = concernedTrees;
		}

	}

	@Override
	public boolean initGUI() throws Exception {

		NumberFormat nf = NumberFormat.getInstance ();
		nf.setGroupingUsed (false);
		ok = false;
		Vector treeIds  = new Vector ();
		try {
			treeIds = makeTreeIds(nf);
		} catch (Exception e){
			Log.println (Log.ERROR, "IndividualThinner.c (ExtensionStarter)",
					"Error while preparing tree list : "+e.toString (), e);
			return false;
		}

		// 2. Create dialog box for user sublist selection
		// nb-06.08.2018
		//DListSelector dlg = new DListSelector ( Translator.swap(NAME) +" - "+scene.getCaption (),
		//		Translator.swap ("IndividualThinner.selectTheTreesToCut"),
		//		treeIds);
		DListSelector dlg = new DListSelector ( getName() +" - "+scene.getCaption (),
				Translator.swap ("IndividualThinner.selectTheTreesToCut"),
				treeIds);
		if (dlg.isValidDialog ()) {
			for (Iterator ite = dlg.getSelectedItems ().iterator (); ite.hasNext ();) {
				String str = (String) ite.next ();

				// fc - 26.3.2004 - discard "(number=...)"
				int i1 = str.indexOf (" ");
				if (i1 != -1) {str = str.substring (0, i1);}

				Integer id = new Integer (str);
				treeIdsToCut.add (id);
			}
			ok = true;
		}
		dlg.dispose ();
		return ok;
	}


	protected Vector<String> makeTreeIds(NumberFormat nf) {
		Vector<String> vec = new Vector<String>();

		//~ Collection trees = ((TreeCollection) stand).getTrees ();
		for (Iterator i = concernedTrees.iterator (); i.hasNext ();) {		// fc - 22.9.2004
			Tree t = (Tree) i.next ();

			if (t.isMarked ()) {continue;}	// fc - 5.1.2004 - ignore marked trees

			StringBuffer b = new StringBuffer (nf.format (t.getId ()));
			if (t instanceof Numberable) {
				double number = ((Numberable) t).getNumber ();	// fc - 22.8.2006 - Numberable returns double

				if (number <= 0) {continue;}	// fc - 19.9.2005 - forget trees with number = 0

				b.append (" ("+Translator.swap("IndividualThinner.numberOfTrees")+"=");			// string included in the translator to make sure French version is available MF2009-09-28
				b.append (number);
				b.append (")");
			}

			vec.addElement (b.toString ());
		}
		return vec;
	}



	/**	Extension dynamic compatibility mechanism.
	 *	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			if (!(s instanceof TreeList)) {return false;}	// fc - 19.3.2004

		} catch (Exception e) {
			Log.println (Log.ERROR, "IndividualThinner.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("IndividualThinner.name");
	}

	@Override
	public String getAuthor() {
		return "F. de Coligny";
	}

	@Override
	public String getDescription() {
		return Translator.swap("IndividualThinner.description");
	}

	@Override
	public String getVersion() {
		return "1.2";
	}

	@Override
	public String getSubType() {
		return Translator.swap("IndividualThinner.subType");
	}

	/**	GroupableIntervener interface. This intervener acts on trees,
	 *	tree groups can be processed.
	 */
	public GroupableType getGrouperType () {return TreeList.GROUP_ALIVE_TREE;}		// fc - 22.9.2004

	/**	These assertions must be checked before apply.
	*/
	private boolean assertionsAreOk () {
		if (mode != CUT && mode != MARK) {
			Log.println (Log.ERROR, "IndividualThinner.assertionsAreOk ()", "Wrong mode="+mode
					+", should be "+CUT+" (CUT) or "+MARK+" (MARK). IndividualThinner is not appliable.");
			return false;
		}
		if (model == null) {
			Log.println (Log.ERROR, "IndividualThinner.assertionsAreOk ()",
			"model is null. IndividualThinner is not appliable.");
			return false;
		}
		if (scene == null) {
			Log.println (Log.ERROR, "IndividualThinner.assertionsAreOk ()",
			"stand is null. IndividualThinner is not appliable.");
			return false;
		}
		if (treeIdsToCut == null) {
			Log.println (Log.ERROR, "IndividualThinner.assertionsAreOk ()",
			"treeIdsToCut = null. IndividualThinner is not appliable.");
			return false;
		}
		return true;
	}

	/**	From Intervener.
	 *	Control input parameters.
	 */
	public boolean isReadyToApply () {
		return assertionsAreOk () && ok;
	}

	/**	From Intervener.
	 *	Makes the action: thinning.
	 */
	public Object apply () throws Exception {
		// 0. Check if apply possible (should have been done before : security)
		if (!isReadyToApply ()) {
			throw new Exception ("IndividualThinner.apply () - Wrong input parameters, see Log");
		}
		


		scene.setInterventionResult (true);

		// 1. Retrieve trees to cut from their ids
		Vector treesToCut = new Vector ();
		for (Iterator i = treeIdsToCut.iterator (); i.hasNext ();) {
			int id = ((Integer) i.next ()).intValue ();
			treesToCut.add (((TreeCollection) scene).getTree (id));
		}
		
		// fa-09.01.2023
		List<Tree> treesToCutAlreadyRemovedAsVirtuals = new ArrayList<>();

		// 2. CUT or MARK trees in reference stand according to current mode
		Numberable n = null;
		for (Iterator ite = treesToCut.iterator (); ite.hasNext ();) {
			Tree t = (Tree) ite.next ();
			if (t instanceof Numberable) {n = ((Numberable) t);}
			double nMemo = 0;	// fc - 22.8.2006 - Numberable returns double
			
			// fa-09.01.2023
			if (scene instanceof ContainsVirtualTrees && treesToCutAlreadyRemovedAsVirtuals.contains(t))
				continue; // this tree has already been removed => go to next tree

			if (mode == CUT) {
				if (n != null) {
					nMemo = n.getNumber ();
					n.setNumber (0);	// fc - 5.7.2005 - (cm) do not remove Numberable trees, set their number to 0
				} else {
					((TreeCollection) scene).removeTree (t);
				}
			} else if (mode == MARK) {
				t.setMarked (true);
			}

			if (n != null) {
				((TreeList) scene).storeStatus (n, "cut", nMemo);	// fc - 5.7.2005 - "cut all this tree"

				
			} else {
				((TreeList) scene).storeStatus (t, "cut");		// fc - 18.3.2004
				
	
				
				// fa-09.01.2023
				if (scene instanceof ContainsVirtualTrees) { // e.g. Heterofor
					// Remove virtual trees based on the removed tree
					ContainsVirtualTrees cvt = (ContainsVirtualTrees) scene;
					if (!cvt.isVirtualTree(t)) {
						List<Integer> virtualTreeIds = cvt.getVirtualTreeIdsBasedOnMe(t);
						if (virtualTreeIds != null && !virtualTreeIds.isEmpty()) {
							for (int vId : virtualTreeIds) {
								Tree vt =  ((TreeList) scene).getTree(vId);
								if (vt != null) {
									((TreeList) scene).removeTree (vt);
									((TreeList) scene).storeStatus (vt, "cut");
									
	
									
									if (treesToCut.contains(vt))
										treesToCutAlreadyRemovedAsVirtuals.add(vt);
								}
							}
						}
					}
				}
			}			
		}

		
		return scene;
	}

	/**
	 * Normalized toString () method : should allow to rebuild a filter with
	 * same parameters.
	 */
	public String toString () {
		return "class="+getClass().getName ()
		// nb-06.08.2018
		//+" name=\""+ NAME +"\""
		+" name=\""+ getName() +"\""
		+" treeIdsToCut="+treeIdsToCut;
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}

