/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-SYSTEM), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.extension.intervener.safepruning;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Vector;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import safe.model.SafeModel;
import safe.model.SafeTree;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.gui.DListSelector;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;


/**
 * SafeTree pruning given a new crown base height
 *
 * For interactive mode, use constructor with ExtensionStarter (containing stand to
 * thin and mode PRUNE/MARK trees). A dialog box is showed to get user choices.
 * For console mode, use the other constructor with specific paramater SafePruningStarter.
 *
 * @author C Alauzet - July 2004
 */

public class SafePruning implements Intervener {

	static {
		Translator.addBundle ("safe.extension.intervener.safepruning.SafePruning");
	}

	// nb-20.08.2018
	//public static final String NAME = "SafePruning";
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR =  "C.Alauzet";
	//public static final String DESCRIPTION = "SafePruning.description";

	// nb-30.08.2018
	//static public String SUBTYPE = "SelectiveThinner";

	public  double pruningHeight;

	private boolean constructionCompleted = false;		// if cancel in interactive mode, false
	
	private GScene stand;						// Reference stand: will be altered by apply ()
	private GModel model;						// Associated model
	private Collection treeIdsToPrune;			// Cut (or mark) these trees (contains Integers)

	/**
	 * Default constructor.
	 */
	public SafePruning () {}

	/**
	 * This constructor is for interactive mode. It manages a dialog box
	 * for the user to choose the tree ids to prune.
	 * Then, apply () must be called.
	 */
	public SafePruning ( Vector treeIdsToPrune, double pruningHeight) {
		
		
		this.treeIdsToPrune = treeIdsToPrune;
		this.pruningHeight = pruningHeight;
		constructionCompleted = true;

	}


	@Override
	public void init(GModel m, Step s, GScene scene, Collection c) {
		// fc - 5.1.2004
		

		model = m;
		stand = scene;

		
	}

	@Override
	public boolean initGUI() throws Exception {
		NumberFormat nf = NumberFormat.getInstance ();
		nf.setGroupingUsed (false);
		
		treeIdsToPrune = new ArrayList ();
		constructionCompleted = false;
		// 1. Vector of tree candidate ids
		Vector treeIds  = new Vector ();
		try {
			Collection trees = ((TreeCollection) stand).getTrees ();
			for (Iterator i = trees.iterator (); i.hasNext ();) {
				Tree t = (Tree) i.next ();

				if (t.isMarked ()) {continue;}	// fc - 5.1.2004 - ignore marked trees

				StringBuffer b = new StringBuffer (nf.format (t.getId ()));
				if (t instanceof Numberable) {
					b.append (" (number=");
					b.append (((Numberable) t).getNumber ());
					b.append (")");
				}

				treeIds.addElement (b.toString ());
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "SafePruning.c (ExtensionStarter)",
					"Error while preparing tree list : "+e.toString (), e);
			return false;
		}

		// 2. Create dialog box for user sublist selection
		// nb-20.08.2018
		//DListSelector dlg = new DListSelector ( Translator.swap(NAME) + " - "+stand.getCaption (),
		DListSelector dlg = new DListSelector ( getName() + " - "+stand.getCaption (),
				Translator.swap ("SafePruning.selectTheTreesToPrune"),
				treeIds);
		if (dlg.isValidDialog ()) {
			for (Iterator ite = dlg.getSelectedItems ().iterator (); ite.hasNext ();) {
				String str = (String) ite.next ();

				// fc - 26.3.2004 prend le premier mot de str si elle contient lus d'un mot
				int i1 = str.indexOf (" ");
				if (i1 != -1) {str = str.substring (0, i1);}

				Integer id = new Integer (str);
				treeIdsToPrune.add (id);
			}

			SafePruningDialog dlg2 = new SafePruningDialog();

			if (dlg2.isValidDialog ()) {
				pruningHeight = dlg2.getPruningHeight();
				constructionCompleted = true;
			}
			dlg2.dispose ();
		}
		dlg.dispose ();
		return constructionCompleted;
		
	}

	/**
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof SafeModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			if (!(s instanceof TreeList)) {return false;}	// fc - 19.3.2004

		} catch (Exception e) {
			Log.println (Log.ERROR, "SafePruning.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("SafePruning.name");
	}

	@Override
	public String getAuthor() {
		return "C. Alauzet";
	}

	@Override
	public String getDescription() {
		return Translator.swap("SafePruning.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public String getSubType() {
		return Translator.swap("SafePruning.subType");
	}

	//
	// These assertions must be checked before apply.
	//
	private boolean assertionsAreOk () {

		if (model == null) {
			Log.println (Log.ERROR, "SafePruning.assertionsAreOk ()",
				"model is null. SafePruning is not appliable.");
			return false;
		}
		if (stand == null) {
			Log.println (Log.ERROR, "SafePruning.assertionsAreOk ()",
				"stand is null. SafePruning is not appliable.");
			return false;
		}
		if (treeIdsToPrune == null) {
			Log.println (Log.ERROR, "SafePruning.assertionsAreOk ()",
					"treeIdsToPrune = null. SafePruning is not appliable.");
			return false;
		}
		return true;
	}

	/**
	 * From Intervener.
	 * Control input parameters.
	 */
	public boolean isReadyToApply () {
		// Cancel on dialog in interactive mode -> constuctionCompleted = false
		if (constructionCompleted && assertionsAreOk ()) {return true;}
		return false;
	}

	/**
	 * From Intervener.
	 * Makes the action : thinning.
	 */
	public Object apply () throws Exception {
		// 0. Check if apply possible (should have been done before : security)
		if (!isReadyToApply ()) {
			throw new Exception ("SafePruning.apply () - Wrong input parameters, see Log");
		}

		stand.setInterventionResult (true);

		//1. Retrieve trees to prune from their ids and

		for (Iterator i = treeIdsToPrune.iterator (); i.hasNext ();) {
			int id = ((Integer) i.next ()).intValue ();
			SafeTree t =  ((SafeTree) ((TreeCollection) stand).getTree (id));

			try {
					t.pruning (0.5,pruningHeight,0,0);
			}
			catch(Exception e) {System.out.println(""+e);}

		}


		return stand;
	}
	
	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}
