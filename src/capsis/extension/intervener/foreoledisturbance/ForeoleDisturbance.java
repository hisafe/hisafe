/*
 * Biomechanics library for Capsis4.
 *
 * Copyright (C) 2001-2003  Philippe Ancelin.
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

package capsis.extension.intervener.foreoledisturbance;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.Intervener;
import capsis.lib.biomechanics.MecaTreeInfo;

/**
 * ForeoleDisturbance - Model FOREOLE: model tool to analyse wind damages on trees at stand level.
 *
 * @author Ph. Ancelin - October 2001
 */
public class ForeoleDisturbance implements Intervener {

	static {
		Translator.addBundle("capsis.extension.intervener.foreoledisturbance.ForeoleDisturbance");
	}

	// nb-06.08.2018
	//public static final String NAME = "ForeoleDisturbance";
	//public static final String VERSION = "2.0";
	//public static final String AUTHOR =  "Ph. Ancelin";
	//public static final String DESCRIPTION = "ForeoleDisturbance.description";

	// nb-29.08.2018
	//static public String SUBTYPE = "NaturalDisturbance";

	private boolean constructionCompleted = false;		// if cancel in interactive mode, false
	private int mode;				// CUT or MARK
	private GScene stand;			// Reference stand: will be altered by apply ()
	private GModel model;			// Associated model
	private Collection treeIdsToCut;	// Cut (or mark) these trees (contains Integers)

	public double windSpeedEdgeAt10m;

	private Step step;

	/**
	 * Default constructor.
	 */
	public ForeoleDisturbance () {}

	/**
	 * Script constructor.
	 */
	public ForeoleDisturbance (double windSpeedEdgeAt10m) throws Exception {
		
		this.windSpeedEdgeAt10m = windSpeedEdgeAt10m;
		constructionCompleted = true;
		
	}
	
	@Override
	public void init(GModel m, Step s, GScene scene, Collection c) {
		
		// This is always in starter for every intervener
		model = m;
		stand = scene;	// this is referentStand.getInterventionBase ();
		treeIdsToCut = new ArrayList ();

		// 0. Define cutting mode : ask model
		mode = (model.isMarkModel ()) ? MARK : CUT;

		step = s;
		TreeCollection tc = (TreeCollection) (step.getScene ());
		
		if (tc.getTrees ().isEmpty ()) {
			Log.println ("\n  ForeoleDisturbance () : Actual TreeCollection is Empty! No compatibility!\n");
			return;
		} else {
			Tree tree = tc.getTrees ().iterator ().next ();
			if (!(tree instanceof MecaTreeInfo)) {
				Log.println ("\n  ForeoleDisturbance () : FirstTree is not a MecaTreeInfo! No compatibility!\n");
				return;
			}
		}
		
	}

	@Override
	public boolean initGUI() throws Exception {
		// 3. Interactive start
		ForeoleDisturbanceDialog dlg = new ForeoleDisturbanceDialog (this, step);
		constructionCompleted = false;
		if (dlg.isValidDialog ()) {
			// valid -> ok was hit and check were ok
			try {
				treeIdsToCut = (ArrayList) (dlg.getTreeIdsToCut ());
				constructionCompleted = true;
			} catch (Exception e) {
				constructionCompleted = false;
				throw new Exception ("ForeoleDisturbance (): Could not get parameters in ForeoleDisturbanceDialog due to "+e);
			}

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
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;

			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof TreeCollection)) {return false;}
			if (!(s instanceof TreeList)) {return false;}	// fc - 19.3.2004

			TreeCollection tc = (TreeCollection) s;
			if (tc.getTrees ().isEmpty ()) {
				Log.println ("\n  ForeoleDisturbance.matchWith () : Initial TreeCollection is Empty! Potential compatibility!\n");
			} else {
				Tree tree = tc.getTrees ().iterator ().next ();
				if (!(tree instanceof MecaTreeInfo)) {return false;}
				// the first tree of collection must be an instance of MecaTreeInfo
				// but it can be a Maid or Madd Tree...
				// if Maid Tree : simulated spatialization
				// if Madd Tree : spatialization using the x, y, z properties of tree.
			}

		} catch (Exception e) {
			Log.println (Log.ERROR, "ForeoleDisturbance.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("ForeoleDisturbance.name");
	}

	@Override
	public String getAuthor() {
		return "Ph. Ancelin";
	}

	@Override
	public String getDescription() {
		return Translator.swap("ForeoleDisturbance.description");
	}

	@Override
	public String getVersion() {
		return "2.0";
	}

	@Override
	public String getSubType() {
		return Translator.swap("ForeoleDisturbance.subType");
	}

	//
	// These assertions are checked at the beginning of apply ().
	//
	private boolean assertionsAreOk () {
		if (mode != CUT && mode != MARK) {
			Log.println (Log.ERROR, "ForeoleDisturbance.assertionsAreOk ()", "Wrong mode="+mode
					+", should be "+CUT+" (CUT) or "+MARK+" (MARK). ForeoleDisturbance is not appliable.");
			return false;
		}
		if (model == null) {
			Log.println (Log.ERROR, "ForeoleDisturbance.assertionsAreOk ()",
				"model is null. ForeoleDisturbance is not appliable.");
			return false;
		}
		if (stand == null) {
			Log.println (Log.ERROR, "ForeoleDisturbance.assertionsAreOk ()",
				"stand is null. ForeoleDisturbance is not appliable.");
			return false;
		}
		return true;
	}

	/**
	 * From Intervener.
	 * Control input parameters.
	 */
	public boolean isReadyToApply () {
		// Cancel on dialog in interactive mode -> constructionCompleted = false
		if (constructionCompleted && assertionsAreOk ()) {
			return true;
		}
		return false;
	}

	/**
	 * From Intervener.
	 * Makes the action : thinning.
	 */
	public Object apply () throws Exception {
		// 0. Check if apply possible (should have been done before : security)
		if (!isReadyToApply ()) {
			throw new Exception ("ForeoleDisturbance.apply () - Wrong input parameters, see Log");
		}

		stand.setInterventionResult (true);

		// CUT or MARK trees in reference stand according to current mode
		for (Iterator i = treeIdsToCut.iterator (); i.hasNext ();) {
			int id = ((Integer) i.next ()).intValue ();
			Tree t = ((TreeCollection) stand).getTree (id);
			if (mode == CUT) {
				((TreeCollection) stand).removeTree (t);
			} else if (mode == MARK) {
				t.setMarked (true);
			}
			
			// fc - 18.3.2004
			if (!(t instanceof Numberable)) {((TreeList) stand).storeStatus (t, "cut");}
			
		}

		return stand;
	}

	public Step getStep () {return step;}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

}

