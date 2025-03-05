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

package capsis.extension.intervener.albrechtwindstormmodel;

import java.util.Collection;
import java.util.Random;

import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeCollection;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.AbstractGroupableIntervener;
import capsis.kernel.extensiontype.Intervener;
import capsis.util.group.GroupableIntervener;
import capsis.util.group.GroupableType;
import jeeb.lib.util.Automatable;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;
import jeeb.lib.util.annotation.Ignore;
import lerfob.windstormdamagemodels.awsmodel.AWSStand;
import lerfob.windstormdamagemodels.awsmodel.AWSTreatment;
import lerfob.windstormdamagemodels.awsmodel.AWSTree;
import lerfob.windstormdamagemodels.awsmodel.AlbrechtWindStormModel;

/**	
 * The AlbrechtWindStormModel model implements the model developed by Albrecht et al.
 * @author M. Fortin - August 2010
 */
public class AWSIntervener extends AbstractGroupableIntervener implements Intervener, GroupableIntervener, Automatable {
//public class AWSIntervener implements Intervener, GroupableIntervener, Automatable {
	
	// fc-8.6.2020 Added AbstractGroupableIntervener for getSceneArea (scene)

	/*
	 * Bundle
	 * capsis.extension.intervener.awsmodel.Labels
	 * 
	 * Labels
	 * 
	 * 
	 */
	static {
		Translator.addBundle ("capsis.extension.intervener.albrechtwindstormmodel.Labels");
	}

	// nb-06.08.2018
	//public static final String NAME = "AlbrechtWindStormModel";
	//public static final String VERSION = "1.0";
	//public static final String AUTHOR =  "M. Fortin and A. Albrecht";
	//public static final String DESCRIPTION = "AWSIntervener.description";

	// nb-29.08.2018
	//static public String SUBTYPE = "NaturalDisturbance";
	
	public enum StandStatusAfterWindStorm {NoDamage, PartialDamage, FullDamage}
	private Random randomizer = new Random();
	
	private AlbrechtWindStormModel stormMaker;
	@Ignore
	private boolean constructionCompleted = false;		// if cancel in interactive mode, false
	@Ignore
	private int mode;				// CUT or MARK
	@Ignore
	private GScene stand;			// Reference stand: will be altered by apply ()
	@Ignore
	private GScene originalStand;
	@Ignore
	protected Collection<Tree> concernedTrees;
	



	/**
	 * Default constructor.
	 */
	public AWSIntervener () {}
	
	
	@SuppressWarnings("unchecked")
	@Override
	public void init(GModel m, Step s, GScene scene, Collection c) {
		stormMaker = new AlbrechtWindStormModel();
		
		originalStand = s.getScene();
		// This is always in starter for every intervener
		stand = scene;	// this is referentStand.getInterventionBase ();

		if (c == null) {		// fc - 22.9.2004
			concernedTrees = (Collection<Tree>) ((TreeList) stand).getTrees ();
		} else {
			concernedTrees = c;
		}
		
		// 0. Define cutting mode : ask model
		mode = (m.isMarkModel ()) ? MARK : CUT;
		constructionCompleted = true;
	}
	
	
	@Override
	public boolean initGUI() throws Exception {
		constructionCompleted = true;
		return constructionCompleted;
		
	}
	


	/**	
	 * Extension dynamic compatibility mechanism.
	 * This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	 */
	static public boolean matchWith(Object referent) {
		try {
			if (!(referent instanceof GModel)) {return false;}
			GModel m = (GModel) referent;
			GScene s = ((Step) m.getProject ().getRoot ()).getScene ();
			if (!(s instanceof AWSStand)) {return false;}
		} catch (Exception e) {
			Log.println (Log.ERROR, "AWSIntervener.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("AWSIntervener.name");
	}

	@Override
	public String getAuthor() {
		return "M. Fortin and A. Albrecht";
	}

	@Override
	public String getDescription() {
		return Translator.swap("AWSIntervener.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public String getSubType() {
		return Translator.swap("AWSIntervener.subType");
	}

	/**	
	 * GroupableIntervener interface. This intervener acts on trees,
	 * tree groups can be processed.
	 */
	public GroupableType getGrouperType () {return TreeList.GROUP_ALIVE_TREE;}

	//	These assertions are checked at the beginning of apply ().
	//
	private boolean assertionsAreOk () {return true;}

	/**	
	 * From Intervener.
	 *	Control input parameters.
	 */
	public boolean isReadyToApply () {
		// Cancel on dialog in interactive mode -> constructionCompleted = false
		if (constructionCompleted && assertionsAreOk ()) {return true;}
		return false;
	}

	/**	
	 * From Intervener.
	 *	Makes the action : thinning.
	 */
	public Object apply () throws Exception {
		if (!isReadyToApply ()) {
			throw new Exception (getClassName() + ".apply () - Wrong input parameters, see Log");
		}
		
		Collection<AWSTree> trees = ((AWSStand) stand).getAlbrechtWindStormModelTrees();
		
		double[] probabilities = stormMaker.getPredictionForThisStand((AWSStand) originalStand, (AWSTreatment) originalStand);
		
		StandStatusAfterWindStorm standStatus = getStandAfterStormResult(probabilities);

		if (standStatus == StandStatusAfterWindStorm.PartialDamage || standStatus == StandStatusAfterWindStorm.FullDamage) {
			for (AWSTree tree : trees) {
				boolean windthrow = false;
				double nb = 1d;
				if (tree instanceof Numberable) {
					nb = ((Numberable) tree).getNumber();
				}
				
				if (standStatus == StandStatusAfterWindStorm.FullDamage) {
					((TreeCollection) stand).removeTree ((Tree) tree);
					if (tree instanceof Numberable) {
						((TreeList) stand).storeStatus((Numberable) tree, "windthrow", nb);					
					} else {
						((TreeList) stand).storeStatus((Tree) tree, "windthrow");					
					}
				} else {
					double nbAlive = 0;
					double nbWindthrow = 0;
					double prediction = 0;
					for (int i = 0; i < Math.ceil(nb); i++) {
							// TODO algorithm is incomplete
					}
//				} else {
//					windthrow = true;
				}
				
			}
			
		}
		return stand;
	}

	
	private StandStatusAfterWindStorm getStandAfterStormResult(double[] probabilities) {
		if (probabilities.length != 3) {
			return null;
 		} else {
 			if (randomizer.nextDouble() < probabilities[0]) {
 				if (randomizer.nextDouble() < probabilities[1]) {
 					return StandStatusAfterWindStorm.FullDamage;
 				} else  {
 					return StandStatusAfterWindStorm.PartialDamage;
 				}
 			} else {
 				return StandStatusAfterWindStorm.NoDamage;
 			} 
 		}
	}
	
	/**	Normalized toString () method : should allow to rebuild a filter with
	*	same parameters.
	*/
	public String toString () {
		return "class="+getClassName()
				// nb-06.08.2018
				//+" name="+ NAME
				+" name="+getName()
				+" constructionCompleted=" + constructionCompleted
				+" mode=" + mode
				+" stand="+stand
				;
	}

	// fc-14.12.2020 Unused, deprecated
//	public void activate() {
//	}

	private String getClassName() {return getClass().getSimpleName();}	
	
}

