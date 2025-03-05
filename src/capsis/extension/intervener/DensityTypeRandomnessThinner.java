package capsis.extension.intervener;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Random;

import capsis.defaulttype.ContainsTargetTrees;
import capsis.defaulttype.ContainsVirtualTrees;
import capsis.defaulttype.Numberable;
import capsis.defaulttype.Tree;
import capsis.defaulttype.TreeList;
import capsis.kernel.GModel;
import capsis.kernel.GScene;
import capsis.kernel.Step;
import capsis.kernel.extensiontype.AbstractGroupableIntervener;
import capsis.kernel.extensiontype.Intervener;

import capsis.util.group.GroupableIntervener;
import capsis.util.group.GroupableType;
import capsis.util.methodprovider.GProvider;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.Translator;

/**
 * Generic thinner based on previous version of the Gymnos module
 * It thin trees according to a given intensity, type and randomness
 * 
 * @author Gauthier Ligot - June 2019
 */
public class DensityTypeRandomnessThinner extends AbstractGroupableIntervener implements Intervener, GroupableIntervener {
//public class DensityTypeRandomnessThinner implements Intervener, GroupableIntervener {
	
	// fc-8.6.2020 Added AbstractGroupableIntervener for getSceneArea (scene)

	static {
		Translator.addBundle ("capsis.extension.intervener.DensityTypeRandomnessThinner");
	}

	private boolean constructionCompleted = false; 
	private int mode; 
	protected GScene stand; 
	protected GModel model;
	protected Collection<Tree> concernedTrees; 
	private Collection<Integer> treeIds;
	transient private List<ScoredTree> arraytree = new ArrayList<ScoredTree>();
	private boolean excludeTargetTrees;
	private double intensity;
	private double type;
	private String thinningType;
	private double randomness;
	private double initGHA;
	private boolean isSimmemThinning; 

	
	/**
	 * Default constructor.
	 */
	public DensityTypeRandomnessThinner() {
		isSimmemThinning = false;
	}
	
	/**
	 * Script constructor.
	 * 
	 * @param thType - the type of thinning : nha, gha (or rdi)
	 * @param intensity - the intensity in [0;1]
	 * @param type - the type in [0;1]
	 * @param excludeTargetTrees - boolean to exclude target trees if any
	 */
	public DensityTypeRandomnessThinner(String thType, double intensity, double type, double randomness, 
			boolean excludeTargetTrees) {
		this.thinningType = thType.toLowerCase ();
		this.intensity = intensity;
		this.type = type;
		this.randomness = randomness;
		this.excludeTargetTrees = excludeTargetTrees;
		isSimmemThinning = false;
	}
	
	/**
	 * Simmem constructor.
	 * 
	 * @param thType - the type of thinning : nha, gha (or rdi)
	 * @param intensity - the intensity in [0;1]
	 * @param type - the type in [0;1]
	 * @param excludeTargetTrees - boolean to exclude target trees if any
	 * @param maxCut - number of minimum unthinned tree 
	 */
	public DensityTypeRandomnessThinner(String thType, double intensity, double type, double randomness, 
			boolean excludeTargetTrees, boolean isSimmemThinning) {
		this.thinningType = thType.toLowerCase ();
		this.intensity = intensity;
		this.type = type;
		this.randomness = randomness;
		this.excludeTargetTrees = excludeTargetTrees;
		this.isSimmemThinning = isSimmemThinning;
	}

	public GroupableType getGrouperType () {
		return TreeList.GROUP_ALIVE_TREE;
	}

	/**
	 * Initializing without gui
	 */
	public void init (GModel m, Step s, GScene scene, Collection c) {
		stand = scene;  // this is referentStand.getInterventionBase ();
		model = m;
		// The trees that can be cut
		if (c == null) {
			concernedTrees = new ArrayList<Tree> ((Collection<Tree>) ((TreeList) stand).getTrees ());
		} else {
			concernedTrees = new ArrayList<Tree> (c);
		}

		// Save ids for future use
		treeIds = new HashSet<Integer> ();
		for (Object o : concernedTrees) {
			Tree t = (Tree) o;
			treeIds.add (t.getId ());
		}

		// Define cutting mode: ask model
		mode = (m.isMarkModel ()) ? MARK : CUT;
		
		// fc-8.6.2020 considers group area is any
		initGHA =  ((GProvider) m.getMethodProvider ()).getG (stand, ((TreeList) scene).getTrees()) * (10000/getSceneArea (scene));
//		initGHA =  ((GProvider) m.getMethodProvider ()).getG (stand, ((TreeList) scene).getTrees()) * (10000/scene.getArea ());

		constructionCompleted = true;	
	}

	/**
	 * Initializing GUI
	 */
	public boolean initGUI () throws Exception {
		// Interactive start
		DensityTypeRandomnessThinnerDialog gtd = new DensityTypeRandomnessThinnerDialog (this);

		constructionCompleted = false;
		if (gtd.isValidDialog ()) {
			// valid -> ok was hit and all checks were ok
			try {
				excludeTargetTrees = gtd.isExcludeTargetTrees ();
				intensity = gtd.getIntensity ();
				type = gtd.getTypeValue ();
				randomness = gtd.getRandomness ();
				thinningType = gtd.getThinningType ();
				constructionCompleted = true;
			} catch (Exception e) {
				constructionCompleted = false;
				throw new Exception ("DensityTypeRandomnessThinner (): Could not get parameters in GymnoGlobalThinnerDialog", e);
			}
		}
		gtd.dispose ();

		return constructionCompleted;
	}
	
	/**
	 * Define possible match
	 */
	static public boolean matchWith (Object referent) {
		try {
			GModel model = (GModel) referent;
			GScene scene = ((Step)model.getProject().getRoot()).getScene();
			if (!(scene instanceof TreeList)) { return false; }
			TreeList tl = (TreeList)scene;
			Collection reps = AmapTools.getRepresentatives(tl.getTrees());
			for(Object o : reps){
				if ((o instanceof Numberable)) { return false; }
			}
		} catch (Exception e) {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}

		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("DensityTypeRandomnessThinner.name");
	}

	@Override
	public String getAuthor() {
		return "Gauthier Ligot";
	}

	@Override
	public String getDescription() {
		return Translator.swap("DensityTypeRandomnessThinner.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}

	@Override
	public String getSubType() {
		return Translator.swap("DensityTypeRandomnessThinner.subType");
	}

	private boolean assertionsAreOk() {
		if (mode != CUT && mode != MARK) {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()", "Wrong mode=" + mode + ", should be " + CUT + " (CUT) or " + MARK
					+ " (MARK). DensityTypeRandomnessThinner is not appliable.");
			return false;
		}
		if (stand == null) {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()", "stand is null. DensityTypeRandomnessThinner is not appliable.");
			return false;
		}
		if ((thinningType.equals ("nha") || thinningType.equals ("rdi")) && (intensity<0 || intensity>1)){
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()","DensityTypeRandomnessThinner() - Intensity must be a double between 0 and 1");
			return false;
		}
		if (thinningType.equals ("gha") && intensity < 0) {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()","DensityTypeRandomnessThinner() - Intensity must be >= 0");
			return false;
		}
		if (type < 0 || type > 1){
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()","DensityTypeRandomnessThinner() - The thinning type must be a double between 0 and 1");
			return false;
		}
		if (randomness < 0 || randomness > 1) {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()","DensityTypeRandomnessThinner() - The total randomness must be a double between 0 and 1");
			return false;
		}
		if (!thinningType.equals ("nha") && !thinningType.equals ("gha") && !thinningType.equals ("rdi")) {
			Log.println (Log.ERROR, "DensityTypeRandomnessThinner.assertionsAreOk ()","DensityTypeRandomnessThinner() - The thinning type must be nha, gha or rdi");
			return false;
		}

		return true;
	}
	
	public boolean isReadyToApply () {
		if (constructionCompleted && assertionsAreOk ())  
			return true; 
		return false;
	}

	/**
	 * Thin the trees, main function.
	 */
	public Object apply () throws Exception {		
		//Check if we can proceed
		if (!isReadyToApply ()) {
			throw new Exception ("DensityTypeRandomnessThinner.apply () - Wrong input parameters, see Log");
		}
		
		
		
		double cMin, cMax, ci, circOfMaxProba, rangeMax;
		Random random = new Random();
		
		//compute cmin and cmax needed to interpret the type (cm)
		cMin = 1000;
		cMax = 0;
		for (Iterator i = concernedTrees.iterator (); i.hasNext ();) {
			Tree t = (Tree) i.next ();
			ci = t.getDbh()*Math.PI;
			if (ci<cMin) cMin = ci;
			if (ci>cMax) cMax = ci;
		}
		
		circOfMaxProba = type * ( cMax- cMin) + cMin;
		rangeMax = circOfMaxProba - cMin + 1;
		
		if (( - circOfMaxProba +cMax + 1) > rangeMax) 
			rangeMax = ( - circOfMaxProba + cMax + 1);

		// fc+bc-10.3.2021 Was missing for use during Samsara2 evolution: the same
		// intervener instance is used several times
		arraytree.clear();

		for (Iterator i = concernedTrees.iterator (); i.hasNext ();) {
			Tree t = (Tree) i.next ();
			ci = t.getDbh()*Math.PI;
			double uniformProba = random.nextDouble();
			double probaOfBeingCut = 1 - Math.abs(ci - circOfMaxProba)/rangeMax;
			double score = randomness * uniformProba + (1-randomness) * probaOfBeingCut;
			ScoredTree scoredTree = new ScoredTree (t, score);
			arraytree.add(scoredTree);
		}
		Collections.sort(arraytree);
		
		// fa-09.01.2023
		List<Tree> concernedTreesAlreadyRemovedAsVirtuals = new ArrayList<>();
		
		int i = 0;
//		double curVal, target, tmpN, tmpCg, tmpG, tmpNMax;
//		curVal = targCet = tmpN = tmpCg = tmpG = tmpNMax = 0;
			
		double curVal, target;
		curVal = target = 0;
		
		if (thinningType.equals ("nha"))
		{
			
			// fc-8.6.2020 considers group area is any
			curVal = ((TreeList) stand).size () * getSceneArea (stand) / 10000;
//			curVal = ((TreeList) stand).size () * stand.getArea () / 10000;

			target = curVal * (1-intensity);
		}
		else if (thinningType.equals ("gha"))
		{
			curVal = initGHA;
			target = curVal * (1-intensity);
//			target = intensity;
		} else {
			throw new Exception ("DensityTypeRandomnessThinner.apply () - Thinning with RDI has not been implemented yet, see Log");
		}
//		else {
//			// Retrieve method provider
//			methodProvider = model.getMethodProvider ();
//			
//			getRDI (GModel model, double Nha, double Dg, Species species); 
//			
//			
//			
//			curVal =  ((RDIProvider) methodProvider).getRDI (model,initNHA,initDg,species);
//			
//		}
//			tmpN = ((TreeList) stand).size ();
//			
//			double coefHa =  10000 / scene.getArea (); // fc-8.6.2020 use getSceneArea (scene)
//			tmpG =  ((GProvider) methodProvider).getG (stand, stand.getTrees()) * coefHa;
//			
//			tmpG = ((TreeList) stand).getBasalArea () * stand.getArea (); // fc-8.6.2020 use getSceneArea (scene)
//			tmpCg = Math.sqrt ((tmpG / tmpN) * 4 * Math.PI);
//			tmpNMax = ((GymnoStand) stand).getSpecies ().calcNTreesMaxPerHa (tmpCg) * stand.getArea ()/10000; // fc-8.6.2020 use getSceneArea (scene)
//			curVal = tmpN / tmpNMax;
//			target = intensity;
//		}
		
		int nbUnthinnedTrees = ((TreeList) stand).size ();

			
		while (curVal > target && i < arraytree.size ()) {
			Tree t = arraytree.get(i).t;
			
			// fa-09.01.2023
			if (stand instanceof ContainsVirtualTrees && concernedTreesAlreadyRemovedAsVirtuals.contains(t)) {
				i++;
				continue; // this concerned tree has already been removed => go to next tree
			}
			
//			if (random.nextDouble() < totRand) { // GL 20190617 - possible bug - Why using two different randomness coefficient? If totRand is not filled up, it should get the randomnsess value computed above 
//				Double randVal = Math.random();
				// MP 060416: Bug IndexOutOfBounds possible: if i == arraytree.size()-1 (last item)
//				int j = (i + 1) + (int) (randVal  * (arraytree.size() - (i + 1)));
//				t = arraytree.remove(j).t;
//				i--;
//			}
			
			// cut this tree
			((TreeList) stand).removeTree (arraytree.get(i).t);
			((TreeList) stand).storeStatus (arraytree.get(i).t, "cut");
			
			
			if (thinningType.equals ("nha"))
				
				// fc-8.6.2020 considers group area is any
				curVal -= getSceneArea (stand) / 10000;
//				curVal -= stand.getArea () / 10000;
			
			else if (thinningType.equals ("gha"))
				
				// fc-8.6.2020 considers group area is any
				curVal -= Math.pow(t.getDbh ()/100, 2) * Math.PI / 4 * 10000/getSceneArea (stand);
//				curVal -= Math.pow(t.getDbh ()/100, 2) * Math.PI / 4 * 10000/stand.getArea ();
			
			else {
//				tmpN--;
//				tmpG -= Math.pow(t.getDbh(),2)*Math.PI/4;
//				tmpCg = Math.sqrt((tmpG / tmpN) * 4 * Math.PI);
//				tmpNMax = ((GymnoStand) stand).getSpecies ().calcNTreesMaxPerHa (tmpCg) * stand.getArea ()/10000; // fc-8.6.2020 use getSceneArea (scene)
//				curVal = tmpN / tmpNMax;	
			}
					
			((TreeList) stand).removeTree (t);
			((TreeList) stand).storeStatus (t, "cut");
			i++;
			
			// fa-09.01.2023
			if (stand instanceof ContainsVirtualTrees) { // e.g. Heterofor
				// Remove virtual trees based on the removed tree
				ContainsVirtualTrees cvt = (ContainsVirtualTrees) stand;
				if (!cvt.isVirtualTree(t)) {
					List<Integer> virtualTreeIds = cvt.getVirtualTreeIdsBasedOnMe(t);
					if (virtualTreeIds != null && !virtualTreeIds.isEmpty()) {
						for (int vId : virtualTreeIds) {
							Tree vt =  ((TreeList) stand).getTree(vId);
							if (vt != null) {
								((TreeList) stand).removeTree (vt);
								((TreeList) stand).storeStatus (vt, "cut");
								
								if (thinningType.equals("nha"))
									curVal -= getSceneArea (stand) / 10000;
								else if (thinningType.equals("gha"))
									curVal -= Math.pow(t.getDbh ()/100, 2) * Math.PI / 4 * 10000/getSceneArea (stand);
								
							
								if (concernedTrees.contains(vt))
									concernedTreesAlreadyRemovedAsVirtuals.add(vt);
							}
						}
					}
				}
			}
			
			if (--nbUnthinnedTrees < 3) break; //if simmem is used for thinning (not clear cut) then leave at least three tree per stand

		}
		
		
		return stand;
	}

	public GScene getScene () {
		return stand;
	}
	
	/**
	 * class to order the trees to be cut
	 */
	protected class ScoredTree implements java.lang.Comparable {

		public Tree t;
		private double score;

		ScoredTree (Tree t, double score) {
			this.t = t;
			this.score=score;
		}

		public double getScore(){return score;}
		public void setScore(double score){this.score=score;}

		@Override
		public int compareTo(Object scoredtree) {
			double score1 = ((ScoredTree) scoredtree).getScore();
			double score2 = this.getScore();
			if (score1 < score2) return -1;
			else if(score1 == score2)return 0;
			else return 1;
		}
	}
	
	public double getInitGHA() {return initGHA;}
	
}
