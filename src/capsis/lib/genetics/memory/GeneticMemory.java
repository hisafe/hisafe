package capsis.lib.genetics.memory;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import java.util.TreeSet;

import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.Genotypable;

/**
 * Genetics library memory system. Keeps in memory information needed later,
 * when searching genotypable parents at a time they are not any more in the
 * GeneticScene.
 * 
 * @author F. de Coligny, S. Muratorio - May 2017
 */
public class GeneticMemory implements Serializable {

	/**
	 * A key type for the maps below
	 */
	private static class Key implements Comparable, Serializable {
		public int date; // a GScene date
		public int code; // an Object hashCode

		/**
		 * Constructor
		 */
		public Key(GeneticScene scene) {
			try {
				init(scene, scene.getDate());
			} catch (Exception e) {
				this.date = -1;
				this.code = -1;
			}
		}

		/**
		 * Constructor 2, specific to PDG: scenes are not cloned, prevDate must be set
		 * explicitly
		 */
		public Key(GeneticScene scene, int date) {
			init(scene, date);
		}

		private void init(GeneticScene scene, int date) {
			if (scene == null) {
				this.date = -1;
				this.code = -1;

			} else {
				this.date = date;

				// fc+mb-21.7.2021 generalized originalSceneCode, in MemoGeneticScene and
				// IbaBreederList
//				if (scene instanceof MemoGeneticScene)

				if (scene instanceof WithOriginalSceneCode)
					// consider the hashcode of the original GeneticScene
					this.code = ((WithOriginalSceneCode) scene).getOriginalSceneCode();
				else
					// consider the hashcode of the GeneticScene
					this.code = scene.hashCode();

			}
		}

		/**
		 * Two objects returning true on equals () must have the same hashCode ().
		 */
		@Override
		public int hashCode() {
			return code * 1000 + this.date;
		}

		/**
		 * Two objects returning true on equals () must have the same hashCode ().
		 */
		@Override
		public boolean equals(Object o) {
			Key otherKey = (Key) o;
			return this.hashCode() == otherKey.hashCode();
			// return this.date == otherKey.date && this.code == otherKey.code;
		}

		public String toString() {
			return "" + code + "." + date;
		}

		@Override
		public int compareTo(Object o) {
			Key other = (Key) o;
			if (date < other.date)
				return -1;
			else if (date > other.date)
				return 1;
			else {
				if (code < other.code)
					return -1;
				else if (code > other.code)
					return 1;
				else
					return 0;
			}
		}

	}

	// Keep a ref on the initial scene
	private MemoGeneticScene initialGeneticScene;

	// Returns the MemoGeneticScene matching an original scene Key
	public Map<Key, MemoGeneticScene> sceneMap;

	// Given a scene Key, gives the Key of the previous scene
	public Map<Key, Key> linkMap; // scene_prevScene;

	// fc+mb-21.7.2021 In Ibasam, we have more steps in the project than the entries
	// in the GeneticMap. If searchGee () is called on a scene which was not added
	// in the GeneticMemory, start the search back in time from this
	// lastMemorizedKey
	private Key lastMemorizedKey;

	/**
	 * Constructor
	 */
	public GeneticMemory() {

		sceneMap = new HashMap<Key, MemoGeneticScene>();
		linkMap = new HashMap<Key, Key>();

	}

	/**
	 * GenericMemory created and stored in an instance variable of the
	 * initialParameters, passed to this method each time a scene is added in a
	 * project (e.g. end of initializeModel (), then each iteration of
	 * processEvolution ()).
	 */
	public MemoGeneticScene memorize(GeneticScene prevScene, GeneticScene scene) {

		// fc+mb+al-2.4.2021 memorize now returns the stored MemoGeneticScene

		// fc+som-22.5.2017
		// This memorize method is for the general case like Luberon: prevScene
		// and scene are two different object (check getEvolutionBase())
		// prevScene my be null at initializeModel () stage, when scene is
		// initScene

		if (prevScene == null) {
			return memorize(null, -1, scene);
		} else {
			return memorize(prevScene, prevScene.getDate(), scene);
		}

	}

	public MemoGeneticScene memorize(GeneticScene prevScene, int prevDate, GeneticScene scene) {

		// fc+mb+al-2.4.2021 memorize now returns the stored MemoGeneticScene

		// fc+som-22.5.2017
		// This memorize method is for the special case, like PDG, where
		// prevScene and scene are the same object in memory: PDG has a specific
		// history management with no clones
		// prevScene may be null at initializeModel () stage, when scene is
		// initScene

		MemoGeneticScene memoScene = new MemoGeneticScene(scene);

		// fc+som-27.6.2017
		if (scene.isInitialScene())
			initialGeneticScene = memoScene;

		// If scene is the initial scene in the project, prevScene is null ->
		// use -1 in kPrev
		Key kPrev = new Key(prevScene, prevDate);
		Key k = new Key(scene);

		sceneMap.put(k, memoScene);
		linkMap.put(k, kPrev);

		// fc+mb-21.7.2021
		lastMemorizedKey = k;

		// fc+mb+al-2.4.2021
		return memoScene;
	}

	/**
	 * Returns true if the initial scene contains the given genotypable id.
	 */
	public boolean initialSceneContains(int geeId) {
		if (initialGeneticScene == null)
			return false;

		return initialGeneticScene.getGenotypable(geeId) != null;
	}

	/**
	 * Returns null if initial scene
	 */
	public MemoGeneticScene getPreviousScene(GeneticScene geeScene) {

		Key k = new Key(geeScene);
		// System.out.println(" getPreviousScene()" + geeScene + " key: " + k);

		Key kPrev = linkMap.get(k);
		if (kPrev == null) {
			// System.out.println(" key: " + k + " NOT FOUND in linkMap");
			return null;
		}

		// System.out.println(" kPrev: " + kPrev);

		MemoGeneticScene prevScene = sceneMap.get(kPrev);
		// System.out.println(" prevScene: " + prevScene);
		return prevScene;

	}

	/**
	 * Find a genotypable given its id and a scene. If the genotypable is not in the
	 * scene (dead or cut at this date), go back in the history to find a scene
	 * containing an occurrence of the genotypable at a previous date.
	 */
	public Genotypable searchGee(GeneticScene fromScene, int geeId) {
		// System.out.println("GeneticMemory.searchGee (), fromScene: " + fromScene + "
		// geeId: " + geeId + "...");

		if (geeId == -1) {
			// System.out.println(" *** geeId == -1, returned null");
			return null;
		}

		// fc+mb-16.3.2022 At first call, fromScene may not be in the geneticMemory,
		// Ibasam only stores one scene per year, with the breeder list of the year
		// -> can help at first searchGee() call (should not happen during the recursive
		// calls)
		Key checkKey = new Key(fromScene);
		if (!sceneMap.containsKey(checkKey)) {
			if (lastMemorizedKey != null)
				fromScene = sceneMap.get(lastMemorizedKey);
		}

		Genotypable gee = (Genotypable) fromScene.getGenotypable(geeId);

		if (gee != null) {
			// System.out.println(" *** FOUND in scene: " + fromScene + ", returned: " +
			// gee);
			return gee;

		} else {

			MemoGeneticScene prevScene = getPreviousScene(fromScene);

			if (prevScene == null) {
				// System.out.println(" *** NOT FOUND reached root, returned null");
				return null;
			}

			return searchGee(prevScene, geeId);

		}
	}

	public String toString() {
		StringBuffer b = new StringBuffer("GeneticMemory...");
		b.append("\n  sceneMap:");
		for (Key k : new TreeSet<>(sceneMap.keySet())) {
			MemoGeneticScene s = sceneMap.get(k);
			b.append("\n    " + k + " : " + s);
		}
		b.append("\n  linkMap:");
		for (Key k : new TreeSet<>(linkMap.keySet())) {
			Key value = linkMap.get(k);
			b.append("\n    " + k + " : " + value);
		}

		return b.toString();

	}

}
