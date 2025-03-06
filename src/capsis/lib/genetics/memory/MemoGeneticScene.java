package capsis.lib.genetics.memory;

import java.io.Serializable;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

import capsis.kernel.Step;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.Genotypable;

/**
 * Genetics library memory system. Keeps in memory information needed later,
 * when searching genotypable parents at a time they are not any more in the
 * GeneticScene.
 * 
 * @author F. de Coligny, S. Muratorio - May 2017
 */
public class MemoGeneticScene implements GeneticScene, WithOriginalSceneCode, Serializable {

	private GeneticMemory geneticMemory; // fc+som-22.5.2017

	private int originalSceneCode;

	private Map<Integer, MemoGenotypable> memoGees;

	private int storedDate;
	private boolean storedInitialScene;

	public String toString() {
		return "MemoGeneticScene storedDate: " + storedDate + "#memoGees: " + memoGees.size();
	}

	/**
	 * Constructor.
	 */
	public MemoGeneticScene(GeneticScene originalScene) {

		this.geneticMemory = originalScene.getGeneticMemory();

		originalSceneCode = originalScene.hashCode();

		memoGees = new HashMap<>();

		for (Object o : originalScene.getGenotypables()) {
			if (!(o instanceof Genotypable))
				continue;
			
			Genotypable gee = (Genotypable) o;

			MemoGenotypable memoGee = new MemoGenotypable(gee, this);
			memoGees.put(memoGee.getId(), memoGee);
		}

		this.storedDate = originalScene.getDate();
		this.storedInitialScene = originalScene.isInitialScene();

	}
	
	/**
	 * Returns the code stored for linkage in the genetic memory, e.g. for a
	 * IbaBreederList: the code of an IbaScene. GeneticMemory.searchGee
	 * (GeneticScene fromScene, int geeId) looks in the scenes history starting with
	 * the code of fromScene, which is an instance of GScene (e.g. an IbaScene).
	 */
	public int getOriginalSceneCode() {
		return originalSceneCode;
	}

	@Override
	public Genotypable getGenotypable(int id) {
		return memoGees.get(id);
	}

	@Override
	public Collection getGenotypables() {
		return memoGees.values();
	}

	@Override
	public Step getStep() {
		return null; // unused in genetic memory
	}

	@Override
	public int getDate() {
		return storedDate;
	}

	@Override
	public boolean isInitialScene() {
		return storedInitialScene;
	}

	@Override
	public GeneticMemory getGeneticMemory() {
		return geneticMemory;
	}

}
