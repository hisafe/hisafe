package capsis.lib.genetics.memory;

/**
 * This interface is for some GeneticScenes which use a code which is not their
 * hashCode. This code is used in the GeneticMemory to link the memorized
 * MemoGeneticScenes and be able to go back in time when searching a genotypable
 * mother or father at child genotype creation time with GeneticMemory.searchGee
 * ().
 * 
 * Note: this was implemented to manage IbaBreederLists in the GeneticMemory,
 * containing only reproducer fish during the reproduction window.
 * 
 * @author F. de Coligny, M. Buoro - July 2021
 */
public interface WithOriginalSceneCode {

	/**
	 * Returns the code stored for linkage in the genetic memory, e.g. for a
	 * IbaBreederList: the code of an IbaScene. GeneticMemory.searchGee
	 * (GeneticScene fromScene, int geeId) looks in the scenes history starting with
	 * the code of fromScene, which is an instance of GScene (e.g. an IbaScene).
	 */
	public int getOriginalSceneCode();

}
