package capsis.lib.genetics.memory;

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;

import capsis.lib.genetics.AlleleParameters;
import capsis.lib.genetics.GeneticScene;
import capsis.lib.genetics.GenoSpecies;
import capsis.lib.genetics.Genotypable;
import capsis.lib.genetics.GenotypableImpl;
import capsis.lib.genetics.Genotype;
import capsis.lib.genetics.IndividualGenotype;
import capsis.lib.genetics.MultiGenotype;

/**
 * Genetics library memory system. Keeps in memory information needed later,
 * when searching genotypable parents at a time they are not any more in the
 * GeneticScene.
 * 
 * @author F. de Coligny, S. Muratorio - May 2017
 */
public class MemoGenotypable implements Genotypable, Serializable {

	private MemoGeneticScene memoScene;

	private Genotypable originalGee;
	private double storedNumber;
	private MultiGenotype storedMultiGenotype;
	private Map storedPhenoValues;

	/**
	 * Constructor
	 */
	public MemoGenotypable(Genotypable gee, MemoGeneticScene memoScene) {
		this.originalGee = gee;
		this.memoScene = memoScene;

		// The variables below may change during the simulation (PDGTrees are
		// not cloned...), pick and store the value at creation time
		this.storedNumber = gee.getNumber();
		this.storedMultiGenotype = MultiGenotype.getMemo(gee.getMultiGenotype());
		this.storedPhenoValues = gee.getPhenoValues() == null ? null : new HashMap(gee.getPhenoValues());

	}

	public int getId() {
		return originalGee.getId();
	}

	public Genotypable getOriginalGenotypable () { // fc-9.6.2017 added
		return originalGee;
	}
	
	public GeneticScene getGeneticScene() {
		return memoScene;
	}

	public GenotypableImpl getImpl() {
		return originalGee.getImpl();
	}

	public GenoSpecies getGenoSpecies() {
		return originalGee.getGenoSpecies();
	}

	public boolean isGenotyped() {
		return originalGee.isGenotyped();
	}

	public boolean isMultiGenotyped() {
		return originalGee.isMultiGenotyped();
	}

	public int getMId() {
		return originalGee.getMId();
	}

	public int getPId() {
		return originalGee.getPId();
	}

	public int getCreationDate() {
		return originalGee.getCreationDate();
	}

	public double getNumber() {
		return storedNumber;
	}

	public AlleleParameters getAlleleParameters() {
		return originalGee.getAlleleParameters();
	}

	public MultiGenotype getMultiGenotype() {
		return storedMultiGenotype;
	}

	// Unused in Genetics memory (see constructor).
	public void setMultiGenotype(MultiGenotype mg) {
	}

	// Unused in Genetics memory (see constructor).
	public void setIndividualGenotype(IndividualGenotype ig) {
	}

	public Genotype getGenotype() {
		return originalGee.getGenotype();
	}

	public double getConsanguinity() {
		return originalGee.getConsanguinity();
	}

	// Unused in Genetics memory (see constructor).
	public void setConsanguinity(double f) {
	}

	public double getGlobalConsanguinity() {
		return originalGee.getGlobalConsanguinity();
	}

	// Unused in Genetics memory (see constructor).
	public void setGlobalConsanguinity(double f) {
	}

	public double getGeneticValue(String caractereName) {
		return originalGee.getGeneticValue(caractereName);
	}

	public double getFixedEnvironmentalValue(String parameterName) {
		return originalGee.getFixedEnvironmentalValue(parameterName);
	}

	public Map getPhenoValues() {
		return storedPhenoValues;
	}

	// Unused in Genetics memory (see constructor).
	public void setPhenoValues(Map m) {
	}

	public double getPhenoValue(String caractereName) {
		return originalGee.getImpl().getPhenoValue(this, caractereName);
	}

}
