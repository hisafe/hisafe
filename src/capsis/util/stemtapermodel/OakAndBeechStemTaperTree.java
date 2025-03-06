package capsis.util.stemtapermodel;

import repicea.simulation.covariateproviders.treelevel.CrownBaseHeightProvider;
import repicea.simulation.covariateproviders.treelevel.DbhCmProvider;
import repicea.simulation.covariateproviders.treelevel.HeightMProvider;
import repicea.simulation.stemtaper.AbstractStemTaperPredictor.BasicStemTaperTree;

public interface OakAndBeechStemTaperTree extends BasicStemTaperTree,
												CrownBaseHeightProvider, 
												DbhCmProvider, 
												HeightMProvider {
	
	public enum Species {Oak, Beech};
	
	/**
	 * This method returns the species of the OakAndBeechStemTaperTree instance.
	 * @return a Species enum
	 */
	public Species getSpecies();
	

}
