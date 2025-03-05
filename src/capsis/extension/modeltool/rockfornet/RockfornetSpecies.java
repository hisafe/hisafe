package capsis.extension.modeltool.rockfornet;

/**
 * An interface for the species object of the modules compatible with Rockfornet
 * (see Samsa2Species).
 * 
 * @author B. Courbaud, F. de Coligny - February 2022
 */
public interface RockfornetSpecies {

	// fc+bc-16.2.2022 Added species reference names to improve processed by V. Lafond
	// and Rockfornet
	public static final String REFERENCE_PICEA = "REFERENCE_PICEA";
	public static final String REFERENCE_ABIES = "REFERENCE_ABIES";
	public static final String REFERENCE_FAGUS = "REFERENCE_FAGUS";
	public static final String REFERENCE_OTHER = "REFERENCE_OTHER";
	
	/**
	 * Returns the reference name matching this species: REFERENCE_PICEA, REFERENCE_ABIES,
	 * REFERENCE_FAGUS or REFERENCE_OTHER.
	 */
	public String getSpeciesReferenceName();

}
