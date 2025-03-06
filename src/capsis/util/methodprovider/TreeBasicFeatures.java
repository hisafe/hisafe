package capsis.util.methodprovider;

import jeeb.lib.util.Identifiable;
import capsis.defaulttype.Numberable;


/**
 * This interface extends the Idenfiable and Numberable interfaces
 * and provides the species name and the DBH
 * @author Mathieu Fortin - November 2009
 *
 */
public interface TreeBasicFeatures extends Numberable, Identifiable {
	public String getSpeciesName();
	public double getDbh();
}
