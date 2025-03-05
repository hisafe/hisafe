package capsis.extension.dataextractor.configuration;

import capsis.extensiontype.DataBlock;

/**
 * An interface for objects creating dataBlocks. They should be told when a
 * DataBlock is disposed. The main dbManager is the Selector, which holds an up
 * to date list of opened dataBlocks.
 * 
 * @author F. de Coligny - September 2021
 */
public interface DataBlockManager {

	/**
	 * Called by a dataBlock when it is disposing (this is used by the Selector to
	 * remove the matching entry in its opened dataBlock list).
	 */
	public void dataBlockDisposed(DataBlock db);

}
