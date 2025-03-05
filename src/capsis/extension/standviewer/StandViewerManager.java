package capsis.extension.standviewer;

import capsis.extensiontype.StandViewer;

/**
 * An interface for objects creating standViewers. They should be told when a
 * standViewer is disposed. The main svManager is the Selector, which holds an up
 * to date list of opened standViewers.
 * 
 * @author F. de Coligny - September 2021
 */
public interface StandViewerManager {

	/**
	 * Called by a standViewer when it is disposing (this is used by the Selector to
	 * remove the matching entry in its opened standViewer list).
	 */
	public void standViewerDisposed(StandViewer sv);

}
