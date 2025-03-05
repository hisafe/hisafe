package capsis.extension.modeltool.woodqualityworkshop;

/**
 * This interface is to be set for a JDialog that implements a progress bar feature.
 * The dialog is given to a non gui class. This non gui class may then ask the dialog to
 * show the progress bar.
 * @author M. Fortin - September 2010
 */
public interface ProgressBarFeatureDialog {
	@SuppressWarnings("unchecked")
	public void showProgressBar(Object task, javax.swing.SwingWorker job);
}
