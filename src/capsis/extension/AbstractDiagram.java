package capsis.extension;

import javax.swing.JPanel;

/**
 * A superclass for Capsis diagrams that can be managed by the positioners to be
 * dispatched in the diagrams zone of the mainFrame.
 * 
 * @author F. de Coligny - December 2015
 */
public abstract class AbstractDiagram extends JPanel {

	private int index;

	/**
	 * Constructor.
	 */
	public AbstractDiagram() {
		super();
	}

	public void setIndex(int index) {
		this.index = index;
	}

	public int getIndex() {
		return index;
	}

	/**
	 * If the diagram is a StandViewer, this is the standviewer class name. If
	 * the diagram is a DataRenderer, this is the matching DataExtractor
	 * className. Used by the positioners.
	 */
	abstract public String getDiagramClassName();
}
