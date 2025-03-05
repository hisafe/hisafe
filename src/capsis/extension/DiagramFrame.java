package capsis.extension;

import javax.swing.JInternalFrame;

/**
 * An internal frame which content is an AbstractDiagram. Can be sorted on the
 * diagram's index values.
 * 
 * @author F. de Coligny - December 2015
 */
public class DiagramFrame extends JInternalFrame implements Comparable {
	
	// The diagram in the frame
	// (AbstractPanelDataRenderer, AbstractStandViewer)
	private AbstractDiagram diagram;

	/**
	 * Constructor.
	 */
	public DiagramFrame(AbstractDiagram diagram, boolean resizable, boolean closable, boolean maximizable,
			boolean iconifiable) {
		super(diagram.getName(), resizable, closable, maximizable, iconifiable);

		this.diagram = diagram;
	}

	/**
	 * The frames order is the same than the diagrams order.
	 */
	public int getIndex () {
		return diagram.getIndex();
	}

	/**
	 * For original diagrams order restoration.
	 */
	@Override
	public int compareTo(Object o) {
		DiagramFrame other = (DiagramFrame) o;
		// Returns a positive value if this object is 'greater'
		// Returns a negative value if this object is 'smaller'
		// Returns 0 if 'equal'
		return getIndex() - other.getIndex();
	}
	
	public String toString () { // fc-14.8.2018 Checking memory releasing
		String name = diagram == null ? "null" : diagram.getClass ().getName ();
		return "DiagramFrame, diagram: "+name;
	}
	
}
