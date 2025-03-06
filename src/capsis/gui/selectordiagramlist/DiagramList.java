package capsis.gui.selectordiagramlist;

import java.util.Collection;
import java.util.Set;
import java.util.TreeSet;

import jeeb.lib.util.Translator;

/**
 * A list of diagram lines with a name.
 * 
 * @author F. de Coligny - December 2015
 */
public class DiagramList implements Comparable {

	private String name;
	// In fact, this is a sorted Set
	private Set<DiagramLine> diagramLines;

	// Optional: this may be used for special diagramLists (e.g. from
	// extractorGroups), not editable, cannot be removed, not saved...
	private boolean locked;

	// Automatic names are sometimes not unique... fc-10.12.2015
	// /**
	// * Constructor with a default name.
	// */
	// public DiagramList() {
	// this(null); // default name, see getName ()
	// }

	/**
	 * Constructor with a given name.
	 */
	public DiagramList(String name) {
		this.name = name;
		// DiagramLines will be sorted according to their natural ordering
		// (rank)
		this.diagramLines = new TreeSet<>();
	}

	/**
	 * Constructor 2, copies the given diagramList. Warning: does not clone the
	 * diagramLines.
	 */
	public DiagramList(DiagramList dl) {
		this(dl.getName());
		setLocked(dl.isLocked());
		addDiagramLines(dl.getDiagramLines());
	}

	public void setLocked(boolean locked) {
		this.locked = locked;
	}

	public boolean isLocked() {
		return locked;
	}

	public void addDiagramLine(String className, String type, String encodedBounds) {
		DiagramLine d = new DiagramLine(className, type, encodedBounds);
		diagramLines.add(d);
	}

	public void addDiagramLine(DiagramLine d) {
		diagramLines.add(d);
	}

	public void addDiagramLines(Collection<DiagramLine> diagramLines) {
		this.diagramLines.addAll(diagramLines);
	}

	public Set<DiagramLine> getDiagramLines() {
		return diagramLines;
	}

	public boolean isEmpty () {
		return diagramLines == null || diagramLines.isEmpty ();
	}
	
	/**
	 * For renaming.
	 */
	public void setName(String name) {
		this.name = name;
	}

	public String getName() {
		String finalName = name;

		if (isLocked())
			finalName += " (*)";

		return finalName;

	}

	/**
	 * Returns a proposed name (which unicity is not checked, may need a suffix)
	 * for this diagramList.
	 */
	public String proposedName() {
		if (diagramLines.size() == 1)
			return Translator.swap("DiagramList.oneDiagram");
		else
			return Translator.swap("DiagramList.listOf") + " " + diagramLines.size() + " "
					+ Translator.swap("DiagramList.diagrams");

	}

	public String getTrace() {
		StringBuffer b = new StringBuffer("DiagramList: " + getName() + " locked: " + isLocked() + "\n");
		int k = 1;
		for (DiagramLine d : diagramLines) {
			b.append("" + (k++));
			b.append(" : " + d);
			b.append("\n");
		}
		return b.toString();
	}

	@Override
	public String toString() {
		return getName();
	}

	@Override
	public int compareTo(Object o) {
		// This will sort the diagramLists on their names in a sorted list
		return toString().compareTo(o.toString());
	}

}
