package capsis.gui.selectordiagramlist;

import java.awt.Rectangle;
import java.util.StringTokenizer;

import jeeb.lib.util.Translator;
import jeeb.lib.util.extensionmanager.ExtensionManager;

/**
 * A diagram light description: className + type. ClassName of a DataExtractor
 * or a StandViewer. Type: CapsisExtensionManager.DATA_EXTRACTOR or
 * CapsisExtensionManager.STAND_VIEWER.
 * 
 * @author F. de Coligny - December 2015
 */
public class DiagramLine implements Comparable {

	private String className;
	// Type is CapsisExtensionManager.DATA_EXTRACTOR or
	// CapsisExtensionManager.STAND_VIEWER
	private String type;
	// Bounds of the diagram
	private Rectangle bounds;
	// Optional, for DATA_EXTRACTOR type, possible to add the renderer className
	private String rendererClassName;

	/**
	 * Constructor.
	 */
	public DiagramLine(String className, String type, String encodedBounds) {
		this(className, type, decodeBounds(encodedBounds));
	}

	/**
	 * Constructor 2.
	 */
	public DiagramLine(String className, String type, Rectangle bounds) {
		this.className = className;
		this.type = type;
		this.bounds = bounds;
	}
	
	public String getClassName() {
		return className;
	}

	public String getType() {
		return type;
	}

	public Rectangle getBounds() {
		return bounds;
	}

	public String getEncodedBounds() {
		return encodeBounds(bounds);
	}

	/**
	 * This method makes it possible to sort the diagramLines in lines and
	 * columns. It is used to restore the original order of the diagrams when a
	 * diagramList is restored.
	 */
	public int getRank() {
		return bounds.y * 1000000 + bounds.x;
	}

	public void setRendererClassName(String rendererClassName) {
		this.rendererClassName = rendererClassName;
	}
	
	public String getRendererClassName() {
		return rendererClassName;
	}

	/**
	 * For original diagrams order restoration.
	 */
	@Override
	public int compareTo(Object o) {
		DiagramLine other = (DiagramLine) o;
		// Returns a positive value if this object is 'greater'
		// Returns a negative value if this object is 'smaller'
		// Returns 0 if 'equal'
		return getRank() - other.getRank();
	}

	/**
	 * Encodes the given Rectangle in a String: x y w h.
	 */
	static private String encodeBounds(Rectangle r) {
		StringBuffer b = new StringBuffer();
		b.append((int) Math.round(r.getX()));
		b.append(' ');
		b.append((int) Math.round(r.getY()));
		b.append(' ');
		b.append((int) Math.round(r.getWidth()));
		b.append(' ');
		b.append((int) Math.round(r.getHeight()));
		return b.toString();
	}

	/**
	 * Decodes the given String: x y w h into a Rectangle.
	 */
	static private Rectangle decodeBounds(String encodedBounds) {
		StringTokenizer t = new StringTokenizer(encodedBounds, " ");
		int x = Integer.parseInt(t.nextToken());
		int y = Integer.parseInt(t.nextToken());
		int w = Integer.parseInt(t.nextToken());
		int h = Integer.parseInt(t.nextToken());
		Rectangle r = new Rectangle(x, y, w, h);
		return r;
	}

	@Override
	public String toString() {
		StringBuffer b = new StringBuffer();

		b.append(ExtensionManager.getName(className));
		b.append(" (" + Translator.swap(type) + ")");

		return b.toString();
	}

}
