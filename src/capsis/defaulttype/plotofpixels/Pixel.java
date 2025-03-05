package capsis.defaulttype.plotofpixels;

import java.awt.Rectangle;
import java.awt.Shape;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;
import capsis.kernel.Plot;
import capsis.kernel.PlotSubdivision;

/**
 * A Pixel cell in a PixelMap / PixelPlot. A PixelPlot may contain several
 * PixelMaps. A Pixel may contain Spatialized elements, e.g. trees.
 * 
 * @author F. de Coligny - October/November 2017
 */
public class Pixel implements PlotSubdivision, Cloneable {

	// Note fc-7.11.2017
	// class definition formerly was :
	// public class Pixel<T extends Spatialized> implements PlotSubdivision
	// but difficult problems then arised in use of this class, in particular
	// when writing
	// for (Lub2Pixel pixel : pixelMap.getPixels()) {, we got an error like
	// this:
	// Type mismatch: cannot convert from element type capture#4-of ? extends
	// Pixel to Lub2Pixel. I do not have time to play with this, so I decided to
	// simplify and use less generics.

	private int id;

	private int iGrid;
	private int jGrid;

	private Vertex3d center;

	private Parcel parcel; // fc-17.11.2017
	private PixelMap pixelMap;

	// The pixel contains elements: trees or other spatialized elements
	private List<Spatialized> elements;

	/**
	 * Constructor
	 */
	public Pixel(int id, int iGrid, int jGrid, double xCenter, double yCenter, double zCenter, Parcel parcel) throws Exception {
		this(id, iGrid, jGrid, new Vertex3d(xCenter, yCenter, zCenter), parcel);
	}

	/**
	 * Constructor
	 */
	public Pixel(int id, int iGrid, int jGrid, Vertex3d center, Parcel parcel) throws Exception {

		if (id <= 0)
			throw new Exception("Could not create a Pixel: wrong id: " + id + ", id must be strictly positive");
		if (iGrid < 0)
			throw new Exception("Could not create a Pixel: wrong iGrid: " + iGrid + ", iGrid must be positive");
		if (jGrid < 0)
			throw new Exception("Could not create a Pixel: wrong jGrid: " + jGrid + ", jGrid must be positive");
		if (center == null)
			throw new Exception("Could not create a Pixel: wrong center: " + center);

		this.id = id;
		this.iGrid = iGrid;
		this.jGrid = jGrid;
		this.center = center;
		this.parcel = parcel;

	}

	public Object clone() {
		try {
			Pixel copy = (Pixel) super.clone();

			// parcel and pixelMap need to be updated by caller
			copy.parcel = null;
			copy.pixelMap = null;

			// the cloned instance is returned with no elements inside
			copy.elements = null;
			
//			if (getId () == 1) System.out.println("Pixel.clone ()");

			return copy;
			
		} catch (Exception e) {
			Log.println(Log.ERROR, "Pixel.clone ()", "Exception in clone ()", e);
			return null;
		}
	}

	public void setParcel(Parcel parcel) {
		// used in parcel clone (à method
		this.parcel = parcel;
	}
	
	public Parcel getParcel() {
		return parcel;
	}

	public void setPixelMap(PixelMap pixelMap) {
		this.pixelMap = pixelMap;
	}
	
	public PixelMap getPixelMap() {
		return pixelMap;
	}

	public int getId() {
		return id;
	}

	public int getIGrid() {
		return iGrid;
	}

	public int getJGrid() {
		return jGrid;
	}

	public Vertex3d getCenter() {
		return center;
	}

	public double getSize() {
		return pixelMap.getPixelSize();
	}

	public double getArea() {
		return getSize() * getSize();
	}

	public boolean contains(Spatialized e) {
		double hSize = pixelMap.getHalfPixelSize();
		return e.getX() >= center.x - hSize && e.getX() < center.x + hSize && e.getY() >= center.y - hSize
				&& e.getY() < center.y + hSize;
	}

	/**
	 * Distance from this pixel to the given pixel.
	 */
	public double distance(Pixel other) {

		double v0 = center.x - other.center.x;
		double v1 = center.y - other.center.y;

		return Math.sqrt(v0 * v0 + v1 * v1);
	}

	// The pixel contains spatialized elements
	public void addElement(Spatialized e) {
		if (elements == null)
			elements = new ArrayList<>();
		elements.add(e);
	}

	// The pixel contains spatialized elements
	public void removeElement(Spatialized e) {
		if (elements != null)
			elements.remove(e);
	}

	// Remove all elements
	public void clearElements() {
		elements = null;
	}

	// The pixel contains spatialized elements
	public List<Spatialized> getElements() {
		return elements;
	}

	public boolean isEmpty() {
		return getElements() == null || getElements().isEmpty();

	}

	@Override
	public String toString() {
		String content = elements == null ? "empty" : "" + elements.size() + " elements";
		return "Pixel id: " + id + ", iGrid/jGrid: " + iGrid + "/" + jGrid + ", center: " + center + " content: "
				+ content;
	}

	// PlotSubdivision interface

	private Plot plot;

	@Override
	public Plot getPlot() {
		return plot;
	}

	@Override
	public void setPlot(Plot plot) {
		this.plot = plot;
	}

	@Override
	public Vertex3d getOrigin() {
		return new Vertex3d(center.x - pixelMap.getHalfPixelSize(), center.y - pixelMap.getHalfPixelSize(), center.z);
	}

	@Override
	public Collection<Vertex3d> getVertices() {
		Collection<Vertex3d> vs = new ArrayList<>();
		vs.add(lowerLeft());
		vs.add(upperLeft());
		vs.add(upperRight());
		vs.add(lowerRight());
		return vs;

	}

	@Override
	public Shape getShape() {
		Vertex3d origin = getOrigin();
		return new Rectangle.Double(origin.x, origin.y, getSize(), getSize());
	}

	@Override
	public void setOrigin(Vertex3d v) {
		// unused for Pixel
	}

	@Override
	public void setVertices(Collection<Vertex3d> c) {
		// unused for Pixel
	}

	@Override
	public void setArea(double a) {
		// unused for Pixel
	}

	@Override
	public String bigString() {
		return toString(); // unused for Pixel
	}

	// Other methods

	private Vertex3d lowerLeft() {
		return new Vertex3d(center.x - pixelMap.getHalfPixelSize(), center.y - pixelMap.getHalfPixelSize(), center.z);
	}

	private Vertex3d upperLeft() {
		return new Vertex3d(center.x - pixelMap.getHalfPixelSize(), center.y + pixelMap.getHalfPixelSize(), center.z);
	}

	private Vertex3d upperRight() {
		return new Vertex3d(center.x + pixelMap.getHalfPixelSize(), center.y + pixelMap.getHalfPixelSize(), center.z);
	}

	private Vertex3d lowerRight() {
		return new Vertex3d(center.x + pixelMap.getHalfPixelSize(), center.y - pixelMap.getHalfPixelSize(), center.z);
	}

}
