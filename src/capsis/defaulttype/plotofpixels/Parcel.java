package capsis.defaulttype.plotofpixels;

import java.awt.Shape;
import java.awt.geom.GeneralPath;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import jeeb.lib.maps.geom.Point2;
import jeeb.lib.maps.geom.Polygon2;
import jeeb.lib.util.Log;
import capsis.defaulttype.SceneConnected;
import capsis.defaulttype.ScenePart;
import capsis.util.Polygon2D;

/**
 * A Parcel in a PlotOfPixels. The plot contains parcels. Some parcels may be
 * pixelized, some others not. A Pixel may contain Spatialized elements, e.g.
 * trees. The pixelized parcels have a PixelMap for neighbours searching. Each
 * pixelized PArcel can have its own pixelMap (separate parcels), or several
 * parcels may share the same pixelMap (so neighbours can be found in several
 * parcels sharing the same pixelMap).
 * 
 * @author F. de Coligny - October/November 2017
 */
public abstract class Parcel implements ScenePart, Serializable, Cloneable {

	private int id;
	private String name;
	private Polygon2 polygon;

	protected PixelMap pixelMap;
	private List<Integer> pixelIds;

	/**
	 * Constructor.
	 */
	public Parcel(int id, String name) {
		this.id = id;
		this.name = name;
	}

	// Cloning

	public Object clone() {
		try {
			Parcel parcelCopy = (Parcel) super.clone();

			parcelCopy.polygon = new Polygon2(polygon);

			if (pixelIds != null)
				parcelCopy.pixelIds = new ArrayList<>(pixelIds);

			// fc-27.11.2017 MOVED to PlotOfPixels.clone ()
//			// pixelMap clone () clones the pixels
//			if (pixelMap != null)
//				parcelCopy.pixelMap = (PixelMap) pixelMap.clone();
//
//			// Set parcel ref in the cloned pixels
//			for (Pixel pixCopy : parcelCopy.getPixels()) {
//				pixCopy.setParcel(parcelCopy);
//			}

//			if (getId () == 1) System.out.println("Parcel.clone ()");
			
			return parcelCopy;

		} catch (Exception e) {
			Log.println(Log.ERROR, "Parcel.clone ()", "Could not clone Parcel " + id, e);
			return null;
		}
	}

	public void setPolygon(Polygon2 polygon) {
		this.polygon = polygon;
	}

	public Polygon2 getPolygon() {
		return polygon;
	}

	public PixelMap getPixelMap() {
		return pixelMap;
	}

	/**
	 * Method to add pixels in the parcel. The pixels must already have their
	 * pixelMap set.
	 */
	public void addPixels(List<Pixel> pixels) throws Exception {
		for (Pixel p : pixels)
			addPixel(p);
	}

	/**
	 * Add the given pixel in the parcel. The pixel must already have its
	 * pixelMap set. We keep the pixel id in its pixelMap.
	 */
	public void addPixel(Pixel pixel) throws Exception {

		if (pixel == null)
			throw new Exception("Parcel error: could not add a null Pixel in the Parcel " + id);

		if (pixel.getPixelMap() == null)
			throw new Exception("Parcel error: could not add the Pixel " + pixel.getId() + " in the Parcel " + id
					+ ", pixelMap is required in pixel");

		// First pixel
		if (pixelMap == null)
			pixelMap = pixel.getPixelMap();

		if (pixelIds == null)
			pixelIds = new ArrayList<>();

		pixelIds.add(pixel.getId());

	}

	public List<Pixel> getPixels() {
		List<Pixel> pixels = new ArrayList<>();

		if (pixelIds == null)
			return pixels;

		for (int id : pixelIds) {

			if (pixelMap.getPixel(id) == null) {
				System.out.println("Parcel error in getPixel(): parcel " + getId() + ", could not find pixel " + id
						+ "-> pixel == null");
			}

			pixels.add(pixelMap.getPixel(id));
		}
		return pixels;
	}

	public double getPixelSize() {
		return pixelMap == null ? 0 : pixelMap.getPixelSize();
	}

	public boolean isPixelized() {
		return pixelMap != null;
	}

	public int getId() {
		return id;
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public int[] getRGB() {
		return new int[] { 170, 200, 233 };
	}

	@Override
	public double getArea_m2() {
		// Default method: can be redefined to answer differently
		return polygon.getPositiveArea();
	}

	@Override
	public List<Polygon2> getPolygons() {

		// Add the parcel's polygon
		List<Polygon2> list = new ArrayList<>();
		list.add(polygon);

		return list;
	}

	public double getXMin () {
		return polygon.getXmin();
	}
	
	public double getXMax () {
		return polygon.getXmax();
	}
	
	public double getYMin () {
		return polygon.getYmin ();
	}
	
	public double getYMax () {
		return polygon.getYmax ();
	}
	
	public boolean contains (double x, double y) {
		return polygon.contains(new Point2 (x, y));
	}
	
	@Override
	public Collection getObjectsInPart() {
		// This can be enhanced, see superclass comment
		return Collections.EMPTY_LIST;
	}

	public String get3FirstPixelIds() {
		if (pixelIds == null)
			return " pixelIds is null";
		String s = "";
		if (pixelIds.size() >= 1)
			s += " " + pixelIds.get(0);
		if (pixelIds.size() >= 2)
			s += " " + pixelIds.get(1);
		if (pixelIds.size() >= 3)
			s += " " + pixelIds.get(2);
		return s;
	}

	public Shape getShape() {
		return getShape(this.getPolygon());
	}

	/**
	 * Turns a polygon2 into a Shape
	 */
	static public Shape getShape(Polygon2 polygon) {
		List<Point2> points = polygon.getPoints();

		int n = points.size();

		double[] xs = new double[n];
		double[] ys = new double[n];

		int k = 0;
		for (Point2 p : points) {
			xs[k] = p.getX();
			ys[k] = p.getY();
			k++;
		}
		GeneralPath path = Polygon2D.getGeneralPath(xs, ys, n);

		return path;
	}

	public String toString() {
		return "Parcel " + name + "#pixels: " + getPixels().size() + " first pixelsIds: " + get3FirstPixelIds() + "...";
	}
}
