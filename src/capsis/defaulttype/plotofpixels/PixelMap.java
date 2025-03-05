package capsis.defaulttype.plotofpixels;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.defaulttype.SpatializedTree;
import capsis.kernel.GScene;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;

/**
 * A PixelMap contains Pixels. This is a rectangular matrix of pixels, but some
 * pixels may be missing in the matrix.
 * 
 * @author F. de Coligny - October 2017
 */
public class PixelMap implements Serializable, Cloneable {

	// Redefinition of equals with an EPSILON
	private static class V2d implements Comparable, Serializable {

		public static double EPSILON = 0.00001;

		private double x;
		private double y;

		public V2d(double x, double y) {
			this.x = x;
			this.y = y;
		}

		@Override
		public int hashCode() {
			// fc+ed-16.3.2018 CAUTION: hashCode AND equals must consider
			// EPSILON, else the HashMap.get will not find some elements
			// fc+ed-16.3.2018, CHECKED, this works
			double invEpsilon = 1d / EPSILON;

			double xa = (double) Math.round(x * invEpsilon) / invEpsilon;
			double ya = (double) Math.round(y * invEpsilon) / invEpsilon;

			// picked in java.awt.geom.Point2D
			long bits = java.lang.Double.doubleToLongBits(xa);
			bits ^= java.lang.Double.doubleToLongBits(ya) * 31;
			return (((int) bits) ^ ((int) (bits >> 32)));

			// return (int) (x * 1000000 + y);
		}

		@Override
		public boolean equals(Object other) {
			// fc+ed-16.3.2018 CAUTION: hashCode AND equals must consider
			// EPSILON, else the HashMap.get will not find some elements
			// fc+ed-16.3.2018, CHECKED, this works
			if (!(other instanceof V2d))
				return false;

			V2d o = (V2d) other;

			// fc+ed-15.3.2018
			double v1 = Math.abs(o.x - x);
			double v2 = Math.abs(o.y - y);
			boolean res = v1 < EPSILON && v2 < EPSILON;
			// System.out.println("PixelMap V2d equals v1: "+v1+" v2: "+v2+"
			// EPSILON: "+EPSILON+" res: "+res);
			return res;
		}

		@Override
		public String toString() {
			return "V2d (" + x + ", " + y + ")";
		}

		@Override
		public int compareTo(Object o) {
			V2d other = (V2d) o;

			if (x < other.x)
				return -1;
			else if (x > other.x)
				return 1;
			else {
				if (y < other.y)
					return -1;
				else if (y > other.y)
					return 1;
				else
					return 0;
			}

		}

	}

	private int id;

	protected GScene scene; // requested

	// plot may be null
	protected PlotOfPixels plot;

	// All pixels have same size
	private double pixelSize; // m

	private Map<Integer, Pixel> map;

	// PixelMap bounding rectangle
	private double xMin;
	private double xMax;
	private double yMin;
	private double yMax;

	// Topology, used for neighbourhoods
	// [line][column], line in [0, nLin[, col in [O, nCol[
	// This 2 dims array is rectangular
	// If an id in the array is 0, this means there is no Pixel at this
	// location (Pixel ids are strictly positive)
	private int[][] pixelIdMatrix;
	private int nLin;
	private int nCol;

	// Working map for getPixel(x, y)
	private Map<V2d, Integer> center2id;

	/**
	 * Constructor 1, preferred. Plot may be null.
	 */
	public PixelMap(int id, GScene scene, PlotOfPixels plot, double pixelSize, List<Pixel> pixels) {
		this(id, scene, plot, pixelSize);
		init(pixels);
	}

	/**
	 * Constructor 2, with deferred init (). Plot may be null.
	 */
	public PixelMap(int id, GScene scene, PlotOfPixels plot, double pixelSize) {

		super();

		this.id = id;
		this.plot = plot; // may be null.
		this.scene = scene; // requested

		map = new HashMap<>();
		this.pixelSize = pixelSize;

		// init() must be called by caller
	}

	public void init(List<Pixel> pixels) {

		xMin = Double.MAX_VALUE;
		xMax = -Double.MAX_VALUE;
		yMin = Double.MAX_VALUE;
		yMax = -Double.MAX_VALUE;
		double hs = getHalfPixelSize(); // half size

		int maxIGrid = 0;
		int maxJGrid = 0;

		// Store pixels in the Map
		for (Pixel pix : pixels) {

			pix.setPixelMap(this);
			map.put(pix.getId(), pix);

			xMin = Math.min(xMin, pix.getCenter().x - hs);
			xMax = Math.max(xMax, pix.getCenter().x + hs);
			yMin = Math.min(yMin, pix.getCenter().y - hs);
			yMax = Math.max(yMax, pix.getCenter().y + hs);

			maxIGrid = Math.max(maxIGrid, pix.getIGrid());
			maxJGrid = Math.max(maxJGrid, pix.getJGrid());
		}

		// Create the pixelMatrix
		nLin = maxIGrid + 1;
		nCol = maxJGrid + 1;
		this.pixelIdMatrix = new int[nLin][nCol];

		// System.out.println("PixelMap init(), nLin: " + nLin + " nCol: " +
		// nCol + " matrix size: " + nLin * nCol);
		// System.out.println("PixelMap init(), storing pixels, #: "+pixels.size
		// ());

		// Store the pixel ids in the matrix
		for (Pixel p : pixels) {
			pixelIdMatrix[p.getIGrid()][p.getJGrid()] = p.getId();
		}

	}

	/**
	 * The pixels may sometimes be translated, this reset method refreshes the
	 * pixelMap accordingly to their new locations.
	 */
	public void reset() {
		// will be recreated at next getPixelCovering () call
		center2id = null;

		xMin = Double.MAX_VALUE;
		xMax = -Double.MAX_VALUE;
		yMin = Double.MAX_VALUE;
		yMax = -Double.MAX_VALUE;
		double hs = getHalfPixelSize(); // half size

		// Store pixels in the Map
		for (Pixel pix : map.values()) {

			xMin = Math.min(xMin, pix.getCenter().x - hs);
			xMax = Math.max(xMax, pix.getCenter().x + hs);
			yMin = Math.min(yMin, pix.getCenter().y - hs);
			yMax = Math.max(yMax, pix.getCenter().y + hs);

		}

	}

	public GScene getScene() {
		return scene;
	}

	public int getId() {
		return id;
	}

	// Cloning

	public Object clone() {
		try {

			PixelMap pmCopy = (PixelMap) super.clone();

			pmCopy.plot = null; // will be set by caller

			// Calling init () would pmCopy the pixelIdMatrix, we prefer sharing
			// it with the cloned instance (contains ids, it is the same)
			// pmCopy.init(pixelList); // Nope

			// Clone the pixels, ** they are returned empty **
			pmCopy.map = new HashMap<>();
			for (Pixel pix : getPixels()) {

				Pixel pixCopy = (Pixel) pix.clone();
				pixCopy.setPixelMap(pmCopy);

				pmCopy.map.put(pixCopy.getId(), pixCopy);

			}

			// center2id is kept, same than the cloned instance (contains ids)

//			if (getId() == 1)
//				System.out.println("PixelMap.clone ()");

			return pmCopy;

		} catch (Exception e) {
			Log.println(Log.ERROR, "PixelMap.clone ()", "Exception in clone ()", e);
			return null;
		}

	}

	public int getnLin() {
		return nLin;
	}

	public int getnCol() {
		return nCol;
	}

	public double getPixelSize() {
		return pixelSize;
	}

	public double getHalfPixelSize() {
		return pixelSize / 2d;
	}

	// fc-7.11.2017 was 'public Collection<? extends Pixel> getPixels()' but
	// with
	// 'Lub2Pixel extends Pixel', could not compile the following:
	// 'for (Lub2Pixel pixel : pixelMap.getPixels())', got an error: 'Type
	// mismatch: cannot convert from element type capture#4-of ? extends Pixel
	// to Lub2Pixel', I understand the error but there is no interest in using
	// such a complicated generics signature if we can not use it to write the
	// fast way loop. So I simplified the method signature
	public Collection<Pixel> getPixels() {
		return map.values();
	}

	public Pixel getPixel(int id) {
		return map.get(id);
	}

	private V2d getPixelCenter(double x, double y) {
		double hs = getHalfPixelSize(); // half size

		// Calc the center x and y of the Pixel that would cover x, y
		double cx = xMin + hs + ((int) ((x - xMin) / pixelSize)) * pixelSize;
		double cy = yMin + hs + ((int) ((y - yMin) / pixelSize)) * pixelSize;

		return new V2d(cx, cy);
	}

	/**
	 * Returns the Pixel covering the given location or null if no pixel found.
	 */
	public Pixel getPixelCovering(double x, double y) {

		// see RectangularPlot.getCell (x, y)

		if (center2id == null) {
			center2id = new HashMap<>();
			for (Pixel p : getPixels()) {
				V2d v2 = new V2d(p.getCenter().x, p.getCenter().y);
				center2id.put(v2, p.getId());

			}
		}

		V2d centerMatchingXY = getPixelCenter(x, y);

		// id may be null if no pixel covers x,y
		Integer id = center2id.get(centerMatchingXY);

		Pixel p = null;
		if (id != null)
			p = getPixel(id); // p may still be null

//		if (p == null) {
//			System.out.println("PixelMap error, getPixelCovering (" + x + ", " + y
//					+ "): null (could not find a pixel covering x, y)");
//			System.out.println("PixelMap error, searched a pixel with center: (" + centerMatchingXY.x + ", "
//					+ centerMatchingXY.y + ")");
//			System.out.println("PixelMap: " + traceCenter2id());
//			System.out.println("PixelMap: center2id.contains (40.0, 12.776085000000002): "
//					+ center2id.containsKey(new V2d(40.0, 12.776085000000002)));
//			System.out.println("PixelMap: center2id.contains (45.00000000000001, 12.776085000000002): \"\n"
//					+ center2id.containsKey(new V2d(45.00000000000001, 12.776085000000002)));
//			System.out.println(
//					"PixelMap: center2id.contains (45, 12.77): \"\n" + center2id.containsKey(new V2d(45, 12.77)));
//		}

		return p;

	}

	/**
	 * Returns the Pixel at location (i, j) in the matrix, or null if no Pixel
	 * found.
	 */
	public Pixel getPixel(int i, int j) {

		int a = i % nLin;
		int b = j % nCol;

		// i,j is outside matrix
		if (i < 0 || j < 0 || i >= nLin || j >= nCol)
			return null;

		int id = pixelIdMatrix[a][b];

		// trace
		// System.out.println("pm.getPixel (" + i + ", " + j + "): " +
		// map.get(id));

		return map.get(id);

	}

	public PlotOfPixels getPlot() {
		return plot;
	}

	public static void main(String[] args) {
		test();
	}

	public static void test() {
		try {

			// A tree class for the test
			class SmallTree extends SpatializedTree {
				public SmallTree(int id, double x, double y) {
					super(id, null, 0, 0, 0, false, x, y, 0);
				}
			}

			double pixelSize = 10;

			List<Pixel> pixels = new ArrayList<>();

			Pixel p1 = (new Pixel(1, 0, 2, 35.1, 35.2, 0, null));
			p1.addElement(new SmallTree(11, 31, 31));
			p1.addElement(new SmallTree(12, 32, 32));
			p1.addElement(new SmallTree(13, 35.1, 35.2));
			pixels.add(p1);

			pixels.add(new Pixel(2, 0, 3, 45.1, 35.2, 0, null));
			pixels.add(new Pixel(3, 1, 0, 15.1, 25.2, 0, null));

			Pixel p4 = (new Pixel(4, 1, 1, 25.1, 25.2, 0, null));
			p4.addElement(new SmallTree(41, 29.1, 29.2));
			p4.addElement(new SmallTree(42, 20.1, 20.2));
			p4.addElement(new SmallTree(43, 25.1, 25.2));
			pixels.add(p4);

			pixels.add(new Pixel(5, 1, 2, 35.1, 25.2, 0, null));
			pixels.add(new Pixel(6, 1, 3, 45.1, 25.2, 0, null));
			pixels.add(new Pixel(22, 2, 1, 25.1, 15.2, 0, null));

			PlotOfPixels plot = null;
			int pmId = 1;
			PixelMap pixelMap = new PixelMap(pmId, null, plot, pixelSize, pixels);

			System.out.println("PixelMap test ()...\n" + pixelMap);

			// getPixel (i, j)
			pixelMap.getPixel(0, 0);
			pixelMap.getPixel(0, 1);
			pixelMap.getPixel(0, 2);
			pixelMap.getPixel(0, 3);
			pixelMap.getPixel(1, 0);
			pixelMap.getPixel(1, 1);
			pixelMap.getPixel(1, 2);
			pixelMap.getPixel(1, 3);
			pixelMap.getPixel(2, 0);
			pixelMap.getPixel(2, 1);
			pixelMap.getPixel(2, 2);
			pixelMap.getPixel(2, 3);
			pixelMap.getPixel(12, 101);

			// getPixelCovering (x, y)
			pixelMap.getPixelCovering(35.1, 35.2);
			pixelMap.getPixelCovering(30.1, 30.2);
			pixelMap.getPixelCovering(31, 31);
			pixelMap.getPixelCovering(40.1, 30.2);
			pixelMap.getPixelCovering(15, 35);
			pixelMap.getPixelCovering(22, 13);
			pixelMap.getPixelCovering(60, 60);
			pixelMap.getPixelCovering(-1, -12);

			// Testing neighbours
			double radius = 8.49; // m
			PixelRoundMask m = new PixelRoundMask(pixelMap, radius);

			boolean includingXYElement = true;
			List<Spatialized> neighbours = m.getElementsNear(35.1, 35.2, includingXYElement);

			System.out.println("neighbours: " + AmapTools.toString(neighbours));
			System.out.println("distances: " + AmapTools.toString(m.getElementDistances()));

		} catch (Exception e) {
			System.out.println("PixelMap test () error");
			e.printStackTrace(System.out);
		}

	}

	// fc+ed-15.3.2018
	public String traceCenter2id() {
		StringBuffer b = new StringBuffer("PixelMap id: " + id + " center2id: \n");
		for (V2d key : center2id.keySet()) {

			Integer pixelId = center2id.get(key);

			b.append("" + key + " -> " + pixelId + "\n");
		}
		return b.toString();
	}

	@Override
	public String toString() {
		return "PixelMap pixelSize: " + pixelSize + ", xMin/xMax/yMin/yMax: " + xMin + "/" + xMax + "/" + yMin + "/"
				+ yMax + ", nPixels: " + map.size();
	}

}
