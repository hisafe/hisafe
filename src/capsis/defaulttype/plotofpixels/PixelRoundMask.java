package capsis.defaulttype.plotofpixels;

import java.awt.Point;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import capsis.defaulttype.plotofcells.RoundMask;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex2d;

/**
 * A round mask to get neighbour elements (e.g. tree) of a given location at a
 * given distance in a PixelMap.
 * 
 * @see capsis.defaulttype.plotofcells.RoundMask
 * @author F. de Coligny - October/November 2017
 */
public class PixelRoundMask {

	// For pixel selection, see getNeighbourPixels ()
	public static final byte PARTIALLY_INCLUDED = 0;
	public static final byte CENTER_INCLUDED = 1;
	public static final byte COMPLETELY_INCLUDED = 2;

	protected PixelMap pixelMap;
	protected double radius;
	protected double pixelSize; // m

	protected Collection<Point> sites; // relative coordinates (Points)

	protected Map<Spatialized, Double> elementDistances; // m
	// Angle (cw, deg) between the positive Y axis (O deg) and the the neighbour
	// element (angle with the positive X axis is 90)
	protected Map<Spatialized, Double> elementAngles; // deg

	protected Map<Pixel, Double> pixelDistances; // m

	/**
	 * Constructor
	 */
	public PixelRoundMask(PixelMap pixelMap, double radius) {

		this.pixelMap = pixelMap;
		this.radius = radius;
		this.pixelSize = pixelMap.getPixelSize();

		elementDistances = new HashMap<>();
		elementAngles = new HashMap<>();
		pixelDistances = new HashMap<>();

		// Compute size in pixels around the target pixel
		// according to radius and pixel size
		double s = radius / pixelSize;
		int size = (int) s;
		if (size < s)
			size++;

		// Build a square site collection, getElementsNear will
		// rely on it
		for (int i = -size; i <= size; i++) {
			for (int j = -size; j <= size; j++) {
				addSite(new Point(i, j));
			}
		}

	}

	/**
	 * Returns the pixels around the given pixel according to the mask sites.
	 */
	public List<Pixel> getNeighbours(Pixel pixel, boolean onlyPixelsWithElementsInside) {
		List<Pixel> neighbours = new ArrayList<>();

		for (Point site : getSites()) {

			int iDec = site.x;
			int jDec = site.y;

			int iGrid = pixel.getIGrid();
			int jGrid = pixel.getJGrid();

			Pixel cPixel = pixelMap.getPixel(iGrid + iDec, jGrid + jDec);

			// Outside matrix or hole in matrix
			if (cPixel == null)
				continue; // jump to next site

			if (onlyPixelsWithElementsInside && cPixel.isEmpty())
				continue; // jump to next site

			neighbours.add(cPixel);
		}

		return neighbours;
	}

	/**
	 * Returns the list of elements at the distance 'radius' of the given (x,y),
	 * including or not an element that would be at this location. Based on the
	 * Pixels around the Pixel covering the given location, and on the PixelMap
	 * containing these pixels, using the topology of the Pixel matrix.
	 */
	public List<Spatialized> getElementsNear(double x, double y, boolean includingXYElement) throws Exception {
		List<Spatialized> elements = new ArrayList<>();

		elementDistances = new HashMap<>();
		elementAngles = new HashMap<>();

		Pixel targetPixel = pixelMap.getPixelCovering(x, y);

		if (targetPixel == null) {

			Log.println(Log.ERROR, "PixelRoundMask.getElementsNear (" + x + ", " + y + ")",
					"error, could not find a pixel covering point, aborted");
			Log.println(pixelMap.traceCenter2id());

			throw new Exception("PixelRoundMask.getElementsNear (): error, could not find a pixel covering point (" + x
					+ ", " + y + "), aborted");
		}

		boolean onlyPixelsWithElementsInside = true;
		List<Pixel> cPixels = getNeighbours(targetPixel, onlyPixelsWithElementsInside);

		if (cPixels == null || cPixels.isEmpty())
			return elements; // empty list

		for (Pixel cPixel : cPixels) {

			List<Spatialized> elementsInCPixel = cPixel.getElements();

			for (Spatialized t : elementsInCPixel) {

				if (t.getX() == x && t.getY() == y && !includingXYElement)
					continue; // ignore an element accurately located in (x, y)

				double distance = Math.sqrt(Math.pow(t.getX() - x, 2) + Math.pow(t.getY() - y, 2));

				if (distance <= radius) {
					elements.add(t);

					elementDistances.put(t, distance);

					// The returned angle is in [0,360]
					double diffX = t.getX() - x;
					double diffY = t.getY() - y;
					double angle = Math.atan2(diffX, diffY);
					double angle_deg = Math.toDegrees(angle);
					if (angle_deg < 0)
						angle_deg += 360;

					elementAngles.put(t, angle_deg);

				}

			}

		}

		return elements;
	}

	/**
	 * Returns the pixels "near" the given position. Criterion is either
	 * PARTIALLY_INCLUDED, CENTER_INCLUDED, or COMPLETELY_INCLUDED. If
	 * PARTIALLY_INCLUDED, the pixels are returned if they intersect the mask
	 * circle (approximatively: few extra pixels may be returned with this
	 * option), if CENTER_INCLUDED, the pixels are returned if their center is
	 * included in the mask circle (accurate), if COMPLETELY_INCLUDED, the
	 * pixels are returned if they are completely in the mask circle (accurate).
	 */
	public List<Pixel> getPixelsNear(Pixel pixel, byte criterion) {
		return getPixelsNear(pixel.getCenter().x, pixel.getCenter().y, criterion);
	}

	public List<Pixel> getPixelsNear(double x, double y, byte criterion) {
		List<Pixel> cPixels = new ArrayList<>();
		pixelDistances.clear();

		Pixel targetPixel = pixelMap.getPixelCovering(x, y);

		boolean onlyCellsWithTreesInside = false;
		Collection<Pixel> neighbours = getNeighbours(targetPixel, onlyCellsWithTreesInside);

		for (Pixel cPixel : neighbours) {

			// Outside matrix or hole in matrix
			if (cPixel == null)
				continue; // jump to next pixel

			double x1 = cPixel.getCenter().x;
			double y1 = cPixel.getCenter().y;

			double distanceToCenter = Math.sqrt(Math.pow(x1 - x, 2) + Math.pow(y1 - y, 2));
			double halfDiagonal = Math.sqrt(2) * cPixel.getSize() / 2d;

			// For PARTIALLY_INCLUDED, this distance works better (few extra
			// cells may be included)
			double distanceMin = distanceToCenter - halfDiagonal;

			// For COMPLETELY_INCLUDED, test that all the vertices of the square
			// are in the circle
			double distanceMax = distanceMax(x, y, cPixel);

			double distance = 0;
			if (criterion == RoundMask.PARTIALLY_INCLUDED) {
				distance = distanceMin;
			} else if (criterion == RoundMask.CENTER_INCLUDED) {
				distance = distanceToCenter;
			} else if (criterion == RoundMask.COMPLETELY_INCLUDED) {
				distance = distanceMax;
			}

			if (distance <= radius) {
				cPixels.add(cPixel);
				pixelDistances.put(cPixel, distance);
			}
		}

		return cPixels;
	}

	private double distanceMax(double x, double y, Pixel pixel) {
		double d = -Double.MAX_VALUE;
		for (Object o : pixel.getVertices()) {
			Vertex2d v2 = new Vertex2d((Vertex2d) o);
			double distanceToThisVertex = Math.sqrt(Math.pow(v2.x - x, 2) + Math.pow(v2.y - y, 2));
			d = Math.max(d, distanceToThisVertex);
		}
		return d;
	}

	/**
	 * Returns the area of the round mask, m2
	 */
	public double getArea() {
		return Math.PI * radius * radius; // m2
	}

	public void addSite(Point p) {
		if (sites == null)
			sites = new ArrayList<Point>();
		sites.add(p);
	}

	public Collection<Point> getSites() {
		return sites;
	}

	public void removeSite(Point p) {
		sites.remove(p);
	}

	public Map<Spatialized, Double> getElementDistances() {
		return elementDistances;
	}

	public Map<Spatialized, Double> getElementAngles() {
		return elementAngles;
	}

	@Override
	public String toString() {
		return "PixelRoundMask radius: " + radius;
	}

}
