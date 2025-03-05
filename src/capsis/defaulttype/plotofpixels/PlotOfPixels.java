package capsis.defaulttype.plotofpixels;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import jeeb.lib.maps.geom.Polygon2;
import jeeb.lib.util.ListMap;
import jeeb.lib.util.Log;
import jeeb.lib.util.Spatialized;
import jeeb.lib.util.Vertex3d;
import capsis.defaulttype.Tree;
import capsis.kernel.GScene;
import capsis.kernel.Plot;

/**
 * A superclass for plots containing Pixels. A plot is a geometrical description
 * of the terrain associated to the GScene.
 * 
 * Warning: all pixels in the plot must have different ids.
 * 
 * @author F. de Coligny - November 2017
 */
public class PlotOfPixels<T extends Pixel> extends Plot<T> {

	private List<Parcel> parcels;

	// private List<PixelMap> pixelMaps;

	/**
	 * Constructor.
	 */
	public PlotOfPixels(GScene scene) {
		super();
		setScene(scene);
	}

	/**
	 * Searches and returns the pixel covering the given location or returns
	 * null if not found.
	 */
	public Pixel getPixelCovering(double x, double y) {

		Set<PixelMap> pixelMaps = getPixelMaps();

		for (PixelMap pm : pixelMaps) {
			Pixel pix = pm.getPixelCovering(x, y);
			if (pix != null)
				return pix;

		}
		return null; // not found

	}

	/**
	 * Returns a set containing all pixelMaps of all parcels with no duplicates
	 * (several parcels may share the same pixelMap, some parcels may have no
	 * pixelMap).
	 */
	public Set<PixelMap> getPixelMaps() {

		// Some parcels may share their pixelMaps, use a Map to avoid
		// duplicates with same id
		Map<Integer, PixelMap> pixelMaps = new HashMap<>();
		for (Parcel parcel : parcels) {
			if (parcel.getPixelMap() != null)
				pixelMaps.put(parcel.getPixelMap().getId(), parcel.getPixelMap());
		}

		return new HashSet(pixelMaps.values());
	}

	// Trees management: connected to Scene.addTree ()

	/**
	 * Add the given tree in the matching plot subdivision (optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public boolean addTree(Tree tree) {

		if (tree instanceof Spatialized) {
			Spatialized spa = (Spatialized) tree;

			Pixel pix = getPixelCovering(spa.getX(), spa.getY());

			// System.out.println("PlotOfPixels addTree() tree: " + tree.getId()
			// + ", found pixel: " + pix);

			if (pix != null) {
				pix.addElement(spa);

			} else {

				// Note: a subclass may attempt to add the tree elsewhere, e.g.
				// in a lateral parcel for Luberon2

				return false; // was not added in plot
			}

		}

		return true; // true: no problem (not spatialized or added)
	}

	/**
	 * Remove the given tree from the matching plot subdivision(s) (Optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public void removeTree(Tree tree) {

		if (tree instanceof Spatialized) {
			Spatialized spa = (Spatialized) tree;

			Pixel pix = getPixelCovering(spa.getX(), spa.getY());

			if (pix != null)
				pix.removeElement(spa);
		}

	}

	/**
	 * Remove all trees from their matching plot subdivision(s) (Optional).
	 */
	// fc-9.11.2017 refactored trees management in plots
	public void clearTrees() {
		Set<PixelMap> pixelMaps = getPixelMaps();

		for (PixelMap pm : pixelMaps) {
			if (pm.getPixels() != null)
				for (Pixel pix : pm.getPixels()) {
					pix.clearElements();
				}
		}

	}

	// Cloning

	public Object clone() {
		try {
			PlotOfPixels<T> plotCopy = (PlotOfPixels<T>) super.clone();

			plotCopy.parcels = null;

			if (parcels != null) {

				ListMap<PixelMap, Integer> pmToBeCloned = new ListMap<>();
				Map<Integer, Parcel> clonedParcelsMap = new HashMap<>();

				for (Parcel parcel : parcels) {

					Parcel parcelCopy = (Parcel) parcel.clone();

					// fc-28.11.2017 lateral parcels do not have a pixelMap
					if (parcel.getPixelMap() != null) {

						// PixelMaps may be shared by several Parcels, memo
						// which pixelMaps must be cloned and to which
						// parcel(s) they must be linked
						pmToBeCloned.addObject(parcel.getPixelMap(), parcel.getId()); // memo

						clonedParcelsMap.put(parcelCopy.getId(), parcelCopy); // memo
					}

					plotCopy.addParcel(parcelCopy);
				}

				// Clone the PixelMaps
				Set<PixelMap> pms = pmToBeCloned.getKeys();

				for (PixelMap pm : pms) {

					// PixelMap clone () clones the pixels
					PixelMap pmCopy = (PixelMap) pm.clone();
					pmCopy.plot = plotCopy;

					List<Integer> parcelIds = pmToBeCloned.getObjects(pm);

					for (int id : parcelIds) {

						Parcel parcelCopy = clonedParcelsMap.get(id);
						parcelCopy.pixelMap = pmCopy;

						// Set parcel ref in the cloned pixels
						for (Pixel pixCopy : parcelCopy.getPixels()) {
							pixCopy.setParcel(parcelCopy);
							pixCopy.setPlot(plotCopy);
						}

					}

				}

			}

			// ////////
			// // pixelMap clone () clones the pixels
			// if (pixelMap != null)
			// parcelCopy.pixelMap = (PixelMap) pixelMap.clone();
			//
			// // Set parcel ref in the cloned pixels
			// for (Pixel pixCopy : parcelCopy.getPixels()) {
			// pixCopy.setParcel(parcelCopy);
			// }
			// ////////

			return plotCopy;

		} catch (Exception e) {
			Log.println(Log.ERROR, "PlotOfPixels.clone ()", "Exception in clone ()", e);
			return null;
		}

	}

	/**
	 * Explores all parcels to search and return the parcel with the given id.
	 */
	public Parcel getParcel(int parcelId) {
		// fc+vf+fl-2.7.2021 Added this method
		for (Parcel parcel : parcels) {
			if (parcel.getId () == parcelId) {
				return parcel;
			}
		}
		return null; // null if not found
	}

	/**
	 * Explores all parcels to search and return the pixel with the given id.
	 */
	public Pixel getPixel(int pixelId) {
		Pixel res = null;
		for (Parcel parcel : parcels) {
			if (parcel.isPixelized()) {
				res = parcel.getPixelMap().getPixel(pixelId);
				if (res != null)
					break;
			}
		}
		return res; // null if not found
	}

	// Parcels

	public void addParcel(Parcel parcel) throws Exception {

		// Add the parcel in the plot
		if (parcels == null)
			parcels = new ArrayList<>();
		parcels.add(parcel);

		// Update plot origin, xSize and ySize
		Vertex3d origin = getOrigin();
		double xSize = getXSize();
		double ySize = getYSize();

		if (parcel.isPixelized()) {

			for (Pixel p : parcel.getPixels()) {

				Vertex3d pOrigin = new Vertex3d(p.getOrigin());

				if (origin == null) {
					origin = pOrigin;
					xSize = p.getSize();
					ySize = p.getSize();

				} else {
					origin.x = Math.min(origin.x, pOrigin.x);
					origin.y = Math.min(origin.y, pOrigin.y);
					origin.z = Math.min(origin.z, pOrigin.z);

					xSize = Math.max(xSize, origin.x + pOrigin.x + p.getSize());
					ySize = Math.max(ySize, origin.y + pOrigin.y + p.getSize());

				}
			}
		} else {
			Polygon2 polygon = parcel.getPolygon();

			Vertex3d polygonOrigin = new Vertex3d(polygon.getXmin(), polygon.getYmin(), 0d);
			double polygonXSize = polygon.getXmax() - polygon.getXmin();
			double polygonYSize = polygon.getYmax() - polygon.getYmin();

			if (origin == null) {
				origin = polygonOrigin;
				xSize = polygonXSize;
				ySize = polygonYSize;

			} else {
				origin.x = Math.min(origin.x, polygonOrigin.x);
				origin.y = Math.min(origin.y, polygonOrigin.y);
				origin.z = Math.min(origin.z, polygonOrigin.z);

				xSize = Math.max(xSize, origin.x + polygonOrigin.x + polygonXSize);
				ySize = Math.max(ySize, origin.y + polygonOrigin.y + polygonYSize);

			}

		}

		setOrigin(origin);
		setXSize(xSize);
		setYSize(ySize);

	}

	public double getPixelSize() {
		// Search the first pixelised parcel
		for (Parcel parcel : parcels) {
			if (parcel.isPixelized())
				return parcel.getPixelSize();
		}
		// Not found
		return 0;
	}

	/**
	 * Returns all parcels in this plot.
	 */
	public List<Parcel> getParcels() {
		return parcels;
	}

	// Other methods of Plot superclass

	@Override
	public void adaptSize(Spatialized t) {
		// Unused for PlotOfPixels: cancels superclass' proposal
	}

	/**
	 * Returns the bounding rectangle of the pixelMaps.
	 */
	@Override
	public Collection<Vertex3d> getVertices() {
		Vertex3d origin = getOrigin();
		double xSize = getXSize();
		double ySize = getYSize();

		Collection<Vertex3d> vs = new ArrayList<>();
		vs.add(new Vertex3d(origin.x, origin.y, 0d));
		vs.add(new Vertex3d(origin.x, origin.y + ySize, 0d));
		vs.add(new Vertex3d(origin.x + xSize, origin.y + ySize, 0d));
		vs.add(new Vertex3d(origin.x + xSize, origin.y, 0d));

		return vs;

	}

	@Override
	public void setVertices(Collection<Vertex3d> v) {
		// Unused for PlotOfPixels: cancels superclass' proposal
	}

}
