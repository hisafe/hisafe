package capsis.util.shifter;

import jeeb.lib.util.Vertex2d;
import jeeb.lib.util.Vertex3d;

/**
 * A tool to transform x,y,z coordinates to get rid of huge values, e.g. coming
 * from GIS / Lambert system.
 * 
 * @author F. de Coligny, B. Courbaud - November 2023
 */
public abstract class CoordinateShifter3d {

	protected Vertex3d v0;

	/**
	 * Constructor. Should be overriden to init the shifter by calling updateV0 ()
	 * with all x,y coordinates found in the input file loader.
	 */
	public CoordinateShifter3d() {

		// Init v0, the subclass constructor should find the min x and y of the scene by
		// calling updateV0 () for all objects with coordinates before execute () is
		// called
		v0 = new Vertex3d(Double.MAX_VALUE, Double.MAX_VALUE, Double.MAX_VALUE);

	}

	/**
	 * Updates v0 if needed to find the minimum value for all passed x and y
	 * coordinates (at construction time), can be used in a subclass constructor to
	 * init the shifter.
	 */
	protected void updateV0(double x, double y, double z) {
		v0.x = Math.min(v0.x, x);
		v0.y = Math.min(v0.y, y);
		v0.z = Math.min(v0.z, z);
	}

	/**
	 * Can be overriden to apply the shifting in all objects with coordinates
	 */
	public abstract void execute() throws Exception;

	/**
	 * File import time, calculates the shifted value for the given x.
	 */
	public double shiftX(double x) {
		return x - v0.x;
	}

	/**
	 * File import time, calculates the shifted value for the given y.
	 */
	public double shiftY(double y) {
		return y - v0.y;
	}

	/**
	 * File import time, calculates the shifted value for the given z.
	 */
	public double shiftZ(double z) {
		return z - v0.z;
	}

	/**
	 * File export time, restores the original unshifted value for the given shifted
	 * x.
	 */
	public double restoreX(double x) {
		return x + v0.x;
	}

	/**
	 * File export time, restores the original unshifted value for the given shifted
	 * y.
	 */
	public double restoreY(double y) {
		return y + v0.y;
	}

	/**
	 * File export time, restores the original unshifted value for the given shifted
	 * z.
	 */
	public double restoreZ(double z) {
		return z + v0.z;
	}

	/**
	 * File export time, restores the original unshifted vertex (the given vertex is
	 * not changed, a copy is returned).
	 */
	public Vertex3d restoreVertex3d(Vertex3d v) { // fc+bc-15.1.2024
		return new Vertex3d(restoreX(v.x), restoreY(v.y), restoreZ(v.z));

	}

	/**
	 * File export time, restores the original unshifted vertex (the given vertex is
	 * not changed, a copy is returned).
	 */
	public Vertex2d restoreVertex2d(Vertex2d v) { // fc+bc-15.1.2024
		return new Vertex2d(restoreX(v.x), restoreY(v.y));

	}

}
