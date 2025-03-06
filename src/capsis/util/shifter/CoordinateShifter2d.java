package capsis.util.shifter;

import java.util.Iterator;

import jeeb.lib.util.Log;
import jeeb.lib.util.Record;
import jeeb.lib.util.Vertex2d;


/**
 * A tool to transform x,y coordinates to get rid of huge values, e.g. coming
 * from GIS / Lambert system.
 * 
 * @author F. de Coligny - September 2022
 */
public abstract class CoordinateShifter2d {

	protected Vertex2d v0;

	/**
	 * Constructor. Should be overriden to init the shifter by calling updateV0 ()
	 * with all x,y coordinates found in the input file loader.
	 */
	public CoordinateShifter2d() {

		// Init v0, the subclass constructor should find the min x and y of the scene by
		// calling updateV0 () for all objects with coordinates before execute () is
		// called
		v0 = new Vertex2d(Double.MAX_VALUE, Double.MAX_VALUE);

	}

	/**
	 * Updates v0 if needed to find the minimum value for all passed x and y
	 * coordinates (at construction time), can be used in a subclass constructor to
	 * init the shifter.
	 */
	protected void updateV0(double x, double y) {
		v0.x = Math.min(v0.x, x);
		v0.y = Math.min(v0.y, y);
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

}
