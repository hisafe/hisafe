package capsis.util.shifter;

/**
 * A dummy shifter does not shift any coordinates (can be used when a coordinate
 * shifter is to be ignored).
 * 
 * @author F. de Coligny - September 2022
 */
public class DummyShifter2d extends CoordinateShifter2d {

	public DummyShifter2d() {
		super();
		v0 = null; // unused
	}

	protected void updateV0(double x, double y) {
		// Do nothing
	}

	@Override
	public void execute() throws Exception {
		// Do nothing
	}

	public double shiftX(double x) {
		return x; // Do nothing
	}

	public double shiftY(double y) {
		return y; // Do nothing
	}

	public double restoreX(double x) {
		return x; // Do nothing
	}

	public double restoreY(double y) {
		return y; // Do nothing
	}

}
