package capsis.util.shifter;

/**
 * A dummy shifter does not shift any coordinates (can be used when a coordinate
 * shifter is to be ignored).
 * 
 * @author F. de Coligny - January 2024
 */
public class DummyShifter3d extends CoordinateShifter3d {

	public DummyShifter3d() {
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

	public double shiftZ(double z) {
		return z; // Do nothing
	}

	public double restoreX(double x) {
		return x; // Do nothing
	}

	public double restoreY(double y) {
		return y; // Do nothing
	}

	public double restoreZ(double z) {
		return z; // Do nothing
	}


}
