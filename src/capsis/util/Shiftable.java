package capsis.util;

/**
 * An interface for object with x,y coordinates that can be shifted. This can
 * be used when input data come from a GIS with very large coordinate values
 * (e.g. Lambert).
 * 
 * @author F. de Coligny - September 2022
 */
public interface Shiftable {

	// To get the original coordinate values, before shifting
	
	public double getUnshiftedX ();
	public double getUnshiftedY ();
	
}
