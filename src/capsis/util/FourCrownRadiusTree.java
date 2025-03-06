package capsis.util;

/**
 * An interface for individual trees with 4 crown radius (north, south, east, west)
 *  
 * @author F. de Coligny - October 2021
 */
public interface FourCrownRadiusTree {

	public double getCrownRadius (); // m., avg radius
	public double getCrownRadiusNorth (); // m.
	public double getCrownRadiusEast (); // m.
	public double getCrownRadiusSouth (); // m.
	public double getCrownRadiusWest (); // m.
	
}
