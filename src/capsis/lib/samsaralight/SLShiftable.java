package capsis.lib.samsaralight;

/**
 * An interface for some crown parts which an be shifted in x, y and z. Related to
 * Samsara2's PeripheryCreator tool, clones trees to fill the border of the
 * stand, the crown parts must be moved in x, y, but also in z.
 * 
 * @author F. de Coligny, B. Courbaud - June 2022
 */
public interface SLShiftable {
	
	/**
	 * This method is specific to Samsara2 and the PeripheryCreator
	 * tool: some trees are cloned to be located in the periphery of the stand,
	 * their location must be updated.
	 */
	public void setXY(double x, double y);

	/**
	 * This method is specific to Samsara2 and the PeripheryCreator
	 * tool: some trees are cloned to be located in the periphery of the stand,
	 * their z must be updated.
	 */
	public void shiftZ(double zShift);
}
