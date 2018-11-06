package safe.model;

import java.text.NumberFormat;
import java.util.Vector;

import capsis.lib.samsaralight.SLBeam;

/**
 * A light beam (direct or diffuse used in the tree light interception process )
 *
 * @see SafeBeamSet
 * @author B. Courbaud - January 2000
 */
public class SafeBeam extends SLBeam {
 
	
	private float diffuseEnergy;	//=(fraction of diffuse radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)
	private float directEnergy;		//=(fraction of direct radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)
	private float infraRedEnergy;	//=(fraction of infra-red radiation allocated to this beam)/(sin(heightAngle)) 	(units : m-2 of beam section)

	private Vector shadingMasks;		//for each CellImpact, a mask of potentially shading neighbour crops.
										//competcell. shadingMasks is a Vector of SafeShadingMask elements.


	public SafeBeam (double a, double h, float difE, float dirE, float ire, float convFactor) {

		super(a, h, convFactor, false);
		

		//lightening a surface of unit projection on horizontal and energy of a beam lightening a unit
		//horizontal surface (=1 when no slope)
		diffuseEnergy=difE;
		directEnergy=dirE;
		infraRedEnergy=ire;
		shadingMasks = new Vector();

	}

	public float getDiffuseEnergy () {
		return diffuseEnergy;
	}

	public float getDirectEnergy () {
		return directEnergy;
	}

	public void setDirectEnergy (float dirE) {
		directEnergy=dirE;
	}

	public float getInfraRedEnergy () {
		return infraRedEnergy;
	}

	public void setInfraRedEnergy (float ire) {
		infraRedEnergy=ire;
	}

	public void resetDirectEnergy (){
		directEnergy=0;
	}

	public void addShadingMask (SafeShadingMask mask){	//competcell
		shadingMasks.add(mask);
	}

	public Vector getShadingMasks() {	//competcell
		return shadingMasks;
	}

	public void removeShadingMasks (){
		shadingMasks.removeAllElements();
	}

	/**
	 * Clear the neighbourCells collection
	 */
	public void removeAllNeighbourCell () {
		sites.clear ();
	}

	public String toString(){
		NumberFormat nf = NumberFormat.getNumberInstance();
		nf.setMaximumFractionDigits(0);

		String str = super.toString ()
			+ " azimut="+nf.format(Math.toDegrees(this.getAzimut_rad()))
			+ " heightAngle="+nf.format(Math.toDegrees(this.getHeightAngle_rad()));
		return str;
	}
	
}