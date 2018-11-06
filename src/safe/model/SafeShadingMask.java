package safe.model;

import java.io.Serializable;
import java.util.Vector;

import jeeb.lib.util.Vertex3d;


/**
 * A collection of SafeShadingNeighbour (used for light interception by crops)
 *
 * @see SafeShadingNeighbour and SafeBeam
 * @author G. Talbot - May 2007
 */


public class SafeShadingMask implements Serializable {

protected Vector shadingNeighbours;	// relative coordinates (Points)
private Vertex3d impact;

	public SafeShadingMask(Vertex3d imp) {
		impact = imp;
		shadingNeighbours = new Vector ();
	}

	public void addShadingNeighbour (SafeShadingNeighbour c) {
		shadingNeighbours.add (c);
	}

	public Vector getShadingNeighbours () {
		return shadingNeighbours;
	}

	public Vertex3d getImpact (){
		return impact;
	}
}

