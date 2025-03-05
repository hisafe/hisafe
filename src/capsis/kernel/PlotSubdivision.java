package capsis.kernel;

import java.awt.Shape;
import java.io.Serializable;
import java.util.Collection;

import capsis.defaulttype.SceneConnected;
import jeeb.lib.util.Identifiable;
import jeeb.lib.util.Vertex3d;

/**
 * Interface for plot subdivisions
 * 
 * author F. de Coligny - October 2017
 */
public interface PlotSubdivision extends Identifiable, Cloneable, Serializable {
	
// The plot this PlotSubdivision is part of

	public Plot getPlot();

	public void setPlot(Plot plot);


// Cell basic geometry

	/**
	 * Returns the origin of the Cell: the point at the bottom left corner if
	 * the Cell is Rectangular, or a point of the shape if not.
	 */
	public Vertex3d getOrigin(); // Vertex3d

	/**
	 * Returns the list of vertices of the Cell polygon.
	 */
	public Collection<Vertex3d> getVertices(); // Vertex3d inside

	/**
	 * Returns the area of the Cell in m2
	 */
	public double getArea();

	/**
	 * Returns the geometrical shape of the Cell (from the top)
	 */
	public Shape getShape();

	public void setOrigin(Vertex3d v);

	public void setVertices(Collection<Vertex3d> c);

	public void setArea(double a);


// String representations

	public String toString();

	public String bigString();

	
}
