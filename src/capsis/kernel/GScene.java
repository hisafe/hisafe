/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2010 INRA 
 * 
 * Authors: F. de Coligny, S. Dufour-Kowalski, 
 * 
 * This file is part of Capsis
 * Capsis is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Capsis is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * along with Capsis.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

package capsis.kernel;

import jeeb.lib.util.Vertex3d;

/**
 * GScene is the superclass for the modules main data structure (e.g. a forest
 * OR a plantation in Capsis / an ArchiTree in Xplo / a composite scene in
 * Simeo...). It may be a list of individuals or not. It has a date and is
 * carried by a Step (with the same date) in the simulation Project. In the
 * 'model class' of a module (extends GModel), there is generally at least one
 * method to create an initial scene (e.g. loading a file) and the evolution
 * method creates new scenes and new steps to carry them.
 * 
 * @author F. de Coligny - december 2000, september 2010
 */
public interface GScene {

	// Initial scene

	/**
	 * The scene is initial if it is the first of the project, i.e. under the
	 * Project's root Step.
	 */
	public void setInitialScene(boolean b);

	public boolean isInitialScene();

	// Evolution stage methods

	/**
	 * Returns a GScene to be used as a basis for a new Step in an evolution
	 * stage. This can be a clone but is generally only a partial clone. E.g.
	 * for a forest in an individual based model with trees, the evolution base
	 * may contain no trees because they must be grown before to be added in the
	 * new scene. Cloning them and re-looping to make them grow would be a waste
	 * of time. This is related to the capability of the capsis.kernel to manage
	 * a time history with the different scenes at each calculated date
	 * memorized under matching Steps in a Project. Used in
	 * GModel.processEvolution ().
	 */
	public GScene getEvolutionBase();

	// Intervention related methods

	/**
	 * Returns a perfect clone of the GScene. This clone will be modified by the
	 * interveners. E.g. if the intervention is a thinning in a forestry model,
	 * some trees will be removed from the clone before it is linked to a new
	 * step.
	 */
	public GScene getInterventionBase();

	/**
	 * The scene is the result of an intervention if it was created by an
	 * intervener.
	 */
	public void setInterventionResult(boolean b);

	public boolean isInterventionResult();

	// Plot and simple geometry related methods

	/**
	 * A plot is a geometric description associated to the the scene. It may
	 * contain ground cells with a geometry (squares, polygons...) and custom
	 * properties. The cells may contain individual trees if any. See Plot,
	 * PlotOfCells.
	 */
	public void setPlot(Plot p);

	// fc+sg-25.10.2017 added plot subclasses
	public <T extends Plot> T getPlot();
//	public Plot getPlot();

	public boolean hasPlot();

	// The 8 methods below are shortcuts to plot methods, see Plot and
	// PlotOfCells.

	public Vertex3d getOrigin(); // m

	public double getXSize(); // m

	public double getYSize(); // m

	/**
	 * Returns the scene's area expressed in m2 
	 * @return the scene's area expressed in m2
	 */
	public double getArea();

	public void setOrigin(Vertex3d o); // m

	public void setXSize(double v); // m

	public void setYSize(double v); // m

	public void setArea(double a); // m2

	// Accessors

	/**
	 * The date of this scene. Each module has its own time step. The date can
	 * be a simple index from 0 to n, or a year (1995, 1996...), or a number of
	 * days from the beginning, a number of weeks, of months...
	 */
	public int getDate();

	public void setDate(int d);

	/**
	 * Each scene calculated during a simulation is linked under a Step to be
	 * kept in memory in a project.
	 */
	public Step getStep();

	public void setStep(Step stp);

	/**
	 * Source data of the project, e.g. initial inventory file name or
	 * "virtual stand"
	 */
	public String getSourceName(); // convenient

	public void setSourceName(String str);

	// Other methods

	/**
	 * Default expected caption: if isInteventionResult(): "*"+getDate(), else
	 * getDate ().
	 */
	public String getCaption(); // convenient

	/**
	 * A toolTipText possibly assigned to the stepButton matching the Step
	 * carrying this scene in the projectManager of the capsis gui Pilot.
	 */
	public String getToolTip(); // convenient

	public Object clone();

	public String toString();

	public String bigString();

}
