/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2003  Francois de Coligny
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A
 * PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */
package capsis.util.methodprovider;

import java.awt.Shape;
import java.util.Collection;
import java.util.Set;

import jeeb.lib.util.RGB;
import capsis.kernel.Plot;

/**
 * A cell description for compatibility with the SVSamsara viewer. The Cell
 * object of the module must implement this interface.
 * 
 * @author F. de Coligny - november 2008
 */
public interface SVSamCell extends RGB {

	public Shape getShape();

	public Plot getPlot();

	public double getRelativeHorizontalEnergy();

	// fc+bc-16.2.2022 saplings are in the regeneration library, samsa2Saplings are
	// in samsara and samsara2
	public int getSamsa2SaplingNb(int code);

	public double getProp(String key);

	public Set<String> getPropKeys();

	public int[] getRGB();

	public void setRGB(int[] v); // 3 ints

	// fc+bc-16.2.2022 saplings are in regeneration library, samsa2Saplings instead
	// here
	public Collection getSamsa2Saplings(); // fc-7.11.2013

	public double getArea(); // m2, fc+bc-13.1.2017

}
