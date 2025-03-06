/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2013 Francois de Coligny
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library;
 * if not, write to the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
 * 02111-1307 USA
 */
package capsis.util.methodprovider;

/**
 * A sapling description for compatibility with the SVSamsara viewer. The
 * Sapling object of the module must implement this interface.
 * 
 * @author F. de Coligny - November 2013
 */
public interface SVSamSapling {

	// fc+bc-2024 getX() and getY() now return double instead of float (merging
	// Samsa2Sapling and RGSapling)

	public int getSpeciesId();

	public double getX();

	public double getY();

	public double getHeight();

}
