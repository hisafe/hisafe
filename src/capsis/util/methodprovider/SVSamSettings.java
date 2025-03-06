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

import java.awt.Color;
import java.util.Map;

/**
 * A settings description for compatibility with the SVSamsara viewer. The
 * Settings object of the module must implement this interface.
 * 
 * @author F. de Coligny - November 2008
 */
public interface SVSamSettings {

	public int getSpeciesNumber();

	public double getPlotAspect();

	public double getBottomAzimuth();

	public String getSpeciesName(int code);

	public double getPlotCellWidth();

	// fc-12.1.2017 spName -> color
	public Map<String, Color> getSpeciesNameColorMap();

}
