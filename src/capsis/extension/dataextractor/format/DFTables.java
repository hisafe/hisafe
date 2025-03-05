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

package capsis.extension.dataextractor.format;

import java.util.Collection;

import capsis.extension.DataFormat;

/**
 * This interface describes the data format to build tables.
 * Some Data Renderers may show this data type.
 * 
 * @author F. de Coligny - february 2002
 */
public interface DFTables extends DataFormat {
	
	/**
	 * Return a Collection containing n tables.
	 * Each element is a String [][]. 
	 * First line contains column headers (mandatory).
	 */
	public Collection<String[][]> getTables ();
	
	/**
	 * Return (may return null if unused) a Collection containing titles associated to each table
	 * described in getTables ().
	 */
	public Collection<String> getTitles ();
	
	
}
