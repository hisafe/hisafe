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

package capsis.extension;

import java.awt.Color;

/**
 * DataFormat are the link between data extractors and data renderers.
 * Extractors implement some subclasses of DataFormat, which give methods to
 * retrieve the collected data. Renderers can work with some known formats and
 * they use the methods to draw some curves, histograms, table or drawings.
 * 
 * @author F. de Coligny - august 2000
 */
public interface DataFormat {

	/**
	 * All extractors must be able to return their name. The caller can try to
	 * translate it with Translator.swap (name) if necessary (ex: gui renderer
	 * translates, file writer does not).
	 */
	public String getName();

	/**
	 * All extractors must be able to return their caption. This tells what data
	 * are represented there (from which Step, which tree...).
	 */
	public String getCaption();

	/**
	 * All extractors must be able to return their color.
	 */
	public Color getColor();

	/**
	 * All extractors must be able to return their default data renderer class
	 * name.
	 */
	public String getDefaultDataRendererClassName();

	/**
	 * Returns true is the extractor can work on the current Step (e.g.
	 * false if works on cut trees and no cut trees on this step).
	 */
	public boolean isAvailable();
	
}
