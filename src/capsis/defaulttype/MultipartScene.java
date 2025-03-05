/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA 
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
package capsis.defaulttype;

import java.awt.Graphics2D;
import java.awt.Shape;
import java.awt.geom.Rectangle2D;
import java.awt.geom.Rectangle2D.Double;
import java.util.List;
import java.util.Map;

import javax.swing.JPanel;

/**
 * An interface for GScenes made of several parts.
 *
 * @author F. de Coligny - january 2011
 */
public interface MultipartScene {

	public int getDate();

	public List<ScenePart> getParts();

	public List<String> getPartNames();

	public ScenePart getPart(String name);

	// fc-25.6.2015 added this method for optional finer drawing
	// May do nothing
	public void draw(Graphics2D g2, Rectangle2D.Double visibleRectangle, ScenePart part, Map<Shape, Object> drawnMap);

	// fc-7.1.2019 This is called after draw () has been called on all the scene
	// parts in case something else should be drawn after all the parts (See
	// Lub2Scene), may do nothing
	public void drawMore(Graphics2D g2, Double visibleRectangle, Map<Shape, Object> drawnMap);

	// fc-23.6.2016 added an optional legend, may return null;
	public JPanel getLegend();

}
