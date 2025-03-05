/* 
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 2000-2010  Francois de Coligny, Samuel Dufour
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
package capsis.commongui.projectmanager;

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Dimension;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.swing.JPanel;

import jeeb.lib.util.Visitable;

import capsis.kernel.Project;
import capsis.kernel.Step;
import jeeb.lib.util.Visiter;

/**
 * A panel do draw a Project within an application based on the capsis kernel.
 * Contains a StepButton for each Step in the Project.
 * 
 * @author F. de Coligny - december 2009
 */
public class ProjectDrawing extends JPanel implements Visiter {

	protected ProjectManager projMan;
	protected Project project;

	private ArrayList<ProjectLine> lines;
	private int filler = 5; // pixels

	/**
	 * Constructor.
	 */
	public ProjectDrawing(ProjectManager projMan, Project project) {
		super(null); // no layout manager, absolute coordinates with setBounds
						// ()

		this.projMan = projMan;
		this.project = project;
		lines = new ArrayList<ProjectLine>();

		update();
	}

	public void update() {
		removeAll();
		// ~ drawAllSteps (project);
		drawVisibleSteps(project);
		repaint(); // should call paintComponent to draw the lines
	}

	/**
	 * Add a component in the scenario panel. Update the panel size to enclose
	 * the new component.
	 */
	public Component add(Component c) {
		super.add(c);
		Dimension size = getSize();

		int w = c.getBounds().x + c.getBounds().width + filler;
		if (w > size.width) {
			size.width = w;
		}

		int h = c.getBounds().y + c.getBounds().height + filler;
		if (h > size.height) {
			size.height = h;
		}

		setSize(new Dimension(size.width, size.height));
		return c;
	}

	/**
	 * Needed if this Panel is added in a container with a BoxLayout or
	 * derivative (LinePanel, ColumnPanel): theu use the preferredSize.
	 */
	public Dimension getPreferredSize() {
		return getSize();
	}

	/**
	 * Draw the lines between the step buttons. Three segments for each line.
	 */
	public void paintComponent(Graphics g) {
		// 1. Draw the StepButtons which were added by drawVisibleSteps ()
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;

		// 2. Draw the lines between the StepButtons
		for (ProjectLine line : lines) {
			g.setColor(line.getColor());
			g2.setStroke(line.getStroke());

			g.drawLine(line.getP1().x, line.getP1().y, line.getFirstInflectionPoint().x,
					line.getFirstInflectionPoint().y);
			g.drawLine(line.getFirstInflectionPoint().x, line.getFirstInflectionPoint().y,
					line.getSecondInflectionPoint().x, line.getSecondInflectionPoint().y);
			g.drawLine(line.getSecondInflectionPoint().x, line.getSecondInflectionPoint().y, line.getP2().x,
					line.getP2().y);

		}
		g2.setStroke(new BasicStroke(1f));
	}

	/**
	 * Draw a scenario inside this panel.
	 */
	public void drawVisibleSteps(Project p) {

		StepButton.setSuggestedWidth(calculateSuggestedWidth(p));

		// Draw
		Iterator ite = p.visiblePreorderIterator();
		while (ite.hasNext()) {
			Step s = (Step) ite.next();
			s.accept(this); // Visitor pattern : s will call this.visit (s);
		}
	}

	/**
	 * Draw a scenario inside this panel.
	 */
	public void drawAllSteps(Project p) {

		StepButton.setSuggestedWidth(calculateSuggestedWidth(p));

		// Draw
		Iterator i = p.preorderIterator();
		while (i.hasNext()) {
			Step s = (Step) i.next();
			s.accept(this); // Visitor pattern : s will call this.visit (s);
		}
	}

	/**
	 * Try to get a suggested width for the step buttons by scanning the step
	 * captions;
	 */
	private int calculateSuggestedWidth(Project p) {

		Iterator i = p.preorderIterator();
		String maxLabel = "";
		while (i.hasNext()) {
			Step s = (Step) i.next();
			String label = s.getScene().getCaption();
			if (label.length() > maxLabel.length()) {
				maxLabel = label;
			}
		}

		// fc-11.2.2020 missing a little space to see the complete labels
		maxLabel += 2;

		FontMetrics fm = getFontMetrics(getFont());

		// fc-24.2.2020 added ICON_SIZE to ensure the whole step button label is
		// always visible
		return fm.stringWidth(maxLabel) + StepButton.ICON_SIZE;

	}

	/**
	 * Draw a visible step in this panel.
	 */
	public void visit(Visitable visitable) {
		Step step = (Step) visitable;

		// Create the StepButton if needed, returns it if already created
		StepButton sb = projMan.getCreateStepButton(step);

		sb.draw(this); // will add the sb into this ProjectDrawing
	}

	public List<ProjectLine> getLines() {
		return lines;
	}

	public String toString() {
		StringBuffer b = new StringBuffer();
		b.append("ProjectPanel_(");
		b.append(project.toString());
		b.append(")");
		return b.toString();
	}

}
