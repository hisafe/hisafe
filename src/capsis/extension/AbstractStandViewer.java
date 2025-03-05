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
import java.awt.Container;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.geom.Rectangle2D;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;

import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JInternalFrame;

import capsis.commongui.projectmanager.ButtonColorer;
import capsis.commongui.projectmanager.ButtonColorerListener;
import capsis.commongui.projectmanager.ProjectManager;
import capsis.commongui.projectmanager.StepButton;
import capsis.extension.standviewer.StandViewerManager;
import capsis.extensiontype.StandViewer;
import capsis.gui.Pilot;
import capsis.gui.Positioner;
import capsis.gui.Repositionable;
import capsis.kernel.GModel;
import capsis.kernel.Project;
import capsis.kernel.Step;
import capsis.util.PrintContext;
import jeeb.lib.util.Disposable;

/**
 * An abstract superclass for Capsis StandViewers.
 * 
 * @author F. de Coligny - November 2000
 */
abstract public class AbstractStandViewer extends AbstractDiagram
		implements StandViewer, Repositionable, Printable, Disposable, ButtonColorerListener {

	// fc-9.12.2015 added AbstractDiagram for better positioner management
	// (order)

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	protected GModel model;
	protected Step step;
	protected StepButton stepButton;

	// Temporary, to easy change from JInternalFrame to JPanel
	// fc - 19.3.2003
	protected boolean sameStep = false;

	// fc-28.9.2021
	private StandViewerManager standViewerManager;
	
	@Override
	public void init(GModel model, Step s, StepButton but) throws Exception {
		this.model = model;
		this.step = s;
		this.stepButton = but;

		ButtonColorer.getInstance().addListener(this);

//		// fc-23.12.2020
//		updateFrameTitle(); // Too early...

	}

	// fc-28.9.2021 Optional, a standViewerManager wants to be told when the
	// standViewer closes
	public void setStandViewerManager(StandViewerManager standViewerManager) {
		this.standViewerManager = standViewerManager;
	}

	
	/**
	 * If the diagram is a StandViewer, this is the standviewer class name. If the
	 * diagram is a DataRenderer, this is the matching DataExtractor className. Used
	 * by the positioners.
	 */
	@Override
	public String getDiagramClassName() {
		return getClass().getName();
	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public void colorMoved(StepButton previousButton, StepButton newButton) {
		
//		System.out.println("AbstractStandViewer colorMoved()...");
		
		if (previousButton == this.stepButton) {
			
//			System.out.println("AbstractStandViewer calling update(newButton)...");

			update(newButton);

			JInternalFrame ifr = Pilot.getInstance().getPositioner().getInternalFrame(this);
			if (ifr != null) {
				colorize(ifr, newButton.getColor());
			}
		}
	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public void colorRemoved(StepButton stepButton) {
		if (stepButton == this.stepButton) {
			close();
		}
	}

	/**
	 * ButtonColorerListener interface
	 */
	@Override
	public boolean isListening(StepButton sb) { // fc-2.1.2017
		return this.stepButton == sb;
	}

	public Container getContentPane() {
		return this;
	}

	public Container getDesktopPane() {
		return this;
	}

	public void setTitle(String s) {
		// Update container title (our title contains the step name)
		JInternalFrame ifr = Pilot.getInstance().getPositioner().getInternalFrame(this);
		if (ifr != null) {
			ifr.setTitle(s);
		}

	}

	public void setSelected(boolean b) {
	}

	/**
	 * The method to trigger the update of the standViewer, must be overriden, call
	 * super.update () and to the update job.
	 */
	@Override // fc-28.9.2021 Added in the superclass
	public void update() {

		// fc-23.12.2020
		updateFrameTitle();

	}

	public boolean isHidden() {
		return false;
	}

	// fc - 19.3.2003

	/** Ask the current positionner to layout the component. */
	@Override
	public void reposition() {
		Pilot.getPositioner().layOut(this);
	}

	/**
	 * May be used by Positionner to put some color in a JInternalFrame.
	 */
	public StepButton getStepButton() {
		return stepButton;
	}

	/**
	 * A stand viewer may be updated to synchronize itself with a given step.
	 */
	public void update(Step s) {
		update(ProjectManager.getInstance().getStepButton(s));
	}

	/**
	 * A stand viewer may be updated to synchronize itself with a given step button.
	 * In subclasses, redefine this method (beginning by super.update (sb);) to
	 * update your representation of the step.
	 */
	public void update(StepButton sb) {
		
		
//		System.out.println("AbstractStandViewer in update(newButton)...");

		
		this.stepButton = sb;
		this.step = stepButton.getStep();

		updateFrameTitle();
	}

	/**
	 * Search the frame containing the viewer to update its title (at init time then
	 * when changing stepButtons or configuration)
	 */
	private void updateFrameTitle() {

		JInternalFrame ifr = Pilot.getInstance().getPositioner().getInternalFrame(this);
		if (ifr != null) {

			// fc-23.12.2020
			ifr.setTitle(getStandardisedTitle());
//			ifr.setTitle(getName());

		}

	}

	/**
	 * Build a title from step button and extension name.
	 */
	// fc-23.12.2020 This getName () was overriden in all StandViewer subclasses by
	// the standard Extension getName () method restoration by nb and thus
	// disappeared for a while. The viewers title bars missed the step caption.
	// Renamed it getTitle () to restore correct stand viewers title bars.
	private String getStandardisedTitle() {
//	public String getName() {

		// Reference to the step
		if (step != null) {
			Project project = step.getProject();
			if (project != null) {
				StringBuffer b = new StringBuffer();

				// fc-23.12.2020
				b.append(getName());
//				b.append(ExtensionManager.getName(this));

				b.append(" - ");
				b.append(project.getName());
				b.append(".");
				b.append(step.getName());

				// fc-23.12.2020 Added the title complement, e.g. (/ha)
				String complement = getTitleComplement();
				if (complement != null && complement.length() > 0) {
					b.append(" (");
					b.append(complement);
					b.append(")");
				}

				return b.toString();
			}
		}

		// fc-23.12.2020
		return getName();
//		return ExtensionManager.getName(this);

	}

	/**
	 * Can be overriden in subclasses to ass something to the standardised title
	 * (for title bars). Can return null.
	 */
	public String getTitleComplement() {

		// fc-23.12.2020 StandViewers titles hamonisation

		// Default: no complement
		return null;

	}

	/**
	 * From Extension interface. May be redefined by subclasses. Called after
	 * constructor at extension creation.
	 */
	@Override
	public void resettle() {

		// fc-14.12.2020 Extensions unused activate () method are being removed, kept
		// only in StandViewers where they are used, renamed in resettle ()
		// fc-16.5.2007 Check activate call in Extension and ExtensionLoader

		reposition();

		// fc-23.12.2020
		updateFrameTitle();

	}

	/** Dispose the extension. */
	@Override
	public void dispose() {
		ButtonColorer.getInstance().removeListener(this);

		// fc-2.1.2017
		ButtonColorer.getInstance().seeIfColorShouldBeRemoved(stepButton);
		
		// fc-28.9.2021
		standViewerManager.standViewerDisposed(this);
		
	}

	public void close() {
		Pilot.getPositioner().remove(this); // Important

	}

	/**
	 * Print method. Called by a printJob. See capsis.gui.command.PrintPreview and
	 * capsis.gui.command.Print.
	 */
	public int print(Graphics g, PageFormat pf, int pi) throws PrinterException {
		if (pi >= 1) {
			return Printable.NO_SUCH_PAGE;
		}
		Graphics2D g2 = (Graphics2D) g;

		g2.translate(pf.getImageableX(), pf.getImageableY());

		// optionally : scale to fit to page size
		if (PrintContext.isFitToSize()) {
			double xScale = pf.getImageableWidth() / getWidth();
			double yScale = pf.getImageableHeight() / getHeight();
			double scale = Math.min(xScale, yScale);
			g2.scale(scale, scale);
		} else {
			// else, clip
			Rectangle2D.Double r2d = new Rectangle2D.Double();
			r2d.setRect(0, 0, pf.getImageableWidth(), pf.getImageableHeight());
			g2.clip(r2d);
		}

		// To the screen: print () gives the best effect...
		if (PrintContext.isPrintPreview()) {
			print(g2); // troubles with Of width in Strokes

			// And to Printer, paint avoids trouble of curves with one device
			// pixel width:
			// not wide enough to be seen... (Stroke with width=0 to be x & y
			// transform independant)
		} else {
			paint(g2); // WORKS WORKS WORKS !!!!!!!!!!!!!!!!!!!!!!!!!!!!
		}

		return Printable.PAGE_EXISTS;
	}

	@Override
	public void setLayout(Positioner p) {
		p.layoutComponent(this);
	}

	/**
	 * Create an icon of the given color and set it in the title bar.
	 */
	static public void colorize(JInternalFrame f, Color c) {
		Image image = f.createImage(10, 10);
		Graphics gra = image.getGraphics();
		int w = 9;
		int h = 9;
		gra.setColor(c);
		gra.fillRect(0, 0, w, h);
		gra.setColor(Color.black);
		gra.drawRect(0, 0, w, h);
		gra.setColor(Color.white);
		gra.drawLine(0, h, w, h);
		gra.drawLine(w, h, w, 0);
		Icon icon = new ImageIcon(image);
		f.setFrameIcon(icon);
	}

}
