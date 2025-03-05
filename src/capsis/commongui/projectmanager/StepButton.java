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
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.image.BufferedImage;
import java.util.Collection;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JPopupMenu;
import javax.swing.JTable;
import javax.swing.TransferHandler;

import capsis.kernel.Engine;
import capsis.kernel.Project;
import capsis.kernel.Session;
import capsis.kernel.Step;
import jeeb.lib.util.Log;
import jeeb.lib.util.Settings;
import jeeb.lib.util.WxHString;

/**
 * StepButton is a JButton showing a capsis.kernel.Step in the ProjectManager.
 * It is drawn by ProjectPanel inside itself. A StepPopup is associated to
 * trigger actions with a mouse right click. StepButtons are used to show a
 * project.
 * 
 * @author F. de Coligny - august 2009
 */
public class StepButton extends JButton implements ActionListener, MouseListener /* , ChangeListener */ {

	// nb-27.08.2018
	// private static final long serialVersionUID = 1L;

	// fc-9.11.2020 Removed UserColor
	static public final Color DEFAULT_COLOR = new Color(255, 255, 204); // null colorProvider
//	static public final UserColor DEFAULT_COLOR = new UserColor(null, 0, 255, 255, 204); // null colorProvider

	public static final int X_ORIGIN = 10;
	public static final int Y_ORIGIN = 5;
	public static final int MIN_WIDTH = 60;
	public static final int MIN_HEIGHT = 20;
	public static final int COMPACT_MIN_WIDTH = 56;
	public static final int COMPACT_MIN_HEIGHT = 15;

	public static final int X_SPACE = 20;
	public static final int Y_SPACE = 10;
	public static final int COMPACT_X_SPACE = 5;
	public static final int COMPACT_Y_SPACE = 5;

	// fc-24.2.2020 Ensure all the step button text is always visible
	public static final int ICON_SIZE = 10;

	// Optional: this can be set when drawing all the buttons of the same project
	// by scanning the labels length before: max label width
	// See draw ()
	private static int suggestedWidth = 0;

	// ~ private static Border selectionBorder =
	// BorderFactory.createLoweredBevelBorder ();
	// ~ private static Border normalBorder =
	// BorderFactory.createEtchedBorder();

	private static Font tableFont = new JTable().getFont();

	private Step step;

	private ProjectManager projectManager;

	private int width;
	private int height;
	private int drawingDepth; // row and column of the Step when being drawn
	// (only
	private int drawingWidth; // for the visible Steps)

	protected Color color;

//	static protected class ButtonTransferHandler extends TransferHandler {
//		private static final long serialVersionUID = 1L;
//	
//		public ButtonTransferHandler() {
//			super("text");
//		}
//		
//		@Override
//		public boolean canImport(JComponent c, DataFlavor[] flavors) {
//
//			if(c instanceof DataRenderer) { return true; }
//			return false;
//		}
//		
//		@Override
//		public boolean importData(JComponent c, Transferable t) {
//	        if (canImport(c, null)) {
//	        	
//	        	Step s = Current.getInstance ().getStep ();
//				StepButton sb = ProjectManager.getInstance ().getStepButton (s);
//				
//				ButtonColorer.getInstance ().newColor (sb);
//				DataRenderer dr = (DataRenderer) c;
//				dr.getDataBlock().addExtractor (sb.getStep ());
//				
//	            return true;
//	        }
//	        return false;
//	    }
//		
//	}

	static public TransferHandler transfertHandler;

	/**
	 * Constructor. Scope is package (protected). The step buttons should be created
	 * only by the method ProjectManager.getStepButton (Step). This method maintains
	 * a Map up to date: Step -> StepButton.
	 */
	protected StepButton(ProjectManager projectManager, Step stp) {
		super();

		this.projectManager = projectManager;
		this.step = stp;

		setMargin(new Insets(0, 0, 0, 0)); // top, left, bottom, right
		setToolTipText(stp.getScene().getToolTip());

		setFont(tableFont);
		// setText (getCaption ());

		addMouseListener(this);
		addActionListener(this);

//		if(transfertHandler == null) { 
//			transfertHandler = new ButtonTransferHandler();
//		}
//		//drag and drop
//		setTransferHandler(transfertHandler);

	}

	public String getText() {
		return getCaption();
	}

	/**
	 * Open expand popup
	 */
	private JPopupMenu openExpandPopup(int x, int y, StepButton first, StepButton second) {
		try {
			Current.getInstance().setStep(step);

			// Highlight target StepButton range
			String range = null;

			if (first != null && second != null) {
				if (first.getX() < second.getX()) {
					range = " [" + first.getCaption() + "-" + second.getCaption() + "]";
				} else {
					range = " [" + second.getCaption() + "-" + first.getCaption() + "]";
				}
			}

			boolean correctRange = projectManager.setSelectionRange(first, second);
			projectManager.update();

			if (correctRange) {

				// Open expand popup in this range
				ExpandPopup popup = new ExpandPopup(projectManager, range, first, second);
				popup.show(this, x, y);
				return popup;

			} else {
				return null;

			}

		} catch (Exception e) {
			Log.println(Log.ERROR, "StepButton.openExpandPopup ()", "Exception", e);
			return null;
		}

	}

	/**
	 * Open step popup
	 */
	private JPopupMenu openStepPopup(int x, int y) {
		try {
			Current.getInstance().setStep(step);

			StepPopup popup = new StepPopup(this);
			popup.show(this, x, y);
			return popup;

		} catch (Exception e) {
			Log.println(Log.ERROR, "StepButton.openStepPopup ()", "Exception", e);
			return null;
		}

	}

	/**
	 * Choose what popup must be opened when clicking on the StepButton
	 */
//	private JPopupMenu popupStrategy(int x, int y, boolean popupTrigger,
//			boolean shift) {
//		if (popupTrigger) {
//			return openStepPopup(x, y);
//		} else {
//			if (shift) {
//				Step step0 = Current.getInstance().getPreviousStep();
//				Step step1 = Current.getInstance().getStep();
//				if (step0 == null) {
//					return null;
//				}
//
//				if (step0.getProject().equals(step1.getProject())) {
//					StepButton first = ProjectManager.getInstance()
//							.getStepButton(step0);
//					StepButton second = this;
//					return openExpandPopup(x, y, first, second);
//				}
//			}
//		}
//		return null;
//	}

	/**
	 * MouseListener interface.
	 */
	public void mousePressed(MouseEvent e) {

		Current.getInstance().setStep(step);

//		popupStrategy(e.getX(), e.getY(), e.isPopupTrigger(), e.isShiftDown());

		// popupTrigger may be on mousePressed or mouseReleased depending on the L&F
		if (e.isPopupTrigger()) {
			openStepPopup(e.getX(), e.getY());

		} else {

			// shift + mousePressed opens expand / collapse popup
			if (e.isShiftDown()) {
				Step step0 = Current.getInstance().getPreviousStep();
				Step step1 = Current.getInstance().getStep();
				if (step0 != null && !step0.equals(step1)) {

					if (step0.getProject().equals(step1.getProject())) {
						StepButton first = ProjectManager.getInstance().getStepButton(step0);
						StepButton second = this;
						openExpandPopup(e.getX(), e.getY(), first, second);
					}

				}
			}

		}

		// Tidy the selection range (may have stayed enlightened)
		if (!e.isShiftDown()) {
			// Remove selection range if any
			projectManager.setSelectionRange(null, null);
			projectManager.update();
		}

		// Shift is for expand / collapse, do nothing more (otherwise,
		// interference)
		if (e.isShiftDown()) {
			return;
		}

		// Tell the ButtonColorer
		if (e.getClickCount() == 2) {

			if (isColored())
				return; // double click forbidden on colored stepButton

			ButtonColorer.getInstance().moveColor(this);
			return;
		}

		if (e.isControlDown()) {
			ButtonColorer.getInstance().removeColor(this);
			return;
		}

		if (isColored()) {
			ButtonColorer.getInstance().rearm(this);
		}

//		// enable drag and drop
//		if(e.getButton() == MouseEvent.BUTTON1) {
//			
//			TransferHandler handler = getTransferHandler();
//			handler.exportAsDrag(this, e, TransferHandler.COPY);
//			e.consume();
//			
//		}

	}

	/**
	 * MouseListener interface.
	 */
	public void mouseReleased(MouseEvent e) {

//		popupStrategy(e.getX(), e.getY(), e.isPopupTrigger(), e.isShiftDown());

		// popupTrigger may be on mousePressed or mouseReleased depending on the L&F
		if (e.isPopupTrigger()) {
			openStepPopup(e.getX(), e.getY());
		}

	}

	/**
	 * From MouseListener interface.
	 */
	public void mouseEntered(MouseEvent e) {
	}

	/**
	 * From MouseListener interface.
	 */
	public void mouseExited(MouseEvent e) {
	}

	/**
	 * MouseListener interface.
	 */
	public void mouseClicked(MouseEvent e) {
	}

	/**
	 * Click on the step button. Ctrl+click and Shift+click may be detected.
	 * Alt+click does not trigger an ActionEvent.
	 */
	public void actionPerformed(ActionEvent e) {
		// ~ System.out.println ("*** StepButton.actionPerformed ()");
		// ~ boolean ctrl = (e.getModifiers() & ActionEvent.CTRL_MASK) != 0;
		// ~ boolean shift = (e.getModifiers() & ActionEvent.SHIFT_MASK) != 0;
		// ~ boolean alt = (e.getModifiers() & ActionEvent.ALT_MASK) != 0;

		// ~ // Remove selection range if any
		// ~ projectManager.setSelectionRange (null, null);

		// ~ Current.getInstance ().setStep (step);
		// ~ projectManager.update ();

	}

	public static void setSuggestedWidth(int w) {
		StepButton.suggestedWidth = (int) Math.round(w * 1.2d);
	}

	/**
	 * Draw the stepButton in the given ProjectDrawing panel: calculate its exact
	 * position (setBounds ()), adds it in the panel, then build a line to its
	 * father and store it in the panel. Note: all the StepButtons in the same
	 * project have the same size.
	 */
	public void draw(ProjectDrawing panel/* , boolean connectToVisible */) {
		boolean connectToVisible = true; // fc-12.1.2010

		// 1. compute bounds and add the step button in the given scenario panel
		drawingDepth = step.getDepth(); // to be used by the sons, to draw their
		// line to their father
		drawingWidth = step.getWidth();

		// fc-22.12.2020 Enlarged a bit the step buttons default size to better see
		// their labels (see capsisPreferences for more customisation)
		String buttonSize = Settings.getProperty("step.button.size", "70x20");
//		String buttonSize = Settings.getProperty("step.button.size", "60x20");
		WxHString wxh = new WxHString(buttonSize);
		width = wxh.getW();
		height = wxh.getH();

		int local_xspace = X_SPACE;
		int local_yspace = Y_SPACE;
		if (Settings.getProperty("project.manager.compact.mode", true)) {
			local_xspace = 3;
			local_yspace = 1;
		}
		int local_min_width = MIN_WIDTH;
		int local_min_height = MIN_HEIGHT;

		if (suggestedWidth > 0 && width < suggestedWidth) {
			width = suggestedWidth;
		}
		if (width < local_min_width) {
			width = local_min_width;
		}
		if (height < local_min_height) {
			height = local_min_height;
		}

		int x = X_ORIGIN + step.getDepth() * (width + local_xspace);
		int y = Y_ORIGIN + step.getWidth() * (height + local_yspace);

		this.setBounds(x, y, width, height);

		panel.add(this);

		// 2. compute scenario lines inflexion points
		Step father = null;
		if (connectToVisible) {
			father = (Step) step.getVisibleFather();
		} else {
			father = (Step) step.getFather();
		}

		// 3. Draw a line from the father to this step
		if (father != null) { // root has null father, no line
			// Calculate the 4 points
			int p1x = X_ORIGIN + projectManager.getStepButton(father).getDrawingDepth() * (local_xspace + width)
					+ width; // Scenario Line
			int p1y = Y_ORIGIN + projectManager.getStepButton(father).getDrawingWidth() * (local_yspace + height)
					+ height / 2;
			int p2x = x;
			int p2y = y + (height / 2);

			// Create the line
			ProjectLine line = new ProjectLine(new Point(p1x, p1y), new Point(p2x, p2y));

			// Set its color and stroke
			Collection<StepButton> selection = projectManager.getSelectedButtons();
			if (selection != null && selection.contains(this)
					&& selection.contains(projectManager.getStepButton(father))) {
				line.setColor(projectManager.getSelectionColor());
				line.setStroke(new BasicStroke(2f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
			}

			panel.getLines().add(line);
		}

	}

//	public void setSelected (boolean v) {
//		super.setSelected (v);
//		setBorder (v ? projectManager.getStepSelectionBorder () : null);
//	}

	/**
	 * Colorize the button with a color
	 */
	public void colorize() {

		// fc-25.9.2012 reviewing the Colors
		RelatedColorProvider cp = projectManager.getColorProvider();

		Session s = Engine.getInstance().getSession();
		List<Project> ps = s.getProjects();
		Project p = getStep().getProject();
		int i = ps.indexOf(p); // index of the project in the session

		// fc-5.11.2020 Main colors are colors very different
		color = cp.getMainColor();
//		color = cp.getColor (i);

		colorize(color);
	}

	/**
	 * Colorize the button with the given color
	 */
	public void colorize(Color col) {

		if (col == null) {
			color = StepButton.DEFAULT_COLOR; // fc-24.9.2012 sb color is never null
			setIcon(null);
			return;
		}

		color = col;

		// Note: setBackground is not supported by all the look&feels and thus
		// can not be used here

		int h = Math.min(ICON_SIZE, getHeight()); // at max ICON_SIZE
//		int h = Math.min(10, getHeight()); // at max 10

		if (h <= 0)
			return; // trouble, do not draw color

		// Add a colored icon
		BufferedImage img = new BufferedImage(h, h, BufferedImage.TYPE_INT_ARGB);
		Graphics g = img.getGraphics();
		g.setColor(color);
		g.fillRect(0, 0, h, h);
		ImageIcon icon = new ImageIcon(img);
		setIconTextGap(1);
		setIcon(icon);

	}

	public void uncolorize() {

		colorize(null);

	}

	public Color getColor() {
		return color;
	}

	public boolean isColored() {
		if (color == null || color.equals(StepButton.DEFAULT_COLOR)) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Release references to allow good garbage collection.
	 */
	public void dispose() {
		// Release the tools synchronized on the StepButton
		ButtonColorer.getInstance().removeColor(this);

		step = null;
	}

	public int getWidth() {
		return width;
	}

	public Step getStep() {
		return step;
	}

	/**
	 * StepButton caption is of type '*17a' (see Step.getName ()).
	 */
	public String getCaption() {
		return step != null ? step.getName() : "";
	}

	public int getHeight() {
		return height;
	}

	public int getDrawingDepth() {
		return drawingDepth;
	}

	public int getDrawingWidth() {
		return drawingWidth;
	}

	public String toString() {
		// ~ System.out.println ("StepButton toString () x = "+getX
		// ()+" y = "+getY ());
		StringBuffer b = new StringBuffer();
		b.append("StepButton(");
		try {
			b.append(step.toString());
		} catch (Exception e) {
			b.append("step=trouble");
		}
		b.append(")");
		return b.toString();
	}

	// ~ public String bigString () {
	// ~ StringBuffer sb = new StringBuffer (toString ());
	// ~ sb.append (" colored=");
	// ~ sb.append (isColored ());

	// ~ return sb.toString ();
	// ~ }

	public ProjectManager getProjectManager() {
		return projectManager;
	}

	/**
	 * Returns true if the StepButton should be disposed (project closed...).
	 */
	public boolean shouldBeDisposed() {
		try {
			if (step == null) {
				return true;
			}
			if (step.getScene() == null) {
				return true;
			}
			if (step.getProject() == null) {
				return true;
			}
			if (!Engine.getInstance().getSession().getProjects().contains((step.getProject()))) {
				return true;
			}
		} catch (Exception e) {
			// Abnormal situation -> dispose
			return true;
		}

		return false;
	}

}
