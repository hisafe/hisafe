/** 
 * Capsis - Computer-Aided Projections of Strategies in Silviculture
 * 
 * Copyright (C) 1999-2011 INRA 
 * 
 * Author: F. de Coligny
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
package capsis.gui;

import java.awt.AWTEvent;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Container;
import java.awt.Cursor;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.AWTEventListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.beans.PropertyVetoException;

import javax.swing.JInternalFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.SwingUtilities;
import javax.swing.plaf.basic.BasicInternalFrameUI;

import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * A replacement for internal frames titlebar.
 * 
 * @author F. de Coligny - November 2011
 */
public class SmartInternalFrame extends JInternalFrame {

	/**
	 * GlassPane tutorial "A well-behaved GlassPane"
	 * http://weblogs.java.net/blog/alexfromsun/
	 * <p/>
	 * This is the final version of the GlassPane it is transparent for
	 * MouseEvents, and respects underneath component's cursors by default, it
	 * is also friedly for other users, if someone adds a mouseListener to this
	 * GlassPane or set a new cursor it will respect them
	 * 
	 * @author Alexander Potochkin
	 */
	static private class FinalGlassPane extends JPanel implements
			AWTEventListener {
		private final JInternalFrame internalFrame;
		private Point point = new Point();

		public FinalGlassPane(JInternalFrame internalFrame) {
			super(null);
			this.internalFrame = internalFrame;
			setOpaque(false);

		}

		public void setPoint(Point point) {
			this.point = point;
		}

		protected void paintComponent(Graphics g) {
			// super.paintComponent(g);

			// // green shapes for testing
			// Graphics2D g2 = (Graphics2D) g;
			// g2.setColor(Color.GREEN.darker());
			// g2.setComposite(AlphaComposite.getInstance(
			// AlphaComposite.SRC_OVER, 0.7f));
			// int d = 22;
			// g2.fillRect(getWidth() - d, 0, d, d);
			// if (point != null) {
			// g2.fillOval(point.x + d, point.y + d, d, d);
			// }
			// g2.dispose();

		}

		public void eventDispatched(AWTEvent event) {
			if (event instanceof MouseEvent) {

				MouseEvent me = (MouseEvent) event;
				if (!SwingUtilities.isDescendingFrom(me.getComponent(),
						internalFrame)) {
					return;
				}
				if (me.getID() == MouseEvent.MOUSE_EXITED
						&& me.getComponent() == internalFrame) {
					point = null;
				} else {
					MouseEvent converted = SwingUtilities
							.convertMouseEvent(me.getComponent(), me,
									internalFrame.getGlassPane());
					point = converted.getPoint();
				}
				repaint();
			}
		}

		/**
		 * If someone adds a mouseListener to the GlassPane or set a new cursor
		 * we expect that he knows what he is doing and return the
		 * super.contains(x, y) otherwise we return false to respect the cursors
		 * for the underneath components
		 */
		public boolean contains(int x, int y) {
			if (getMouseListeners().length == 0
					&& getMouseMotionListeners().length == 0
					&& getMouseWheelListeners().length == 0
					&& getCursor() == Cursor
							.getPredefinedCursor(Cursor.DEFAULT_CURSOR)) {
				return false;
			}
			
			return super.contains(x, y);

		}
	}

	private FinalGlassPane glassPane;
	private LinePanel controls; // a control panel with buttons
	private JLabel close;

	/**
	 * Constructor. 
	 */
	public SmartInternalFrame () {

		FrameActivator fa = new FrameActivator (this);
		this.addMouseListener(fa); // to show / hide controls when the mouse enters / exits

		glassPane = new FinalGlassPane(this) {};
		glassPane.addMouseListener(fa);
		this.setGlassPane(glassPane);

		AWTEventListener al = (AWTEventListener) glassPane;
		Toolkit.getDefaultToolkit().addAWTEventListener(al,
				AWTEvent.MOUSE_MOTION_EVENT_MASK | AWTEvent.MOUSE_EVENT_MASK);

		glassPane.setVisible (true);

		// Remove the frame title bar
		BasicInternalFrameUI ui = (BasicInternalFrameUI) this.getUI();
		ui.setNorthPane(null); // Remove the title bar

		prepareControls();
		showControls(false);
	}

	@Override
	public void setSelected (boolean v) throws PropertyVetoException {
		super.setSelected (v);
		// Show / hide controls
		showControls (v);
	}
	
	/**
	 * Frame activation management
	 */
	private static class FrameActivator extends MouseAdapter {
		private final JInternalFrame frame;

		public FrameActivator(JInternalFrame frame) {
			this.frame = frame;
		}

		public void mousePressed (MouseEvent evt) {
			Container c = frame.getContentPane();
			if (c instanceof MouseListener) { // redirect to contentPane (popups...)
				((MouseListener) c).mousePressed (evt);
			}
		}
		
		public void mouseEntered(MouseEvent e) {
			try {
				frame.setSelected(true);
			} catch (PropertyVetoException e1) {} // no matter
		}
	}

	private void showControls(boolean v) {
		controls.setVisible(v);
	}

	private void prepareControls() {
		glassPane.setLayout(new BorderLayout());

		controls = new LinePanel();

		close = new JLabel("<html><u>" + Translator.swap("Shared.close")
				+ "</u></html>");
		close.setBackground(Color.WHITE);
		close.setOpaque(false); // transparent
		close.addMouseListener(new MouseAdapter() {
			@Override
			public void mouseClicked(MouseEvent e) {
				System.out.println("SmartIFTitleBar mouseClicked...");
				closeAction();
			}
		});

		controls.add(close);
		controls.addGlue();
		controls.setOpaque(false); // transparent

		glassPane.add(controls, BorderLayout.NORTH);
	}

	public void closeAction() {
		try {
			this.setClosed(true);
		} catch (PropertyVetoException e) {
		} // do nothing
	}

}
