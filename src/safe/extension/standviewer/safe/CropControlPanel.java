/*
 * Capsis 4 - Computer-Aided Projections of Strategies in Silviculture
 *
 * Copyright (C) 2000-2001  Francois de Coligny
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * See the GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package safe.extension.standviewer.safe;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.Rectangle;
import java.awt.Shape;
import java.awt.Window;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

import javax.swing.JDialog;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JPanel;

import jeeb.lib.util.AmapTools;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import safe.model.SafePlot;
import capsis.commongui.util.Tools;
import capsis.util.InfoDialog;
/**
 * CropControlPanel - A component to see results on CROP CONTROL cell
 *
 * @author I Lecomte - July 2003
 */
public class CropControlPanel extends JPanel {
// review - fc - 11.3.2004

	private JPanel greenSquare;
	private InfoDialog infoDialog;


	/**	Constructor.
	*/
	public CropControlPanel (Color color, SVSafe svSafe) {
		super (new BorderLayout ());

		LinePanel l1 = new LinePanel ();

		final Color finalColor = color;

		greenSquare = new JPanel () {
			//private Shape witnessCell = new Rectangle.Double (30, 5, 30, 30);
			private Shape witnessCell = new Rectangle.Double (0, 0, 30, 30);
			public void paintComponent (Graphics g) {
				Graphics2D g2 = (Graphics2D) g;
				g2.setColor (finalColor);
				g2.fill (witnessCell);
				g2.setColor (Color.BLACK);
				g2.draw (witnessCell);
			}
		};
		greenSquare.setOpaque (false);

		final SVSafe finalSvSafe = svSafe;
		greenSquare.addMouseListener (new MouseAdapter () {
			public void mouseClicked (MouseEvent e) {
				if (e.getClickCount () == 2) {
					SafePlot plot = (SafePlot) finalSvSafe.getStepButton ()
							.getStep ().getScene ().getPlot ();
				}
			}
		});
		greenSquare.setMinimumSize (new Dimension (40, 40));
		greenSquare.setPreferredSize (new Dimension (40, 40));
		greenSquare.setSize (new Dimension (40, 40));

		JLabel text = new JLabel (Translator.swap (
				"SVSafe.OptionPanel.SafeCropControl"));

		l1.add (new JWidthLabel ("", 5));
		l1.add (greenSquare);
		l1.add (text);
		l1.addGlue ();

		l1.setPreferredSize (new Dimension (40+text.getPreferredSize ().width, 40));

		l1.setBackground (Color.WHITE);

		add (l1, BorderLayout.NORTH);

	}


	/**	Show an inspector for crop control cell
	*	for current step
	*/
	protected void showInfo (JPanel pan) {	// fc - 11.3.2004

		if (pan == null) {
			if (infoDialog != null) {
				infoDialog.setVisible (false);
				infoDialog.close ();
				infoDialog.dispose ();
				infoDialog = null;
			}
			return;
		}

		if (infoDialog == null) {
			Window w = Tools.getWindow (this);
			if (w instanceof JDialog) {
				infoDialog = new InfoDialog ((JDialog) w);
			} else if (w instanceof JFrame) {
				infoDialog = new InfoDialog ((JFrame) w);
			} else {
				infoDialog = new InfoDialog ();
			}

			infoDialog.addWindowListener (new WindowAdapter (){
				public void windowClosing (WindowEvent evt) {
					infoDialog.setVisible (false);
					infoDialog.close ();
					infoDialog.dispose ();
					infoDialog = null;
				}
			});

			// fc - 27.2.2004 - use positionner is difficult because an info dialog can
			// open another infodialog...
			//
			infoDialog.setLocationRelativeTo (this.getParent ());
			infoDialog.setTitle (Translator.swap ("Shared.info"));
			infoDialog.setSize (new Dimension (200, 200));
			infoDialog.setResizable (true);

			infoDialog.getContentPane ().setLayout (new GridLayout (1, 1));
			infoDialog.getContentPane ().add (pan);	// don't pack / resize / reposition

			infoDialog.pack ();
			infoDialog.setVisible (true);

		} else {
            infoDialog.getContentPane ().removeAll ();
            infoDialog.getContentPane ().add (pan);	// don't pack / resize / reposition

			infoDialog.setVisible (true);

		}

	}


}

