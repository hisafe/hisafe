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
package capsis.util;

import java.awt.BasicStroke;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Rectangle;
import java.awt.RenderingHints;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Rectangle2D;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.JButton;
import javax.swing.JCheckBox;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.FileAccessory;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.PathManager;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.gui.MainFrame;

/**	A class to export a drawer's drawing in an image file with a given resolution.
*	@author  F. de Coligny - 12.1.2005
*/
public class ExportDrawer extends AmapDialog implements ActionListener {
	private Drawer drawer;
	private Rectangle.Double userRectangle;	// userXMin, userYMin, userWidth, userHeight (meters)
	
	private Object[] candidateFormats = {"JPEG", "PNG"};
	private Object[] formatExtensions = {"jpeg", "png"};	// order must match candidateFormats
	
	private JTextField pixelWidth;
	private JCheckBox fromBlankBackground;
	private JCheckBox antiAliased;
	
	private JTextField dirName;
	private JTextField filePrefix;
	private JButton browse;
	private JComboBox format;
	private JButton ok;
	private JButton cancel;
	private JButton help;
	
	/**	Creates a new instance of ExportDrawer 
	*/
	public ExportDrawer (Drawer drawer, Rectangle.Double userRectangle, JDialog mother) {
		super (mother);
		init (drawer, userRectangle);
	}

	/**	Creates a new instance of ExportDrawer 
	*/
	public ExportDrawer (Drawer drawer, Rectangle.Double userRectangle, JFrame mother) {
		super (mother);
		init (drawer, userRectangle);
	}
	
	/**	Creates a new instance of ExportDrawer 
	*/
	public ExportDrawer (Drawer drawer, Rectangle.Double userRectangle) {
		super (MainFrame.getInstance ());
		init (drawer, userRectangle);
	}
	
	//	Constructors call this method : construction
	//
	private void init (Drawer drawer, Rectangle.Double userRectangle) {
		this.drawer = drawer;
		this.userRectangle = userRectangle;
		setTitle (Translator.swap ("ExportDrawer"));
		
		// check default path
		String path = Settings.getProperty ("capsis.export.dir", "");
		if (path == null || path.length () == 0) {
			Settings.setProperty ("capsis.export.dir", PathManager.getDir("tmp"));
		}
		
		// check default path
		String prefix = Settings.getProperty ("capsis.export.file.prefix", "");
		if (prefix == null || prefix.length () == 0) {
			Settings.setProperty ("capsis.export.file.prefix", "export");
		}
		
		createUI ();
		setModal(true);
		pack ();
		show ();
	}
	
	/**	Exports the given drawer into the file with the required format 
	*/
	static public void export (Drawer drawer, boolean blankBackground, boolean antiAliased, 
			Rectangle.Double userBounds, int pixelWidth, 
			String fileName, String format) throws Exception {
		
		double ratio = userBounds.height / userBounds.width;
		int pixelHeight = (int) (pixelWidth * ratio);
		Rectangle pixelBounds = new Rectangle (0, 0, pixelWidth, pixelHeight);
				
		BufferedImage bi = new BufferedImage (pixelWidth, pixelHeight, BufferedImage.TYPE_INT_RGB);
		Graphics g = bi.getGraphics ();
		
		// Set blank background first
		if (blankBackground) {
//~ System.out.println ("ExportDrawer: blankBackground");
			g.setColor (Color.WHITE);
			g.fillRect (0, 0, pixelWidth, pixelHeight);
		}
				
		// Transform the graphics to make the user and pixel coordinates match
		ExportDrawer.prepareGraphics (g, userBounds, pixelBounds, antiAliased);
		
		// ask the drawer to draw in the image
		drawer.draw (g, userBounds);
		
		// write it out in the format you want
		ImageIO.write (bi, format, new File (fileName));	// exception may be thrown
		
		//dispose of the graphics content
		g.dispose ();
	}
    
	static private void prepareGraphics (Graphics g, Rectangle2D.Double userBounds, 
			Rectangle pixelBounds, boolean antiAliased) {
		Graphics2D g2 = (Graphics2D) g;
		
		// Apply scale factor to represent the chosen user zone on
		// the available device zone
		Point2D.Double scaleFactor = ExportDrawer.getScale (userBounds, pixelBounds);

		// Transform : Origin is panel's bottom left corner and Y grows upward
		AffineTransform panelTransform = new AffineTransform ();
		panelTransform.translate (((double)pixelBounds.width)/2, ((double) pixelBounds.height)/2);
		panelTransform.scale (1, -1);
		panelTransform.scale (scaleFactor.x, scaleFactor.y);
		panelTransform.translate (	-userBounds.width/2 -userBounds.x, 
									-userBounds.height/2 -userBounds.y);
		g2.transform (panelTransform);
		
		// Panel's font transform
		AffineTransform fontTransform = new AffineTransform ();
		fontTransform.scale (1, -1);
		fontTransform.scale (1/scaleFactor.x, 1/scaleFactor.y);
		
		Font gFont = g.getFont ();	
		Font userFont = new Font (gFont.getName (), gFont.getStyle (), gFont.getSize ()-2);	// main font used to write in the panel
		g2.setFont (userFont.deriveFont (fontTransform));

		// Like this, pencel size is always one pixel large
		g2.setStroke (new BasicStroke (0f));
		
		//~ Rectangle.Double r = getUserRectangle (getBounds ());
		
		// Not sufficient : outside trees are drawn if nothing is done 
		// (so something can be done in drawer according to the given rectangle 
		// below :-). See SVSimple "preprocesses").
		//~ g2.setClip (r);

		// Set antialiasing and pencil size
		if (antiAliased) {g2.setRenderingHint (RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);}
		g2.setStroke (new BasicStroke (0f));	// pencilSize = 0
	}
	
	/**	Calculate scale, used during graphics preparation, comes to Panel2D.
	*/
	static private Point2D.Double getScale (Rectangle.Double userBounds, Rectangle bounds) {
		
		// fc-5.2.2018 wrong if x/y origin is not zero
//		double w = bounds.getWidth () / userBounds.getWidth ();
//		double h = bounds.getHeight () / userBounds.getHeight ();
		
		// fc-5.2.2018
		double userXSize = userBounds.getWidth () - userBounds.x;
		double userYSize = userBounds.getHeight () - userBounds.y;
		
		double w = bounds.getWidth () / userXSize;
		double h = bounds.getHeight () / userYSize;

		double d = Math.min (w, h);
		Point2D.Double currentScale = new Point2D.Double (d, d);
			
		return currentScale;
	}

	//	Action on ok
	//
	private void okAction () {
		// Checks
		if (Check.isEmpty (pixelWidth.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("ExportDrawer.pixelWidthIsRequired"));
			return;
		}
		int pWidth = 0;
		try {
			Integer w = new Integer (pixelWidth.getText ().trim ());	// trouble => exception
			if (w.intValue () <= 0) {throw new Exception ();}			// more than 0
			pWidth = w.intValue ();
		} catch (Exception e) {
			MessageDialog.print (this, Translator.swap ("ExportDrawer.pixelWidthMustBeAnIntegerGreaterThanZero"));
			return;
		}
		
		if (!Check.isDirectory (dirName.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("ExportDrawer.wrongDirectory"));
			return;
		}
		
		// Checks ok
		String fileName = dirName.getText ().trim ()
				+File.separator
				+filePrefix.getText ().trim ()
				+"."
				+formatExtensions[format.getSelectedIndex ()]
				;
		try {
			ExportDrawer.export (drawer, fromBlankBackground.isSelected (), 
					antiAliased.isSelected (), userRectangle, pWidth, 
					fileName, (String) format.getSelectedItem ());
			
		} catch (Exception e) {
			MessageDialog.print (this, "ExportDrawer: could not write image into file "+fileName+" due to "+e);
			return;		// do not close dialog till user cancels
		}
		
		Settings.setProperty ("capsis.export.dir", dirName.getText ().trim ());
		Settings.setProperty ("capsis.export.file.prefix", filePrefix.getText ().trim ());
		Settings.setProperty ("capsis.export.format", (String) format.getSelectedItem ());
		Settings.setProperty ("capsis.export.drawer.blank.background", ""+fromBlankBackground.isSelected ());
		Settings.setProperty ("capsis.export.drawer.anti.aliased", ""+antiAliased.isSelected ());
		StatusDispatcher.print (Translator.swap ("ExportDrawer.exportSucceddedIntoFile")
				+" "+fileName);
		
		setValidDialog (true);
	}
	
	//	Action on browse
	//
	private void browseAction () {
		JFileChooser chooser = new JFileChooser (
				Settings.getProperty ("capsis.export.dir", (String)null));
		chooser.setFileSelectionMode (JFileChooser.DIRECTORIES_ONLY);
		new FileAccessory (chooser);
		
		//chooser.setFileSelectionMode ();
		int returnVal = chooser.showOpenDialog (this);

		if (returnVal == JFileChooser.APPROVE_OPTION) {
			String fn = chooser.getSelectedFile ().toString ().trim ();
			//Settings.setProperty ("capsis.export.dir", fn);
			dirName.setText (fn);
		}
	}
	
	/**	Actions on button selection
	*/
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (browse)) {
			browseAction ();
		} else if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}
	
	//	User interface creation
	//
	private void createUI () {
		getContentPane ().setLayout (new BorderLayout ());
		
		ColumnPanel part1 = new ColumnPanel ();
		
		// pixelWidth
		LinePanel l0 = new LinePanel ();
		l0.add (new JWidthLabel (Translator.swap ("ExportDrawer.pixelWidth")+" :", 140));
		pixelWidth = new JTextField (25);
		pixelWidth.setText ("500");	// default
		l0.add (pixelWidth);
		part1.add (l0);
		
		// fromBlankBackground
		LinePanel l01 = new LinePanel ();
		fromBlankBackground = new JCheckBox (Translator.swap ("ExportDrawer.fromBlankBackground"));
		fromBlankBackground.setSelected (Settings.getProperty ("capsis.export.drawer.blank.background", false));
		l01.add (fromBlankBackground);
		l01.addGlue ();
		part1.add (l01);
		
		// fromBlankBackground
		LinePanel l02 = new LinePanel ();
		antiAliased = new JCheckBox (Translator.swap ("ExportDrawer.antiAliased"));
		antiAliased.setSelected (Settings.getProperty ("capsis.export.drawer.anti.aliased", false));
		l02.add (antiAliased);
		l02.addGlue ();
		part1.add (l02);
		
		// dirName
		LinePanel l1 = new LinePanel ();
		l1.add (new JWidthLabel (Translator.swap ("ExportDrawer.dirName")+" :", 140));
		dirName = new JTextField (25);
		dirName.setText (Settings.getProperty ("capsis.export.dir", ""));	// default
		l1.add (dirName);
		browse = new JButton (Translator.swap ("Shared.browse"));
		browse.addActionListener (this);
		l1.add (browse);
		part1.add (l1);
		
		// filePrefix
		LinePanel l2 = new LinePanel ();
		l2.add (new JWidthLabel (Translator.swap ("ExportDrawer.filePrefix")+" :", 140));
		filePrefix = new JTextField (25);
		filePrefix.setText (Settings.getProperty ("capsis.export.file.prefix", ""));	// default
		l2.add (filePrefix);
		part1.add (l2);
		
		// format
		LinePanel l3 = new LinePanel ();
		l3.add (new JWidthLabel (Translator.swap ("ExportDrawer.format")+" :", 140));
		format = new JComboBox (candidateFormats);
		String lastSelection = Settings.getProperty ("capsis.export.format", "");
		format.setSelectedItem (lastSelection == null ? candidateFormats[0] : lastSelection);	// default
		l3.add (format);
		part1.add (l3);
		
		JPanel controlPanel = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		ok.addActionListener (this);
		controlPanel.add (ok);
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		cancel.addActionListener (this);
		controlPanel.add (cancel);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		controlPanel.add (help);
		
		getContentPane ().add (part1, BorderLayout.CENTER);
		getContentPane ().add (controlPanel, BorderLayout.SOUTH);
	
	}

}
