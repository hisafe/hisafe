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

import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.GraphicsEnvironment;
import java.util.Vector;

import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jeeb.lib.util.Translator;


/**
 * Allows to choose a font. Can be used in a dialog box or directly as a panel.
 *
 * @author F. de Coligny - october 2000
 */
public class FontChooser extends JPanel implements ListSelectionListener{
	private String font;
	private int style;
	private int size;
	private JList listFont;
	private JList listStyle;
	private JList listSize;

	public FontChooser (Font defaultFont) {
		// default values
		font = defaultFont.getName ();
		style = defaultFont.getStyle ();
		size = defaultFont.getSize ();
		
		String[] fonts = GraphicsEnvironment.getLocalGraphicsEnvironment ().getAvailableFontFamilyNames ();
		listFont = new JList (fonts);
		listFont.addListSelectionListener (this);
		listFont.setVisibleRowCount (6);
		listFont.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		JScrollPane scroll1 = new JScrollPane (listFont);
		listFont.setSelectedValue (font, true);

		Vector styles = new Vector ();
		styles.add ("Plain");
		styles.add ("Bold");
		styles.add ("Italic");
		listStyle = new JList (styles);
		listStyle.addListSelectionListener (this);
		listStyle.setVisibleRowCount (6);
		listStyle.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		JScrollPane scroll2 = new JScrollPane (listStyle);
		String buf = "";
		if (style == Font.PLAIN) {
			buf = "Plain";
		} else if (style == Font.BOLD) {
			buf = "Bold";
		} else {
			buf = "Italic";
		}
		listStyle.setSelectedValue (buf, true);

		Vector sizes = new Vector ();
		sizes.add ("6");
		sizes.add ("8");
		sizes.add ("10");
		sizes.add ("12");
		sizes.add ("14");
		sizes.add ("16");
		sizes.add ("18");
		sizes.add ("20");
		listSize = new JList (sizes);
		listSize.addListSelectionListener (this);
		listSize.setVisibleRowCount (6);
		listSize.setSelectionMode (ListSelectionModel.SINGLE_SELECTION);
		JScrollPane scroll3 = new JScrollPane (listSize);
		listSize.setSelectedValue (""+size, true);
		
		setLayout (new BoxLayout (this, BoxLayout.X_AXIS));
		
		Box box1 = Box.createVerticalBox ();
//		box1.setSize (new Dimension (150, 300));
		JPanel p1 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p1.add (new JLabel (Translator.swap ("FontChooser.font")+" :"));
		box1.add (p1);
		box1.add (scroll1);
		
		Box box2 = Box.createVerticalBox ();
//		box2.setSize (new Dimension (150, 300));
		JPanel p2 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p2.add (new JLabel (Translator.swap ("FontChooser.style")+" :"));
		box2.add (p2);
		box2.add (scroll2);
		
		Box box3 = Box.createVerticalBox ();
//		box3.setSize (new Dimension (150, 300));
		JPanel p3 = new JPanel (new FlowLayout (FlowLayout.LEFT));
		p3.add (new JLabel (Translator.swap ("FontChooser.size")+" :"));
		box3.add (p3);
		box3.add (scroll3);
		
		add (box1);
		add (box2);
		add (box3);
		
//		setPreferredSize (new Dimension (600, 500));

	}

	public void valueChanged(ListSelectionEvent evt) {
		JList src = (JList) evt.getSource ();
		String value = (String) src.getSelectedValue ();
		if (src.equals (listFont)) {
			font = value;
		} else if (src.equals (listStyle)) {
			if (value.equals ("Plain")) {
				style = Font.PLAIN;
			} else if (value.equals ("Bold")) {
				style = Font.BOLD;
			} else if (value.equals ("Italic")) {
				style = Font.ITALIC;
			}
		} else {
			size = Integer.parseInt (value);
		}			
	}

	public Font getFont () {
		Font f = new Font (font, style, size);
		return f;
	}


}

