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
package capsis.extension.ioformat;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Check;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Question;
import jeeb.lib.util.Translator;
import capsis.commongui.util.Helper;
import capsis.gui.MainFrame;

/**	Configuration dialog for Pp3LogWithJuvenileWoodExport.
*	From SpagediExportDialog
*	@author F. de Coligny - june 2005
*/
public class JaegerBlobFormatDialog extends AmapDialog implements ActionListener {

	private JTextField fldTopGirth;
	private double topGirth;
	private JTextField fldStumpHeight;
	private double stumpHeight;
	private JTextField fldTop1Girth;
	private double top1Girth;
	private JTextField fldLog1Length;
	private double log1Length;
	private JTextField fldTop2Girth;
	private double top2Girth;
	private JTextField fldLog2Length;
	private double log2Length;
	private double top3Girth;
	private JTextField fldLog3Length;
	private double log3Length;

	private JButton ok;
	private JButton cancel;
	private JButton help;


	/**	Dialog construction.
	*/
	public JaegerBlobFormatDialog () {

		setTitle (Translator.swap ("JaegerBlobFormatDialog.title"));
		createUI ();
		pack ();
		setModal (true);
		show ();
	}


	/**	Checks before leaving on Ok.
	*/
	public void okAction () {

		// Check request coherence
		//
		if (Check.isEmpty (fldTopGirth.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.topGirthIsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		} else if (!Check.isDouble (fldTopGirth.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.topGirthIsNotFloat"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (Check.doubleValue (fldTopGirth.getText ()) < 0d) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.topGirthMustBeGreaterOrEqualTo0"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		if (Check.isEmpty (fldStumpHeight.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.stumpHeightIsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		} else if (!Check.isDouble (fldStumpHeight.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.stumpHeightIsNotFloat"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (Check.doubleValue (fldStumpHeight.getText ()) < 0.30) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.stumpHeightMustBeGreaterOrEqualTo0.30"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		if (Check.isEmpty (fldLog1Length.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.log1LengthIsEmpty"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		} else if (!Check.isDouble (fldLog1Length.getText ())) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.log1LengthIsNotFloat"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}
		if (Check.doubleValue (fldLog1Length.getText ()) < 0.10) {
			JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.logLengthMustBeGreaterOrEqualTo0.10"),
				Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
			return;
		}

		if (!Check.isEmpty (fldLog2Length.getText ())) {
			if (!Check.isDouble (fldLog2Length.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.log2LengthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldLog2Length.getText ()) < 0.10) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.logLengthMustBeGreaterOrEqualTo0.10"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.isEmpty (fldTop1Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top1GirthIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			} else if (!Check.isDouble (fldTop1Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top1GirthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldTop1Girth.getText ()) <= Check.doubleValue (fldTopGirth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top1GirthIsTooSmall"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
		} else if (!Check.isEmpty (fldTop1Girth.getText ())) {
			if (!Check.isDouble (fldTop1Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top1GirthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldTop1Girth.getText ()) <= Check.doubleValue (fldTopGirth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top1GirthIsTooSmall"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
		}

		if (!Check.isEmpty (fldLog3Length.getText ())) {
			if (!Check.isDouble (fldLog3Length.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.log3LengthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldLog3Length.getText ()) < 0.10) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.logLengthMustBeGreaterOrEqualTo0.10"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.isEmpty (fldTop2Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			} else if (!Check.isDouble (fldTop2Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldTop2Girth.getText ()) >= Check.doubleValue (fldTop1Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsTooBig"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldTop2Girth.getText ()) <= Check.doubleValue (fldTopGirth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsTooSmall"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.isEmpty (fldLog2Length.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.log2LengthIsEmpty"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			} else if (!Check.isDouble (fldLog2Length.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.log2LengthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldLog2Length.getText ()) < 0.10) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.logLengthMustBeGreaterOrEqualTo0.10"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
		} else if (!Check.isEmpty (fldTop2Girth.getText ())) {
			if (!Check.isDouble (fldTop2Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsNotFloat"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldTop2Girth.getText ()) <= Check.doubleValue (fldTopGirth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsTooSmall"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
			if (Check.doubleValue (fldTop2Girth.getText ()) >= Check.doubleValue (fldTop1Girth.getText ())) {
				JOptionPane.showMessageDialog (this, Translator.swap ("JaegerBlobFormatDialog.top2GirthIsTooBig"),
					Translator.swap ("Shared.warning"), JOptionPane.WARNING_MESSAGE );
				return;
			}
		}


		// 2. retrieve the collected data
		topGirth = Check.doubleValue (fldTopGirth.getText ());
		stumpHeight = Check.doubleValue (fldStumpHeight.getText ());
		log1Length = Check.doubleValue (fldLog1Length.getText ());
		if (!Check.isEmpty (fldLog2Length.getText ())) log2Length = Check.doubleValue (fldLog2Length.getText ());
		else log2Length = log1Length;
		if (!Check.isEmpty (fldLog3Length.getText ())) log3Length = Check.doubleValue (fldLog3Length.getText ());
		else log3Length = log2Length;
		top3Girth = topGirth;
		if (!Check.isEmpty (fldTop2Girth.getText ())) top2Girth = Check.doubleValue (fldTop2Girth.getText ());
		else top2Girth = topGirth;
		if (!Check.isEmpty (fldTop1Girth.getText ())) top1Girth = Check.doubleValue (fldTop1Girth.getText ());
		else top1Girth = topGirth;

		Log.println ("topGirth : "+topGirth+", stumpHeight : "+stumpHeight+", log1Length : "+log1Length+", top1Girth : "+top1Girth);
		Log.println ("log2Length : "+log2Length+", top2Girth : "+top2Girth+", log3Length : "+log3Length+", top3Girth : "+top3Girth);

		setValidDialog (true);
	}


	/**	From ActionListener interface.
	*/
	public void actionPerformed (ActionEvent evt) {

		if (evt.getSource ().equals (ok)) {
			okAction ();
		} else if (evt.getSource ().equals (cancel)) {
			setValidDialog (false);
		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}
	}


	/**	Called on escape : ask for confirmation.
	*/
	protected void escapePressed () {

		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("JaegerBlobFormatDialog.confirm"), Translator.swap ("JaegerBlobFormatDialog.confirmClose"))) {
			dispose ();
		}
	}


	/**	Initializes the GUI.
	*/
	private void createUI () {

		topGirth = 20;
		stumpHeight = 0.65;
		top1Girth = 150;
		log1Length = 2.6;
		log2Length = 2.1;

		Border etched = BorderFactory.createEtchedBorder ();

		Box box1 = Box.createVerticalBox ();
		Border bor1 = BorderFactory.createTitledBorder (etched, Translator.swap ("JaegerBlobFormatDialog.comment1"));
		box1.setBorder (bor1);
//		JPanel comment1Line = new JPanel (new FlowLayout (FlowLayout.LEFT));
//		comment1Line.add (new JLabel (Translator.swap ("JaegerBlobFormatDialog.comment1")));

		JPanel topGirthLine = new JPanel (new FlowLayout (FlowLayout.LEFT));
		fldTopGirth = new JTextField (5);
		if (topGirth > 0) fldTopGirth.setText (""+topGirth);
		topGirthLine.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.topGirth")+" :", 300));
		topGirthLine.add (fldTopGirth);

		JPanel stumpHeightLine = new JPanel (new FlowLayout (FlowLayout.LEFT));
		fldStumpHeight = new JTextField (5);
		if (stumpHeight > 0) fldStumpHeight.setText (""+stumpHeight);
		stumpHeightLine.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.stumpHeight")+" :", 300));
		stumpHeightLine.add (fldStumpHeight);

		JPanel legendLine = new JPanel (new FlowLayout (FlowLayout.LEFT));
		legendLine.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.log"), 100));
		legendLine.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.logLength"), 120));
		legendLine.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.logTopGirth"), 120));

		JPanel log1Line = new JPanel (new FlowLayout (FlowLayout.LEFT));
		log1Line.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.firstLog"), 100));
		fldLog1Length = new JTextField (5);
		if (log1Length > 0) fldLog1Length.setText (""+log1Length);
		fldTop1Girth = new JTextField (5);
		if (top1Girth > 0) fldTop1Girth.setText (""+top1Girth);
		log1Line.add (fldLog1Length);
		log1Line.add (new JWidthLabel ("", 60));
		log1Line.add (fldTop1Girth);
		log1Line.add (new JWidthLabel ("", 10));
		log1Line.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.firstLogComment"), 100));

		JPanel log2Line = new JPanel (new FlowLayout (FlowLayout.LEFT));
		log2Line.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.secondLog"), 100));
		fldLog2Length = new JTextField (5);
		if (log2Length > 0) fldLog2Length.setText (""+log2Length);
		fldTop2Girth = new JTextField (5);
		if (top2Girth > 0) fldTop2Girth.setText (""+top2Girth);
		log2Line.add (fldLog2Length);
		log2Line.add (new JWidthLabel ("", 60));
		log2Line.add (fldTop2Girth);
		log2Line.add (new JWidthLabel ("", 10));
		log2Line.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.firstLogComment"), 100));

		JPanel log3Line = new JPanel (new FlowLayout (FlowLayout.LEFT));
		log3Line.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.otherLog"), 100));
		fldLog3Length = new JTextField (5);
		if (log3Length > 0) fldLog3Length.setText (""+log3Length);
		log3Line.add (fldLog3Length);
		log3Line.add (new JWidthLabel ("", 60));
		log3Line.add (new JWidthLabel (Translator.swap ("JaegerBlobFormatDialog.otherLogComment"), 100));

		JPanel comment2Line = new JPanel (new FlowLayout (FlowLayout.LEFT));
		comment2Line.add (new JLabel (Translator.swap ("JaegerBlobFormatDialog.comment2")));

//		box1.add (comment1Line);
		box1.add (topGirthLine);
		box1.add (stumpHeightLine);
		box1.add (legendLine);
		box1.add (log1Line);
		box1.add (log2Line);
		box1.add (log3Line);
		box1.add (comment2Line);
		getContentPane ().add (box1, BorderLayout.CENTER);

		//2. Control panel (ok cancel help);
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		ok = new JButton (Translator.swap ("Shared.ok"));
		cancel = new JButton (Translator.swap ("Shared.cancel"));
		help = new JButton (Translator.swap ("Shared.help"));
		pControl.add (ok);
		pControl.add (cancel);
		pControl.add (help);
		ok.addActionListener (this);
		cancel.addActionListener (this);
		help.addActionListener (this);

		// sets ok as default (see AmapDialog)
		ok.setDefaultCapable (true);
		getRootPane ().setDefaultButton (ok);

		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}


	public double getStumpHeight () {
		return stumpHeight;
	}


	public double getTopGirth () {
		return topGirth;
	}


	public double getLog1Length () {
		return log1Length;
	}


	public double getTop1Girth () {
		return top1Girth;
	}


	public double getLog2Length () {
		return log2Length;
	}


	public double getTop2Girth () {
		return top2Girth;
	}


	public double getLog3Length () {
		return log3Length;
	}


	public double getTop3Girth () {
		return top3Girth;
	}


}
