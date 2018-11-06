package safe.gui;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JPanel;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Translator;

/**
 * SafeTreeSpeciesDialog is a dialog box for TREE SPECIES parameters VISUALISATION.
 *
 * @author Isabelle LECOMTE - April 2003
 */
public class SafeTreeSpeciesDialog extends AmapDialog implements ActionListener {

	/**
	 * Constructor.
	 */
	public SafeTreeSpeciesDialog (JPanel select) {
		super ();

		// Activate AmapDialog features
		activateSizeMemorization (getClass ().getName ());
		activateLocationMemorization (getClass ().getName ());

		createUI (select);
		// location is set by AmapDialog
		setSize (new Dimension (400, 500));
		show ();
	}


	public void actionPerformed (ActionEvent evt) {

	}

	/**
	 * Initializes the GUI.
	 */
	private void createUI (JPanel select) {

		getContentPane ().setLayout (new BorderLayout ());

		getContentPane ().add (select, BorderLayout.CENTER);

		setTitle (Translator.swap ("SafeDTreeSpeciesParameters.title"));
		
		setModal (true);
	}


}





