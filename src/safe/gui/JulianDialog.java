package safe.gui;

import java.awt.Container;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.StringTokenizer;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JDialog;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.Border;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.Translator;


/**
 * This class is a AmapDialog. It allows to fill the given JTable with a value
 * choosed by means of this interface.
 * Its interface is splited in two parts: One part for the Greorian date description (dd/month//yyyy)
 * and the other for the Julian one (DOY)
 * Changes on one of them affect the value of the other one.
 */
public class JulianDialog extends AmapDialog implements ActionListener {

	private JTable jTable = null;
	//Calendar objects
	private JComboBox jours, mois, an;
	private JTextField jourJulien;
	private JButton ok;
	private JButton cancel;
	//internal tips
	private int [] nbJours = {31,28,31,30,31,30,31,31,30,31,30,31};
	//calling object informations
	private int row = 0; //(when JTable)
	private int col = 0;
	private Object o = null;



////CONSTRUCTOR
	public JulianDialog(Container dial, JTable jt,int row, int col){

		super((JDialog)dial);
		//save the calling cell coordinates and container
		this.row = row; this.col = col;
		this.jTable = jt;
		//gui
		createUI();

		pack ();
		show ();
	}

	public JulianDialog(Container dial, Object o){

		super((JDialog)dial);
		this.o = o;
		//gui
		createUI();

		pack ();
		show ();
		System.out.println("affiché");
	}



////INTERFACE
	private void createUI(){
	//3 ComboBox, 1 JTextField, OK et Cancel

		Box boite = Box.createVerticalBox();
		//Bords
		Border etched = BorderFactory.createEtchedBorder ();

		//Calendrier GREGORIEN
		JPanel fond = new JPanel(new FlowLayout());
		Border borDate = BorderFactory.createTitledBorder (etched, "Calendrier Gregorien");
		fond.setBorder (borDate);
		//jours
		jours = new JComboBox();	rempli(jours,31);
		jours.addActionListener(this);
		//mois
		 StringTokenizer st = new StringTokenizer(Translator.swap ("JulianDialog.month"));
		 int nb = st.countTokens();
		String [] m = new String[nb];
		int i=0;
		while (i<nb) {	 m[i] = st.nextToken(); i++; }
		mois = new JComboBox(m);
		mois.addActionListener(this);
		//an
		an = new JComboBox();		rempli(an,1900,2400);
		an.setSelectedIndex(104);
		an.addActionListener(this);
		fond.add(jours);
		fond.add(mois);
		fond.add(an);

		//Calendrier JULIEN
		JPanel fond2 = new JPanel(new FlowLayout());
		Border borJulien = BorderFactory.createTitledBorder (etched, "Calendrier Julien");
		fond2.setBorder (borJulien);
		//Jour julien
		jourJulien = new JTextField(5);
		jourJulien.addActionListener(this);
		if (this.jTable!=null){
			Object premVal = jTable.getValueAt(row,col);
			if (premVal==null) {jourJulien.setText("1");}
			else {
				jourJulien.setText(""+premVal);
				actualiseGregorien();
			}
		}
		else {
			jourJulien.setText("1");
		}
		fond2.add(jourJulien);

		//CONTROLES
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		//-OK/Cancel
		ok = new JButton (Translator.swap ("Shared.ok"));			ok.addActionListener (this);
		cancel = new JButton (Translator.swap ("Shared.cancel"));	cancel.addActionListener (this);
		pControl.add (ok);
		pControl.add (cancel);
		boite.add(fond);
		boite.add(fond2);
		boite.add(pControl);

		this.getContentPane().add(boite);
	}



////EVENTS MANAGMENT
	public void actionPerformed (ActionEvent e) {

		//OK
		if 		(e.getSource().equals(ok)) 			okAction();
		//CANCEL
		else if (e.getSource().equals(cancel))		setValidDialog (false);
		//GREGORIAN Calendar
		//DAYS
		else if (e.getSource().equals(jours)) 		actualiseJulien();
		//MONTH or YEARS
		else if (e.getSource().equals(mois)	|| e.getSource().equals(an)){
				//save the day selected in the (gregorian) day combobox
				int j = jours.getSelectedIndex();
				//empty the day combobox
				jours.removeAllItems();
				//IF ferbruary AND bissextil year
				if ( mois.getSelectedIndex()==1 && isBissextile(Integer.parseInt(""+an.getSelectedItem()))) {
					//THEN fill 29 days
					rempli(jours,29);
					//load the (february) day selected in the (gregorian) day combobox
					if (j<29) jours.setSelectedIndex(j);
				}
				else{
					//ELSE fill with the correct number of day of the actual month
					int m = nbJours[mois.getSelectedIndex()];
					rempli(jours,m);
					//load the day selected in the (gregorian) day combobox
					if (j<m) jours.setSelectedIndex(j);
				}
				//refresh the julain calendar (JTextField)
				actualiseJulien();
		}

		//JULIAN Calendar
		//JULIAN DAYS
		else if (e.getSource().equals(jourJulien)) 	{
			int jj = Integer.parseInt(jourJulien.getText());
			//IF Julian Day is valid
			if (jj>0 && jj<=366){
				//refresh the Gregorian calendar
				actualiseGregorien();
			}
			//ESLE
			else{
				//Display an error message dialog
				JOptionPane.showMessageDialog(this , Translator.swap ("JulianDialog.invalid"));
			}
		}
	}







////ACTIONS
	private void okAction () {
	//IF choosen day is valid set the value into the JTable and close this
	//ELSE display an error message dialog

		int jj = Integer.parseInt(jourJulien.getText());
		//IF julian day is valid
		if (jj>0 && jj<=366){
			//set the value into the Calling object
			if (jTable!=null) {
				jTable.setValueAt(jourJulien.getText(),row,col);
			}
			else if (o!=null){
				((JTextField)o).setText(jourJulien.getText());
			}
			//and close this
			this.setValidDialog(true);
		}
		//ELSE display an error message dialog
		else{
			JOptionPane.showMessageDialog(this , Translator.swap ("JulianDialog.invalid"));
		}
	}


////CALENDAR MODIFICATIONS
	private void actualiseJulien(){
	//Set the Julian Calendar with the actual Gregorian Date

		//Get data from interface
		int jour = this.jours.getSelectedIndex() + 1;
		int mois = this.mois.getSelectedIndex() + 1;
		int an 	 = Integer.parseInt((String)this.an.getSelectedItem());
		//bissextil year managment
		if (isBissextile(an)) nbJours[1] = 29;

		//we begin from zero
		int jj = 0;
		//for each months passed, add the matching number of day
		for (int i=0; i<mois-1; i++) jj +=nbJours[i];
		//and finally for the curant month
		jj += jour;
		//set the julian JTextField
		jourJulien.setText(""+jj);

		//reset the usual values
		nbJours[1] = 28;
	}


	private void actualiseGregorien(){
	//Actualise le Calendrier Grégorien avec le JourJulien actuel

			//nb = julian day
			int nb = Integer.parseInt(jourJulien.getText());
			int an 	 = Integer.parseInt((String)this.an.getSelectedItem());
			int mois = 0;
			int jours = 0;
			//bissextil year managment
			if (isBissextile(an)) nbJours[1] = 29;

			//nb décrementation : remove the number of day for each month
			//(counting the month)
			while (nb>nbJours[mois]){
				nb = nb-nbJours[mois];
				mois++;
			}

			//count the remaining days
			jours = nb-1;

			//actualise ComboBoxes
			this.mois.setSelectedIndex(mois);
			this.jours.setSelectedIndex(jours);

			//reset the usual values
			nbJours[1] = 28;
	}


////MODIFICATION functions
	private void rempli(JComboBox jcb, int i){
	//fill given combobox from 1 to i
		for (int j=1; j<=i; j++) jcb.addItem(""+j);
	}

	private void rempli(JComboBox jcb, int i, int j){
	//fill given combobox from i to j
			for (int k=i; k<=j; k++) jcb.addItem(""+k);
	}


////TEST
	private boolean isBissextile(int i){
	//return true if the given year is bissextil
		return ( (i%4 == 0) && ( (i%100 != 0) || (i%400 == 0) ) );
	}

}
