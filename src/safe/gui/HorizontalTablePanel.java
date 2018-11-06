package safe.gui;

import java.awt.Container;
import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.Vector;

import javax.swing.BorderFactory;
import javax.swing.BoundedRangeModel;
import javax.swing.Box;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JScrollBar;
import javax.swing.JScrollPane;
import javax.swing.JTable;
import javax.swing.SwingUtilities;
import javax.swing.table.DefaultTableModel;

import jeeb.lib.util.Check;
import jeeb.lib.util.Translator;

/**
 *	This Class is a JPanel. It is used to display the name, values and help message
 * of the given data (array)
 * It allow to display given data (Vector) within an horyzontal table. Titles are given to (vector)
 * they are displayed in the left margin of the jscrollpane.
 *
 * A JButton allow to call a JOptionPane to display help
 *
 * The first line use an Input Interface (associating a dialog box)
 * (here a Gregorian<->Julian calendar converter to chose a day)



 * Cette classe est un JPanel.
 * Elle represente les données passées en paramètres (Vecteur) sous la forme d'une
 * table horizontale. Les titres passé eux aussi en parametres sont affichés dans la marge
 * gauche de la table (dans l'en tête du panneau déroulant)
 *
 * un petit bouton à droite de la table permet d'afficher un dialogue d'aide
 *
 * La premiere ligne utilise une boite de dialogue gérant l'entrée des données
 * (ici un convertisseur julien<->grégorien permettant de choisir un jour)
 */


public class HorizontalTablePanel extends JPanel implements ActionListener{

	//Preferences
	private int marginWidth = 140;
	private int columnsWidth = 0;
	private int nbColMin = 7;

	//Calling dialog (allow to display the inputUI if needed)
	private Dialog 		parent		= null;
	private JScrollPane jScrollPane = null;
	//table and data
	private JPanel		jPanel 		= null;
	private JTable		jTable		= null;
	private Vector		data 		= null;
	private Vector		titles 		= null;


////CONSTRUCTOR
	public HorizontalTablePanel(Dialog parent, Vector data, Vector titles){
		super();
		this.parent 		= parent;
		this.data 			= data;
		this.titles			= titles;
		createUI();
	}

//INTERFACE
	private void createUI(){

		//Layout Manager
		FlowLayout l = new FlowLayout(FlowLayout.LEFT);
		l.setVgap(0);
		this.setLayout(l);
		//JPanel
		JPanel centre = new JPanel(new FlowLayout(FlowLayout.LEFT));
		jPanel = getPanel(data,titles);
		centre.add(jPanel);

		this.add(centre);
	}


////EVENTS MANAGMENT
	public void actionPerformed(ActionEvent e){

	}




////TABLE MODIFICATIONS
	/**
	 * To fill this.data Vector with the JTable data
	 */
	public void actualiseData(){

		data = new Vector();
		for (int i=0; i<jTable.getRowCount(); i++){
			data.add(new Vector());
			for (int j=0; j<jTable.getColumnCount();j++){
				((Vector)data.get(i)).add(jTable.getValueAt(i,j));
			}
		}
	}

	/**
	 * To add a column
	 */
	public void addColumn(){

		//refresh this.data with the JTable values
		actualiseData();
		//add an element to te end of each line of this.data
		for (int i=0; i<data.size(); i++) ((Vector)data.get(i)).add("");
		//set the jscrollpane view with a new Table filled with this.data
		jScrollPane.setViewportView(getTable(data));
		//adjustment of the JscrollPane
		JScrollBar b = jScrollPane.getHorizontalScrollBar();
		BoundedRangeModel bM = b.getModel();
		//the maximum value of the bar is maximum-extent (-1 pour que ça marche)
		b.setValue(bM.getMaximum()-bM.getExtent()-1);
	}


////ACCESSORS
	//set
	/**
	 * To set and display new values
	 * nb. this function MAY be re-written
	 * (it uses 2 JTables to change data (else first JTable disappears))
	 */
	public void setValues(Vector v){

		//If given vector is well formed
		if (v != null && v.size()!=0){
			//get column count (minimum 7 column displayed)
			//to fill a vector with "" (empty string) value
			int taille = ((Vector)v.get(0)).size();
			if (taille<nbColMin) taille = nbColMin;
			Vector titres = new Vector();
			for (int i=0; i<taille;i++) titres.add("");
			//create a new model and set jTable with
			DefaultTableModel monModele = new DefaultTableModel (v,titres);
			jTable.setModel(monModele);
			//(actualise data)
			data = new Vector();
			for (int i=0; i<jTable.getRowCount(); i++){
				data.add(new Vector());
				for (int j=0; j<jTable.getColumnCount();j++){
					((Vector)data.get(i)).add(jTable.getValueAt(i,j));
				}
			}
			//create a second model and set a new jTable with
			monModele = new DefaultTableModel (data,titres); //data
			//create a new JTable
			jTable = new JTable();
			jTable.addMouseListener(new JTableListener(parent,this));
			jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			jTable.setModel(monModele);
			//column size
			for (int i = 0; i <taille; i++) {
				if (columnsWidth>0) jTable.getColumnModel().getColumn(i).setPreferredWidth(columnsWidth);
			}
			jScrollPane.setViewportView(jTable);
		}
	}


	public void setValueAt(Object o,int r,int c) {jTable.setValueAt(o,r,c);}

	//get
	/**
	 * GET a vector filled with the i° line values (RETURN VECTOR)
	 */
	public Vector getLigne(int i){
	//attention : ommission des cellules vides

		Vector tmp = new Vector();
		Object o = null;
		for (int j=0; j<jTable.getColumnCount(); j++){
			o = jTable.getValueAt(i,j);
			if (o!=null && o!="") tmp.add(jTable.getValueAt(i,j));
		}
		return tmp;
	}



	/**
	 * GET this JPanel (RETURN JPANEL)
	 */
	 public JPanel getPanel(){return jPanel;}


	/**
	 * GET a panel which contains a table initialized with data & titles (RETURN JPANEL)
	 */
	public JPanel getPanel(Vector data,Vector titles){

		JPanel resultat = new JPanel(new GridLayout(1,0,0,0));
		//IF there is data to display
		if (data!=null && titles!=null){
		//1 A JPanel for titles (at the left of the table)
			JPanel monEnTeteDeTable = new JPanel(new GridLayout(1,0,0,0));
			Box box = Box.createVerticalBox();
		//2 adding titles to the margin
			JLabel name = null;
			//For each elements of titles vector
			for (int i=0; i<titles.size(); i++){
				//add the corresponding label to the box
				name = new JLabel((String)titles.get(i));
				name.setPreferredSize(new Dimension(marginWidth,20));
				box.add(name);
			}
			//add the box to the JPanel
			monEnTeteDeTable.add(box);

		//3 IF there is at least one element
			if (data.size()!=0){
				//JTable construction
				jTable = getTable(data);
				//JScrollPane construction
				jScrollPane = new JScrollPane();
				jScrollPane.setViewportView(jTable);
				jScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_ALWAYS );
				jScrollPane.setVerticalScrollBarPolicy(JScrollPane.VERTICAL_SCROLLBAR_NEVER );
				//adding the titles labels within the jscrollbar margin
				jScrollPane.setRowHeaderView(monEnTeteDeTable);
				jScrollPane.setBorder(BorderFactory.createEmptyBorder());
				//set the returned JPanel size according the table number of lines
				resultat.setPreferredSize(new Dimension(500,(18+16*data.size())));
				//add the jscroolpane
				resultat.add(jScrollPane);
			}
			return resultat;
		}
		return null;
	}




	/**
	 * GET this JTable (RETURN JTable)
	 */
	 public JTable getTable() {return jTable;}



	/**
	 * GET a table initialized with the given data (RETURN JTABLE)
	 */
	public JTable getTable(Vector donnees) {

		//IF there is data to display
		if (donnees != null && donnees.size()!=0){
			//column count (minimum 7 column displayed)
			int taille = ((Vector)donnees.get(0)).size();
			if (taille<nbColMin) taille = nbColMin;
			//initialization of the JTable title vector (here titles are empty)
			Vector titres = new Vector();
			//If they're not empty a column header is display
			for (int i=0; i<taille;i++) titres.add("");

			DefaultTableModel monModele = new DefaultTableModel (donnees,titres);

			jTable = new JTable();
			jTable.addMouseListener(new JTableListener(parent,this));
			jTable.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);
			jTable.setModel(monModele);
			for (int i = 0; i <taille; i++) {
				if (columnsWidth>0) jTable.getColumnModel().getColumn(i).setPreferredWidth(columnsWidth);
			}
			return jTable;
		}
		return null;
	}




	/**
	 * GET an int array filled with the values of the line number r (RETURN int[])
	 * If errors occurs return null
	 */
	public int [] getInt(int r){

		Vector v = getLigne(r);
		int [] resultat = new int[v.size()];
		try{
			for (int i=0; i<v.size(); i++) resultat[i]=Integer.parseInt(""+v.get(i));
		}
		catch (Exception e) {return null;}
		return resultat;
	}



	/**
	 * GET an float array filled with the values of the line number r (RETURN float[])
	 * If errors occurs return null
	 */
	public double [] getDouble(int r){

		Vector v = getLigne(r);
		double [] resultat = new double[v.size()];
		try{
			for (int i=0; i<v.size(); i++) resultat[i]=Double.parseDouble(""+v.get(i));
		}
		catch (Exception e) {return null;}
		return resultat;
	}







////CHECK functions
	/**
	 * TEST if the jTable is well formed
	 */
	public boolean isWellFilled(){
	//RETURN FALSE if a column is incomplet (non completly filled from top to bottom)

		boolean complete=true;
		boolean trouve=false;
		//IF the user is editing THEN message dialog
		if (jTable.isEditing()){
			//(please validate your last entry
			JOptionPane.showMessageDialog(parent , Translator.swap ("HorizontalTablePanel.validateField"));
			return false;
		}
		//ELSE
		else
		{	//for each column verify:
			for (int c=0; c<jTable.getColumnCount(); c++){
				complete=true;
				trouve = false;
				//for each row of the column
				for (int r=0; r<jTable.getRowCount(); r++){
					//IF cell is empty
					if (jTable.getValueAt(r,c)==null || jTable.getValueAt(r,c).equals("")) {
						//AND something has been found before THEN incomplet column
						if (trouve) complete = false;
					}
					//IF celle is full
					else{
						//something has been found
						trouve = true;
						//check if first line is composed of julian days
						if (r==0) {
							if ( !Check.isInt(""+jTable.getValueAt(r,c)) ) {
								JOptionPane.showMessageDialog(parent ,
															  Translator.swap ("HorizontalTablePanel.field")
															  +" "+titles.get(r)+" "+
															  Translator.swap ("HorizontalTablePanel.badValue")
															  +" "+jTable.getValueAt(r,c));
								return false;
							}
							else{
								if ( ! isJulianDay(Integer.parseInt(""+jTable.getValueAt(r,c))) ) {
									JOptionPane.showMessageDialog(parent , ""+jTable.getValueAt(r,c)+Translator.swap ("JulianDialog.invalid"));
									return false;
								}
							}
						}
					}
				}
				//IF a column is incomplet, the table is not well filled
				if (!complete) return false;
			}
			//Here all the columns are complet
			return true;
		}
	}

	private boolean isJulianDay(int d){return (d>=0 && d<=730);}







}










//////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////











/**
 *
 * This class is a mouse adapter.
 * It's applied to the upper class' jTable
 * It listen to mouse events and call JTable modifications functions
 * if needed
 *
 */

class JTableListener extends MouseAdapter {

	private Container parent = null;
	private JTable table = null;
	private HorizontalTablePanel panel = null;

	JTableListener(Container parent, HorizontalTablePanel jt) {
		this.table = jt.getTable();
		this.panel = jt;
		this.parent = parent;
	}


	public void mousePressed(MouseEvent e) {

		if ( SwingUtilities.isLeftMouseButton(e) ) {
			/** Bouton GAUCHE */
		} else if(SwingUtilities.isMiddleMouseButton(e) ) {
			/** Bouton du MILIEU */
		} else if(SwingUtilities.isRightMouseButton(e)) {
			table.repaint();
			//take coordinates
			int col = table.columnAtPoint(e.getPoint());
			int row = table.rowAtPoint(e.getPoint());
			//if user clicked on the last column then add a new column
			if (col==table.getColumnCount()-1) 	panel.addColumn();
			//here this.table references the old table (without the new columns)...
			this.table = panel.getTable();
			//...now it references the new one
			//if user clicked on the first line
			if ((row == 0)){
				//display the input UI
				JulianDialog j = new JulianDialog(parent,table,row,col);
				System.out.println("r:"+row+",col:"+col);
	  		}
		}
	}
}



