package safe.gui;

import java.awt.Dialog;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.Check;
import jeeb.lib.util.IconLoader;

/**
 *
 * This Class is used to display the name, value and help message of a data.
 * It contains a JLabel to display the name of the data which is described
 * JTextField for the value and a JButton which call a JOptionPane to display help
 *
 */
public class VariablePanel extends JPanel implements ActionListener{

	private int marginWidth = 110;

	private Dialog parent = null;
	private JPanel 		left,right 	= null;
	private JLabel 		name 	= null;
	private JTextField 	value 	= null;
	private JLabel 		unit 	= null;
	private JButton		help	= null;
	private String		helpContent = "";



////CONSTRUCTOR
	public VariablePanel (Dialog p, String name, String value, String unit, String help) {

		super();
		//to display help message in parent dialog
		parent = p;
		//data description
		this.helpContent = help;
		this.name = new JLabel (name);
		this.name.setPreferredSize(new Dimension(marginWidth-5,20));
		this.value = new JTextField(5);
		this.value.setText (value);
		this.unit = new JLabel (unit);
		ImageIcon icon = IconLoader.getIcon ("help_16.png");
		this.help = new JButton (icon);
		this.help.setPreferredSize (new Dimension(16,16));
		this.help.addActionListener (this);
		//Interface
		left = new JPanel (new FlowLayout(FlowLayout.LEFT));
		left.setPreferredSize (new Dimension(360,24));
		left.add (this.name);
		left.add (this.value);
		left.add (this.unit);
		right = new JPanel(new GridLayout(1,0,0,0));
		right.add (this.help);

		FlowLayout l = new FlowLayout(FlowLayout.RIGHT);
		l.setVgap (0);
		this.setLayout (l);
		this.add (left);
		this.add (right);
	}


////EVENT MANAGMENT
	public void actionPerformed(ActionEvent e){

		if (e.getSource().equals(help)){
			JOptionPane.showMessageDialog (parent, helpContent);
		}
	}


////CHECK functions
	public boolean isInt () {return Check.isInt(value.getText());}
	public boolean isFloat () {return Check.isDouble(value.getText());}



////ACCESSORS
	public void setValue (String s) {this.value.setText(s);}
	public int getInt () {return Integer.parseInt(value.getText());}
	public float getFloat () {return Float.parseFloat(value.getText());}

}
