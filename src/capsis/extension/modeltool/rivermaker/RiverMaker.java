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

package capsis.extension.modeltool.rivermaker;

import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Random;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JTextField;

import jeeb.lib.util.Check;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.DefaultNumberFormat;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Log;
import jeeb.lib.util.Question;
import jeeb.lib.util.Settings;
import jeeb.lib.util.Translator;
import jeeb.lib.util.Vertex3d;
import jeeb.lib.util.Visitable;
import capsis.commongui.util.Helper;
import capsis.extension.DialogModelTool;
import capsis.extension.objectviewer.WatershedViewer;
import capsis.gui.DialogWithClose;
import capsis.gui.MainFrame;
import capsis.kernel.GModel;
import jeeb.lib.util.NTree;
import jeeb.lib.util.Node;
import jeeb.lib.util.PathManager;
import capsis.kernel.Step;
import jeeb.lib.util.Visiter;
import capsis.util.FishModel;
import capsis.util.WatershedNode;

/**
 * A tool for river generation, fish models.
 *
 * @author B. Parisi - Aapril 2006
 */
public class RiverMaker extends DialogModelTool implements ActionListener, Visiter {

	static {
		Translator.addBundle("capsis.extension.modeltool.rivermaker.RiverMaker");
	}

	// nb-07.08.2018
	//static public final String AUTHOR="B. Parisi";
	//static public final String VERSION="1.0";

	// Both fgi and fgo file names are conventionnal
	// They are writen / read in capsis.tmp directory
	//
	public static final String fgiFileName = PathManager.getDir("tmp")+File.separator+"FG_Capsis.fgi";
	public static final String fgoFileName = PathManager.getDir("tmp")+File.separator+"FG_Capsis.fgo";


	private Random random;



	// User controls
	private JTextField reachNumber;
	private JTextField p1;
	private JTextField p2;

	private int maxMagnitude;

	private int [][] depthTable;			// will record the distribution of reaches per depth		jl-04-2007
	private int maxDepth;

	private JButton generate;
	private JButton randomProba;
	private JButton close;	// after confirmation
	private JButton help;

	/**	Phantom constructor.
	*	Only to ask for extension properties (authorName, version...).
	*/
	public RiverMaker () {
		super();
	}

	@Override
	public void init(GModel m, Step s){

		try {
			random = new Random ();

			setTitle (getName ());

			createUI ();

			pack ();	// sets the size
			setModal (true);
			setVisible (true);

		} catch (Exception exc) {
			Log.println (Log.ERROR, "RiverMaker.c ()", exc.toString (), exc);
		}
	}

	/**	Extension dynamic compatibility mechanism.
	*	This matchwith method checks if the extension can deal (i.e. is compatible) with the referent.
	*/
	static public boolean matchWith (Object referent) {
		try {
			if (!(referent instanceof FishModel)) {return false;}

		} catch (Exception e) {
			Log.println (Log.ERROR, "RiverMaker.matchWith ()", "Error in matchWith () (returned false)", e);
			return false;
		}
		return true;
	}

	@Override
	public String getName() {
		return Translator.swap("RiverMaker.name");
	}

	@Override
	public String getAuthor() {
		return "B. Parisi";
	}

	@Override
	public String getDescription() {
		return Translator.swap("RiverMaker.description");
	}

	@Override
	public String getVersion() {
		return "1.0";
	}
	
	/**
	 * Action on fgiBrowse button.
	 */
/*	private void fgiBrowseAction () {
		JFileChooser chooser = null;
		try {
			chooser = new JFileChooser (defaultInventoryPath);
			ProjectFileAccessory acc = new ProjectFileAccessory ();
			chooser.setAccessory (acc);
			chooser.addPropertyChangeListener (acc);
		} catch (Exception exc) {
			Log.println (Log.ERROR, "RiverMaker.fgiBrowseAction ()",
					"Error while opening JFileChooser."
					+" "+exc.toString (), exc);
			return;
		}
		//chooser.setFileSelectionMode ();
		int returnVal = chooser.showOpenDialog(this);
		if (returnVal == JFileChooser.APPROVE_OPTION) {
			Settings.setProperty ("capsis.inventory.path", chooser.getSelectedFile ().toString ());
			fgiFileName.setText (chooser.getSelectedFile ().toString ());
		}
	}*/

	/**
	*	autogenerate some values for patch number probabilities
	*/
	private void probaProvide () {

		double p =1d;
		double q =1d;
		do {
		p = random.nextDouble ();
		q = random.nextDouble ();
		} while (p + q > 1);
//		p1.setText(""+p);
//		p2.setText(""+q);
		p1.setText(DefaultNumberFormat.getInstance().format(p)); // 3 decimals
		p2.setText(DefaultNumberFormat.getInstance().format(q)); // 3 decimals
	}

	/**
	 * Check for input validity
	 */
	private void generateAction () {

		// checks
		if (!Check.isInt (reachNumber.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RiverMaker.reachNumberShouldBeAnInteger"));
			return;
		}
		int numberOfReach_i = Check.intValue (reachNumber.getText ().trim ());

		if (!Check.isDouble (p1.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RiverMaker.p1ShouldBeAnDouble"));
			return;
		}
		double p1_d = Check.doubleValue (p1.getText ().trim ());
		if (p1_d  < 0 || p1_d > 1) {
			MessageDialog.print (this, Translator.swap ("RiverMaker.p1MustBeBetween0And1"));
			return;
		}
		if (!Check.isDouble (p2.getText ().trim ())) {
			MessageDialog.print (this, Translator.swap ("RiverMaker.p2ShouldBeAnDouble"));
			return;
		}
		double p2_d = Check.doubleValue (p2.getText ().trim ());
		if (p2_d  < 0 || p2_d > 1) {
			MessageDialog.print (this, Translator.swap ("RiverMaker.p2MustBeBetween0And1"));
			return;
		}
		if (p1_d + p2_d > 1) {
			MessageDialog.print (this, Translator.swap ("RiverMaker.p1Plusp2MustBeLowerThan1"));
			return;
		}

		// Memorizing the user choice for convenience
		Settings.setProperty("river.maker.reach.number", ""+numberOfReach_i);
		Settings.setProperty("river.maker.p1", ""+p1_d);
		Settings.setProperty("river.maker.p2", ""+p2_d);

		Collection nodes;
		boolean exitBoucle = false;
// Here: create a timer to avoid infinite generation of networks if	areRatiosBetweenExtremValues remains FALSE - jl 07.12
		int count = 0;
		do {
			boolean ratiosBetweenExtremValuesRespected = false;
			boolean reachNbrRespected = false;

			// river generation
			Collection addresses = buildAddresses (numberOfReach_i, p1_d, p2_d);

			Node root = buildTree (addresses);
			Tree tree = new Tree (root);

			//update order, origin, end
			// x in [0, 100], xRoot = 50
			// y in [0, magnitude]
			maxMagnitude = 0;

			WatershedNode n = (WatershedNode) root;
			n.setOrigin (new Vertex3d (50, 0, 0));
			nodes = traverseTree (tree);
			//processCalculOrders (nodes);
			WatershedViewer falseViewer = new WatershedViewer ();
			ratiosBetweenExtremValuesRespected = falseViewer.areRatiosBetweenExtremValues(nodes);

			if (((numberOfReach_i*2)+1) == nodes.size()) {reachNbrRespected = true;}
			if (ratiosBetweenExtremValuesRespected && reachNbrRespected) {exitBoucle = true;}
		} while (!exitBoucle && count++ <= 200); // fc-1.8.2012 added count, 200 can be changed

		// Do not write 'if (exitBoucle == true)', write 'if (exitBoucle)'

		try  {
			WatershedViewer viewer = new WatershedViewer ();
			viewer.init(nodes);

			viewer.setPreferredSize(new Dimension(500, 500));

			new DialogWithClose (this, viewer, Translator.swap ("RiverMaker.networkViewer"), true, false);
		} catch (Exception e) {
			Log.println (Log.ERROR, "RiverMaker.generateAction ()", "exception while opening viewer", e);
			MessageDialog.print (this, Translator.swap ("RiverMaker.exceptionWhileOpeningViewerSeeLog"));
			return;
		}

	}
	// traversing through the tree in preorder
	//
	public Collection  traverseTree (NTree tree) {
		Collection nodes = new ArrayList ();

		//restrictToVisible = false;

		maxDepth = 0;
		int maxId = 0;
		//sonsTable = new HashMap();
		Iterator i = tree.preorderIterator ();
			while (i.hasNext ()) {
				Node s = (Node) i.next ();
				nodes.add (s);
				//s.accept (this);		// Visitor pattern : s will call this.visit (s);
			}

		processCalculOrders (nodes);

		Iterator j = nodes.iterator ();
		while (j.hasNext ()) {
			Node s = (Node) j.next ();

			if (s instanceof Reach) {
				Reach r = (Reach) s ;
				maxDepth = (r.getDepth()> maxDepth ? r.getDepth() : maxDepth);
			}
			//s.accept (this);		// Visitor pattern : s will call this.visit (s);
		}




System.out.println ("maxDepth="+maxDepth);
		int [][] dt = new int [maxDepth+1][2];
		depthTable = dt;
		Iterator l = nodes.iterator ();
		while (l.hasNext ()) {
			Node s = (Node) l.next ();
			if (s instanceof Reach) {
				Reach r = (Reach) s ;
				depthTable [r.getDepth()][0]=depthTable [r.getDepth()][0]+1;
				depthTable [r.getDepth()][1]=depthTable [r.getDepth()][1]+r.getUpstream(maxDepth).size();
				int Id = r.getId();
				int order= r.getOrder();
//System.out.println ("order is= "+order);
				int sonsNumber = r.getUpstream(maxDepth).size();
//System.out.println ("sonsNumber= "+sonsNumber);
				//sonsTable.put(Id,sonsNumber);
			}

		}




		for (int u=0; u<depthTable.length; u++) {
			//System.out.println ("depth is "+u+" and nodes are "+ depthTable[u][0]);
			//System.out.println ("depth is "+u+" and sons are "+ depthTable[u][1]);
		}


		Iterator k = nodes.iterator ();
		while (k.hasNext ()) {
			Node s = (Node) k.next ();
			s.accept (this);
		}


		return nodes;
	}

	/**	Visit a tree node to calculate its origin and end.
	*/
	public void visit (Visitable visitable) {
		Node node = (Node) visitable;

		int magnitude = node.getDepth ()/2;
		if (maxMagnitude < magnitude) {maxMagnitude = magnitude;}

//System.out.println ("magnitude="+magnitude);
		int width = node.getWidth ();

		if (node.isRoot ()) {return;}	// origin was set previously
//~ System.out.println ("SPan.visit(): "+step.toString ());

		if (node instanceof Reach) {
			WatershedNode n = (WatershedNode) node;

			WatershedNode father = (WatershedNode) node.getFather ();
			n.setOrigin (father.getOrigin ());

			double xFather = father.getOrigin ().x;
			double x = 0;
			String address = n.getAddress ();
			Reach r = (Reach) node;
			double interval = 0d;
			double a = (double)r.getUpstream(maxDepth).size();
			if (depthTable[node.getDepth()][1]!=0) {
				interval = 100 * (a)/(depthTable[node.getDepth()][1]);
			}
System.out.println ("a sons is "+ a+ "total sons is "+depthTable[node.getDepth()][1]);
System.out.println ("interval is "+ interval);



			//~ double shift = (xFather * Math.pow (0.5, Math.log (1+magnitude)));
			//double shift = (xFather * Math.pow (0.5, magnitude));
			//minShift = Math.min (minShift, shift);
			double shift = interval/10;

			if (address.endsWith ("g")) {
				x = xFather - shift;
			} else if (address.endsWith ("m")) {
				x = xFather;
			} else {	// "d"
				x = xFather + shift;
			}
			n.setEnd (new Vertex3d (x, magnitude*5 + 1, 0));

		} else {	// Weir
			WatershedNode n = (WatershedNode) node;
			WatershedNode father = (WatershedNode) node.getFather ();
			n.setOrigin (father.getEnd ());
		}
	}


	/**	Visit a tree node to calculate its origin and end.

	public void newVisit (Visitable visitable) {
		Node node = (Node) visitable;





		int magnitude = node.getDepth ()/2;
		if (maxMagnitude < magnitude) {maxMagnitude = magnitude;}

System.out.println ("magnitude="+magnitude);
		int width = node.getWidth ();

		if (node.isRoot ()) {return;}	// origin was set previously
//~ System.out.println ("SPan.visit(): "+step.toString ());

		if (node instanceof Reach) {
			WatershedNode n = (WatershedNode) node;

			WatershedNode father = (WatershedNode) node.getFather ();
			n.setOrigin (father.getOrigin ());

			double xFather = father.getOrigin ().x;
			double x = 0;
			String address = n.getAddress ();

			//~ double shift = (xFather * Math.pow (0.5, Math.log (1+magnitude)));
			double shift = (xFather * Math.pow (0.6, magnitude));
			minShift = Math.min (minShift, shift);

			if (address.endsWith ("g")) {
				x = xFather - shift;
			} else if (address.endsWith ("m")) {
				x = xFather;
			} else {	// "d"
				x = xFather + shift;
			}
			n.setEnd (new Vertex3d (x, magnitude*5 + 1, 0));

		} else {	// Weir
			WatershedNode n = (WatershedNode) node;
			WatershedNode father = (WatershedNode) node.getFather ();
			n.setOrigin (father.getEnd ());
		}
	}


*/





	private Node buildTree (Collection addresses) {
		Map memo = new HashMap ();
		int reachId = 1;
		int weirId = 1;
		for (Iterator i = addresses.iterator (); i.hasNext ();) {
			String address = (String) i.next ();

			if (!address.endsWith ("w")) {
				int order = -1;
				short length = 1;
				float meanWidth = 1;
				Vertex3d origin = null;
				Vertex3d end = null;

				Reach reach = new Reach (reachId++, address, order, length, meanWidth,
						origin, end);
				memo.put (address, reach);

			} else {
				byte upPass = 0;
				byte downPass = 0;
				Vertex3d origin = null;

				Weir weir = new Weir (weirId++, address, upPass, downPass, origin);
				memo.put (address, weir);

			}
		}

		Node root = null;
		for (Iterator i = memo.values ().iterator (); i.hasNext ();) {
			Node node = (Node) i.next ();

			WatershedNode addressable = (WatershedNode) node;
			String address = addressable.getAddress ();

			if (address.length () == 1) {
				root = node;
			} else {
				String fatherAddress = address.substring (0, address.length ()-1);
//System.out.println ("address="+address+" fatherAddress="+fatherAddress);
				Node father = (Node) memo.get (fatherAddress);
				if (father == null) {
					System.out.println ("Trouble with address "+address+": father not found, line was ignored");
				} else {
					father.insertSon (node);
				}
			}
		}
		return root;

	}

	//	First attempt to autogenerate a virtual network.
	//	n : number of reach
	//
	public Collection buildAddresses (int n, double  p, double q) {

		Collection memo = new ArrayList ();

		memo.add ("w");
		memo.add ("wm");
		memo.add ("wmw");

		Collection tempo = new ArrayList();

		int magnitude = 2;
		boolean endOfWhile = false;
		do {

			tempo.clear();
			for (Iterator j = memo.iterator(); j.hasNext ();) {
				String s = (String) j.next ();
				if ((s.length()+1)/2 == magnitude) {
					tempo.add(s);
				}
			}
			magnitude ++;
			int nMemo = n;
			for (Iterator j = tempo.iterator (); j.hasNext ();) {
				String s = (String) j.next ();

				double rand = random.nextDouble();
				if (rand < p) {
					String newS = s +"m";
					memo.add (newS);
					newS = newS + "w";
					memo.add (newS);
					n --;
//System.out.println("n is " + n);
					if (n - 1 < 1) {
						endOfWhile = true;
						break;
					}

				} else if (rand < p+q) {
					String newSg = s +"g";
					memo.add (newSg);
					newSg = newSg + "w";
					memo.add (newSg);
					n --;
//System.out.println("n is " + n);

					if (n - 1 < 1) {
						endOfWhile = true;
						break;
					}

					String newSd = s + "d";
					memo.add (newSd);
					newSd = newSd + "w";
					memo.add (newSd);
					n --;
//System.out.println("n is " + n);
					if (n - 1 < 1) {
						endOfWhile = true;
						break;
					}

				} else {
//System.out.println("rand >= p+q");

				}

			}
			// if no reach was created for this magnitude, stop
			if (n == nMemo) {endOfWhile = true;}

		} while (!endOfWhile);
//System.out.println("end-of-while");

		//control on terminal
		int cptReach = 0;
		for (Iterator j = memo.iterator (); j.hasNext ();) {
			String s = (String) j.next ();
//System.out.println (s);
			if (!s.endsWith ("w")) {cptReach++;}
		}
//System.out.println ("cptReach is " + cptReach);
		return (memo);
	}


	/**
	 * From ActionListener interface.
	 */
	public void actionPerformed (ActionEvent evt) {
		if (evt.getSource ().equals (close)) {
			if (Question.ask (MainFrame.getInstance (),
					Translator.swap ("RiverMaker.confirm"), Translator.swap ("RiverMaker.confirmClose"))) {
				dispose ();
			}

		} else if (evt.getSource ().equals (generate)) {
			generateAction ();

		} else if (evt.getSource ().equals (randomProba)) {
			probaProvide ();

		} else if (evt.getSource ().equals (help)) {
			Helper.helpFor (this);
		}

	}

	/**	Called on Escape. Redefinition of method in AmapDialog : ask for user confirmation.
	*/
	protected void escapePressed () {
		if (Question.ask (MainFrame.getInstance (),
				Translator.swap ("RiverMaker.confirm"), Translator.swap ("RiverMaker.confirmClose"))) {
			dispose ();
		}
	}

	/**	User interface defintion
	*/
	private void createUI () {
		ColumnPanel part1 = new ColumnPanel ();

		LinePanel l0 = new LinePanel ();
		reachNumber = new JTextField ();
		reachNumber.setText (Settings.getProperty("river.maker.reach.number", "100"));
		l0.add (new JWidthLabel (Translator.swap ("RiverMaker.reachNumber")+" :", 120));
		l0.add (reachNumber);
		l0.addStrut0 ();

		LinePanel l1 = new LinePanel ();
		p1 = new JTextField ();
		p1.setText (Settings.getProperty("river.maker.p1", "0.4"));
		l1.add (new JWidthLabel (Translator.swap ("RiverMaker.p1")+" :", 120));
		l1.add (p1);
		l1.addStrut0 ();

		LinePanel l2 = new LinePanel ();
		p2 = new JTextField ();
		p2.setText (Settings.getProperty("river.maker.p2", "0.3"));
		l2.add (new JWidthLabel (Translator.swap ("RiverMaker.p2")+" :", 120));
		l2.add (p2);
		l2.addStrut0 ();

		part1.add (l0);
		part1.add (l1);
		part1.add (l2);

		part1.addGlue ();

		// 4. Control panel
		JPanel pControl = new JPanel (new FlowLayout (FlowLayout.RIGHT));
		randomProba = new JButton (Translator.swap ("Shared.randomProba"));
		randomProba.addActionListener (this);
		generate = new JButton (Translator.swap ("Shared.generate"));
		generate.addActionListener (this);
		close = new JButton (Translator.swap ("Shared.close"));
		close.addActionListener (this);
		help = new JButton (Translator.swap ("Shared.help"));
		help.addActionListener (this);
		pControl.add (randomProba);
		pControl.add (generate);
		pControl.add (close);
		pControl.add (help);
		// set close as default (see AmapDialog)
		//close.setDefaultCapable (true);
		//getRootPane ().setDefaultButton (close);

		setDefaultButton (generate);

		// layout parts
		getContentPane ().add (part1, BorderLayout.CENTER);
		getContentPane ().add (pControl, BorderLayout.SOUTH);
	}

	/**	Determine the order of each newly created reach.
	*	b.parisi -21.04.2006
	*/
	private void processCalculOrders (Collection nodes) {


		int compteur = 0;
		for (Iterator j = nodes.iterator(); j.hasNext ();) {
			Node n = (Node) j.next ();
			if (n instanceof Reach) {
				compteur ++;
			}
		}

		while (compteur >= 1) {
			for (Iterator i = nodes.iterator(); i.hasNext ();) {

				Node t = (Node) i.next ();

				if (t instanceof Reach) {
					Reach r = (Reach) t;

					if (r.getOrder () == -1) {

						if (r.getUpstream (1).isEmpty()) {
							r.setOrder (1);
							compteur --;
						} else if (r.getUpstream (1).size() == 1) {
							Iterator it = r.getUpstream(1).iterator();
							int upOrder = ((Reach) it.next()).getOrder();
							if (upOrder != -1) {
								r.setOrder (upOrder);
								compteur --;
							}
						}
						else if (r.getUpstream (1).size () == 2) {
							Iterator it = r.getUpstream(1).iterator();
							int orderR1 = (((Reach) it.next()).getOrder ());
							int orderR2 = (((Reach) it.next()).getOrder ());


							if ((orderR1 != -1) && (orderR2 != -1)) {
								if (orderR1 == orderR2) {
									r.setOrder (orderR1 + 1);
									compteur --;
								}
								else if (orderR1 != orderR2) {
									r.setOrder (Math.max(orderR1,orderR2));
									compteur --;
								}

							}
						}
					}
				}
			}
		}
	}

}


