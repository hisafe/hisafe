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

import java.awt.BorderLayout;
import java.awt.Color;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.Set;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.border.TitledBorder;

import capsis.util.configurable.Configurable;
import capsis.util.configurable.ConfigurationPanel;
import capsis.util.configurable.SharedConfigurable;
import jeeb.lib.util.gui.NorthPanel;


/**
 * MuteConfigurationPanel is a ConfigurationPanel only used in display mode.
 * there is no Configurable associated.
 * It's a way of showing some things where a ConfigurationPanel is asked for.
 * 
 * @author F. de Coligny - march 2001
 */
public class MuteConfigurationPanel extends ConfigurationPanel {
	
	private Configurable customConfigurable;
	private Collection embeddedConfigPanels;
	private JPanel content;	// fc - 6.1.2004

	public MuteConfigurationPanel () {}		// masks superclass dafault constructor

	public MuteConfigurationPanel (Configurable obj) {}			// usused

	public MuteConfigurationPanel (SharedConfigurable obj) {}	// unused

	/**
	 * A configuration panel to embed a component which is not 
	 * an instance of  Configurable.
	 * <pre>
	 * panel = new MuteConfigurationPanel ("generalLabel", aComponent);
	 * // panel.execute (); // will have no action
	 * </pre>
	 */
	public MuteConfigurationPanel (String label, JComponent component) {		// this one was added ;-)
		customConfigurable = new MuteConfigurable (label);
		embeddedConfigPanels = null;
		
		setLayout (new BorderLayout ());
		//~ add (component, BorderLayout.NORTH);
		add (component, BorderLayout.CENTER);
	}

	/**
	 * A configuration panel to embed several configuration panels (in one single column).
	 * When execute () is run, it will run the execute () method for each embedded panel.
	 * <pre>
	 * panel = new MuteConfigurationPanel ("generalLabel");
	 * panel.add (title1, configurable1);
	 * panel.add (title2, configurable2);
	 * panel.addGlue ();
	 * ...
	 * panel.execute ();
	 * </pre>
	 */
	public MuteConfigurationPanel (String label) {
		customConfigurable = new MuteConfigurable (label);
		
		//~ setLayout (new BoxLayout (this, BoxLayout.Y_AXIS));	// fc - 6.1.2004
		setLayout (new BorderLayout ());	// fc - 6.1.2004
		content = new JPanel ();	// fc - 6.1.2004
		content.setLayout (new BoxLayout (content, BoxLayout.Y_AXIS));	// fc - 6.1.2004
		//~ super.add (content, BorderLayout.NORTH);	// fc - 6.1.2004
		super.add (new JScrollPane (new NorthPanel (content)), BorderLayout.CENTER);	// fc - 28.9.2006 ==================
	}

	/**	fc - 22.4.2004 - If several config panels inside this one, execute them in sequence.
	*/
	public void add (String title, Color color, ConfigurationPanel panel) {
//		JPanel aux = new JPanel (new BorderLayout ());
		TitledBorder b = BorderFactory.createTitledBorder (title);
		b.setTitleColor (color);
		panel.setBorder (b);
		
		content.add (panel);	// fc - 6.1.2004
		content.add (Box.createVerticalStrut (2));	// fc - 6.1.2004
		
		if (embeddedConfigPanels == null) {embeddedConfigPanels = new ArrayList ();}
		embeddedConfigPanels.add (panel);	
	}

	/**
	 * Layout correctly by pushing everything up.
	 */
	public void addGlue () {
		content.add (Box.createGlue ());
	}

	/**
	 * fc - 6.5.2003 - if several config panels inside this one, execute them in sequence.
	 */
	public void execute () {
		if (embeddedConfigPanels != null) {
			for (Iterator i = embeddedConfigPanels.iterator (); i.hasNext ();) {
				ConfigurationPanel p = (ConfigurationPanel) i.next ();
				p.execute ();
			}
		}
	}




	/**
	 * Return true is ConfigurationPanel is empty.
	 */
	public boolean isEmpty () {return false;}

	/**
	 * This method is called by containing dialog before callig execute ().
	 * If trouble, the method is responsible for user information ("Integer needed...").
	 */
	public boolean checksAreOk () {
		// fc - 23.3.2004 - each panel must check its data and send a message in case of trouble
		//
		if (embeddedConfigPanels != null) {
			for (Iterator i = embeddedConfigPanels.iterator (); i.hasNext ();) {
				ConfigurationPanel p = (ConfigurationPanel) i.next ();
				if (!p.checksAreOk ()) {return false;}
			}
		}
		return true;
	}

	/**
	 * To add things in a ConfigurationPanel, use getContentPane ().add (...).
	 */
	public JPanel getContentPane () {return this;}

	/**
	 * Return true if the panel concerns several configurable (multi-configuration).
	 */
	public boolean isSharedConfPanel () {return false;}		// unused

	/**
	 * Return the configurable if single mode.
	 */
	public Configurable getConfigurable () {return customConfigurable;}

	/**
	 * Return the multi configurable if multi mode.
	 */
	public SharedConfigurable getSharedConfigurable () {return null;}		// unused

	/**
	 * Add a MultiConfigurable to the list. They will all be reconfigured
	 * with the panel.
	 */
	public void addSharedConfigurable (SharedConfigurable c) {}	// unused

	/**
	 * Return the configurables Set. Contains one Configurable in
	 * single mode or several MultiConfigurable in multi mode.
	 */
	public Set getConfigurables () {return null;}	// unused
	
	public String toString () {
		return "MuteConfigurationPanel";
	}
	
	
	/**
	 * A Configurable with only a label. Used to add Mute Configuration panels
	 * in tab panes (see capsis.gui.command.ConfigureScenario).
	 *
	 * @author F. de Coligny - march 2001
	 */
	static class MuteConfigurable implements Configurable {
		private String label;
		public MuteConfigurable (String label) {this.label = label;}
		
		public String getConfigLabel () {return label;}
		public ConfigurationPanel getConfigPanel (Object param) {return null;}
		public void applyConfig (ConfigurationPanel panel) {}
		public void postConfig () {}
	}

	
}






















