/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Grégoire Talbot 1, Nicolas Barbault 1, 
 * Grégoire Vincent 2 , Rachmat Mulia 3, François Bussière 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Hervé Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Université Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d’étude de la forêt, Université du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d’Hères, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share — copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt — remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution — 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions — You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
 *               
 * Notices:
 * 		You do not have to comply with the license for elements of the material in the public domain or where your use is permitted 
 *      by an applicable exception or limitation .
 *		No warranties are given. The license may not give you all of the permissions necessary for your intended use. 
 *		For example, other rights such as publicity, privacy, or moral rights may limit how you use the material.  
 *
 * For more details see <https://creativecommons.org/licenses/by/4.0/>.
 *
 */

package safe.extension.standviewer.safe;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;

import javax.swing.JPanel;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.Translator;
import safe.model.SafePlotSettings;
import capsis.util.InfoDialog;

/**
 * LegendPanel - A component to see legend of SVSafe viewer
 *
 * @author N.Marchais - September 2003
 */
public class LegendPanel extends ColumnPanel {
	private static final long serialVersionUID = 1L;

	protected InfoDialog infoDialog;
	
	private SVSafeSettings svSafeSettings;


	/**	Constructor.
	*/
	public LegendPanel (
			SafePlotSettings settings,
			SVSafeSettings svSafeSettings,
			SVSafe svSafe) {	// needed (fc)

		super ();	// fc - 8.3.2004

		this.svSafeSettings = svSafeSettings;

		
		// Compass to display north and slope orientation
		Compass compass = new Compass (settings.northOrientation);
		add (compass);


		//Color legend
		ColorLegend colorLegend = new ColorLegend ();
		add (colorLegend);
		addStrut0 ();
		addGlue ();

		setOpaque (true);
		setBackground (Color.WHITE);
	}


	//	Color legend panel class
	//
	private class ColorLegend extends JPanel{

		private String str1, str2, str3;	// labels
		private Color [] colors;			// colors

		public ColorLegend () {
			setLayout (new BorderLayout ());
			setPreferredSize (new Dimension (100,180));
			setMaximumSize (new Dimension (100,180));
			setBackground (Color.WHITE);
			//setVisible (true);
		}

		public void paintComponent (Graphics g) {
			super.paintComponent (g);
			Graphics2D g2d = (Graphics2D) g;

			// DISPLAY OPTION = CROP
			if (svSafeSettings.cellOption == 1) {
				if (svSafeSettings.cropChoice == 1) { //LAI
					double middle = svSafeSettings.laiValueMin + ((svSafeSettings.laiValueMax - svSafeSettings.laiValueMin) / 2);
					str1 = svSafeSettings.laiValueMax + " " + Translator.swap ("SVSafe.OptionPanel.cropScale1");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.cropScale1");
					str3 = svSafeSettings.laiValueMin + " " + Translator.swap ("SVSafe.OptionPanel.cropScale1");
					colors = svSafeSettings.cropColors;
				} else if (svSafeSettings.cropChoice == 2) {	//BIOMASS
					double middle = svSafeSettings.yieldValueMin + ((svSafeSettings.yieldValueMax - svSafeSettings.yieldValueMin) / 2);
					str1 = svSafeSettings.yieldValueMax +" " + Translator.swap ("SVSafe.OptionPanel.cropScale2");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.cropScale2");
					str3 = svSafeSettings.yieldValueMin +" " + Translator.swap ("SVSafe.OptionPanel.cropScale2");
					colors = svSafeSettings.cropColors;
				} else if (svSafeSettings.cropChoice == 3) {	//TEMPERATURE
					double middle = svSafeSettings.temperatureValueMin + ((svSafeSettings.temperatureValueMax - svSafeSettings.temperatureValueMin) / 2);
					str1 = svSafeSettings.temperatureValueMax +" " + Translator.swap ("SVSafe.OptionPanel.cropScale3");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.cropScale3");
					str3 = svSafeSettings.temperatureValueMin +" " + Translator.swap ("SVSafe.OptionPanel.cropScale3");
					colors = svSafeSettings.cropColors;
				} else if (svSafeSettings.cropChoice == 4 ) {	//ROOT DEPTH
					double middle = svSafeSettings.rootDepthValueMin + ((svSafeSettings.rootDepthValueMax - svSafeSettings.rootDepthValueMin) / 2);
					str1 = svSafeSettings.rootDepthValueMax +" " + Translator.swap ("SVSafe.OptionPanel.cropScale4");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.cropScale4");
					str3 = svSafeSettings.rootDepthValueMin +" " + Translator.swap ("SVSafe.OptionPanel.cropScale4");
					colors = svSafeSettings.cropColors;
				}

			// DISPLAY OPTION = LIGHT
			} else if (svSafeSettings.cellOption == 2) {
				double middle = svSafeSettings.lightValueMin + ((svSafeSettings.lightValueMax - svSafeSettings.lightValueMin) / 2);
				str1 = svSafeSettings.lightValueMax +" " + Translator.swap ("SVSafe.OptionPanel.lightScale");
				str2 = middle +" " + Translator.swap ("SVSafe.OptionPanel.lightScale");
				str3 = svSafeSettings.lightValueMin +" " + Translator.swap ("SVSafe.OptionPanel.lightScale");
				colors = svSafeSettings.lightColor;

			// DISPLAY OPTION = GROUND
			} else if (svSafeSettings.cellOption == 3) {
				if (svSafeSettings.groundChoice == 1) {	// WATER
					double middle = svSafeSettings.waterValueMin + ((svSafeSettings.waterValueMax - svSafeSettings.waterValueMin) / 2);
					str1 = svSafeSettings.waterValueMax +" " + Translator.swap ("SVSafe.OptionPanel.groundScale1");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.groundScale1");
					str3 = svSafeSettings.waterValueMin +" " + Translator.swap ("SVSafe.OptionPanel.groundScale1");
					colors = svSafeSettings.waterColor;
				} else if (svSafeSettings.groundChoice ==2 ) {	// NITROGEN
					double middle = svSafeSettings.nitrogenValueMin + ((svSafeSettings.nitrogenValueMax - svSafeSettings.nitrogenValueMin) / 2);
					str1 = svSafeSettings.nitrogenValueMax +" " + Translator.swap ("SVSafe.OptionPanel.groundScale2");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.groundScale2");
					str3 = svSafeSettings.nitrogenValueMin +" " + Translator.swap ("SVSafe.OptionPanel.groundScale2");
					colors = svSafeSettings.nitrogenColor;
				} else if (svSafeSettings.groundChoice == 3) {	// TREE ROOTS
					double middle = svSafeSettings.treeRootValueMin + ((svSafeSettings.treeRootValueMax - svSafeSettings.treeRootValueMin) / 2);
					str1 = svSafeSettings.treeRootValueMax +" " + Translator.swap ("SVSafe.OptionPanel.groundScale3");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.groundScale3");
					str3 = svSafeSettings.treeRootValueMin +" " + Translator.swap ("SVSafe.OptionPanel.groundScale3");
					colors = svSafeSettings.rootColor;
				} else if (svSafeSettings.groundChoice == 4) {	// CROP ROOTS
					double middle = svSafeSettings.cropRootValueMin + ((svSafeSettings.cropRootValueMax - svSafeSettings.cropRootValueMin) / 2);
					str1 = svSafeSettings.cropRootValueMax +" " + Translator.swap ("SVSafe.OptionPanel.groundScale4");
					str2 = rounding (middle,1) +" " + Translator.swap ("SVSafe.OptionPanel.groundScale4");
					str3 = svSafeSettings.cropRootValueMin +" " + Translator.swap ("SVSafe.OptionPanel.groundScale4");
					colors = svSafeSettings.rootColor;
				}
        	}

			//Draw the legend woth color colums and labels
        	for (int i=0; i < colors.length; i++){
        		g2d.setColor (colors[i]);
        		g2d.fill3DRect (5, i*12, 20, (i*12)+12, true);
        	}
        	g2d.setColor (Color.BLACK);
        	g2d.drawString (str1,30,12);
        	g2d.drawString (str2,30,93);
        	g2d.drawString (str3,30,180);
		}
	}


	// Utility method
	//
	private double rounding (double x, int n) {
		double r =  (Math.round (x*Math.pow (10,n))) / (Math.pow (10,n));
		return r;
	}


}
