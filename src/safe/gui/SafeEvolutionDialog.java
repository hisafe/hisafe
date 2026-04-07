/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree�Crop Interactions
 * 
 * Copyright (C) 2000-2025 INRAE - CC-BY License
 * 
 * LIST OF AUTHORS
 * --------------- 
 * Christian Dupraz 1, Kevin J.Wolz 1 , Isabelle Lecomte 1, Gr�goire Talbot 1, Nicolas Barbault 1, 
 * Gr�goire Vincent 2 , Rachmat Mulia 3, Fran�ois Bussi�re 4, Harry Ozier-Lafontaine 4,
 * Sitraka Andrianarisoa 1, Nick Jackson 5, Gerry Lawson 5, Nicolas Dones 6, Herv� Sinoquet 6,
 * Betha Lusiana 3, Degi Harja 3, Suzy Domenicano 7 , Francesco Reyes 1 , Marie Gosme 1 ,
 * Meine Van Noordwijk 3, Benoit Courbaud 8
 *
 * 1 INRA (UMR-ABSYS), University of Montpellier, 34090 Montpellier, France
 * 2 IRD (UMR-AMAP), University of Montpellier, 34090 Montpellier, France
 * 3 ICRAF, Bogor 16001, Indonesia
 * 4 INRA (UR ASTRO 1231) Centre Antilles-Guyane, Petit-Bourg, 97170 Guadeloupe, France
 * 5 CEH, NERC,Wallingford OX10 8BB, UK
 * 6 INRA (UMR-PIAF), Universit� Clermont Auvergne, 63000 Clermont-Ferrand, France
 * 7 Centre d��tude de la for�t, Universit� du Quebec, Montreal H2X 3Y5, Canada
 * 8 CEMAGREF, Mountain Ecosystems and Landcapes Research Unit, Saint-Martin-d�H�res, France
 *
 *----------------------------------------------------------------------------------------------
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software under the terms of the CC-BY License as published by the Creative Commons Corporation
 *
 * You are free to:
 *		Share � copy and redistribute the material in any medium or format for any purpose, even commercially.
 *		Adapt � remix, transform, and build upon the material for any purpose, even commercially.
 *		The licensor cannot revoke these freedoms as long as you follow the license terms.
 * 
 * Under the following terms:
 * 		Attribution � 	You must give appropriate credit , provide a link to the license, and indicate if changes were made . 
 *               		You may do so in any reasonable manner, but not in any way that suggests the licensor endorses you or your use.
 *               
 * 		No additional restrictions � You may not apply legal terms or technological measures that legally restrict others from doing anything the license permits.
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

package safe.gui;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;

import capsis.commongui.EvolutionDialog;
import capsis.kernel.Step;

/**
 * SafeEvolutionDialog - Dialog box to run the model
 * WARNING : GUI interface IS NOT USED IN HI-sAFe
 */
public class SafeEvolutionDialog extends EvolutionDialog implements ActionListener, MouseListener {

	private static final long serialVersionUID = 1L;

	/**
	 * The aim is to initialize the stand given to the constructor.
	 */
	public SafeEvolutionDialog (Step s) throws Exception {
		super ();
	}


	public void actionPerformed (ActionEvent evt) {}

	/**
	 * Mouse click event
	 */
	public void mousePressed (MouseEvent e) {}

	public void mouseReleased (MouseEvent e) {}

	public void mouseExited (MouseEvent e) {}

	public void mouseEntered (MouseEvent e) {}

	public void mouseClicked (MouseEvent e) {}

}
