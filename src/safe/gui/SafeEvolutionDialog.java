/** 
 * Hi-SAFE : A 3D Agroforestry Model for Integrating Dynamic Tree–Crop Interactions
 * 
 * Copyright (C) 2000-2024 INRAE 
 * 
 * Authors  
 * C.DUPRAZ       	- INRAE Montpellier France
 * M.GOSME       	- INRAE Montpellier France
 * G.TALBOT       	- INRAE Montpellier France
 * B.COURBAUD      	- INRAE Montpellier France
 * H.SINOQUET		- INRAE Montpellier France
 * N.DONES			- INRAE Montpellier France
 * N.BARBAULT 		- INRAE Montpellier France 
 * I.LECOMTE       	- INRAE Montpellier France
 * M.Van NOORDWIJK  - ICRAF Bogor Indonisia 
 * R.MULIA       	- ICRAF Bogor Indonisia
 * D.HARJA			- ICRAF Bogor Indonisia
 * 
 * This file is part of Hi-SAFE  
 * Hi-SAFE is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * Hi-SAFE is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
 * See the GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU lesser General Public License
 * If not, see <http://www.gnu.org/licenses/>.
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
