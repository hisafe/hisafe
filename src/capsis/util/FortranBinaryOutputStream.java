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

//package org.fudaa.dodico.fortran;
package capsis.util;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

/**
* Une classe facilitant l'écriture de fichiers binaires lus par Fortran.
* ATTENTION les methodes heritees et non redéfinies dans cette 
* donneront des résultats faux.
* L'équivalence d'intructions entre Java et Fortran se fera de la manière
* suivante :<BR><PRE>
* (en considérant i=integer/int, f=real/float, d=double precision/double et
* s=character*()/String)
* 1) Pour un fichier à acces séquentiel :
*   Fortran
*     open  (unit=10,file='fichier.bin',access='sequentiel',form='unformatted')
*     write (unit=10) <i>,<f>,<d>,<s>
*     ...
*     close (unit=10)
*   Java
*     FortranBinaryOutputStream out=
*      new FortranBinaryOutputStream(new FileOutputStream("fichier.bin"),true);
*     out.writeInteger(i);
*     out.writeReal(f);
*     out.writeDoublePrecision(d);
*     out.writeCharacter(s);
*     out.writeRecord();
*     ...
*     out.close();
* 2) Pour un fichier à acces direct :
*   Fortran
*     open(unit=10,file='fichier.bin',access='direct',recl=30,form='unformatted')
*     write (unit=10,rec=1) <i>,<f>,<d>,<s>
*     ...
*     close (unit=10)
*   Java
*     FortranBinaryOutputStream out=
*      new FortranBinaryOutputStream(new FileOutputStream("fichier.bin"),false);
*     out.setRecordLength(30);
*     out.writeInteger(i);
*     out.writeReal(f);
*     out.writeDoublePrecision(d);
*     out.writeCharacter(s);
*     out.writeRecord();
*     ...
*     out.close();
* </PRE>
*
* @version      $Id: FortranBinaryOutputStream.java,v 1.8 2002/11/25 09:59:25 deniger Exp $
* @author       Bertrand Marchand 
*/
public class FortranBinaryOutputStream extends NativeBinaryOutputStream {
	private NativeBinaryOutputStream bufStream_;
	private ByteArrayOutputStream    arrayStream_;
	private boolean                  sequential_;
	private int                      recordLength_;


	/**
	 * Création en précisant si le fichier binaire est à access séquentiel ou non
	 * @param _out OutputStream
	 * @param _sequential <B>true</B> le fichier est binaire à accès
	 * <I>Sequential</I>. <B>false</B> le fichier est binaire à accès <I>Direct</I>
	 */
	public FortranBinaryOutputStream(OutputStream _out, boolean _sequential)
			throws IOException{
		this(_out,_sequential,System.getProperty("os.arch"));
	}

	/**
	* Création en précisant si le fichier binaire est à access séquentiel ou non
	* @param _out OutputStream
	* @param _sequential <B>true</B> le fichier est binaire à accès
	* <I>Sequential</I>. <B>false</B> le fichier est binaire à accès <I>Direct</I>
	*/
	public FortranBinaryOutputStream(OutputStream _out, boolean _sequential,String _architectureID)
			throws IOException{
		super(_out,_architectureID);
		arrayStream_ =new ByteArrayOutputStream();
		bufStream_   =new NativeBinaryOutputStream(arrayStream_,_architectureID);
		sequential_  =_sequential;
		recordLength_=0;
	}

	/**
	* Affectation de la longueur des enregistrements (pour les fichiers à accès
	* <I>Direct</I>)
	* @param _length Longueur d'enregistrement en longworld
	*                (1 longworld=4 octets)
	*/
	public void setRecordLength(int _length) { recordLength_=_length; }

	/**
	* Retourne la longueur des enregistrements (pour les fichiers à accès
	* <I>Direct</I>
	* @return Longueur d'enregistrement en longworld (1 longworld=4 octets)
	*/
	public int getRecordLength() { return recordLength_; }

	/**
	* Ecriture d'un champ chaine de caractères "<I>character</I>" Fortran
	* @param _s string correspondant à la chaine de caractères
	*/
	public void writeCharacter(String _s) throws IOException {
		bufStream_.writeBytes(_s);
	}

	//  public void writeCharacter(char[] _c) {
	//  }
	//  public void writeCharacter(char _c) {
	//  }

	/**
	* Ecriture d'un champ entier "<I>integer</I>" Fortran
	* @param _i int correspondant au integer
	*/
	public void writeInteger(int _i) throws IOException {
		bufStream_.writeInt_32(_i);
	}

	/**
	* Ecriture d'un champ réel "<I>real</I>" Fortran
	* @param _f float correspondant au real
	*/
	public void writeReal(float _f) throws IOException {
		bufStream_.writeFloat_32(_f);
	}

	/**
	* Ecriture d'un champ réel en double précision "<I>double precision</I>"
	* Fortran
	* @param _d double correspondant au double precision
	*/
	public void writeDoublePrecision(double _d) throws IOException {
		bufStream_.writeFloat_64(_d);
	}

	/**
	* Ecriture des champs de l'enregistrement. L'enregistrement doit etre écrit
	* pour que les champs soient également écrits sur le fichiers.
	* Un enregistrement correspond à une instruction WRITE du Fortran
	*/
	public void writeRecord() throws IOException {
		if (sequential_) {
			int t=arrayStream_.size();
			writeInt_32(t);
			arrayStream_.writeTo(this);
			writeInt_32(t);
		}
		else {
			byte[] buf=arrayStream_.toByteArray();
			write(buf,0,Math.min(buf.length,4*recordLength_));
			for (int i=buf.length; i<4*recordLength_; i++) write(0);
		}

		arrayStream_.reset();
	}

	/**
	* Fermeture du fichier
	*/
	public void close() throws IOException {
		super       .close();
		arrayStream_.close();
		bufStream_  .close();
	}
}
