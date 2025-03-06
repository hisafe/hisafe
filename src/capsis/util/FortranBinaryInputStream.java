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

//~ package org.fudaa.dodico.fortran;
package capsis.util;

import java.io.IOException;
import java.io.InputStream;

/**
 * Une classe facilitant la lecture de fichiers binaires �crits par Fortran
 * L'�quivalence d'intructions entre Java et Fortran se fera de la mani�re
 * suivante :<BR><PRE>
 * (en consid�rant i=integer/int, f=real/float, d=double precision/double et
 * s=character*()/String)
 * 1) Pour un fichier � acces s�quentiel :
 * Fortran
 * open  (unit=10,file='fichier.bin',access='sequentiel',form='unformatted')
 * read  (unit=10) <i>,<f>,<d>,<s> 
 * ...
 * close (unit=10)
 * Java
 * FortranBinaryInputStream in=
 * new FortranBinaryInputStream(new FileInputStream("fichier.bin"),true);
 * in.readRecord();
 * i=in.readInteger();
 * f=in.readReal();
 * d=in.readDoublePrecision();
 * s=in.readCharacter(s.length());
 * ...
 * in.close();
 * 2) Pour un fichier � acces direct :
 * Fortran
 * open  (unit=10,file='fichier.bin',access='direct',recl=30,form='unformatted')
 * read  (unit=10,rec=1) <i>,<f>,<d>,<s>
 * ...
 * close (unit=10)
 * Java
 * FortranBinaryInputStream in=
 * new FortranBinaryInputStream(new FileInputStream("fichier.bin"),false);
 * in.setRecordLength(30);
 * in.readRecord();
 * i=in.readInteger();
 * f=in.readReal();
 * d=in.readDoublePrecision();
 * s=in.readCharacter(s.length());
 * ...
 * in.close();
 * </PRE>
 *
 * @version      $Revision: 1.9 $ $Date: 2002/11/25 09:59:25 $ by $Author: deniger $
 * @author       Bertrand Marchand 
 */
public class FortranBinaryInputStream extends NativeBinaryInputStream {
//  private NativeBinaryInputStream bufStream_;
//  private ByteArrayInputStream    arrayStream_;
  private boolean sequential_;
  private int     recordLength_;
  private long    currentPos_;
  private long    nextPos_;

  /**
   * Cr�ation en pr�cisant si le fichier binaire est � access s�quentiel ou non.
   * Si sequentiel, alors certaines donnees ( generees par la meme instruction write 
   * de fortran) sont entourees par 4 octets qui correspondent a la longueur
   * de l'enregistrement. La variable systeme ("os.arch") permet de d�terminer
   * l'architecture a utiliser.
   * @param _in InputStream
   * @param _sequential <B>true</B> le fichier est binaire � acc�s
   * <I>Sequential</I>. <B>false</B> le fichier est binaire � acc�s <I>Direct</I>
   */
  public FortranBinaryInputStream(InputStream _in, boolean _sequential)
   throws IOException{
     this(_in,_sequential,System.getProperty("os.arch"));

  }
  
  /**
   * Il est possible de definir l'architecture intel ( little indian) ou sparc
   * (big indian).
   * @param _in InputStream.
   * @param _sequential <B>true</B> le fichier est binaire � acc�s
   *        <I>Sequential</I>. <B>false</B> le fichier est binaire � acc�s 
   *        <I>Direct</I>
   * @param _architectureID "sparc" pour le big endian et "X86" pour le little
   *        endian. Utiliser les identifiant de NativeBinaryInputStream
   * @see "org.fudaa.dodico.fortran.NativeBinaryInputStream"
   */
  public FortranBinaryInputStream(InputStream _in, boolean _sequential, String _architectureID)
   throws IOException{
    super(_in,_architectureID);
    //arrayStream_ =new ByteArrayInputStream();
    //bufStream_   =new NativeBinaryInputStream(arrayStream_,
   //                                            System.getProperty("os.arch"));
    sequential_  =_sequential;
    recordLength_=0;
    nextPos_     =0;
    currentPos_  =nextPos_;
  }
  
  /**
   * ATTENTION Different du cas acces direct. 
   * Renvoie la taille de l'enregistrement lu par le flux. 
   */
  public int getSequentialRecordLength()
  {
    return recordLength_;
  }
  
  /**
   * Affectation de la longueur des enregistrements (pour les fichiers � acc�s
   * <I>Direct</I>
   * @param _length Longueur d'enregistrement en longworld
   *                (1 longworld=4 octets)
   */
  public void setRecordLength(int _length) { recordLength_=_length*4; }

  /**
   * Retourne la longueur des enregistrements (pour les fichiers � acc�s
   * <I>Direct</I>
   * @return Longueur d'enregistrement en longworld (1 longworld=4 octets)
   */
  public int getRecordLength() { return recordLength_/4; }

  /**
   * Lecture d'un champ chaine de caract�res "<I>character</I>" Fortran
   * @param _lgString Longueur de la chaine � lire
   * @return String correspondant � la chaine de caract�res
   */
  public String readCharacter(int _lgString) throws IOException {
    byte[] buf=new byte[_lgString];
    currentPos_+=_lgString;
    read(buf);
    return new String(buf);
  }

//  public void writeCharacter(char[] _c) {
//  }
//  public void writeCharacter(char _c) {
//  }

  /**
   * Lecture d'un champ entier "<I>integer</I>" Fortran
   * @return int correspondant au integer
   */
  public int readInteger() throws IOException {
    currentPos_+=4;
    return super.readInt_32();
  }

  /**
   * Lecture d'un champ r�el "<I>real</I>" Fortran
   * @return float correspondant au real
   */
  public float readReal() throws IOException {
    currentPos_+=4;
    return super.readFloat_32();
  }

  /**
   * Lecture d'un champ r�el en double pr�cision "<I>double precision</I>"
   * Fortran
   * @return double correspondant au double precision
   */
  public double readDoublePrecision() throws IOException {
    currentPos_+=8;
    return super.readFloat_64();
  }
  
  /**
   * Redefinition de la methode (changement: la position courante est 
   * geree).
   */
  public byte readInt_8() throws IOException
  {
    currentPos_+=1;
    return super.readInt_8();
  }

  /**
   * Redefinition de la methode (changement: la position courante est 
   * geree).
   */
  public short readUInt_8() throws IOException
  {
    currentPos_+=1;
    return super.readUInt_8();
  }

  /**
   * Redefinition de la methode (changement: la position courante est 
   * geree).
   */
  public short readInt_16() throws IOException
  {
    currentPos_+=2;
    return super.readInt_16();
  }

  /**
   * Redefinition de la methode (changement: la position courante est 
   * geree).
   */
  public int readUInt_16() throws IOException
  {
    currentPos_+=2;
    return super.readUInt_16();
  }

  /**
   * Redefinition de la methode (changement: la position courante est 
   * geree).
   */
  public int readInt_32() throws IOException
  {
    currentPos_+=4;
    return super.readInt_32();
  }

  /**
   * Redefinition de la methode (changement: la position courante est 
   * geree).
   */
  public long readUInt_32() throws IOException
  {
    currentPos_+=4;
    return super.readUInt_32();
  }

  public long readInt_64() throws IOException
  {
    currentPos_+=8;
    return super.readInt_64();
  }

  public float readFloat_32() throws IOException
  {
    currentPos_+=4;
    return super.readFloat_32();
  }

  public double readFloat_64() throws IOException
  {
    currentPos_+=8;
    return super.readFloat_64();
  }
  
  /**
   * Renvoie en nb d'octets la position courante. 
   */
  public long positionCourante()
  {
    return currentPos_;
  }
  
  
  

  /**
   * Lecture des champs de l'enregistrement. L'enregistrement doit etre lu
   * avant la lecture des champs.
   * Un enregistrement correspond � une instruction READ du Fortran
   */
  public void readRecord() throws IOException {
    skip(nextPos_-currentPos_);
    currentPos_=nextPos_;

    if (sequential_) {
      recordLength_=readInt_32();
      nextPos_+=(long)recordLength_+8;
    }
    else {
      nextPos_+=(long)recordLength_;
    }
  }
}
