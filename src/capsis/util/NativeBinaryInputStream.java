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

import java.io.DataInputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * Une classe etendant DataInputStream et permettant de facilement lire
 * des fichiers binaires dependants de la machine (sparc, i386, ...)
 * Attention: cette classe est utilisée par FortranBinaryInputStream. A chaque
 * appel de fonction le compteur de FortranBinaryInputStream est incrémenté.
 * Pour l'instant ,il ne faut pas chainer les appels de methodes.<br/>
 * Ne pas faire:<br/>
 * public int readToto()
 * {
 *   readToto2();
 * }
 * il y aura eu 2 increments dans FortranBinaryInputStream ( pour readToto
 * et pour readToto2)) ce qui fausse la lecture.
 *
 * @version      $Revision: 1.8 $ $Date: 2002/12/17 16:47:12 $ by $Author: deniger $
 * @author       Axel von Arnim
 */
public class NativeBinaryInputStream
             extends DataInputStream
{
  public final static String X86="86";
  public final static String X86_NAME="X86";
  public final static String SPARC="sparc";
  public final static String SPARC_NAME="SPARC";

  /**
   * entier renvoye par la methode read.
   */
  private int nbByteLus_;

  int l1, l2, l3, l4, l5, l6, l7, l8;
  int i1, i2, i3, i4;
  int s1, s2;
  byte buf[];

  public NativeBinaryInputStream(InputStream _in, String _machine) throws IOException
  {
    super(_in);
    buf=null;
    setMachineType(_machine);
  }

/**
 * Renvoie a partir de l'identifiant de la machine <code>_desc</code>,
 * l'identifiant correctement géré par cette classe. Si non trouvee, <code>null</code>
 * est retourne.
 */
  public static String getMachineId(String _desc)
  {
    if(isX86(_desc)) return X86;
    else if(isSparc(_desc)) return SPARC;
    else return null;
  }


  /**
   * Renvoie true si <code>_machine</code> est du type X86 ( si la chaine finit
   * par <code>"X86"</code>).
   */
  public static boolean isX86(String _machine)
  {
    if(_machine==null) return false;
    if( _machine.endsWith(X86) ) return true;
    return false;
  }

  /**
   * Renvoie true, si <code>_machine</code> est un identifiant d'une machine
   * sparc ( si finit par <code>"sparc"</code>).
   */
  public static boolean isSparc(String _machine)
  {
    if(_machine==null) return false;
    if( _machine.endsWith(SPARC) ) return true;
    return false;
  }

  /**
   * Renvoie <code>true</code> si <code>_machine</code> est geree par cette
   * classe.
   */
  public static boolean isMachineKnown(String _machine)
  {
    return (isX86(_machine)||isSparc(_machine));
  }

  public void setMachineType(String _machine)
  {
    if( isX86(_machine )) {
      l1=i1=s1=0; l2=i2=s2=1; l3=i3=2; l4=i4=3; l5=4; l6=5; l7=6; l8=7;
    } else {
      l1=7; l2=6; l3=5; l4=4; l5=i1=3; l6=i2=2; l7=i3=s1=1; l8=i4=s2=0;
    }
  }

  public byte readInt_8() throws IOException
  {
    buf=new byte[1];
    nbByteLus_=read(buf);
    return buf[0];
  }

  public short readUInt_8() throws IOException
  {
    buf=new byte[1];
    nbByteLus_=read(buf);
    return buf[0]<0?(short)(256+buf[0]):(short)buf[0];
  }

  /**
   * Indique si la fin du fichier est atteinte. Si  <code>true</code>
   * ,cela signifie que la derniere lecture est erronnee.
   * @return si fin du fichier.
   */
  public boolean isFinFichier()
  {
    return nbByteLus_==-1;
  }


  public short readInt_16() throws IOException
  {
    buf=new byte[2];
    nbByteLus_=read(buf);
    return (short)((buf[s1]<0?(256+buf[s1]):buf[s1])+
                   (buf[s2]*0x100));
  }

  public int readUInt_16() throws IOException
  {
    buf=new byte[2];
    nbByteLus_=read(buf);
    return (buf[s1]<0?(256+buf[s1]):buf[s1])+
           (buf[s2]<0?(256+buf[s2]):buf[s2])*0x100;
  }

  public int readInt_32() throws IOException
  {
    buf=new byte[4];
    nbByteLus_=read(buf);
    return (buf[i1]<0?(256+buf[i1]):buf[i1])+
           (buf[i2]<0?(256+buf[i2]):buf[i2])*0x100+
           (buf[i3]<0?(256+buf[i3]):buf[i3])*0x10000+
           (buf[i4]*0x1000000);
  }

  public long readUInt_32() throws IOException
  {
    buf=new byte[4];
    nbByteLus_=read(buf);
    return (buf[i1]<0?(256+buf[i1]):buf[i1])+
           (buf[i2]<0?(256+buf[i2]):buf[i2])*0x100L+
           (buf[i3]<0?(256+buf[i3]):buf[i3])*0x10000L+
           (buf[i4]<0?(256+buf[i4]):buf[i4])*0x1000000L;
  }

  public long readInt_64() throws IOException
  {
    buf=new byte[8];
    nbByteLus_=read(buf);
    return (buf[l1]<0?(256+buf[l1]):buf[l1])+
           (buf[l2]<0?(256+buf[l2]):buf[l2])*0x100L+
           (buf[l3]<0?(256+buf[l3]):buf[l3])*0x10000L+
           (buf[l4]<0?(256+buf[l4]):buf[l4])*0x1000000L+
           (buf[l5]<0?(256+buf[l5]):buf[l5])*0x100000000L+
           (buf[l6]<0?(256+buf[l6]):buf[l6])*0x10000000000L+
           (buf[l7]<0?(256+buf[l7]):buf[l7])*0x1000000000000L+
           (buf[l8])*0x100000000000000L;
  }

  public float readFloat_32() throws IOException
  {
    /*Reviens a faire readInt_32().
    Ne pas utiliser cette méthode car fausse l'incrementation de
    FortranBinaryInputStream*/
    buf=new byte[4];
    nbByteLus_=read(buf);
    int temp= (buf[i1]<0?(256+buf[i1]):buf[i1])+
           (buf[i2]<0?(256+buf[i2]):buf[i2])*0x100+
           (buf[i3]<0?(256+buf[i3]):buf[i3])*0x10000+
           (buf[i4]*0x1000000);
    return Float.intBitsToFloat(temp);
  }

  public double readFloat_64() throws IOException
  {
    /*Reviens a faire readInt_64().
    Ne pas utiliser cette méthode car fausse l'incrementation de
    FortranBinaryInputStream*/
    buf=new byte[8];
    nbByteLus_=read(buf);
    long temp= (buf[l1]<0?(256+buf[l1]):buf[l1])+
           (buf[l2]<0?(256+buf[l2]):buf[l2])*0x100L+
           (buf[l3]<0?(256+buf[l3]):buf[l3])*0x10000L+
           (buf[l4]<0?(256+buf[l4]):buf[l4])*0x1000000L+
           (buf[l5]<0?(256+buf[l5]):buf[l5])*0x100000000L+
           (buf[l6]<0?(256+buf[l6]):buf[l6])*0x10000000000L+
           (buf[l7]<0?(256+buf[l7]):buf[l7])*0x1000000000000L+
           (buf[l8])*0x100000000000000L;
    return Double.longBitsToDouble(temp);
  }
}
