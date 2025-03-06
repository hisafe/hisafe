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

import java.io.DataInput;
import java.io.EOFException;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.PushbackInputStream;
import java.io.UTFDataFormatException;

/**
* This class is a copy of DataInputStream with a difference concerning the short / int / long 
* codage. DataInputStream reads them high byte first. SwapDataInputStream reads them 
* low byte first, in reverse order. This was made to read a stream that can be generated 
* through the network by a C program under Linux. For connexion with a C program under 
* IRIX (SGI), use DataInputStream instead (high byte first codage).
* This code is strongly based upon DataInputStream.

* @author F. de Coligny - november 2002
*/
public class SwapDataInputStream extends FilterInputStream implements DataInput {
	/**
	* Creates a <code>FilterInputStream</code>
	* and saves its  argument, the input stream
	* <code>in</code>, for later use. An internal
	*
	* @param  in   the input stream.
	*/
	public SwapDataInputStream(InputStream in) {
		super(in);
	}

	/**
	* See the general contract of the <code>read</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @param      b   the buffer into which the data is read.
	* @return     the total number of bytes read into the buffer, or
	*             <code>-1</code> if there is no more data because the end
	*             of the stream has been reached.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	* @see        java.io.InputStream#read(byte[], int, int)
	*/
	public final int read(byte b[]) throws IOException {
		return in.read(b, 0, b.length);
	}

	/**
	* See the general contract of the <code>read</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @param      b     the buffer into which the data is read.
	* @param      off   the start offset of the data.
	* @param      len   the maximum number of bytes read.
	* @return     the total number of bytes read into the buffer, or
	*             <code>-1</code> if there is no more data because the end
	*             of the stream has been reached.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	* @see        java.io.InputStream#read(byte[], int, int)
	*/
	public final int read(byte b[], int off, int len) throws IOException {
		return in.read(b, off, len);
	}

	/**
	* See the general contract of the <code>readFully</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @param      b   the buffer into which the data is read.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading all the bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final void readFully(byte b[]) throws IOException {
		readFully(b, 0, b.length);
	}

	/**
	* See the general contract of the <code>readFully</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @param      b     the buffer into which the data is read.
	* @param      off   the start offset of the data.
	* @param      len   the number of bytes to read.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading all the bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final void readFully(byte b[], int off, int len) throws IOException {
		if (len < 0)
			throw new IndexOutOfBoundsException();
		InputStream in = this.in;
		int n = 0;
		while (n < len) {
			int count = in.read(b, off + n, len - n);
			if (count < 0)
				throw new EOFException();
			n += count;
		}
	}

	/**
	* See the general contract of the <code>skipBytes</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @param      n   the number of bytes to be skipped.
	* @return     the actual number of bytes skipped.
	* @exception  IOException   if an I/O error occurs.
	*/
	public final int skipBytes(int n) throws IOException {
		InputStream in = this.in;
		int total = 0;
		int cur = 0;
		
		while ((total<n) && ((cur = (int) in.skip(n-total)) > 0)) {
			total += cur;
		}
		
		return total;
	}
	
	/**
	* See the general contract of the <code>readBoolean</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the <code>boolean</code> value read.
	* @exception  EOFException  if this input stream has reached the end.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final boolean readBoolean() throws IOException {
		int ch = in.read();
		if (ch < 0)
			throw new EOFException();
		return (ch != 0);
	}

	/**
	* See the general contract of the <code>readByte</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next byte of this input stream as a signed 8-bit
	*             <code>byte</code>.
	* @exception  EOFException  if this input stream has reached the end.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final byte readByte() throws IOException {
		int ch = in.read();
		if (ch < 0)
			throw new EOFException();
		return (byte)(ch);
	}

	/**
	* See the general contract of the <code>readUnsignedByte</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next byte of this input stream, interpreted as an
	*             unsigned 8-bit number.
	* @exception  EOFException  if this input stream has reached the end.
	* @exception  IOException   if an I/O error occurs.
	* @see         java.io.FilterInputStream#in
	*/
	public final int readUnsignedByte() throws IOException {
		int ch = in.read();
		if (ch < 0)
			throw new EOFException();
		return ch;
	}

	/**
	* See the general contract of the <code>readShort</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next two bytes of this input stream, interpreted in reverse order as a
	*             signed 16-bit number.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading two bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final short readShort() throws IOException {
		InputStream in = this.in;
		int ch1 = in.read();
		int ch2 = in.read();
		if ((ch1 | ch2) < 0)
			throw new EOFException();
		return (short)((ch2 << 8) + (ch1 << 0));	// fc - swapped bytes - nov 2002
	}

	/**
	* See the general contract of the <code>readUnsignedShort</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next two bytes of this input stream, interpreted in reverse order  as an
	*             unsigned 16-bit integer.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading two bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final int readUnsignedShort() throws IOException {
		InputStream in = this.in;
		int ch1 = in.read();
		int ch2 = in.read();
		if ((ch1 | ch2) < 0)
			throw new EOFException();
		return (ch2 << 8) + (ch1 << 0);	// fc - swapped bytes - nov 2002
	}

	/**
	* See the general contract of the <code>readChar</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next two bytes of this input stream as a Unicode
	*             character.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading two bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final char readChar() throws IOException {
		InputStream in = this.in;
		int ch1 = in.read();
		int ch2 = in.read();
		if ((ch1 | ch2) < 0)
			throw new EOFException();
		return (char)((ch1 << 8) + (ch2 << 0));
	}

	/**
	* See the general contract of the <code>readInt</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next four bytes of this input stream, interpreted in reverse order as an
	*             <code>int</code>.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading four bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final int readInt() throws IOException {
		InputStream in = this.in;
		int ch1 = in.read();
		int ch2 = in.read();
		int ch3 = in.read();
		int ch4 = in.read();
		if ((ch1 | ch2 | ch3 | ch4) < 0)
			throw new EOFException();
		return ((ch4 << 24) + (ch3 << 16) + (ch2 << 8) + (ch1 << 0));	// fc - swapped bytes - nov 2002
	}

	/**
	* See the general contract of the <code>readLong</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next eight bytes of this input stream, interpreted in reverse order as a
	*             <code>long</code>.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading eight bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.FilterInputStream#in
	*/
	public final long readLong() throws IOException {
		InputStream in = this.in;
		//return ((long)(readInt()) << 32) + (readInt() & 0xFFFFFFFFL);
		return ((long)(readInt() & 0xFFFFFFFFL) + (readInt()) << 32);	// fc - swapped bytes - nov 2002
	}
	
	/**
	* See the general contract of the <code>readFloat</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next four bytes of this input stream, interpreted in reverse order as a
	*             <code>float</code>.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading four bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.DataInputStream#readInt()
	* @see        java.lang.Float#intBitsToFloat(int)
	*/
	public final float readFloat() throws IOException {
		return Float.intBitsToFloat(readInt());
	}

	/**
	* See the general contract of the <code>readDouble</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     the next eight bytes of this input stream, interpreted in reverse order as a
	*             <code>double</code>.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading eight bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.DataInputStream#readLong()
	* @see        java.lang.Double#longBitsToDouble(long)
	*/
	public final double readDouble() throws IOException {
		return Double.longBitsToDouble(readLong());
	}

	private char lineBuffer[];

	/**
	* See the general contract of the <code>readLine</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @deprecated This method does not properly convert bytes to characters.
	* As of JDK&nbsp;1.1, the preferred way to read lines of text is via the
	* <code>BufferedReader.readLine()</code> method.  Programs that use the
	* <code>SwapDataInputStream</code> class to read lines can be converted to use
	* the <code>BufferedReader</code> class by replacing code of the form:
	* <blockquote><pre>
	*     SwapDataInputStream d =&nbsp;new&nbsp;SwapDataInputStream(in);
	* </pre></blockquote>
	* with:
	* <blockquote><pre>
	*     BufferedReader d
	*          =&nbsp;new&nbsp;BufferedReader(new&nbsp;InputStreamReader(in));
	* </pre></blockquote>
	*
	* @return     the next line of text from this input stream.
	* @exception  IOException  if an I/O error occurs.
	* @see        java.io.BufferedReader#readLine()
	* @see        java.io.FilterInputStream#in
	*/
	public final String readLine() throws IOException {
		InputStream in = this.in;
		char buf[] = lineBuffer;
		
		if (buf == null) {
			buf = lineBuffer = new char[128];
		}
		
		int room = buf.length;
		int offset = 0;
		int c;
		
		loop:	while (true) {
			switch (c = in.read()) {
				case -1:
				case '\n':
					break loop;
				
				case '\r':
					int c2 = in.read();
					if ((c2 != '\n') && (c2 != -1)) {
					if (!(in instanceof PushbackInputStream)) {
					in = this.in = new PushbackInputStream(in);
					}
					((PushbackInputStream)in).unread(c2);
					}
					break loop;
				
				default:
					if (--room < 0) {
					buf = new char[offset + 128];
					room = buf.length - offset - 1;
					System.arraycopy(lineBuffer, 0, buf, 0, offset);
					lineBuffer = buf;
					}
					buf[offset++] = (char) c;
					break;
			}
		}
		if ((c == -1) && (offset == 0)) {
			return null;
		}
		return String.copyValueOf(buf, 0, offset);
	}

	/**
	* See the general contract of the <code>readUTF</code>
	* method of <code>DataInput</code>.
	* <p>
	* Bytes
	* for this operation are read from the contained
	* input stream.
	*
	* @return     a Unicode string.
	* @exception  EOFException  if this input stream reaches the end before
	*               reading all the bytes.
	* @exception  IOException   if an I/O error occurs.
	* @see        java.io.DataInputStream#readUTF(java.io.DataInput)
	*/
	public final String readUTF() throws IOException {
		return readUTF(this);
	}

	/**
	* Reads from the
	* stream <code>in</code> a representation
	* of a Unicode  character string encoded in
	* Java modified UTF-8 format; this string
	* of characters  is then returned as a <code>String</code>.
	* The details of the modified UTF-8 representation
	* are  exactly the same as for the <code>readUTF</code>
	* method of <code>DataInput</code>.
	*
	* @param      in   a data input stream.
	* @return     a Unicode string.
	* @exception  EOFException            if the input stream reaches the end
	*               before all the bytes.
	* @exception  IOException             if an I/O error occurs.
	* @exception  UTFDataFormatException  if the bytes do not represent a
	*               valid UTF-8 encoding of a Unicode string.
	* @see        java.io.DataInputStream#readUnsignedShort()
	*/
	public final static String readUTF(DataInput in) throws IOException {
		int utflen = in.readUnsignedShort();
		StringBuffer str = new StringBuffer(utflen);
		byte bytearr [] = new byte[utflen];
		int c, char2, char3;
		int count = 0;
		
		in.readFully(bytearr, 0, utflen);
		
		while (count < utflen) {
			c = (int) bytearr[count] & 0xff;
			switch (c >> 4) {
				case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
					/* 0xxxxxxx*/
					count++;
					str.append((char)c);
					break;
				case 12: case 13:
					/* 110x xxxx   10xx xxxx*/
					count += 2;
					if (count > utflen)
						throw new UTFDataFormatException();
					char2 = (int) bytearr[count-1];
					if ((char2 & 0xC0) != 0x80)
						throw new UTFDataFormatException(); 
					str.append((char)(((c & 0x1F) << 6) | (char2 & 0x3F)));
					break;
				case 14:
					/* 1110 xxxx  10xx xxxx  10xx xxxx */
					count += 3;
					if (count > utflen)
						throw new UTFDataFormatException();
					char2 = (int) bytearr[count-2];
					char3 = (int) bytearr[count-1];
					if (((char2 & 0xC0) != 0x80) || ((char3 & 0xC0) != 0x80))
						throw new UTFDataFormatException();	  
					str.append((char)(((c & 0x0F) << 12) |
							((char2 & 0x3F) << 6)  |
							((char3 & 0x3F) << 0)));
					break;
				default:
					/* 10xx xxxx,  1111 xxxx */
					throw new UTFDataFormatException();		  
			}
		}
		// The number of chars produced may be less than utflen
		return new String(str);
	}
}
