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

import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.lang.ref.WeakReference;
import java.util.StringTokenizer;

import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.JTable;
import javax.swing.KeyStroke;

/**
 * ExcelAdapter enables Copy-Paste Clipboard functionality on JTables.
 * The clipboard data format used by the adapter is compatible with
 * the clipboard format used by Excel. This provides for clipboard
 * interoperability between enabled JTables and Excel.
 * 
 * @author Ashok Banerjee & Jignesh Mehta - JavaWorld Tip #77
 */
public class ExcelAdapter implements ActionListener
{
   private String rowstring,value;
   private Clipboard system;
   private StringSelection stsel;
   private WeakReference   tableRef;

	/**
	 * The Excel Adapter is constructed with a
	 * JTable on which it enables Copy-Paste and acts
	 * as a Clipboard listener.
	 */

   public ExcelAdapter(JTable myJTable)
   {
      tableRef = new WeakReference(myJTable);

      KeyStroke copy =
         KeyStroke.getKeyStroke(KeyEvent.VK_C,ActionEvent.CTRL_MASK,false);

      // Identifying the copy KeyStroke user can modify this
      // to copy on some other Key combination.
      KeyStroke paste =
         KeyStroke.getKeyStroke(KeyEvent.VK_V,ActionEvent.CTRL_MASK,false);

      // Identifying the Paste KeyStroke user can modify this
      //to copy on some other Key combination.

      myJTable.
         registerKeyboardAction(this,"Copy",copy,JComponent.WHEN_FOCUSED);
      myJTable.
         registerKeyboardAction(this,"Paste",paste,JComponent.WHEN_FOCUSED);

      system = Toolkit.getDefaultToolkit().getSystemClipboard();
   }

   /**
    * Public Accessor methods for the Table on which this adapter acts.
    */
   public JTable getJTable() {return ((JTable) tableRef.get());}

   public void setJTable(JTable jTable1)
   {
      tableRef = new WeakReference(jTable1);
   }

   /**
    * This method is activated on the Keystrokes we are listening to
    * in this implementation. Here it listens for Copy and Paste ActionCommands.
    * Selections comprising non-adjacent cells result in invalid selection and
    * then copy action cannot be performed.
    * Paste is done by aligning the upper left corner of the selection with the
    * 1st element in the current selection of the JTable.
    */
   public void actionPerformed(ActionEvent e)
   {
      JTable   table = (JTable) tableRef.get();

      if (e.getActionCommand().compareTo("Copy")==0)
      {
         StringBuffer sbf=new StringBuffer();

         // Check to ensure we have selected only a contiguous block of
         // cells
         int numcols=table.getSelectedColumnCount();
         int numrows=table.getSelectedRowCount();
         int[] rowsselected=table.getSelectedRows();
         int[] colsselected=table.getSelectedColumns();

         if (!((numrows-1==rowsselected[rowsselected.length-1]-rowsselected[0] &&
                numrows==rowsselected.length) &&

               (numcols-1==colsselected[colsselected.length-1]-colsselected[0] &&
                numcols==colsselected.length)))
         {
            JOptionPane.showMessageDialog(null, "Invalid Copy Selection",
                                          "Invalid Copy Selection",
                                          JOptionPane.ERROR_MESSAGE);

            return;
         }


         for (int i = 0; i < numrows; ++i)
         {
            for (int j = 0; j < numcols; ++j)
            {
               sbf.append(table.getValueAt(rowsselected[i], colsselected[j]));
               if (j < numcols - 1)
                  sbf.append("\t");
            }

            sbf.append("\n");
         }
         
         stsel = new StringSelection(sbf.toString());
         system = Toolkit.getDefaultToolkit().getSystemClipboard();
         system.setContents(stsel,stsel);
      }

      if (e.getActionCommand().compareTo("Paste")==0)
      {
         //System.out.println("Trying to Paste");
         int startRow=(table.getSelectedRows())[0];
         int startCol=(table.getSelectedColumns())[0];
         try
         {
            String   trstring =
               (String)(system.getContents(this).
                        getTransferData(DataFlavor.stringFlavor));

            //System.out.println("String is:"+trstring);
            StringTokenizer st1=new StringTokenizer(trstring,"\n");
            for(int i=0;st1.hasMoreTokens();i++)
            {
               rowstring=st1.nextToken();
               StringTokenizer st2=new StringTokenizer(rowstring,"\t");

               for(int j=0;st2.hasMoreTokens();j++)
               {
                  value=(String)st2.nextToken();
                  if (startRow+i< table.getRowCount() &&
                      startCol+j< table.getColumnCount())
                     table.setValueAt(value,startRow+i,startCol+j);
                  //System.out.println("Putting " + value +"at row=" +
                  //                  startRow + i + "column=" + startCol + j);
               }
            }
         }
         catch(Exception ex)
         { ex.printStackTrace(); }
      }
   }
}