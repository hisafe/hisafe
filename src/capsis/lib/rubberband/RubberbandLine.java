/*--- formatted by Jindent 2.1, (www.c-lab.de/~jindent) ---*/

package capsis.lib.rubberband;

import java.awt.Component;
import java.awt.Graphics;

/**
 * A Rubberband that does lines.
 * 
 * @version 1.0, 12/27/95
 * @author  David Geary
 * @see     Rubberband
 * @see "gjt.test.RubberbandTest"
 */
public class RubberbandLine extends Rubberband {

  /**
   * Constructor declaration
   * 
   * 
   * @param component
   */
  public RubberbandLine(Component component) {
    super(component);
  }

  /**
   * Method declaration
   * 
   * 
   * @param graphics
   */
  public void drawLast(Graphics graphics) {
    graphics.drawLine(anchor.x, anchor.y, last.x, last.y);
  } 

  /**
   * Method declaration
   * 
   * 
   * @param graphics
   */
  public void drawNext(Graphics graphics) {
    graphics.drawLine(anchor.x, anchor.y, stretched.x, stretched.y);
  } 

}



/*--- formatting done in "Guillaume CORNU" style on 11-24-1999 ---*/

