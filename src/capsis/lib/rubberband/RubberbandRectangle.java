/*--- formatted by Jindent 2.1, (www.c-lab.de/~jindent) ---*/

package capsis.lib.rubberband;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * A Rubberband that does rectangles.
 * 
 * @version 1.00, 12/27/95
 * @author  David Geary
 * @see     Rubberband
 * @see "gjt.test.RubberbandTest"
 */
public class RubberbandRectangle extends Rubberband {

  /**
   * Constructor declaration
   * 
   * 
   * @param component
   */
  public RubberbandRectangle(Component component) {
    super(component);
  }

  /**
   * Method declaration
   * 
   * 
   * @param graphics
   */
  public void drawLast(Graphics graphics) {
    Rectangle rect = lastBounds();

    graphics.drawRect(rect.x, rect.y, rect.width, rect.height);
  } 

  /**
   * Method declaration
   * 
   * 
   * @param graphics
   */
  public void drawNext(Graphics graphics) {
    Rectangle rect = bounds();

    graphics.drawRect(rect.x, rect.y, rect.width, rect.height);
  } 

}



/*--- formatting done in "Guillaume CORNU" style on 11-24-1999 ---*/

