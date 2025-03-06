/*--- formatted by Jindent 2.1, (www.c-lab.de/~jindent) ---*/

package capsis.lib.rubberband;

import java.awt.Component;
import java.awt.Graphics;
import java.awt.Rectangle;

/**
 * A Rubberband that does ellipses.
 * 
 * @version 1.00, 12/27/95
 * @author  David Geary
 * @see     Rubberband
 * @see "gjt.test.RubberbandTest"
 */
public class RubberbandEllipse extends Rubberband {
  private final int startAngle = 0;
  private final int endAngle = 360;

  /**
   * Constructor declaration
   * 
   * 
   * @param component
   */
  public RubberbandEllipse(Component component) {
    super(component);
  }

  /**
   * Method declaration
   * 
   * 
   * @param graphics
   */
  public void drawLast(Graphics graphics) {
    Rectangle r = lastBounds();

    graphics.drawArc(r.x, r.y, r.width, r.height, startAngle, endAngle);
  } 

  /**
   * Method declaration
   * 
   * 
   * @param graphics
   */
  public void drawNext(Graphics graphics) {
    Rectangle r = bounds();

    graphics.drawArc(r.x, r.y, r.width, r.height, startAngle, endAngle);
  } 

}



/*--- formatting done in "Guillaume CORNU" style on 11-24-1999 ---*/

