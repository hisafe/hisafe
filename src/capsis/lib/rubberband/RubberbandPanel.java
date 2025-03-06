/*--- formatted by Jindent 2.1, (www.c-lab.de/~jindent) ---*/

package capsis.lib.rubberband;

import java.awt.Event;
import java.awt.Panel;
import java.awt.Point;

/**
 * An extension of Panel which is fitted with a Rubberband.
 * Handling of mouse events is automatically handled for
 * rubberbanding.<p>
 * 
 * Clients may set or get the Rubberband at any time.<p>
 * 
 * @version 1.0, Dec 27 1995
 * @author  David Geary
 * @see     Rubberband
 * @see "gjt.test.RubberbandTest"
 */
public class RubberbandPanel extends Panel {
  private Rubberband rubberband;

  /**
   * Method declaration
   * 
   * 
   * @param rubberband
   */
  public void setRubberband(Rubberband rubberband) {
    this.rubberband = rubberband;
  } 

  /**
   * Method declaration
   * 
   */
  public Rubberband getRubberband() {
    return rubberband;
  } 

  /**
   * Method declaration
   * 
   * 
   * @param event
   * @param x
   * @param y
   * 
   */
  public boolean mouseDown(Event event, int x, int y) {
    rubberband.anchor(new Point(x, y));

    return false;
  } 

  /**
   * Method declaration
   * 
   * 
   * @param event
   * @param x
   * @param y
   * 
   */
  public boolean mouseDrag(Event event, int x, int y) {
    rubberband.stretch(new Point(x, y));

    return false;
  } 

  /**
   * Method declaration
   * 
   * 
   * @param event
   * @param x
   * @param y
   * 
   */
  public boolean mouseUp(Event event, int x, int y) {
    rubberband.end(new Point(x, y));

    return false;
  } 

}



/*--- formatting done in "Guillaume CORNU" style on 11-24-1999 ---*/

