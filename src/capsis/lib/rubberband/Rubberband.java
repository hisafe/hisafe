/*--- formatted by Jindent 2.1, (www.c-lab.de/~jindent) ---*/

package capsis.lib.rubberband;

import java.awt.BasicStroke;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Point;
import java.awt.Rectangle;

/**
 * A abstract base class for rubberbands.<p>
 * 
 * Rubberbands do their rubberbanding inside of a Component,
 * which must be specified at construction time.<p>
 * 
 * Subclasses are responsible for implementing
 * <em>void drawLast(Graphics g)</em> and
 * <em>void drawNext(Graphics g)</em>.
 * 
 * drawLast() draws the appropriate geometric shape at the last
 * rubberband location, while drawNext() draws the appropriate
 * geometric shape at the next rubberband location.  All of the
 * underlying support for rubberbanding is taken care of here,
 * including handling XOR mode setting; extensions of Rubberband
 * need not concern themselves with anything but drawing the
 * last and next geometric shapes.<p>
 * GCO : added constrained, centered and dashed outline
 * 
 * @version 1.00, 12/27/95
 * @author  David Geary
 * @see     RubberbandLine
 * @see     RubberbandRectangle
 * @see     RubberbandEllipse
 */
abstract public class Rubberband {
  protected Point anchor = new Point(0, 0);
  protected Point stretched = new Point(0, 0);
  protected Point last = new Point(0, 0);
  protected Point end = new Point(0, 0);
  private Component component;
  private boolean firstStretch = true;
  private boolean constrained = false;      // GCO dx=dy
  private boolean centered = false;         // GCO
  private final static float dash1[] = {    // GCO dashed outline
    4.0f
  };
  //fc - jdk 1.4.1 bug : stroke size must be 1f (instead of 0f for jdk 1.3)
  private final static BasicStroke dashed = new BasicStroke(1f, 	// was 0, fc put 1
          BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash1, 0.0f);

  /**
   * Method declaration
   * 
   * 
   * @param g
   */
  abstract public void drawLast(Graphics g);

  /**
   * Method declaration
   * 
   * 
   * @param g
   */
  abstract public void drawNext(Graphics g);

  /**
   * Constructor declaration
   * 
   * 
   * @param component
   */
  public Rubberband(Component component) {
    setComponent(component);
  }

  /**
   * Method declaration
   * 
   * 
   * @param component
   */
  public void setComponent(Component component) {
    this.component = component;
  } 

  /**
   * Method declaration
   * 
   */
  public Point getAnchor() {
    return anchor;
  } 

  /**
   * Method declaration
   * 
   */
  public Point getStretched() {
    return stretched;
  } 

  /**
   * Method declaration
   * 
   */
  public Point getLast() {
    return last;
  } 

  /**
   * Method declaration
   * 
   */
  public Point getEnd() {
    return end;
  } 

  /**
   * Method declaration
   * 
   * 
   * @param p
   * 
   */
  public void anchor(Point p) {
    firstStretch = true;
    anchor.x = p.x;
    anchor.y = p.y;
    stretched.x = last.x = anchor.x;
    stretched.y = last.y = anchor.y;
  } 

	/**
	* Method declaration
	* 
	* 
	* @param p
	* 
	*/
	public void stretch(Point p) {
		
		//~ System.out.println ("rubberband - STRETCH");
		
		last.x = stretched.x;
		last.y = stretched.y;
		
		if (constrained) {    // GCO constrained support
			int dx = p.x - anchor.x;
			int dy = p.y - anchor.y;
			
			if (Math.abs(dx) < Math.abs(dy)) {
				dy = dy > 0 ? Math.abs(dx) : -Math.abs(dx);
			} else {
				dx = dx > 0 ? Math.abs(dy) : -Math.abs(dy);
			} 
			
			p.setLocation(anchor.x + dx, anchor.y + dy);
		} 
		
		stretched.x = p.x;
		stretched.y = p.y;
		
		Graphics g = component.getGraphics();
		
		if (g != null) {
			
			//~ System.out.println ("g != null");
			
			((Graphics2D) g).setStroke(dashed);    // GCO dashed outline
			g.setXORMode(component.getBackground()); 
			
			if (firstStretch == true) {
				firstStretch = false;
			} else {
				drawLast(g);
			} 
			
			drawNext(g);
			
		} 
	} 

	/**
	* Method declaration
	* 
	* 
	* @param p
	* 
	*/
	public void end(Point p) {
		
		//~ System.out.println ("rubberband - END");
		
		if (constrained) {    // GCO constrained support
			int dx = p.x - anchor.x;
			int dy = p.y - anchor.y;
			
			if (Math.abs(dx) < Math.abs(dy)) {
				dy = dy > 0 ? Math.abs(dx) : -Math.abs(dx);
			} else {
				dx = dx > 0 ? Math.abs(dy) : -Math.abs(dy);
			} 
			
			p.setLocation(anchor.x + dx, anchor.y + dy);
		} 
		
		last.x = end.x = p.x;
		last.y = end.y = p.y;
		
		Graphics g = component.getGraphics();
		
		if (g != null) {
			
			//~ System.out.println ("g != null");
			
			((Graphics2D) g).setStroke(dashed);    // GCO dashed outline
			g.setXORMode(component.getBackground());
			drawLast(g);
		} 
	} 
	
  /**
   * Method declaration
   * 
   */
  public Rectangle bounds() {
    int dx = Math.abs(stretched.x - anchor.x);
    int dy = Math.abs(stretched.y - anchor.y);

    if (centered) { // GCO centered support
      return new Rectangle(anchor.x - dx, anchor.y - dy, dx * 2, dy * 2);
    } else {
      return new Rectangle(stretched.x < anchor.x ? stretched.x : anchor.x, 
                           stretched.y < anchor.y ? stretched.y : anchor.y, 
                           dx, dy);
    } 
  } 

  /**
   * Method declaration
   * 
   */
  public Rectangle lastBounds() {
    int dx = Math.abs(last.x - anchor.x);
    int dy = Math.abs(last.y - anchor.y);

    if (centered) { // GCO centered support
      return new Rectangle(anchor.x - dx, anchor.y - dy, dx * 2, dy * 2);
    } else {
      return new Rectangle(last.x < anchor.x ? last.x : anchor.x, 
                           last.y < anchor.y ? last.y : anchor.y, 
                           Math.abs(last.x - anchor.x), 
                           Math.abs(last.y - anchor.y));
    } 
  } 

  /**
   * Method declaration
   * 
   * 
   * @param c
   */
  public void setConstrained(boolean c) {
    constrained = c;
  } 

  /**
   * Method declaration
   * 
   */
  public boolean isConstrained() {
    return constrained;
  } 

  /**
   * Method declaration
   * 
   * 
   * @param c
   */
  public void setCentered(boolean c) {
    centered = c;
  } 

  /**
   * Method declaration
   * 
   */
  public boolean isCentered() {
    return centered;
  } 

}



/*--- formatting done in "Guillaume CORNU" style on 10-17-2000 ---*/

