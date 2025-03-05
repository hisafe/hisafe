package capsis.extension.datarenderer.drgraph;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import javax.swing.JPanel;

import org.jfree.chart.block.RectangleConstraint;
import org.jfree.chart.title.LegendTitle;
import org.jfree.ui.Size2D;

/**
 * Trying to manage large legends in graphs.
 * 
 * @author F. de Coligny - April 2018
 */
public class LegendPanel extends JPanel {

	private DRLegendExplorer legendExplorer;
	private Size2D size;

	/**
	 * Constructor
	 */
	public LegendPanel(DRLegendExplorer legendExplorer) {
		this.legendExplorer = legendExplorer;
		
		this.setBackground(Color.WHITE);
		this.setOpaque(true);
		
		this.size = new Size2D(600, 200);

	}

	/**
	 * Triggers a refresh (repaint () call paintComponent ()).
	 */
	public void update() {
		
		this.revalidate();
		this.repaint();
		
		

	}

	/**
	 * Paints the legend items in the panel.
	 */
	@Override
	protected void paintComponent(Graphics g) {
		super.paintComponent(g);

		Graphics2D g2 = (Graphics2D) g;

		LegendTitle legendTitle = legendExplorer.getLegendTitle();

		size = legendTitle.arrange(g2, new RectangleConstraint(600, 300));

		Rectangle2D area = new Rectangle2D.Double(0, 0, size.getWidth(), size.getHeight());
		
		// Rectangle2D area = new Rectangle2D.Double(0, 0, 500, 500);

		legendTitle.draw(g2, area);

	}

	// @Override
	public Dimension getPreferredSize() {
		return new Dimension((int) size.width, (int) size.height);
	}

}
