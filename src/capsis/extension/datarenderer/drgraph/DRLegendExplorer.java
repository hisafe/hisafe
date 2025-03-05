package capsis.extension.datarenderer.drgraph;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.SwingConstants;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.LegendItem;
import org.jfree.chart.LegendItemCollection;
import org.jfree.chart.LegendItemSource;
import org.jfree.chart.block.BlockBorder;
import org.jfree.chart.title.LegendTitle;
import org.jfree.ui.HorizontalAlignment;
import org.jfree.ui.RectangleEdge;

import jeeb.lib.util.DialogWithClose;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * A manager to deal with large legends in a JFreeChart.
 * 
 * @author F. de Coligny - April 2018
 */
public class DRLegendExplorer {

	private String chartTitle;

	private ChartPanel chartPanel;
	private JFreeChart chart;

	private LegendTitle legendTitle;
	private LegendItemSource[] sources;
	private List<LegendItem> legendItems;

	private LegendPanel legendPanel;
	private JLabel openLegend;
	private DialogWithClose dlg;

	/**
	 * Constructor
	 */
	public DRLegendExplorer() {
		// call update after construction

		// Will show a large legend if needed
		legendPanel = new LegendPanel(this);

	}

	/**
	 * Updates the explorer with the given data.
	 */
	public void update(String chartTitle, ChartPanel chartPanel) {

		this.chartTitle = chartTitle;
		this.chartPanel = chartPanel;
		this.chart = chartPanel.getChart();

		this.legendTitle = new LegendTitle(chart.getPlot());
		this.sources = legendTitle.getSources();

		this.legendItems = new ArrayList<>();

		for (LegendItemSource itemSource : sources) {

			LegendItemCollection liCollection = itemSource.getLegendItems();

			for (int i = 0; i < liCollection.getItemCount(); i++) {
				LegendItem legendItem = liCollection.get(i);
				legendItems.add(legendItem);
			}

		}

//		System.out.println(trace());

		// update the legendPanel if any
		if (dlg != null && dlg.isVisible()) {
			
			legendPanel.update();
			
			// To update the scrollbars
			Dimension size = dlg.getSize ();
			dlg.setSize (size.width+1, size.height+1);
			
		}

	}

	public void dispose() {

		// dispose the dialog if not null
		if (dlg != null)
			dlg.dispose();

	}

	public int getLegendItemCount() {
		return legendItems.size();
	}

	/**
	 * Return true if the complete legend is considered too large to be added above the graph.
	 */
	private boolean isTooLarge () {
		
		// Not enough if some legendItems have a very long text
//		return getLegendItemCount() > 3;
		
		// Count the letters
		int count = 0;
		for (LegendItem li : legendItems) {
			count += li.getLabel().length();
		}
		return count > 50; // arbitrary
		
	}
	
	
	/**
	 * If the chart's legend is small draw it above the graph, else remove it
	 * and replace it by a button to open it in a separate panel to let the
	 * space to the chart.
	 */
	public JComponent getChartWithLegend() {

		if (!isTooLarge ()) {

			// If legend size is small, complete legend

			LegendTitle legend = chart.getLegend();
			legend.setPosition(RectangleEdge.TOP);
			legend.setFrame(BlockBorder.NONE);
			legend.setHorizontalAlignment(HorizontalAlignment.RIGHT);

			return chartPanel; // ok, do nothing

		} else {

			// replace the legend

			// remove default legend
			chart.removeLegend();

			// Create openLegend if null
			if (openLegend == null) {
				openLegend = new JLabel("<html><u>" + Translator.swap("DRGraph.legend") + "</u></html>");
				openLegend.setForeground(new Color(113, 114, 227)); // some blue
				openLegend.setOpaque(true); // needed for background
				openLegend.setBackground(Color.WHITE);
				openLegend.setHorizontalAlignment(SwingConstants.RIGHT);
				openLegend.addMouseListener(new MouseAdapter() {

					@Override
					public void mouseClicked(MouseEvent e) {

						if (dlg == null) {

							boolean modal = false;
							boolean withControlPanel = false;
							boolean memoSize = false;
							boolean memoLocation = false;
							
							dlg = new DialogWithClose(chartPanel, new JScrollPane(legendPanel), chartTitle, modal,
									withControlPanel, memoSize, memoLocation);
							
							// To make the scrollbars appear
							Dimension size = dlg.getSize ();
							dlg.setSize (size.width+1, size.height+1);
							
						} else {

							dlg.setVisible(!dlg.isVisible());

						}
					}
				});
			}

			// add an underlined label above the chart
			LinePanel l1 = new LinePanel();
			l1.addGlue();
			l1.add(openLegend);
			l1.addStrut0();

			JPanel aux = new JPanel(new BorderLayout());

			aux.add(l1, BorderLayout.NORTH);
			aux.add(chartPanel, BorderLayout.CENTER);

			return aux;

		}

	}

	private String trace() {

		StringBuffer b = new StringBuffer();

		String LN = "\n";

		b.append(LN + "LegendPanel trace" + LN);
		b.append("   legendTitle: " + LN);

		b.append("   #sources: " + sources.length + LN);
		b.append("   #legendItems: " + legendItems.size() + LN);

		for (LegendItemSource itemSource : sources) {

			LegendItemCollection liCollection = itemSource.getLegendItems();

			b.append("   source: " + itemSource + LN);
			b.append("      #legendItem: " + liCollection.getItemCount() + LN);

			for (int i = 0; i < liCollection.getItemCount(); i++) {
				LegendItem legendItem = liCollection.get(i);
				b.append("      legendItem label: " + legendItem.getLabel() + " seriesKey: " + legendItem.getSeriesKey()
						+ " description: " + legendItem.getDescription() + LN);

			}
		}

		return b.toString();

	}

	public JFreeChart getChart() {
		return chart;
	}

	public LegendTitle getLegendTitle() {
		return legendTitle;
	}

	public LegendItemSource[] getSources() {
		return sources;
	}

	public List<LegendItem> getLegendItems() {
		return legendItems;
	}

}
