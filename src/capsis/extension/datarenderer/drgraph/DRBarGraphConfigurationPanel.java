package capsis.extension.datarenderer.drgraph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;

import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;
import capsis.extension.dataextractor.configuration.DRConfigurationPanel;
import capsis.util.configurable.Configurable;

/**
 * Configuration Panel for DRBarGraph.
 * 
 * @author F. de Coligny - October 2015
 */
public class DRBarGraphConfigurationPanel extends DRConfigurationPanel implements ActionListener {

	private DRBarGraph graph;
	
	private JCheckBox visibleValues;
	private JCheckBox cumulative;
	
	/**
	 * Constructor
	 */
	public DRBarGraphConfigurationPanel(Configurable graph) {
		super(graph);
		
		this.graph = (DRBarGraph) graph;
		
		// Main layout
		ColumnPanel main = new ColumnPanel();
		
		// Values written on the top of the bars
		LinePanel l1 = new LinePanel();
		visibleValues = new JCheckBox(Translator.swap("DRBarGraph.visibleValues"), this.graph.visibleValues);
		l1.add(visibleValues);
		l1.addGlue();
		main.add(l1);
		
		LinePanel l2 = new LinePanel();
		cumulative = new JCheckBox(Translator.swap("DRBarGraph.cumulative"), this.graph.cumulative);
		l2.add(cumulative);
		l2.addGlue();
		main.add(l2);
		

		mainContent.add(main);
		
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// Nothing at this time: a checkbox can not be in error
		
	}

	@Override
	public boolean checksAreOk() {
		// A checkbox can not be in error
		return true;
		
	}
	
	public boolean isVisibleValues() {
		return visibleValues.isSelected();
	}
	
	public boolean isCumulative() {
		return cumulative.isSelected();
	}


	
}
