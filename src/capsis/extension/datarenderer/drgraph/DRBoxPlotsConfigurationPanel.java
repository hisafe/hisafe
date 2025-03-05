package capsis.extension.datarenderer.drgraph;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JCheckBox;

import capsis.extension.dataextractor.configuration.DRConfigurationPanel;
import capsis.util.configurable.Configurable;
import jeeb.lib.util.ColumnPanel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.Translator;

/**
 * Configuration Panel for DRBoxPlots
 * 
 * @author F. de Coligny - July 2021
 */
public class DRBoxPlotsConfigurationPanel extends DRConfigurationPanel implements ActionListener {

	private DRBoxPlots graph;
	
	private JCheckBox meanVisible;
	
	/**
	 * Constructor
	 */
	public DRBoxPlotsConfigurationPanel(Configurable graph) {
		super(graph);
		
		this.graph = (DRBoxPlots) graph;
		
		// Main layout
		ColumnPanel main = new ColumnPanel();
		
		// Mean visible in the box plots
		LinePanel l1 = new LinePanel();
		meanVisible = new JCheckBox(Translator.swap("DRBoxPlots.meanVisible"), this.graph.meanVisible);
		l1.add(meanVisible);
		l1.addGlue();
		main.add(l1);		

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
	
	public boolean isMeanVisible() {
		return meanVisible.isSelected();
	}


	
}
