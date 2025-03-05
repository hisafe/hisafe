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
 * Configuration Panel for DRGraph.
 * 
 * @author F. de Coligny - October 2015
 */
public class DRGraphConfigurationPanel extends DRConfigurationPanel implements ActionListener {

	private DRGraph drgraph;
	
	private JCheckBox enlargedMode;
	
	/**
	 * Constructor
	 */
	public DRGraphConfigurationPanel(Configurable drgraph) {
		super(drgraph);
		
		this.drgraph = (DRGraph) drgraph;
		
		// Enlarged mode
		LinePanel l1 = new LinePanel();
		enlargedMode = new JCheckBox(Translator.swap("DRGraph.enlargedMode"), this.drgraph.enlargedMode);
		l1.add(enlargedMode);
		l1.addGlue();
		
		
		// Main layout
		ColumnPanel main = new ColumnPanel();
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

	public boolean isEnlargedMode() {
		return enlargedMode.isSelected();
	}

	
	
}
