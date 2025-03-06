package capsis.util.extendeddefaulttype;

import java.awt.BorderLayout;
import java.awt.FlowLayout;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JButton;
import javax.swing.JComboBox;
import javax.swing.JLabel;
import javax.swing.JPanel;
import javax.swing.border.Border;

import repicea.gui.OwnedWindow;
import repicea.gui.REpiceaDialog;
import repicea.gui.REpiceaMemorizerHandler;
import repicea.gui.UIControlManager;
import repicea.gui.UIControlManager.CommonControlID;
import repicea.serial.Memorizable;
import repicea.util.REpiceaTranslator;
import repicea.util.REpiceaTranslator.TextableEnum;

public abstract class ExtSimulationSettingsDialog<P extends ExtSimulationSettings> extends REpiceaDialog implements ActionListener,	OwnedWindow {

	public static final String MonteCarloVarSourcePrefix = "MonteCarloVarSource_";
	public static final String ModelComponentPrefix = "ModelComponent_";
	
	public static enum MessageID implements TextableEnum {
		MonteCarloOptions("Variability of the following components:", "Variabilit\u00E9 des \u00E9l\u00E9ments suivants :"),
		MonteCarloTitle("Stochastic mode", "Mode stochastique"),
		ModelComponentTitle("Model components", "Composantes du mod\u00E8le")
		;

		MessageID(String englishString, String frenchString) {
			setText(englishString, frenchString);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}

		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}

	}

	public static enum EnableDisableOption implements TextableEnum {
		Enabled("Enabled", "Activ\u00E9"),
		Disabled("Disabled", "Desactiv\u00E9");

		EnableDisableOption(String englishString, String frenchString) {
			setText(englishString, frenchString);
		}

		@Override
		public void setText(String englishText, String frenchText) {
			REpiceaTranslator.setString(this, englishText, frenchText);
		}

		@Override
		public String toString() {
			return REpiceaTranslator.getString(this);
		}

	}

	protected P settings;
	protected JButton ok;
	protected JButton cancel;
	protected List<JComboBox<EnableDisableOption>> monteCarloVariabilityOptions;

	protected ExtSimulationSettingsDialog(Window w, P settings) {
		super(w);
		this.settings = settings;

		monteCarloVariabilityOptions = new ArrayList<JComboBox<EnableDisableOption>>();

		for (TextableEnum source : settings.getPossibleMonteCarloSources()) {
			JComboBox<EnableDisableOption> checkBox = new JComboBox<EnableDisableOption>(EnableDisableOption.values());
			checkBox.setName(MonteCarloVarSourcePrefix.concat(((Enum) source).name()));
			monteCarloVariabilityOptions.add(checkBox);
		}

		setTitle(REpiceaTranslator.getString(CommonControlID.Options));
		ok = UIControlManager.createCommonButton(CommonControlID.Ok);
		cancel = UIControlManager.createCommonButton(CommonControlID.Cancel);

		new REpiceaMemorizerHandler(this);

	}

	public void actionPerformed(ActionEvent evt) {
		if (evt.getSource().equals(ok)) {
			okAction();
		} else if (evt.getSource().equals(cancel)) {
			cancelAction();
		}
	}


	@Override
	public void listenTo() {
		ok.addActionListener(this);
		cancel.addActionListener(this);
		for (JComboBox<EnableDisableOption> cb: monteCarloVariabilityOptions) {
			cb.addItemListener(settings);
		}
	}

	@Override
	public void doNotListenToAnymore() {
		ok.removeActionListener(this);
		cancel.removeActionListener(this);
		for (JComboBox<EnableDisableOption> cb: monteCarloVariabilityOptions) {
			cb.removeItemListener(settings);
		}
	}

	@Override
	public void synchronizeUIWithOwner() {
		for (int i = 0; i < settings.getPossibleMonteCarloSources().size(); i++) {
			boolean isEnabled = settings.getMonteCarloSettings(settings.getPossibleMonteCarloSources().get(i));
			EnableDisableOption opt;
			if (isEnabled) {
				opt = EnableDisableOption.Enabled;
			} else {
				opt = EnableDisableOption.Disabled;
			}
			monteCarloVariabilityOptions.get(i).setSelectedItem(opt);
		}
	}

	@Override
	public Memorizable getWindowOwner() {return settings;}

	protected JPanel getMonteCarloPanel() {
		JPanel c1 = new JPanel();
		Border b = BorderFactory.createTitledBorder(MessageID.MonteCarloTitle.toString());
		c1.setBorder(b);
		c1.setLayout(new BoxLayout(c1, BoxLayout.Y_AXIS));
		c1.add(Box.createVerticalStrut(5));
		
		JLabel label = UIControlManager.getLabel(MessageID.MonteCarloOptions);
		JPanel labelPanel = new JPanel(new FlowLayout(FlowLayout.LEFT));
		labelPanel.add(label);
		labelPanel.add(Box.createGlue());
		c1.add(labelPanel);
		c1.add(Box.createVerticalStrut(5));
		
		JPanel checkBoxPanel;
		for (int i = 0; i < settings.getPossibleMonteCarloSources().size(); i++) {
			checkBoxPanel = new JPanel();
			checkBoxPanel.setLayout(new BoxLayout(checkBoxPanel, BoxLayout.X_AXIS));
			checkBoxPanel.add(Box.createHorizontalStrut(10));
			checkBoxPanel.add(UIControlManager.getLabel(settings.getPossibleMonteCarloSources().get(i)));
			checkBoxPanel.add(Box.createHorizontalGlue());
			JComboBox cb = monteCarloVariabilityOptions.get(i);
			cb.setMaximumSize(cb.getPreferredSize());
			checkBoxPanel.add(cb);
			checkBoxPanel.add(Box.createHorizontalStrut(10));
			c1.add(checkBoxPanel);
			c1.add(Box.createVerticalStrut(5));
		}
		
		return c1;
	}

	protected JPanel getControlPanel() {
		JPanel controlPanel = new JPanel(new FlowLayout(FlowLayout.RIGHT));
		controlPanel.add(ok);
		controlPanel.add(cancel);
		return controlPanel;
	}
	
	@Override
	protected void initUI() {
		getContentPane().add(getMonteCarloPanel(), BorderLayout.CENTER);
		getContentPane().add(getControlPanel(), BorderLayout.SOUTH);
	}

}
