package capsis.extension.dataextractor.configuration;

import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Component;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JPanel;
import javax.swing.JScrollPane;

import jeeb.lib.util.AmapDialog;
import jeeb.lib.util.AmapTools;
import jeeb.lib.util.JWidthLabel;
import jeeb.lib.util.LinePanel;
import jeeb.lib.util.MemoPanel;
import jeeb.lib.util.Translator;
import jeeb.lib.util.gui.NorthPanel;
import jeeb.lib.util.gui.SouthPanel;

/**
 * A superclass for specific properties in dataExtractors (i.e. not a boolean,
 * double, int, set...).
 *
 * @see DESettings.
 * @author F. de Coligny - July 2019
 */
public abstract class DESpecificProperty {

	// fc-4.11.2020 Review, added display...

	// This display is managed by getConfigurationLine() and action on the
	// configButton
	private MemoPanel shortConfigurationDisplay;

	// --- Stuff to be provided by subclasses

	/**
	 * Returns a dialog to configure this property. Note: the dialog is supposed to
	 * be modal (return only when closed), when closing, it should check that
	 * everything is ok and configure the property before indeed closing. See a
	 * subclass for example. The dialog is supposed to open directly when this
	 * method is called.
	 */
	public abstract AmapDialog getConfigurationDialog(Window parent);

	/**
	 * Returns a short String telling about the current configuration state.
	 */
	// fc-3.11.2020
	public abstract String getShortConfigurationLabel();

	// --- Generic stuff

	/**
	 * Returns a line with a button to open the configuration dialog for the given
	 * property. The button may be disabled in case the configuration is not
	 * permitted.
	 */
	public JPanel getConfigurationLine(Component parentComponent, String propertyName, boolean buttonEnabled) {

		LinePanel l1 = new LinePanel();

		JPanel aux = new JPanel(new BorderLayout());
		l1.add(aux);

		aux.add(new NorthPanel(new JWidthLabel(Translator.swap(propertyName) + " : ", 150)), BorderLayout.WEST);
		
		shortConfigurationDisplay = new MemoPanel("");
		shortConfigurationDisplay.setBackground (Color.WHITE);
		aux.add(new JScrollPane(shortConfigurationDisplay), BorderLayout.CENTER);
		
		
		// Short configuration label
		updateShortConfigurationDisplay();

		SpecificPropertyButton configButton = new SpecificPropertyButton(parentComponent,
				Translator.swap("Shared.configure"), propertyName, this);
		configButton.setEnabled(buttonEnabled);

		aux.add(new SouthPanel (configButton), BorderLayout.EAST);

		l1.addGlue();

		return l1;
	}

	private void updateShortConfigurationDisplay() {
		// Update the short configuration label in the textfield
		if (shortConfigurationDisplay != null) {

			String label = Translator.swap("Shared.noConfiguration");
			if (getShortConfigurationLabel() != null && getShortConfigurationLabel().length() != 0)
				label = getShortConfigurationLabel();

			shortConfigurationDisplay.setText(label);
		}

	}

	/**
	 * A button to open a specificProperty configuration panel
	 */
	private static class SpecificPropertyButton extends JButton implements ActionListener {
		// fc-4.7.2019
		private Component parentComponent;
		private DESpecificProperty property;
		private String propertyName;

		/**
		 * Constructor
		 */
		public SpecificPropertyButton(Component parentComponent, String buttonText, String propertyName,
				DESpecificProperty property) {
			super(buttonText);
			this.parentComponent = parentComponent;
			this.propertyName = propertyName;
			this.property = property;
			this.addActionListener(this);
		}

		public String getPropertyName() {
			return propertyName;
		}

		public DESpecificProperty getProperty() {
			return property;
		}

		@Override
		public void actionPerformed(ActionEvent e) {
			Window parent = AmapTools.getWindow(parentComponent);

			// The dialog should be modal (return only when closed)

			AmapDialog dlg = property.getConfigurationDialog(parent);
			// The dialog is supposed to open directly, then it should check all
			// user input and configure the specific property on close

			property.updateShortConfigurationDisplay();
		}

	}

}
