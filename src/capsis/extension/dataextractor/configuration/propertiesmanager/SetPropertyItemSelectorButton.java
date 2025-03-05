package capsis.extension.dataextractor.configuration.propertiesmanager;

import javax.swing.JButton;
import javax.swing.JTextField;

/**
 * This class is related to setProperties. A button to open an item selector to
 * choose selected values among a list of possible values.
 * 
 * @author F. de Coligny - September 2021
 */
public class SetPropertyItemSelectorButton extends JButton {

	public String propertyName;
	public SetPropertyItemSelector itemSelector;
	public JTextField textField;

	/**
	 * Constructor
	 */
	public SetPropertyItemSelectorButton(String text, String propertyName, SetPropertyItemSelector itemSelector,
			JTextField textField) {

		super(text);

		this.propertyName = propertyName;
		this.itemSelector = itemSelector;
		this.textField = textField;
	}

}
