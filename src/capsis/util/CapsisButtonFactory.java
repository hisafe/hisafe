package capsis.util;

import java.util.ArrayList;
import java.util.Collection;

import javax.swing.ImageIcon;
import javax.swing.JButton;

import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Translator;

/**
 * The ButtonFactory class provides a static method for creating the most common controls, such as ok, help, open, etc.
 * By default, the icons are enabled. However, it is possible to switch them on and off through the setIconEnabled method.
 * The class keeps the buttons in memory in the buttons member.
 * @author Mathieu Fortin - September 2011
 */
public class CapsisButtonFactory {
	
	public static class CapsisButton extends JButton {
		private ButtonID buttonID;
		
		private CapsisButton(ButtonID buttonID) {
			super();
			this.buttonID = buttonID;
			setText(Translator.swap("Shared." + buttonID.name()));
		}
		
		private void setIcon() {
			if (iconEnabled) {
//				ImageIcon icon = IconLoader.getIcon ("gtk/" + buttonID.name().concat(".png"));
				ImageIcon icon = IconLoader.getIcon ("" + buttonID.name().concat("_16.png")); // fc-2.10.2012 reviewed the icons framework, this is the most likely to work... see src/capsis/images
				setIcon(icon);
				
			} else {
				setIcon(null);
			}
			validate();
		}
		
	}
	
	public static enum ButtonID {ok, cancel, help, open, save, saveas, export}
	
	private static boolean iconEnabled = true;
	
	private static Collection<CapsisButton> buttons = new ArrayList<CapsisButton>();
	
	/**
	 * This method generates a button. The method is recorded in the buttons member.
	 * @param buttonID an enum that defines the button
	 * @return a JButton instance
	 */
	public static JButton createButton(ButtonID buttonID) {
		CapsisButton button = new CapsisButton(buttonID);
		buttons.add(button);
		button.setIcon();
		
		return button;
	}
	
	/**
	 * This method enables or disables the icons.
	 * @param iconEnabled a boolean
	 */
	public static void setIconEnabled(boolean iconEnabled) {
		if (CapsisButtonFactory.iconEnabled != iconEnabled) {
			CapsisButtonFactory.iconEnabled = iconEnabled;
			for (CapsisButton button : buttons) {
				button.setIcon();
			}
		}
	}
	
	/**
	 * This method returns true if the icons are enabled or false otherwise
	 * @return a boolean
	 */
	public static boolean isIconEnabled() {return CapsisButtonFactory.iconEnabled;}

}
