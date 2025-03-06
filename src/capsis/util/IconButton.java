package capsis.util;

import javax.swing.BorderFactory;
import javax.swing.ImageIcon;
import javax.swing.JButton;

/**
 * A button with an icon and a pressed icon, but with no border, compatible with
 * many Look and Feels.
 * 
 * @author F. de Coligny - September 2021
 */
public class IconButton extends JButton {

//	static private Border focusBorder = BorderFactory.createLineBorder(Color.GRAY);
	
	private ImageIcon icon;
	private ImageIcon pressedIcon;

	/**
	 * Constructor
	 */
	public IconButton(ImageIcon icon, ImageIcon pressedIcon) {
		super (icon);
		setPressedIcon(pressedIcon);
		
		this.icon = icon;
		this.pressedIcon = pressedIcon;	
		
		// This set of options seem ok with various L&F under Linux, to be checked under
		// Windows and Mac...
		setBorder(null);
		setBorder(BorderFactory.createEmptyBorder());
		setContentAreaFilled(false);
		setBorderPainted(false);

		// Brings trouble...
//		setFocusable(true);
//		setFocusPainted (true);
		
	}

}
