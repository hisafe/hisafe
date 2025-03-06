package capsis.gui.command;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.JFrame;

import capsis.commongui.projectmanager.ProjectManager;
import capsis.gui.Pilot;
import jeeb.lib.util.ActionCommand;
import jeeb.lib.util.IconLoader;
import jeeb.lib.util.Log;
import jeeb.lib.util.MessageDialog;
import jeeb.lib.util.Settings;
import jeeb.lib.util.StatusDispatcher;
import jeeb.lib.util.Translator;

/**
 * Reset the layout of the diagrams in the tool page(s) in the mainFrame.
 * 
 * @author F. de Coligny - March 2021
 */
public class AutoLayoutDesktop extends AbstractAction implements ActionCommand {

	static {
		IconLoader.addPath("capsis/images");
	}

	static private String name = Translator.swap("MainFrame.helpAutoLayoutDesktop");

	private JFrame frame;

	/**
	 * Constructor
	 */
	public AutoLayoutDesktop(JFrame frame) {
		super(name);

		putValue(SMALL_ICON, IconLoader.getIcon("grid_16.png"));
		putValue(LARGE_ICON_KEY, IconLoader.getIcon("grid_24.png"));

		this.frame = frame;
		this.putValue(Action.SHORT_DESCRIPTION, name); // fc-30.3.2021

//		this.putValue (Action.ACCELERATOR_KEY, 
//				KeyStroke.getKeyStroke (KeyEvent.VK_H, ActionEvent.CTRL_MASK));
//		this.putValue (Action.MNEMONIC_KEY, 'N');
	}

	/**
	 * Action interface
	 */
	@Override
	public void actionPerformed(ActionEvent e) {
		execute();

	}

	/**
	 * Command interface
	 */
	@Override
	public int execute() {

		try {

			// Check if mosaic is on
			boolean mosaicOn = Settings.getProperty("auto.mode.mosaic", true);

			// Ensure mosaic on while running autoLayoutDesktop
			Settings.setProperty("auto.mode.mosaic", true);
			Pilot.getPositioner().autoLayoutDesktop();

			// Turn off mosaic if it was off
			if (!mosaicOn)
				Settings.setProperty("auto.mode.mosaic", false);

			// fc-25.10.2023 Added a refresh of the project manager
			ProjectManager.getInstance().update();

		} catch (Throwable e) {
			Log.println(Log.ERROR, "AutoLayoutDesktop.execute ()", "An Exception/Error occured", e);
			StatusDispatcher.print(Translator.swap("Shared.commandFailed"));
			MessageDialog.print(this, Translator.swap("Shared.commandFailed"), e);
			return 1;
		}
		return 0;
	}

}
